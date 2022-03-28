
#' Leaflet options
#'

lead_opts <-  settings::options_manager(
  token = "",
  crs = NULL,
  layers = c(),
  pal = NULL
)


#' Leaf init
#'
#' Create a leaflet object with deafults
#'
#' @param crs numeric 3006 SWEREF99 as default
#'
#' @importFrom leaflet leaflet leafletOptions
#' @export

leaf_init <- function(crs = 3006){
  # library(leaflet)


  leaf_crs <- leaf_set_crs(crs)

  leaflet(options = leafletOptions(crs = leaf_crs, minZoom = 0,worldCopyJump = F))
}

#' Leaflet background
#'
#' Add leaflet background tiles from lantmäteriverket topotgraphical map in EPSG:3006 projectopmn
#'
#' @param x leaflet object
#' @param token Lantmäteriverket user token, otherwise uses option "leaf.token"
#' @importFrom leaflet addTiles tileOptions setView
#' @importFrom settings options_manager reset
#' @export


leaf_background <- function(x, token = NULL){
  token <- ifelse(is.null(token), options("leaf.token"), token)

  addTiles(x,
           urlTemplate = sprintf("https://api.lantmateriet.se/open/topowebb-ccby/v1/wmts/token/%s/?service=wmts&request=GetTile&version=1.0.0&layer=topowebb_nedtonad&style=default&tilematrixset=3006&tilematrix={z}&tilerow={y}&tilecol={x}&format=image/png",
                                 token),
           attribution = '&copy; <a href="https://www.lantmateriet.se/en/">Lantmateriet</a> Topografisk Webbkarta Visning, CCB',
           options = tileOptions(minZoom = 0, maxZoom = 12)) %>%
    leaflet::setView(17,62,1)

}

#' Set CRS for leaflet
#'
#' @importFrom sf st_crs
#' @importFrom leaflet leafletCRS

leaf_set_crs <- function(crs){
  crs_o <- st_crs(crs)

  outp <- leafletCRS(
    crsClass = "L.Proj.CRS",
    code = sprintf("EPSG:%d", crs),
    proj4def = crs_o$proj4string,
    resolutions = 2^(12:2), # 4096 down to 8
    origin = c(-12e5,85e5),
    bounds = c(-12e5,85e5,4305696,2994304)
  )

  lead_opts(crs = outp)
  outp
}


#' Make label for leaflet object
#'
#' @importFrom htmltools HTML

make_label <- function(x, lbl){

  if (!is.null(lbl) & any(colnames(x) == lbl)){
    res <- x %>% pluck(lbl) %>% lapply(htmltools::HTML)
  } else {
    res <- x %>% pluck(1) %>% lapply(htmltools::HTML)
  }
  res
}

#' Get color palet for leaflet object
#'
#' @importFrom leaflet colorFactor colorNumeric
#' @importFrom leaflet colorFactor colorNumeric
#' @importFrom purrr pluck
#' @importFrom dplyr n_distinct
#' @importFrom ggthemes tableau_color_pal
#' @importFrom magrittr `%>%`
#' @importFrom histmaps st_as_data_frame

make_color <- function(x, colorby){
  x <- pluck(x, colorby)
  if(is.character(x) | is.factor(x)){
    pal <- colorFactor( ggthemes::tableau_color_pal()(n_distinct(x)), unique(x))
  } else {
    pal <- colorNumeric(c("white", ggthemes::tableau_color_pal()(1)), unique(x))
  }
  lead_opts(pal = pal)
  pal(x)

}

#' Make popup
#'

make_popup <- function(x){
  apply(t(st_as_data_frame(x)), 2, function(y) paste(paste(names(y), y, sep = ": "), collapse = "<br/>")) %>%
    lapply(htmltools::HTML)
}



#' Leaflet polygon wrapper
#'
#' Add polygon to leaflet
#'
#' @param x leaflet object
#' @param d polygon data, sf object
#' @param lbl name of variable to use as label
#' @param interactive boolean
#' @param colorby name of variable used to fill polygons
#' @param opacity between 0 and 1
#'
#' @importFrom sf st_transform
#' @importFrom leaflet addPolygons highlightOptions addLegend
#'
#' @export

leaf_polygon <- function(x, d, lbl = NULL, interactive = FALSE, colorby = NULL, opacity = 1){
  nm <-deparse(substitute(d))

  lead_opts(layers =  c(lead_opts("layers"), nm))

  d <- st_transform(d, crs = 4326)
  p <- addPolygons(
    x,
    data = d,
    group = nm,
    weight = 1,
    opacity = 1,
    fillOpacity = opacity,
    label = if(interactive){
      ~make_label(d, lbl)
    }else{
      NULL
    },
    popup = if(interactive){
      ~make_popup(d)
    }else{
      NULL
    },
    color = ifelse(interactive, "#f0f0f0", "#F6CF65"),
    fillColor = if(is.null(colorby)){
      ifelse(interactive, "#008FD5","#FFF2AF")
    } else {
      ~make_color(d, colorby)
    },
    highlight = if (interactive){
      highlightOptions(
        weight = 1.2,
        color = "#666",
        fillColor = "#666",
        fillOpacity = 0.2,
        bringToFront = FALSE)
    } else {
      NULL
    }
  )
  if (!is.null(colorby)){
    vals <- d %>% pluck(colorby)
    p <- p %>% addLegend("bottomright", pal = lead_opts("pal"), values = vals,
              title = colorby,
              opacity = 1
    )
  }
  p
}

#' Leaflet marker wrapper
#'
#' Add marker to leaflet
#'
#' @param x leaflet object
#' @param d polygon data, sf object
#' @param lbl name of variable to use as label
#' @param interactive boolean
#' @param size size of circle
#' @param colorby name of variable to use for fill color
#' @param opacity between 0 and 1
#'
#' @importFrom leaflet addCircles
#' @importFrom sf st_coordinates
#'
#' @export



leaf_marker <- function(x, d, lbl = NULL, interactive = FALSE, color = NULL, size = NULL, colorby=NULL, opacity = 1){
  nm <-deparse(substitute(d))

  lead_opts(layers =  c(lead_opts("layers"), nm))

  d <- st_transform(d, crs = 4326)

  d_coord <- st_coordinates(d)

  p <- addCircles(
    x,
    data = d,
    lng = d_coord[,1],
    lat = d_coord[,2],
    group = nm,
    weight = ifelse(is.null(size), 10, size),
    opacity = opacity,
    # fillOpacity = 1,
    label = if(interactive){
      ~make_label(d, lbl)
    }else{
      NULL
    },
    popup = if(interactive){
      ~make_popup(d)
    }else{
      NULL
    },
    color = if (is.null(colorby)) {
      ifelse(is.null(color), "#FF3500", color)
    }
    else {
      ~make_color(d, colorby) # histmaps::make_color?
    },
    fillColor = ifelse(interactive, "#008FD5","#FFF2AF")#~pal(geom_id %in% sample_info$geom_id),
  )
  if (!is.null(colorby)) {
    vals <- d %>% pluck(colorby)
    p <- p %>% addLegend("bottomright", pal = lead_opts("pal"),
                         values = vals, title = colorby, opacity = 1)
  }
  p
}



#' Leaflet controls wrapper
#'
#' Add marker to leaflet
#'
#' @param x leaflet object
#'
#' @importFrom leaflet addLayersControl layersControlOptions
#' @importFrom settings reset
#'
#' @export


leaf_controls <- function(x){
  rr <- addLayersControl(
    x,
    overlayGroups = lead_opts("layers"),
    options = layersControlOptions(collapsed = FALSE)
  )
  settings::reset(lead_opts)
  rr
}
