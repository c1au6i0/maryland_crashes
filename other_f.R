library(tidyverse)
library(janitor)
library(ggridges)
library(leaflet)
library(plotly)
library(rvent)
library(ggiraph)
library(htmlwidgets)
library(plotly)
library(shinydashboard)
library(shinyWidgets)
library(shiny)
library(DT)

# Get data -----
all <- readRDS("c_dat.RDS")
counties <- na.omit(unique(all$county_desc))

# List for SelectInput -----
other_var <- list(
                  "none",
                  "year", 
                  "quarter", 
                  "light_desc", 
                  "junction_desc",
                  "collision_type_desc",
                  "surf_cond_desc",
                  "lane_desc",
                  "rd_cond_desc",
                  "rd_div_desc",
                  "report_type",
                  "weather_desc",
                  "signal_flag_desc",
                  "harm_event_desc2"
                  )

hover_var <- names(all)[1:57]

# Authenticate for mapbox -----
map_token <- readRDS("map_token.RDS")
Sys.setenv("MAPBOX_TOKEN" = map_token) # for Orca

# Mapbox graph -----
mapbox <- function(dat = dat, variab, hover, c_lon = -72, c_lat = 40, c_zoom  = 5) {
  
  if( variab == "none") {
    fig <- 
      dat  %>%
      plot_mapbox(
        lat = ~latitude,
        lon = ~longitude,
        mode = "markers",
        color = I("grey50"),
        type = 'scattermapbox',
        hovertext = ~dat[,hover]) 
  } else {
  fig <- 
    dat  %>%
      plot_mapbox(
        lat = ~latitude,
        lon = ~longitude,
        mode = "markers",
        color = dat[,variab],
        type = 'scattermapbox',
        hovertext = ~dat[,hover]) 
  }
  
  fig <- fig %>%
    layout(
      mapbox = list(
        style = 'open-street-map',
        zoom = c_zoom,
        center = list(lon = c_lon, lat = c_lat)))
  
  fig
}

# Ist Plot ----
ist_plot <-  function(dat, variab){
  variab <- sym(variab)
  dat %>%
    group_by(!!variab) %>%
    summarise(n = n()) %>%
    ggplot(aes(!!variab, n)) +
    geom_col(fill = "grey", col = "black") +
    theme_bw() +
    labs(y = "number of crashes") +
    geom_label(aes(!!variab, 1, label = n)) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2))
   }



# https://plot.ly/r/group-by/
# split = ~class


