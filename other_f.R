
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
library(DT)


all <- readRDS("c_dat.RDS")

counties <- na.omit(unique(all$county_desc))

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

map_token <- readRDS("map_token.RDS")

Sys.setenv("MAPBOX_TOKEN" = map_token) # for Orca


mapbox <- function(dat = dat, variab, hover, c_lon = -76, c_lat = 39) {
  
  if( variab == "none") {
    fig <- 
      dat  %>%
      plot_mapbox(
        lat = ~latitude,
        lon = ~longitude,
        mode = "markers",
        color = I("grey20"),
        type = 'scattermapbox',
        hovertext = ~dat[,hover]) 
  } else {
  fig <- 
    dat  %>%
      plot_mapbox(
        lat = ~latitude,
        lon = ~longitude,
        mode = "markers",
        # symbol =  ~dat[,var],
        color = dat[,variab],
        type = 'scattermapbox',
        hovertext = ~dat[,hover]) 
  }
  
  fig <- fig %>%
    layout(
      mapbox = list(
        style = 'open-street-map',
        zoom = 5,
        center = list(lon = c_lon, lat = c_lat)))
  
  fig
}


# https://plot.ly/r/group-by/
# split = ~class


