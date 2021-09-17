#### library packages ####
library(tidyverse)
library(leaflet)
library(rgeos)

#### load data ####
load("data/data.Rdata")

#### draw a map ####
# read map data
bound <- sf::st_read(dsn = "data/map/wbdhu2_a_us_september2020.gdb")
# set the bottom map
GetURL <- function(service, host = "basemap.nationalmap.gov") {
  sprintf("https://%s/arcgis/services/%s/MapServer/WmsServer", host, service)
}
# draw the map
bound %>% filter(huc2=="07") %>%
  leaflet() %>%
  #  setView(lng = -95, lat = 40, zoom = 4) %>%
  addCircleMarkers(
    data = Siteinfor %>% mutate(index = 1:nrow(Siteinfor)),
    lng = ~dec_long_va,
    lat = ~dec_lat_va,
    radius = ~3,
    label = ~ htmltools::htmlEscape(index),
    labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T),
    stroke = FALSE,
    fill = TRUE,
    color = "red",
    fillOpacity = 0.4,
  )  %>%
  addWMSTiles(GetURL("USGSHydroCached"), layers = "0")

# display name
library(stringr)
Siteinfor <- Siteinfor %>% select(station_nm, site_no, dec_lat_va, dec_long_va) %>%
  mutate(station_nm = str_to_lower(station_nm) )