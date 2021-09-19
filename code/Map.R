#### library packages ####
library(tidyverse)
library(leaflet)
library(rgeos)

#### load data ####
load("data/data.Rdata")

#### draw a map to display the locations and rivers ####
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

#### An alternative method to draw a river map ####

# read river data from a stored river map .Rdata
load("data/usa_rivers.RData")
lines.rivers <- subset(lines.rivers, (NAME_UC %in% c('DES MOINES RIVER',
                                                     'MINNESOTA RIVER',
                                                     'WAPSIPINICON RIVER',
                                                     'IOWA RIVER')))
df.usa_rivers <- fortify(lines.rivers)

# map link: https://tapiquen-sig.jimdofree.com/english-version/free-downloads/americas/
# To subset the mississippi river

riversData <- readOGR("data/Americas_Hydrography")
riversData@data %>% filter(SYSTEM %in% "Mississippi")

MISS <- subset(riversData, (SYSTEM %in% "Mississippi"))
miss <- fortify(MISS)

# Combine the two river dataset
df.usa_rivers <- rbind(df.usa_rivers,miss)

# Mark the gauge stations
ggplot(data = bound) +
  geom_sf() +
  geom_path(data = df.usa_rivers, 
            aes(x = long, y = lat, group = group), 
            color = "#8ca7c0", size = 1.5) +
  geom_point(data = Siteinfor, aes(x = dec_long_va, y = dec_lat_va), size = 2)+
  coord_sf(xlim = c(-98,-89), ylim = c(40,48)) +
  labs(title = "Rivers and waterways of the United States")



