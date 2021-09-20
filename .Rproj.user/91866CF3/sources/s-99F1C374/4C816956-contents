#### library packages ####
library(tidyverse)
library(leaflet)
library(rgeos)

#### load data ####
load("data/data.Rdata")

data <- data %>% filter(Month %in% 6:8)
data$station <- rep(Siteinfor$site_no,each = 3680)
data <- data %>% filter(Q >= 0)

#### Temporal EDA ####

# boxplot
data %>% mutate(Month = as.factor(Month)) %>%
  select("station", "Q", "DecYear", "Month", "River") %>%
  group_by(DecYear, River) %>%
  ggplot(aes(x=DecYear,y=Q, group = DecYear))+
  geom_boxplot() +
  facet_wrap(~River)

# Year ~ Month ~ river
data %>% mutate(Month = as.factor(Month)) %>%
  select("station", "Q", "DecYear", "Month", "River") %>%
  group_by(DecYear, Month, River) %>%
  summarise(meanQ=mean(Q)) %>% 
  ggplot(aes(x=DecYear,y=meanQ,group=Month, color = Month), color = Month)+
  geom_line() +
  facet_wrap(~River)

# Year ~ river
data %>% mutate(Month = as.factor(Month)) %>%
  select("station", "Q", "DecYear", "Month", "River") %>%
  group_by(DecYear, River) %>%
  summarise(meanQ=mean(Q)) %>% 
  ggplot(aes(x=DecYear,y=meanQ,group=River, color = River), color = River)+
  geom_line() 

# Year ~ station grouped by river
data %>% mutate(Month = as.factor(Month)) %>%
  select("station", "Q", "DecYear", "Month", "River") %>%
  group_by(DecYear, River, station) %>%
  summarise(meanQ=mean(Q)) %>% 
  ggplot(aes(x=DecYear,y=meanQ,group=station, color = station), color = station)+
  geom_line() +
  facet_wrap(~River)

#### Spatial EDA ####

load("data/map.Rdata")
# summary statistics for all locations For instance

RiverSummary <- data %>%
  select("station", "Q", "DecYear", "Month", "River") %>%
  group_by(station) %>%
  summarise(
  mean = mean(Q),
  sd = sd(Q),
  Q0.05 = quantile(Q, 0.05),
  Q0.1 = quantile(Q, 0.1),
  Q0.2 = quantile(Q, 0.2),
  Q0.5 = quantile(Q, 0.5),
  Q0.7 = quantile(Q, 0.7),
  Q0.9 = quantile(Q, 0.9),
  Q0.95 = quantile(Q, 0.95),
  Q0.975 = quantile(Q, 0.975),
  Q0.99 = quantile(Q, 0.99)
) 

Quantile <- paste0("Q", c(0.05, 0.1, 0.2, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99))

for(i in 1:length(Quantile)){
  u <- RiverSummary %>% select(station, Quantile[i])
  
  MeanExceedance <- data %>%
    select("station", "Q", "DecYear", "Month", "River") %>%
    left_join(u, c("station" = "station")) %>%
    mutate(u = get(Quantile[i])) %>%
    filter(if(Quantile[i]<0.5) Q < u else Q > u) %>%
    group_by(station) %>%
    summarise(MeanExceedance = mean(Q)) %>% 
    left_join((Siteinfor %>% select(site_no, dec_long_va, dec_lat_va)), 
              c("station" = "site_no"))
  
  ggplot(data = bound) +
    geom_sf() +
    geom_path(data = df.usa_rivers, 
              aes(x = long, y = lat, group = group), 
              color = "skyblue", size = 1.5, alpha = 0.5) +
    geom_point(data = MeanExceedance, aes(x = dec_long_va, y = dec_lat_va, color = MeanExceedance), size = 2)+
    coord_sf(xlim = c(-98,-89), ylim = c(40,48)) +
    labs(title = paste("MeanExceedance of", Quantile[i]))
  ggsave(paste0("plot/", Quantile[i],".png"), plot = last_plot())
  ggsave(paste0("plot/", Quantile[i],".SVG"), plot = last_plot())
}








#### Spatiotemporal ####

RiverSummary <- data %>%
  select("station", "Q", "DecYear", "Month", "River") %>%
  group_by(station, DecYear) %>%
  summarise(
    mean = mean(Q),
    sd = sd(Q),
    Q0.05 = quantile(Q, 0.05),
    Q0.1 = quantile(Q, 0.1),
    Q0.2 = quantile(Q, 0.2),
    Q0.5 = quantile(Q, 0.5),
    Q0.7 = quantile(Q, 0.7),
    Q0.9 = quantile(Q, 0.9),
    Q0.95 = quantile(Q, 0.95),
    Q0.975 = quantile(Q, 0.975),
    Q0.99 = quantile(Q, 0.99)
  ) 

Quantile <- paste0("Q", c(0.05, 0.1, 0.2, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99))

for(i in 1:length(Quantile)){
  u <- RiverSummary %>% select(station, DecYear, Quantile[i])
  
  MeanExceedance <- data %>%
    select("station", "Q", "DecYear", "Month", "River") %>%
    left_join(u, c("station" = "station", "DecYear" = "DecYear")) %>%
    mutate(u = get(Quantile[i])) %>%
    filter(if(Quantile[i]<0.5) Q < u else Q > u) %>%
    group_by(station, DecYear) %>%
    summarise(MeanExceedance = mean(Q)) %>% 
    left_join((Siteinfor %>% select(site_no, dec_long_va, dec_lat_va)), 
              c("station" = "site_no"))
  
  ggplot(data = bound) +
    geom_sf() +
    geom_path(data = df.usa_rivers, 
              aes(x = long, y = lat, group = group), 
              color = "skyblue", size = 1.5, alpha = 0.5) +
    geom_point(data = MeanExceedance, aes(x = dec_long_va, y = dec_lat_va, color = MeanExceedance), size = 2)+
    coord_sf(xlim = c(-98,-89), ylim = c(40,48)) +
    labs(title = paste("MeanExceedance of", Quantile[i])) +
    facet_wrap(~DecYear)
  ggsave(paste0("plot/Spatiotemporal", Quantile[i],".png"), plot = last_plot(),height=12,width=9)
  ggsave(paste0("plot/Spatiotemporal", Quantile[i],".SVG"), plot = last_plot())
}
