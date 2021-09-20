#### library packages ####
library(tidyverse)
library(ismev)
library(evd)
library(strucchange)
source("code/NonLinearGEV/MovingWindow.R")
source("code/NonLinearGEV/gev.fix.R")
#### load data ####
load("data/data.Rdata")

data <- data %>% filter(Month %in% 6:8)
data$station <- rep(Siteinfor$site_no,each = 3680)
data <- data %>% filter(Q >= 0)

NoSt <- 24
#### GEV marginal ####
SummerMaxima <- data %>% group_by(station, DecYear) %>% summarise(max(Q)) %>% spread(station, `max(Q)`)

#### define breakpoint ####
shape <- numeric(NoSt)
se <- numeric(NoSt)
for (i in 1:NoSt) {
  Station <- SummerMaxima[,c(1, i+1)]
  names(Station) <- c("DecYear", "Q")
  fit <- gev.fit(Station$Q, Station, show = FALSE)
  shape[i] <- fit$mle[3]
  se[i] <- fit$se[3]
}

point <- matrix(NA, ncol = 2,nrow = NoSt)
for (i in 1:NoSt) {
  Station <- SummerMaxima[,c(1, i+1)]
  names(Station) <- c("DecYear", "Q")
  MOV <- MovingWindow(
    x = Station, win = 10, url = "plot/MovingWindow",fix.sh = shape[i],
    save = TRUE, name = i
  )
  point[i,1] <-  breakpoints(Fstats(MOV[,2] ~ 1))$breakpoints
  point[i,2] <-  breakpoints(Fstats(MOV[,3] ~ 1))$breakpoints
}
point <- as.data.frame(point)

rm(fit, MOV, Station, i)





