#### library packages ####
library(tidyverse)
library(ismev)
library(evd)
#### load data ####
load("data/data.Rdata")

data <- data %>% filter(Month %in% 6:8)
data$station <- rep(Siteinfor$site_no,each = 3680)
data <- data %>% filter(Q >= 0)

#### GEV marginal ####
SummerMaxima <- data %>% group_by(station, DecYear) %>% summarise(max(Q)) %>% spread(station, `max(Q)`)

method <- "BFGS" 
control <- list(maxit = 1000, reltol=10^(-30), abstol=0.0001, trace=2)

Maxima <- data %>% group_by(station, DecYear) %>% summarise(max(Q))
StsChos <- c(1:24)
NoSt <- length(StsChos)

# test the trend
ResGev <- list()
GevPars <- matrix(0, NoSt, 3)
FreeGevNLLTemp <- numeric()

for (i in 1:24){
  stat <- unique(Maxima$station)[i]
  x <- Maxima %>%
    filter(station == stat)
  fit1 <- gev.fit(as.matrix(x[,3]), show = FALSE)
  fit2 <- gev.fit(as.matrix(x[,3]), ydat=as.matrix(x[,2]-as.numeric(x[1,2])),mul=1, show = FALSE)
  
  if(2*(fit2$nllh - fit1$nllh) < qchisq(0.95, 1)){ResGev[[i]]=fit1;print("no")}else{ResGev[[i]]=fit2;print("yes")}
  GevPars[i,] <- ResGev[[i]]$mle
  FreeGevNLLTemp[i] <- ResGev[[i]]$nll
}






