#### library packages ####
library(tidyverse)
library(ismev)
library(evd)
library(GEVcdn)
#### load data ####
load("data/data.Rdata")

data <- data %>% filter(Month %in% 6:8)
data$station <- rep(Siteinfor$site_no,each = 3680)
data <- data %>% filter(Q >= 0)

#### GEV marginal ####
SummerMaxima <- data %>% group_by(station, DecYear) %>% summarise(max(Q)) %>% spread(station, `max(Q)`)



## Generate synthetic data, quantiles
for(j in 1:24){
x <- as.matrix(SummerMaxima$DecYear)
y <- as.matrix(SummerMaxima[,j+1])
## Define a hierarchy of models of increasing complexity
models <- vector("list", 4)
# Stationary model
models[[1]] <- list(Th = gevcdn.identity,
                    fixed = c("location", "scale", "shape"))
# Linear model
models[[2]] <- list(Th = gevcdn.identity)
# Nonlinear, 1 hidden node
models[[3]] <- list(n.hidden = 1, Th = gevcdn.logistic)
# Nonlinear, 2 hidden nodes
models[[4]] <- list(n.hidden = 2, Th = gevcdn.logistic)

## Fit models

weights.models <- vector("list", length(models))
for(i in seq_along(models)){
  weights.models[[i]] <- gevcdn.fit(x = x, y = y, n.trials = 1,
                                    n.hidden = models[[i]]$n.hidden,
                                    Th = models[[i]]$Th,
                                    fixed = models[[i]]$fixed)
}

## Select model with minimum AICc

models.AICc <- sapply(weights.models, attr, which = "AICc")
weights.best <- weights.models[[which.min(models.AICc)]]
parms.best <- gevcdn.evaluate(x, weights.best)

## 10th, 50th, and 90th percentiles

q.best <- sapply(c(0.1, 0.5, 0.9), qgev,
                 location = parms.best[,"location"],
                 scale = parms.best[,"scale"],
                 shape = parms.best[,"shape"])

## Plot data and quantiles

matplot(x, cbind(y, q.best), type = c("b", rep("l", 6)),
        lty = c(1, rep(c(1, 1), 2)),
        lwd = c(1, rep(c(3, 3), 2)),
        col = c("red", rep("orange", 3), rep("blue", 3)),
        pch = 19, xlab = "x", ylab = "y", main = "gevcdn.fit")
}