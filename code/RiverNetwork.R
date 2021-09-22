load("data/data.Rdata")

source("F:/Extre/Codes/Functions.R")

data <- data %>% 
  filter(Month %in% 6:8) %>% 
  filter(Q >= 0) %>% 
  select(Date, Q)

data$station <- rep(Siteinfor$site_no,each = 3680)
data <- data %>% spread(station, Q)

StsChos <- c(1:24)
NoSt <- length(StsChos)

Years <- unique(as.numeric(substr(data$Date,1,4)))
StNames <- as.character(StsChos)
YearsWithEvent <- numeric()
AllEvMat <- matrix(0,length(StNames),1)
AllRes <- list()
for (i in 1:length(Years)){
  # events in the i^th year at each station
  Res <- ObtainMultVarEvents(TSs=data,U=0.1,Lag=4,Year=Years[i],StNames=StNames,Plotting=0,mfrow=c(NoSt,1))  ## For Censored Likelihood
  Events <- Res$AllEvMat
  Locations <- Res$Location
  if (length(Events)>0) {
    YearsWithEvent <- c(YearsWithEvent,rep(Years[i],dim(Events)[2]))
    AllEvMat <- cbind(AllEvMat,Events)
  }
  AllRes[[i]] <- Res 
}

DataEvents <- t(AllEvMat[,-1])
rownames(DataEvents) <- YearsWithEvent


CW <- read.csv("data/weight.csv")
CatchtrWt <- CW %>% select(wx,wy)
p <- cbind(CatchtrWt$wy, CatchtrWt$wx)
library(SpatialExtremes)
library(geosphere)
RiverDisWt <- distm(p, p, fun = distHaversine)
RiverDisWt <- RiverDisWt/1000

FlowCon <- read.csv("data/FlowCon.csv",header = FALSE)
con <- FlowCon[lower.tri(FlowCon)]
FlowCon <- t(FlowCon)
FlowCon[lower.tri(FlowCon)] <- con

locations <- Siteinfor %>% select(dec_long_va, dec_lat_va)
locations <- as.matrix(locations)
colnames(locations) <- c("lon", "lat")
##Compute the madogram
theta2 <- madogram(DataEvents, locations)
theta <- madogram(DataEvents, as.matrix(RiverDisWt))

## Hydrological distance

Theta.cen <- matrix(1,ncol = 24,nrow = 24)
for(i in 1:(24-1))
  for(j in (i+1):24) {
    Data.biv <- DataEvents[,c(i,j)]
    Theta.cen[i,j] <- CensoredEstimationHR(Data.biv, thresholds = .90)
    Theta.cen[j,i] <- Theta.cen[i,j]
  }
Theta <- Theta.cen
Theta <- Theta[lower.tri(Theta)]
dist <- theta[,1]
Theta <- data.frame(Theta, dist)
Theta$group <- con
Theta <- Theta %>% mutate(
  group = ifelse(group==1, "connected", "unconnected")
)

ggplot(data = Theta[-c(103,121),]) + 
  geom_point(aes(x = dist, y=Theta, color = as.factor(group)),size = 2) +
  #  coord_fixed(ratio = 10) +
  ylim(c(1,2))

## Euclidean distance

Theta2 <- Theta.cen
Theta2 <- Theta2[lower.tri(Theta2)]
dist <- theta2[,1]*100
Theta2 <- data.frame(Theta2, dist)
Theta2$group <- con
Theta2 <- Theta2 %>% mutate(
  group = ifelse(group==1, "connected", "unconnected")
)

ggplot(data = Theta2[-c(103,121),]) + 
  geom_point(aes(x = dist, y=Theta2, color = as.factor(group)),size = 2) +
  #  coord_fixed(ratio = 10) +
  ylim(c(1,2))

# Extreme Coffecients for madogram
## Hydrological distance

m <- matrix(NA, ncol = 24, nrow = 24)
m[lower.tri(m)] <- 1:276

theta <- madogram(DataEvents, as.matrix(RiverDisWt))

theta <- as.data.frame(theta)
theta$group <- con

theta <- theta %>% mutate(
  group = ifelse(group==1, "connected", "unconnected")
)

ggplot(data = theta[-c(103,121),]) + 
  geom_point(aes(x = dist, y=ext.coeff, color = as.factor(group)),size = 2) +
  #  coord_fixed(ratio = 10) +
  ylim(c(1,2))

## Euclidean distance
theta2 <- as.data.frame(theta2)
theta2$group <- con

theta2 <- theta2 %>% mutate(
  group = ifelse(group==1, "connected", "unconnected")
)

ggplot(data = theta2[-c(103,121),]) + 
  geom_point(aes(x = dist, y=ext.coeff, color = as.factor(group)),size = 2) +
  #  coord_fixed(ratio = 10) +
  ylim(c(1,2))

# Madogram vs HR

m2 <- matrix(NA, 24, 24)
m2[lower.tri(m2)] <- theta[,3]

m2 <- t(m2)
m2[lower.tri(m2)] <- theta[,3]
diag(m2) <- 1

plotECF(ECemp = m2,
        ECtheo = Theta.cen,
        Dist = RiverDisWt,
        is.con = FlowCon,
        StsIdx  = StsChos,
        which.plots = c(FALSE,FALSE,TRUE),
        which.labels = c(FALSE, FALSE,FALSE),
        PDF = FALSE,
        filename = paste("Plots","ECF_Emp_HR.pdf",sep="/"))



