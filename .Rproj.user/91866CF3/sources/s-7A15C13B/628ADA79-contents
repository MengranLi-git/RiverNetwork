#### library packages ####
library(data.table)
library(EGRET)
library(dataRetrieval)

#### A function to read data from a gauge station ####
GetStation <- function(x){
  url <- paste0("data/",x,".txt")
  
  gauge <- read.table(url, header = FALSE)
  gauge[,2] <- as.character(gauge[,2])
  gauge[,2] <- paste0("0",gauge[, 2])
  
  parameterCd <- "00060"
  Streamflow <- lapply(
    gauge[, 2], readNWISDaily, parameterCd,
    "1980-01-01", "2019-12-31"
  )
  index <- which(sapply(Streamflow, nrow)==14610)
  gauge<- gauge[index,]
  Streamflow <- Streamflow[index]
  Streamflow <- rbindlist(Streamflow)[
    ,DecYear := floor(DecYear)]
  Streamflow$River <- x
  result <- list(readNWISsite(gauge[, 2]), Streamflow)
  return(result)
}

#### select five stations ####

river <- c("mississippi", "des", "iowa", "minnesota", "wapsipinicon")

for(i in 1:length(river)){
  assign(river[i], GetStation(river[i]))
}

Siteinfor <- mississippi[[1]]
for(i in 2:length(river)){
  Siteinfor <- rbind(Siteinfor, get(river[i])[[1]])
}

data <- mississippi[[2]]
for(i in 2:length(river)){
  data <- rbind(data, get(river[i])[[2]])
}

save(Siteinfor, data, file="data/data.Rdata")
