library(purrr)
library(ggpubr)
source("code/NonLinearGEV/FitLinear.R")
source("code/NonLinearGEV/CutBreakpoint.R")
source("code/NonLinearGEV/ll.R")
source("code/NonLinearGEV/CI.R")
source("code/NonLinearGEV/FitQuadratic.R")
source("code/NonLinearGEV/FitAbrupt.R")
source("code/NonLinearGEV/ReturnLevel.R")
source("code/NonLinearGEV/GraphData.R")
source("code/NonLinearGEV/PlotParameter.R")

Graph <- list()
ReturnPlot <- list()
for(i in 1:NoSt){
Station <- SummerMaxima[,c(1, i+1)]
names(Station) <- c("DecYear", "Q")
x = Station
fit1 <- FitLinear(as.matrix(x$Q), point[i, 1], point[i, 2], shape[i],se[i])
fit2 <- FitQuadratic(as.matrix(x$Q), shape[i], se[i])
fit3 <- FitAbrupt(as.matrix(x$Q), point[i, 1], point[i, 2], shape[i],se[i])
FitList <- list(fit1[[1]], fit1[[2]], fit1[[3]], fit1[[4]], fit2, fit3)

r <- which.min(c(
  fit1[[2]]$AIC,
  fit1[[3]]$AIC,
  fit1[[4]]$AIC,
  fit2$AIC,
  fit3$AIC
))

FitList[[7]] <- FitList[[r + 1]]

nR2 <- ReturnLevel(FitList[[7]], 1 / 2)
nR10 <- ReturnLevel(FitList[[7]], 1 / 10)
nR100 <- ReturnLevel(FitList[[7]], 1 / 100)

R2 <- ReturnLevel(fit1[[2]], 1 / 2)
R10 <- ReturnLevel(fit1[[2]], 1 / 10)
R100 <- ReturnLevel(fit1[[2]], 1 / 100)


p2 <- ggplot(nR2) +
  geom_line(aes(x = Year, y = est, color = "nsGEV", linetype = "model")) +
  geom_line(aes(x = Year, y = U, color = "nsGEV", linetype = "95% CI")) +
  geom_line(aes(x = Year, y = L, color = "nsGEV", linetype = "95% CI")) +
  geom_line(data = R2, aes(x = Year, y = est, color = "sGEV", linetype = "model")) +
  geom_line(data = R2, aes(x = Year, y = U, color = "sGEV", linetype = "95% CI")) +
  geom_line(data = R2, aes(x = Year, y = L, color = "sGEV", linetype = "95% CI")) +
  labs(title = paste0("Station", i, ": 2-year"), ylab = "Return level") +
  xlab(NULL)+
  ylab("Return level") +
  scale_linetype_manual(values = c(2, 1))

p10 <- ggplot(nR10) +
  geom_line(aes(x = Year, y = est, color = "nsGEV", linetype = "model")) +
  geom_line(aes(x = Year, y = U, color = "nsGEV", linetype = "95% CI")) +
  geom_line(aes(x = Year, y = L, color = "nsGEV", linetype = "95% CI")) +
  geom_line(data = R10, aes(x = Year, y = est, color = "sGEV", linetype = "model")) +
  geom_line(data = R10, aes(x = Year, y = U, color = "sGEV", linetype = "95% CI")) +
  geom_line(data = R10, aes(x = Year, y = L, color = "sGEV", linetype = "95% CI")) +
  labs(title = paste0("Station", i, ": 10-year")) +
  xlab(NULL)+
  ylab(NULL)+
  scale_linetype_manual(values = c(2, 1))

p100 <- ggplot(nR100) +
  geom_line(aes(x = Year, y = est, color = "nsGEV", linetype = "model")) +
  geom_line(aes(x = Year, y = U, color = "nsGEV", linetype = "95% CI")) +
  geom_line(aes(x = Year, y = L, color = "nsGEV", linetype = "95% CI")) +
  geom_line(data = R100, aes(x = Year, y = est, color = "sGEV", linetype = "model")) +
  geom_line(data = R100, aes(x = Year, y = U, color = "sGEV", linetype = "95% CI")) +
  geom_line(data = R100, aes(x = Year, y = L, color = "sGEV", linetype = "95% CI")) +
  labs(title = paste0("Station", i, ": 100-year"), ylab = "Return level") +
  xlab(NULL)+
  ylab(NULL)+
  scale_linetype_manual(values = c(2, 1))

ReturnPlot[[i]] <- list(p2, p10, p100)

est <- GraphData(
  FitList,
  c("linear_fit", "s_fit", "single_fit", "double_fit", "quad_fit", "abrupt_fit", "best_fit")
)
Graph[[i]] <- PlotParameter(
  est[[1]], est[[2]], est[[3]],
  Station, i,
  ignore = c("linear_fit", "s_fit", "single_fit", "double_fit", "quad_fit", "abrupt_fit")
)
print(paste("Station", i))
print(round(FitList[[7]]$mle, 2))
print(FitList[[7]]$formulation)
}

save(Graph, file = "data/Graph.Rdata")























