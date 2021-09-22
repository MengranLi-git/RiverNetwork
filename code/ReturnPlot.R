load("data/Graph.Rdata")

plot_final <- list()
for (i in 1:6) {
  p <- ReturnPlot[[i]]
  plot_final[[3 * (i - 1) + 1]] <- p[[1]]
  plot_final[[3 * (i - 1) + 2]] <- p[[2]]
  plot_final[[3 * (i - 1) + 3]] <- p[[3]]
}
plot_final[["ncol"]] <- 3
plot_final[["nrow"]] <- 6
plot_final[["legend"]] <- FALSE
# plot_final[["common.legend"]] <- TRUE

# save(plot_final,file = "best.Rdata")
do.call(ggarrange, plot_final)

ggsave("ReturnLevel1.png", plot = last_plot(), width = 6, height = 8)

plot_final <- list()
for (i in 7:12) {
  p <- ReturnPlot[[i]]
  plot_final[[3 * (i - 7) + 1]] <- p[[1]]
  plot_final[[3 * (i - 7) + 2]] <- p[[2]]
  plot_final[[3 * (i - 7) + 3]] <- p[[3]]
}
plot_final[["ncol"]] <- 3
plot_final[["nrow"]] <- 6
plot_final[["legend"]] <- FALSE
# plot_final[["common.legend"]] <- TRUE

# save(plot_final,file = "best.Rdata")
do.call(ggarrange, plot_final)

ggsave("ReturnLevel2.png", plot = last_plot(), width = 6, height = 8)

plot_final <- list()
for (i in 13:18) {
  p <- ReturnPlot[[i]]
  plot_final[[3 * (i - 13) + 1]] <- p[[1]]
  plot_final[[3 * (i - 13) + 2]] <- p[[2]]
  plot_final[[3 * (i - 13) + 3]] <- p[[3]]
}
plot_final[["ncol"]] <- 3
plot_final[["nrow"]] <- 6
plot_final[["legend"]] <- FALSE
# plot_final[["common.legend"]] <- TRUE

# save(plot_final,file = "best.Rdata")
do.call(ggarrange, plot_final)

ggsave("ReturnLevel3.png", plot = last_plot(), width = 6, height = 8)


plot_final <- list()
for (i in 19:24) {
  p <- ReturnPlot[[i]]
  plot_final[[3 * (i - 19) + 1]] <- p[[1]]
  plot_final[[3 * (i - 19) + 2]] <- p[[2]]
  plot_final[[3 * (i - 19) + 3]] <- p[[3]]
}
plot_final[["ncol"]] <- 3
plot_final[["nrow"]] <- 6
plot_final[["legend"]] <- FALSE
# plot_final[["common.legend"]] <- TRUE

# save(plot_final,file = "best.Rdata")
do.call(ggarrange, plot_final)

ggsave("ReturnLevel4.png", plot = last_plot(), width = 6, height = 8)