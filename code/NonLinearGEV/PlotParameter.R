PlotParameter <- function(est_mu, est_sig, est_sh, Region, n = 1, ignore = NULL) {
  if (is_empty(ignore)) {
    ignore <- 0
  }
  p1 <- est_mu %>%
    filter(!Group %in% ignore) %>%
    ggplot(aes(x = t, y = est), alpha = I(0.7)) +
    geom_line(aes(color = "Orange", size = I(1)), alpha = I(0.7)) +
    geom_line(aes(x = t, y = U, color = "Orange"), linetype = 2, size = I(1), alpha = I(0.7)) +
    geom_line(aes(x = t, y = L, color = "Orange"), linetype = 2, size = I(1), alpha = I(0.7)) +
  #  geom_point(data = MOV, aes(x = t, y = location, color = "window", size = I(2)), alpha = I(0.7)) +
  #  geom_point(data = Region, aes(x = DecYear - 1980, y = V1, size = I(2)), alpha = I(0.7)) +
    xlab(paste0("Region", n,"  ", "t")) +
    ylab(expression(mu(t))) +
    scale_colour_manual(values = c("Orange" = "Orange",
                                   "Green" = "Green",
                                   "Blue" = "Blue"))+
    theme(legend.position="none")
  
  p2 <- est_sig %>%
    filter(!Group %in% ignore) %>%
    ggplot(aes(x = t, y = est), alpha = I(0.7)) +
    geom_line(aes(color = "Green", size = I(1)), alpha = I(0.7)) +
    geom_line(aes(x = t, y = U, color = "Green"), linetype = 2, size = I(1), alpha = I(0.7)) +
    geom_line(aes(x = t, y = L, color = "Green"), linetype = 2, size = I(1), alpha = I(0.7)) +
  #  geom_point(data = MOV, aes(x = t, y = scale, color = "window", size = I(2)), alpha = I(0.7)) +
    xlab(paste0("Region", n,"  ", "t")) +
    ylab(expression(log(sigma(t))))+
    scale_colour_manual(values = c("Orange" = "Orange",
                                   "Green" = "Green",
                                   "Blue" = "Blue"))+
    theme(legend.position="none")
  
  p3 <- est_sh %>%
    filter(!Group %in% ignore) %>%
    ggplot(aes(x = t, y = est), alpha = I(0.7)) +
    geom_line(aes(color = "Blue", size = I(1)), alpha = I(0.7)) +
    geom_line(aes(x = t, y = U, color = "Blue"), linetype = 2, size = I(1), alpha = I(0.7)) +
    geom_line(aes(x = t, y = L, color = "Blue"), linetype = 2, size = I(1), alpha = I(0.7)) +
  #  geom_point(data = MOV, aes(x = t, y = shape, color = "window", size = I(2)), alpha = I(0.7)) +
    xlab(paste0("Region", n,"  ", "t")) +
    ylab(expression(xi(t)))+
    scale_colour_manual(values = c("Orange" = "Orange",
                                   "Green" = "Green",
                                   "Blue" = "Blue"))+
    theme(legend.position="none")
  
  p <- list(p1, p2, p3)
  #  p <- grid.arrange(p1,p2,p3,nrow=3)
  #  ggsave(paste0("Region",n,".png"),p,path="F:/Streamflow/Plot/Fit",width=6,height=6)
  return(p)
}