PlotPattern_c<-function (pattern, location, max.cutoff = 0.9, pt.size = 1, alpha.min = 0.1) 
{
  if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
    install.packages("RColorBrewer")
  }
  colnames(location) <- c("x", "y")
  npattern <- dim(pattern$pattern)[1]
  plist <- list()
  for (i in 1:npattern) {
    feature = pattern$pattern[i, ]
    max.use <- quantile(feature, max.cutoff)
    feature[feature > max.use] <- max.use
    alpha = (feature - min(feature))/(max(feature) - min(feature)) * 
      (1 - alpha.min) + alpha.min
    tmp <- as.data.frame(cbind(location, exp = feature, alpha = alpha))
    p1 <- ggplot(tmp, aes(x = x, y = y, col = exp, alpha = alpha)) + 
      geom_point(size = pt.size) + scale_y_reverse() + 
      scale_color_gradientn(colours = rev(RColorBrewer::brewer.pal(n = 10, 
                                                                   name = "RdYlBu"))) + xlab("") + ylab("") +
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.x=element_blank(),
            axis.ticks.y=element_blank()) +
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank(),
            plot.title = element_text(hjust = 0.5))+
      guides(color = "none", alpha = "none") + ggtitle(paste0("Pattern", 
                                                              i))+
      coord_equal()
    plist[[i]] <- p1
  }
  patchwork::wrap_plots(plist)
}
