
#QC4：Noise ratio==========================================

#calculate noise score for each ion-------------------------------
noise_detection <- eventReactive(input$noise_calculate, {
  sp4 <- peakData_remove_background_points()
  x1 <- min(sp4$X)
  x2 <- max(sp4$X)
  y1 <- min(sp4$Y)
  y2 <- max(sp4$Y)
  locations <- sp4[,c(1,2)]
  all_loc <- data.frame(X=sort(rep(c(x1:x2),(y2-y1+1))),
                        Y=rep(c(y1:y2),(x2-x1+1)))
  row.names(all_loc) <- paste(all_loc$X,all_loc$Y,sep = "_")
  all_loc$mark <- "background"
  all_loc$mark[row.names(all_loc) %in% row.names(locations)] <- "tissue"
  background_loc <- all_loc[all_loc$mark == "background",]
  n <- nrow(background_loc)
  pattern_test <- apply(sp4[,-c(1,2)],2,function(x){
    mx <- median(x,na.rm = T)
    x[x<mx] <- NA
    value_loc <- locations[-which(is.na(x)),]
    ratio_to_remove <- sum(is.na(x))/length(x)
    if(ratio_to_remove>0){
      n_to_remove <- ceiling(ratio_to_remove * n)
      rows_to_remove <- sample(1:n, size = n_to_remove)
      background_reduced <- background_loc[-rows_to_remove, -3]
      value_loc <- rbind(value_loc,background_reduced)
    }
    mypattern <- ppp(value_loc[,1], value_loc[,2], c(x1,x2), c(y1,y2))
    k <- quadrat.test(mypattern, alternative = "two.sided")
    return(k$p.value)
  })
  return(pattern_test)
})

#distribution of noise score------------------------
output$noise_distribution <- renderPlot({
  noise_score <- noise_detection()
  noise_data <- data.frame(noise_score = noise_score)
  
  np <- ggplot(noise_data, aes(x = -log10(noise_score))) +
    geom_histogram() +
    labs(title = "Noise Distribution", x = "Noise score", y = "Frequency") +
    theme_bw()
  return(np)
})

#Noise ion ratio----------------------
output$noise_ratio <- renderTable({
  noise_score <- noise_detection()
  noise_cutoff <- input$noise_cutoff
  
  expr <- paste0("1e", -1*noise_cutoff)
  noise_cutoff2 <- eval(parse(text = expr))
  total_mz_number <- length(noise_score)
  noise_mz_number <- sum(noise_score>noise_cutoff2)
  noise_mz_ratio <- noise_mz_number/total_mz_number
  data.frame(total_mz_number=total_mz_number,
             noise_mz_number=noise_mz_number,
             noise_mz_ratio=noise_mz_ratio)
})

#peak table after noise removal-----------------------------
peakData_remove_noise <- reactive({
  data <- peakData_remove_background_points()
  noise_score <- noise_detection()
  cutoff <- input$noise_cutoff
  
  expr <- paste0("1e", -1*cutoff)
  noise_cutoff2 <- eval(parse(text = expr))
  nose_mz <- names(noise_score)[which(noise_score>noise_cutoff2)]
  data_clean <- data[,!(names(data) %in% nose_mz)]
  return(data_clean)
})


#peak image of ions near cutoff--------------
output$noise_score_selected <- renderPlot({
  data <- peakData_remove_background_points()
  noise_score <- noise_detection()
  cutoff <- input$noise_cutoff
  
  expr <- paste0("1e", -1*cutoff)
  cutoff2 <- eval(parse(text = expr))
  
  
  
  differences <- abs(noise_score-cutoff2)
  closest_indices <- order(differences)[1:10]
  target_mz <- noise_score[closest_indices]
  
  for(i in c(1:10)){
    single_mz <- names(target_mz[i])
    td <- data[,c(1,2,which(names(data) == single_mz))]
    names(td)[3] <- "intensity"
    td <- td[!is.na(td$intensity),]
    p <- ggplot(td,aes(X,Y))+geom_point(aes(colour = log10(intensity)),size=0.8)+
      scale_color_distiller(palette = "Spectral")+theme_bw()+
      coord_equal()+
      ggtitle(single_mz)
    eval(parse(text = paste("p",i," <- p",sep = "")))
  }
  pp <- cowplot::plot_grid(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10, nrow = 2)
  return(pp)
  
})

#----------------------------------------------------------------------------------------

#QC5:intensity================================================
output$total_intensity_after_pioints_remove <- renderPlot({
  data <- peakData_remove_noise()
  loc <- locs()
  sam_view_data <- apply(data[,-c(1,2)],1,function(x){
    sum_intensity <- sum(x,na.rm = T)
    return(sum_intensity)
  })
  md_intensity <- data.frame(intensity=sam_view_data,point=names(sam_view_data))
  md_intensity$X <- as.numeric(lapply(md_intensity$point,function(x){str_split(x,"_")[[1]][1]}))
  md_intensity$Y <- as.numeric(lapply(md_intensity$point,function(x){str_split(x,"_")[[1]][2]}))
  md_plot <-  ggplot(md_intensity,aes(X,Y,colour =log10(intensity)))+geom_point()+
    scale_color_distiller(palette = "Spectral")+coord_equal()+
    theme_bw()
  return(md_plot)
})

mz_view_data <- reactive({
  data <- peakData_remove_noise()
  spotNb <- nrow(data)
  mz_view_data <- apply(data[,-c(1,2)],2,function(x){
    non_miss_nb <- sum(!is.na(x))
    miss_ratio <- 1-non_miss_nb/spotNb
    median_intensity <- median(x,na.rm = T)
    mz_wise <- c(non_miss_nb,miss_ratio,median_intensity)
    names(mz_wise) <- c("non_miss_nb","miss_ratio","median_intensity")
    return(mz_wise)
  })
  mz_view_data <- as.data.frame(t(mz_view_data))
  mz_view_data$mz <- row.names(mz_view_data)
  return(mz_view_data)
})

output$mz_intensity_hist <- renderPlot({
  mz_view_data <- mz_view_data()
  intensity_distribution <- ggplot(mz_view_data,aes(log10(median_intensity)))+geom_histogram()+theme_bw()
  return(intensity_distribution)
})

output$mz_intensity_spectra <- renderPlot({
  mz_view_data <- mz_view_data()
  mz_view_data$mz <- as.numeric(mz_view_data$mz)
  mz_range <- range(mz_view_data$mz)  
  mz_breaks <- seq(from = floor(mz_range[1] / 100) * 100,  
                   to = ceiling(mz_range[2] / 100) * 100,  
                   by = 100)
  intensity_range <- ggplot(mz_view_data,aes(x=mz,y=median_intensity))+
    geom_segment( aes(x=mz, xend=mz, y=0, yend=median_intensity))+theme_bw()+
    scale_x_continuous(breaks = mz_breaks) 
  return(intensity_range)
})

#--------------------------------------------------------


#QC6：missing value distribution========
#mz missing value
output$mz_missing_hist <- renderPlot({
  data <- peakData_remove_noise()
  spotNb <- nrow(data)
  mz_view_data <- mz_view_data()
  miss_distribution <- ggplot(mz_view_data,aes(miss_ratio))+geom_histogram(binwidth = 0.01)+theme_bw()
  return(miss_distribution)
})

output$mz_missing_distribution <- renderPlot({
  data <- peakData_remove_noise()
  spotNb <- nrow(data)
  mz_view_data <- mz_view_data()
  mz_view_data$mz <- as.numeric(mz_view_data$mz)
  mz_range <- range(mz_view_data$mz)  
  mz_breaks <- seq(from = floor(mz_range[1] / 100) * 100,  
                   to = ceiling(mz_range[2] / 100) * 100,  
                   by = 100)
  miss_distribution <- ggplot(mz_view_data,aes(mz,miss_ratio))+geom_point()+theme_bw()+scale_x_continuous(breaks = mz_breaks)
  return(miss_distribution)
})
#sample missing value--------------
output$sample_missing_distribution <- renderPlot({
  data <- peakData_remove_noise()
  loc <- locs()
  mzNb <- ncol(data)-2
  sam_view_data <- apply(data[,-c(1,2)],1,function(x){
    non_miss_nb <- sum(!is.na(x))
    miss_ratio <- 1-non_miss_nb/mzNb
    return(miss_ratio)
  })
  md_intensity <- data.frame(miss_ratio=sam_view_data,point=names(sam_view_data))
  md_intensity$X <- as.numeric(lapply(md_intensity$point,function(x){str_split(x,"_")[[1]][1]}))
  md_intensity$Y <- as.numeric(lapply(md_intensity$point,function(x){str_split(x,"_")[[1]][2]}))
  miss_range <- ggplot(md_intensity,aes(x=X,y=Y,colour=miss_ratio))+geom_point()+
    scale_color_distiller(palette = "Spectral")+theme_bw()+coord_equal()
  return(miss_range)
})

