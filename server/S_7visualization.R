


observe({
  data <- peakData_remove_noise()
  ions <- as.numeric(names(data)[-c(1,2)])
  updateSelectizeInput(session = getDefaultReactiveDomain(), "single_ions_visualization_select", choices = ions,selected = ions[1],server = TRUE)
  updateSelectizeInput(session = getDefaultReactiveDomain(), "multiple_ions_select", choices = ions,selected = c(ions[1],ions[2]),server = TRUE)
})


output$single_ion_visualizaiton <- renderPlot({
  
  data <- peakData_remove_noise()
  
  req(input$single_ions_visualization_select) 
  ion <- input$single_ions_visualization_select
  
  w <- which(names(data) %in% c("X","Y",ion))
  d <- data %>%
    dplyr::select(all_of(w))
  names(d)[3] <- "intensity"
  
  p <- ggplot(d,aes(X,Y,colour=log10(intensity)))+geom_point()+scale_color_distiller(palette = "Spectral")+theme_bw()+coord_equal()
  p
})

correlated_ions <- reactive({
  data <- peakData_remove_noise()
  data[is.na(data)] <- 0.1
  
  ion <- input$single_ions_visualization_select
  
  w <- which(names(data) == ion)
  ion_intensity <- data[,w]
  corr_data <- apply(data[,-1],2,function(x){
    cor(x,ion_intensity)
  })
  corr_data <- corr_data[-(w-1)]
  corr_data <- sort(corr_data)
  corr_data
})


output$positively_correlated_ions <- renderPlot({
  corr_data <- correlated_ions()
  data <- peakData_remove_noise()
  
  k <- tail(corr_data)
  for(i in c(1:length(k))){
    w <- which(names(data) %in% c("X","Y",names(k)[i]))
    td <- data %>%
      dplyr::select(all_of(w))
    
    names(td)[3] <- "intensity"
    td <- td[!is.na(td$intensity),]
    p <- ggplot(td,aes(X,Y))+geom_point(aes(colour = log10(intensity)),size=0.8)+
      scale_color_distiller(palette = "Spectral")+theme_bw()+
      coord_equal()+
      ggtitle(paste("mz:",round(as.numeric(names(k)[i]),4)," ; Cor:",round(k[i],2),sep = ""))+
      theme(plot.title = element_text(size = 12))
    eval(parse(text = paste("p",i," <- p",sep = "")))
  }
  pp <- cowplot::plot_grid(p1,p2,p3,p4,p5,p6, nrow = 2)
  return(pp)
})


output$negatively_correlated_ions <- renderPlot({
  corr_data <- correlated_ions()
  data <- peakData_remove_noise()
  
  k <- head(corr_data)
  for(i in c(1:length(k))){
    w <- which(names(data) %in% c("X","Y",names(k)[i]))
    td <- data %>%
      dplyr::select(all_of(w))
    
    names(td)[3] <- "intensity"
    td <- td[!is.na(td$intensity),]
    p <- ggplot(td,aes(X,Y))+geom_point(aes(colour = log10(intensity)),size=0.8)+
      scale_color_distiller(palette = "Spectral")+theme_bw()+
      coord_equal()+
      ggtitle(paste("mz:",round(as.numeric(names(k)[i]),4)," ; Cor:",round(k[i],2),sep = ""))+
      theme(plot.title = element_text(size = 12))
    eval(parse(text = paste("p",i," <- p",sep = "")))
  }
  pp <- cowplot::plot_grid(p1,p2,p3,p4,p5,p6, nrow = 2)
  return(pp)
})


output$Pseudo_color_plot <- renderPlot({
  data <- peakData_remove_noise()
  data[is.na(data)] <- 0.1
  
  ions <- input$multiple_ions_select
  req(length(ions)>1)
  if(length(ions) == 2){
    w <- which(names(data) %in% c("X","Y",ions))
    data1 <- data %>%
      dplyr::select(all_of(w))
    
    names(data1)[c(3,4)] <- c("mz1","mz2")
    data_scaled <- as.data.frame(apply(data1[,-c(1,2)], 2, function(x) {
      x <- log(x)
      (x - min(x)) / (max(x) - min(x))
    }))
    data1$color <- rgb(data_scaled$mz1, 0,data_scaled$mz2, maxColorValue = 1)
    p <- ggplot(data1, aes(x = X, y = Y)) +
      geom_point(aes(color = color), size = 1) +
      scale_color_identity() +  
      labs(title = "Pseudo plot of two ions", x = "X", y = "Y") +
      theme_minimal()+
      coord_equal()+
      annotate("rect", xmin = 0, xmax = 2, ymin = 0, ymax = 4, 
               fill = "red", color = "black") + 
      annotate("text", x = 3, y = 2, label = ions[1], 
               hjust = 0, vjust = 0.5, size = 3, color = "black") +  

      annotate("rect", xmin = 0, xmax = 2, ymin = 10, ymax = 14, 
               fill = "blue", color = "black") + 
      annotate("text", x = 3, y = 12, label = ions[2], 
               hjust = 0, vjust = 0.5, size = 3, color = "black")  
  }
  
  if(length(ions) == 3){
    w <- which(names(data) %in% c("X","Y",ions))
    data1 <- data %>%
      dplyr::select(all_of(w))
    
    names(data1)[c(3,4,5)] <- c("mz1","mz2","mz3")

    data_scaled <- as.data.frame(apply(data1[,-c(1,2)], 2, function(x) {
      x <- log(x)
      (x - min(x)) / (max(x) - min(x))
    }))

    data1$color <- rgb(data_scaled$mz1, data_scaled$mz2, data_scaled$mz3, maxColorValue = 1)
    p <- ggplot(data1, aes(x = X, y = Y)) +
      geom_point(aes(color = color), size = 1) +
      scale_color_identity() +  
      labs(title = "Pseudo plot of three ions", x = "X", y = "Y") +
      theme_minimal()+
      coord_equal()+

      annotate("rect", xmin = 0, xmax = 2, ymin = 0, ymax = 4, 
               fill = "red", color = "black") +  
      annotate("text", x = 3, y = 2, label = ions[1], 
               hjust = 0, vjust = 0.5, size = 3, color = "black") +  

      annotate("rect", xmin = 0, xmax = 2, ymin = 10, ymax = 14, 
               fill = "blue", color = "black") +  
      annotate("text", x = 3, y = 12, label = ions[2], 
               hjust = 0, vjust = 0.5, size = 3, color = "black")+   
      annotate("rect", xmin = 0, xmax = 2, ymin = 20, ymax = 24, 
             fill = "green", color = "black") +  
      annotate("text", x = 3, y = 22, label = ions[3], 
               hjust = 0, vjust = 0.5, size = 3, color = "black")   
  }
  
  p
  
})


