# creat reactive to store background and tissue selection
background <- reactiveVal(list())  
tissue <- reactiveVal(list())      

# creat reactive to store selection_data
selection_data <- reactiveVal(data.frame(
  Group = character(),
  Region = character(),
  X_Min = numeric(),
  X_Max = numeric(),
  Y_Min = numeric(),
  Y_Max = numeric(),
  Point_Count = integer(),
  stringsAsFactors = FALSE
))

peak <- reactive({
  data <- peakData()
  sam_view_data <- apply(data[,-c(1,2)],1,function(x){
    median_intensity <- sum(x,na.rm = T)
    return(median_intensity)
  })
  md_intensity <- data.frame(intensity=sam_view_data,point=names(sam_view_data))
  md_intensity$x <- as.numeric(lapply(md_intensity$point,function(x){str_split(x,"_")[[1]][1]}))
  md_intensity$y <- as.numeric(lapply(md_intensity$point,function(x){str_split(x,"_")[[1]][2]}))
  md_intensity <- md_intensity[,c(3,4,1)]
  return(md_intensity)
})

# ploty
output$scatter_plot <- renderPlotly({
  data <- peak()
  plot_ly(data, x = ~x, y = ~y, color = ~log10(intensity), colors = colorRamp(c("blue", "red")),
          type = 'scatter', mode = 'markers', source = "scatter",marker = list(size = 5)) %>%
    layout(dragmode = "lasso",xaxis = list(scaleanchor = "y", scaleratio = 1),
           yaxis = list(scaleanchor = "x", scaleratio = 1))
})


selected_points <- reactiveVal()
observe({
  data <- peak()
  selected_data <- event_data("plotly_selected", source = "scatter")
  
  if (is.null(selected_data)) {
    selected_points(NULL)
  } else {
    selected_points(data[selected_data$pointNumber + 1, ])
  }
})

# add selection to background
observeEvent(input$add_to_background, {
  if (!is.null(selected_points())) {
    new_background <- background()
    new_background <- append(new_background, list(selected_points()))
    names(new_background) <- paste0("background_",c(1:length(new_background)))
    background(new_background) 
    showNotification("Selected area has been added to the background.")
  } else {
    showNotification("No points selected.")
  }
})

# add selection to tissue
observeEvent(input$add_to_tissue, {
  if (!is.null(selected_points())) {
    new_tissue <- tissue()
    new_tissue <- append(new_tissue, list(selected_points()))
    names(new_tissue) <- paste0("tissue_",c(1:length(new_tissue)))
    tissue(new_tissue)  
    showNotification("Selected area has been added to the tissue.")
  } else {
    showNotification("No points selected.")
  }
})

output$selection_table <- renderTable({
  tissue <- tissue()
  background <- background()
  validate(
    need((length(tissue)>0) | length(background)>0,"Please choose an area")
  )
  calculate_min_max <- function(df) {
    x_min <- min(df$x)
    x_max <- max(df$x)
    y_min <- min(df$y)
    y_max <- max(df$y)
    point_number <- nrow(df)
    return(c(x_min, x_max,y_min,y_max,point_number))
  }
  tissue_df <- NULL
  t <- length(tissue)
  if(t>0){
    tissue_results <- lapply(tissue, calculate_min_max)
    tissue_df <- as.data.frame(do.call(rbind, tissue_results))
    colnames(tissue_df) <- c("X_min", "X_max","Y_min","Y_max","point_number")
    tissue_df$region_name <- rownames(tissue_df)
  }
  background_df <- NULL
  b <- length(background)
  if(b>0){
    background_results <- lapply(background, calculate_min_max)
    background_df <- as.data.frame(do.call(rbind, background_results))
    colnames(background_df) <- c("X_min", "X_max","Y_min","Y_max","point_number")
    background_df$region_name <- rownames(background_df)
  }
  result <- rbind(tissue_df,background_df)
  result
})

output$selection_plot <- renderPlot({
  data <- peak()
  tissue <- tissue()
  background <- background()
  data$intensity[is.na(data$intensity)] <- 0.1
  base_plot <- ggplot(data, aes(x = x, y = y)) +
    geom_point(aes(color = log10(intensity)), size = 2) +
    scale_color_gradient(low = "blue", high = "red") +
    theme_minimal()+
    theme(
      axis.title.x = element_text(size = 16),   
      axis.title.y = element_text(size = 16),   
      axis.text.x = element_text(size = 14),    
      axis.text.y = element_text(size = 14)     
    )+coord_equal()
  
  if(length(tissue)>0){
    combined_tissue <- do.call(rbind, tissue)
    base_plot <- base_plot +
      geom_point(data = combined_tissue, aes(x = x, y = y), 
                 color = "green", size = 2, alpha = 0.15)
  }
  
  if(length(background)>0){
    combined_background <- do.call(rbind, background)
    base_plot <- base_plot +
      geom_point(data = combined_background, aes(x = x, y = y), 
                 color = "yellow", size = 2, alpha = 0.15) 
  }
  return(base_plot)
})

# empty selection
observeEvent(input$clear_selection, {
  tissue(list())  
  background(list())  
  selection_data(data.frame(
    Group = character(),
    Region = character(),
    X_Min = numeric(),
    X_Max = numeric(),
    Y_Min = numeric(),
    Y_Max = numeric(),
    Point_Count = integer(),
    stringsAsFactors = FALSE
  ))
  
  showNotification("All selection have been cleared")
})


#QC2:difference of the spectrum=================================
tissue_spectrum_reactive <- eventReactive(input$finish_selection, {
  tissue <- tissue()
  data <- peakData()
  validate(
    need(length(tissue)>0,"Please choose an tissue area")
  )
  
  data$mark <- "unlabel"
  for(i in names(tissue)){
    k <- which(row.names(data) %in% row.names(tissue[[i]]))
    data$mark[k] <- i
  }
  
  data <- data[data$mark != "unlabel",]
  spectra_tissue <- data[,-c(1,2)] %>%
    group_by(mark) %>%
    dplyr::summarise(across(everything(), ~ median(., na.rm = TRUE)))
  spectra_tissue <- as.data.frame(spectra_tissue)
  row.names(spectra_tissue) <- spectra_tissue$mark
  spectra_tissue <- spectra_tissue[,-1]
  spectra_tissue <- as.data.frame(t(spectra_tissue))
  spectra_tissue$mz <- row.names(spectra_tissue)
  spectra_tissue <- spectra_tissue %>%
    pivot_longer(
      cols = -mz,               
      names_to = "area",      
      values_to = "median_intensity"         
    )
  spectra_tissue <- spectra_tissue[!is.na(spectra_tissue$median_intensity),]
  p_tissue <- ggplot(spectra_tissue, aes(x = mz, y = median_intensity)) +
    geom_segment( aes(x=mz, xend=mz, y=0, yend=median_intensity)) +
    facet_wrap(area ~ .,ncol = 1) +            
    labs(title = "Tissue_area", x = "mz", y = "median intensity") +
    theme_light() +theme(axis.text.x = element_blank())                
  return(p_tissue)
})

output$tissue_spectrum <- renderPlot({
  tissue_spectrum_reactive()
})


background_spectrum_reactive <- eventReactive(input$finish_selection, {
  background <- background()
  data <- peakData()
  validate(
    need(length(background)>0,"Please choose an background area")
  )
  
  data$mark <- "unlabel"
  for(i in names(background)){
    k <- which(row.names(data) %in% row.names(background[[i]]))
    data$mark[k] <- i
  }
  
  data <- data[data$mark != "unlabel",]
  spectra_tissue <- data[,-c(1,2)] %>%
    group_by(mark) %>%
    dplyr::summarise(across(everything(), ~ median(., na.rm = TRUE)))
  spectra_tissue <- as.data.frame(spectra_tissue)
  row.names(spectra_tissue) <- spectra_tissue$mark
  spectra_tissue <- spectra_tissue[,-1]
  spectra_tissue <- as.data.frame(t(spectra_tissue))
  spectra_tissue$mz <- row.names(spectra_tissue)
  spectra_tissue <- spectra_tissue %>%
    pivot_longer(
      cols = -mz,               
      names_to = "area",      
      values_to = "median_intensity"        
    )
  spectra_tissue <- spectra_tissue[!is.na(spectra_tissue$median_intensity),]
  p_tissue <- ggplot(spectra_tissue, aes(x = mz, y = median_intensity)) +
    geom_segment( aes(x=mz, xend=mz, y=0, yend=median_intensity)) +
    facet_wrap(area ~ .,ncol = 1) +           
    labs(title = "Background_area", x = "mz", y = "median intensity") +
    theme_light() +theme(axis.text.x = element_blank())                   
  return(p_tissue)
})

output$background_spectrum <- renderPlot({
  background_spectrum_reactive()
})

#----------------------------------------------------------------------------------------
#QC3:correlation of background areas=============================
peak_table_background <- eventReactive(input$finish_selection, {
  background <- background()
  data <- peakData()
  validate(
    need(length(background)>0,"Please choose an background area")
  )
  
  data$mark <- "unlabel"
  for(i in names(background)){
    k <- which(row.names(data) %in% row.names(background[[i]]))
    data$mark[k] <- i
  }
  
  data <- data[data$mark != "unlabel",]
  spectrum_data <- NULL
  for(i in unique(data$mark)){
    kd <- data[data$mark == i,]
    spectra <- apply(kd[,-c(1,2,ncol(kd),ncol(kd)-1)],2,function(x){median(x,na.rm = T)})
    spectrum <- data.frame(mz=names(spectra),intensity=spectra,region=i)
    spectrum_data <- rbind(spectrum_data,spectrum)
  }
  return(spectrum_data)
  
})

output$background_correlation <- renderPlot({
  spectrum_data <- peak_table_background()
  spectrum_data2 <- reshape2::dcast(spectrum_data,mz ~ region,value.var = "intensity")
  row.names(spectrum_data2) <- spectrum_data2$mz
  spectrum_data2 <- spectrum_data2[,-1]
  na <- apply(spectrum_data2,1,function(x){sum(is.na(x)/ncol(spectrum_data2))})
  na <- na[na ==1]
  if(length(na)>0){
    na_col <- which(row.names(spectrum_data2) %in% names(na))
    spectrum_data2 <- spectrum_data2[-na_col,]
  }
  spectrum_data2[is.na(spectrum_data2)] <- 0.1
  back_cor <- cor(spectrum_data2)
  pheatmap(back_cor)
})

output$background_boxplot <- renderPlot({
  spectrum_data <- peak_table_background()
  ggplot(spectrum_data,aes(region,log10(intensity)))+geom_boxplot()+theme_bw()
})

#--------------------------------------------------------------------------
#QC4:the ratios of ions higher in tissue areas====================
volcano_table <- eventReactive(input$finish_selection, {
  background <- background()
  tissue <- tissue()
  data <- peakData()
  validate(
    need((length(tissue)>0) & length(background)>0,"Please choose tissue and background area")
  )
  data$mark <- "unlabel"
  data$group <- "unlabel"
  for(i in names(background)){
    k <- which(row.names(data) %in% row.names(background[[i]]))
    data$mark[k] <- i
    data$group[k] <- "background"
  }
  for(j in names(tissue)){
    v <- which(row.names(data) %in% row.names(tissue[[j]]))
    data$mark[v] <- j
    data$group[v] <- "tissue"
  }
  
  data <- data[data$mark != "unlabel",]
  spectrum_data <- NULL
  for(i in unique(data$mark)){
    kd <- data[data$mark == i,]
    spectra <- apply(kd[,-c(1,2,ncol(kd),(ncol(kd)-1))],2,function(x){median(x,na.rm = T)})
    spectrum <- data.frame(mz=names(spectra),intensity=spectra,region=i,group=kd$group[1])
    spectrum_data <- rbind(spectrum_data,spectrum)
  }
  
  spectrum_data3 <- reshape2::dcast(spectrum_data,region+group ~ mz,value.var = "intensity")
  na_cal <- apply(spectrum_data3,2,function(x){sum(is.na(x))/nrow(spectrum_data3)})
  all_miss <- which(names(spectrum_data3) %in% names(na_cal[na_cal == 1]))
  if(length(all_miss)>0){
    spectrum_data3 <- spectrum_data3[,-all_miss]
  }
  spectrum_data3[is.na(spectrum_data3)] <- runif(sum(is.na(spectrum_data3)), min = 0, max = 1)
  b <- which(spectrum_data3$group == "background")
  t <- which(spectrum_data3$group == "tissue")
  vol <- apply(spectrum_data3[,-c(1,2)],2,function(x){
    #x <- spectrum_data3[,3]
    back_mean <- median(x[b])
    tissue_mean <- median(x[t])
    fd <- tissue_mean/back_mean 
    p <- t.test(log2(x[b]),log2(x[t]))
    k <- c(back_mean,tissue_mean,fd,p$p.value)
    names(k) <- c("back_mean","tissue_mean","fold_change","p")
    return(k)
  })
  
  vol <- as.data.frame(t(vol))
  vol$mz <- row.names(vol)
  vol$Status <- ifelse(vol$fold_change > 1, "higher in tissue", "higher in background")
  return(vol)
})

#volcano plot
output$volcano_background_vs_tissue <- renderPlot({
  vol <- volcano_table()
  k <- ggplot(vol,aes(log2(fold_change),-log10(p),colour = Status))+geom_point()+theme_bw()+
    ggtitle("tissue region / background region")
  return(k)
})
#table output
output$number_background_vs_tissue <- renderTable({
  vol <- volcano_table()
  status <- table(vol$Status)
  ratio <- status[2]/(status[1]+status[2])
  k <- data.frame(
    higer_in_tissue_peaks=status[2],
    higher_in_background_peaks=status[1],
    higher_in_tissue_percent=status[2]/(status[1]+status[2])
  )
  return(k)
})
#Process2:remove ions higher in background=================
peakData_remove_background_feature <- reactive({
  data <- peakData()
  vol <- volcano_table()
  mz_high_in_background <-vol$mz[vol$fold_change<1]
  mz_remove <- which(names(data) %in% mz_high_in_background)
  data <- data[,-mz_remove]
  return(data)
})
#sum intensity plot with ions higer in tissue
output$total_intensity_before_mz_remove <- renderPlot({
  data <- peakData()
  loc <- locs()
  sam_view_data <- apply(data[,-c(1,2)],1,function(x){
    sum_intensity <- sum(x,na.rm = T)
    return(sum_intensity)
  })
  md_intensity <- data.frame(intensity=sam_view_data,point=names(sam_view_data))
  md_intensity$X <- as.numeric(lapply(md_intensity$point,function(x){str_split(x,"_")[[1]][1]}))
  md_intensity$Y <- as.numeric(lapply(md_intensity$point,function(x){str_split(x,"_")[[1]][2]}))
  md_plot <-  ggplot(md_intensity,aes(X,Y,colour =intensity))+geom_point()+
    scale_color_distiller(palette = "Spectral")+
    theme_bw()+coord_equal()
  return(md_plot)
  
})
#intensity distribution of the sum intensity plot---------------
output$total_intensity_after_mz_remove <- renderPlot({
  data <- peakData_remove_background_feature()
  loc <- locs()
  sam_view_data <- apply(data[,-c(1,2)],1,function(x){
    sum_intensity <- sum(x,na.rm = T)
    return(sum_intensity)
  })
  md_intensity <- data.frame(intensity=sam_view_data,point=names(sam_view_data))
  md_intensity$X <- as.numeric(lapply(md_intensity$point,function(x){str_split(x,"_")[[1]][1]}))
  md_intensity$Y <- as.numeric(lapply(md_intensity$point,function(x){str_split(x,"_")[[1]][2]}))
  md_plot <-  ggplot(md_intensity,aes(X,Y,colour =intensity))+geom_point()+
    scale_color_distiller(palette = "Spectral")+
    theme_bw()+coord_equal()
  return(md_plot)
})


#Process3:remove background piexls==================================
remove_background_mz_intensity <- reactive({
  data <- peakData_remove_background_feature()
  sam_view_data <- apply(data[,-c(1,2)],1,function(x){
    sum_intensity <- sum(x,na.rm = T)
    return(sum_intensity)
  })
  md_intensity <- data.frame(intensity=sam_view_data,point=names(sam_view_data))
  return(md_intensity)
})

observe({
  K <- remove_background_mz_intensity()
  updateSliderInput(session = getDefaultReactiveDomain(), "datapoint_cutoff",
                    min = round(log10(min(K$intensity)),2), 
                    max = round(log10(max(K$intensity)),2), 
                    value = log10(median(K$intensity)))
})

#total intensity intensity his plot-----------------
output$total_intensity_hist <- renderPlot({
  md_intensity <- remove_background_mz_intensity()
  cutoff <- 10^input$datapoint_cutoff
  intensity_distribution <- ggplot(md_intensity,aes(log10(intensity)))+geom_histogram()+
    geom_vline(xintercept = log10(cutoff), color = "red", linetype = "dashed", size = 1)+theme_bw()
  return(intensity_distribution)
})

#intensity plot after remove backgrounds---------
md_intensity <- reactive({
  data <- peakData_remove_background_feature()
  cutoff <- 10^input$datapoint_cutoff
  #print(cutoff)
  loc <- locs()
  sam_view_data <- apply(data[,-c(1,2)],1,function(x){
    sum_intensity <- sum(x,na.rm = T)
    return(sum_intensity)
  })
  md_intensity <- data.frame(intensity=sam_view_data,point=names(sam_view_data))
  md_intensity$X <- as.numeric(lapply(md_intensity$point,function(x){str_split(x,"_")[[1]][1]}))
  md_intensity$Y <- as.numeric(lapply(md_intensity$point,function(x){str_split(x,"_")[[1]][2]}))
  
  md_intensity$intensity_mark <- "background"
  md_intensity$intensity_mark[md_intensity$intensity > cutoff] <- "tissue"
  return(md_intensity)
})

output$bakcground_tissue_classify <- renderPlot({
  md_intensity <- md_intensity()
  loc <- locs()
  layer2 <- ggplot(md_intensity,aes(X,Y))+geom_point(aes(colour = intensity_mark))+
    theme_bw()+coord_equal()
  return(layer2)
})

#peak data after background removal
peakData_remove_background_points <- reactive({
  data <- peakData()
  req(ncol(data)>0)
  md_intensity <- md_intensity()
  md_intensity <- md_intensity[md_intensity$intensity_mark == "tissue",]
  remain <- which(row.names(data) %in% md_intensity$point)
  data <- data[remain,]
  return(data)
})


