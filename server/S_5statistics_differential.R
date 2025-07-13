

group <- reactiveVal(list())     

diff_selection_data <- reactiveVal(data.frame(
  Group = character(),
  Region = character(),
  X_Min = numeric(),
  X_Max = numeric(),
  Y_Min = numeric(),
  Y_Max = numeric(),
  Point_Count = integer(),
  stringsAsFactors = FALSE
))

deal_measure_data <- reactive({
  data <- peakData_remove_noise()
  data <- replace(data, data == 0, NA)
  names(data)[c(1,2)] <- c("x","y")
  return(data)
})


diff_peak <- reactive({
  data <- deal_measure_data()
  sam_view_data <- apply(data[,-c(1,2)],1,function(x){
    median_intensity <- sum(x,na.rm = T)
    return(median_intensity)
  })
  md_intensity <- data.frame(x=data$x,y=data$y,intensity=sam_view_data)
  return(md_intensity)
})


output$diff_scatter_plot <- renderPlotly({
  data <- diff_peak()
  xmin <- min(data$x)-5
  xmax <- max(data$x)
  ymin <- min(data$y)-5
  ymax <- max(data$y)
  plot_ly(data, x = ~x, y = ~y, color = ~log10(intensity), 
          colors = colorRamp(c("blue", "red")),
          type = 'scatter', mode = 'markers', source = "scatter",marker = list(size = 5)) %>%
    layout(dragmode = "lasso",
           height = 300,
           xaxis = list(range = c(xmin, xmax), scaleanchor = "y", scaleratio = 1),
           yaxis = list(range = c(ymin, ymax)))
})


diff_selected_points <- reactiveVal()
observe({
  data <- diff_peak()
  selected_data <- event_data("plotly_selected", source = "scatter")
  
  if (is.null(selected_data)) {
    diff_selected_points(NULL)
  } else {
    diff_selected_points(data[selected_data$pointNumber + 1, ])
  }
})


observeEvent(input$add_to_group, {
  if (!is.null(diff_selected_points())) {
    new_group <- group()
    new_group <- append(new_group, list(diff_selected_points()))
    names(new_group) <- paste0("group_",c(1:length(new_group)))
    group(new_group)  
    showNotification("Selected area has been added.")
  } else {
    showNotification("No points selected.")
  }
})

output$diff_selection_table <- renderTable({
  group <- group()
  validate(
    need(length(group)>0,"Please choose an area")
  )
  calculate_min_max <- function(df) {
    x_min <- min(df$x)
    x_max <- max(df$x)
    y_min <- min(df$y)
    y_max <- max(df$y)
    point_number <- nrow(df)
    return(c(x_min, x_max,y_min,y_max,point_number))
  }
  group_df <- NULL
  group_results <- lapply(group, calculate_min_max)
  group_df <- as.data.frame(do.call(rbind, group_results))
  colnames(group_df) <- c("X_min", "X_max","Y_min","Y_max","point_number")
  group_df$region_name <- rownames(group_df)
  group_df
})

output$diff_selection_plot <- renderPlot({
  data <- diff_peak()
  group <- group()
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
  
  if(length(group)>0){
    combined_group <- do.call(rbind, group)
    base_plot <- base_plot +
      geom_point(data = combined_group, aes(x = x, y = y), 
                 color = "green", size = 2, alpha = 0.5)
    for(i in c(1:length(group))){
        name <- names(group)[i]  
        d <- group[[i]]
        lx <- mean(d$x)
        ly <- mean(d$y)
        base_plot <- base_plot+
          annotate("text", x = lx, y = ly, label = name, vjust = -1)
    }
  }
  return(base_plot)
})


observeEvent(input$diff_clear_selection, {
  group(list())  
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
  showNotification("All selections have been cleared")
})

#--------------------------------------------------------------------------------------------------

observeEvent(input$diff_finish_selection, {
  group <- group()
  ch <- names(group)
  updateSelectInput(session = getDefaultReactiveDomain(), "case_group", choices = ch)
  updateSelectInput(session = getDefaultReactiveDomain(), "control_group", choices = ch)
})



diff_data <- reactive({
  group <- group()
  data_rds<-data_rds()
  case <- input$case_group
  control <- input$control_group
  
  validate(
    need((length(case)>0) & length(control)>0 & length(group)>0,"Please choose case and control area")
  )
  data_rds@meta.data$mark <- "unlabel"
  data_rds@meta.data$group <- "unlabel"
  data_rds@meta.data$x_y<-paste0(data_rds@meta.data$x,"_",data_rds@meta.data$y)
  for(i in names(group)){
    if(i %in% case){
      k <- which(data_rds@meta.data$x_y %in% row.names(group[[i]]))
      data_rds@meta.data$mark[k] <- i
      data_rds@meta.data$group[k] <- "case"
    }
    if(i %in% control){
      k <- which(data_rds@meta.data$x_y %in% row.names(group[[i]]))
      data_rds@meta.data$mark[k] <- i
      data_rds@meta.data$group[k] <- "control"
    }
  }
  source("./source/find_marker.R")
  vol <- find_marker(data=data_rds,group=c("case","control"))
  return(vol)
})

diff_volcano_table <- reactive({
  diff_data<-diff_data()
  req(diff_data)
  FC_Threshold <- input$Fold_change_maunal
  pvalue <- input$p_value_adjust_manula
  source("./source/find_marker.R")
  vol <- add_State(markers=diff_data,FC_Threshold, pvalue)
  return(vol)
})

output$diff_volcano <- renderPlot({
  source("./source/volcano_plot_processing.R")
  volcano_data <- diff_volcano_table()

  up_count <- sum(volcano_data$State == "Up", na.rm = TRUE)
  down_count <- sum(volcano_data$State == "Down", na.rm = TRUE)
  FC_Threshold <- input$Fold_change_maunal
  pvalue <- input$p_value_adjust_manula
  k<-volcano_plot_processing(data=volcano_data,pvalue,FC_Threshold) +
    ggtitle(paste("Up:",up_count,"Down:",down_count))
  k
})

output$group_setting_maual <- renderPlot({
  group <- group()
  data <- deal_measure_data()
  case <- input$case_group
  control <- input$control_group

  validate(
    need((length(case)>0) & length(control)>0,"Please choose case and control area")
  )
  data <- data[,c(1,2)]
  data$group <- "blank"
  for(i in names(group)){
    if(i %in% case){
      k <- which(row.names(data) %in% row.names(group[[i]]))
      data$group[k] <- "case"
    }
    if(i %in% control){
      k <- which(row.names(data) %in% row.names(group[[i]]))
      data$group[k] <- "control"
    }
  }

  p <- ggplot(data,aes(x,y,colour=group))+
    geom_point()+
    scale_color_manual(values = c("case" = "red", "control" = "blue")) +
    theme_bw()+coord_equal()
  p
})

observe({
  vol <- diff_volcano_table()
  FC_Threshold <- input$Fold_change_maunal
  pvalue <- input$p_value_adjust_manula
  chs <- vol$mz[vol$State =="Up" | vol$State =="Down"]
  updateSelectizeInput(session = getDefaultReactiveDomain(), "diff_ion_select_manual", choices = chs,server = TRUE)
})


output$diff_ion_plot_maunal <- renderPlot({
  data <- deal_measure_data()
  names(data)[c(1,2)] <- c("X","Y")
  data$X <- as.numeric(data$X)
  data$Y <- as.numeric(data$Y)
  
  req(input$diff_ion_select_manual)
  ion <- input$diff_ion_select_manual
  w <- which(names(data) %in% c("X","Y",ion))
  d <- data %>%
    dplyr::select(all_of(w))
  names(d)[3] <- "intensity"
  
  p <- ggplot(d,aes(X,Y,colour=log10(intensity)))+
         geom_point()+
         scale_color_distiller(palette = "Spectral")+
         theme_bw()+
         coord_equal()
  p
})


merged_data_manual <- reactive({
  DemoData<-diff_volcano_table()
  identi <- identi_table()
  DemoData <- DemoData[,c(1,5,8,9)]
  identi <- identi[,c(1,10,11,12,13,14,15)]

  DemoData$mz <- as.character(DemoData$mz)
  identi$mz <- as.character(identi$mz)
  merged_data <- left_join(DemoData, identi, by = "mz")
  merged_data
})
output$download_volcano_data_manual <- downloadHandler(
  filename = function() {
    paste0("Metabolite_diff_data_manual_selection.txt")
  },
  content = function(file) {
    DemoData2 <- merged_data_manual()
    write_delim(DemoData2,file,delim="\t")
  })

