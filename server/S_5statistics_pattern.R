



data_long_pattern <- reactive({
  deal_measure_data <- peakData_remove_noise()
  deal_measure_data[is.na(deal_measure_data)] <- 0
  data_long_pattern <- deal_measure_data %>%
    pivot_longer(
      cols = -c(X, Y),
      names_to = "mz",
      values_to = "intensity"
    )
  names(data_long_pattern)[c(1,2)] <- c("x", "y")
  return(data_long_pattern)
})


data_rds <- reactive({
  data<-data_long_pattern()
  source("./source/runxy.R")
  withProgress(message = "Processing data...",value=0.8,{
    data_rds<-runxy(data)
  })
  return(data_rds)

})


find_spatial_pattern<- reactive({
    data_rds_pattern<-data_rds()
    set.seed(123)
    spatialpattern<-function(rds){
      expr=rds@assays$SCT$counts
      location=data.frame(x=rds@meta.data$x,y=rds@meta.data$y)
      rownames(location)<-paste0("sample:",location$x,"_",location$y)
      k=SpaGene(expr,location)
      parttern=FindPattern(k)
      data<-list(parttern,location,k$spagene_res)
      return(data)
    }
    find_spatial_pattern<-spatialpattern(rds=data_rds_pattern)
    find_spatial_pattern
})



output$spatial_pattern_plot <- renderPlot({
  find_spatial_pattern <- find_spatial_pattern()
  source("./source/spatialpattern.R")
  spatial_pattern_plot_save<-PlotPattern_c(find_spatial_pattern[[1]],find_spatial_pattern[[2]])

  return(spatial_pattern_plot_save)
})


spatial_pattern_all<- reactive({
    pattern_cutoff <- input$pattern_correlation_cutoff

    find_spatial_pattern<-find_spatial_pattern()
    spatialpatternall<-function(parttern){
      all <- apply(parttern$genepattern, 2, function(x) names(x)[order(x, decreasing = TRUE)])
      genepattern_all <-parttern$genepattern
      return(genepattern_all)
    }
    spatial_pattern_all<-spatialpatternall(parttern=find_spatial_pattern[[1]])
    max_cols <- apply(spatial_pattern_all, 1, function(x) {
      max_val <- max(x)          
      if (max_val > pattern_cutoff) {       
        colnames(spatial_pattern_all)[which.max(x)]  
      } else {
        "unclassified"           
      }
    })
    pattern_class <- data.frame(
      mz = rownames(spatial_pattern_all),
      class = max_cols,
      stringsAsFactors = FALSE
    )
    return(pattern_class)
})

output$pattern_metabolite_count <- renderPlot({
    pattern_class <- spatial_pattern_all()
    pattern_count <- table(pattern_class$class)
    pattern_count_df <- as.data.frame(pattern_count)
    colnames(pattern_count_df) <- c("Pattern", "Count")

    p <- ggplot(pattern_count_df, aes(x = Pattern, y = Count)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      theme_minimal() +
      labs(title = "Pattern Metabolite Count", x = "Pattern", y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    p
})


observe({
  pattern_class <- spatial_pattern_all()
  available_patterns <- unique(pattern_class$class)
  available_patterns <- available_patterns[available_patterns != "unclassified"]  

  updateSelectInput(
    session = getDefaultReactiveDomain(),
    inputId = "pattern_select",
    choices = available_patterns,
    selected = if (length(available_patterns) > 0) available_patterns[1] else NULL  
  )
})



observe({
  pa <- input$pattern_select
  spatial_pattern_top <- spatial_pattern_all()

  req(
    spatial_pattern_top,
    pa,
    pa %in% spatial_pattern_top$class
  )

  ions <- as.character(
    spatial_pattern_top$mz[spatial_pattern_top$class == pa]
  )

  updateSelectInput(
    session = getDefaultReactiveDomain(),
    inputId = "Pattern_specific_m_select",
    choices = ions,
    selected = if (length(ions) > 0) ions[1] else NULL
  )
})



output$patternspecific_distribution_plot <- renderPlot({

  req(input$Pattern_specific_m_select)
  ion <- tryCatch(
    as.character(input$Pattern_specific_m_select),
    error = function(e) {
      showNotification("Invalid metabolite selection!", type = "error")
      return(NULL)
    }
  )
  req(ion, nzchar(ion))  

  data <- data_long_pattern()
  plot_data <- data[data$mz == ion, ]
  req(nrow(plot_data) > 0)  
  
  heatmap_palette <- colorRampPalette(rev(RColorBrewer::brewer.pal(11, 'Spectral')))

  ggplot(plot_data, aes(x = x, y = y)) +
    geom_point(aes(color = intensity), size = 1.5, alpha = 0.8) +
    scale_color_gradientn(
      colours = heatmap_palette(100),
      name = "Intensity",
      limits = c(min(plot_data$intensity), max(plot_data$intensity))
    ) +
    coord_equal() +
    labs(
      title = paste("Metabolite Distribution:", ion)
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        margin = margin(b = 10)
      ),
      legend.position = "right",
      legend.key.height = unit(1.5, "cm")
    )
})


output$download_pattern_metabolite_count_data <- downloadHandler(
  filename = function() {
    paste0("Metabolite_spatial_pattern.txt")
  },
  content = function(file) {
      spatial_pattern_all<-spatial_pattern_all()
      DemoData<-as.data.frame(spatial_pattern_all)
      write.table(DemoData,file,row.names = T, quote = F, sep = '\t')
})
