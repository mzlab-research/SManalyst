
cluster_plotlist <-reactive({
  req(data_rds())
  data_rds<-data_rds()
  source("./source/clusterplot.R")
  if(input$cluster_select=="UMAP-kmeans"){
    clustertype=0
    k=7
  }else if(input$cluster_select=="LV"){
    clustertype=1
  }else if(input$cluster_select=="LM"){
    clustertype=2
  }else{
    clustertype=3
  }
  
  resolution=input$cluster_resolution
  if(!is.null(resolution) && !is.null(clustertype)){
  withProgress(message = "Processing data...",value=0.8,{
    cluster_plotlist<-run_clusterplot(obj=data_rds,pointSize=1,breakseq=50,resolution=resolution,clustertype)
  }) 
    return(cluster_plotlist)
  }

}) 


output$cluster_plot <- renderPlot({
  resolution=input$cluster_resolution
  file_name <- paste("3b",resolution, sep = "_")
  cluster_plotlist()[[1]][[1]]
})


observeEvent(c(cluster_plotlist(),input$clear_selection_cluster), {
  
  data <- cluster_plotlist()[[3]]
  req(data)
  clusters <- unique(data@meta.data$seurat_clusters)
  if (length(clusters) > 0) {
    updateSelectizeInput(session = getDefaultReactiveDomain(), "auto_add_to_case",
                         choices = clusters, selected = NULL)
    if (length(clusters) > 1) {
      updateSelectizeInput(session = getDefaultReactiveDomain(), "auto_add_to_control",
                           choices = clusters, selected = NULL)
    }
  }
})
observeEvent(input$auto_add_to_case, {

  data <- cluster_plotlist()[[3]]
  req(data)
  clusters <- unique(data@meta.data$seurat_clusters)
  updateSelectInput(session=getDefaultReactiveDomain(), "auto_add_to_control",
                    choices = setdiff(clusters, input$auto_add_to_case),
                    selected = input$auto_add_to_control)
})


observeEvent(input$auto_add_to_control, {

  data <- cluster_plotlist()[[3]]
  req(data)
  clusters <- unique(data@meta.data$seurat_clusters)
  updateSelectInput(session=getDefaultReactiveDomain(), "auto_add_to_case",
                    choices = setdiff(clusters, input$auto_add_to_control),
                    selected = input$auto_add_to_case)
})

control_cluster<-reactive({
  data <- cluster_plotlist()[[3]]
  req(data)#,input$auto_add_to_control
  control_cluster<-data@meta.data %>% filter(seurat_clusters %in% input$auto_add_to_control)
  return(control_cluster)

})
case_cluster<-reactive({
  data <- cluster_plotlist()[[3]]
  req(data)#input$auto_add_to_case
  case_cluster<-data@meta.data %>% filter(seurat_clusters %in% input$auto_add_to_case)
  return(case_cluster)

})


split_groupunique_cluster <- eventReactive(input$finish_selection_cluster, {

  case_cluster <- case_cluster()
  control_cluster <- control_cluster()
  validate(
    need(nrow(case_cluster)>0,"Please select an case area and a control area.")
  )
  validate(
    need(nrow(control_cluster)>0,"Please select an case area and a control area.")
  )
  req(case_cluster,control_cluster)
  control_all <- control_cluster %>%
    mutate(groups = "control") %>%#input$control_group
    distinct(cell, .keep_all = TRUE)

  case_all <- case_cluster %>%
    mutate(groups = "case") %>%#input$case_group
    distinct(cell, .keep_all = TRUE)

  groupunique <- rbind(case_all, control_all) %>%
    distinct(cell, .keep_all = TRUE)

  split_groupunique_cluster <- split(groupunique, groupunique$groups)
  return(split_groupunique_cluster)
})

data_rds_group <- reactive({
  data_rds <-  cluster_plotlist()[[3]]
  split_groupunique<-split_groupunique_cluster()
  data_rds@meta.data$groups<-"blank"
  change_rds_group<-function(data,split_groupunique){
    for(i in names(split_groupunique)){
      k <- which(row.names(data@meta.data) %in% row.names(split_groupunique[[i]]))
      data@meta.data$groups[k] <- i
    }
    return(data)
  }
  data_rds_group=change_rds_group(data=data_rds,split_groupunique)
  return(data_rds_group)
})

group_spectrum_reactive <- reactiveVal(NULL)
observeEvent(input$finish_selection_cluster, {
  req(data_rds_group())
  color_mapping<-cluster_plotlist()[[1]][[2]]
  data_rds_group <- data_rds_group()
  data<-data_rds_group@meta.data
  data <- data %>%
    group_by(groups) 
  group_color<-c("case"="red","control"="blue","blank"="grey")
  p <-  ggplot(data, aes(x = x, y = y)) +
    geom_point(aes(color = groups), size = 1) +
    guides(colour = guide_legend(title = "Group",override.aes = list(size=3), nrow = 10))+
    scale_color_manual(values = group_color) +
    labs(title="overview")+
    theme_minimal()+
    xlim(min(data$x),max(data$x))+
    ylim(min(data$y),max(data$y))+
    theme(plot.title = element_text(hjust = 0.5))+
    coord_equal()
  group_spectrum_reactive(p)
}) 

output$group_spectrum_cluster <- renderPlot({
  if (!is.null(group_spectrum_reactive())) {
    
    group_spectrum_reactive()
  }
})

observeEvent(input$clear_selection_cluster, {

  group_spectrum_reactive(NULL)
})

output$download_cluster_plot  <- downloadHandler(
  filename = function() {
    "cluster_plot.png"
  },
  content = function(file) {
    withProgress(message = 'Downloading file...', value = 0.7, {
      p <- cluster_plotlist()[[1]][[1]]
      ggsave(file, plot = p, device = "png", width = 12, height = 8,bg = "#FFFFFF", dpi = 300)
    })
  })
output$download_cluster_data <- downloadHandler(
  filename = function() {
    paste0("cluster_data.txt")
  },
  content = function(file) {
    withProgress(message = 'Downloading file...', value = 0.7, {
      cluster_plotlist<-cluster_plotlist()
      DemoData<-cluster_plotlist[[2]]
      write.table(DemoData,file,row.names = F, quote = F, sep = '\t')
    })
  })

output$culster_resolution_button_container <- renderUI({
  if (input$cluster_select=="UMAP-kmeans") {
    sliderInput("cluster_resolution","Number of Clusters",value = 25,min=2,max=50,step = 1)
  } else {
    sliderInput("cluster_resolution","Resolution of clusters",value = 2,min=0.1,max=5,step = 0.1)
  }
})
# 

diff_cluster_data<-reactive({
  data_rds_group <- data_rds_group()
  group<-c("case","control")
  source("./source/find_marker.R")
  diff_m <- find_marker(data=data_rds_group,group)
  return(diff_m)
})
diff_omics <-reactive({
  diff_cluster_data <- diff_cluster_data()
  FC_Threshold <- input$Fold_change
  pvalue <- input$p_value_adjust
  source("./source/find_marker.R")
  diff_m <- add_State(markers=diff_cluster_data,FC_Threshold, pvalue)
  diff_num_m<-diff_m %>%
    dplyr::filter(State!="Non-significant")
  if (nrow(diff_num_m) == 0) {
    diff_omics<-NULL
    showNotification("The number of differential metabolite is zero. please modify the threshold.")
  }else{
    diff_omics<-diff_m
  }
  return(diff_omics)
})
diff_omics_ion <-reactive({
  diff_omics<-diff_omics()
  if(!is.null(diff_omics)){
    diff_m<-diff_omics %>%
      filter(State!="Non-significant")
    mz<-as.character(diff_m$mz)
  }else{
    mz<-NULL
  }
  return(mz)
})



output$volcano_plot <- renderPlot({
  volcano_data<-diff_omics()
  req(volcano_data)
  group<-c("case","control")
  up_count <- sum(volcano_data$State == "Up", na.rm = TRUE)
  down_count <- sum(volcano_data$State == "Down", na.rm = TRUE)
  FC_Threshold=input$Fold_change
  pvalue<-input$p_value_adjust
  source("./source/volcano_plot_processing.R")
  volcano_plot_save<-volcano_plot_processing(volcano_data,pvalue,FC_Threshold) +
    ggtitle(paste("Up:",up_count,"Down:",down_count))
  volcano_plot_save
})



diffion_distribution_data <- reactive({
  diff_omics<-diff_omics()
  data_rds_group <- data_rds_group()
  req(data_rds_group,diff_omics)
  req(input$diff_ion_select)
  ion <- tryCatch(
    as.character(input$diff_ion_select),
    error = function(e) {
      showNotification("Invalid metabolite selection!", type = "error")
      return(NULL)
    }
  )
  req(ion, nzchar(ion))  
  plot_data <- data_rds_group@meta.data
  count<-data_rds_group@assays$SCT$counts
  if(ion %in% rownames(count)){
    plot_data$intensity<- count[ion,]
  }else{
    plot_data<-NULL
  }
  return(list(plot_data,ion))
})
output$diffion_distribution_plot <- renderPlot({
  diffion_distribution_data<-diffion_distribution_data()
  req(diffion_distribution_data)
  plot_data<-diffion_distribution_data[[1]]
  ion<-diffion_distribution_data[[2]]
  req(plot_data,ion)
  heatmap_palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')))
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

merged_data_cluster <- reactive({
  DemoData<-diff_omics()
  identi <- identi_table()

  DemoData <- DemoData[,c(1,5,8,9)]
  identi <- identi[,c(1,10,11,12,13,14,15)]

  DemoData$mz <- as.character(DemoData$mz)
  identi$mz <- as.character(identi$mz)
  merged_data <- left_join(DemoData, identi, by = "mz")
  merged_data
})

output$download_volcano_data <- downloadHandler(
  filename = function() {
    paste0("cluster_diff_volcano_table.txt")
  },
  content = function(file) {
    write_delim(merged_data_cluster(),file,delim="\t")
  })

####
observe({
  diff_omics_ion<-diff_omics_ion()
  if(!is.null(diff_omics_ion)){
    ions<-diff_omics_ion
  }else{
    ions<-NA
  }
  updateSelectInput(session = getDefaultReactiveDomain(), "diff_ion_select", choices = ions,selected = ions[1])
})