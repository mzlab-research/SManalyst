

Clustering <- function(object,resolution,clustertype, dims = 30){
  options(warn = -1)
  object <- RunPCA(object,npcs=30)
  options(warn = 0)
  object <- FindNeighbors(object, dims = 1:dims)
  object <- FindClusters(object, algorithm=clustertype,verbose = FALSE, resolution = resolution)
  return(object)
}


iPlot <- function(object, pt.size,breakseq){
  qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
  cluster_Palette <- unique(unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals))))
  k <- length(unique(object$seurat_clusters))
  color_mapping <- setNames(cluster_Palette[1:k], unique(object$seurat_clusters))
  
  plot <- ggplot(object@meta.data, aes(x = x, y = y, color = seurat_clusters)) +
    geom_point(shape = 19, size = pt.size) +
    xlim(min(object@meta.data$x),max(object@meta.data$x))+
    ylim(min(object@meta.data$y),max(object@meta.data$y))+
    coord_equal()+
    xlab("")+ylab("")+
    labs(title="")+
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank()) +
    theme_minimal() +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5)) + 
    scale_color_manual(values = color_mapping) +
    guides(colour = guide_legend(title = "Group",override.aes = list(size=3), nrow = 10))
  
  return(list(plot,color_mapping))
}

run_clusterplot <- function(obj,pointSize,breakseq,resolution,clustertype,umapk=7) {
  if(clustertype==0){
    clustertype=1
    umapk=resolution
    resolution=0.5
    obj <- Clustering(object=obj,resolution=resolution,clustertype=clustertype,dims = 30)
    obj <- RunUMAP(obj, dims = 1:30)
    umap = obj@reductions$umap@cell.embeddings %>%
      as.data.frame()
    kmeans_clusters1<- kmeans(umap, centers = umapk)
    clusterdata<-umap %>% cbind(Group =as.character(kmeans_clusters1$cluster) ,y=obj@meta.data[["y"]],x=obj@meta.data[["x"]])
    obj@meta.data$seurat_clusters<-as.character(kmeans_clusters1$cluster)
  }else{
    
    obj <- Clustering(object=obj,resolution=resolution,clustertype=clustertype,dims =30)
    Group_cluster<-obj@active.ident
    clusterdata<-data.frame(Group_cluster,y=obj@meta.data[["y"]],x=obj@meta.data[["x"]],
                            total_counts=obj@meta.data[["nCount_Spatial"]],n_ions_per_spot=obj@meta.data[["nFeature_Spatial"]])
    obj@meta.data$seurat_clusters<-Group_cluster
    
  }
  plot1 <- iPlot(obj, pt.size = pointSize,breakseq)
  
  plotlist<-list(plot1,clusterdata,obj)
  return(plotlist)
}