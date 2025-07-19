find_marker<-function(data,group){
  obj <- data 
  obj@meta.data$group<-as.factor(obj@meta.data$group)
  
  obj@active.ident<-obj$group

  markers <- FindMarkers(obj,ident.1 = group[1],ident.2 = group[2],group.by = 'group', min.pct = 0, logfc.threshold =0)

  return(markers)
}

add_State<-function(markers,FC_Threshold=1.2,pvalue=0.05,Pvalue_type1="p_val_adj"){
  fc<-abs(log2(as.numeric(FC_Threshold)))
  py<-abs(-log10(as.numeric(pvalue)))
 
  markers$mz<-as.character(rownames(markers))
  markers <- markers[order(abs(markers$avg_log2FC),decreasing=TRUE),]
  markers$avg_log2FC<-as.numeric(markers$avg_log2FC)
  markers$p_val_adj <-as.numeric(markers$p_val_adj )
  markers %<>%
    mutate(log_p_val_adj = ifelse(.[[Pvalue_type1]]==0,0,(-log10(.[[Pvalue_type1]])))) %>%
    mutate(`log2(Fold Change)` = avg_log2FC) %>%
    mutate(`Fold Change` =  2^avg_log2FC)
  markers$State <- ifelse(markers$log_p_val_adj >= py & abs(markers$`log2(Fold Change)`) >= fc , 
                          ifelse(markers$`log2(Fold Change)` > fc, "Up", "Down"), "Non-significant")
  markers$State <- factor(markers$State, levels = c('Down', 'Non-significant', 'Up')) ##New add
  markers<-markers %>%
    dplyr::select("mz","pct.1","pct.2","p_val","p_val_adj","log_p_val_adj","log2(Fold Change)","Fold Change","State") %>%
    filter(State %in% c("Up", "Down", "Non-significant"))
  markers$mz<-as.character(markers$mz)
  markers$State <- factor(markers$State, levels = c('Down', 'Non-significant', 'Up')) ##New add
  return(markers)
}

