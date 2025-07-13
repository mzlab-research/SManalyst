#plot
volcano_plot_processing<-function(data,pvalue,FC_Threshold, Pvalue_type="p_val_adj"){#,group,
  xlims <- ceiling(max(abs(data$`log2(Fold Change)`)))
  State_value <- unique(data$State)
  State_len <- length(State_value)
  ###color args
  if(State_len == 1){
    if(State_value == 'Non-significant'){
      scale_color <- "grey"
    }else if(State_value == "Down"){
      scale_color <- "LightSkyBlue"
    }else if(State_value == "Up"){
      scale_color <- "HotPink"
    }
  }else if(State_len == 2){
    if('Down' %in% State_value & 'Non-significant' %in% State_value){
      scale_color <- c("LightSkyBlue","grey")
    }else if('Non-significant' %in% State_value & 'Up' %in% State_value){
      scale_color <- c("grey","HotPink")
    }else if('Down' %in% State_value & 'Up' %in% State_value){
      scale_color <- c("LightSkyBlue","HotPink")
    }
  }else if(State_len == 3){
    scale_color <- c("LightSkyBlue","grey","HotPink")
  }
    p<- data %>%
      ggplot(aes(`log2(Fold Change)`,log_p_val_adj))+
      theme_classic()+
      labs(title="")+
      geom_point(alpha= I(1/2),aes(color = State),size = 2.5)+
      scale_color_manual(values = scale_color)+
      scale_shape_manual(values = c(17, 16))+
      geom_hline(yintercept = -log10(pvalue),linetype=6,linewidth= .3,color = "black")+
      geom_vline(xintercept=c(-log2(FC_Threshold),log2(FC_Threshold)),linetype=6,linewidth= .3,color = "black")+
      xlim(-xlims,xlims)+
      xlab("log2(Fold Change)")+
      ylab(paste("-log10(",Pvalue_type,")",sep = ""))+
      theme(panel.grid=element_blank())+
      theme(axis.line.x = element_line(color="black", size = 0.5),
            axis.line.y = element_line(color="black", size = 0.5),
            plot.title = element_text(hjust = 0.5))
return(p)
}