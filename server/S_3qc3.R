# peakData_remove_noise <- reactive({
#   readRDS("/Users/chaohuyecao/1.Projects/2.SManalyst/Script/SManalyst0610/test_data/rds/peakData_remove_noise.rds")
# })

#QC7:isotope recognition===========================================

isotope_list <- eventReactive(input$iso_adduct_calculate, {
  data <- peakData_remove_noise()
  data[is.na(data)] <- 0  
  cor_cutoff <- input$iso_corr_cutoff
  ppm_cutoff <- input$iso_ppm_setting
  # data[is.na(data)] <- 0  
  # cor_cutoff <- 0.5
  # p_cutoff <- 0.05
  # ppm <- 5
  
  calculate_spatial_correlation <- function(col_indices) {
    intensity_mat <- as.matrix(data[, col_indices + 2])
    n_peaks <- length(col_indices)
    
    I_values <- numeric(0)
    p_values <- numeric(0)
    
    if (n_peaks < 2) {
      return(list(mean_I = NA, min_p = NA, n_pairs = 0))
    }
    
    for (i in 1:(n_peaks - 1)) {
      j <- i + 1  
      
      vec1 <- intensity_mat[, i]
      vec2 <- intensity_mat[, j]
      if (sd(vec1) == 0 || sd(vec2) == 0) next
      # Moran's I
      tryCatch({
        bv_moran <- moran_bv(
          x = vec1, 
          y = vec2, 
          listw = weights, 
          nsim = 99
        )
        p_val <- mean(abs(bv_moran$t) >= abs(bv_moran$t0))
        
        I_values <- c(I_values, bv_moran$t0)
        p_values <- c(p_values, p_val)
      }, error = function(e) {
      })
    }
    
    if (length(I_values) == 0) {
      return(list(mean_I = NA, min_p = NA, n_pairs = 0))
    }
    
    list(
      mean_I = mean(I_values, na.rm = TRUE),
      min_p = min(p_values, na.rm = TRUE),
      n_pairs = length(I_values)
    )
  }

  filter_and_optimize_clusters <- function(clusters, mz_vector, ppm = 5) {
    c13_diff <- 1.003355
    
    optimized_clusters <- lapply(clusters, function(cluster) {
      cluster_mz <- mz_vector[cluster]
      sorted_mz <- sort(cluster_mz)
      mono_mz <- sorted_mz[1]
      theoretical <- mono_mz + (0:4) * c13_diff
      best_matches <- sapply(theoretical, function(th_mz) {
        ppm_diff <- abs(cluster_mz - th_mz) / th_mz * 1e6
        candidates <- which(ppm_diff <= ppm)
        if (length(candidates) == 0) return(NA)
        best_candidate <- candidates[which.min(ppm_diff[candidates])]
        cluster_mz[best_candidate]
      })
      valid_matches <- na.omit(best_matches)
      first_missing <- which(is.na(best_matches))[1]
      if (!is.na(first_missing)) {  
        valid_matches <- best_matches[1:(first_missing - 1)]
      }
      matched_indices <- match(valid_matches, mz_vector)
      matched_indices
    })
    valid_clusters <- optimized_clusters[sapply(optimized_clusters, length) >= 2]
    cluster_strings <- sapply(valid_clusters, function(cluster) {
      paste(sort(mz_vector[cluster]), collapse = ",")
    })
    unique_indices <- which(!duplicated(cluster_strings))
    unique_clusters <- valid_clusters[unique_indices]
    return(unique_clusters)
  }
  
  mz_vector <- as.numeric(colnames(data)[3:ncol(data)])
  coords_df <- data[, c("X", "Y")]

  intensity_means <- colMeans(data[, 3:ncol(data)], na.rm = TRUE)
  mz_df <- data.frame(mz = mz_vector, intensity = intensity_means)
  mz_df <- mz_df[order(mz_df$mz), ]

  # use isotopologues to find isotpes
  withProgress(message = 'Identifying isotope clusters...', value = 0.2, {
    isos_list <- isotopologues(mz_df, ppm = ppm_cutoff)
    
    orig_idx <- 1:length(mz_vector)
    names(orig_idx) <- as.character(mz_vector)
    
    isos_list_orig <- lapply(isos_list, function(cluster) {
      cluster_mz <- mz_df$mz[cluster]
      as.numeric(orig_idx[as.character(cluster_mz)])
    })
    
    isos_list_orig <- isos_list_orig[sapply(isos_list_orig, length) >= 2]

    incProgress(0.2, detail = "Building spatial weights...")
    coords_sf <- sf::st_as_sf(coords_df, coords = c("X", "Y"))
    knn <- spdep::knn2nb(spdep::knearneigh(coords_sf, k = 5))
    weights <- spdep::nb2listw(knn, style = "W")
    

    incProgress(0.3, detail = "Calculating spatial correlations...")
    valid_clusters <- list()
    cluster_stats <- list()

    results <- pblapply(isos_list_orig, function(cluster) {
      cor_result <- calculate_spatial_correlation(cluster)

      valid <- ifelse(
        !is.na(cor_result$mean_I) && 
          cor_result$mean_I > cor_cutoff && 
          cor_result$min_p < 0.05,
        TRUE, FALSE
      )
      
      list(
        cluster = cluster,
        mz_values = mz_vector[cluster],
        mean_I = cor_result$mean_I,
        min_p = cor_result$min_p,
        n_pairs = cor_result$n_pairs,
        valid = valid
      )
    })

    for (i in seq_along(results)) {
      if (results[[i]]$valid) {
        valid_clusters[[length(valid_clusters) + 1]] <- results[[i]]$cluster
        cluster_stats[[length(cluster_stats) + 1]] <- results[[i]][c("mz_values", "mean_I", "min_p", "n_pairs")]
      }
    }
    
    incProgress(0.2, detail = "Filtering isotope clusters...")
    if (length(valid_clusters) > 0) {

      filtered_clusters <- filter_and_optimize_clusters(valid_clusters, mz_vector, ppm = 5)

      names(filtered_clusters) <- paste0("iso_pair_", seq_along(filtered_clusters))
      cleaned_list <- filtered_clusters
    } else {
      cleaned_list <- list()
    }
    
    incProgress(0.1, detail = "Completed!")
    return(cleaned_list)
  })
})


observe({
  cleaned_list <- isotope_list()
  ll <- c(1:length(cleaned_list))
  updateSelectizeInput(session = getDefaultReactiveDomain(), "isotope_select", choices = ll,server = TRUE)
})


output$isotope_peaks_group_visualization <- renderPlot({
  req(input$isotope_select, input$iso_corr_cutoff, input$iso_ppm_setting)
  data <- peakData_remove_noise()
  selected_index <- as.numeric(input$isotope_select)
  valid_clusters <- isotope_list()
  
  plot_isotope_cluster <- function(cluster_idx) {

    if (cluster_idx < 1 || cluster_idx > length(valid_clusters)) {
      stop("Invalid cluster index. Must be between 1 and ", length(valid_clusters))
    }
    

    col_indices <- valid_clusters[[cluster_idx]]
    mz_vector <- as.numeric(colnames(data)[3:ncol(data)])
    mz_values <- mz_vector[col_indices]

    plot_data <- data[, c("X", "Y", colnames(data)[col_indices + 2])]
    
    long_data <- reshape2::melt(plot_data, id.vars = c("X", "Y"), 
                                variable.name = "mz", 
                                value.name = "intensity")
    
    long_data$mz_label <- paste0("m/z: ", round(as.numeric(as.character(long_data$mz)), 4))
    
    mz_levels <- unique(long_data$mz_label)[order(as.numeric(gsub(".* ", "", unique(long_data$mz_label))))]
    long_data$mz_label <- factor(long_data$mz_label, levels = mz_levels)

    main_title <- paste0("Isotope Cluster #", cluster_idx)

    facet_plot <- ggplot(long_data, aes(x = X, y = Y)) +
      geom_point(aes(color = intensity), size = 1.2, alpha = 0.8) +
      scale_color_viridis_c(
        option = "plasma", 
        trans = "sqrt", 
        name = "Intensity", 
        breaks = scales::pretty_breaks(n = 4)
      ) +
      facet_wrap(~ mz_label) +
      coord_equal() +
      labs(title = main_title) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size = 10, face = "bold"),
        strip.background = element_rect(fill = "gray90", color = NA),
        legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8)
      )

    print(facet_plot)

    invisible(list(
      facet_plot = facet_plot,
      plot_data = long_data
    ))
  }
  plot_isotope_cluster(selected_index)

})
  


#isotope peak ratio----------------------
output$isotope_ratio_box <- renderInfoBox({
  cleaned_list <- isotope_list()
  data <- peakData_remove_noise()
  k <- length(unlist(cleaned_list))/(ncol(data)-2)
  infoBox(
    "The percent of isotope peak is:", sprintf("%.2f%%", k * 100), icon = icon("list"),
    color = "purple"
  )
})


output$Isotope_peaks_distribution <- renderPlot({
  cleaned_list <- isotope_list()
  data <- peakData_remove_noise()
  mz_view_data <- apply(data[,-c(1,2)],2,function(x){
    median_intensity <- median(x,na.rm = T)
    return(median_intensity)
  })
  y <- data.frame(intensity=mz_view_data,mz=names(mz_view_data))
  jl <- list(y=y,cleaned_list=cleaned_list)

  plot(y$mz, y$intensity, type = "h",xlab = "m/z",ylab = "Intensity")
  for (i in seq_along(cleaned_list)) {
    z <- cleaned_list[[i]]
    points(y$mz[z], y$intensity[z], col = i + 1)
  }
})

deal_data_new <- reactive({
  cleaned_list <- isotope_list()
  data <- peakData_remove_noise()
  mz_view_data <- mz_view_data()
  data[is.na(data)] <- 0.1

  sorted_list <- lapply(cleaned_list, sort)
  all_iso_mark <- data.frame()
  for(i in 1:length(sorted_list)){
    temp <- data[,sorted_list[[i]]+2] 
    temp_data <- data.frame(mz = names(temp),single_ios = min(as.numeric(names(temp))))
    all_iso_mark <- bind_rows(all_iso_mark,temp_data)
  }

  mz_view_data$mz <- as.character(mz_view_data$mz)
  deal_data <- mz_view_data %>%
    dplyr::select(mz,miss_ratio,median_intensity) %>%
    left_join(all_iso_mark,c("mz"="mz")) %>%
    mutate(single_ios = ifelse(is.na(single_ios),"Unknown",single_ios))
  return(deal_data)
})

clean_data <- reactive({
  deal_data_new <- deal_data_new()
  data <- peakData_remove_noise()

 Non_monoisotope <- deal_data_new %>%
   filter(single_ios != "Unknown") %>%
   filter(single_ios != mz)

 clean_data <- data %>%
   dplyr::select(-Non_monoisotope$mz)

 return(clean_data)
})












#QC8：adduct ions==========================================

observeEvent(input$mode_select, {
  positive_ions <- adductNames(polarity = "positive")
  negative_ions <- adductNames(polarity = "negative")
  mode <- input$mode_select
  if (mode == "pos") {
    updateSelectInput(session = getDefaultReactiveDomain(), "adduct_ion", choices = positive_ions,selected = positive_ions[c(13,16,18,20,36)])
  } else if (mode == "neg") {
    updateSelectInput(session = getDefaultReactiveDomain(), "adduct_ion", choices = negative_ions,selected = negative_ions[c(3,5)])
  }
})

adduct_ion_table <- eventReactive(input$iso_adduct_calculate, {
  req(input$adduct_ion, input$add_ppm_setting, input$add_corr_cutoff)
  data <- clean_data()

  validate(
    need(nrow(data) > 0, "please upload peak file first"),
    need(ncol(data) > 2, "The peak table should contain at least two ions")
  )
  
  adducts <- input$adduct_ion
  ppm_threshold <- input$add_ppm_setting
  adduct_cor_cutoff <- input$add_corr_cutoff

  data[is.na(data)] <- 0
  mz_vector <- as.numeric(colnames(data)[3:ncol(data)])

  neutral_mass <- mz2mass(mz_vector, adducts)

  neutral_mass_df <- as.data.frame(neutral_mass)
  neutral_mass_df$mz_vector <- colnames(data)[3:ncol(data)]
  neutral_mass_long <- melt(
    neutral_mass_df, 
    id.vars = "mz_vector", 
    variable.name = "adduct", 
    value.name = "neutral_mass"
  )
  neutral_mass_long <- as.data.table(neutral_mass_long)

  setorder(neutral_mass_long, neutral_mass)

  neutral_mass_long[, adduct_mark := NA_character_]
  current_mark <- 0
  n <- nrow(neutral_mass_long)
  i <- 1

  withProgress(message = 'Find Adducts', value = 0, {
    while (i <= n) {
      incProgress(1/n, detail = paste("Processing", round(i/n*100, 1), "%"))
      
      current_mass <- neutral_mass_long$neutral_mass[i]
      current_adduct <- neutral_mass_long$adduct[i]
      current_mz <- neutral_mass_long$mz_vector[i]
      
      current_group <- data.table(
        index = i,
        neutral_mass = current_mass,
        adduct = current_adduct,
        mz = current_mz
      )
      
      j <- i + 1
      group_found <- FALSE
      
      while (j <= n) {
        next_mass <- neutral_mass_long$neutral_mass[j]
        mass_diff <- abs(next_mass - current_mass)
        
        if (mass_diff > 0.05) break
        
        mass_mean <- (current_mass + next_mass) / 2
        ppm_error <- (mass_diff / mass_mean) * 1e6
        
        if (ppm_error <= ppm_threshold) {
          current_group <- rbind(current_group, list(
            j, next_mass, 
            neutral_mass_long$adduct[j],
            neutral_mass_long$mz_vector[j]
          ))
          group_found <- TRUE
        }
        j <- j + 1
      }
      
      if (group_found && nrow(current_group) >= 2) {
        if (length(unique(current_group$adduct)) >= 2) {
          current_mark <- current_mark + 1
          mark_name <- paste0("adduct-", current_mark)
          neutral_mass_long$adduct_mark[current_group$index] <- mark_name
          i <- max(current_group$index) + 1
        } else {
          i <- i + 1
        }
      } else {
        i <- i + 1
      }
    }
  })
  
  adduct_groups <- neutral_mass_long[!is.na(adduct_mark)]
  
  coords_df <- data[, c("X", "Y")]
  coords_sf <- st_as_sf(coords_df, coords = c("X", "Y"))
  knn <- knn2nb(knearneigh(coords_sf, k = 5))
  weights <- nb2listw(knn, style = "W")
  data_dt <- as.data.table(data)
  
  unique_marks <- unique(adduct_groups$adduct_mark)
  filtered_adduct_groups <- data.table()
  
  withProgress(message = 'Validate spatial correlations', value = 0, {
    total_marks <- length(unique_marks)
    
    for (idx in seq_along(unique_marks)) {
      #idx <- 1
      mark <- unique_marks[idx]
      incProgress(1/total_marks, detail = paste("Group", idx, "/", total_marks))
      
      group_data <- adduct_groups[adduct_mark == mark]
      mz_values <- unique(group_data$mz_vector)
      
      if (length(mz_values) < 2) next
      
      sub_data <- data_dt[, .SD, .SDcols = c("X", "Y", mz_values)]
      n_mz <- length(mz_values)
      all_pairs <- combn(seq_along(mz_values), 2)
      all_correlated <- TRUE
      
      for (i in 1:ncol(all_pairs)) {
        #i <- 1
        col1 <- mz_values[all_pairs[1, i]]
        col2 <- mz_values[all_pairs[2, i]]
        metabA <- sub_data[[col1]]
        metabB <- sub_data[[col2]]
        
        bv_moran <- moran_bv(
          x = metabA, 
          y = metabB, 
          listw = weights, 
          nsim = 99
        )
        
        if (bv_moran$t0 <= adduct_cor_cutoff) {
          all_correlated <- FALSE
          break
        }
      }
      
      if (all_correlated) {
        filtered_adduct_groups <- rbindlist(list(
          filtered_adduct_groups, 
          group_data
        ))
      }
    }
  })
  
  filtered_adduct_groups[, group_size := .N, by = adduct_mark]
  
  if (nrow(filtered_adduct_groups) > 0) {
    mz_adduct_counts <- filtered_adduct_groups[
      , .(unique_adduct_count = uniqueN(adduct)), 
      by = mz_vector
    ]
    
    filtered_adduct_groups <- merge(
      filtered_adduct_groups, 
      mz_adduct_counts, 
      by = "mz_vector", 
      all.x = TRUE
    )
    
    filtered_adduct_groups[, problem_mz := ifelse(unique_adduct_count > 1, 1, 0)]
    problem_groups <- filtered_adduct_groups[
      problem_mz == 1, 
      unique(adduct_mark)
    ]
    
    filtered_adduct_groups[, unsure_adduct := ifelse(
      adduct_mark %in% problem_groups, 1, 0
    )]
    
    final_adduct_groups <- filtered_adduct_groups[unsure_adduct == 0]
    final_adduct_groups[, c("unique_adduct_count", "problem_mz", "unsure_adduct", "group_size") := NULL]
    names(final_adduct_groups)[1] <- "mz"
    
    return(final_adduct_groups)
  } else {
    return(data.table())  
  }
})


observe({
  adduct <- adduct_ion_table()
  ll <- unique(adduct$adduct_mark)
  updateSelectizeInput(session = getDefaultReactiveDomain(), "adduct_select", choices = ll,server = TRUE)
})


output$adduct_peaks_group_visualization <- renderPlot({
  req(input$adduct_select)
  data <- peakData_remove_noise()
  selected_adduct_index <- input$adduct_select
  final_adduct_groups <- adduct_ion_table()
  
  plot_adduct_group <- function(mark, adduct_groups, data) {  
    if (!mark %in% adduct_groups$adduct_mark) {
      stop("the adduct_mark does not exists!")
    }
    
    group_mz <- adduct_groups[adduct_mark == mark, unique(mz)]
    
    plot_data <- data[, c("X", "Y", group_mz)]

    long_data <- reshape2::melt(
      plot_data, 
      id.vars = c("X", "Y"), 
      variable.name = "mz", 
      value.name = "intensity"
    )

    group_info <- adduct_groups[adduct_mark == adduct_mark, .(mz, adduct, neutral_mass)]
    long_data <- merge(long_data, group_info, by = "mz", all.x = TRUE)

    long_data$mz_label <- paste0(
      "m/z: ", as.character(long_data$mz),
      "\nAdduct: ", long_data$adduct
    )

    main_title <- paste0("Adduct Group: ", mark)

    facet_plot <- ggplot(long_data, aes(x = X, y = Y)) +
      geom_point(aes(color = intensity), size = 1.2, alpha = 0.8) +
      scale_color_viridis_c(
        option = "plasma", 
        trans = "sqrt", 
        name = "Intensity", 
        breaks = scales::pretty_breaks(n = 4)
      ) +
      facet_wrap(~ mz_label) +
      coord_equal() +
      labs(title = main_title) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size = 10, face = "bold"),
        strip.background = element_rect(fill = "gray90", color = NA),
        legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8)
      )
    
    print(facet_plot)
    invisible(facet_plot)
  }
  plot_adduct_group(selected_adduct_index, final_adduct_groups, data)
  
})


output$adduct_ratio_box <- renderInfoBox({
  data <- peakData_remove_noise()
  adduct <- adduct_ion_table()
  
  k <- nrow(adduct)/ncol(data)
  infoBox(
    "The ratio of adduct peaks is:",sprintf("%.2f%%", k * 100), icon = icon("list"),
    color = "purple"
  )
})


output$adduct_peaks_count <- renderPlot({
  add_all2 <- adduct_ion_table()
  add_count <- as.data.frame(table(add_all2$adduct))
  colnames(add_count) <- c("adduct", "Frequency")
  add_plot <- ggplot(add_count, aes(x = adduct, y = Frequency)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    theme_bw() +
    coord_flip()
  return(add_plot)
})



end_deal_data1 <- reactive({
  deal_data_iso <- deal_data_new()  
  add_all3 <- adduct_ion_table() 
  end_deal_data <- deal_data_iso %>%
    left_join(add_all3,c("mz"="mz")) %>%
    mutate(single_ios = na_if(single_ios, "Unknown"))
  names(end_deal_data)[6] <- "netural_mass"
  end_deal_data$miss_ratio <- round(end_deal_data$miss_ratio, 3)
  return(end_deal_data)
})



output$download_peak_info <- downloadHandler(
  filename <- function() {
    paste0("Peak_info.txt")
  },
  content <- function(file) {
    write_delim(end_deal_data1(),file,delim="\t")
  }
)
