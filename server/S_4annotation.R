# end_deal_data1 <- reactive({
#   readRDS("/Users/chaohuyecao/1.Projects/2.SManalyst/Script/SManalyst0610/test_data/0705/end_deal_data0.rds")
# })


user_database <- reactive({
  req(!is.null(input$database_file))
  file_path <- input$database_file$datapath
  
  withProgress(message = 'Reading file...', value = 0, {
    incProgress(0.3, detail = "Reading file...")
    db <- readr::read_delim(file_path, show_col_types = FALSE)
    incProgress(0.7, detail = "Validating format...")
    
    required_cols <- c("MW", "Name", "Formula", "KEGG", "HMDB", "LIPIDMAPS")
    errors <- character(0)
    
    if (!identical(sort(colnames(db)), sort(required_cols))) {
      errors <- c(errors, paste0("The col names should be: ", 
                                 paste(required_cols, collapse = ", "),
                                 "\n uploaded col names: ", 
                                 paste(colnames(db), collapse = ", ")))
    } else {
      if (!is.numeric(db$MW)) {
        errors <- c(errors, "MW must be numbers!")
      } else if (any(is.na(db$MW))) {
        errors <- c(errors, "MW must not be NA!")
      }
      
      if (any(is.na(db$Name) | any(trimws(db$Name) == ""))) {
        errors <- c(errors, "Name must not be NA or empty!")
      }

      if (any(is.na(db$Formula) | any(trimws(db$Formula) == ""))) {
        errors <- c(errors, "Formulamust not be NA or empty!")
      }
    }

    if (length(errors) > 0) {
      shiny::validate(
        shiny::need(length(errors) == 0, 
                    message = paste("File format error:", "\n", paste("-", errors, collapse = "\n")))
      )
    }
    
    incProgress(1, detail = "Validation passed!")
  })
  
  db  
})

output$validation_output <- renderPrint({
  tryCatch({
    db <- user_database()
    cat("Validated database file!\n")
    cat(sprintf("Rows: %d\n", nrow(db)))
    cat(sprintf("Cols: %d\n", ncol(db)))
  }, error = function(e) {
    cat(e$message)
  })
})


output$dynamic_checkboxes <- renderUI({

  if (is.null(input$database_file)) {
    choices <- list("HMDB" = "HMDB", "KEGG" = "KEGG", "LipidMaps" = "LipidMaps")
  } else {
    choices <- list("User_Database" = "User_Database", "HMDB" = "HMDB", "KEGG" = "KEGG", "LipidMaps" = "LipidMaps")
  }

  checkboxGroupInput("database_select", "Choose databases:",
                     choices = choices)
})


# identi_table <- eventReactive(input$goButton, {
#   ####1.parameters------------------------------------------------------------------------
#   database_name <- input$database_select 
#   adduct_name <- input$adduct_ion 
#   MS1_accuracy <- input$MS1_match_tolerance #ppm
#   ######2.files for annotation------------------------------------------------------------------------
# 
#   end_deal_data0 <- end_deal_data1()
# 
#   #2.2把同位素峰存储在一个list中
#   iso_data <- end_deal_data0 %>% 
#     filter(!is.na(single_ios))
# 
#   iso_list <- iso_data %>%
#     group_by(single_ios) %>%
#     group_map(~ {
# 
#       tibble(
#         mz = .x$mz,
#         median_intensity = .x$median_intensity
#       )
#     }) #%>%
#   names(iso_list) <- sapply(iso_list, function(df) df$mz[1])
# 
#   #2.3对同位素峰的强度进行归一化处理
#   iso_list_norm <- map(iso_list, ~ {
#     max_intensity <- max(.x$median_intensity)
#     .x %>%
#       mutate(normalized_intensity = median_intensity / max_intensity*100) %>%
#       select(mz, median_intensity, normalized_intensity)  
#   })
#   
#   non_iso_data <- end_deal_data0 %>%
#     filter(is.na(single_ios) | mz == single_ios)
#   
#   #2.4将目前的表格分为两个部分：1）有加和离子，2）无加和离子
#   adduct_data <- non_iso_data %>%
#     filter(!is.na(adduct_mark)) 
#   no_adduct_data <- non_iso_data %>%
#     filter(is.na(adduct_mark))
#   
#   ####3.鉴定数据库的整理及鉴定的函数准备------------------------------------------------------------------------
#   #3.1读取数据库
#   Combind_database <- read.delim("www/Combind_database.txt") 
# 
#   if (!is.null(input$database_file)) {
#     db <- user_database() 
#     db$User_Database <- paste("USER",c(1:nrow(db)),sep = "_") 
# 
#     Combind_database$User_Database <- ""
#     for(i in 1:nrow(db)) {
#       current_row <- db[i, ]
#       matched_rows <- integer(0)  
#       if(!is.na(current_row$HMDB) && current_row$HMDB != "") {
#         matched_rows <- which(Combind_database$HMDB == current_row$HMDB)
#       }
#       
#       if(length(matched_rows) == 0 && 
#          !is.na(current_row$LIPIDMAPS) && current_row$LIPIDMAPS != "") {
#         matched_rows <- which(Combind_database$LIPIDMAPS == current_row$LIPIDMAPS)
#       }
#       
#       if(length(matched_rows) == 0 && 
#          !is.na(current_row$KEGG) && current_row$KEGG != "") {
#         matched_rows <- which(Combind_database$KEGG == current_row$KEGG)
#       }
# 
#       if(length(matched_rows) > 0) {
#         Combind_database$User_Database[matched_rows] <- current_row$User_Database
#       } else {
#         new_row <- data.frame(
#           MW = current_row$MW,
#           Name = current_row$Name,
#           Formula = current_row$Formula,
#           KEGG = current_row$KEGG,
#           HMDB = current_row$HMDB,
#           LIPIDMAPS = current_row$LIPIDMAPS,
#           User_Database = current_row$User_Database
#         )
#         Combind_database <- rbind(Combind_database, new_row)
#       }
#     }
#   }
#   
#   
#   
#   #3.2根据database_name筛选
#   filter_database <- function(data, db_names) {
# 
#     valid_rows <- integer(0)
# 
#     for (db in db_names) {
#       if (!is.null(db)) {
# 
#         non_na_rows <- which(!is.na(data[[db]]) & 
#                                (data[[db]] != ""))  
#         valid_rows <- c(valid_rows, non_na_rows)
#       }
#     }
# 
#     valid_rows <- unique(sort(valid_rows))
# 
#     if (length(valid_rows) > 0) {
#       return(data[valid_rows, ])
#     } else {
#       return(data[0, ])
#     }
#   }
# 
#   Annotation_database <- filter_database(Combind_database, database_name)
#   if (!is.null(input$database_file)) {
#     Annotation_database <- Annotation_database[,-7]
#   }
# 
#   #3.3准备鉴定函数
#   #3.3.1 MS1数据库匹配
# 
#   MS1_mass_match <- function(mass, MS1_accuracy, database = Annotation_database) {
# 
#     lapply(mass, function(current_mass) {
# 
#       matches <- database %>%
#         mutate(
#           mass_error_da = MW - current_mass,
#           mass_error_ppm = mass_error_da / current_mass * 1e6
#         ) %>%
#         filter(abs(mass_error_ppm) <= MS1_accuracy)
#       
#       if(nrow(matches) > 0) {
#         matches <- matches %>%
#         mutate(netural_mass = current_mass) %>%
# 
#           arrange(abs(mass_error_ppm))  
#       }
# 
#       if (nrow(matches) == 0) {
#         return(tibble(
#           netural_mass = current_mass,
#           mass_error_ppm = NA_real_,
#           MW = NA_real_,
#           Name = NA_character_,
#           Formula = NA_character_,
#           KEGG = NA_character_,
#           HMDB = NA_character_,
#           LIPIDMAPS = NA_character_
#         ))
#       }
#       matches
#     }) %>%
#       bind_rows()  
#   }
#   #3.3.2同位素打分
# 
#   iso_score_function <- function(mz, adduct, formula) {
#     
#     data(isotopes) 
#     pattern <- isopattern(
#       isotopes,
#       formula,
#       threshold = 0.1,
#       plotit = FALSE,
#       charge = FALSE,
#       emass = 0.00054858,
#       algo = 1
#     )
# 
#     mz1 <-  pattern[[formula]][, "m/z"]
#     intensity <-  pattern[[formula]][, "abundance"]
#     
#     mz1 <- as.numeric(mass2mz(mz1, adduct))
#     theoretical_peaks <- data.frame(mz = mz1, intensity = intensity)
# 
#     actual_peaks <- as.data.frame(iso_list_norm[[as.character(mz)]])
#     
#     if (is.null(actual_peaks)) {
#       stop("No matching actual peak found for the given mz.")
#     }
#     actual_peaks <- data.frame(
#       mz = as.numeric(actual_peaks$mz),
#       intensity = actual_peaks$normalized_intensity
#     )
#     
#     similarity_score <- msentropy_similarity(
#       as.matrix(theoretical_peaks),
#       as.matrix(actual_peaks),
#       ms2_tolerance_in_da = 1
#     )
# 
#     theoretical_peaks$data_set <- "Theoretical"
#     actual_peaks$data_set <- "Actual"
#     theoretical_peaks <- theoretical_peaks %>%
#       mutate(type = "Theoretical",
#              position = "top")
#     
#     actual_peaks <- actual_peaks %>%
#       mutate(type = "Actual",
#              intensity = -intensity, 
#              position = "bottom")
# 
#     combined_peaks <- bind_rows(theoretical_peaks, actual_peaks)
# 
#     p <- ggplot(combined_peaks, aes(x = mz)) +
#       geom_segment(
#         aes(xend = mz, y = 0, yend = intensity, color = type),
#         linewidth = 0.7,
#         alpha = 0.8
#       ) +
#       geom_hline(yintercept = 0, color = "gray50", linewidth = 0.5) +
#       scale_color_manual(values = c("Theoretical" = "#1f77b4", "Actual" = "#ff7f0e")) +
#       scale_y_continuous(
#         name = "Relative Intensity",
#         breaks = function(lims) {
#           top_breaks <- pretty(lims[lims > 0], n = 5)
#           bottom_breaks <- -rev(pretty(-lims[lims < 0], n = 5))
#           c(bottom_breaks, top_breaks)
#         },
#         labels = function(x) ifelse(x < 0, -x, x)  
#       ) +
#       labs(
#         x = "m/z",
#         title = paste(mz,adduct, formula,similarity_score, sep = " "),
#         color = "Spectrum"
#       ) +
#       theme_minimal(base_size = 14) +
#       theme(
#         panel.grid.major = element_line(color = "gray90"),
#         panel.grid.minor = element_blank(),
#         legend.position = "top",
#         plot.title = element_text(hjust = 0.5, face = "bold"),
#         axis.title = element_text(face = "bold")
#       ) 
# 
#     result <- list(similarity_score = similarity_score, plot = p)
#     return(result)
#   }
# 
# 
#   ####4.有加和离子中性质量adduct_data的鉴定------------------------------------------------------------------------
#   #4.1首先取出中性质量，去除重复
#   adduct_data <- adduct_data %>%
#     mutate(netural_mass = as.numeric(netural_mass))
#   adduct_data_netural_mass <- adduct_data %>%
#     select(netural_mass) %>%
#     distinct() %>%
#     arrange(netural_mass)
#   # 4.2调用数据库匹配函数进行分析
#   adduct_data_match_results <- MS1_mass_match(adduct_data_netural_mass$netural_mass, MS1_accuracy = MS1_accuracy)
#   
#   # 4.3合并数据（保留所有匹配项）
#   adduct_data_merged_results <- adduct_data %>%
#     left_join(adduct_data_match_results, by = "netural_mass",relationship = "many-to-many") 
#   #4.4同位素峰打分
# 
#   adduct_data_merged_results <- adduct_data_merged_results %>%
#     mutate(
#       info_paste = paste(mz, adduct, Formula, sep = " | ")
#     )
# 
#   addcut_isotope_peak <- adduct_data_merged_results %>%
# 
#     select(mz, adduct, single_ios, Formula, info_paste) %>%
# 
#     filter(!is.na(single_ios) & !is.na(Formula)) %>%
# 
#     distinct(info_paste, .keep_all = TRUE)
# 
#   if (nrow(addcut_isotope_peak) > 0) {
# 
#     #pdf("/Users/chaohuyecao/1.Projects/2.SManalyst/Script/identification/adduct_isotope_similarity_score.pdf")
#     for (i in 1:nrow(addcut_isotope_peak)) {
#       row <- addcut_isotope_peak[i, ]
#       
#       if (!is.na(row$single_ios) && !is.na(row$Formula)) {
#         tryCatch({
#           similarity_score <- iso_score_function(
#             mz = as.numeric(row$single_ios),
#             adduct = as.character(row$adduct),
#             formula = as.character(row$Formula)
#           )
#           addcut_isotope_peak$isotope_similarity_score[i] <- similarity_score$similarity_score
#           #print(similarity_score$plot)  
#         }, error = function(e) {
#           warning(paste("Error processing row", i, ":", e$message))
#           addcut_isotope_peak$isotope_similarity_score[i] <- NA
#         })
#       }
#     }
#     #dev.off()
# 
#     adduct_data_merged_results <- adduct_data_merged_results %>%
#       left_join(addcut_isotope_peak %>% select(info_paste, isotope_similarity_score), 
#                 by = "info_paste")
# 
#     adduct_data_merged_results <- adduct_data_merged_results %>%
#       select(-info_paste)
#   }else{
#     adduct_data_merged_results$isotope_similarity_score <- NA
#   }
#   
#   
#   
# 
#   
#   #4.5整体鉴定结果打分
#   adduct_data_merged_results$annotation_score <- NA
#   
#   for (i in 1:nrow(adduct_data_merged_results)) {
#     if (is.na(adduct_data_merged_results$Name[i])) next
#     if (!is.na(adduct_data_merged_results$mass_error_ppm[i])) {
#       ppm_abs <- abs(adduct_data_merged_results$mass_error_ppm[i])
#       ppm_score <- 100 * (1 - ppm_abs / MS1_accuracy)
#       ppm_score <- max(0, min(100, ppm_score))
#     } else {
#       ppm_score <- 0
#     }
#     if (!is.na(adduct_data_merged_results$isotope_similarity_score[i])) {
#       iso_score <- adduct_data_merged_results$isotope_similarity_score[i] * 100
#     } else {
#       iso_score <- 0
#     }
#     adduct_data_merged_results$annotation_score[i] <- 
#       (ppm_score * 0.5) + (iso_score * 0.3) + (100 * 0.2)
#   }
# 
#   ####5.non_adduct_data的鉴定------------------------------------------------------------------------
#   #5.1首先计算每一个mz的中性质量
# 
#   no_adduct_data <- no_adduct_data %>%
#     mutate(mz = as.numeric(mz))
# 
#   mass_results <- mz2mass(no_adduct_data$mz, adduct_name)
# 
#   no_adduct_data_netural_mass <- no_adduct_data %>%
#     select(mz) %>%
#     bind_cols(as_tibble(mass_results)) %>% 
#     pivot_longer(
#       cols = -mz,
#       names_to = "assume_adduct_name",
#       values_to = "calculated_netural_mass"
#     )
# 
#   #5.2再用中性质量来做数据库的匹配
#   no_adduct_match_results <- MS1_mass_match(no_adduct_data_netural_mass$calculated_netural_mass, MS1_accuracy = MS1_accuracy)
# 
#   #5.3合并数据
#   no_adduct_data <- no_adduct_data %>% mutate(mz = as.numeric(mz))
#   no_adduct_data_netural_mass <- no_adduct_data_netural_mass %>% mutate(mz = as.numeric(mz))
#   names(no_adduct_match_results)[9] <- "calculated_netural_mass"
# 
#   no_adduct_match_results <- no_adduct_match_results %>%
#     filter(!is.na(Name))
# 
#   merged_step1 <- no_adduct_data %>%
#     left_join(no_adduct_data_netural_mass, by = "mz")
# 
#   no_adduct_merged_results <- merged_step1 %>%
#     left_join(no_adduct_match_results, 
#               by = c("calculated_netural_mass" = "calculated_netural_mass"))
#   #5.4同位素峰打分
# 
#   no_adduct_merged_results <- no_adduct_merged_results %>%
#     mutate(
#       info_paste = paste(mz, assume_adduct_name, Formula, sep = " | ")
#     )
#   
# 
#   no_addcut_isotope_peak <- no_adduct_merged_results %>%
#     select(mz, assume_adduct_name, single_ios, Formula, info_paste) %>%
#     filter(!is.na(single_ios) & !is.na(Formula)) %>%
#     distinct(info_paste, .keep_all = TRUE)
#   
#   if (nrow(no_addcut_isotope_peak) > 0) {
#     #pdf("/Users/chaohuyecao/1.Projects/2.SManalyst/Script/identification/no_adduct_isotope_similarity_score.pdf")
#     for (i in 1:nrow(no_addcut_isotope_peak)) {
#       row <- no_addcut_isotope_peak[i, ]
#       
#       if (!is.na(row$single_ios) && !is.na(row$Formula)) {
#         tryCatch({
#           similarity_score <- iso_score_function(
#             mz = as.numeric(row$single_ios),
#             adduct = as.character(row$assume_adduct_name),
#             formula = as.character(row$Formula)
#           )
#           no_addcut_isotope_peak$isotope_similarity_score[i] <- similarity_score$similarity_score
#           #print(similarity_score$plot)  
#         }, error = function(e) {
#           warning(paste("Error processing row", i, ":", e$message))
#           no_addcut_isotope_peak$isotope_similarity_score[i] <- NA
#         })
#       }
#     }
#     #dev.off()
# 
#     no_adduct_merged_results <- no_adduct_merged_results %>%
#       left_join(no_addcut_isotope_peak %>% select(info_paste, isotope_similarity_score), 
#                 by = "info_paste")
# 
#     no_adduct_merged_results <- no_adduct_merged_results %>%
#       select(-info_paste)
#   }else{
#     no_adduct_merged_results$isotope_similarity_score <- NA
#   }
#   
#   
#   
#   #5.5整体鉴定结果打分
#   no_adduct_merged_results$annotation_score <- NA
#   for (i in 1:nrow(no_adduct_merged_results)) {
#     if (is.na(no_adduct_merged_results$Name[i])) next
#     if (!is.na(no_adduct_merged_results$mass_error_ppm[i])) {
#       ppm_abs <- abs(no_adduct_merged_results$mass_error_ppm[i])
#       ppm_score <- 100 * (1 - ppm_abs / MS1_accuracy) 
#       ppm_score <- max(0, min(100, ppm_score))
#     } else {
#       ppm_score <- 0
#     }
# 
#     if (!is.na(no_adduct_merged_results$isotope_similarity_score[i])) {
#       iso_score <- no_adduct_merged_results$isotope_similarity_score[i] * 100
#     } else {
#       iso_score <- 0
#     }
# 
#     no_adduct_merged_results$annotation_score[i] <- 
#       (ppm_score * 0.5) + (iso_score * 0.3) + (0 * 0.2)
#   }
# 
#   ###6.鉴定结果的合并与输出------------------------------------------------------------------------
# 
#   adduct_data_merged_results$assume_adduct_name <- NA
#   adduct_data_merged_results$mz <- as.numeric(adduct_data_merged_results$mz)
# 
#   adduct_data_merged_results <- adduct_data_merged_results %>%
#     select(mz, miss_ratio, median_intensity, single_ios, adduct, netural_mass, 
#            adduct_mark, assume_adduct_name, calculated_netural_mass = netural_mass,
#            mass_error_ppm, MW, Name, Formula, KEGG, HMDB, 
#            isotope_similarity_score, annotation_score)
#   no_adduct_merged_results <- subset(no_adduct_merged_results, select = -netural_mass)
# 
#   combined_results <- bind_rows(adduct_data_merged_results, no_adduct_merged_results)
# 
#   
#   ###7.对不可能的加合形式进行去除------------------------------------------------------------------------
# 
#   combined_results$adduct_lost <- NA
#   combined_results$adduct_check <- NA
# 
#   standardize_formula <- function(formula) {
#     if (is.na(formula)) return(NA)
# 
#     temp <- gsub("([A-Z][a-z]*)", "\\1@", formula, perl = TRUE)
# 
#     temp <- gsub("(@)(\\d+)", "\\2", temp, perl = TRUE)
# 
#     temp <- gsub("@", "1", temp, perl = TRUE)
# 
#     if (grepl("[A-Za-z]$", temp)) {
#       temp <- paste0(temp, "1")
#     }
#     return(temp)
#   }
# 
# 
#   extract_deduct_part <- function(adduct) {
#     if (!grepl("-(?=[^+]+[\\+\\]])", adduct, perl = TRUE)) {
#       return(NA)
#     }
#     deduct_part <- sub(".*?-([^+\\]]+?)(?:[\\+\\]]|$).*", "\\1", adduct, perl = TRUE)
#     if (grepl("^[0-9]+", deduct_part)) {
#       num <- regmatches(deduct_part, regexpr("^[0-9]+", deduct_part))
#       elem <- sub("^[0-9]+", "", deduct_part)
#       deduct_part <- paste0(elem, num)
#     }
#     return(deduct_part)
#   }
# 
#   combined_results$adduct_lost <- sapply(1:nrow(combined_results), function(i) {
#     current_adduct <- ifelse(!is.na(combined_results$adduct[i]),
#                              as.character(combined_results$adduct[i]),
#                              combined_results$assume_adduct_name[i])
#     if (!is.na(current_adduct)) extract_deduct_part(current_adduct) else NA
#   })
#   
# 
#   combined_results$Formula_std <- sapply(combined_results$Formula, standardize_formula)
#   combined_results$adduct_lost <- sapply(combined_results$adduct_lost, standardize_formula)
# 
#   for (i in 1:nrow(combined_results)) {
# 
#     current_formula <- combined_results$Formula_std[i]
#     current_deduct <- combined_results$adduct_lost[i]
# 
#     if (!is.na(current_formula) && !is.na(current_deduct)) {
# 
#       combined_results$adduct_check[i] <- check_ded(current_formula, current_deduct)
#     }
#   }
# 
#   combined_results <- combined_results[is.na(combined_results$adduct_check) | 
#                                          combined_results$adduct_check != TRUE, ]
# 
#   columns_to_remove <- c("adduct_lost", "adduct_check", "Formula_std")
#   combined_results <- combined_results[, !(names(combined_results) %in% columns_to_remove)]
#   combined_results <- combined_results[,c(1,4,5,7,8,6,10,9,15,16,11,12,13,14,17)]
#   names(combined_results) <- c("mz","single_ios","adduct","adduct_mark","assume_adduct","netural_mass","MW","mass_error_ppm","isotope_score","annotation_score","name","formula","KEGG","HMDB","LIPIDMAPS")
# 
#   combined_results <- combined_results[!is.na(combined_results$name), ]
# 
#   combined_results <- combined_results %>%
#     mutate(
#       annotation_score = round(annotation_score, 2),
#       mass_error_ppm = round(mass_error_ppm, 2),
#       isotope_score = round(isotope_score, 2)
#     )
# 
#   combined_results <- combined_results %>%
#     mutate(across(everything(), ~ {
#       x <- as.character(.)
#       x <- ifelse(is.na(x) | x == "NA", "-", x)
#       x <- gsub(" ", "-", x)
#       return(x)
#     }))
#   return(combined_results)
# })

identi_table <- eventReactive(input$goButton, {
  withProgress(message = 'Identification in progress', value = 0, {
    #### 1. Parameters ------------------------------------------------------------------------
    incProgress(0.05, detail = "Loading parameters")
    database_name <- input$database_select 
    adduct_name <- input$adduct_ion 
    MS1_accuracy <- input$MS1_match_tolerance # ppm
    
    ###### 2. Files for annotation ------------------------------------------------------------
    incProgress(0.1, detail = "Preparing annotation files")
    end_deal_data0 <- end_deal_data1()
    
    # 2.2 Store isotope peaks in a list
    iso_data <- end_deal_data0 %>% 
      filter(!is.na(single_ios))
    
    iso_list <- iso_data %>%
      group_by(single_ios) %>%
      group_map(~ {
        tibble(
          mz = .x$mz,
          median_intensity = .x$median_intensity
        )
      })
    names(iso_list) <- sapply(iso_list, function(df) df$mz[1])
    
    # 2.3 Normalize isotope peak intensities
    iso_list_norm <- map(iso_list, ~ {
      max_intensity <- max(.x$median_intensity)
      .x %>%
        mutate(normalized_intensity = median_intensity / max_intensity * 100) %>%
        select(mz, median_intensity, normalized_intensity)  
    })
    
    non_iso_data <- end_deal_data0 %>%
      filter(is.na(single_ios) | mz == single_ios)
    
    # 2.4 Split into adduct-containing and non-adduct data
    adduct_data <- non_iso_data %>%
      filter(!is.na(adduct_mark)) 
    no_adduct_data <- non_iso_data %>%
      filter(is.na(adduct_mark))
    
    #### 3. Database preparation and identification functions ---------------------------------
    incProgress(0.15, detail = "Preparing databases")
    # 3.1 Load combined database
    Combind_database <- read.delim("www/Combind_database.txt") 
    
    if (!is.null(input$database_file)) {
      db <- user_database() 
      db$User_Database <- paste("USER", c(1:nrow(db)), sep = "_") 
      
      Combind_database$User_Database <- ""
      for(i in 1:nrow(db)) {
        current_row <- db[i, ]
        matched_rows <- integer(0)  
        if(!is.na(current_row$HMDB) && current_row$HMDB != "") {
          matched_rows <- which(Combind_database$HMDB == current_row$HMDB)
        }
        
        if(length(matched_rows) == 0 && 
           !is.na(current_row$LIPIDMAPS) && current_row$LIPIDMAPS != "") {
          matched_rows <- which(Combind_database$LIPIDMAPS == current_row$LIPIDMAPS)
        }
        
        if(length(matched_rows) == 0 && 
           !is.na(current_row$KEGG) && current_row$KEGG != "") {
          matched_rows <- which(Combind_database$KEGG == current_row$KEGG)
        }
        
        if(length(matched_rows) > 0) {
          Combind_database$User_Database[matched_rows] <- current_row$User_Database
        } else {
          new_row <- data.frame(
            MW = current_row$MW,
            Name = current_row$Name,
            Formula = current_row$Formula,
            KEGG = current_row$KEGG,
            HMDB = current_row$HMDB,
            LIPIDMAPS = current_row$LIPIDMAPS,
            User_Database = current_row$User_Database
          )
          Combind_database <- rbind(Combind_database, new_row)
        }
      }
    }
    
    # 3.2 Filter database based on selection
    filter_database <- function(data, db_names) {
      valid_rows <- integer(0)
      
      for (db in db_names) {
        if (!is.null(db)) {
          non_na_rows <- which(!is.na(data[[db]]) & 
                                 (data[[db]] != ""))  
          valid_rows <- c(valid_rows, non_na_rows)
        }
      }
      
      valid_rows <- unique(sort(valid_rows))
      
      if (length(valid_rows) > 0) {
        return(data[valid_rows, ])
      } else {
        return(data[0, ])
      }
    }
    
    Annotation_database <- filter_database(Combind_database, database_name)
    if (!is.null(input$database_file)) {
      Annotation_database <- Annotation_database[, -7]
    }
    
    # 3.3 Prepare identification functions
    # 3.3.1 MS1 database matching
    MS1_mass_match <- function(mass, MS1_accuracy, database = Annotation_database) {
      lapply(mass, function(current_mass) {
        matches <- database %>%
          mutate(
            mass_error_da = MW - current_mass,
            mass_error_ppm = mass_error_da / current_mass * 1e6
          ) %>%
          filter(abs(mass_error_ppm) <= MS1_accuracy)
        
        if(nrow(matches) > 0) {
          matches <- matches %>%
            mutate(netural_mass = current_mass) %>%
            arrange(abs(mass_error_ppm))  
        }
        
        if (nrow(matches) == 0) {
          return(tibble(
            netural_mass = current_mass,
            mass_error_ppm = NA_real_,
            MW = NA_real_,
            Name = NA_character_,
            Formula = NA_character_,
            KEGG = NA_character_,
            HMDB = NA_character_,
            LIPIDMAPS = NA_character_
          ))
        }
        matches
      }) %>%
        bind_rows()  
    }
    
    # 3.3.2 Isotope pattern scoring
    iso_score_function <- function(mz, adduct, formula) {
      data(isotopes) 
      pattern <- isopattern(
        isotopes,
        formula,
        threshold = 0.1,
        plotit = FALSE,
        charge = FALSE,
        emass = 0.00054858,
        algo = 1
      )
      
      mz1 <-  pattern[[formula]][, "m/z"]
      intensity <-  pattern[[formula]][, "abundance"]
      
      mz1 <- as.numeric(mass2mz(mz1, adduct))
      theoretical_peaks <- data.frame(mz = mz1, intensity = intensity)
      
      actual_peaks <- as.data.frame(iso_list_norm[[as.character(mz)]])
      
      if (is.null(actual_peaks)) {
        stop("No matching actual peak found for the given mz.")
      }
      actual_peaks <- data.frame(
        mz = as.numeric(actual_peaks$mz),
        intensity = actual_peaks$normalized_intensity
      )
      
      similarity_score <- msentropy_similarity(
        as.matrix(theoretical_peaks),
        as.matrix(actual_peaks),
        ms2_tolerance_in_da = 1
      )
      
      theoretical_peaks$data_set <- "Theoretical"
      actual_peaks$data_set <- "Actual"
      theoretical_peaks <- theoretical_peaks %>%
        mutate(type = "Theoretical",
               position = "top")
      
      actual_peaks <- actual_peaks %>%
        mutate(type = "Actual",
               intensity = -intensity, 
               position = "bottom")
      
      combined_peaks <- bind_rows(theoretical_peaks, actual_peaks)
      
      p <- ggplot(combined_peaks, aes(x = mz)) +
        geom_segment(
          aes(xend = mz, y = 0, yend = intensity, color = type),
          linewidth = 0.7,
          alpha = 0.8
        ) +
        geom_hline(yintercept = 0, color = "gray50", linewidth = 0.5) +
        scale_color_manual(values = c("Theoretical" = "#1f77b4", "Actual" = "#ff7f0e")) +
        scale_y_continuous(
          name = "Relative Intensity",
          breaks = function(lims) {
            top_breaks <- pretty(lims[lims > 0], n = 5)
            bottom_breaks <- -rev(pretty(-lims[lims < 0], n = 5))
            c(bottom_breaks, top_breaks)
          },
          labels = function(x) ifelse(x < 0, -x, x)  
        ) +
        labs(
          x = "m/z",
          title = paste(mz, adduct, formula, similarity_score, sep = " "),
          color = "Spectrum"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          panel.grid.major = element_line(color = "gray90"),
          panel.grid.minor = element_blank(),
          legend.position = "top",
          plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.title = element_text(face = "bold")
        ) 
      
      result <- list(similarity_score = similarity_score, plot = p)
      return(result)
    }
    
    #### 4. Identify adduct_data (with known adducts) -----------------------------------------
    incProgress(0.3, detail = "Identifying adduct features")
    # 4.1 Get unique neutral masses
    adduct_data <- adduct_data %>%
      mutate(netural_mass = as.numeric(netural_mass))
    adduct_data_netural_mass <- adduct_data %>%
      select(netural_mass) %>%
      distinct() %>%
      arrange(netural_mass)
    
    # 4.2 Perform database matching
    adduct_data_match_results <- MS1_mass_match(adduct_data_netural_mass$netural_mass, MS1_accuracy = MS1_accuracy)
    
    # 4.3 Merge results (keep all matches)
    adduct_data_merged_results <- adduct_data %>%
      left_join(adduct_data_match_results, by = "netural_mass", relationship = "many-to-many") 
    
    # 4.4 Score isotope patterns
    adduct_data_merged_results <- adduct_data_merged_results %>%
      mutate(info_paste = paste(mz, adduct, Formula, sep = " | "))
    
    addcut_isotope_peak <- adduct_data_merged_results %>%
      select(mz, adduct, single_ios, Formula, info_paste) %>%
      filter(!is.na(single_ios) & !is.na(Formula)) %>%
      distinct(info_paste, .keep_all = TRUE)
    
    if (nrow(addcut_isotope_peak) > 0) {
      for (i in 1:nrow(addcut_isotope_peak)) {
        row <- addcut_isotope_peak[i, ]
        if (!is.na(row$single_ios) && !is.na(row$Formula)) {
          tryCatch({
            similarity_score <- iso_score_function(
              mz = as.numeric(row$single_ios),
              adduct = as.character(row$adduct),
              formula = as.character(row$Formula)
            )
            addcut_isotope_peak$isotope_similarity_score[i] <- similarity_score$similarity_score
          }, error = function(e) {
            warning(paste("Error processing row", i, ":", e$message))
            addcut_isotope_peak$isotope_similarity_score[i] <- NA
          })
        }
      }
      
      adduct_data_merged_results <- adduct_data_merged_results %>%
        left_join(addcut_isotope_peak %>% select(info_paste, isotope_similarity_score), 
                  by = "info_paste")
      
      adduct_data_merged_results <- adduct_data_merged_results %>%
        select(-info_paste)
    } else {
      adduct_data_merged_results$isotope_similarity_score <- NA
    }
    
    # 4.5 Calculate overall annotation score
    adduct_data_merged_results$annotation_score <- NA
    
    for (i in 1:nrow(adduct_data_merged_results)) {
      if (is.na(adduct_data_merged_results$Name[i])) next
      if (!is.na(adduct_data_merged_results$mass_error_ppm[i])) {
        ppm_abs <- abs(adduct_data_merged_results$mass_error_ppm[i])
        ppm_score <- 100 * (1 - ppm_abs / MS1_accuracy)
        ppm_score <- max(0, min(100, ppm_score))
      } else {
        ppm_score <- 0
      }
      if (!is.na(adduct_data_merged_results$isotope_similarity_score[i])) {
        iso_score <- adduct_data_merged_results$isotope_similarity_score[i] * 100
      } else {
        iso_score <- 0
      }
      adduct_data_merged_results$annotation_score[i] <- 
        (ppm_score * 0.5) + (iso_score * 0.3) + (100 * 0.2)
    }
    
    #### 5. Identify non_adduct_data ---------------------------------------------------------
    incProgress(0.6, detail = "Identifying non-adduct features")
    # 5.1 Calculate neutral masses
    no_adduct_data <- no_adduct_data %>%
      mutate(mz = as.numeric(mz))
    
    mass_results <- mz2mass(no_adduct_data$mz, adduct_name)
    
    no_adduct_data_netural_mass <- no_adduct_data %>%
      select(mz) %>%
      bind_cols(as_tibble(mass_results)) %>% 
      pivot_longer(
        cols = -mz,
        names_to = "assume_adduct_name",
        values_to = "calculated_netural_mass"
      )
    
    # 5.2 Database matching with calculated masses
    no_adduct_match_results <- MS1_mass_match(no_adduct_data_netural_mass$calculated_netural_mass, MS1_accuracy = MS1_accuracy)
    
    # 5.3 Merge results
    no_adduct_data <- no_adduct_data %>% mutate(mz = as.numeric(mz))
    no_adduct_data_netural_mass <- no_adduct_data_netural_mass %>% mutate(mz = as.numeric(mz))
    names(no_adduct_match_results)[9] <- "calculated_netural_mass"
    
    no_adduct_match_results <- no_adduct_match_results %>%
      filter(!is.na(Name))
    
    merged_step1 <- no_adduct_data %>%
      left_join(no_adduct_data_netural_mass, by = "mz")
    
    no_adduct_merged_results <- merged_step1 %>%
      left_join(no_adduct_match_results, 
                by = c("calculated_netural_mass" = "calculated_netural_mass"))
    
    # 5.4 Score isotope patterns
    no_adduct_merged_results <- no_adduct_merged_results %>%
      mutate(info_paste = paste(mz, assume_adduct_name, Formula, sep = " | "))
    
    no_addcut_isotope_peak <- no_adduct_merged_results %>%
      select(mz, assume_adduct_name, single_ios, Formula, info_paste) %>%
      filter(!is.na(single_ios) & !is.na(Formula)) %>%
      distinct(info_paste, .keep_all = TRUE)
    
    if (nrow(no_addcut_isotope_peak) > 0) {
      for (i in 1:nrow(no_addcut_isotope_peak)) {
        row <- no_addcut_isotope_peak[i, ]
        if (!is.na(row$single_ios) && !is.na(row$Formula)) {
          tryCatch({
            similarity_score <- iso_score_function(
              mz = as.numeric(row$single_ios),
              adduct = as.character(row$assume_adduct_name),
              formula = as.character(row$Formula)
            )
            no_addcut_isotope_peak$isotope_similarity_score[i] <- similarity_score$similarity_score
          }, error = function(e) {
            warning(paste("Error processing row", i, ":", e$message))
            no_addcut_isotope_peak$isotope_similarity_score[i] <- NA
          })
        }
      }
      
      no_adduct_merged_results <- no_adduct_merged_results %>%
        left_join(no_addcut_isotope_peak %>% select(info_paste, isotope_similarity_score), 
                  by = "info_paste")
      
      no_adduct_merged_results <- no_adduct_merged_results %>%
        select(-info_paste)
    } else {
      no_adduct_merged_results$isotope_similarity_score <- NA
    }
    
    # 5.5 Calculate overall annotation score
    no_adduct_merged_results$annotation_score <- NA
    for (i in 1:nrow(no_adduct_merged_results)) {
      if (is.na(no_adduct_merged_results$Name[i])) next
      if (!is.na(no_adduct_merged_results$mass_error_ppm[i])) {
        ppm_abs <- abs(no_adduct_merged_results$mass_error_ppm[i])
        ppm_score <- 100 * (1 - ppm_abs / MS1_accuracy) 
        ppm_score <- max(0, min(100, ppm_score))
      } else {
        ppm_score <- 0
      }
      
      if (!is.na(no_adduct_merged_results$isotope_similarity_score[i])) {
        iso_score <- no_adduct_merged_results$isotope_similarity_score[i] * 100
      } else {
        iso_score <- 0
      }
      
      no_adduct_merged_results$annotation_score[i] <- 
        (ppm_score * 0.5) + (iso_score * 0.3) + (0 * 0.2)
    }
    
    ### 6. Merge and output results ----------------------------------------------------------
    incProgress(0.8, detail = "Merging results")
    adduct_data_merged_results$assume_adduct_name <- NA
    adduct_data_merged_results$mz <- as.numeric(adduct_data_merged_results$mz)
    
    adduct_data_merged_results <- adduct_data_merged_results %>%
      select(mz, miss_ratio, median_intensity, single_ios, adduct, netural_mass, 
             adduct_mark, assume_adduct_name, calculated_netural_mass = netural_mass,
             mass_error_ppm, MW, Name, Formula, KEGG, HMDB, 
             isotope_similarity_score, annotation_score)
    no_adduct_merged_results <- subset(no_adduct_merged_results, select = -netural_mass)
    
    combined_results <- bind_rows(adduct_data_merged_results, no_adduct_merged_results)
    
    ### 7. Remove impossible adduct forms -----------------------------------------------------
    incProgress(0.9, detail = "Validating adducts")
    combined_results$adduct_lost <- NA
    combined_results$adduct_check <- NA
    
    standardize_formula <- function(formula) {
      if (is.na(formula)) return(NA)
      temp <- gsub("([A-Z][a-z]*)", "\\1@", formula, perl = TRUE)
      temp <- gsub("(@)(\\d+)", "\\2", temp, perl = TRUE)
      temp <- gsub("@", "1", temp, perl = TRUE)
      if (grepl("[A-Za-z]$", temp)) {
        temp <- paste0(temp, "1")
      }
      return(temp)
    }
    
    extract_deduct_part <- function(adduct) {
      if (!grepl("-(?=[^+]+[\\+\\]])", adduct, perl = TRUE)) {
        return(NA)
      }
      deduct_part <- sub(".*?-([^+\\]]+?)(?:[\\+\\]]|$).*", "\\1", adduct, perl = TRUE)
      if (grepl("^[0-9]+", deduct_part)) {
        num <- regmatches(deduct_part, regexpr("^[0-9]+", deduct_part))
        elem <- sub("^[0-9]+", "", deduct_part)
        deduct_part <- paste0(elem, num)
      }
      return(deduct_part)
    }
    
    combined_results$adduct_lost <- sapply(1:nrow(combined_results), function(i) {
      current_adduct <- ifelse(!is.na(combined_results$adduct[i]),
                               as.character(combined_results$adduct[i]),
                               combined_results$assume_adduct_name[i])
      if (!is.na(current_adduct)) extract_deduct_part(current_adduct) else NA
    })
    
    combined_results$Formula_std <- sapply(combined_results$Formula, standardize_formula)
    combined_results$adduct_lost <- sapply(combined_results$adduct_lost, standardize_formula)
    
    for (i in 1:nrow(combined_results)) {
      current_formula <- combined_results$Formula_std[i]
      current_deduct <- combined_results$adduct_lost[i]
      
      if (!is.na(current_formula) && !is.na(current_deduct)) {
        combined_results$adduct_check[i] <- check_ded(current_formula, current_deduct)
      }
    }
    
    combined_results <- combined_results[is.na(combined_results$adduct_check) | 
                                           combined_results$adduct_check != TRUE, ]
    
    columns_to_remove <- c("adduct_lost", "adduct_check", "Formula_std")
    combined_results <- combined_results[, !(names(combined_results) %in% columns_to_remove)]
    combined_results <- combined_results[, c(1,4,5,7,8,6,10,9,15,16,11,12,13,14,17)]
    names(combined_results) <- c("mz","single_ios","adduct","adduct_mark","assume_adduct","netural_mass","MW","mass_error_ppm","isotope_score","annotation_score","name","formula","KEGG","HMDB","LIPIDMAPS")
    
    combined_results <- combined_results[!is.na(combined_results$name), ]
    
    combined_results <- combined_results %>%
      mutate(
        annotation_score = round(annotation_score, 2),
        mass_error_ppm = round(mass_error_ppm, 2),
        isotope_score = round(isotope_score, 2)
      )
    
    combined_results <- combined_results %>%
      mutate(across(everything(), ~ {
        x <- as.character(.)
        x <- ifelse(is.na(x) | x == "NA", "-", x)
        x <- gsub(" ", "-", x)
        return(x)
      }))
    
    incProgress(1, detail = "Completed")
    return(combined_results)
  })
})


output$identi_table <- renderDataTable({
  identi_table()
})

output$downloadidentiData <- downloadHandler(
  filename <- function() {
    paste0("Annotation_information.txt")
  },
  content <- function(file) {
    write_delim(identi_table(),file,delim="\t")
  }
)

output$downloadmeasureData <- downloadHandler(
  filename <- function() {
    paste0("Processed_peak_table.txt")
  },
  content <- function(file) {
    write_delim(peakData_remove_noise(),file,delim="\t")
  }
)


output$identification_pie <- renderPlot({
  req(identi_table(), end_deal_data1())

  identi <- identi_table()
  end_data <- end_deal_data1()

  identified <- length(unique(identi$mz))
  total <- length(unique(end_data$mz))
  unidentified <- total - identified

  data <- data.frame(
    Category = c("Identified", "Unidentified"),
    Count = c(identified, unidentified)
  )

  data$Percentage <- round(data$Count / sum(data$Count) * 100, 1)
  data$Label <- paste0(data$Category, "\n", data$Count, " (", data$Percentage, "%)")

  ggplot(data, aes(x = "", y = Count, fill = Category)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar("y", start = 0) +
    geom_text(aes(label = Label), 
              position = position_stack(vjust = 0.5),
              color = "white", size = 5) +
    scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) +
    theme_void() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5, size = 16)) +
    ggtitle("Metabolite Identification Status")
})

output$identification_distribution <- renderPlot({
  req(identi_table())
  
  identi <- identi_table()

  k <- table(identi$mz)

  kk <- as.data.frame(table(k))
  colnames(kk) <- c("Num_Identifications", "Num_mz")

  kk$Num_Identifications <- as.numeric(as.character(kk$Num_Identifications))

  ggplot(kk, aes(x = Num_Identifications, y = Num_mz)) +
    geom_bar(stat = "identity", fill = "#2ca02c", alpha = 0.8) +
    geom_text(aes(label = Num_mz), 
              vjust = -0.5, size = 4, color = "black") +
    labs(title = "Distribution of Identification Results per m/z",
         x = "Number of Identifications per m/z",
         y = "Number of m/z Features") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, hjust = 0.5),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    scale_x_continuous(breaks = unique(kk$Num_Identifications)) +
    ylim(0, max(kk$Num_mz) * 1.1)
})