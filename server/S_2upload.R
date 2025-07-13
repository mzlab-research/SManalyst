# read uploaded files-------------------------------------
peakData <- reactive({
  req((!is.null(input$peakfile)) | (input$use_example > 0))
  
  if (input$use_example > 0) {
    readRDS("./example_data/peakData.rds")
  } else if (!is.null(input$peakfile)) {
    file_path <- input$peakfile$datapath
    
    withProgress(message = 'Reading and validating file...', value = 0, {
      incProgress(0.3, detail = "Reading file...")
      sp <- tryCatch(
        read.table(file_path, header = TRUE, sep = "\t", check.names = FALSE),
        error = function(e) {
          validate(paste("File reading error:", e$message))
          return(NULL)
        }
      )
      
      incProgress(0.5, detail = "Validating data structure...")
      validate(
        need(ncol(sp) >= 3, 
             "Data must have at least 3 columns (X, Y and at least one mz column)"),
        
        need(names(sp)[1] == "X" && names(sp)[2] == "Y",
             "First two columns must be named 'X' and 'Y'")
      )
      validate(
        need(is.numeric(sp$X), "X column must be numeric"),
        need(is.numeric(sp$Y), "Y column must be numeric"),
        
        need(all(!is.na(sp$X)), "X column contains missing values"),
        need(all(!is.na(sp$Y)), "Y column contains missing values"),
        
        need(all(sp$X != 0), "X column contains zeros"),
        need(all(sp$Y != 0), "Y column contains zeros")
      )
      mz_cols <- names(sp)[-(1:2)]
      are_mz_numeric <- suppressWarnings(!is.na(as.numeric(mz_cols)))
      validate(
        need(length(mz_cols) >= 1, 
             "At least one mz column required after X and Y columns"),
        
        need(all(are_mz_numeric), 
             paste("Invalid m/z column names:",
                   paste(mz_cols[!are_mz_numeric], collapse = ", ")))
      )
      
      mz_data <- sp[, -(1:2), drop = FALSE]
      are_numeric_cols <- sapply(mz_data, is.numeric)
      validate(
        need(all(are_numeric_cols), 
             paste("Non-numeric values found in m/z columns:",
                   paste(names(mz_data)[!are_numeric_cols], collapse = ", ")))
      )
      incProgress(0.8, detail = "Processing data...")
      sp[sp == 0] <- NA
      row.names(sp) <- paste(sp$X, sp$Y, sep = "_")
      
      incProgress(1, detail = "Data validation complete!")
    })
    sp  
  } else {
    NULL
  }
})

output$peakfile_validation_output <- renderPrint({
  tryCatch({
    sp <- peakData()
    cat("Validated input file!\n")
  }, error = function(e) {
    cat(e$message)
  })
})


#basis information output--------------------------------
output$basic_info <- renderTable({
  data <- peakData()
  req(ncol(data)>0)
  point_number <- nrow(data)
  peak_number <- ncol(data)-2
  number_of_rows <- length(unique(data$X))
  number_of_cols <- length(unique(data$Y))
  data.frame(Item=c("point_number","number_of_rows","number_of_cols","peak_number"),
             Number=c(point_number,number_of_rows,number_of_cols,round(peak_number,0))
  )
})

#location information------------------------
locs <- reactive({
  data <- peakData()
  req(ncol(data)>0)
  min_loc <- min(c(data$X,data$Y))
  max_loc <- max(c(data$X,data$Y))
  loc <- c(min_loc,max_loc)
  loc
})

#update ion selection--------------------------
observe({
  data <- peakData()
  req(ncol(data)>0)
  ions <- as.numeric(names(data)[-c(1,2)])
  updateSelectizeInput(session = getDefaultReactiveDomain(), "ion_select", choices = ions,selected = ions[1],server = TRUE)
})

#ion visualization output---------------------
output$specific_ion_plot <- renderPlot({
  req(input$ion_select) 
  data <- peakData()
  req(ncol(data)>0)
  ion <- as.numeric(input$ion_select)
  loc <- locs()
  n <- min(abs(as.numeric(names(data[,-c(1,2)]))-ion))
  w <- which(abs(as.numeric(names(data[,-c(1,2)]))-ion)==n)
  if (n < 0.1) {
    d <- data[,c(1,2,w+2)]
    names(d)[3] <- "intensity"
    p <- ggplot(d,aes(X,Y,colour=log10(intensity)))+geom_point()+scale_color_distiller(palette = "Spectral")+
      ggtitle(names(data)[w+2])+theme_bw()+coord_equal()
    p
  } else {
    p <- ggplot() + xlim(0, 10) + ylim(0, 10) +  
      geom_text(aes(x = 5, y = 5, label = paste(ion," is not found. The nearest is: ",names(data)[w],sep = "")),  
                color = "blue", size = 6) + 
      theme_void()+coord_equal()
    p
  }
})