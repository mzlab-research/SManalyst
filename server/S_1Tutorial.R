

output$downloadData <- downloadHandler(
  filename <- function() {
    paste("example_data","txt",sep = ".")
  },
  
  content <- function(file) {
    file.copy("example_data/example_data.txt", file)
  }
)