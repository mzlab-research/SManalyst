tabItem(tabName = "upload",
        fluidRow(
          h2("Data Upload"),
          box(title = "Data Upload and Initial Configuration",
              width = 8,
              style = "height: 500px",
              tabBox(
                tabPanel("Use Example Dataset",
                         h2("Explore with Preloaded Data"),
                         tags$p("Quickly evaluate SManalyst functionality using our curated mouse brain spatial metabolomics dataset."),
                         tags$p(strong("Dataset contains:"), "14,260 pixels, 3,044 m/z features (positive mode)"),
                         actionButton("use_example", "Load Example Dataset")
                ),
                tabPanel("Upload Your Data",
                         h2("Custom Data Analysis"),
                         tags$p(strong("Prerequisite:"), "Uploaded files must be in the required", 
                                tags$a(href = "#", onclick = "shinyjs.tab('Tutorial');", "feature matrix format")),
                         fileInput("peakfile", "Upload Feature Matrix File (.txt)", accept = ".txt"),
                         verbatimTextOutput("peakfile_validation_output"),
                         radioButtons("mode_select", "Select Ionization Mode:",
                                      choices = list("Negative Mode" = "neg", "Positive Mode" = "pos"),
                                      selected = "pos"),
                         tags$p(icon("info-circle"), " Mode selection ensures correct adduct identification during annotation.")
                )
              )
          ),
          box(title = "Data Overview",
              width = 4,
              style = "height: 500px",
              tags$p(strong("After upload, this panel will display:")),
              tags$ul(
                tags$li("Number of spatial pixels"),
                tags$li("Total detected m/z features"),
                tags$li("Coordinate range (X/Y)"),
                tags$li("Intensity distribution summary")
              ),
              tableOutput("basic_info")
          )
        ),
        fluidRow(
          box(title = "Metabolite Ion Selection",
              width = 4,
              style = "height: 600px",
              tags$p("Select an m/z feature to preview its spatial distribution:"),
              selectizeInput("ion_select", "Choose m/z Value:", 
                             choices = NULL, 
                             options = list(
                               placeholder = 'Start typing m/z value...',
                               maxOptions = 5000))
          ),
          box(title = "Spatial Distribution Visualization",
              width = 8,
              style = "height: 600px",
              tags$p("Ion intensity mapped to tissue coordinates"),
              plotOutput("specific_ion_plot")
          )
        )
)