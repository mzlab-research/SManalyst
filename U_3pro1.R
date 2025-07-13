tabItem(tabName = "pro1",
        fluidRow(
          h2("QC1: Background Signal Consistency Assessment"),
          h3("Define Tissue and Background Regions"),
          p("This critical step establishes reference regions for quality control. Follow these guidelines:"),
          tags$ol(
            tags$li(strong("Selection Method:"), " Use the interactive plot (left) to delineate regions - default lasso tool recommended"),
            tags$li(strong("Region Types:"), " Classify selections as either", strong("Background"), "(matrix/support areas) or", strong("Tissue"), "(biological sample)"),
            tags$li(strong("Best Practices:"), " Select 3-5 spatially distributed background regions and 3-5 representative tissue regions"),
            tags$li(strong("Error Correction:"), " Use 'Clear Selection' to remove incorrectly drawn areas")
          ),
          
          box(title = "Interactive Tissue Section Map",
              width = 6,
              style = "height: 600px; overflow-y: auto;",
              plotlyOutput("scatter_plot",height = 550)
          ),
          box(title = "Region Classification",
              width = 6,
              style = "height: 600px; overflow-y: auto;",
              actionButton("add_to_background", "Assign as Background Region"),
              p(em("(Background areas with minimal biological content)")),
              actionButton("add_to_tissue", "Assign as Tissue Region"),
              p(em("(Biological sample areas of interest)")),
              actionButton("clear_selection", "Clear Current Selection"),
              hr(),
              actionButton("finish_selection", "Finalize Region Definitions"),
              p(em("(Click after completing selections to proceed)"))
          )
        ),
        
        fluidRow(
          box(title = "Selected Regions Visualization",
              width = 6,
              style = "height: 500px; overflow-y: auto;",
              plotOutput("selection_plot"),
              p(strong("Color Legend:"), "Background = yellow, Tissue = green")
          ),
          box(title = "Region Metadata Summary",
              width = 6,
              style = "height: 500px; overflow-y: auto;",
              tableOutput("selection_table")
          )),
          
          fluidRow(
            h3("Spectral Comparison: Background vs Tissue"),
            p("Evaluate spectral characteristics to confirm proper region classification:"),
            tags$ul(
              tags$li(strong("Key Assessment 1:"), "Significant intensity differences between background and tissue spectra?"),
              tags$li(strong("Key Assessment 2:"), "Polymer contamination indicators in either spectrum?"),
              tags$li(strong("Key Assessment 3:"), "Consistent spectral patterns across background regions?")
            ),
            
            box(title = "Background Region Spectrum",
                width = 6,
                plotOutput("background_spectrum",height = 300),
                p(em("Expected: Low complexity, consistent pattern across regions"))
            ),
            box(title = "Tissue Region Spectrum",
                width = 6,
                plotOutput("tissue_spectrum",height = 300),
                p(em("Expected: Higher complexity, biologically relevant peaks"))
            )
          ),
          
          fluidRow(
            h3("Background Consistency Metrics"),
            p("SManalyst quantifies background signal stability using:"),
            tags$ol(
              tags$li(strong("Intensity Distribution:"), " Compares median intensity distributions across regions"),
              tags$li(strong("Inter-region Correlation:"), " Computes pairwise Pearson correlations between all background regions")
            ),
            p(icon("chart-line"), " Interpretation: Higher correlations (>0.85) indicate stable instrumentation during acquisition."),
            
            box(title = "Background Region Intensity Distribution",
                width = 6,
                plotOutput("background_boxplot",height = 300),
                p(em("Assesses signal uniformity across different background areas"))
            ),           
            box(title = "Inter-region Correlation Matrix",
                width = 6,
                plotOutput("background_correlation",height = 300),
                p(em("Values near 1.0 indicate high spectral consistency"))
            )
          )
        )


