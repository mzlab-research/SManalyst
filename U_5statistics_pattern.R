tabItem(tabName = "statistics_pattern",
        fluidRow(
          h2("Spatial Pattern Analysis of Metabolite Ions"),
          p("SManalyst employs the SpaGene algorithm to identify spatially co-expressed metabolite groups:"),
          tags$ol(
            tags$li(strong("Pattern discovery:"), " Clusters ions with similar spatial distribution profiles"),
            tags$li(strong("Spatial mapping:"), " Visualizes distribution patterns across tissue microenvironments")
          ),
          
          box(title = "Metabolite Spatial Expression Patterns",
              width = 6,
              plotOutput("spatial_pattern_plot"),
              p(em("Each cluster represents a distinct spatial expression pattern"))
          ),
          box(title = "Pattern Identification Parameters",
              width = 6,
              sliderInput("pattern_correlation_cutoff", 
                          "Set Spatial Pattern Correlation Threshold:",
                          min = 0, max = 1, value = 0.6, step = 0.1),
              p(em("Higher values create more stringent pattern definitions")),
              plotOutput("pattern_metabolite_count",height = 300),
              downloadButton("download_pattern_metabolite_count_data", "Export Pattern Statistics")
          ),
          
          box(title = "Pattern-Specific Metabolite Exploration",
              width = 6,
              p("Investigate metabolites driving each spatial pattern:"),
              selectInput("pattern_select", "Select Spatial Pattern Cluster:", 
                          choices = NULL, selected = NULL),
              selectInput("Pattern_specific_m_select", "Select Metabolite for Visualization:", 
                          choices = NULL, selected = NULL)
          ),
          box(title = "Metabolite Spatial Distribution",
              width = 6,
              plotOutput("patternspecific_distribution_plot",height = 300),
              p(em("Intensity plot showing tissue localization of selected metabolite"))
          )
        ))