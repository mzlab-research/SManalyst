tabItem(tabName = "pro3",
        fluidRow(
          h2("Process2 Noise Peak Removal  and QC2 Noise Ratio"),
          p("SManalyst employs spatial statistics to distinguish biological signals from technical noise:"),
          tags$ol(
            tags$li(strong("Spatial randomness test:"), " Uses spatstat's quadrat test to evaluate Complete Spatial Randomness (CSR)"),
            tags$li(strong("Noise score calculation:"), " -log₁₀(p-value) from CSR test (higher = less random)"),
            tags$li(strong("Threshold-based filtering:"), " Ions below threshold are classified as noise")
          ),
          p(icon("lightbulb-o"), " Biological signals typically show clustered distributions, while noise exhibits random spatial patterns."),
          
          box(title = "Noise Identification Parameters",
              width = 6,
              sliderInput("noise_cutoff",
                          "Set Noise Score Threshold:",
                          min = 0,
                          max = 100,
                          value = 30),
              p(em("Recommended: Start at 30 then adjust based on distribution")),
              actionButton("noise_calculate", "Calculate Noise Metrics"),
              hr(),
              h4("Noise Ion Statistics"),
              tableOutput("noise_ratio")
          ),
          box(title = "Noise Score Distribution",
              width = 6,
              plotOutput("noise_distribution",height = 300),
              p(em("Histogram showing noise scores - left of threshold = noise ions"))
          )),
          
          fluidRow(
            box(title ="Spatial Distribution Examples at Selected Threshold",
                width = 12,
                plotOutput("noise_score_selected")
            )
          )
        )