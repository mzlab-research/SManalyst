tabItem(tabName = "qc2",
        h2("QC3: Ion Intensity Distribution Assessment"),
        h4("Metabolite Ion Intensity Profiling"),
        p("This quality control evaluates the intensity characteristics of detected metabolic features:"),
        tags$ul(
          tags$li(strong("Per-ion analysis:"), " Median intensity across all pixels for each m/z feature"),
          tags$li(strong("Per-pixel analysis:"), " Median intensity across all ions for each spatial pixel")
        ),
        p(icon("chart-bar"), " Expected pattern: Intensity distribution with majority of features exceeding 10³ intensity units."),
        
        fluidRow(
          box(title = "Metabolite Ion Intensity Distribution",
              width = 6,
              plotOutput("mz_intensity_spectra",height = 300),
              p(em("Histogram showing median intensity values across all detected ions"))
          ),
          box(title = "Spatial Pixel Intensity Profile",
              width = 6,
              plotOutput("total_intensity_after_pioints_remove",height = 300),
              p(em("Median ion intensity distribution at each tissue location"))
          ),

              p(strong("Interpretation Note:"), "While low-intensity features aren't automatically removed, this QC helps identify potential issues:"),
              tags$ul(
                tags$li("Spatial intensity patterns may reveal technical artifacts"),
                tags$li("Extremely low-intensity ions (<10³) warrant caution in downstream analysis")
              )
          ),
          
          h2("QC4: Missing Value Pattern Analysis"),
          p("This assessment evaluates data completeness across two dimensions:"),
          tags$ul(
            tags$li(strong("Per-ion missingness:"), " Proportion of pixels where feature is undetected"),
            tags$li(strong("Per-pixel missingness:"), " Proportion of ions undetected at each location")
          ),
          
          fluidRow(
            box(title = "Ion-wise Missing Value Distribution",
                width = 6,
                plotOutput("mz_missing_distribution",height = 300),
                p(em("Histogram of missing value proportions across all m/z features"))
            ),
            box(title = "Spatial Missing Value Pattern",
                width = 6,
                plotOutput("sample_missing_distribution",height = 300),
                p(em("Tissue map showing proportion of undetected ions per pixel"))
            ),

                p(strong("Analytical Guidance:"), "High missingness may indicate:"),
                tags$ul(
                  tags$li(strong("Ion-level:"), " Low-abundance metabolites or detection sensitivity limits"),
                  tags$li(strong("Pixel-level:"), " Sample preparation artifacts or biological variability")
                )

          )
        )