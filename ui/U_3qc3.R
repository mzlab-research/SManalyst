tabItem(tabName = "qc3",
        fluidRow(#f3
          h2("Metabolite Ion Characterization: Isotope and Adduct Identification"),
          h4("Isotopologue Detection Workflow"),
          p("SManalyst implements a robust two-stage isotope identification process as described in our methodology:"),
          tags$ol(
            tags$li(strong("Mass-based filtering:"), " Identify potential isotopologues using theoretical mass differences (M+1 ≈ 1.0035 Da) within user-defined ppm tolerance"),
            tags$li(strong("Spatial correlation validation:"), " Confirm true biological isotopologues through spatial pattern correlation analysis (Moran's I)")
          ),
          p(icon("lightbulb-o"), " Isotopologues should exhibit near-identical spatial distributions in biological samples."),
          
          box(title = "Isotopologue Identification Parameters",
              width = 6,
              sliderInput("iso_ppm_setting",              
                          "Mass Accuracy Tolerance (ppm):",          
                          min = 0,                 
                          max = 10,               
                          value = 5),
              sliderInput("iso_corr_cutoff",              
                          "Spatial Correlation Threshold:",          
                          min = 0,                 
                          max = 1,               
                          value = 0.5),
              actionButton("iso_adduct_calculate", "Execute Isotope & Adduct Analysis"),
              infoBoxOutput("isotope_ratio_box",width = 10),
              p(em("QC Metric: Percentage of ions with confirmed isotopologues"))
          ),
          box(title = "Isotopologue Spatial Validation",
              width = 6,
              selectInput("isotope_select", "Select Isotope Group for Visualization:", choices = NULL, multiple = FALSE),
              plotOutput("isotope_peaks_group_visualization", height = 300),
              p(em("Expected: Highly correlated spatial patterns between monoisotopic and M+1 peaks"))
          )),#f3
        
        fluidRow(
          box(title = "Isotopologue Distribution by Abundance",
              width = 12,
              plotOutput("Isotope_peaks_distribution",height = 300),
              p(em("Pattern Check: High-abundance ions should predominantly show isotope relationships"))
          )),
          
          fluidRow(#f2
            h4("Adduct Ion Characterization"),
            p("Adduct identification follows a similar two-step validation approach:"),
            tags$ol(
              tags$li(strong("Mass difference matching:"), " Compare observed mass differences to predefined adduct mass shifts"),
              tags$li(strong("Spatial correlation:"), " Validate through spatial pattern similarity analysis")
            ),
            p(icon("cog"), " Default adduct lists are provided for positive/negative modes; customizable based on experimental conditions."),
            
            box(title = "Adduct Identification Parameters",
                width = 6,
                selectInput("adduct_ion", "Select Adduct Forms for Detection:", choices = NULL, multiple = TRUE),
                sliderInput("add_ppm_setting",              
                            "Mass Accuracy Tolerance (ppm):",          
                            min = 0,                 
                            max = 10,               
                            value = 5),
                sliderInput("add_corr_cutoff",              
                            "Spatial Correlation Threshold:",          
                            min = 0,                 
                            max = 1,               
                            value = 0.5),
                infoBoxOutput("adduct_ratio_box",width = 10),
                p(em("Percentage of ions with confirmed adduct relationships"))
            ),
            box(title = "Adduct Pair Spatial Validation",
                width = 6,
                selectInput("adduct_select", "Select Adduct Group for Visualization:", choices = NULL, multiple = FALSE),
                plotOutput("adduct_peaks_group_visualization"),
                p(em("Expected: Near-identical spatial patterns for different adducts of same metabolite"))
            )
          ),#f2
          fluidRow(#f1
            box(title = "Adduct Relationship Visualization",
                width = 6,
                plotOutput("adduct_peaks_count"),
                p(em("Distribution of detected adduct types (e.g., [M+H]⁺, [M+Na]⁺ in positive mode)"))
            ),
            box(title = "Comprehensive Feature Information Export",
                width = 6,
                p("Download the complete metabolite feature annotation table containing:"),
                tags$ul(
                  tags$li(tags$strong("mz"), ": Detected mass-to-charge ratio"),
                  tags$li(tags$strong("miss_ratio"), ": Percentage of pixels where feature is undetected"),
                  tags$li(tags$strong("median_intensity"), ": Median intensity across all pixels"),
                  tags$li(tags$strong("single_ios"), ": Monoisotopic peak assignment (NA if none)"),
                  tags$li(tags$strong("adduct"), ": Detected adduct form (NA if none)"),
                  tags$li(tags$strong("netural_mass"), ": Calculated neutral mass (requires adduct identification)")
                ),
                p(icon("table"), " This comprehensive annotation table facilitates downstream biological interpretation."),
                downloadButton("download_peak_info", label = "Export Feature Annotation Table")
            )),#f1
        )#tab panel