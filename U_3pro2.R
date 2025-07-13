tabItem(tabName = "pro2",
        fluidRow(
          h2("Process1：Remove Bakcground Piexls:"),
          p("This critical preprocessing step identifies and removes non-biological background pixels using tissue-enriched ions:"),
          tags$ol(
            tags$li(strong("Tissue-specific ion selection:"), " Identify ions significantly elevated in tissue regions (fold change > 1)"),
            tags$li(strong("Total intensity calculation:"), " Sum intensities of tissue-enriched ions per pixel"),
            tags$li(strong("Threshold determination:"), " Set cutoff to separate tissue (high intensity) from background (low intensity)"),
            tags$li(strong("Pixel classification:"), " Remove background pixels while retaining all detected ions")
          ),
          p(icon("lightbulb-o"), " Note: This process removes only spatial pixels, not individual ions - preserving metabolic features for downstream analysis."),
          
          box(title = "Identification of Tissue-Enriched Ions",
              width = 6,
              plotOutput("volcano_background_vs_tissue",height = 300),
              p(em("Volcano plot showing ions significantly elevated in tissue regions (fold change > 1)"))
          ),
          box(title = "Spatial Distribution of Total Tissue Ion Intensity",
              width = 6,
              plotOutput("total_intensity_after_mz_remove",height = 300),
              p(em("Summed intensity of tissue-enriched ions at each pixel"))
          )
        ),
        
        fluidRow(
          p(strong("Intensity Threshold Setting Guide:")),
          tags$ul(
            tags$li("Examine the bimodal distribution in the histogram (below)"),
            tags$li("Set threshold at the valley between background (left peak) and tissue (right peak)"),
            tags$li("Adjust interactively to minimize misclassification")
          ),
          sliderInput("datapoint_cutoff", 
                      label = "Set Intensity Threshold for Tissue Classification:",
                      min = 0, 
                      max = 10, 
                      value = c(1), 
                      step = 0.01,
                      width = '90%')
        ),
        
        fluidRow(
          box(title = "Pixel Intensity Distribution Analysis",
              width = 6,
              plotOutput("total_intensity_hist",height = 300),
              p(em("Histogram showing pixel intensity distribution - optimal threshold separates bimodal peaks"))
          ),
          box(title = "Tissue-Background Classification Map",
              width = 6,
              plotOutput("bakcground_tissue_classify",height = 300),
              p(em("Spatial visualization: Tissue pixels (blue) vs. Background pixels (red)"))
          )
        )
)