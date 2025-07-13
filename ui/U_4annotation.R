tabItem(tabName = "annotation",
        fluidRow(
          h2("Metabolite Annotation"),
          box(title = "Metabolite Annotation Configuration",
              width = 6,
              h4("Database Selection and Matching Parameters"),
              fileInput("database_file", "Upload Custom Annotation Database", accept = ".txt"),
              p(em("Recommended: LC-MS/MS results from same sample type")),
              verbatimTextOutput("validation_output"),
              uiOutput("dynamic_checkboxes"),
              sliderInput("MS1_match_tolerance", "Mass Accuracy Tolerance (ppm):",
                          min = 0, max = 100, value = 10, step = 1),
              p(icon("lightbulb-o"), " Lower values increase specificity but may miss valid matches"),
              actionButton("goButton", "Execute Annotation")
          ),
          box(title = "Database Format Specification",
              width = 6,
              h4("Required Structure for Custom Databases"),
              tags$p("Custom databases must contain these essential columns:"),
              tags$ul(
                tags$li(strong("name:"), " Metabolite name"),
                tags$li(strong("formula:"), " Molecular formula"),
                tags$li(strong("MW:"), " Monoisotopic mass"),
                tags$li(strong("KEGG/HMDB/LIPIDMAPS:"), " Identifier(s)")
              ),
              tags$img(src = "database_format.png", width = "80%", style = "border: 1px solid #ddd; padding: 5px;")
              ),
          ),
          
          fluidRow(
            h3("Comprehensive Annotation Report"),
            column(
              width = 6,
              h4("Metabolite Identification Summary"),
              plotOutput("identification_pie", height = "500px"),
              p(em("Proportion of ions with confident vs. tentative identifications"))
            ),
            column(
              width = 6,
              h4("Annotation Number Distribution"),
              plotOutput("identification_distribution", height = "500px"),
              p(em("Higher scores indicate more reliable annotations"))
            )),
            
            fluidRow(
              div(class = "alert alert-info",
                  h4("Annotation Confidence Scoring System"),
                  p("SManalyst calculates a multi-evidence confidence score (0-100) using:"),
                  tags$ul(
                    tags$li(strong("Mass accuracy (50%):"), " Normalized ppm error (1-100 scale)"),
                    tags$li(strong("Isotope match (30%):"), " Theoretical vs observed isotopic pattern similarity"),
                    tags$li(strong("Adduct evidence (20%):"), " 100 points if adduct form confirmed, else 0")
                  )
              ),
              dataTableOutput("identi_table")
            ),
            
            fluidRow(
              box(title = "Annotation Report Columns:",
                  div(style = "font-size: 12px; background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-top: 15px;",
                      tags$p(tags$strong("mz"), ": Measured mass-to-charge ratio"),
                      tags$p(tags$strong("single_ios"), ": Monoisotopic peak assignment ('-' if not applicable)"),
                      tags$p(tags$strong("adduct"), ": Experimentally determined adduct form ('-' if unconfirmed)"),
                      tags$p(tags$strong("adduct_mark"), ": Unique identifier for metabolite-adduct groups"),
                      tags$p(tags$strong("assume_adduct"), ": Hypothesized adduct form based on annotation ('-' if experimentally confirmed)"),
                      tags$p(tags$strong("netural_mass"), ": Calculated neutral mass (Da)"),
                      tags$p(tags$strong("MW"), ": Database monoisotopic mass (Da)"),
                      tags$p(tags$strong("mass_error_ppm"), ": Mass match error (parts per million)"),
                      tags$p(tags$strong("isotope_score"), ": Isotopic pattern similarity score ('-' if no isotopes detected)"),
                      tags$p(tags$strong("annotation_score"), ": Composite confidence score (0-100)"),
                      tags$p(tags$strong("name"), ": Identified metabolite name"),
                      tags$p(tags$strong("formula"), ": Molecular formula"),
                      tags$p(tags$strong("KEGG/HMDB/LIPIDMAPS"), ": Database identifiers")
                  )),
              box(title = "Results Table Download:",
                  downloadButton("downloadidentiData", label = "Download Complete Annotation Report", class = "btn-primary"),
                  p(),
                  downloadButton("downloadmeasureData", label = "Download Processed Feature Matrix", class = "btn-default")
              ))
)#tab panel