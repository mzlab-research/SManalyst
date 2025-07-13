tabItem(tabName = "statistics_differential",
        fluidRow(
          h2("Region-Specific Differential Analysis"),
          h3("Define Regions of Interest (ROIs)"),
          p("Manually delineate tissue regions for comparisons:"),
          tags$ol(
            tags$li(strong("Selection method:"), " Use lasso (default) or rectangular tool to outline ROIs"),
            tags$li(strong("Group assignment:"), " Classify each ROI as Case or Control group"),
            tags$li(strong("Best practices:"), " Define ≥3 ROIs per group for statistical robustness"),
            tags$li(strong("Error correction:"), " Clear incorrectly drawn regions before finalizing")
          ),
          box(title = "Interactive Tissue Section Map",
              width = 6,
              plotlyOutput("diff_scatter_plot")
          ),
          box(title = "Selected ROIs Visualization",
              width = 6,
              plotOutput("diff_selection_plot", height = 300)
          )),
          
          fluidRow(
            box(title = "ROI Management",
                width = 6,
                actionButton("add_to_group", "Assign Selected ROI to Group"),
                p(em("Assigns current selection to Case/Control group")),
                actionButton("diff_clear_selection", "Clear Current Selection"),
                actionButton("diff_finish_selection", "Finalize ROI Definitions")
            ),
            box(title = "ROI Metadata Summary",
                width = 6,
                tableOutput("diff_selection_table")
            )),
            
            fluidRow(
              h3("Configure Differential Comparison"),
              box(title = "Comparison Group Assignment",
                  width = 6,
                  selectInput("case_group", "Select Case Group ROIs:", choices = NULL, multiple = TRUE),
                  selectInput("control_group", "Select Control Group ROIs:", choices = NULL, multiple = TRUE),
                  hr(),
                  h4("Statistical Thresholds"),
                  numericInput("Fold_change_maunal","Fold Change Threshold:",min=1,max=10,value=2,step=0.1),
                  numericInput("p_value_adjust_manula","Adjusted p-value Threshold:",min=0.001,max=1,value=0.01,step=0.01)
              ),
              box(
                title ="Final Comparison Groups",
                width = 6,
                plotOutput("group_setting_maual",height = "300px")
              )),
              
              fluidRow(
                box(title = "Differential Metabolite Volcano Plot",
                    width = 6,
                    p("Statistical summary of region-specific metabolites:"),
                    plotOutput("diff_volcano", height = 280,width = 500),
                    downloadButton("download_volcano_data_manual", "Export Differential Features")
                ),
                box(
                  title = "Spatial Distribution of Differential Metabolites",
                  width = 6,
                  style = "height: 400px",
                  selectInput("diff_ion_select_manual", "Select Differential Metabolite:", choices = NULL,selected = NULL),
                  plotOutput("diff_ion_plot_maunal",height = "300px")
                )
              ))