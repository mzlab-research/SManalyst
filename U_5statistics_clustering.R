tabItem(tabName = "statistics_clustering",
        fluidRow(
          h2("Tissue Region Clustering via Metabolic Profiling"),
          p("SManalyst integrates four advanced clustering algorithms to identify metabolically distinct tissue regions:"),
          tags$ul(
            tags$li(strong("Seurat-LV:"), " Original Louvain algorithm (community detection)"),
            tags$li(strong("Seurat-LM:"), " Louvain algorithm with multilevel refinement"),
            tags$li(strong("Seurat-SLM:"), " Smart Local Moving algorithm"),
            tags$li(strong("UMAP-kmeans:"), " Uniform Manifold Approximation and Projection with K-means clustering")
          ),
          p(icon("lightbulb-o"), " Clustering resolution controls region granularity - higher values yield more subregions."),
          
          box(title = "Clustering Configuration",
              width = 5,
              style = "height: 400px",
              selectizeInput("cluster_select", "Select Clustering Algorithm:", 
                             choices = c("LV","LM","SLM","UMAP-kmeans"), 
                             selected = "UMAP-kmeans"),
              uiOutput("culster_resolution_button_container"),
              p(em("Adjust parameters to optimize region segmentation"))
          ),
          box(title = "Metabolically Defined Clustering Regions",
              width = 7,
              style = "height: 400px",
              plotOutput("cluster_plot",height = 300),
              p(em("Each color represents a distinct metabolic tissue region")),
              column(3, downloadButton("download_cluster_plot", "Export Region Map")),
              column(3, downloadButton("download_cluster_data", "Export Region Data"))
          )),
          
          fluidRow(
            h3("Differential Analysis Between Clustering Regions"),
            p("Compare metabolic profiles between user-defined region groups:"),
            box(title = "Comparison Group Setup",
                width = 6,
                selectizeInput("auto_add_to_case", "Select Case Group Regions:",
                               choices =NULL,
                               selected = NULL,
                               multiple = TRUE),
                selectizeInput("auto_add_to_control", "Select Control Group Regions:",
                               choices =NULL,
                               selected = NULL,
                               multiple = TRUE),
                actionButton("clear_selection_cluster", "Reset Group Selection"),
                actionButton("finish_selection_cluster", "Confirm Group Assignment"),
                hr(),
                h4("Statistical Thresholds"),
                numericInput("Fold_change","Fold Change Threshold:",min=1,max=10,value=2,step=0.1),
                numericInput("p_value_adjust","Adjusted p-value Threshold:",min=0.001,max=1,value=0.01,step=0.01)
            ),
            box(
              title ="Selected Region Groups Visualization",
              width = 6,
              plotOutput("group_spectrum_cluster",height = "300px")
            )),
            
            fluidRow(
              box(
                title = "Differential Metabolite Volcano Plot",
                width = 6,
                p("Visual summary of significantly altered metabolites:"),
                plotOutput("volcano_plot"),
                downloadButton("download_volcano_data", "Export Differential Features")
              ),
              box(
                title = "Spatial Distribution of Differential Metabolites",
                width = 6,
                style = "height: 400px",
                selectInput("diff_ion_select", "Select Differential Metabolite:", choices = NULL,selected = NULL),
                plotOutput("diffion_distribution_plot",height = "300px")
              )
            ))