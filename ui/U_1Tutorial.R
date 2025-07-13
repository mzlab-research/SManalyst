tabItem(tabName = "Tutorial",
          # 软件概述部分
          h2("SManalyst: Spatial Metabolomics Data Analysis Platform"),
          p(strong("SManalyst"), "is an innovative open-source platform for comprehensive analysis of spatial metabolomics data. Developed in R with Shiny framework, it integrates advanced computational methods to address key challenges in spatial metabolomics research, including:"),
          tags$ul(
            tags$li("Systematic data quality control"),
            tags$li("Rigorous metabolite annotation with reliability scoring"),
            tags$li("Multidimensional pattern discovery"),
            tags$li("Flexible differential analysis strategies"),
            tags$li("Interactive visualization of spatial distributions")
          ),
          p("The software implements a complete analytical workflow from raw data processing to biological interpretation, enabling researchers to uncover metabolic heterogeneity in tissue microenvironments."),
          
          hr(),
          
          # 核心功能模块
          h2("Core Functionalities"),
          fluidRow(
            column(4,
                   h4(icon("flask"), " Quality Control"),
                   p("Comprehensive quality assessment through:"),
                   tags$ul(
                     tags$li("Background signal consistency (QC1)"),
                     tags$li("Noise ion ratio (QC2)"),
                     tags$li("Intensity distribution (QC3)"),
                     tags$li("Missing value patterns (QC4)")
                   )
            ),
            column(4,
                   h4(icon("search"), " Metabolite Annotation"),
                   p("Advanced annotation features:"),
                   tags$ul(
                     tags$li("Isotope and adduct identification"),
                     tags$li("Multi-evidence scoring system"),
                     tags$li("Support for custom & public databases"),
                     tags$li("HMDB, KEGG, LIPIDMAPS integration")
                   )
            ),
            column(4,
                   h4(icon("project-diagram"), " Spatial Analysis"),
                   p("Multidimensional pattern discovery:"),
                   tags$ul(
                     tags$li("Metabolite-level pattern clustering"),
                     tags$li("Pixel-level tissue region clustering"),
                     tags$li("Differential analysis (ROI-based or clustering-based)"),
                     tags$li("Ion co-localization analysis")
                   )
            )
          ),
          hr(),
          
          # 工作流程图
          h2("Analysis Workflow"),
          fluidRow(
            column(6,
                   tags$img(src = "sm_workflow.png", width = "80%", style = "border: 1px solid #ddd; padding: 5px;")
            ),
            column(6,
                   p("The SManalyst workflow follows a structured pipeline:"),
                   tags$ol(
                     tags$li(strong("Data Upload"), ": Import feature matrix with spatial coordinates"),
                     tags$li(strong("Quality Control"), ": Automatic quality assessment and filtering"),
                     tags$li(strong("Preprocessing"), ": Background removal and noise reduction"),
                     tags$li(strong("Metabolite Annotation"), ": Database matching with reliability scoring"),
                     tags$li(strong("Pattern Analysis"), ": Spatial clustering at metabolite and pixel levels"),
                     tags$li(strong("Differential Analysis"), ": ROI-based or clustering-based comparisons"),
                     tags$li(strong("Visualization & Export"), ": Interactive exploration of results")
                   )

            )
          ),
          hr(),
          
          # 输入文件要求
          h2("Input File Requirements"),
          fluidRow(
            column(6,
                   p("Example feature matrix structure:"),
                   tags$img(src = "feature_matrix.png", width = "80%", style = "border: 1px solid #ddd; padding: 5px;"),
                   p(em("Note: This standardized format ensures compatibility with data from various spatial mass spectrometry platforms including MALDI, DESI, and SIMS."))
            ),
            column(6,
                   p("SManalyst requires data in", strong("Feature Matrix format"), "with specific structure:"),
                   tags$ul(
                     tags$li("First two columns", strong("MUST"), "be named 'X' and 'Y' representing spatial coordinates"),
                     tags$li("Subsequent columns represent m/z features (ion peaks)"),
                     tags$li("Each row corresponds to a spatial pixel (tissue location)"),
                     tags$li("Cell values indicate ion intensity at given coordinate"),
                     tags$li("File format: Tab-delimited text file (.txt)")
                   )
            )
          ),
          # 示例数据下载
          h3("Example Dataset"),
          p("Download our mouse brain spatial metabolomics dataset to test SManalyst:"),
          downloadButton("downloadData", label = "Download Example Data", class = "btn-primary"),
          p(em("Dataset details: AFAD-ESI platform, mouse brain coronal section (7-week male), 14,260 pixels, 3,044 m/z features")),
          
          hr(),
          
          # 使用方式
          h2("Usage Options"),
          div(class = "panel panel-primary",
              div(class = "panel-heading", h3("Web Application")),
              div(class = "panel-body",
                  p(strong("Access our online server:"), tags$a(href = "https://metax.genomics.cn/app/SManalyst", "https://metax.genomics.cn/app/SManalyst", target = "_blank"),
                    p("- No installation required"),
                    p("- Runs on cloud server (128 CPU cores, 1000GB RAM)")
              ))),
              div(class = "panel panel-default",
                  div(class = "panel-heading", h3("Local Installation")),
                  div(class = "panel-body",
                      p(strong("Source code:"), tags$a(href = "https://github.com/mzlab-research/SManalyst.git", "https://github.com/mzlab-research/SManalyst.git", target = "_blank"),
                        p("- Recommended for large datasets"),
                        p("- Requires R environment (v4.0+)"),
                  ))),
                  
                  hr(),
                  
                  h2("Contact Us"),
                  p("For questions and suggestions:", br(),
                    icon("envelope"), strong(" Contact:"), tags$code("meizhanlong@genomics.cn"))
          ) # tabItem




