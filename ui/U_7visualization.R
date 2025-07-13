tabItem(tabName = "visualization",
        h2("Spatial Metabolite Visualization"),
        h3("Single Ion Imaging"),
        fluidRow(
          box(title = "Metabolite Ion Selection",
              width = 6,
              style = "height: 350px; overflow-y: auto;",
              selectInput("single_ions_visualization_select", 
                          "Select Metabolite Ion for Visualization:", 
                          choices = NULL, multiple = FALSE)
          ),
          box(title = "Spatial Distribution of Selected Ion",
              width = 6,
              style = "height: 350px; overflow-y: auto;",
              plotOutput("single_ion_visualizaiton",height = 300)
          )),
          
          h3("Ion Co-localization Analysis"),
          p("Identifies metabolites with spatially correlated expression patterns:"),
          tags$ul(
            tags$li(strong("Positive correlation:"), " Metabolites with similar tissue distribution"),
            tags$li(strong("Negative correlation:"), " Metabolites with complementary distributions")
          ),
          
          fluidRow(
            box(title = "Top Positively Correlated Metabolites",
                width = 6,
                style = "height: 500px; overflow-y: auto;",
                plotOutput("positively_correlated_ions")
            ),
            box(title = "Top Negatively Correlated Metabolites",
                width = 6,
                style = "height: 500px; overflow-y: auto;",
                plotOutput("negatively_correlated_ions")
            )
          ),
          
          h3("Multi-ion Composite Imaging"),
          p("Visualize up to 3 metabolites simultaneously using RGB pseudo-coloring:"),
          tags$ol(
            tags$li("Select 2-3 metabolites of interest"),
            tags$li("Their intensities will be mapped to RGB color channels"),
            tags$li("Overlapping expression appears as mixed colors")
          ),
          
          fluidRow(
            box(title = "Metabolite Selection for RGB Mapping",
                width = 6,
                style = "height: 350px; overflow-y: auto;",
                selectInput("multiple_ions_select", 
                            "Select 2-3 Metabolites (R/G/B Channels):", 
                            choices = NULL, multiple = TRUE),
                p(em("Order: First = Red, Second = Blue, Third = Green"))
            ),
            box(title = "Composite Pseudo-color Visualization",
                width = 6,
                style = "height: 350px; overflow-y: auto;",
                plotOutput("Pseudo_color_plot",height = 300)
            )
          ))