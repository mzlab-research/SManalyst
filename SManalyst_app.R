## app.R ##
library(shinydashboard)
library(ggplot2)
library(shiny)
library(shinythemes)
library(tidyverse)
library(reshape2)
library(pheatmap)
library(MetaboCoreUtils)
library(spatstat)
library(shinyjs)
library(plotly)
library(Seurat)
library(ggnewscale)
library(cowplot)
library(readr)
library(dplyr)
library(magrittr)
library(plyr)
library(tidyr)
library(enviPat)
library(msentropy)
library(Spectra)
library(DT)
library(qvalue)
library(SpaGene)
library(data.table)
library(Matrix)
library(rjson)
library(RColorBrewer)
library(glmGamPoi)
library(pbapply)
library(shinyBS)
library(sf)
library(spdep)

options(encoding='UTF-8')
set.seed(1234)

ui <- dashboardPage(
  dashboardHeader(
    title = span(icon("chart-area"), "Spatial Metabolomics data Analysis (SManalysis)"),
    titleWidth = 550,
    tags$li(class = "dropdown",
            tags$style(".main-header .logo {height: 50px; line-height: 50px;}")
    )
  ),
  
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      id = "tabs",
      menuItem("Tutorial", tabName = "Tutorial", icon = icon("home")),
      menuItem("Data Upload", tabName = "upload", icon = icon("upload")),
      menuItem("Quality Control", tabName = "quality", icon = icon("search"), startExpanded = TRUE,
               menuSubItem("QC1: Background Consistency", tabName = "pro1", icon = icon("layer-group")),
               menuSubItem("Process1: Remove Backgrounds", tabName = "pro2", icon = icon("eraser")),
               menuSubItem("Process2 & QC2: Noise Ions", tabName = "pro3", icon = icon("filter")),
               menuSubItem("QC3&4: Intensity & Missing Values", tabName = "qc2", icon = icon("chart-bar"))
      ),
      menuItem("Metabolite Annotation", tabName = "annotation", icon = icon("tag"), startExpanded = TRUE,
               menuSubItem("Isotope/Adduct Identification", tabName = "qc3", icon = icon("atom")),
               menuSubItem("Metabolite Annotation", tabName = "annotation", icon = icon("database"))
      ),
      menuItem("Statistical Analysis", tabName = "statistics", icon = icon("calculator"), startExpanded = TRUE,
               menuSubItem("Pattern Analysis", tabName = "statistics_pattern", icon = icon("project-diagram")),
               menuSubItem("Clustering Analysis", tabName = "statistics_clustering", icon = icon("object-group")),
               menuSubItem("Differential Analysis", tabName = "statistics_differential", icon = icon("balance-scale"))
      ),
      menuItem("Data Visualization", tabName = "visualization", icon = icon("image"))
    ),
    tags$style(HTML("
      .sidebar-menu li a { 
        font-size: 15px;
        padding: 12px 15px;
      }
      .sidebar-menu .treeview-menu li a {
        padding: 10px 15px 10px 35px;
      }
    "))
  ),
  
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$title("SManalyst"),
      tags$link(rel = "stylesheet", type = "text/css", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css"),
      tags$style(HTML("
        .content-wrapper {
          background-color: #f9fbfd;
        }
        .box {
          border-radius: 8px;
          box-shadow: 0 2px 6px rgba(0,0,0,0.08);
          border-top: 3px solid #3c8dbc;
        }
        .box.box-primary {
          border-top-color: #3c8dbc;
        }
        .box.box-info {
          border-top-color: #00c0ef;
        }
        .box.box-success {
          border-top-color: #00a65a;
        }
        .box.box-warning {
          border-top-color: #f39c12;
        }
        .box.box-danger {
          border-top-color: #dd4b39;
        }
        h2, h3, h4 {
          color: #2c3e50;
          font-weight: 600;
        }
        .btn {
          border-radius: 4px;
          font-weight: 500;
        }
      "))
    ),
    tabItems(
      source(file.path("ui","U_1Tutorial.R"),  local = TRUE)$value,
      source(file.path("ui","U_2upload.R"),  local = TRUE)$value,
      source(file.path("ui","U_3pro1.R"),  local = TRUE)$value,
      source(file.path("ui","U_3pro2.R"),  local = TRUE)$value,
      source(file.path("ui","U_3pro3.R"),  local = TRUE)$value,
      source(file.path("ui","U_3qc2.R"),  local = TRUE)$value,
      source(file.path("ui","U_5statistics_pattern.R"),  local = TRUE)$value,
      source(file.path("ui","U_5statistics_clustering.R"),  local = TRUE)$value,
      source(file.path("ui","U_5statistics_differential.R"),  local = TRUE)$value,
      source(file.path("ui","U_3qc3.R"),  local = TRUE)$value,
      source(file.path("ui","U_4annotation.R"),  local = TRUE)$value,
      source(file.path("ui","U_7visualization.R"),  local = TRUE)$value
    )
  )
)

server <- function(input, output) {
  options(shiny.maxRequestSize=1000*1024^2)
  source(file.path("server","S_1Tutorial.R"),  local = TRUE)$value
  source(file.path("server","S_2upload.R"),  local = TRUE)$value
  source(file.path("server","S_3qc1.R"),  local = TRUE)$value
  source(file.path("server","S_3qc2.R"),  local = TRUE)$value
  source(file.path("server","S_3qc3.R"),  local = TRUE)$value
  source(file.path("server","S_4annotation.R"),  local = TRUE)$value
  source(file.path("server","S_5statistics_pattern.R"),  local = TRUE)$value
  source(file.path("server","S_5statistics_clustering.R"),  local = TRUE)$value
  source(file.path("server","S_5statistics_differential.R"),  local = TRUE)$value
  source(file.path("server","S_7visualization.R"),  local = TRUE)$value
}

shinyApp(ui, server)