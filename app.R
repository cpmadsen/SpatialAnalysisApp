library(shiny)
library(shinydashboard)
library(tidyverse) #For data manipulation and piping.
library(sf) #For vector spatial functions.
library(leaflet)
library(leaflet.extras)
library(pals)
library(DT) #For interactive data tables.
library(terra) #For raster functions.
library(readxl) #To read in excel files.
library(ggthemes) #For map themes.
library(ggpubr) #For ggarrange of final maps.
library(rasterVis) #To visualize the rasters.
library(classInt); library(BAMMtools) #For binning
library(sortable) #Allows drag-and-drop input in Shiny apps

rm(list = ls())

#--------------------------------------------------------------------
#Custom functions

#--------------------------------------------------------------------
# OPTIONS

#Increase max size of upload file to 30 MB.
options(shiny.maxRequestSize = 150*1024^2)

#--------------------------------------------------------------------
# LOAD IN SOURCE FILES

source('source_scripts/UI_01_Tab_Data.R')
source('source_scripts/UI_02_Tab_Cleaning.R')
source('source_scripts/UI_03_Tab_Model.R')
source('source_scripts/UI_04_Tab_Results.R')

#--------------------------------------------------------------------
# LOAD IN BACKGROUND FILES

# Read in BC shapefile and subwatersheds.
bc = read_sf("bc_simple.gpkg") %>% st_transform(crs = 4326)
#subw = read_sf("W:/CMadsen/SpatialData/WatershedGroups.shp") %>% st_transform(crs = 4326)
flnro = read_sf("FLNRO_Fishing_Boundaries.gpkg") %>% st_transform(crs = 4326)

#--------------------------------------------------------------------
#SET UP USER INTERFACE

ui <- fluidPage(
  withMathJax(),
  #titlePanel("Risk Model \nTool"),
  
  sidebarLayout(
    sidebarPanel(
      inputPanel(
        align = "center",
        h2("Spatial Analysis Tool"),
        #HTML('<p><img src="robot-head.png"/></p>'),
        img(src="robot-head.png", width = '100px'),
        #h3(""),
        hr(style = "border-top: 4px solid #980028;"),
        h4("Step 1. Upload your dataset, \nselect variables"),
        hr(style = "border-top: 1px solid #980028; opacity: 0"),
        h4("Step 2. Clean and bin variables"),
        hr(style = "border-top: 1px solid #980028; opacity: 0"),
        h4("Step 3. Set up model. How will your variables interact?"),
        hr(style = "border-top: 1px solid #980028; opacity: 0"),
        h4("Step 4. Inspect results. Download shapefile."),
        hr(style = "border-top: 1px solid #980028; opacity: 0"),
        hr(style = "border-top: 5px solid #980028;"),
        h3("Contact Information"),
        h5("App Developer: Chris Madsen"),
        h5("Chris.Madsen@gov.bc.ca")
      ),
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        data_tab,
        cleaning_tab,
        model_tab,
        results_tab
      ),
      width = 9
    )
  )
)

#--------------------------------------------------------------------
# SET UP SERVER

server <- function(input, output, session) {
  #bc = read_sf("bc_simple.gpkg")
  #flnro = read_sf("FLNRO_Fishing_Boundaries.gpkg")
  
  #Logic for tab 1, "Data"
  source(file.path('source_scripts/server_01_Read_Data.R'),  local = TRUE)$value
  
  #Logic for tab 2, "Cleaning"
  source(file.path('source_scripts/server_02_Clean_Data.R'),  local = TRUE)$value
  
  #Logic for tab 3, "Model"
  source(file.path('source_scripts/server_03_Model.R'), local = TRUE)$value
  
  #Logic for tab 4, "Results"
  source(file.path('source_scripts/server_04_Results.R'), local = TRUE)$value
}

shinyApp(ui = ui, server = server)
