library(shiny)
library(shinydashboard)
library(tidyverse) #For data manipulation and piping.
library(sf) #For vector spatial functions.
# library(leaflet)
# library(leaflet.extras)
library(DT) #For interactive data tables.
library(terra) #For raster functions.
library(readxl) #To read in excel files.
library(ggthemes) #For map themes.
library(ggpubr) #For ggarrange of final maps.
library(rasterVis) #To visualize the rasters.
library(classInt); library(BAMMtools) #For binning
library(sortable) #Allows drag-and-drop input in Shiny apps
library(thematic)

thematic::thematic_shiny()

rm(list = ls())

#--------------------------------------------------------------------
# OPTIONS

#Increase max size of upload file to 30 MB.
options(shiny.maxRequestSize = 150*1024^2)

#--------------------------------------------------------------------
# LOAD IN BACKGROUND FILES

# Read in BC shapefile and subwatersheds.
bc = read_sf("bc_simple.gpkg") %>% st_transform(crs = 4326)
#subw = read_sf("W:/CMadsen/SpatialData/WatershedGroups.shp") %>% st_transform(crs = 4326)
flnro = read_sf("FLNRO_Fishing_Boundaries.gpkg") %>% st_transform(crs = 4326)

source("App_UI_design.R")

#--------------------------------------------------------------------
# SET UP SERVER

server <- function(input, output, session) {
  bc = read_sf("bc_simple.gpkg")
  flnro = read_sf("FLNRO_Fishing_Boundaries.gpkg")
  
  # Read in the server component scripts #
  source(file.path('server_data_upload.R'), local = T)$value
  source(file.path('server_data_cleaning_and_binning.R'), local = T)$value
  source(file.path('server_model_specification.R'), local = T)$value
  source(file.path('server_results.R'), local = T)$value
}

shinyApp(ui = ui, server = server)
