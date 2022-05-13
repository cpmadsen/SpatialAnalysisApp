library(shiny)
library(shinydashboard)
library(tidyverse)
library(sf)
library(leaflet)
library(leaflet.extras)
library(DT)
library(readxl)
library(classInt); library(BAMMtools) #For binning

### THIS SECTION SETS UP FILES BEFORE LAUNCHING R SHINY APPLICATION ###

rm(list = ls())

#Custom functions

#Increase max size of upload file to 30 MB.
options(shiny.maxRequestSize = 120*1024^2)

# Read in BC shapefile and subwatersheds.
bc = read_sf("W:/CMadsen/SpatialData/bc_simple.shp") %>% st_transform(crs = 4326)
#subw = read_sf("W:/CMadsen/SpatialData/WatershedGroups.shp") %>% st_transform(crs = 4326)
flnro = read_sf("W:/CMadsen/SpatialData/FLNRO_Fishing_Boundaries.shp") %>% st_transform(crs = 4326)

all_data_files = c("Variable 1" = "UserDat1",
                   "Variable 2" = "UserDat2",
                   "Variable 3" = "UserDat3",
                   "Variable 4" = "UserDat4",
                   "Variable 5" = "UserDat5",
                   "Variable 6" = "UserDat6")

# Generate fake badger data.
boundaries = as.data.frame(as.matrix(st_bbox(bc)))

bc_center = data.frame(lon = (boundaries[1,1] + boundaries[3,1])/2,
                       lat = (boundaries[2,1] + boundaries[4,1])/2)

badg = data.frame(year = sample(2015:2021, 100, replace = T), 
                  data_type = sample(c("Den","Sighting"), 100, replace = T),
                  a = sample(1:10, 100, replace = T),
                  b = sample(20:40, 100, replace = T),
                  c = sample(0.1:1, 100, replace = T),
                  d = rnorm(100, mean = 3, sd = 1),
                  e = rnorm(100, mean = 100, sd = 10),
                  lon = rnorm(100, mean = bc_center$lon, sd = 4),
                  lat = rnorm(100, mean = bc_center$lat, sd = 4)) %>% 
  mutate(lon_map = lon, lat_map = lat)

badg = st_as_sf(badg, coords = c("lon_map","lat_map"), crs = 4326)

#Filter out points outside of bc...
badg = st_join(badg, bc, st_intersects) %>% 
  filter(!is.na(TYPE_1)) %>% select(year:lat)

#Set up user interface.
ui <- fluidPage(
  withMathJax(),
  #titlePanel("Risk Model \nTool"),
  
  sidebarLayout(
    sidebarPanel(
      inputPanel(
        align = "center",
        h2("Risk Model Tool"),
        hr(style = "border-top: 4px solid #980028;"),
        h4("Step 1. Upload your dataset(s) of and select \nthe variable(s) you will use. \nThese can be either tables or spatial files \n(.xlsx, .zip, or .gpkg)"),
        hr(style = "border-top: 1px solid #980028; opacity: 0"),
        h4("Step 2. Clean and bin your variable(s)"),
        hr(style = "border-top: 1px solid #980028; opacity: 0"),
        h4("Step 3. Set up the model. How will your variables interact?"),
        hr(style = "border-top: 1px solid #980028; opacity: 0"),
        h4("Step 4. Run the model. Inspect the output in tabs 3 and 4"),
        actionButton("run_model","Run Model!"),
        hr(style = "border-top: 1px solid #980028; opacity: 0"),
        h4("Step 5. Download the results"),
        hr(style = "border-top: 5px solid #980028;"),
        h3("Contact Information"),
        h5("App Developer: Chris Madsen"),
        h5("Chris.Madsen@gov.bc.ca")
      ),
      width = 2
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Upload",
                 column(width = 8,
                        inputPanel(
                          fileInput(inputId = "user_data", label = "Dataset",
                                    accept = c(".zip",".gpkg","xlsx"),
                                    placeholder = "Upload file (zip, gpkg, or xlsx"),
                        ),
                        inputPanel(
                          dataTableOutput('data_preview')
                        )
                 ),
                 column(width = 4,
                        inputPanel(
                          numericInput(inputId = "number_vars",
                                     label = "Number of Variables",
                                     value = 1, min = 1, max = 6, width = '700px')
                          ),
                        inputPanel(
                          tags$div(id = 'variable_selectors')
                        )
                 )),
        tabPanel("Data Cleaning + Binning",
                 fluidRow(
                   actionButton(inputId = "start_cleaning", "Start Cleaning Data"),
                   verbatimTextOutput('selectedcolumns')
                   ),
                 verbatimTextOutput('variable_1_check'),
                 column(4,
                        inputPanel(
                          tags$div(id = 'variable_filters')
                        )
                 ),
                 column(4,
                        inputPanel(
                          tags$div(id = 'variable_bins')
                        )),
                 column(4,
                        inputPanel(
                          tags$div(id = 'variable_histograms')
                        )),
                 fluidRow(
                   dataTableOutput('bin_check')
                 )
                 ),
        tabPanel("Model Specification",
                 uiOutput('modeltext'),
                 tags$div(id = "variable_radiobuttons")
        ),
        tabPanel("Output Options",
                 inputPanel(selectInput("spat_scale", label = "Spatial Scale", 
                                        choices = c("B.C." = "bc_scale",
                                                    "Subwatershed" = "subw_scale",
                                                    "FLNRORD fisheries regions" = "flnro",
                                                    "Your Polygon(s)" = "user_poly_scale"), 
                                        selectize = F),
                            fileInput(inputId = "user_scale_poly", label = "Your Polygon(s)",
                                      accept = c(".zip",".gpkg","xlsx"),
                                      placeholder = "Upload file (zip, gpkg, or xlsx")),
                 inputPanel(selectInput("output_type", label = "Output Type",
                                        choices = c("Polygon/Vector" = "vec_output",
                                                    "Raster" = "rast_output"))),
                 dataTableOutput('valence_check')
        ),
        tabPanel("Results",
                 plotOutput('spatial_results_plot'),
                 dataTableOutput('spatial_results_table'))
      ),
      width = 10
    )
  )
  #),
  # width = '300px'
)

server <- function(input, output, session) {
  bc = read_sf("W:/CMadsen/SpatialData/bc_simple.shp")
  
  #Render the variable selection drop-downs based on the user's number input.
  inserted_variableselections <- c()
  observeEvent(input$number_vars, {
    
    variable_number <- input$number_vars
    id <- paste0('variable_', variable_number)
    if (input$number_vars > length(inserted_variableselections)) {
      insertUI(selector = '#variable_selectors',
               ui = tags$div(tags$p(
                 selectInput(
                   inputId = paste0("variable_",variable_number),
                   label = "Variable Selection",
                   choices = names(UserDat()),
                   multiple = F,
                   width = "200%"
                 )),
                 id = id))
      inserted_variableselections <<- c(id, inserted_variableselections)
    }
    else {
      inserted_variableselections <- sort(inserted_variableselections)
      removeUI(selector = paste0('#', inserted_variableselections[length(inserted_variableselections)]))          
      inserted_variableselections <<- inserted_variableselections[-length(inserted_variableselections)]
    }
  })
  
  #Reactive expression to read in user data. This function can parse .zip, .gpkg, and .xlsx files.
  UserDat = reactive({
    
      file_name = input$user_data
      
      if(is.null(file_name)) return(NULL)

      #If it's a geopackage, read it in directly.
      if(str_detect(file_name$datapath, ".gpkg")){
        userpoly = read_sf(file_name$datapath) %>%
          st_transform(crs = 4326)

        return(userpoly)
      } else if (str_detect(file_name$datapath, ".zip")){
        #If it's a zipped shapefile, unzip then read in.

        filelist <- unzip(file_name$datapath)
        userpoly = read_sf(filelist[str_detect(filelist, ".shp")]) %>%
          st_transform(crs = 4326)

        return(userpoly)
      } else if (str_detect(file_name$datapath, ".xlsx")){
        #If it's an excel file, read that in.
        userpoly = read_excel(file_name$datapath)
        
        #If the excel table has lat/lon, use the following chunk.
        if(str_detect(str_to_lower(names(userpoly)),"(lat|lon)")){
          likely_lon = names(userpoly)[str_detect(str_to_lower(names(userpoly)), "lon(g)?")]
          likely_lat = names(userpoly)[str_detect(str_to_lower(names(userpoly)), "lat")]
          userpoly = st_as_sf(userpoly, coords = c(likely_lon, likely_lat), crs = 4326)
        }
        
        #If the excel table has BC Albers, use the following chunk.
        if(str_detect(str_to_lower(names(userpoly)),"bcalber")){
          likely_easting = names(userpoly)[str_detect(str_to_lower(names(userpoly)), "bcalbers_")]
          likely_northing = names(userpoly)[str_detect(str_to_lower(names(userpoly)), "bcalbers1")]
          userpoly = st_as_sf(userpoly, coords = c(likely_easting, likely_northing), crs = 3005)
        }
        
        return(userpoly)
      }
  })
  
  #Preview table to show user the data they've uploaded.
  output$data_preview <- renderDataTable(UserDat()[1:5,])
  
  #Once the user has uploaded their dataset, we need to update the first variable selector drop-down
  #with the column names of that dataset.
  observeEvent(input$user_data, {
    updateSelectInput("variable_1", session = session,
                      choices = names(UserDat()))
  })
  
  # ---------------------------------------- # 
  #             Data Cleaning                #
  # ---------------------------------------- # 
  #----------------------------------------------------------------------------
  
  #Make reactive expression to get column subset of original data for just those
  #columns that user has selected.
  # UserDatF = reactive({
  #   vars_to_keep = c()
  #   for(i in 1:input$number_vars){
  #     vars_to_keep = c(vars_to_keep, eval(sym(paste0("input$variable_",i))))
  #   }
  #   
  #   UserDat() %>% 
  #     dplyr::select(all_of(vars_to_keep))
  # })
  
  # Get a reactive vector of column names that have been selected.
  SelectedColumns = reactive({
    selected_vars = c()
    for(i in 1:input$number_vars){
      selected_vars = c(selected_vars, input[[paste0("variable_",i)]])
    }
    selected_vars
  })

  output$selectedcolumns = renderText({SelectedColumns()})
  
  #Render the data filtering and binning options for each selected variable.
  observeEvent(input$start_cleaning, {
    
    if(is.null(input$variable_1))return(NULL)

    for(i in 1:input$number_vars){

    #Make filter UI (range selector)
      id <- paste0('variable_filter_', i)

      variable_name = input[[paste0("variable_", i)]]
      
      insertUI(selector = '#variable_filters',
               ui = tags$div(tags$p(
                 sliderInput(
                   inputId = paste0("slider_",i),
                   label = paste0("Filter ",variable_name),
                   value = c(min(UserDat()[[variable_name]]),
                             max(UserDat()[[variable_name]])),
                   min = min(UserDat()[[variable_name]]),
                   max = max(UserDat()[[variable_name]]),
                   width = "200%"
                 )),
                 id = id))
    }

    #Take each variable from the user's dataset and apply the range filters. Save
    # the result as reactive expression.
    UserDatFiltered = reactive({
      dat = UserDat()
      
      #Cycle through all of the variable inputs, applying filters as you go.
      for(i in 1:input$number_vars){
        
        variable_name = input[[paste0("variable_", i)]]
        
        dat = dat %>% 
          dplyr::filter(.[[variable_name]] >= input[[paste0("slider_",i)]][1],
                        .[[variable_name]] < input[[paste0("slider_",i)]][2])
      }
      
      dat
    })
  
    for(i in 1:input$number_vars){
      #Make binning selector (drop down)
      id <- paste0('variable_bin_', i)
      
      variable_name = input[[paste0("variable_", i)]]
      
      insertUI(selector = '#variable_bins',
               ui = tags$div(tags$p(
                 selectInput(
                   inputId = paste0("bin_",i),
                   label = paste0("Bin ",variable_name),
                   choices = c("Equal Width Bins","Equal Sample Bins","Natural Jenks"),
                   width = "200%"
                 )),
                 id = id))
      
    }
    
    #Bin user data, depending on bin input.
    UserDatBinned = reactive({
      
      dat = UserDatFiltered()
      
      variables = SelectedColumns()
      
      for(i in 1:length(variables)){
        
        if(input[[paste0("bin_",i)]] == "Equal Width Bins"){
          dat = dat %>% 
            mutate(!!sym(paste0(variables[i],"_binned")) := as.numeric(cut(!!sym(variables[i]),3)))
        }
        if(input[[paste0("bin_",i)]] == "Equal Sample Bins"){
          dat = dat %>% 
            mutate(!!sym(paste0(variables[i],"_binned")) := as.numeric(Hmisc::cut2(!!sym(variables[i]), g = 3)))
        }
        if(input[[paste0("bin_",i)]] == "Natural Jenks"){
          
          #my.breaks = getJenksBreaks(dat[[paste0(variables[i],"_binned")]], k = 4)
          #Subtract 1 from lowest break, otherwise we lose the lowest observation.
          #my.breaks[1] = (my.breaks[1]-1)
          
          dat = dat %>% 
            mutate(!!sym(paste0(variables[i],"_binned")) := as.numeric(cut(!!sym(variables[i]), 
                                                                           breaks = getJenksBreaks(!!sym(variables[i]),
                                                                                                   k = 4),
                                                                           include.lowest = T)))
        }
      }
      dat
    })
    
    output$bin_check = renderDataTable(UserDatBinned()[1:5,])
    
    for(i in 1:input$number_vars){
      #Make plot that is informed by the above selections.
      id <- paste0('variable_histogram_', i)
      
      local({
        
        variable_name = input[[paste0("variable_", i)]]
        
        insertUI(selector = '#variable_histograms',
                 ui = tags$div(tags$p(
                   renderPlot(
                     UserDatBinned() %>% 
                       ggplot() +
                       geom_histogram(aes(x = !!sym(variable_name), fill = as.character(!!sym(paste0(variable_name,"_binned"))))) + 
                       #geom_vline(xintercept = variable_breaks) +
                       theme_minimal() +
                       theme(legend.position = "none",
                             text = element_text(size = 20)) + 
                       scale_fill_brewer(palette = "Dark2"),
                     width = 500, height = 200
                   )),
                   id = id))
      })
    }
  })
  
  # ---------------------------------------- # 
  #          Model Specification             #
  # ---------------------------------------- # 
  #----------------------------------------------------------------------------
  
  # Render radio buttons for each variable with which user can choose valence of variable.
  inserted_radiobuttons <- c()
  observeEvent(input$number_vars, {
    
    variable_number <- input$number_vars
    id <- paste0('variable_radiobutton_', variable_number)
    if (input$number_vars > length(inserted_radiobuttons)) {
      insertUI(selector = '#variable_radiobuttons',
               ui = tags$div(tags$p(
                 radioButtons(
                   inputId = paste0("variable_radiobutton_",variable_number),
                   label = paste0("Valence of ",input[[paste0("variable_",variable_number)]]),
                   choices =list("Positive" = 1, "Negative" = -1),
                   selected = 1
                 )),
                 id = id))
      inserted_radiobuttons <<- c(id, inserted_radiobuttons)
    }
    else {
      inserted_radiobuttons <- sort(inserted_radiobuttons)
      removeUI(selector = paste0('#', inserted_radiobuttons[length(inserted_radiobuttons)]))          
      inserted_radiobuttons <<- inserted_radiobuttons[-length(inserted_radiobuttons)]
    }
  })
  
  UserDatValenced = reactive({
    
    dat = UserDatBinned()
    
    for(i in 1:input$number_vars){
      variable_name = input[[paste0("variable_", i)]]
      variable_name = paste0(variable_name,"_binned")
      
      if(input[[paste0("variable_radiobutton_",i)]] == -1){
        
        dat[[variable_name]] = -1*dat[[variable_name]]
        
      }
    }
    
    return(dat)
  })
  
  #Make math form of model. This is just to make a UI for the user.
  ModelDisplay = reactive({
    
    vars = c()
    
    for(i in 1:input$number_vars){
      
      varname = input[[paste0("variable_", i)]]
      
      mod = ifelse(input[[paste0("variable_radiobutton_",i)]] == -1, "(-", "(")
      
      vars = c(vars, paste0(mod,varname,")"))
    }
      
    model = paste0("Output = ",paste0(vars, collapse = " + "))
    
    model
})
  
  output$modeltext = renderUI({
    withMathJax(paste0("Use this formula: $$", ModelDisplay(),"$$"))
  })
  
  #Sum across columns to calculate result column.
  UserDatSummed = reactive({
    if(is.null(input$variable_1)){return(NULL)}
  
    result = rowSums(UserDatValenced() %>% 
                           dplyr::select(ends_with("_binned")))
    
    UserDatValenced() %>% 
      dplyr::select(result, geometry)
  })
  
  output$valence_check = renderDataTable(UserDatValenced()[1:5,])
  #Join output values from model to spatial polygon that user has chosen.
  SpatialResults = reactive({
    #If selected FLNRORD regions as output...
    summed_dat = UserDatSummed() %>% 
      st_join(flnro) %>% 
      st_drop_geometry() %>% 
      group_by(REGION_N) %>% 
      summarise(mean_result = mean(result,na.rm=T))
    
    spatial_results = flnro %>% 
      left_join(summed_dat)
    
    spatial_results
  })

  output$spatial_results_table = renderDataTable(SpatialResults())
  output$spatial_results = renderPlot({
    ggplot() +
      geom_sf(data = SpatialResults(), aes(fill = mean_result))
  })
  
  # ---------------------------------------- # 
  #             Output Options               #
  # ---------------------------------------- # 
  #----------------------------------------------------------------------------
  #Read in user polygon for scale, if provided.
  UserScalePoly = reactive({
    file_name = input$user_scale_poly
    
    if(is.null(file_name)) return(NULL)
    
    #If it's a geopackage, read it in directly.
    if(str_detect(file_name$datapath, ".gpkg")){
      userpoly = read_sf(file_name$datapath) %>%
        st_transform(crs = 4326)
      
      return(userpoly)
    } else if (str_detect(file_name$datapath, ".zip")){
      #If it's a zipped shapefile, unzip then read in.
      
      filelist <- unzip(file_name$datapath)
      userpoly = read_sf(filelist[str_detect(filelist, ".shp")]) %>%
        st_transform(crs = 4326)
      
      return(userpoly)
    } else if (str_detect(file_name$datapath, ".xlsx")){
      #If it's an excel file, read that in.
      userpoly = read_excel(file_name$datapath)
      
      #If the excel table has lat/lon, use the following chunk.
      if(str_detect(str_to_lower(names(userpoly)),"(lat|lon)")){
        likely_lon = names(userpoly)[str_detect(str_to_lower(names(userpoly)), "lon(g)?")]
        likely_lat = names(userpoly)[str_detect(str_to_lower(names(userpoly)), "lat")]
        userpoly = st_as_sf(userpoly, coords = c(likely_lon, likely_lat), crs = 4326)
      }
      
      #If the excel table has BC Albers, use the following chunk.
      if(str_detect(str_to_lower(names(userpoly)),"bcalber")){
        likely_easting = names(userpoly)[str_detect(str_to_lower(names(userpoly)), "bcalbers_")]
        likely_northing = names(userpoly)[str_detect(str_to_lower(names(userpoly)), "bcalbers1")]
        userpoly = st_as_sf(userpoly, coords = c(likely_easting, likely_northing), crs = 3005)
      }
      
      return(userpoly)
    }
  })
}

shinyApp(ui = ui, server = server)
