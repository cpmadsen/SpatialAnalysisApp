library(shiny)
library(shinydashboard)
library(tidyverse)
library(sf)
library(leaflet)
library(leaflet.extras)
library(DT)
library(terra)
library(readxl)
library(classInt); library(BAMMtools) #For binning

rm(list = ls())

#--------------------------------------------------------------------
#Custom functions

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

#--------------------------------------------------------------------
#SET UP USER INTERFACE

ui <- fluidPage(
  withMathJax(),
  #titlePanel("Risk Model \nTool"),
  
  sidebarLayout(
    sidebarPanel(
      inputPanel(
        align = "center",
        h2("Risk Model Tool"),
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
                   actionButton(inputId = "restart_cleaning", "Restart Cleaning"),
                   ),
                 #uiOutput('binning_panel'),
                 column(3,
                        inputPanel(
                          tags$div(id = 'variable_filters')
                        )
                 ),
                 column(3,
                        inputPanel(
                          tags$div(id = 'variable_bins')
                        )),
                 column(6,
                        inputPanel(
                          tags$div(id = 'variable_histograms')
                        )),
                 fluidRow(
                   dataTableOutput('bin_check')
                 )
                 ),
        tabPanel("Model Specification",
                 h3("Model:"),
                 uiOutput('modeltext'),
                 tags$div(id = "variable_coefs"),
                 radioButtons(inputId = "bin_results",
                              label = "Bin model results into 3 bins?",
                              choices = c("Yes","No"),
                              selected = "No"),
                 hr(),
                 hr(),
                 h3("Model Output"),
                 dataTableOutput('model_result'),
        ),
        tabPanel("Results",
                 inputPanel(selectInput("spat_scale", label = "Spatial Scale", 
                                        choices = c("FLNRORD fisheries regions" = "flnro",
                                                    "Your Polygon(s)" = "user_poly_scale"), 
                                        selectize = F),
                            fileInput(inputId = "user_scale_poly", label = "Your Polygon(s)",
                                      accept = c(".zip",".gpkg","xlsx"),
                                      placeholder = "Upload file (zip, gpkg, or xlsx")),
                 inputPanel(selectInput("output_type", label = "Output Type",
                                        choices = c("Polygon/Vector" = "vec_output",
                                                    "Raster" = "rast_output")),
                            uiOutput('raster_res')),
                 inputPanel(
                   plotOutput('spatial_results_map', width = '100%', height = '100%')
                 ),
                 inputPanel(h3("Download"),
                            downloadButton('downloadData',"Download Shapefile with results")
                            )
        )
      ),
      width = 9
    )
  )
  #),
  # width = '300px'
)

#--------------------------------------------------------------------
# SET UP SERVER

server <- function(input, output, session) {
  bc = read_sf("bc_simple.gpkg")
  flnro = read_sf("FLNRO_Fishing_Boundaries.gpkg")
  
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
  #with the column names of that dataset.F
  observeEvent(input$user_data, {
    updateSelectInput("variable_1", session = session,
                      choices = names(UserDat()))
  })
  
  # ---------------------------------------- # 
  #             Data Cleaning                #
  # ---------------------------------------- # 
  #----------------------------------------------------------------------------
  
  # Get a reactive vector of column names that have been selected.
  SelectedColumns = reactive({
    selected_vars = c()
    for(i in 1:input$number_vars){
      selected_vars = c(selected_vars, input[[paste0("variable_",i)]])
    }
    selected_vars
  })

  #Render the data filtering and binning options for each selected variable,
  # once the user clicks on the "start cleaning" button.
  observeEvent(input$start_cleaning, {
    
    if(is.null(input$variable_1))return(NULL)

    
    ## USE THIS TO MAKE THE MULTI-FIGURE PANELS:
    # ui <- fluidPage(
    #   uiOutput("moreControls")
    # )
    # 
    # server <- function(input, output) {
    #   output$moreControls <- renderUI({
    #     tagList(
    #       sliderInput("n", "N", 1, 1000, 500),
    #       textInput("label", "Label")
    #     )
    #   })
    # }
    # output[["binning_panel"]] <- renderUI({
    #   
    #   n <- input[["number_vars"]]
    #   
    #   selectors <- lapply(1:n, function(i){
    #     
    #   list_of_UI_elements = list(
    #       ## 
    #       sliderInput(
    #       inputId = paste0("slider_",i),
    #       label = paste0("Filter ",variable_name),
    #       value = c(min(UserDat()[[variable_name]]),
    #                 max(UserDat()[[variable_name]])),
    #       min = min(UserDat()[[variable_name]]),
    #       max = max(UserDat()[[variable_name]]),
    #       width = "200%"),
    #       ## Binning method selections.
    #       selectInput(
    #         inputId = paste0("bin_",i),
    #         label = paste0("Bin ",variable_name),
    #         choices = c("Equal Width Bins","Equal Sample Bins","Natural Jenks"),
    #         width = "150%"
    #       ),
    #       ## Histogram with bins visualized.
    #       renderPlot(
    #         UserDatBinned() %>% 
    #           ggplot() +
    #           geom_histogram(aes(x = !!sym(variable_name), fill = as.character(!!sym(paste0(variable_name,"_binned"))))) + 
    #           #geom_vline(xintercept = variable_breaks) +
    #           theme_minimal() +
    #           theme(legend.position = "none",
    #                 text = element_text(size = 20)) + 
    #           scale_fill_brewer(palette = "Dark2"),
    #         width = 500, height = 200
    #       )
    #   )
    #   
    #   return(list_of_UI_elements)
    #   })
    #     
    #   do.call(function(...){
    #     box(..., width = 2, status = "primary")
    #   }, selectors)
    # })
    
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
                   sep = "",
                   #If the range of the variable in question is more than 6, make steps size of 1.
                   step = ifelse(max(UserDat()[[variable_name]]) - min(UserDat()[[variable_name]]),1,NULL),
                   width = "200%"
                 )),
                 id = id))
    }

    #Make binning selector (drop down)
    for(i in 1:input$number_vars){
      id <- paste0('variable_bin_', i)

      variable_name = input[[paste0("variable_", i)]]

      insertUI(selector = '#variable_bins',
               ui = tags$div(tags$p(
                 selectInput(
                   inputId = paste0("bin_",i),
                   label = paste0("Bin ",variable_name),
                   choices = c("Equal Width Bins","Equal Sample Bins","Natural Jenks"),
                   width = "150%"
                 )),
                 id = id))

    }

    #Make histograms that show, for each variable, the result of the binning style.
    for(i in 1:input$number_vars){
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
  
  #If the user clicks the "restart binning" button:
  observeEvent(input$restart_cleaning, {
    
  })
  
  #Take each variable from the user's dataset and apply the range filters.
  #Keep only selected variables. 
  #Save the result as a reactive expression.
  UserDatFiltered = reactive({
    if(is.null(input$slider_1))return(NULL)
    
    dat = UserDat() %>% 
      select(SelectedColumns())
    
    #Cycle through all of the variable inputs, applying filters as you go.
    for(i in 1:input$number_vars){
      
      variable_name = input[[paste0("variable_", i)]]
      
      dat = dat %>% 
        dplyr::filter(.[[variable_name]] >= input[[paste0("slider_",i)]][1],
                      .[[variable_name]] < input[[paste0("slider_",i)]][2])
      
      #If this variable is numeric, just keep up to 3 decimal places...
      if(is.numeric(dat[[variable_name]])){
        dat = dat %>% 
          mutate(!!sym(variable_name) := round(!!sym(variable_name),3))
      }
    }
    
    dat
  })
  
  #Bin user data, depending on bin input.
  UserDatBinned = reactive({
    if(is.null(input$slider_1) | is.null(input$bin_1))return(NULL)
    
    dat = UserDatFiltered()
    
    variables = SelectedColumns()
    
    #For each of the variables the user has selected from their dataset...
    for(i in 1:length(variables)){
      
      #If user selects "Equal Width Bins" option...
      if(input[[paste0("bin_",i)]] == "Equal Width Bins"){
        dat = dat %>% 
          mutate(!!sym(paste0(variables[i],"_binned")) := as.numeric(cut(!!sym(variables[i]),3)))
      }
      #If user selects "Equal Sample Bins" option...
      if(input[[paste0("bin_",i)]] == "Equal Sample Bins"){
        dat = dat %>% 
          mutate(!!sym(paste0(variables[i],"_binned")) := as.numeric(Hmisc::cut2(!!sym(variables[i]), g = 3)))
      }
      #If user selects "Natural Jenks" option...
      if(input[[paste0("bin_",i)]] == "Natural Jenks"){
        dat = dat %>% 
          mutate(!!sym(paste0(variables[i],"_binned")) := as.numeric(cut(!!sym(variables[i]), 
                                                                         breaks = getJenksBreaks(!!sym(variables[i]),
                                                                                                 k = 4),
                                                                         include.lowest = T)))
      }
    }
    return(dat)
  })
  
  output$bin_check = renderDataTable(UserDatBinned() %>% 
                                       st_drop_geometry())
  
  # ---------------------------------------- # 
  #          Model Specification             #
  # ---------------------------------------- # 
  #----------------------------------------------------------------------------
  
  # Render numeric inputs for each variable with which the user can set the coefficient of each covariate.
  inserted_coefs <- c()
  observeEvent(input$number_vars, {
    
    variable_number <- input$number_vars
    id <- paste0('variable_coef_', variable_number)
    if (input$number_vars > length(inserted_coefs)) {
      insertUI(selector = '#variable_coefs',
               ui = tags$div(tags$p(
                 textInput(
                   inputId = paste0("variable_coef_",variable_number),
                   value = 1,
                   label = paste0("Coefficient of variable ",variable_number, " (must be numeric)"),
                 )),
                 id = id))
      inserted_coefs <<- c(id, inserted_coefs)
    }
    else {
      inserted_coefs <- sort(inserted_coefs)
      removeUI(selector = paste0('#', inserted_coefs[length(inserted_coefs)]))          
      inserted_coefs <<- inserted_coefs[-length(inserted_coefs)]
    }
  })
  
  #Make math form of model. This is just to make a UI for the user.
  ModelDisplay = reactive({
    
    vars = c()
    
    for(i in 1:input$number_vars){
      
      varname = input[[paste0("variable_", i)]]
      
      mod = input[[paste0("variable_coef_", i)]]
      
      vars = c(vars, paste0(mod,"(",varname,")"))
    }
      
    model = paste0("Output = ",paste0(vars, collapse = " + "))
    
    model
})
  
  output$modeltext = renderUI({
    withMathJax(paste0("$$", ModelDisplay(),"$$"))
  })
  
  #Sum across columns to calculate result column.
  UserDatSummed = reactive({
    if(is.null(input$slider_1)){return(NULL)}

    dat = UserDatBinned()

    #Apply the coefficients entered by the user to each variable.
    for(i in 1:input$number_vars){

      variables = paste0(SelectedColumns(),"_binned")
      
      dat = dat %>% 
        mutate(mod = input[[paste0("variable_coef_", i)]]) %>% 
        mutate(mod = as.numeric(mod)) %>% 
        mutate(!!sym(paste0(variables[i],"_with_mod")) := (mod * !!sym(variables[i])))
    }
    
    dat$result = rowSums(dat %>% st_drop_geometry() %>% select(ends_with("_binned_with_mod")))
    
    dat = dat %>% select(ends_with("_binned"), result)
    
    if(input$bin_results == "Yes"){
      dat = dat %>% mutate(result = as.numeric(cut(result,3)))
    }
    
    return(dat)
  })
  
  output$model_result = renderDataTable(UserDatSummed())

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
  
  #If raster output selected, make UI for resolution specification.
  observe({
    if(input$output_type == "rast_output"){
      renderUI({
        textInput(
          inputId = "raster_res",
          label = "Raster Resolution (m^2)",
          value = 1000
        )
      })
    }
  })
  
  #Join output values from model to spatial polygon that user has chosen.
  SpatialResults = reactive({
    
    if(input$spat_scale == "flnro"){
    #If selected FLNRORD regions as spatial scale
    summed_dat = UserDatSummed() %>%
      st_join(flnro) %>%
      st_drop_geometry() %>%
      group_by(REGION_N) %>%
      summarise(mean_result = mean(result,na.rm=T))
    
    spatial_results = flnro %>%
      left_join(summed_dat)
    }
    
    #If selected User Polygon as spatial scale...
    if(input$spat_scale == "user_poly_scale"){
      summed_dat = UserDatSummed() %>%
        st_join(UserScalePoly() %>% mutate(row.number = row_number())) %>%
        st_drop_geometry() %>%
        group_by(row.number) %>%
        summarise(mean_result = mean(result,na.rm=T))
      
      spatial_results = UserScalePoly() %>%
        mutate(row.number = row_number()) %>% 
        left_join(summed_dat)
    }
    
    #If the user asks for raster output...
    if(input$output_type == "rast_output"){
      
      spatial_results_spat = vect(spatial_results %>% st_transform(crs = 3005))
      spatial_results_res = rast(spatial_results_spat, resolution = as.numeric(input$raster_res))
      
      spatial_results = rasterize(spatial_results_spat, spatial_results_res, field = "mean_result")
    }
    
    return(spatial_results)
  })
  
  
  output$spatial_results_table = renderDataTable(SpatialResults())
  
  output$spatial_results_map = renderPlot({
    if(input$bin_results == "Yes"){
      ggplot() +
        geom_sf(data = SpatialResults(), aes(fill = as.factor(mean_result))) + 
        geom_sf(data = UserDatBinned(), col = "yellow") + 
        scale_fill_manual(values = c("1" = "#52ad2b",
                                     "2" = "#db8035",
                                     "3" = "#e02626")) +
        labs(fill = "Model Output")
    }else{
      ggplot() +
        geom_sf(data = SpatialResults(), aes(fill = mean_result)) + 
        geom_sf(data = UserDatBinned(), col = "yellow") + 
        labs(fill = "Model Output")
    }
  },
  width = 700, 
  height = 500
  )
  
  output$downloadData <- downloadHandler(
      filename = function() {
        paste0('Spatial_Analysis_App_Output_', Sys.Date(), '.gpkg')
      },
      content = function(con) {
        write_sf(SpatialResults(), con)
      }
    )
}

shinyApp(ui = ui, server = server)
