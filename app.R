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
        #HTML('<p><img src="robot-head.png"/></p>'),
        img(src="robot-head.png", width = '100px'),
        h3("Chris 2.0"),
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
                        h5("Snapshot of your data."),
                        inputPanel(
                          uiOutput('cols_checkboxes'),
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
                        ),
                        inputPanel(
                          uiOutput('factorizing')
                        )
                 )),
        tabPanel("Data Cleaning + Binning",
                 uiOutput("binning_panel"),
                 fluidRow(
                   dataTableOutput('bin_check'),
                   textOutput('label_check')
                 )
                 ),
        tabPanel("Model Specification",
                 h3("Model:"),
                 uiOutput("modeltext"),
                 selectInput(inputId = "model_selection_type",
                             label = "Model Selection Type",
                             choices = c("Linear Equation" = "lineareq",
                                         "Stressor Response Function(s)" = "stressor"),
                             selected = "lineareq"),
                 uiOutput('model_selection_panel'),
                 #tags$div(id = 'variable_stressor_models'),
                 #tags$div(id = "variable_coefs"),
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
                 inputPanel(textInput(
                              inputId = "raster_res",
                              label = "Raster Resolution (m^2)",
                              value = 1000
                            ),
                            radioButtons(inputId = "bin_results",
                                         label = "Bin model results into 3 bins?",
                                         choices = c("Yes","No"),
                                         selected = "No")
                 ),
                 tabsetPanel(
                   tabPanel("Maps",
                     plotOutput('spatial_results_map', width = '100%', height = '100%')
                   ),
                   tabPanel("Table",
                     dataTableOutput('spatial_results_table')
                   )
                 ),
                 inputPanel(downloadButton('downloadData',"Download Shapefile"),
                            downloadButton('downloadDataRaster',"Download Raster"),
                            downloadButton('downloadDataTable',"Download Table")
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
        if(str_detect(file_name$datapath, ".xlsx")){
          userpoly = read_excel(file_name$datapath)
        }
        
        #If the excel table has lat/lon, use the following chunk.
          likely_lon = names(userpoly)[str_detect(str_to_lower(names(userpoly)), "lon(g)?")]
          likely_lat = names(userpoly)[str_detect(str_to_lower(names(userpoly)), "lat")]
          userpoly = st_as_sf(userpoly, coords = c(likely_lon, likely_lat), crs = 4326)

        return(userpoly)
      } else if(str_detect(file_name$datapath, ".csv")){
        if(str_detect(file_name$datapath, ".csv")){
          userpoly = read_csv(file_name$datapath)
        }
      
        likely_lon = names(userpoly)[str_detect(str_to_lower(names(userpoly)), "lon(g)?")]
        likely_lat = names(userpoly)[str_detect(str_to_lower(names(userpoly)), "lat")]
        userpoly = st_as_sf(userpoly, coords = c(likely_lon, likely_lat), crs = 4326)

        return(userpoly)
      }
  })
  
  #Once the user has uploaded their dataset, we need to update the first variable selector drop-down
  #with the column names of that dataset.
  observeEvent(input$user_data, {
    updateSelectInput("variable_1", session = session,
                      choices = names(UserDat()))
  })
  
  #Preview table to show user the data they've uploaded.
  output$data_preview <- renderDataTable({
    if(is.null(input$user_data)) return(NULL)

    UserDat()[1:5,] %>% 
      select(all_of(input$show_preview_cols)) %>% 
      mutate_if(is.numeric, ~round(.x,2))
  })
  
  #Which columns of the preview data will we display? Up to the user.
  output$cols_checkboxes = renderUI(
    checkboxGroupInput("show_preview_cols", "Columns in data to preview:",
                       names(UserDat()), selected = names(UserDat())[1:5])
  )
  
  # Get a reactive vector of column names that have been selected.
  SelectedColumns = reactive({
    selected_vars = c()
    for(i in 1:input$number_vars){
      selected_vars = c(selected_vars, input[[paste0("variable_",i)]])
    }
    selected_vars
  })
  
  #For any chosen variables that are characters, we need a way to convert to numbers.
  #Render a UI element for the user to choose factor levels.
  output$factorizing = renderUI({

    #If no variable has been selected yet, skip this for now...
    if(is.null(input$variable_1))return(NULL)
    
    #Initialize tag list for this multipart UI element.
    factorizing_taglist = tagList()
    
    #For each variable that is NOT numeric, add a UI element to the 'factorizing_taglist'
    for(i in 1:input$number_vars){
      #Get variable name for this round of the loop...
      variable_name = input[[paste0("variable_", i)]]
      
      #Is it categorical (i.e. NOT a number)? If so, do the following.
      if(is.character(UserDat()[[variable_name]]) == TRUE){

        tags_to_add = tagList(
          h4(str_to_title(variable_name)),
            rank_list(
              input_id = paste0("factor_list_",i),
              text = "Sort Categorical Data - high to low (NA are removed)",
              labels = na.omit(unique(UserDat() %>% pull(!!sym(variable_name))))
            )
        )
        
        factorizing_taglist = tagList(factorizing_taglist,
                                      tags_to_add)
      }
    }
    return(factorizing_taglist)
  })

  # Take the sorting the user has applied to the categorical variable(s)
  #    and apply it to make dummy variable(s)
  UserDatSelected = reactive({
    if(is.null(input$variable_1))return(NULL)
    
    #Bring in reactive user dataset that they have uploaded.
    dat = UserDat()
    
    # If the variable needs to be converted from a character category to a number,
    # use the ordering the user has provided with the drag-and-drop UI element.
    # If no factorization is needed for a given variable, keep the variable as is.
    for(i in 1:input$number_vars){
      
      #If we have some info from the factorization drag-and-drop for variable i...
      if(is.null(input[[paste0("factor_list_",i)]]) == FALSE){
        
        #Grab variable name and sorting heuristic.
        variable_name = input[[paste0("variable_",i)]]
        variable_sorter = c(rev(input[[paste0("factor_list_",i)]]))
        #variable_sorter = c("Data missing", rev(input[[paste0("factor_list_",i)]]))
        
        #Modify variable selected by loop: convert character to ordered factor (numeric)
        dat = dat %>% 
          #Replace NA with 0...
          #mutate(!!sym(variable_name) := replace(!!sym(variable_name), is.na(!!sym(variable_name)), "Data missing")) %>%
          #Make new column that will be used as x-axis markers in the histograms of the binning page
          mutate(!!sym(paste0(variable_name,"_label")) := !!sym(variable_name)) %>% 
          #Convert variable i to a numeric ordered factor.
          mutate(!!sym(variable_name) := as.numeric(factor(!!sym(variable_name),
                                                           levels = variable_sorter
          ))) %>% 
          #Add the number to the label.
          mutate(!!sym(paste0(variable_name,"_label")) := paste0(!!sym(paste0(variable_name,"_label")),
                                                                 " (",
                                                                 !!sym(variable_name),
                                                                 ")"))
      }
    }
    
    dat
  })

  # ---------------------------------------- # 
  #             Data Cleaning                #
  # ---------------------------------------- # 
  #----------------------------------------------------------------------------
  
  #Take each variable from the user's dataset and apply the range filters.
  #Keep only selected variables. 
  #Save the result as a reactive expression.
  UserDatFiltered = reactive({
    if(is.null(input$slider_1))return(NULL)
    
    user_selected_columns = SelectedColumns()
    
    #If the user has uploaded categorical data that has been turned into an ordered factor,
    #this section adds the 'label' version of those variables so that the user can see them in the
    #preview table of the binning section.
    for(i in 1:input$number_vars){
      
      #variable name.
      variable_name = input[[paste0("variable_", i)]]
      
      if(is.null(UserDatSelected()[[paste0(variable_name,"_label")]]) == FALSE){
        user_selected_columns = c(user_selected_columns, paste0(variable_name,"_label"))
      }
    }
    
    dat = UserDatSelected() %>% 
      select(all_of(user_selected_columns))
    
    #Cycle through all of the variable inputs, applying filters as you go.
    for(i in 1:input$number_vars){
      
      variable_name = input[[paste0("variable_", i)]]
      
      dat = dat %>% 
        dplyr::filter(.[[variable_name]] >= input[[paste0("slider_",i)]][1],
                      .[[variable_name]] <= input[[paste0("slider_",i)]][2])
      
      #If this variable is numeric, just keep up to 3 decimal places...
      if(is.numeric(dat[[variable_name]])){
        dat = dat %>% 
          mutate(!!sym(variable_name) := round(!!sym(variable_name),3))
      }
    }
    
    dat
  })
  
  #Bin user data, depending on bin input. If the variable in question is an ordered factor, 
  #  don't bin it.
  UserDatBinned = reactive({
    if(is.null(input$slider_1) | is.null(input$bin_1))return(NULL)
    
    dat = UserDatFiltered()
    
    variables = SelectedColumns()
    
    #For each of the variables the user has selected from their dataset...
    for(i in 1:length(variables)){
      
      #If the variable is an ordered factor, don't bin it. Skip to next variable.
      if(is.null(input[[paste0("factor_list_",i)]]) == FALSE){
        dat = dat %>% 
          mutate(!!sym(paste0(variables[i],"_binned")) := !!sym(variables[i]))
          #mutate(!!sym(variables[i]) := !!sym(paste0(variables[i],"_label"))) %>% 
          #select(-!!sym(paste0(variables[i],"_label")))
        next
      }
      
      #If user selects "Equal Sample Bins" option...
      if(input[[paste0("bin_",i)]] == "Equal Sample Bins"){
        dat = dat %>% 
          mutate(!!sym(paste0(variables[i],"_binned")) := as.numeric(Hmisc::cut2(!!sym(variables[i]), g = 3)))
      }
      #If user selects "Equal Width Bins" option...
      if(input[[paste0("bin_",i)]] == "Equal Width Bins"){
        dat = dat %>% 
          mutate(!!sym(paste0(variables[i],"_binned")) := as.numeric(cut(!!sym(variables[i]),3)))
        
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
  
  #Render a table for the user to see the effects of their binning choices.
  output$bin_check = renderDataTable(
    #If there's a geometry column, drop it.
    if(!is.na(st_is_longlat(UserDatBinned()))){
      UserDatBinned() %>%
        st_drop_geometry()
    } else {
      #If not geometry column, just call this reactive object.
      UserDatBinned()
    }
    
    )
  
  
  #Render the data filtering and binning options for each selected variable.
  output$binning_panel <- renderUI({
    
    if(is.null(input$variable_1))return(NULL)
    
    binning_panel_taglist = tagList()
    
    for(i in 1:input$number_vars){
      
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated.
      local({
        
        #variable name.
        variable_name = input[[paste0("variable_", i)]]
        
        #Here we create three UI elements for each selected variable:
        #a. A filtering slider
        #b. A binning method selector
        #c. A histogram to show the user the result of their filtering / binning choices
        tags_to_add <<- tagList(
            h3(str_to_title(variable_name)),
            fluidRow(
              column(width = 3,
                     sliderInput(
                       inputId = paste0("slider_",i),
                       label = paste0("Filter ",variable_name),
                       value = c(min(na.omit(UserDatSelected()[[variable_name]])),
                                 max(na.omit(UserDatSelected()[[variable_name]]))),
                       min = min(na.omit(UserDatSelected()[[variable_name]])),
                       max = max(na.omit(UserDatSelected()[[variable_name]])),
                       sep = "",
                       #If the range of the variable in question is more than 6, make steps size of 1.
                       step = ifelse(max(UserDatSelected()[[variable_name]]) - min(UserDatSelected()[[variable_name]]),1,NULL),
                       width = "200%"
                     )
              ), #column end.
              column(width = 3,
                     selectInput(
                       inputId = paste0("bin_",i),
                       label = paste0("Bin ",variable_name),
                       choices = c("Equal Width Bins","Equal Sample Bins","Natural Jenks"),
                       width = "150%"
                     )
              ), #column end.
              column(width = 6,
                     renderPlot({
                       
                       #if(is.numeric(UserDatSelected()[[variable_name]]) == FALSE){
                       #   UserDatBinned() %>%
                       #     ggplot() + 
                       #     geom_bar(aes(x = !!sym(paste0(variable_name,"_binned")), 
                       #                  fill = as.character(!!sym(paste0(variable_name,"_binned"))))) +
                       #     theme_minimal() +
                       #     theme(legend.position = "none",
                       #           text = element_text(size = 20)) +
                       #     scale_fill_brewer(palette = "Dark2")
                       # }
                       
                       #if(is.numeric(UserDatSelected()[[variable_name]]) == TRUE){
                         UserDatBinned() %>%
                           ggplot() +
                             geom_histogram(aes(x = !!sym(variable_name), 
                                                fill = as.character(!!sym(paste0(variable_name,"_binned"))))) +
                           theme_minimal() +
                           theme(legend.position = "none",
                                 text = element_text(size = 20)) +
                           scale_fill_brewer(palette = "Dark2")
                       #}
                     })
              ) #column end.
            ), #fluidRow end.
            hr(style = "border-top: 1px solid #980028"),
        )
      })
  
  binning_panel_taglist = tagList(
    binning_panel_taglist,
    tags_to_add
  )
    }
    
    return(binning_panel_taglist)
  })

  # ---------------------------------------- # 
  #          Model Specification             #
  # ---------------------------------------- # 
  #----------------------------------------------------------------------------
  
  #Render the model selection UI elements:
    #1. Drop-down offering three options: sigmoidal, linear, and hyperbolic.
    #2. Two text entry boxes for model parameters 1 and 2 (detailed below)
    #3. Little figures to show shape of response curve(s)
  
  output$model_selection_panel <- renderUI({
    
    if(is.null(input$variable_1))return(NULL)
    
    if(input$model_selection_type == "lineareq"){
      
      model_selection_panel_taglist = tagList()
      
      for(i in 1:input$number_vars){
        
        #variable name.
        variable_name = input[[paste0("variable_", i)]]
        
        tags_to_add <<- tagList(
          h3(str_to_title(variable_name)),
          fluidRow(
            textInput(
              inputId = paste0("var_coef_",i),
              value = 1,
              label = paste0("Coefficient for variable ",i)
            )
          ), #fluidRow end.
          hr(style = "border-top: 1px solid #980028")
        )
  
        model_selection_panel_taglist = tagList(
          model_selection_panel_taglist,
          tags_to_add
        )
      }
      
      return(model_selection_panel_taglist)
  
    }
    
    if(input$model_selection_type == "stressor"){
      if(is.null(input$variable_1))return(NULL)
      
      model_selection_panel_taglist = tagList()
      
      for(i in 1:input$number_vars){
        
        #dataframe for plotting response curves.
        Plot_DF = reactive({
          #Sigmoidal...
          if(input[[paste0("variable_stressor_model_",i)]] == "sigmoidal"){
            plot_df = data.frame(value = rnorm(100, mean = as.numeric(input[[paste0("variable_coef_1_",i)]]), 
                                               sd = as.numeric(input[[paste0("variable_coef_2_",i)]]))) %>%
              mutate(y = -value^2 + value + 1)
          }
          
          #Hyperbolic...
          if(input[[paste0("variable_stressor_model_",i)]] == "hyperbolic"){
            plot_df = data.frame(value = rnorm(100, mean = 1, 
                                               sd = 1)) %>%
              mutate(y = -(as.numeric(input[[paste0("variable_coef_2_",i)]]))*(value^2) + as.numeric(input[[paste0("variable_coef_1_",i)]]))
          }
          
          #Linear...
          if(input[[paste0("variable_stressor_model_",i)]] == "linear"){
            plot_df = data.frame(value = rnorm(100, mean = 1, 
                                               sd = 1)) %>%
              mutate(y = -as.numeric(input[[paste0("variable_coef_2_",i)]])*(value) + as.numeric(input[[paste0("variable_coef_1_",i)]]))
          }
          return(plot_df)
        })
        
        # Need local so that each item gets its own number. Without it, the value
        # of i in the renderPlot() will be the same across all instances, because
        # of when the expression is evaluated.
        local({
          
          #variable name.
          variable_name = input[[paste0("variable_", i)]]
          
          tags_to_add <<- tagList(
            h3(str_to_title(variable_name)),
            fluidRow(
              column(width = 3,
                     selectInput(
                       inputId = paste0("variable_stressor_model_",i),
                       choices = c("Sigmoidal" = "sigmoidal",
                                   "Linear" = "linear",
                                   "Hyperbolic" = "hyperbolic"),
                       selected = "hyperbolic",
                       label = paste0("Stressor response type for variable ",i),
                     )
              ), #column end.
              column(width = 3,
                     textInput(
                       inputId = paste0("variable_coef_1_",i),
                       value = 1,
                       label = paste0("Mean or Intercept"),
                     ),
                     textInput(
                       inputId = paste0("variable_coef_2_",i),
                       value = 1,
                       label = paste0("Standard deviation or slope"),
                     )
              ), #column end.
              column(width = 6,
                     renderPlot({
                       # #Sigmoidal...
                       # if(input[[paste0("variable_stressor_model_",i)]] == "sigmoidal"){
                       #   plot_df = data.frame(value = rnorm(100, mean = as.numeric(input[[paste0("variable_coef_1_",i)]]), 
                       #                            sd = as.numeric(input[[paste0("variable_coef_2_",i)]]))) %>%
                       #     mutate(y = -value^2 + value + 1)
                       # }
                       # 
                       # #Hyperbolic...
                       # if(input[[paste0("variable_stressor_model_",i)]] == "hyperbolic"){
                       #   plot_df = data.frame(value = rnorm(100, mean = 1, 
                       #                                      sd = 1)) %>%
                       #     mutate(y = -(as.numeric(input[[paste0("variable_coef_2_",i)]]))*(value^2) + as.numeric(input[[paste0("variable_coef_1_",i)]]))
                       # }
                       # 
                       # #Linear...
                       # if(input[[paste0("variable_stressor_model_",i)]] == "linear"){
                       #   plot_df = data.frame(value = rnorm(100, mean = 1, 
                       #                                      sd = 1)) %>%
                       #     mutate(y = -as.numeric(input[[paste0("variable_coef_2_",i)]])*(value) + as.numeric(input[[paste0("variable_coef_1_",i)]]))
                       # }
                       Plot_DF() %>% 
                         #Get rid of plotting values below 0 on y axis.
                         filter(y >= 0) %>% 
                         ggplot(aes(x = value, 
                                    y = y)) +
                         geom_smooth() +
                         theme_minimal() +
                         theme(legend.position = "none",
                               text = element_text(size = 20)) +
                         labs(x = variable_name) +
                         scale_fill_brewer(palette = "Dark2")},
                       height = 400, width = 300)
              ) #column end.
            ), #fluidRow end.
            hr(style = "border-top: 1px solid #980028"),
          )
        })
        
        model_selection_panel_taglist = tagList(
          model_selection_panel_taglist,
          tags_to_add
        )
      }
      
      return(model_selection_panel_taglist)
    }
  })
  
  # Make math form of model. This is just to make a UI for the user.
  ModelDisplay = reactive({
    if(is.null(input$variable_1))return(NULL)
    
    vars = c()
    
    for(i in 1:input$number_vars){
      
      varname = input[[paste0("variable_", i)]]
      
      mod = as.numeric(input[[paste0("var_coef_", i)]])
      
      vars = c(vars, paste0(mod,"(",varname,")"))
    }
    
    model = paste0("Output = ",paste0(vars, collapse = " + "))
    
    return(model)
  })
  
  #output$modeltext_verbatim = renderText(paste0(Output = ModelDisplay()))
  
  output$modeltext = renderUI({
    withMathJax(helpText(paste0("Model: ","$$", ModelDisplay(),"$$")))
    #paste0("$$", ModelDisplay(),"$$")
  })
  
  
  #   #Render drop-down choices for which stressor response model will be used for the variables.
  #   model_choices = c()
  #   observeEvent(input$number_vars, {
  #     
  #     variable_number <- input$number_vars
  #     id <- paste0('variable_stressor_model_', variable_number)
  #     
  #     if (input$number_vars > length(inserted_coefs)) {
  #       insertUI(selector = '#variable_stressor_models',
  #                ui = tags$div(tags$p(
  #                  selectInput(
#                    inputId = paste0("variable_stressor_model_",variable_number),
#                    choices = c("Sigmoidal" = "sigmoidal",
#                                "Linear" = "linear",
#                                "Hyperbolic" = "hyperbolic"),
#                    selected = "sigmoidal",
#                    label = paste0("Stressor model for variable ",variable_number),
#                  )),
#                  id = id))
#       model_choices <<- c(id, model_choices)
#     }
#     else {
#       inserted_coefs <- sort(inserted_coefs)
#       removeUI(selector = paste0('#', inserted_coefs[length(inserted_coefs)]))          
#       inserted_coefs <<- inserted_coefs[-length(inserted_coefs)]
#     }
#   })
#   
#   # Render numeric inputs for each variable with which the user can set the 
#   # parameters of each response function. Parameter 1 is the mean (if sigmoidal) 
#   # or the intercept (if linear or hyperbolic), while parameter 2 is the standard deviation 
#   # (if sigmoidal) or the slope (if linear) or the asymptote (if hyperbolic)
#   inserted_parameter_1 <- c()
#   inserted_parameter_2 <- c()
#   
#   observeEvent(input$number_vars, {
#     
#     variable_number <- input$number_vars
#     id_param_1 <- paste0('variable_param_1_var_', variable_number)
#     id_param_2 <- paste0('variable_param_2_var_', variable_number)
#     
#     if(input$number_vars > length(inserted_parameter_1)) {
#       
#       #If the response curve is sigmoidal...
#       if(input[[paste0("variable_stressor_model_",variable_number)]] == "sigmoidal"){
#         #Parameter 1
#         insertUI(selector = '#variable_coefs',
#                  ui = tags$div(tags$p(
#                    textInput(
#                      inputId = paste0("variable_coef_1_",variable_number),
#                      value = 1,
#                      label = paste0("Mean of sigmoidal distribution"),
#                    )),
#                    id = id_param_1))
#         #Parameter 2
#         insertUI(selector = '#variable_coefs',
#                  ui = tags$div(tags$p(
#                    textInput(
#                      inputId = paste0("variable_coef_2_",variable_number),
#                      value = 1,
#                      label = paste0("Standard deviation"),
#                    )),
#                    id = id_param_1))
#       }
#       
#       #If the response curve is linear...
#       if(input[[paste0("variable_stressor_model_",variable_number)]] == "linear"){
#         #Parameter 1.
#         insertUI(selector = '#variable_coefs',
#                  ui = tags$div(tags$p(
#                    textInput(
#                      inputId = paste0("variable_coef_1_",variable_number),
#                      value = 1,
#                      label = paste0("Intercept"),
#                    )),
#                    id = id_param_1))
#         
#         #Parameter 2.
#         insertUI(selector = '#variable_coefs',
#                  ui = tags$div(tags$p(
#                    textInput(
#                      inputId = paste0("variable_coef_2_",variable_number),
#                      value = 1,
#                      label = paste0("Slope of line"),
#                    )),
#                    id = id_param_2))
#       }
#       
#       #If the response curve is hyperbolic
#       if(input[[paste0("variable_stressor_model_",variable_number)]] == "hyperbolic"){
#         #Parameter 1.
#         insertUI(selector = '#variable_coefs',
#                  ui = tags$div(tags$p(
#                    textInput(
#                      inputId = paste0("variable_coef_1_",variable_number),
#                      value = 1,
#                      label = paste0("Intercept"),
#                    )),
#                    id = id_param_1))
#         
#         #Parameter 2.
#         insertUI(selector = '#variable_coefs',
#                  ui = tags$div(tags$p(
#                    textInput(
#                      inputId = paste0("variable_coef_2_",variable_number),
#                      value = 1,
#                      label = paste0("Asymptote"),
#                    )),
#                    id = id_param_2))
#       }
#       #Add the response curve parameters to their respective vectors.
#       inserted_parameter_1 <<- c(id, inserted_parameter_1)
#       }
#     else {
#       inserted_coefs <- sort(inserted_coefs)
#       removeUI(selector = paste0('#', inserted_coefs[length(inserted_coefs)]))          
#       inserted_coefs <<- inserted_coefs[-length(inserted_coefs)]
#     }
#   })
#   
#   #Make little graphs to show each of the response functions' shape.
#   
#   
  
  #Sum across columns to calculate result column.
  UserDatSummed = reactive({
    if(is.null(input$slider_1)){return(NULL)}

    dat = UserDatBinned()

    #Apply the coefficients entered by the user to each variable.
    
    for(i in 1:input$number_vars){

      variables = paste0(SelectedColumns(),"_binned")
      
      if(input$model_selection_type == "lineareq"){
        dat = dat %>% 
          mutate(mod = input[[paste0("var_coef_", i)]]) %>% 
          mutate(mod = as.numeric(mod)) %>% 
          mutate(!!sym(paste0(variables[i],"_with_mod")) := (mod * !!sym(variables[i])))
      }
      if(input$model_selection_type == "stressor"){
        #See if the variable-in-question's response curve was selected to be sigmoidal, or linear, etc.
        if(input[[paste0("variable_stressor_model_",i)]] == "sigmoidal"){
          
        }
        
        if(input[[paste0("variable_stressor_model_",i)]] == "linear"){
          
        }
        
        if(input[[paste0("variable_stressor_model_",i)]] == "hyperbolic"){
          
        }
      }
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
      
      user_poly = UserScalePoly() %>% 
        mutate(row.number = row_number())
      
      summed_dat = UserDatSummed() %>%
        st_join(user_poly, st_nearest_feature) %>%
        st_drop_geometry() %>%
        group_by(row.number) %>%
        summarise(mean_result = mean(result,na.rm=T))
      
      spatial_results = user_poly %>% 
        left_join(summed_dat)
    }
    
    if(input$bin_results == "Yes"){
      spatial_results = spatial_results %>% 
        mutate(mean_result_binned = as.numeric(cut(mean_result,3)))
    }
    
    return(spatial_results)
  })
  
  #Rasterize spatial results.
  SpatialResultsRast = reactive({
    
    spatial_results_spat = terra::vect(SpatialResults() %>% sf::st_transform(crs = 3005))
    spatial_results_res = terra::rast(spatial_results_spat, resolution = as.numeric(input$raster_res))
    
    if(input$bin_results == "Yes"){
      spatial_results_rast = rasterize(spatial_results_spat, spatial_results_res, field = "mean_result_binned")
    }else{
      spatial_results_rast = rasterize(spatial_results_spat, spatial_results_res, field = "mean_result")
    }
    
    return(spatial_results_rast)
  })
  
  output$spatial_results_table = renderDataTable(SpatialResults())
  
  output$spatial_results_map = renderPlot({
    poly_map = if(input$bin_results == "Yes"){
      ggplot() +
        geom_sf(data = bc, fill = "grey") +
        geom_sf(data = SpatialResults(), aes(col = as.factor(mean_result_binned),
                                             fill = as.factor(mean_result_binned))) +
        geom_sf(data = UserDatBinned(), col = "purple", alpha = 0.5) +
        scale_fill_manual(values = c("1" = "#52ad2b",
                                     "2" = "#db8035",
                                     "3" = "#e02626")) +
        labs(fill = "Model Output",
             col = "Model Output") + 
        theme_minimal() + 
        coord_sf(xlim = c(-125,-120), ylim = c(49, 52))
    }else{
      ggplot() +
        geom_sf(data = bc, fill = "grey") +
        geom_sf(data = SpatialResults(), aes(fill = mean_result, col = mean_result)) +
        geom_sf(data = UserDatBinned(), col = "purple", alpha = 0.5) +
        labs(fill = "Model Output") + 
        theme_minimal() +
        coord_sf(xlim = c(-125,-120), ylim = c(49, 52))
    }

  rast_map = if(input$bin_results == "Yes"){
      gplot(SpatialResultsRast()) +
        geom_tile(aes(fill = as.factor(value))) +
        scale_fill_manual(values = c("1" = "#52ad2b",
                                     "2" = "#db8035",
                                     "3" = "#e02626"),
                          na.value = NA) +
        labs(fill = "Model Output") + 
      theme_minimal()
    }else{
      gplot(SpatialResultsRast()) +
        geom_tile(aes(fill = value)) +
        labs(fill = "Model Output") + 
        theme_minimal() + 
        scale_fill_continuous(na.value = NA)
    }
  
  ggarrange(poly_map, rast_map, ncol = 2)
  },
  width = 1300,
  height = 500)
  
  output$downloadDataTable <- downloadHandler(
    filename = function() {
      paste0('Spatial_Analysis_App_Output_Table_', Sys.Date(), '.gpkg')
    },
    content = function(con) {
      openxlsx::write.xlsx(SpatialResults() %>% st_drop_geometry(), con)
    }
  )
  
  output$downloadData <- downloadHandler(
      filename = function() {
        paste0('Spatial_Analysis_App_Output_Vector_', Sys.Date(), '.gpkg')
      },
      content = function(con) {
        write_sf(SpatialResults(), con)
      }
    )
  
  output$downloadDataRaster <- downloadHandler(
    filename = function() {
      paste0('Spatial_Analysis_App_Output_Raster_', Sys.Date(), '.tiff')
    },
    content = function(con) {
      terra::writeRaster(SpatialResultsRast(), con)
    }
  )
}

shinyApp(ui = ui, server = server)
