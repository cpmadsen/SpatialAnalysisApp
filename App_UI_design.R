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