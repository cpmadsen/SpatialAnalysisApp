## Tab panels
library(bs4Dash)

# Data upload box.

# data_upload_box = box(
#   fileInput(inputId = "user_data", label = "Dataset",
#             accept = c(".zip",".gpkg","xlsx"),
#             placeholder = "Upload file (zip, gpkg, or xlsx")
# )

data_upload_panel = tabItem("dataUpload",
                            column(width = 6,
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
                            column(width = 2,
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
                            ))

# data_upload_panel = tabItem("dataUpload",
#                             accordion(
#                               id = 'data_upload_accordion',
#                               accordionItem(
#                                 title = "Select Dataset",
#                                 solidHeader = T,
#                                 collapsed = F,
#                                 fileInput(inputId = "user_data", label = "Dataset",
#                                           accept = c(".zip",".gpkg","xlsx"),
#                                           placeholder = "Upload file (zip, gpkg, or xlsx")
#                               ),
#                               accordionItem(
#                                 title = 'Data Snapshot',
#                                 #h5("Snapshot of your data."),
#                                 inputPanel(
#                                   uiOutput('cols_checkboxes'),
#                                   dataTableOutput('data_preview')
#                                 )
#                               ),
#                               accordionItem(
#                                 title = "Select Variables",
#                                 numericInput(inputId = "number_vars",
#                                              label = "Number of Variables",
#                                              value = 1, min = 1, max = 6, width = '700px'),
#                                 tags$div(id = 'variable_selectors'),
#                                 uiOutput('factorizing')
#                               )
#                             )
# )

data_cleaning_panel = tabItem("dataCleaning_Binning",
                              uiOutput("binning_panel"),
                              fluidRow(
                                dataTableOutput('bin_check'),
                                textOutput('label_check')
                              )
)

model_specification_panel = tabItem("modelSpecification",
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
)

results_panel = tabItem("Results",
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

## Full panels

my_header = dashboardHeader(
  #title = "Test"
  title = dashboardBrand(
    title = "Spatial Analysis Tool",
    color = "primary"
    # href = "https://adminlte.io/themes/v3",
    # image = div(img(src="robot-head.png", width = '100px')),
  )
  # leftUi = tagList(data_upload_panel,
  #         data_cleaning_panel,
  #         model_specification_panel,
  #         results_panel
  #       )
)

my_sidebar = bs4Dash::dashboardSidebar(
  inputPanel(
    align = "center",
    sidebarMenu(
      div(
        menuItem(
          tagList(
            h5("Step 1"),
            h6('Upload data, select variables')
          ), tabName = "dataUpload"),
        style = 'height:50%;'
      ),
      menuItem(h5("Step 2"), tabName = "dataCleaning_Binning"),
      h6('Clean and bin variables'),
      menuItem(h5("Step 3"), tabName = 'modelSpecification'),
      h6('Select interaction model'),
      menuItem(h5("Step 4"), tabName = 'Results'),
      h6('Inspect and download results')
    ),
    hr(style = "border-top: 1px solid #980028; opacity: 0"),
    hr(style = "border-top: 5px solid #980028;"),
    h3("Contact Information"),
    h5("App Developer: Chris Madsen"),
    h5("Chris.Madsen@gov.bc.ca")
  ),
  width = 4,
  minified = F
)

my_mainpanel = dashboardBody(
  withMathJax(),
  tabItems(
    data_upload_panel,
    data_cleaning_panel,
    model_specification_panel,
    results_panel
  ),
  width = 8
)