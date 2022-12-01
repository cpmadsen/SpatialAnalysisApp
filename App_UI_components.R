## Tab panels
library(bs4Dash)
library(fresh)
# Data upload box.

# data_upload_box = box(
#   fileInput(inputId = "user_data", label = "Dataset",
#             accept = c(".zip",".gpkg","xlsx"),
#             placeholder = "Upload file (zip, gpkg, or xlsx")
# )

data_upload_panel = tabItem("dataUpload",
                            accordion(
                              id = 'variable_upload_accordion',
                              accordionItem(
                                title = '1. Upload Dataset',
                                collapsed = F,
                                fileInput(inputId = "user_data", label = "",
                                          accept = c(".zip",".gpkg","xlsx"),
                                          placeholder = "Upload file (zip, gpkg, or xlsx")
                              ),
                              accordionItem(
                                title = '2. Select Variables',
                                collapsed = T,
                                fluidRow(
                                  column(width = 3,
                                         uiOutput('cols_checkboxes')
                                  ),
                                  column(width = 9,
                                         dataTableOutput('data_preview')
                                  )
                                ),
                                # inputPanel(
                                #   numericInput(inputId = "number_vars",
                                #                label = "Number of Variables",
                                #                value = 1, min = 1, max = 6, width = '700px')
                                # ),
                                # inputPanel(
                                #   tags$div(id = 'variable_selectors')
                                # ),
                                uiOutput('factorizing')
                              )
                            )
)

data_cleaning_panel = tabItem("dataCleaning_Binning",
                              box(
                                title = 'Filter and Bin Your Data',
                                width = 12,
                                uiOutput("binning_panel")
                              ),
                              box(
                                title = "Binned Data Preview",
                                width = 12,
                                dataTableOutput('bin_check') 
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
                          value = 50000
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
      div(
        menuItem(
          tagList(
            h5("Step 2"),
            h6('Clean and bin variables')
          ), tabName = "dataCleaning_Binning"),
        style = 'height:50%;'
      ),
      div(
        menuItem(
          tagList(
            h5("Step 3"),
            h6('Select interaction model')
          ), tabName = "modelSpecification"),
        style = 'height:50%;'
      ),
      div(
        menuItem(
          tagList(
            h5("Step 4"),
            h6('Inspect and download results')
          ), tabName = "Results"),
        style = 'height:50%;'
      )
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