data_tab = tabPanel("Data Upload",
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
                    ))