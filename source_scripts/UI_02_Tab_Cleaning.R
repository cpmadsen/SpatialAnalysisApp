cleaning_tab = tabPanel("Data Cleaning + Binning",
                        uiOutput("binning_panel"),
                        fluidRow(
                          dataTableOutput('bin_check'),
                          textOutput('label_check')
                        )
)