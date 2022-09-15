results_tab = tabPanel("Results",
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