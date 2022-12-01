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