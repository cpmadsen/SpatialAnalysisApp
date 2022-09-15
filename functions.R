# Custom functions for Spatial Analysis App

read_multiform_data = function(x){
  
  #x must be the 'x' of data being read in through Shiny)
  #If it's a geopackage, read it in directly.
  if(str_detect(x, ".gpkg")){
    userpoly = read_sf(x) %>%
      st_transform(crs = 4326)
    return(userpoly)
  } else if (str_detect(x, ".zip")){
    #If it's a zipped shapefile, unzip then read in.
    
    filelist <- unzip(x)
    userpoly = read_sf(filelist[str_detect(filelist, ".shp")]) %>%
      st_transform(crs = 4326)
    
    return(userpoly)
  } else if (str_detect(x, ".xlsx")){
    if(str_detect(x, ".xlsx")){
      userpoly = read_excel(x)
    }
    
    #If the excel table has lat/lon, use the following chunk.
    likely_lon = names(userpoly)[str_detect(str_to_lower(names(userpoly)), "lon(g)?")]
    likely_lat = names(userpoly)[str_detect(str_to_lower(names(userpoly)), "lat")]
    userpoly = st_as_sf(userpoly, coords = c(likely_lon, likely_lat), crs = 4326)
    
    return(userpoly)
  } else if(str_detect(x, ".csv")){
    if(str_detect(x, ".csv")){
      userpoly = read_csv(x)
    }
    
    likely_lon = names(userpoly)[str_detect(str_to_lower(names(userpoly)), "lon(g)?")]
    likely_lat = names(userpoly)[str_detect(str_to_lower(names(userpoly)), "lat")]
    userpoly = st_as_sf(userpoly, coords = c(likely_lon, likely_lat), crs = 4326)
    
    return(userpoly)
  }
}