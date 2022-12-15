# Instructions alert.

observeEvent(input$instructions_request, {
  showModal(
    modalDialog(
    "The Spatial Analysis Tool is an app I created in Rstudio and Shiny
    to facilitate spatial assessments for people without an intimate knowledge 
    of GIS, R or Python. This app allows you to upload your dataset, clean it and split it
    into discrete 'bins', select how your chosen variables will interact, visualize
    the results, and then download the results, if you wish.
    
    \nTo keep reading, click 'Next'.",
    title = 'Spatial Analysis Tool Introduction',
    easyClose = TRUE,
    fade = F,
    footer = actionButton(inputId = 'intro_step_2',
                          label = "Next")
  ))
})

observeEvent(input$intro_step_2, {
  showModal(
    modalDialog(
      "Step one is to upload your data. Accepted formats include a shapefile that 
      has been zipped into one folder, a geopackage, or even an excel file that has obvious columns
      for latitude and longitude (or northings/eastings, depending on your projection system)",
    
      title = 'Spatial Analysis Tool Introduction - 1',
      easyClose = TRUE,
      fade = F,
      footer = actionButton(inputId = 'intro_step_3',
                            label = "Next")
    ))
})

observeEvent(input$intro_step_3, {
  showModal(
    modalDialog(
      "In step two, you can see the distribution of your selected variables, filter
      out any extreme values, and choose the binning method. Check out the data
      preview table at the bottom of the screen to see how your data is being filtered
      and binned.",
    
      title = 'Spatial Analysis Tool Introduction - 2',
      easyClose = TRUE,
      fade = F,
      footer = actionButton(inputId = 'intro_step_4',
                            label = "Next")
    ))
})

observeEvent(input$intro_step_4, {
  showModal(
    modalDialog(
      "In step three, you choose how variables will be combined to produce some output variable.
      Example output variables could be risk of introduction of invasive species, or habitat suitability for endangered species.
      You can choose a linear model or a stressor response function, and choose positive or negative weights
      for your variables. ",
      
      title = 'Spatial Analysis Tool Introduction - 3',
      easyClose = TRUE,
      fade = F,
      footer = actionButton(inputId = 'intro_step_5',
                            label = "Next")
    ))
})

observeEvent(input$intro_step_5, {
  showModal(
    modalDialog(
      "Step four is exciting - you get to see the fruits of your labour! Upload a spatial layer if you have
      one for your project, or use the pre-loaded natural resource regions ('FLNRORD regions'). Inspect the results
      either as vector output on the left or raster output. Click the downoad buttons
      on the bottom of the page to download results as a table, shapefile, or raster.",
      
      title = 'Spatial Analysis Tool Introduction - 4',
      easyClose = TRUE,
      fade = F
    ))
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

#React to data upload by updating which accordion item is open.
observeEvent(input$user_data, {
  updateAccordion('variable_upload_accordion',
                  selected = 2)
}
)

#Preview table to show user the data they've uploaded.
output$data_preview <- renderDataTable({
  if(is.null(input$user_data)) return(NULL)
  
  UserDat()[1:5,] %>% 
    select(all_of(input$selected_cols)) %>% 
    mutate_if(is.numeric, ~round(.x,2)) %>% 
    st_drop_geometry()
})

#Which columns of the preview data will we display? Up to the user.
output$cols_checkboxes = renderUI(
  checkboxGroupInput("selected_cols", "",
                     names(UserDat()), 
                     selected = names(UserDat())[1:3])
)

# Get a reactive vector of column names that have been selected.
SelectedColumns = reactive({
  input$selected_cols
  # selected_vars = c()
  # for(i in 1:input$number_vars){
  #   selected_vars = c(selected_vars, input[[paste0("variable_",i)]])
  # }
  # selected_vars
})

#For any chosen variables that are characters, we need a way to convert to numbers.
#Render a UI element for the user to choose factor levels.
output$factorizing = renderUI({
  
  #If no variable has been selected yet, skip this for now...
  if(is.null(input$selected_cols[1]))return(NULL)
  
  #Initialize tag list for this multipart UI element.
  factorizing_taglist = tagList()
  
  #For each variable that is NOT numeric, add a UI element to the 'factorizing_taglist'
  for(i in 1:length(input$selected_cols)){
    #Get variable name for this round of the loop...
    variable_name = input$selected_cols[i]
    
    #Is it categorical (i.e. NOT a number)? If so, do the following.
    if(is.character(UserDat()[[variable_name]]) == TRUE){
      
      tags_to_add = tagList(
        h4(paste0('Choose levels for variable "',variable_name,'"')),
        rank_list(
          input_id = paste0("factor_list_",i),
          text = "Sort high to low (NA's are removed)",
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
  if(is.null(input$selected_cols[1]))return(NULL)
  
  #Bring in reactive user dataset that they have uploaded.
  dat = UserDat()
  
  # If the variable needs to be converted from a character category to a number,
  # use the ordering the user has provided with the drag-and-drop UI element.
  # If no factorization is needed for a given variable, keep the variable as is.
  for(i in 1:length(input$selected_cols)){
    
    #If we have some info from the factorization drag-and-drop for variable i...
    if(is.null(input[[paste0("factor_list_",i)]]) == FALSE){
      
      #Grab variable name and sorting heuristic.
      variable_name = input$selected_cols[i]
      variable_sorter = c(rev(input[[paste0("factor_list_",i)]]))
      #variable_sorter = c("Data missing", rev(input[[paste0("factor_list_",i)]]))
      
      #Modify variable selected by loop: convert character to ordered factor (numeric)
      dat = dat %>% 
        ##Replace NA with 0...
        ##mutate(!!sym(variable_name) := replace(!!sym(variable_name), is.na(!!sym(variable_name)), "Data missing")) %>%
        ##Make new column that will be used as x-axis markers in the histograms of the binning page
        mutate(!!sym(paste0(variable_name,"_label")) := !!sym(variable_name)) %>% 
        ##Convert variable i to a numeric ordered factor.
        mutate(!!sym(paste0(variable_name,"_as_character")) := factor(!!sym(variable_name),
                                                         levels = variable_sorter)) %>% 
        mutate(!!sym(variable_name) := as.numeric(factor(!!sym(variable_name),
                                                         levels = variable_sorter))) %>% 
        #Add the number to the label.
        mutate(!!sym(paste0(variable_name,"_label")) := paste0(!!sym(paste0(variable_name,"_label")),
                                                               " (",
                                                               !!sym(variable_name),
                                                               ")"))
    }
  }
  
  dat
})