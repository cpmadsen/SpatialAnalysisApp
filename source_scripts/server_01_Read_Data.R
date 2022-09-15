# This script runs the server logic required to read in the user's data, factorize categorical data, 
#   and apply the user's selection of variables to the dataset.

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