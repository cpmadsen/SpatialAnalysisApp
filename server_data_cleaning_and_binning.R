#Take each variable from the user's dataset and apply the range filters.
#Keep only selected variables. 
#Save the result as a reactive expression.
UserDatFiltered = reactive({
  if(is.null(input$slider_1))return(NULL)
  
  user_selected_columns = SelectedColumns()
  
  #If the user has uploaded categorical data that have been turned into an ordered factor,
  #this section adds the 'label' version of those variables so that the user can see them in the
  #preview table of the binning section.
  for(i in 1:length(input$selected_cols)){
    
    #variable name.
    variable_name = input$selected_cols[i]
    
    if(is.null(UserDatSelected()[[paste0(variable_name,"_label")]]) == FALSE){
      user_selected_columns = c(user_selected_columns, paste0(variable_name,"_label"))
    }
  }
  
  dat = UserDatSelected() %>% 
    select(all_of(user_selected_columns))
  
  #Cycle through all of the variable inputs, applying filters as you go.
  for(i in 1:length(input$selected_cols)){
    
    variable_name = input$selected_cols[i]
    
    dat = dat %>% 
      dplyr::filter(.[[variable_name]] >= input[[paste0("slider_",i)]][1],
                    .[[variable_name]] <= input[[paste0("slider_",i)]][2])
    
    #If this variable is numeric, just keep up to 3 decimal places...
    if(is.numeric(dat[[variable_name]])){
      dat = dat %>% 
        mutate(!!sym(variable_name) := round(!!sym(variable_name),3))
    }
  }
  
  dat
})

#Bin user data, depending on bin input. If the variable in question is an ordered factor, 
#  don't bin it.
UserDatBinned = reactive({
  if(is.null(input$slider_1) | is.null(input$bin_1))return(NULL)
  
  dat = UserDatFiltered()
  
  variables = SelectedColumns()
  
  #For each of the variables the user has selected from their dataset...
  for(i in 1:length(variables)){
    
    #If the variable is an ordered factor, don't bin it. Skip to next variable.
    if(is.null(input[[paste0("factor_list_",i)]]) == FALSE){
      dat = dat %>% 
        mutate(!!sym(paste0(variables[i],"_binned")) := !!sym(variables[i]))
      #mutate(!!sym(variables[i]) := !!sym(paste0(variables[i],"_label"))) %>% 
      #select(-!!sym(paste0(variables[i],"_label")))
      next
    }
    
    #If user selects "Equal Sample Bins" option...
    if(input[[paste0("bin_",i)]] == "Equal Sample Bins"){
      dat = dat %>% 
        mutate(!!sym(paste0(variables[i],"_binned")) := as.numeric(Hmisc::cut2(!!sym(variables[i]), g = 3)))
    }
    #If user selects "Equal Width Bins" option...
    if(input[[paste0("bin_",i)]] == "Equal Width Bins"){
      dat = dat %>% 
        mutate(!!sym(paste0(variables[i],"_binned")) := as.numeric(cut(!!sym(variables[i]),3)))
      
    }
    #If user selects "Natural Jenks" option...
    if(input[[paste0("bin_",i)]] == "Natural Jenks"){
      dat = dat %>% 
        mutate(!!sym(paste0(variables[i],"_binned")) := as.numeric(cut(!!sym(variables[i]), 
                                                                       breaks = getJenksBreaks(!!sym(variables[i]),
                                                                                               k = 4),
                                                                       include.lowest = T)))
    }
  }
  return(dat)
})

#Render a table for the user to see the effects of their binning choices.
output$bin_check = renderDataTable(
  #If there's a geometry column, drop it.
  if(!is.na(st_is_longlat(UserDatBinned()))){
    UserDatBinned() %>%
      st_drop_geometry()
  } else {
    #If not geometry column, just call this reactive object.
    UserDatBinned()
  }
  
)


#Render the data filtering and binning options for each selected variable.
# output$binning_panel <- renderUI({
output$binning_panel <- renderUI({
  
  #f(is.null(input$selected_cols[1]))return(NULL)
  
  #binning_panel_taglist = tagList()
  
  # for(i in 1:length(input$selected_cols)){
  #   
  #   # Need local so that each item gets its own number. Without it, the value
  #   # of i in the renderPlot() will be the same across all instances, because
  #   # of when the expression is evaluated.
  #   local({
  #     
  #     #variable name.
  #     variable_name = input$selected_cols[i]
  #     
  #     #Here we create three UI elements for each selected variable:
  #     #a. A filtering slider
  #     #b. A binning method selector
  #     #c. A histogram to show the user the result of their filtering / binning choices
  #     tags_to_add <<- tagList(
  #       h3(str_to_title(variable_name)),
  #       fluidRow(
  #         column(width = 3,
  #                sliderInput(
  #                  inputId = paste0("slider_",i),
  #                  label = paste0("Filter ",variable_name),
  #                  value = c(min(na.omit(UserDatSelected()[[variable_name]])),
  #                            max(na.omit(UserDatSelected()[[variable_name]]))),
  #                  min = min(na.omit(UserDatSelected()[[variable_name]])),
  #                  max = max(na.omit(UserDatSelected()[[variable_name]])),
  #                  sep = "",
  #                  #If the range of the variable in question is more than 6, make steps size of 1.
  #                  step = ifelse(max(UserDatSelected()[[variable_name]]) - min(UserDatSelected()[[variable_name]]),1,NULL),
  #                  width = "200%"
  #                )
  #         ), #column end.
  #         column(width = 3,
  #                selectInput(
  #                  inputId = paste0("bin_",i),
  #                  label = paste0("Bin ",variable_name),
  #                  choices = c("Equal Width Bins","Equal Sample Bins","Natural Jenks"),
  #                  selected = 'Natural Jenks',
  #                  width = "150%"
  #                )
  #         ), #column end.
  #         column(width = 6,
  #                renderPlot({
  #                  #if(is.numeric(UserDatSelected()[[variable_name]]) == TRUE){
  #                  my_plot = UserDatBinned() %>%
  #                    ggplot() +
  #                    geom_histogram(aes(x = !!sym(variable_name), 
  #                                       fill = as.character(!!sym(paste0(variable_name,"_binned"))))) +
  #                    theme_minimal() +
  #                    theme(legend.position = "none",
  #                          text = element_text(size = 20)) +
  #                    scale_fill_brewer(palette = "Dark2")
  #                  
  #                  if(is.character(UserDatBinned()[[variable_name]])){
  #                    my_plot = my_plot + scale_x_discrete()
  #                  }
  #                  my_plot
  #                  #}
  #                })
  #         ) #column end.
  #       ), #fluidRow end.
  #       hr(style = "border-top: 1px solid #980028"),
  #     )
  #   })
  #   
  #   binning_panel_taglist = tagList(
  #     binning_panel_taglist,
  #     tags_to_add
  #   )
  # }
  
  #for(i in 1:length(input$selected_cols)){
  
  # # Need local so that each item gets its own number. Without it, the value
  # # of i in the renderPlot() will be the same across all instances, because
  # # of when the expression is evaluated.
  # local({
  
  #Here we create three UI elements for each selected variable:
  #a. A filtering slider
  #b. A binning method selector
  #c. A histogram to show the user the result of their filtering / binning choices
  
  do.call(
    tabsetPanel, 
    lapply(1:length(input$selected_cols), function(i){
      
      variable_name = input$selected_cols[i]
      
      tags_to_add = tabPanel(
        title = variable_name,
        fluidRow(
          column(width = 6,
                 box(
                   title = "Filter",
                   collapsible = F,
                   status = 'success',
                   width = 12,
                   sliderInput(
                     inputId = paste0("slider_",i),
                     label = '',
                     value = c(min(na.omit(UserDatSelected()[[variable_name]])),
                               max(na.omit(UserDatSelected()[[variable_name]]))),
                     min = min(na.omit(UserDatSelected()[[variable_name]])),
                     max = max(na.omit(UserDatSelected()[[variable_name]])),
                     sep = "",
                     #If the range of the variable in question is more than 6, make steps size of 1.
                     step = ifelse(max(UserDatSelected()[[variable_name]]) - min(UserDatSelected()[[variable_name]]),1,NULL),
                     width = "200%"
                   )
                 ), #top-left fluid row end.
                 box(
                   title = "Bin Method",
                   collapsible = F,
                   status = 'warning',
                   width = 12,
                   selectInput(
                     inputId = paste0("bin_",i),
                     label = '',
                     choices = c("Equal Width Bins","Equal Sample Bins","Natural Jenks"),
                     selected = 'Natural Jenks',
                     width = "150%"
                   )
                 ) #bottom-left fluid row end.
          ), #end of left column.
          column(width = 6,
                 renderPlot({
                   #if(is.numeric(UserDatSelected()[[variable_name]]) == TRUE){
                   my_plot = UserDatBinned() %>%
                     ggplot() +
                     geom_histogram(aes(x = !!sym(variable_name),
                                        fill = as.character(!!sym(paste0(variable_name,"_binned"))))) +
                     theme_minimal() +
                     theme(legend.position = "none",
                           text = element_text(size = 20)) +
                     scale_fill_brewer(palette = "Dark2")
                   
                   if(is.character(UserDatBinned()[[variable_name]])){
                     my_plot = my_plot + scale_x_discrete()
                   }
                   my_plot
                   #}
                 })
          ) #column end.
        ) #fluidRow end.
      )
      
      # binning_panel_taglist = tagList(
      #   binning_panel_taglist,
      #   tags_to_add
      # )
      
    } #End of loop that makes new tab Panels.
    
    )
  )
  # binning_tabsetPanel = tabsetPanel(
  #   id = 'binning_tabset',
  #   binning_panel_taglist
  # )
  
  # return(binning_tabsetPanel)
})