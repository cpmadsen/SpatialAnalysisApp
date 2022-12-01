#Render the model selection UI elements:
#1. Drop-down offering three options: sigmoidal, linear, and hyperbolic.
#2. Two text entry boxes for model parameters 1 and 2 (detailed below)
#3. Little figures to show shape of response curve(s)

output$model_selection_panel <- renderUI({
  
  if(is.null(input$selected_cols[1]))return(NULL)
  
  if(input$model_selection_type == "lineareq"){
    
    model_selection_panel_taglist = tagList()
    
    for(i in 1:length(input$selected_cols)){
      
      #variable name.
      variable_name = input$selected_cols[i]
      
      tags_to_add <<- tagList(
        #h3(str_to_title(variable_name)),
        fluidRow(
          textInput(
            inputId = paste0("var_coef_",i),
            value = 1,
            label = paste0('Coefficient for variable "',variable_name,'"')
          )
        ), #fluidRow end.
        hr(style = "border-top: 1px solid #980028")
      )
      
      model_selection_panel_taglist = tagList(
        model_selection_panel_taglist,
        tags_to_add
      )
    }
    
    return(model_selection_panel_taglist)
    
  }
  
  if(input$model_selection_type == "stressor"){
    if(is.null(input$selected_cols[1]))return(NULL)
    
    model_selection_panel_taglist = tagList()
    
    for(i in 1:length(input$selected_cols)){
      
      #dataframe for plotting response curves.
      Plot_DF = reactive({
        #Sigmoidal...
        if(input[[paste0("variable_stressor_model_",i)]] == "sigmoidal"){
          plot_df = data.frame(value = rnorm(100, mean = as.numeric(input[[paste0("variable_coef_1_",i)]]), 
                                             sd = as.numeric(input[[paste0("variable_coef_2_",i)]]))) %>%
            mutate(y = -value^2 + value + 1)
        }
        
        #Hyperbolic...
        if(input[[paste0("variable_stressor_model_",i)]] == "hyperbolic"){
          plot_df = data.frame(value = rnorm(100, mean = 1, 
                                             sd = 1)) %>%
            mutate(y = -(as.numeric(input[[paste0("variable_coef_2_",i)]]))*(value^2) + as.numeric(input[[paste0("variable_coef_1_",i)]]))
        }
        
        #Linear...
        if(input[[paste0("variable_stressor_model_",i)]] == "linear"){
          plot_df = data.frame(value = rnorm(100, mean = 1, 
                                             sd = 1)) %>%
            mutate(y = -as.numeric(input[[paste0("variable_coef_2_",i)]])*(value) + as.numeric(input[[paste0("variable_coef_1_",i)]]))
        }
        return(plot_df)
      })
      
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated.
      local({
        
        #variable name.
        variable_name = input[[paste0("variable_", i)]]
        
        tags_to_add <<- tagList(
          h3(str_to_title(variable_name)),
          fluidRow(
            column(width = 3,
                   selectInput(
                     inputId = paste0("variable_stressor_model_",i),
                     choices = c("Sigmoidal" = "sigmoidal",
                                 "Linear" = "linear",
                                 "Hyperbolic" = "hyperbolic"),
                     selected = "hyperbolic",
                     label = paste0("Stressor response type for variable ",i),
                   )
            ), #column end.
            column(width = 3,
                   textInput(
                     inputId = paste0("variable_coef_1_",i),
                     value = 1,
                     label = paste0("Mean or Intercept"),
                   ),
                   textInput(
                     inputId = paste0("variable_coef_2_",i),
                     value = 1,
                     label = paste0("Standard deviation or slope"),
                   )
            ), #column end.
            column(width = 6,
                   renderPlot({
                     # #Sigmoidal...
                     # if(input[[paste0("variable_stressor_model_",i)]] == "sigmoidal"){
                     #   plot_df = data.frame(value = rnorm(100, mean = as.numeric(input[[paste0("variable_coef_1_",i)]]), 
                     #                            sd = as.numeric(input[[paste0("variable_coef_2_",i)]]))) %>%
                     #     mutate(y = -value^2 + value + 1)
                     # }
                     # 
                     # #Hyperbolic...
                     # if(input[[paste0("variable_stressor_model_",i)]] == "hyperbolic"){
                     #   plot_df = data.frame(value = rnorm(100, mean = 1, 
                     #                                      sd = 1)) %>%
                     #     mutate(y = -(as.numeric(input[[paste0("variable_coef_2_",i)]]))*(value^2) + as.numeric(input[[paste0("variable_coef_1_",i)]]))
                     # }
                     # 
                     # #Linear...
                     # if(input[[paste0("variable_stressor_model_",i)]] == "linear"){
                     #   plot_df = data.frame(value = rnorm(100, mean = 1, 
                     #                                      sd = 1)) %>%
                     #     mutate(y = -as.numeric(input[[paste0("variable_coef_2_",i)]])*(value) + as.numeric(input[[paste0("variable_coef_1_",i)]]))
                     # }
                     Plot_DF() %>% 
                       #Get rid of plotting values below 0 on y axis.
                       filter(y >= 0) %>% 
                       ggplot(aes(x = value, 
                                  y = y)) +
                       geom_smooth() +
                       theme_minimal() +
                       theme(legend.position = "none",
                             text = element_text(size = 20)) +
                       labs(x = variable_name) +
                       scale_fill_brewer(palette = "Dark2")},
                     height = 400, width = 300)
            ) #column end.
          ), #fluidRow end.
          hr(style = "border-top: 1px solid #980028"),
        )
      })
      
      model_selection_panel_taglist = tagList(
        model_selection_panel_taglist,
        tags_to_add
      )
    }
    
    return(model_selection_panel_taglist)
  }
})

# Make math form of model. This is just to make a UI for the user.
ModelDisplay = reactive({
  if(is.null(input$selected_cols[1]))return(NULL)
  
  vars = c()
  
  for(i in 1:length(input$selected_cols)){
    
    varname = input$selected_cols[i]
    
    mod = as.numeric(input[[paste0("var_coef_", i)]])
    
    vars = c(vars, paste0(mod,"(",varname,")"))
  }
  
  model = paste0(paste0(vars, collapse = " + ")," = Output")
  
  return(model)
})

#output$modeltext_verbatim = renderText(paste0(Output = ModelDisplay()))

output$modeltext = renderUI({
  withMathJax(helpText(paste0("Model: ","$$", ModelDisplay(),"$$")))
  #paste0("$$", ModelDisplay(),"$$")
})


#   #Render drop-down choices for which stressor response model will be used for the variables.
#   model_choices = c()
#   observeEvent(input$number_vars, {
#     
#     variable_number <- input$number_vars
#     id <- paste0('variable_stressor_model_', variable_number)
#     
#     if (input$number_vars > length(inserted_coefs)) {
#       insertUI(selector = '#variable_stressor_models',
#                ui = tags$div(tags$p(
#                  selectInput(
#                    inputId = paste0("variable_stressor_model_",variable_number),
#                    choices = c("Sigmoidal" = "sigmoidal",
#                                "Linear" = "linear",
#                                "Hyperbolic" = "hyperbolic"),
#                    selected = "sigmoidal",
#                    label = paste0("Stressor model for variable ",variable_number),
#                  )),
#                  id = id))
#       model_choices <<- c(id, model_choices)
#     }
#     else {
#       inserted_coefs <- sort(inserted_coefs)
#       removeUI(selector = paste0('#', inserted_coefs[length(inserted_coefs)]))          
#       inserted_coefs <<- inserted_coefs[-length(inserted_coefs)]
#     }
#   })
#   
#   # Render numeric inputs for each variable with which the user can set the 
#   # parameters of each response function. Parameter 1 is the mean (if sigmoidal) 
#   # or the intercept (if linear or hyperbolic), while parameter 2 is the standard deviation 
#   # (if sigmoidal) or the slope (if linear) or the asymptote (if hyperbolic)
#   inserted_parameter_1 <- c()
#   inserted_parameter_2 <- c()
#   
#   observeEvent(input$number_vars, {
#     
#     variable_number <- input$number_vars
#     id_param_1 <- paste0('variable_param_1_var_', variable_number)
#     id_param_2 <- paste0('variable_param_2_var_', variable_number)
#     
#     if(input$number_vars > length(inserted_parameter_1)) {
#       
#       #If the response curve is sigmoidal...
#       if(input[[paste0("variable_stressor_model_",variable_number)]] == "sigmoidal"){
#         #Parameter 1
#         insertUI(selector = '#variable_coefs',
#                  ui = tags$div(tags$p(
#                    textInput(
#                      inputId = paste0("variable_coef_1_",variable_number),
#                      value = 1,
#                      label = paste0("Mean of sigmoidal distribution"),
#                    )),
#                    id = id_param_1))
#         #Parameter 2
#         insertUI(selector = '#variable_coefs',
#                  ui = tags$div(tags$p(
#                    textInput(
#                      inputId = paste0("variable_coef_2_",variable_number),
#                      value = 1,
#                      label = paste0("Standard deviation"),
#                    )),
#                    id = id_param_1))
#       }
#       
#       #If the response curve is linear...
#       if(input[[paste0("variable_stressor_model_",variable_number)]] == "linear"){
#         #Parameter 1.
#         insertUI(selector = '#variable_coefs',
#                  ui = tags$div(tags$p(
#                    textInput(
#                      inputId = paste0("variable_coef_1_",variable_number),
#                      value = 1,
#                      label = paste0("Intercept"),
#                    )),
#                    id = id_param_1))
#         
#         #Parameter 2.
#         insertUI(selector = '#variable_coefs',
#                  ui = tags$div(tags$p(
#                    textInput(
#                      inputId = paste0("variable_coef_2_",variable_number),
#                      value = 1,
#                      label = paste0("Slope of line"),
#                    )),
#                    id = id_param_2))
#       }
#       
#       #If the response curve is hyperbolic
#       if(input[[paste0("variable_stressor_model_",variable_number)]] == "hyperbolic"){
#         #Parameter 1.
#         insertUI(selector = '#variable_coefs',
#                  ui = tags$div(tags$p(
#                    textInput(
#                      inputId = paste0("variable_coef_1_",variable_number),
#                      value = 1,
#                      label = paste0("Intercept"),
#                    )),
#                    id = id_param_1))
#         
#         #Parameter 2.
#         insertUI(selector = '#variable_coefs',
#                  ui = tags$div(tags$p(
#                    textInput(
#                      inputId = paste0("variable_coef_2_",variable_number),
#                      value = 1,
#                      label = paste0("Asymptote"),
#                    )),
#                    id = id_param_2))
#       }
#       #Add the response curve parameters to their respective vectors.
#       inserted_parameter_1 <<- c(id, inserted_parameter_1)
#       }
#     else {
#       inserted_coefs <- sort(inserted_coefs)
#       removeUI(selector = paste0('#', inserted_coefs[length(inserted_coefs)]))          
#       inserted_coefs <<- inserted_coefs[-length(inserted_coefs)]
#     }
#   })
#   
#   #Make little graphs to show each of the response functions' shape.
#   
#   

#Sum across columns to calculate result column.
UserDatSummed = reactive({
  if(is.null(input$slider_1)){return(NULL)}
  
  dat = UserDatBinned()
  
  #Apply the coefficients entered by the user to each variable.
  
  for(i in 1:length(input$selected_cols)){
    
    variables = paste0(SelectedColumns(),"_binned")
    
    if(input$model_selection_type == "lineareq"){
      dat = dat %>% 
        mutate(mod = input[[paste0("var_coef_", i)]]) %>% 
        mutate(mod = as.numeric(mod)) %>% 
        mutate(!!sym(paste0(variables[i],"_with_mod")) := (mod * !!sym(variables[i])))
    }
    if(input$model_selection_type == "stressor"){
      #See if the variable-in-question's response curve was selected to be sigmoidal, or linear, etc.
      if(input[[paste0("variable_stressor_model_",i)]] == "sigmoidal"){
        
      }
      
      if(input[[paste0("variable_stressor_model_",i)]] == "linear"){
        
      }
      
      if(input[[paste0("variable_stressor_model_",i)]] == "hyperbolic"){
        
      }
    }
  }
  
  dat$result = rowSums(dat %>% st_drop_geometry() %>% select(ends_with("_binned_with_mod")))
  
  dat = dat %>% select(ends_with("_binned"), result)
  
  if(input$bin_results == "Yes"){
    dat = dat %>% mutate(result = as.numeric(cut(result,3)))
  }
  
  return(dat)
})

output$model_result = renderDataTable(UserDatSummed() %>% st_drop_geometry())