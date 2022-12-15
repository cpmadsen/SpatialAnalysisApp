#Render the model selection UI elements:
#1. Drop-down offering three options: sigmoidal, linear, and hyperbolic.
#2. Two text entry boxes for model parameters 1 and 2 (detailed below)
#3. Little figures to show shape of response curve(s)

output$model_selection_panel <- renderUI({
  
  if(is.null(input$selected_cols[1]))return(NULL)
  
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

# Conditional Input : Bin results?
output$optional_results_binning = renderUI({
  if(!input$bin_results_question){NULL}
  
  if(input$bin_results_question){
    numericInput(inputId = 'number_bins',
               label = "Number of Bins",
               value = 3,
               min = 2,
               max = 10)
  }
})

#Sum across columns to calculate result column.
UserDatSummed = reactive({
  if(is.null(input$slider_1)){return(NULL)}
  
  dat = UserDatBinned()
  
  #Apply the coefficients entered by the user to each variable.
  
  for(i in 1:length(input$selected_cols)){
    
    variables = paste0(SelectedColumns(),"_binned")
    
    dat = dat %>% 
      mutate(mod = input[[paste0("var_coef_", i)]]) %>% 
      mutate(mod = as.numeric(mod)) %>% 
      mutate(!!sym(paste0(variables[i],"_with_mod")) := (mod * !!sym(variables[i])))
  }
  
  dat$result = rowSums(dat %>% st_drop_geometry() %>% select(ends_with("_binned_with_mod")))
  
  dat = dat %>% select(ends_with("_binned"), result)
  
  if(input$bin_results_question){
    dat = dat %>% mutate(result = as.numeric(cut(result,input$number_bins)))
  }
  
  return(dat)
})

output$model_result = renderDataTable(UserDatSummed() %>% st_drop_geometry())