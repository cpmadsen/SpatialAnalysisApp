# This script allows users to define the kind of model they would like to construct.

# ---------------------------------------- #
#          Model Specification             #
# ---------------------------------------- #
#----------------------------------------------------------------------------

#Render the model selection UI elements:
  #1. Drop-down offering linear model, and three stressor function options: sigmoidal, linear, and hyperbolic.
  #2. Two text entry boxes for model parameters 1 and 2 (detailed below)
  #3. Little figures to show shape of response curve(s)

output$model_selection_panel <- renderUI({

  if(is.null(input$variable_1))return(NULL)

  # Linear model #
  
  if(input$model_selection_type == "lineareq" | input$model_selection_type == "sigmoidal" | input$model_selection_type == "hyperbolic"){

    model_selection_panel_taglist = tagList()

    for(i in 1:input$number_vars){

      #variable name.
      variable_name = input[[paste0("variable_", i)]]

      tags_to_add <<- tagList(
        h3(str_to_title(variable_name)),
        fluidRow(
          textInput(
            inputId = paste0("var_coef_",i),
            value = 1,
            label = paste0("Coefficient for variable ",i)
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

  # I should perhaps add functionality for sigmoidal or hyperbolic stressor functions, in addition to linear equation above.
  # if(input$model_selection_type == "sigmoidal"){}
  # 
  # if(input$model_selection_type == "hyperbolic"){}
})

# Make math form of model. This is just to make a UI for the user.
ModelDisplay = reactive({
  if(is.null(input$variable_1))return(NULL)

  vars = c()

  for(i in 1:input$number_vars){

    varname = input[[paste0("variable_", i)]]

    mod = as.numeric(input[[paste0("var_coef_", i)]])

    vars = c(vars, paste0(mod,"(",varname,")"))
  }

  model = paste0("Output = ",paste0(vars, collapse = " + "))

  return(model)
})

#output$modeltext_verbatim = renderText(paste0(Output = ModelDisplay()))

output$modeltext = renderUI({
  withMathJax(helpText(paste0("Model: ","$$", ModelDisplay(),"$$")))
  #paste0("$$", ModelDisplay(),"$$")
})

#Sum across columns to calculate result column.
UserDatSummed = reactive({
  if(is.null(input$slider_1)){return(NULL)}

  dat = UserDatBinned()

  #Apply the coefficients entered by the user to each variable.

  for(i in 1:input$number_vars){

    variables = paste0(SelectedColumns(),"_binned")

    if(input$model_selection_type == "lineareq"){
      dat = dat %>%
        mutate(mod = input[[paste0("var_coef_", i)]]) %>%
        mutate(mod = as.numeric(mod)) %>%
        mutate(!!sym(paste0(variables[i],"_with_mod")) := (mod * !!sym(variables[i])))
    }
    if(input$model_selection_type == "sigmoidal"){
      dat = dat %>%
        mutate(mod = input[[paste0("var_coef_", i)]]) %>%
        mutate(mod = as.numeric(mod)) %>%
        mutate(!!sym(paste0(variables[i],"_with_mod")) := (mod * !!sym(variables[i])))
    }
    if(input$model_selection_type == "hyperbolic"){
      dat = dat %>%
        mutate(mod = input[[paste0("var_coef_", i)]]) %>%
        mutate(mod = as.numeric(mod)) %>%
        mutate(!!sym(paste0(variables[i],"_with_mod")) := (mod * !!sym(variables[i])))
    }
  }

  dat$result = rowSums(dat %>% st_drop_geometry() %>% select(ends_with("_binned_with_mod")))

  dat = dat %>% select(ends_with("_binned"), result)

  if(input$bin_results == "Yes"){
    dat = dat %>% mutate(result = as.numeric(cut(result,3)))
  }

  return(dat)
})

output$model_result = renderDataTable(UserDatSummed())