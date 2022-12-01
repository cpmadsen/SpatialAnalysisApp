source("App_UI_components.R")

library(bs4Dash)
library(fresh)

# fresh::bs4dash_layout(
#   
# )

ui <- bs4Dash::dashboardPage(
  title = 'Woop',
  header = my_header,
  sidebar = my_sidebar,
  body = my_mainpanel,
  dark = NULL
  #),
  # width = '300px'
)