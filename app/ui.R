library(shiny)
#定义界面外观
shinyUI(fluidPage(
  titlePanel("title panel"),
  
  sidebarLayout(position = "left",
                sidebarPanel( "sidebar panel"),
                mainPanel("main panel")
  ) 
))