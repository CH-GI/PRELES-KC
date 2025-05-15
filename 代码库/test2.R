library(shiny)
library(Rprebasso)
library(jsonlite)
library(httr)
library(markdown)
library(reticulate)
use_condaenv('preles',required = TRUE)


cds <- import("cdsap")

ui <- navbarPage(
  title = 'cs',
  tabPanel(
    sidebarLayout(
      sidebarPanel(
        actionButton("fetch", "Fetch Data")
      ),
      mainPanel(
        textOutput("data")
      )
    )
  )
)
server <- function(input, output, session) {
  # 当用户点击按钮时，调用 Python 函数
  observeEvent(input$fetch, {
    # 调用 Python 的 fetch_data 函数
    data <- cds$fetch_data()
    
    # 将数据输出到 UI
    output$data <- renderText({
      paste("Data fetched:", data)
    })
  })
}

# 运行 Shiny 应用
shinyApp(ui, server)