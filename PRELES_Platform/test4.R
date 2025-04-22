library(shiny)
library(shinythemes)
library(readxl)
library(ggplot2)
library(plotly)
library(DT)
library(Rprebasso)

ui <- navbarPage(
  theme = shinytheme("flatly"),
  title="PRELES 模型计算平台",
  tabPanel(
    "I/O分析",
    sidebarLayout(
    sidebarPanel(
      h4("数据上传"),
      fileInput("file", "选择 CSV 或 Excel 文件",
                accept = c(".csv", ".xls", ".xlsx")),
      
      checkboxInput("header", "文件包含表头", TRUE),
      radioButtons("sep", "分隔符",
                   choices = c(逗号 = ",", 分号 = ";", 制表符 = "\t"),
                   selected = ","),
      
      h4("PRELES 参数"),
      numericInput("PAR", "PAR (光合有效辐射)", value = 0.5),
      numericInput("TAir", "TAir (气温)", value = 15),
      numericInput("VPD", "VPD (蒸汽压差)", value = 0.7),
      numericInput("Precip", "Precip (降水量)", value = 0),
      numericInput("CO2", "CO2 浓度", value = 380),
      numericInput("fAPAR", "fAPAR (吸收的光合有效辐射比例)", value = 0.8),
      
      actionButton("calculate", "运行 PRELES 计算", class = "btn-primary"),
      
      hr(),
      h4("结果下载"),
      radioButtons("format", "下载格式",
                   choices = c("CSV" = "csv", "Excel" = "xlsx"),
                   selected = "csv"),
      downloadButton("download", "下载结果")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("数据预览", DTOutput("preview")),
        tabPanel("GPP 结果", 
                 plotlyOutput("gpp_plot"),
                 DTOutput("gpp_table")),
        tabPanel("ET 结果", 
                 plotlyOutput("et_plot"),
                 DTOutput("et_table")),
        tabPanel("SW 结果", 
                 plotlyOutput("sw_plot"),
                 DTOutput("sw_table"))
      )
    )
  ))
)

server <- function(input, output) {
  
  # 读取上传的数据
  data <- reactive({
    req(input$file)
    
    ext <- tools::file_ext(input$file$name)
    
    if (ext == "csv") {
      read.csv(input$file$datapath,
               header = input$header,
               sep = input$sep)
    } else if (ext %in% c("xls", "xlsx")) {
      read_excel(input$file$datapath, col_names = input$header)
    }
  })
  
  # 数据预览
  output$preview <- renderDT({
    datatable(data(), options = list(scrollX = TRUE))
  })
  
  # PRELES 计算结果
  results <- eventReactive(input$calculate, {
    df <- data()
    
    # 确保数据包含所需列
    required_cols <- c("PAR", "TAir", "VPD", "Precip", "CO2", "fAPAR")
    
    # 如果上传文件包含这些列，使用它们；否则使用用户输入的值
    if (all(required_cols %in% colnames(df))) {
      with(df, PRELES(PAR = PAR, TAir = TAir, VPD = VPD, Precip = Precip, 
                      CO2 = CO2, fAPAR = fAPAR))
    } else {
      # 使用用户输入的参数值创建数据框
      n <- ifelse(is.null(df), 1, nrow(df))
      params <- data.frame(
        PAR = rep(input$PAR, n),
        TAir = rep(input$TAir, n),
        VPD = rep(input$VPD, n),
        Precip = rep(input$Precip, n),
        CO2 = rep(input$CO2, n),
        fAPAR = rep(input$fAPAR, n)
      )
      with(params, PRELES(PAR = PAR, TAir = TAir, VPD = VPD, Precip = Precip, 
                          CO2 = CO2, fAPAR = fAPAR))
    }
  })
  
  # 创建包含原始数据和结果的数据框
  result_df <- reactive({
    cbind(data(), results())
  })
  
  # GPP 图表和表格
  output$gpp_plot <- renderPlotly({
    req(results())
    df <- result_df()
    
    if ("date" %in% colnames(df)) {
      p <- ggplot(df, aes(x = date, y = GPP)) +
        geom_line(color = "darkgreen") +
        labs(title = "总初级生产力 (GPP)", x = "日期", y = "GPP") +
        theme_minimal()
    } else {
      p <- ggplot(df, aes(x = seq_along(GPP), y = GPP)) +
        geom_line(color = "darkgreen") +
        labs(title = "总初级生产力 (GPP)", x = "时间步长", y = "GPP") +
        theme_minimal()
    }
    
    ggplotly(p)
  })
  
  output$gpp_table <- renderDT({
    req(results())
    datatable(result_df()[, c(if ("date" %in% colnames(result_df())) "date", "GPP")], 
              options = list(scrollX = TRUE))
  })
  
  # ET 图表和表格
  output$et_plot <- renderPlotly({
    req(results())
    df <- result_df()
    
    if ("date" %in% colnames(df)) {
      p <- ggplot(df, aes(x = date, y = ET)) +
        geom_line(color = "blue") +
        labs(title = "蒸散发 (ET)", x = "日期", y = "ET") +
        theme_minimal()
    } else {
      p <- ggplot(df, aes(x = seq_along(ET), y = ET)) +
        geom_line(color = "blue") +
        labs(title = "蒸散发 (ET)", x = "时间步长", y = "ET") +
        theme_minimal()
    }
    
    ggplotly(p)
  })
  
  output$et_table <- renderDT({
    req(results())
    datatable(result_df()[, c(if ("date" %in% colnames(result_df())) "date", "ET")], 
              options = list(scrollX = TRUE))
  })
  
  # SW 图表和表格
  output$sw_plot <- renderPlotly({
    req(results())
    df <- result_df()
    
    if ("date" %in% colnames(df)) {
      p <- ggplot(df, aes(x = date, y = SW)) +
        geom_line(color = "brown") +
        labs(title = "土壤水分 (SW)", x = "日期", y = "SW") +
        theme_minimal()
    } else {
      p <- ggplot(df, aes(x = seq_along(SW), y = SW)) +
        geom_line(color = "brown") +
        labs(title = "土壤水分 (SW)", x = "时间步长", y = "SW") +
        theme_minimal()
    }
    
    ggplotly(p)
  })
  
  output$sw_table <- renderDT({
    req(results())
    datatable(result_df()[, c(if ("date" %in% colnames(result_df())) "date", "SW")], 
              options = list(scrollX = TRUE))
  })
  
  # 下载处理
  output$download <- downloadHandler(
    filename = function() {
      paste("preles_results-", Sys.Date(), ".", input$format, sep = "")
    },
    content = function(file) {
      if (input$format == "csv") {
        write.csv(result_df(), file, row.names = FALSE)
      } else {
        writexl::write_xlsx(result_df(), file)
      }
    }
  )
}

shinyApp(ui = ui, server = server)