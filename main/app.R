library(shiny)
library(Rprebasso)
library(jsonlite)
library(httr)
library(markdown)
library(shinythemes)
library(readxl)
library(ggplot2)
library(plotly)
library(DT)

#变量编写规则：变量类型内容+页面序号

Deepseek_api_url <- "https://api.deepseek.com/v1/chat/completions" # api_url
Deepseek_api_key <- "sk-744674fa55f14a959f9ab3e3f97edbbd" # api_key
# deepseekAPI调用函数（页面1）
Call_deepseek_api_1 <- function(GPP, ET, SW) {
  #构造请求头/请求体
  request_body <- list(
    model = "deepseek-chat",
    messages = list(
      list(role = "system", content = "你是一个经验丰富、专业的森林生态学家"),
      list(role = "user", content = paste0("用中文分析森林生态系统数据：GPP=", GPP, ", ET=", ET, ", SW=", SW, "。
                                          要求：
                                           1) 分点说明 
                                           2) 指出潜在问题 
                                           3) 给出管理建议
                                           "))
    ),
    temperature = 0.3  # 降低随机性使回答更专业
  )
  
  response <- POST(
    url = Deepseek_api_url,
    add_headers(
      "Authorization" = paste("Bearer", Deepseek_api_key),
      "Content-Type" = "application/json"
    ),
    body = toJSON(request_body, auto_unbox = TRUE),
    encode = "json"
  )
  
  if(status_code(response) == 200) {
    response_data <- content(response, "parsed")
    # 提取核心内容并移除多余的转义符
    if(!is.null(response_data$choices[[1]]$message$content)) {
      return(gsub("\\\\n", "\n", response_data$choices[[1]]$message$content))
    }
  }
  return("API请求失败，请检查网络或密钥")
}# deepseekAPI调用函数（页面1）

# deepseekAPI调用函数（页面2）
Call_deepseek_api_2 <- function(gpp_series, et_series, sw_series, date_series = NULL) {
  # 准备数据摘要
  data_summary <- list(
    GPP_summary = summary(gpp_series),
    ET_summary = summary(et_series),
    SW_summary = summary(sw_series)
  )
  
  # 准备时间信息（如果有）
  time_info <- if(!is.null(date_series)) {
    paste("\n时间范围:", format(range(date_series), "%Y-%m-%d"), collapse = " 至 ")
  } else {
    paste("\n数据点数:", length(gpp_series))
  }
  
  # 构造请求内容
  data_str <- paste(
    "以下是森林生态系统时间序列数据:",
    time_info,
    "\n\nGPP(总初级生产力)统计摘要:",
    paste(names(data_summary$GPP_summary), round(data_summary$GPP_summary, 2), collapse = ", "),
    "\nET(蒸散发)统计摘要:",
    paste(names(data_summary$ET_summary), round(data_summary$ET_summary, 2), collapse = ", "),
    "\nSW(土壤水分)统计摘要:",
    paste(names(data_summary$SW_summary), round(data_summary$SW_summary, 2), collapse = ", "),
    sep = ""
  )
  
  # 构造提示词
  prompt <- paste(
    "你是一位专业、经验丰富的森林生态学家，请分析以下森林生态系统时间序列数据。要求:",
    "1. 对生态系统健康状况进行全面评估",
    "2. 分析碳吸收(GPP)、水分利用(ET)和土壤湿度(SW)的相互关系",
    "3. 识别季节性模式和异常值",
    "4. 指出潜在生态问题",
    "5. 提供具体管理建议",
    "6. 使用专业术语但保持解释清晰",
    "\n请分点详细回答，并提供数据支持的关键发现。",
    data_str,
    sep = "\n"
  )
  
  # 构造请求体
  request_body <- list(
    model = "deepseek-chat",
    messages = list(
      list(role = "system", content = "你是一位经验丰富的森林生态学家，擅长分析生态系统时间序列数据"),
      list(role = "user", content = prompt)
    ),
    temperature = 0.3,
    max_tokens = 2000
  )
  
  # 发送请求
  response <- POST(
    url = Deepseek_api_url,
    add_headers(
      "Authorization" = paste("Bearer", Deepseek_api_key),
      "Content-Type" = "application/json"
    ),
    body = toJSON(request_body, auto_unbox = TRUE),
    encode = "json"
  )
  
  # 处理响应
  if(status_code(response) == 200) {
    response_data <- content(response, "parsed")
    if(!is.null(response_data$choices[[1]]$message$content)) {
      return(gsub("\\\\n", "\n", response_data$choices[[1]]$message$content))
    }
  }
  return("API请求失败，请检查网络或密钥")
}# deepseekAPI调用函数（页面2）

ui <- navbarPage(
  title = "森林生态系统碳平衡计量平台",
  theme = shinytheme("flatly"),
  tabPanel("分析模块",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               radioButtons("preles_prebas_1", "模块", choices = c("Preles", "Prebas")),#模块选择（页面1）
               conditionalPanel(
                 condition = "input.preles_prebas_1 == 'Preles'",
                 numericInput('PAR_1', '光合有效辐射(PAR)', value = 20),
                 numericInput('TAir_1', '日平均气温(℃)', value =  18),
                 numericInput('VPD_1', '日蒸汽压差(kPa)', value = 1.5),
                 numericInput('Precip_1', '降水量(mm)', value = 3),
                 numericInput('CO2_1', 'CO₂浓度(ppm)', value = 280),
                 numericInput('fAPAR_1', '冠层吸收光合有效辐射比例', value = 1),
                 selectInput('control_1','模型',choices = c('0' = 0, '1' = 1)),
                 actionButton('forecast_preles_1', "预测", class = "btn-primary"),
                 actionButton('analyze_deepseek_1', "分析", class = "btn-primary")),
               conditionalPanel(
                 condition = "input.preles_prebas_1 == 'Prebaso'",)),
             mainPanel(
               
               conditionalPanel(
                 condition = "input.preles_prebas_1 == 'Preles'",
                 h4("基础指标输出"),
                 verbatimTextOutput("preles_forecast_results_1"),
                 hr(),
                 h4("DeepSeek分析"),
                 uiOutput("deepseek_analysis_results_1")))
           )
  ),
  tabPanel(
    "I/O分析",
    sidebarLayout(
      sidebarPanel(
        h4("数据上传"),
        width = 3,
        fileInput("file_2", "选择 CSV 或 Excel 文件",
                  accept = c(".csv", ".xls", ".xlsx")),
        
        checkboxInput("header_2", "文件包含表头", TRUE),
        radioButtons("sep_2", "分隔符",
                     choices = c(逗号 = ",", 分号 = ";", 制表符 = "\t"),
                     selected = ","),
        
        # 动态参数输入UI
        uiOutput("parameter_inputs_2"),
        
        actionButton("forecast_preles_2", "预测", class = "btn-primary"),
        actionButton("analyze_deepseek_2", "分析", class = "btn-success"),
        hr(),
        h4("结果下载"),
        radioButtons("format_2", "下载格式",
                     choices = c("CSV" = "csv", "Excel" = "xlsx"),
                     selected = "csv"),
        downloadButton("download_2", "下载结果")
        
        
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("数据预览", DTOutput("preview_2")),
          tabPanel("GPP 结果", 
                   plotlyOutput("gpp_plot_2"),
                   DTOutput("gpp_table_2")),
          tabPanel("ET 结果", 
                   plotlyOutput("et_plot_2"),
                   DTOutput("et_table_2")),
          tabPanel("SW 结果", 
                   plotlyOutput("sw_plot_2"),
                   DTOutput("sw_table_2")),
          tabPanel("DeepSeek分析报告", 
                   uiOutput("analysis_report_2"),
                   hr(),
                   downloadButton("download_report_2", "下载分析报告"))
        )
      )
    )
  )
)

server <- function(input, output) {
  #Preles函数计算（页面1）
  preles_data_1 <- eventReactive(input$forecast_preles_1, {
    PRELES(
      PAR = input$PAR_1,
      TAir = input$TAir_1,
      VPD = input$VPD_1,
      Precip = input$Precip_1,
      CO2 = input$CO2_1,
      fAPAR = input$fAPAR_1,
      control = input$control_1)})#Preles函数计算获得结果（页面1）
  
  #输出preles预测结果（页面1）
  output$preles_forecast_results_1 <- renderPrint({
    data_1 <- preles_data_1()
    cat(sprintf("GPP: %.2f gC/m²/d\nET: %.2f mm/d\nSW: %.2f mm",
                data_1$GPP, data_1$ET, data_1$SW))})#输出preles预测结果（页面1）
  
  deepseek_result_1 <- reactiveVal("分析报告将显示在这里...") # 预先定义deepseek报告显示（页面1）
  
  # 调用deepseekapi函数返回原始分析文本（页面1）
  observeEvent(input$analyze_deepseek_1, {
    data_1 <- preles_data_1()
    tryCatch({
      result_1 <- Call_deepseek_api_1(GPP = data_1$GPP, ET = data_1$ET, SW = data_1$SW)
      deepseek_result_1(result_1)
    }, error = function(e) {
      deepseek_result_1(paste("分析出错:", e$message))})})# 调用deepseekapi函数返回原始分析文本（页面1）
  
  #处理markdown文本（页面1）
  output$deepseek_analysis_results_1 <- renderUI({ 
    req(deepseek_result_1())
    htmlContent <- markdownToHTML(
      text = deepseek_result_1(),
      fragment.only = TRUE
    )
    tags$div(
      style = "
      background: #f8f9fa;
      padding: 15px;
      border-left: 4px solid #28a745;  # 绿色边框
      margin: 10px 0;
      border-radius: 0 5px 5px 0;  # 只圆化右侧
      font-family: 'Helvetica Neue', Arial, sans-serif;
      line-height: 1.6;
      box-shadow: 0 1px 3px rgba(0,0,0,0.1);  # 添加轻微阴影增强层次感
    ",
      HTML(htmlContent))}) #处理markdown文本（页面1）
  
  #=================================================================================================
  
  # 读取上传的数据（页面2）
  data_2 <- reactive({
    req(input$file_2)
    
    ext <- tools::file_ext(input$file_2$name)
    
    if (ext == "csv") {
      read.csv(input$file_2$datapath,
               header_2 = input$header_2,
               sep_2 = input$sep_2)
    } else if (ext %in% c("xls", "xlsx")) {
      read_excel(input$file_2$datapath, col_names = input$header_2)}})# 读取上传的数据（页面2）
  
  # 检测缺失的参数（页面2）
  missing_params_2 <- reactive({
    req(data_2())
    required_cols_2 <- c("PAR", "TAir", "VPD", "Precip", "CO2", "fAPAR")
    setdiff(required_cols_2, colnames(data_2()))
  })# 检测缺失的参数（页面2）
  
  # 动态生成参数输入UI（页面2）
  output$parameter_inputs <- renderUI({
    req(missing_params_2())
    missing <- missing_params_2()
    
    tagList(
      h4("缺少参数,请手动输入"),
      if ("PAR" %in% missing) numericInput("PAR", "PAR(光合有效辐射)", value = 0.5),
      if ("TAir" %in% missing) numericInput("TAir", "TAir(平均气温)", value = 15),
      if ("VPD" %in% missing) numericInput("VPD", "VPD(蒸气压差)", value = 0.7),
      if ("Precip" %in% missing) numericInput("Precip", "Precip(降水量)", value = 0),
      if ("CO2" %in% missing) numericInput("CO2", "CO2浓度", value = 380),
      if ("fAPAR" %in% missing) numericInput("fAPAR", "fAPAR(冠层吸收的光合有效辐射比例)", value = 0.8))})# 动态生成参数输入UI（页面2）
  
  # 数据预览（页面2）
  output$preview_2 <- renderDT({
    req(data_2())
    datatable(data_2(), options = list(scrollX = TRUE))})# 数据预览（页面2
  
  # 获取所有参数值，来自文件或输入（页面2）
  all_params_2 <- reactive({
    req(data_2())
    df <- data_2()
    missing <- missing_params_2()
    
    # 默认值
    params <- list(
      PAR = 20,
      TAir = 18,
      VPD = 1.5,
      Precip = 3,
      CO2 = 280,
      fAPAR = 1)
    
    # 从文件中获取已有参数
    for (param in setdiff(names(params), missing)) {
      params[[param]] <- df[[param]]
    }
    
    # 从输入中获取缺失参数
    for (param in missing) {
      if (!is.null(input[[param]])) {
        params[[param]] <- input[[param]]
      }
    }
    params})# 获取所有参数值，来自文件或输入（页面2）
  
  # 创建包含所有参数的完整数据框用于下载（页面2）
  full_data_2 <- reactive({
    req(data_2(), all_params_2())
    df <- data_2()
    params <- all_params_2()
    missing <- missing_params_2()
    
    # 添加缺失的参数列
    for (param in missing) {
      df[[param]] <- params[[param]]
    }
    
    df
  })# 创建包含所有参数的完整数据框用于下载（页面2）
  
  # PRELES 计算结果（页面2）
  results_2 <- eventReactive(input$forecast_preles_2, {
    req(full_data_2())
    df <- full_data_2()
    
    with(df, PRELES(PAR = PAR, TAir = TAir, VPD = VPD, Precip = Precip, 
                    CO2 = CO2, fAPAR = fAPAR))
  })# PRELES 计算结果（页面2）
  
  # 创建包含原始数据、所有参数和结果的数据框（页面2）
  result_df_2 <- reactive({
    req(full_data_2(), results_2())
    res <- cbind(full_data_2(), results_2())
    if (!is.data.frame(res)) {
      res <- as.data_2.frame(res)
    }
    res
  })# 创建包含原始数据、所有参数和结果的数据框（页面2）
  
  # GPP 图表和表格（页面2）
  output$gpp_plot_2 <- renderPlotly({
    req(result_df_2())
    df <- result_df_2()
    
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
  output$gpp_table_2 <- renderDT({
    req(result_df_2())
    df <- result_df_2()
    cols_to_keep <- if ("date" %in% colnames(df)) c("date", "GPP") else "GPP"
    datatable(df[, cols_to_keep, drop = FALSE], 
              options = list(scrollX = TRUE))
  })# GPP 图表和表格（页面2）
  
  # ET 图表和表格（页面2）
  output$et_plot_2 <- renderPlotly({
    req(result_df_2())
    df <- result_df_2()
    
    if ("date" %in% colnames(df)) {
      p <- ggplot(df, aes(x = date, y = ET)) +
        geom_line(color = "black") +
        labs(title = "蒸散量 (ET)", x = "日期", y = "ET") +
        theme_minimal()
    } else {
      p <- ggplot(df, aes(x = seq_along(ET), y = ET)) +
        geom_line(color = "black") +
        labs(title = "蒸散量 (ET)", x = "时间步长", y = "ET") +
        theme_minimal()
    }
    
    ggplotly(p)
  })
  output$et_table_2 <- renderDT({
    req(result_df_2())
    df <- result_df_2()
    cols_to_keep <- if ("date" %in% colnames(df)) c("date", "ET") else "ET"
    datatable(df[, cols_to_keep, drop = FALSE], 
              options = list(scrollX = TRUE))
  })# ET 图表和表格（页面2）
  
  # SW 图表和表格（页面2）
  output$sw_plot_2 <- renderPlotly({
    req(result_df_2())
    df <- result_df_2()
    
    if ("date" %in% colnames(df)) {
      p <- ggplot(df, aes(x = date, y = SW)) +
        geom_line(color = "blue") +
        labs(title = "土壤水含量 (SW)", x = "日期", y = "SW") +
        theme_minimal()
    } else {
      p <- ggplot(df, aes(x = seq_along(SW), y = SW)) +
        geom_line(color = "blue") +
        labs(title = "土壤水量 (SW)", x = "时间步长", y = "SW") +
        theme_minimal()
    }
    
    ggplotly(p)
  })
  output$sw_table_2 <- renderDT({
    req(result_df_2())
    df <- result_df_2()
    cols_to_keep <- if ("date" %in% colnames(df)) c("date", "SW") else "SW"
    datatable(df[, cols_to_keep, drop = FALSE], 
              options = list(scrollX = TRUE))
  })# SW 图表和表格（页面2）
  
  # 文件下载处理（页面2）
  output$download_2 <- downloadHandler(
    filename = function() {
      paste("preles_results-", Sys.Date(), ".", input$format_2, sep_2 = "")
    },
    content = function(file) {
      if (input$format_2 == "csv") {
        write.csv(result_df_2(), file, row.names = FALSE)
      } else {
        writexl::write_xlsx(result_df_2(), file)
      }
    })# 文件下载处理（页面2）
  
  analysis_result_2 <- reactiveVal("分析报告将显示在这里...")
  
  observeEvent(input$analyze_deepseek_2, {
    req(result_df_2())
    
    # 准备时间序列数据
    df <- result_df_2()
    has_date <- "date" %in% colnames(df)
    
    tryCatch({
      result <- Call_deepseek_api_2(
        gpp_series = df$GPP,
        et_series = df$ET,
        sw_series = df$SW,
        date_series = if(has_date) df$date else NULL
      )
      
      analysis_result_2(result)
    }, error = function(e) {
      analysis_result_2(paste("分析出错:", e$message))
    })
  })
  
  output$analysis_report_2 <- renderUI({
    req(analysis_result_2())
    htmlContent <- markdownToHTML(
      text = analysis_result_2(),
      fragment.only = TRUE
    )
    tags$div(
      style = "
      background: #f8f9fa;
      padding: 15px;
      border-left: 4px solid #28a745;  # 绿色边框
      margin: 10px 0;
      border-radius: 0 5px 5px 0;  # 只圆化右侧
      font-family: 'Helvetica Neue', Arial, sans-serif;
      line-height: 1.6;
      box-shadow: 0 1px 3px rgba(0,0,0,0.1);  # 添加轻微阴影增强层次感
    ",
      HTML(htmlContent)
    )
  })
  
  # 下载分析报告（页面2）
  output$download_report_2 <- downloadHandler(
    filename = function() {
      paste("forest_analysis_report-", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      writeLines(analysis_result_2(), file)
    })# 下载分析报告（页面2）
}

shinyApp(ui = ui, server = server)#运行程序