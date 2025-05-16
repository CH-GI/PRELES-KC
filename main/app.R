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
library(ncdf4)
library(DBI)
library(RMySQL)
library(lubridate)
library(dplyr)
library(tidyr)
library(purrr)     
library(zoo)
library(bs4Dash)

#变量编写规则：变量类型内容+页面序号
Deepseek_api_url <- "https://api.deepseek.com/v1/chat/completions" # api_url
Deepseek_api_key <- "sk-744674fa55f14a959f9ab3e3f97edbbd" # api_key
fapar_qj <- 0.8
con <- dbConnect(RMySQL::MySQL(), 
                 dbname = "preles",
                 host = "47.108.94.53",
                 user = "navicat_admin",
                 password = "1QAZ2wsx`",
                 port = 3306)
get_nearest_point <- function(lon, lat) {
  # 使用ST_Distance_Sphere计算最短距离
  query <- paste("SELECT lon, lat, ST_Distance_Sphere(POINT(lon, lat), POINT(", lon, ", ", lat, ")) AS distance
                  FROM tair2022
                  WHERE lon BETWEEN ", lon - 0.5, " AND ", lon + 0.5,
                 " AND lat BETWEEN ", lat - 0.5, " AND ", lat + 0.5, 
                 " ORDER BY distance LIMIT 1", sep = "")
  
  result <- dbGetQuery(conn, query)
  return(result)
}

# 获取数据并准备输入preles
get_weather_data <- function(lon, lat, start_date, end_date) {
  nearest_point <- get_nearest_point(lon, lat)
  # 根据返回的坐标和日期区间获取其他变量的数据
  # 例如：precip、par等
  data <- dbGetQuery(conn, paste("SELECT * FROM precip2022 WHERE lon = ", nearest_point$lon, " AND lat = ", nearest_point$lat, 
                                 " AND date BETWEEN '", start_date, "' AND '", end_date, "'"))
  return(data)
}

# 输入到preles函数并生成图形
generate_gpp_et_sw <- function(weather_data) {
  # 调用preles函数并计算GPP、ET、SW
  result <- preles(weather_data)
  
  # 使用ggplot2生成图形
  gpp_plot <- ggplot(result, aes(x = date, y = GPP)) + geom_line() + ggtitle("GPP")
  et_plot <- ggplot(result, aes(x = date, y = ET)) + geom_line() + ggtitle("ET")
  sw_plot <- ggplot(result, aes(x = date, y = SW)) + geom_line() + ggtitle("SW")
  
  return(list(gpp_plot, et_plot, sw_plot))
}
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





ui <- bs4DashPage(
  title = "森林生态系统碳平衡计量平台",
  header = bs4DashNavbar(skin = "light"),
  sidebar = bs4DashSidebar(
    skin = "light",
    status = "primary",
    collapsed = TRUE,  # 初始状态为折叠
    bs4SidebarMenu(
      bs4SidebarMenuItem(
        "分析模块",
        tabName = "analysis",
        icon = icon("calculator")
      ),
      bs4SidebarMenuItem(
        "I/O分析",
        tabName = "io",
        icon = icon("file-upload")
      ),
      bs4SidebarMenuItem(
        "数据库值",
        tabName = "database",
        icon = icon("database")
      ),
      bs4SidebarMenuItem(
        "热力图",
        tabName = "heatmap",
        icon = icon("map")
      )
    )
  ),
  body = bs4DashBody(
    bs4TabItems(
      # 分析模块 ---------------------------------------------------------------
      bs4TabItem(
        tabName = "analysis",
        fluidRow(
          column(
            width = 3,
            bs4Card(
              title = "参数设置",
              width = 12,
              radioButtons("preles_prebas_1", "模块", 
                           choices = c("Preles", "Prebas")),
              conditionalPanel(
                condition = "input.preles_prebas_1 == 'Preles'",
                numericInput('PAR_1', '光合有效辐射(PAR)', value = 20),
                numericInput('TAir_1', '日平均气温(℃)', value = 18),
                numericInput('VPD_1', '日蒸汽压差(kPa)', value = 1.5),
                numericInput('Precip_1', '降水量(mm)', value = 3),
                numericInput('CO2_1', 'CO₂浓度(ppm)', value = 280),
                numericInput('fAPAR_1', '冠层吸收光合有效辐射比例', value = 1),
                selectInput('control_1','模型',choices = c('0' = 0, '1' = 1)),
                actionButton('forecast_preles_1', "预测", status = "primary"),
                actionButton('analyze_deepseek_1', "分析", status = "info")
              )
            )
          ),
          column(
            width = 9,
            bs4Card(
              title = "分析结果",
              width = 12,
              conditionalPanel(
                condition = "input.preles_prebas_1 == 'Preles'",
                h4("基础指标输出"),
                verbatimTextOutput("preles_forecast_results_1"),
                hr(),
                h4("DeepSeek分析"),
                uiOutput("deepseek_analysis_results_1")
              )
            )
          )
        )
      ),
      
      # I/O分析 --------------------------------------------------------------
      bs4TabItem(
        tabName = "io",
        fluidRow(
          column(
            width = 3,
            bs4Card(
              title = "数据上传",
              width = 12,
              fileInput("file_2", "选择文件",
                        accept = c(".csv", ".xls", ".xlsx")),
              checkboxInput("header_2", "包含表头", TRUE),
              radioButtons("sep_2", "分隔符",
                           choices = c(逗号 = ",", 分号 = ";", 制表符 = "\t")),
              uiOutput("parameter_inputs_2"),
              actionButton("forecast_preles_2", "预测", status = "primary"),
              actionButton("analyze_deepseek_2", "分析", status = "success")
            ),
            bs4Card(
              title = "结果下载",
              width = 12,
              radioButtons("format_2", "格式",
                           choices = c("CSV" = "csv", "Excel" = "xlsx")),
              downloadButton("download_2", "下载结果", class = "btn-block")
            )
          ),
          column(
            width = 9,
            bs4Card(
              title = "分析结果",
              width = 12,
              maximizable = TRUE,
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
                tabPanel("分析报告", 
                         uiOutput("analysis_report_2"),
                         downloadButton("download_report_2", "下载报告"))
              )
            )
          )
        )
      ),
      
      # 数据库值 --------------------------------------------------------------
      bs4TabItem(
        tabName = "database",
        fluidRow(
          column(
            width = 3,
            bs4Card(
              title = "参数设置",
              width = 12,
              dateRangeInput("date_range_3", "日期范围", 
                             start = "2019-01-01", end = "2022-02-03"),
              numericInput("longitude_3", "经度", value = 120),
              numericInput("latitude_3", "纬度", value = 30),
              actionButton('forecast_preles_3', "预测", status = "primary"),
              actionButton('analyze_deepseek_3', "分析", status = "info")
            )
          ),
          column(
            width = 9,
            bs4Card(
              title = "分析结果",
              width = 12,
              tabsetPanel(
                tabPanel("GPP 结果", 
                         plotlyOutput("gpp_plot_3"),
                         DTOutput("gpp_table_3")),
                tabPanel("ET 结果", 
                         plotlyOutput("et_plot_3"),
                         DTOutput("et_table_3")),
                tabPanel("SW 结果", 
                         plotlyOutput("sw_plot_3"),
                         DTOutput("sw_table_3")),
                tabPanel("分析报告", 
                         uiOutput("analysis_report_3"),
                         downloadButton("download_report_3", "下载报告"))
              )
            )
          )
        )
      ),
      
      # 热力图 ---------------------------------------------------------------
      bs4TabItem(
        tabName = "heatmap",
        fluidRow(
          column(
            width = 3,
            bs4Card(
              title = "空间参数",
              width = 12,
              numericInput("lat1_4", "纬度1", value = 30, 
                           min = -90, max = 90),
              numericInput("lon1_4", "经度1", value = 110,
                           min = -180, max = 180),
              numericInput("lat2_4", "纬度2", value = 35,
                           min = -90, max = 90),
              numericInput("lon2_4", "经度2", value = 115,
                           min = -180, max = 180),
              dateInput("date_4", "选择日期", value = as.Date("2019-01-01")),
              actionButton("forecast_preles_4", "生成热力图", status = "primary")
            )
          ),
          column(
            width = 9,
            bs4Card(
              title = "可视化结果",
              width = 12,
              maximizable = TRUE,
              tabsetPanel(
                tabPanel("GPP 热力图", plotlyOutput("gpp_heatmap_3")),
                tabPanel("ET 热力图", plotlyOutput("et_heatmap_3")),
                tabPanel("SW 热力图", plotlyOutput("sw_heatmap_3"))
              )
            )
          )
        )
      )
    )
  ),
  footer = bs4DashFooter(
    left = HTML('<div style="text-align: center;">
               <a href="https://beian.miit.gov.cn/" target="_blank">
               陕ICP备2025067822号</a></div>')
  )
)



#================================================================================================
server <- function(input,output,session) {
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
  #=================================================================================================
  weather_rv <- reactiveVal(data.frame(date = as.Date(character()), GPP = numeric(), 
                                       ET = numeric(), SW = numeric(), par = numeric(), 
                                       tair = numeric(), vpd = numeric(), precip = numeric(), 
                                       co2 = numeric(), fapar = numeric()))
  observeEvent(input$forecast_preles_3, {
    
    req(input$longitude_3, input$latitude_3, input$date_range_3)
    
    start_date <- input$date_range_3[1]
    end_date <- input$date_range_3[2]
    start_year <- year(start_date)
    end_year <- year(end_date)
    
    lon_input <- input$longitude_3
    lat_input <- input$latitude_3
    
    variables <- c("par", "precip", "vpd", "tair")
    weather <- data.frame(date = seq(start_date, end_date, by = "1 day"))
    
    for (var in variables) {
      all_years_data <- list()
      
      for (yr in start_year:end_year) {
        table_name <- paste0(var, yr)
        
        # 查找最近点
        query_point <- paste0(
          "SELECT lon, lat FROM ", table_name,
          " ORDER BY POW(lon - ", lon_input, ", 2) + POW(lat - ", lat_input, ", 2) LIMIT 1"
        )
        nearest_point <- dbGetQuery(con, query_point)
        
        if (nrow(nearest_point) == 0) next
        
        nearest_lon <- nearest_point$lon[1]
        nearest_lat <- nearest_point$lat[1]
        
        # 获取该点数据
        date_seq <- seq(start_date, end_date, by = "1 day")
        date_fields <- unique(format(date_seq[year(date_seq) == yr], "%m%d"))
        
        if (length(date_fields) == 0) next
        
        sql <- paste0(
          "SELECT * FROM ", table_name, 
          " WHERE lon = ", nearest_lon, 
          " AND lat = ", nearest_lat
        )
        data <- dbGetQuery(con, sql)
        
        # 提取目标字段
        keep_cols <- c("lon", "lat", date_fields)
        data <- data[, intersect(colnames(data), keep_cols), drop = FALSE]
        
        if (length(setdiff(date_fields, colnames(data))) > 0) next
        # 转置为长格式并构造日期
        data_long <- data.frame(
          date = as.Date(paste0(yr, date_fields), format = "%Y%m%d"),
          value = as.numeric(data[1, date_fields])
        )
        
        names(data_long)[2] <- var
        all_years_data[[as.character(yr)]] <- data_long
      }
      
      if (length(all_years_data) > 0) {
        var_df <- bind_rows(all_years_data)
        weather <- left_join(weather, var_df, by = "date")
      }
    }
    
    # 加入 fapar（固定值）
    weather$fapar <- rep(fapar_qj, nrow(weather))  # 可自定义
    
    # 加入 co2
    co2_dates <- format(weather$date, "%Y%m%d")
    co2_query <- paste0(
      "SELECT * FROM co2 WHERE date IN ('",
      paste(co2_dates, collapse = "','"),
      "')"
    )
    co2_data <- dbGetQuery(con, co2_query)
    
    # 转换为 Date 类型
    co2_data$date <- as.Date(as.character(co2_data$date), format = "%Y%m%d")
    colnames(co2_data)[2] <- "co2"  # 如果第二列不是 co2，请修改字段名
    weather <- left_join(weather, co2_data, by = "date")
    # 除了 date 列，其他列逐列插值
    for (col_name in setdiff(names(weather), "date")) {
      weather[[col_name]] <- na.approx(weather[[col_name]], x = weather$date, na.rm = FALSE)
    }
    preles_result <- PRELES(PAR = weather$par,
                            TAir = weather$tair,
                            VPD = weather$vpd,
                            Precip = weather$precip,
                            CO2 = weather$co2,
                            fAPAR = weather$fapar)
    weather <- cbind(weather, as.data.frame(preles_result))
    weather_rv(weather)
  })
  
  output$gpp_plot_3 <- renderPlotly({
    df <- weather_rv()
    req(nrow(df) > 0)               
    req("date" %in% colnames(df))  
    req("GPP" %in% colnames(df))   
    
    p <- ggplot(df, aes(x = date, y = GPP)) +
      geom_line(color = "darkgreen") +
      labs(title = "总初级生产力 (GPP)", x = "日期", y = "GPP") +
      theme_minimal()
    
    ggplotly(p)
  })
  output$gpp_table_3 <- renderDT({
    df <- weather_rv()
    req(nrow(df) > 0)
    req("date" %in% colnames(df))
    req("GPP" %in% colnames(df))
    
    datatable(df[, c("date", "GPP")], options = list(scrollX = TRUE))
  })
  output$et_plot_3 <- renderPlotly({
    df <- weather_rv()
    req(nrow(df) > 0)               
    req("date" %in% colnames(df))  
    req("ET" %in% colnames(df))   
    
    p <- ggplot(df, aes(x = date, y = ET)) +
      geom_line(color = "blue") +
      labs(title = "蒸散量 (ET)", x = "日期", y = "ET") +
      theme_minimal()
    
    ggplotly(p)
  })
  output$et_table_3 <- renderDT({
    df <- weather_rv()
    req(nrow(df) > 0)
    req("date" %in% colnames(df))
    req("ET" %in% colnames(df))
    
    datatable(df[, c("date", "ET")], options = list(scrollX = TRUE))
  })
  output$sw_plot_3 <- renderPlotly({
    df <- weather_rv()
    req(nrow(df) > 0)               
    req("date" %in% colnames(df))  
    req("SW" %in% colnames(df))   
    
    p <- ggplot(df, aes(x = date, y = SW)) +
      geom_line(color = "black") +
      labs(title = "土壤水含量 (SW)", x = "日期", y = "SW") +
      theme_minimal()
    
    ggplotly(p)
  })
  output$sw_table_3 <- renderDT({
    df <- weather_rv()
    req(nrow(df) > 0)
    req("date" %in% colnames(df))
    req("SW" %in% colnames(df))
    
    datatable(df[, c("date", "SW")], options = list(scrollX = TRUE))
  })
  
  analysis_result_3 <- reactiveVal("分析报告将显示在这里...")
  
  observeEvent(input$analyze_deepseek_3, {
    req(weather_rv())
    
    # 准备时间序列数据
    df <- weather_rv()
    has_date <- "date" %in% colnames(df)
    
    tryCatch({
      result <- Call_deepseek_api_2(
        gpp_series = df$GPP,
        et_series = df$ET,
        sw_series = df$SW,
        date_series = if(has_date) df$date else NULL
      )
      
      analysis_result_3(result)
    }, error = function(e) {
      analysis_result_3(paste("分析出错:", e$message))
    })
  })
  
  output$analysis_report_3 <- renderUI({
    req(analysis_result_3())
    htmlContent <- markdownToHTML(
      text = analysis_result_3(),
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
  
  # 下载分析报告（页面3）
  output$download_report_3 <- downloadHandler(
    filename = function() {
      paste("forest_analysis_report-", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      writeLines(analysis_result_3(), file)
    })# 下载分析报告（页面3）
  #====================================================================================================
  df <- NULL
  df_list <- list()
  heatmap_data <- reactiveVal(NULL)
  observeEvent(input$forecast_preles_4,{
    year <- format(input$date_4, "%Y")
    md <- format(input$date_4, "%m%d")
    lon_min <- min(input$lon1_4, input$lon2_4)
    lon_max <- max(input$lon1_4, input$lon2_4)
    lat_min <- min(input$lat1_4, input$lat2_4)
    lat_max <- max(input$lat1_4, input$lat2_4)
    variables <- c("par", "precip", "vpd", "tair")
    for (var in variables) {
      table_name <- paste0(var,  year)
      sql_query <- sprintf("SELECT lon,lat,`%s` FROM %s WHERE lon BETWEEN %f AND %f 
                    AND lat BETWEEN %f AND %f", md, table_name, lon_min, lon_max, lat_min, lat_max)
      df_var <- dbGetQuery(con, sql_query)
      colnames(df_var)[3] <- var
      df_list[[var]] <- df_var
    }
    df_merged <- df_list[[1]]
    
    for (i in 2:length(df_list)) {
      df_merged <- merge(df_merged, df_list[[i]], by = c("lon", "lat"), all = TRUE)
    }
    co2_query <- sprintf("SELECT * FROM co2 WHERE date = '%s'", format(input$date_4, "%Y%m%d"))  
    co2_row <- dbGetQuery(con, co2_query)
    if(nrow(co2_row) == 1){
      co2_value <- as.numeric(co2_row[[2]])  
    } else {
      co2_value <- NA  
    }
    df_merged$co2 <- rep(co2_value, nrow(df_merged))
    df_merged$fapar <- rep(fapar_qj, nrow(df_merged))
    results <- PRELES(PAR = df_merged$par,
                      TAir = df_merged$tair,
                      Precip = df_merged$precip,
                      CO2 = df_merged$co2,
                      VPD = df_merged$vpd,
                      fAPAR = df_merged$fapar)
    df_merged$GPP <- results$GPP
    df_merged$ET  <- results$ET
    df_merged$SW  <- results$SW
    heatmap_data(df_merged)
  })
  output$gpp_heatmap_3 <- renderPlotly({
    df <- heatmap_data()
    req(df)  # 如果 df 为空，不执行下面的代码
    
    plot_ly(
      data = df,
      x = ~lon,
      y = ~lat,
      z = ~GPP,
      type = "heatmap",
      colorscale = "YlGnBu",
      reversescale = FALSE
    ) %>% layout(
      title = "ET 热力图",
      xaxis = list(title = "经度"),
      yaxis = list(title = "纬度")
    )})
  output$et_heatmap_3 <- renderPlotly({
    df <- heatmap_data()
    req(df)  # 如果 df 为空，不执行下面的代码
    
    plot_ly(
      data = df,
      x = ~lon,
      y = ~lat,
      z = ~GPP,
      type = "heatmap",
      colorscale = "YlGnBu",
      reversescale = FALSE
    ) %>% layout(
      title = "GPP 热力图",
      xaxis = list(title = "经度"),
      yaxis = list(title = "纬度")
    )})
  output$sw_heatmap_3 <- renderPlotly({
    df <- heatmap_data()
    req(df)  # 如果 df 为空，不执行下面的代码
    
    plot_ly(
      data = df,
      x = ~lon,
      y = ~lat,
      z = ~SW,
      type = "heatmap",
      colorscale = "YlGnBu",
      reversescale = FALSE
    ) %>% layout(
      title = "SW 热力图",
      xaxis = list(title = "经度"),
      yaxis = list(title = "纬度")
    )
  })
  
}
shinyApp(ui = ui, server = server)#运行程序