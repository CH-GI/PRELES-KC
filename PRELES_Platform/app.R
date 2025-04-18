library(shiny)
library(Rprebasso)
library(jsonlite)
library(httr)
library(markdown)

DEEPSEEK_API_URL <- "https://api.deepseek.com/v1/chat/completions"
DEEPSEEK_API_KEY <- "sk-744674fa55f14a959f9ab3e3f97edbbd" # 建议改用环境变量

# 修改后的API调用函数
call_deepseek_api <- function(GPP, ET, SW) {
  request_body <- list(
    model = "deepseek-chat",
    messages = list(
      list(role = "system", content = "你是一个经验丰富、专业的森林生态学家"),
      list(role = "user", content = paste0("用中文分析森林生态系统数据：GPP=", GPP, ", ET=", ET, ", SW=", SW, "。要求：1) 分点说明 2) 指出潜在问题 3) 给出管理建议"))
    ),
    temperature = 0.3  # 降低随机性使回答更专业
  )
  
  response <- POST(
    url = DEEPSEEK_API_URL,
    add_headers(
      "Authorization" = paste("Bearer", DEEPSEEK_API_KEY),
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
}

ui <- navbarPage(
  title = "森林生态系统碳平衡计量平台",
  tabPanel("PRELES分析模块",
           sidebarLayout(
             sidebarPanel(
               width = 2,
               numericInput('PAR', '光合有效辐射(PAR)', value = 20),
               numericInput('TAir', '日平均气温(℃)', value =  18),
               numericInput('VPD', '蒸汽压差(kPa)', value = 1.5),
               numericInput('Precip', '降水量(mm)', value = 3),
               numericInput('CO2', 'CO2浓度(ppm)', value = 280),
               numericInput('fAPAR', 'fAPAR', value = 1),
               selectInput('control','控制值',choices = c('0' = 0, '1' = 1)),
               actionButton('analyze_btn', "预测", class = "btn-primary"),
               actionButton('analyze_ai', "分析", class = "btn-primary")
             ),
             mainPanel(
               h4("基础指标输出"),
               verbatimTextOutput("preles_results"),
               hr(),
               h4("DeepSeek分析"),
               uiOutput("deepseek_analysis")  # 修改为uiOutput
             )
           )
  )
)
server <- function(input, output) {
  preles_data <- eventReactive(input$analyze_btn, {
    PRELES(
      PAR = input$PAR,
      TAir = input$TAir,
      VPD = input$VPD,
      Precip = input$Precip,
      CO2 = input$CO2,
      fAPAR = input$fAPAR,
      control = input$control
    )
  })
  
  # 分析结果获取
  deepseek_result <- eventReactive(input$analyze_ai, {
    data <- preles_data()
    call_deepseek_api(GPP = data$GPP, ET = data$ET, SW = data$SW)
  })
  
  output$preles_results <- renderPrint({
    data <- preles_data()
    cat(sprintf("GPP: %.2f μmol CO₂/m²/s\nET: %.2f mm/month\nSW: %.2f mm",
                data$GPP, data$ET, data$SW))
  })
  
  # 优化显示格式
  output$deepseek_analysis <- renderUI({
    req(deepseek_result())
    htmlContent <- markdownToHTML(
      text = deepseek_result(),
      fragment.only = TRUE
    )
    tags$div(
      style = "
      background: #f8f9fa;
      padding: 15px;
      border-left: 4px solid #28a745;  # 这是绿色边框
      margin: 10px 0;
      border-radius: 0 5px 5px 0;  # 只圆化右侧
      font-family: 'Helvetica Neue', Arial, sans-serif;
      line-height: 1.6;
      box-shadow: 0 1px 3px rgba(0,0,0,0.1);  # 添加轻微阴影增强层次感
    ",
      HTML(htmlContent)
    )
  })
}
shinyApp(ui, server)