library(shiny)
library(DBI)
library(RMySQL)
library(dplyr)
library(tidyr)
library(purrr)     
library(Rprebasso)
library(plotly)
library(lubridate)
library(zoo) 
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
# 连接数据库
con <- dbConnect(RMySQL::MySQL(), 
                        dbname = "preles",
                        host = "47.108.94.53",
                        user = "navicat_admin",
                        password = "1QAZ2wsx`",
                        port = 3306)
# 获取与指定经纬度最近的点
get_nearest_point <- function(lon, lat) {
  query <- sprintf("
    SELECT lon, lat 
    FROM tair2022
    ORDER BY POW(lon - %f, 2) + POW(lat - %f, 2)
    LIMIT 1
  ", lon, lat)
  
  result <- dbGetQuery(conn, query)
  return(result)
}

# 按年份和时间范围提取数据并拼接
get_weather_data <- function(lon, lat, start_date, end_date) {
  nearest_point <- get_nearest_point(lon, lat)
  
  # 获取年份范围
  start_year <- as.numeric(format(start_date, "%Y"))
  end_year <- as.numeric(format(end_date, "%Y"))
  
  variables <- c("par", "precip", "vpd", "tair")
  
  data_list <- list()
  
  for (year in start_year:end_year) {
    for (var in variables) {
      # 构造查询语句
      query <- paste("SELECT * FROM ", var, year, " WHERE lon = ", nearest_point$lon, " AND lat = ", nearest_point$lat,
                     " AND date BETWEEN '", format(start_date, "%m%d"), "' AND '", format(end_date, "%m%d"), "'", sep = "")
      data <- dbGetQuery(conn, query)
      
      # 将数据合并到列表中
      data_list[[paste(var, year, sep = "_")]] <- data
    }
  }
  
  return(data_list)
}

# 输入到preles函数并生成图形
generate_gpp_et_sw <- function(weather_data) {
  # 假设preles函数返回的数据有GPP, ET, SW等值
  result <- preles(weather_data)  # 请确保preles函数适配这些数据
  
  # 使用ggplot2生成图形
  gpp_plot <- ggplot(result, aes(x = date, y = GPP)) + geom_line() + ggtitle("GPP")
  et_plot <- ggplot(result, aes(x = date, y = ET)) + geom_line() + ggtitle("ET")
  sw_plot <- ggplot(result, aes(x = date, y = SW)) + geom_line() + ggtitle("SW")
  
  return(list(gpp_plot, et_plot, sw_plot))
}

# Shiny UI
ui <- fluidPage(
  tabPanel(
    "数据库值",
    sidebarLayout(
      sidebarPanel(
        dateRangeInput("date_range_3", "选择日期范围：", start = "2019-01-01", end = "2022-02-3"),
        numericInput("longitude_3", "经度：", value = 120),
        numericInput("latitude_3", "纬度：", value = 30),
        actionButton('forecast_preles_3', "预测", class = "btn-primary"),
        actionButton('analyze_deepseek_3', "分析", class = "btn-primary")
      ),
      mainPanel(
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
          tabPanel("DeepSeek分析报告", 
                   uiOutput("analysis_report_3"),
                   hr(),
                   downloadButton("download_report_3", "下载分析报告")))
      )
    )
  )
)
# Shiny Server
server <- function(input, output, session) {
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
    weather$fapar <- rep(0.6, nrow(weather))  # 可自定义
    
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
  output$ET_table_3 <- renderDT({
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
}
shinyApp(ui = ui, server = server)
