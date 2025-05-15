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
library(leaflet)
library(leaflet.extras)

fapar_qj <- 0.6
con <- dbConnect(RMySQL::MySQL(), 
                 dbname = "preles",
                 host = "47.108.94.53",
                 user = "navicat_admin",
                 password = "1QAZ2wsx`",
                 port = 3306)

ui <- fluidPage(
  titlePanel("GPP, ET, SW 热力图生成"),
  sidebarLayout(
    sidebarPanel(
      numericInput("lat1_4", "纬度1", value = 30, min = -90, max = 90),
      numericInput("lon1_4", "经度1", value = 110, min = -180, max = 180),
      numericInput("lat2_4", "纬度2", value = 35, min = -90, max = 90),
      numericInput("lon2_4", "经度2", value = 115, min = -180, max = 180),
      dateInput("date_4", "选择日期", value = as.Date("2019-01-01")),
      actionButton("forecast_preles_4", "预测")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("GPP 热力图地图", leafletOutput("gpp_leaflet_map", height = "600px")),
        tabPanel("ET 热力图", plotlyOutput("et_heatmap_3")),
        tabPanel("SW 热力图", plotlyOutput("sw_heatmap_3")))
      )
    )
  )

server <- function(input, output, session) {
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
    print(df_merged)
    heatmap_data(df_merged)
  })
  output$gpp_heatmap_3 <- renderLeaflet({
    df <- heatmap_data()
    req(df)
    
    leaflet(df) %>%
      addProviderTiles(providers$OpenStreetMap) %>%  # 或者 providers$CartoDB.Positron
      addHeatmap(
        lng = ~lon,
        lat = ~lat,
        intensity = ~GPP,
        blur = 20,
        max = max(df$GPP, na.rm = TRUE),
        radius = 15
      )
  })
  output$et_heatmap_3 <- renderPlotly({
    df <- heatmap_data()
    req(df)  # 如果 df 为空，不执行下面的代码
    
    plot_ly(
      data = df,
      x = ~lon,
      y = ~lat,
      z = ~ET,
      type = "heatmap",
      colorscale = "YlGnBu",
      reversescale = FALSE
    ) %>% layout(
      title = "ET 热力图",
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

shinyApp(ui, server)
