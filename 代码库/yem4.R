library(shiny)
library(RMySQL)
library(ncdf4)
library(raster)
library(Rprebasso)
library(ggplot2)
library(reshape2)

# 自定义函数：读取一个NC文件内给定范围的变量平均值（按空间分辨率聚合）
read_nc_avg <- function(ncfile, varname, lat1, lon1, lat2, lon2, resolution_km = 10) {
  nc <- nc_open(ncfile)
  on.exit(nc_close(nc))
  
  lat <- ncvar_get(nc, "lat")
  lon <- ncvar_get(nc, "lon")
  
  # 纬度从小到大，确保范围正确
  lat_range <- range(c(lat1, lat2))
  lon_range <- range(c(lon1, lon2))
  
  lat_idx <- which(lat >= lat_range[1] & lat <= lat_range[2])
  lon_idx <- which(lon >= lon_range[1] & lon <= lon_range[2])
  
  var <- ncvar_get(nc, varname)
  subvar <- var[lon_idx, lat_idx]
  
  # 计算聚合窗口：每 resolution_km 进行一次聚合
  deg_per_km <- 1 / 111 # 粗略换算
  window <- max(1, round(resolution_km * deg_per_km / mean(diff(lat)))) # 保证大于1
  
  r <- raster(subvar)
  extent(r) <- extent(range(lon[lon_idx]), range(lat[lat_idx]))
  agg_r <- aggregate(r, fact = window, fun = mean, na.rm = TRUE)
  mat <- as.matrix(flip(agg_r, direction = "y"))
  return(mat)
}

# 假设数据库有表名为 weather_paths，字段为: date, tair_path, vpd_path, ...
get_nc_paths <- function(date_selected) {
  con <- dbConnect(MySQL(), user="root", password="123456", dbname="weatherdb", host="localhost")
  on.exit(dbDisconnect(con))
  
  query <- sprintf("SELECT * FROM weather_paths WHERE date = '%s'", date_selected)
  res <- dbGetQuery(con, query)
  return(res[1, ])
}

# UI
ui <- fluidPage(
  titlePanel("预估森林GPP、ET、SW（支持空间分辨率调节）"),
  sidebarLayout(
    sidebarPanel(
      dateInput("date", "选择日期:", value = "2021-01-01"),
      numericInput("lat1", "纬度1（对角点A）", value = 30),
      numericInput("lon1", "经度1（对角点A）", value = 110),
      numericInput("lat2", "纬度2（对角点B）", value = 32),
      numericInput("lon2", "经度2（对角点B）", value = 112),
      sliderInput("resolution", "空间分辨率（km）", min = 1, max = 100, value = 10),
      actionButton("run", "运行模型")
    ),
    mainPanel(
      plotOutput("heatmap_gpp"),
      plotOutput("heatmap_et"),
      plotOutput("heatmap_sw")
    )
  )
)

# Server
server <- function(input, output) {
  observeEvent(input$run, {
    paths <- get_nc_paths(as.character(input$date))
    
    vars <- c("Tair", "VPD", "SWdown", "Precip", "CO2", "fapar")
    nc_files <- as.list(paths[paste0(tolower(vars), "_path")])
    names(nc_files) <- vars
    
    met_data <- lapply(names(nc_files), function(var) {
      read_nc_avg(nc_files[[var]], var, input$lat1, input$lon1, input$lat2, input$lon2, resolution_km = input$resolution)
    })
    names(met_data) <- vars
    
    # 对每个像元运行 preles，生成矩阵结果
    gpp_mat <- et_mat <- sw_mat <- matrix(NA, nrow = nrow(met_data[[1]]), ncol = ncol(met_data[[1]]))
    for (i in 1:nrow(gpp_mat)) {
      for (j in 1:ncol(gpp_mat)) {
        input_df <- data.frame(
          Tair = met_data$Tair[i, j],
          VPD = met_data$VPD[i, j],
          SWdown = met_data$SWdown[i, j],
          Precip = met_data$Precip[i, j],
          CO2 = met_data$CO2[i, j],
          fapar = met_data$fapar[i, j],
          year = as.numeric(format(input$date, "%Y")),
          doy = as.numeric(format(input$date, "%j")),
          snow = 0
        )
        if (any(is.na(input_df))) next
        result <- preles(input_df)
        gpp_mat[i, j] <- result$GPP
        et_mat[i, j] <- result$ET
        sw_mat[i, j] <- result$SW
      }
    }
    
    draw_heatmap <- function(mat, title) {
      df <- melt(mat)
      colnames(df) <- c("Y", "X", "value")
      ggplot(df, aes(x = X, y = Y, fill = value)) +
        geom_tile() +
        scale_fill_viridis_c() +
        ggtitle(title) +
        theme_minimal()
    }
    
    output$heatmap_gpp <- renderPlot({ draw_heatmap(gpp_mat, "GPP 热力图") })
    output$heatmap_et  <- renderPlot({ draw_heatmap(et_mat, "ET 热力图") })
    output$heatmap_sw  <- renderPlot({ draw_heatmap(sw_mat, "SW 热力图") })
  })
}

shinyApp(ui = ui, server = server)
