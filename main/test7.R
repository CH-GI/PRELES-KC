library(shiny)
library(DBI)
library(RMySQL)
library(ncdf4)
library(ggplot2)
library(zoo)
library(Rprebasso)

get_par_at_point <- function(nc_path, lon_input, lat_input, search_radius = 1) {
  nc <- nc_open(nc_path)
  lon <- ncvar_get(nc, "lon")
  lat <- ncvar_get(nc, "lat")
  data <- ncvar_get(nc, "Solar_Radiation_Flux")
  data[data == -9999] <- NA
  
  # 时间
  time_raw <- ncvar_get(nc, "time")
  time_units <- ncatt_get(nc, "time", "units")$value
  date <- as.Date(time_raw, origin = sub("days since ", "", time_units))
  units <- ncatt_get(nc, "Solar_Radiation_Flux", "units")$value
  
  # 找最近网格索引
  lon_index <- which.min(abs(lon - lon_input))
  lat_index <- which.min(abs(lat - lat_input))
  value <- data[lon_index, lat_index]
  
  # 如果不是 NA，直接返回
  if (!is.na(value)) {
    nc_close(nc)
    return(list(
      date = date,
      lon = round(lon[lon_index], 4),
      lat = round(lat[lat_index], 4),
      value = value * 0.0046,
      units = units
    ))
  }
  
  # 否则，在 search_radius 内找最近非 NA 值
  lon_res <- mean(diff(lon))  # 经度分辨率
  lat_res <- mean(diff(lat))  # 纬度分辨率
  lon_range <- round(search_radius / lon_res)
  lat_range <- round(search_radius / lat_res)
  
  nearby_coords <- expand.grid(
    dx = -lon_range:lon_range,
    dy = -lat_range:lat_range
  )
  
  nearby_coords <- nearby_coords[!(nearby_coords$dx == 0 & nearby_coords$dy == 0), ]  # 排除自身
  
  min_dist <- Inf
  nearest_value <- NA
  nearest_lon <- NA
  nearest_lat <- NA
  
  for (i in seq_len(nrow(nearby_coords))) {
    ix <- lon_index + nearby_coords$dx[i]
    iy <- lat_index + nearby_coords$dy[i]
    
    if (ix >= 1 && ix <= length(lon) && iy >= 1 && iy <= length(lat)) {
      v <- data[ix, iy]
      if (!is.na(v)) {
        dist <- sqrt((lon[ix] - lon_input)^2 + (lat[iy] - lat_input)^2)
        if (dist < min_dist && dist <= search_radius) {
          min_dist <- dist
          nearest_value <- v
          nearest_lon <- lon[ix]
          nearest_lat <- lat[iy]
        }
      }
    }
  }
  
  nc_close(nc)
  
  return(list(
    date = date,
    lon = if (!is.na(nearest_value)) round(nearest_lon, 4) else NA,
    lat = if (!is.na(nearest_value)) round(nearest_lat, 4) else NA,
    value = nearest_value * 0.0046,
    units = units,
    distance = if (!is.na(nearest_value)) round(min_dist, 4) else NA
  ))
}

get_co2_at_point <- function(nc_path, lon_input, lat_input, max_distance_km = 25) {
  nc <- nc_open(nc_path)
  lons <- ncvar_get(nc, "longitude")
  lats <- ncvar_get(nc, "latitude")
  co2 <- ncvar_get(nc, "co2")
  
  co2[co2 == -999] <- NA
  df <- data.frame(lon = lons, lat = lats, co2 = co2)
  distances <- distHaversine(cbind(df$lon, df$lat), c(lon_input, lat_input))
  
  df$distance_km <- distances / 1000
  df_valid <- df[!is.na(df$co2) & df$distance_km <= max_distance_km, ]
  
  if (nrow(df_valid) == 0) {
    return(list(
      lon = lon_input,
      lat = lat_input,
      co2_ppm = NA,
      distance_km = NA
    ))
  } else {
    nearest <- df_valid[which.min(df_valid$distance_km), ]
    return(list(
      lon = nearest$lon,
      lat = nearest$lat,
      co2_ppm = nearest$co2,
      distance_km = nearest$distance_km
    ))
  }
}

# 设置NetCDF根目录路径
nc_root <- "/path/to/nc/files/"

# UI
ui <- navbarPage("森林生态系统模拟",
                 tabPanel("预测",
                          sidebarLayout(
                            sidebarPanel(
                              dateInput("start_date", "开始日期", value = Sys.Date() - 30),
                              dateInput("end_date", "结束日期", value = Sys.Date()),
                              numericInput("lat", "纬度", value = 35.0),
                              numericInput("lon", "经度", value = 105.0),
                              actionButton("run", "运行预测")
                            ),
                            mainPanel(
                              plotOutput("plot_gpp"),
                              plotOutput("plot_et"),
                              plotOutput("plot_sw")
                            )
                          )
                 )
)

# Server
server <- function(input, output) {
  observeEvent(input$run, {
    req(input$start_date, input$end_date, input$lat, input$lon)
    
    # 数据库连接
    conn <- dbConnect(
      RMySQL::MySQL(),
      dbname = "preles",
      host = "47.108.94.53",
      port = 3306,
      user = "navicat_admin",
      password = "1QAZ2wsx`"
    )
    
    date_seq <- seq.Date(input$start_date, input$end_date, by = "day")
    date_str <- sprintf("('%s')", paste(date_seq, collapse = "','"))
    
    get_paths <- function(table_name) {
      sql <- sprintf("SELECT * FROM %s WHERE date IN %s", table_name, date_str)
      df <- dbGetQuery(conn, sql)
      df$full_path <- file.path(nc_root, df[, 2])
      df
    }
    
    co2_df <- get_paths("co2")
    fapar_df <- get_paths("fapar")
    par_df <- get_paths("par")
    precip_dur_df <- get_paths("precip_duration")
    precip_flux_df <- get_paths("precip_flux")
    rh_df <- get_paths("relative_humidity")
    tair_df <- get_paths("tair")
    
    dbDisconnect(conn)
    
    # 读取数据（需你自定义的函数）
    co2 <- mapply(get_co2_at_point, co2_df$full_path, MoreArgs = list(lat = input$lat, lon = input$lon))
    fapar <- mapply(get_fapar_at_point, fapar_df$full_path, MoreArgs = list(lat = input$lat, lon = input$lon))
    par <- mapply(get_par_at_point, par_df$full_path, MoreArgs = list(lat = input$lat, lon = input$lon))
    precip_dur <- mapply(get_precip_duration_at_point, precip_dur_df$full_path, MoreArgs = list(lat = input$lat, lon = input$lon))
    precip_flux <- mapply(get_precip_flux_at_point, precip_flux_df$full_path, MoreArgs = list(lat = input$lat, lon = input$lon))
    rh <- mapply(get_relative_humidity_at_point, rh_df$full_path, MoreArgs = list(lat = input$lat, lon = input$lon))
    tair <- mapply(get_temperature_at_point, tair_df$full_path, MoreArgs = list(lat = input$lat, lon = input$lon))
    
    # 提取出 flux 和 duration 中的值
    precip_flux_vals <- sapply(precip_flux, function(x) x$value)
    precip_dur_vals <- sapply(precip_dur, function(x) x$precip_duration)
    
    # 计算 Precip
    Precip <- precip_dur_vals * precip_flux_vals
    
    # 计算其他变量
    VPD <- 0.6108 * exp((17.27 * tair) / (tair + 237.3)) * (1 - rh / 100)
    
    # 构建输入表
    input_df <- data.frame(
      PAR = sapply(par, function(x) x$value),
      TAir = tair,
      VPD = VPD,
      Precip = Precip,
      fAPAR = sapply(fapar, function(x) x$fapar),
      CO2 = sapply(co2, function(x) x$co2_ppm),
      DOY = as.numeric(format(date_seq, "%j"))
    )
    
    # 线性插值缺失值
    input_df[] <- lapply(input_df, function(col) {
      if (is.numeric(col)) na.approx(col, na.rm = FALSE) else col
    })
    
    # 调用 preles 模型进行预测
    predictions <- preles(input_df)
    
    # 输出结果
    output$plot_gpp <- renderPlot({
      ggplot(predictions, aes(x = DOY, y = GPP)) + geom_line() + labs(title = "GPP Prediction")
    })
    
    output$plot_et <- renderPlot({
      ggplot(predictions, aes(x = DOY, y = ET)) + geom_line() + labs(title = "ET Prediction")
    })
    
    output$plot_sw <- renderPlot({
      ggplot(predictions, aes(x = DOY, y = SW)) + geom_line() + labs(title = "SW Prediction")
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
