get_par_at_point <- function(nc_path, lon_input, lat_input, search_radius = 1) {
  library(ncdf4)
  
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
  library(ncdf4)
  library(geosphere)
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

get_precip_duration_at_point <- function(file_path, lon_input, lat_input, search_radius = 1.0) {
  library(ncdf4)
  precip_file <- nc_open(file_path)
  
  # 读取变量
  lons <- ncvar_get(precip_file, "lon")
  lats <- ncvar_get(precip_file, "lat")
  precip <- ncvar_get(precip_file, "Precipitation_Solid_Duration_Fraction")
  
  # 替换缺失值
  precip[precip == -9999] <- NA
  
  # 查找最近的经纬度索引
  lon_idx <- which.min(abs(lons - lon_input))
  lat_idx <- which.min(abs(lats - lat_input))
  
  # 获取该点的降水值
  value <- precip[lon_idx, lat_idx]
  
  # 如果该点的值缺失，进行范围查找
  if (is.na(value)) {
    # 搜索范围内的经纬度索引
    lon_range <- which(abs(lons - lon_input) <= search_radius)
    lat_range <- which(abs(lats - lat_input) <= search_radius)
    
    # 查找范围内的最近有效值
    found_value <- NA
    for (lon in lon_range) {
      for (lat in lat_range) {
        # 跳过缺失值
        if (!is.na(precip[lon, lat])) {
          found_value <- precip[lon, lat]
          break
        }
      }
      if (!is.na(found_value)) break
    }
    
    value <- found_value
  }
  
  # 关闭文件
  nc_close(precip_file)
  
  # 返回查询的值
  return(list(precip_duration = value))
}

get_precip_flux_at_point <- function(nc_path, target_lon, target_lat, max_distance = 0.5) {
  nc <- nc_open(nc_path)
  
  lon <- ncvar_get(nc, "lon")
  lat <- ncvar_get(nc, "lat")
  flux <- ncvar_get(nc, "Precipitation_Flux", collapse_degen = FALSE)  # 强制保留三维
  time <- ncvar_get(nc, "time")
  time_units <- ncatt_get(nc, "time", "units")$value
  time_origin <- sub("days since ", "", time_units)
  date <- as.character(as.Date(time, origin = time_origin))
  
  grid <- expand.grid(lon = lon, lat = lat)
  grid$value <- as.vector(flux[, , 1])
  
  grid$dist <- sqrt((grid$lon - target_lon)^2 + (grid$lat - target_lat)^2)
  grid <- grid[order(grid$dist), ]
  
  nearest <- grid[1, ]
  
  result <- list(
    date = date,
    lon = nearest$lon,
    lat = nearest$lat,
    value = ifelse(nearest$dist <= max_distance, nearest$value, NA),
    units = ncatt_get(nc, "Precipitation_Flux", "units")$value
  )
  
  nc_close(nc)
  return(result)
}

get_relative_humidity_at_point <- function(file_path, target_lon, target_lat, max_distance = 0.1) {
  library(ncdf4)
  nc <- nc_open(file_path)
  
  lon <- ncvar_get(nc, "lon")
  lat <- ncvar_get(nc, "lat")
  rh <- ncvar_get(nc, "Relative_Humidity_2m_09h", collapse_degen = FALSE)
  time <- ncvar_get(nc, "time")
  time_units <- ncatt_get(nc, "time", "units")$value
  nc_close(nc)
  
  # 日期转换
  origin <- sub("days since ", "", time_units)
  date <- as.Date(time, origin = origin)
  
  # 构建网格并转为 data.frame
  grid_df <- expand.grid(lon = lon, lat = lat)
  grid_df$value <- as.vector(rh[, , 1])
  
  # 计算距离并筛选
  grid_df$dist <- sqrt((grid_df$lon - target_lon)^2 + (grid_df$lat - target_lat)^2)
  grid_df <- grid_df[!is.na(grid_df$value) & grid_df$value != -9999, ]
  
  if (nrow(grid_df) == 0 || min(grid_df$dist) > max_distance) {
    return(list(
      date = as.character(date),
      lon = target_lon,
      lat = target_lat,
      value = NA,
      units = "%"
    ))
  }
  
  nearest <- grid_df[which.min(grid_df$dist), ]
  return(list(
    date = as.character(date),
    lon = nearest$lon,
    lat = nearest$lat,
    value = nearest$value,
    units = "%"
  ))
}

get_temperature_at_point <- function(nc_path, lon_val, lat_val, missing_value = -9999) {
  nc <- nc_open(nc_path)
  on.exit(nc_close(nc))
  
  lon <- ncvar_get(nc, "lon")
  lat <- ncvar_get(nc, "lat")
  
  lon_idx <- which.min(abs(lon - lon_val))
  lat_idx <- which.min(abs(lat - lat_val))
  
  temp <- ncvar_get(nc, "Temperature_Air_2m_Mean_24h", collapse_degen = FALSE)
  time_raw <- ncvar_get(nc, "time")
  time_units <- ncatt_get(nc, "time", "units")$value
  time_origin <- sub("days since ", "", time_units)
  date <- as.character(as.Date(time_raw[1], origin = time_origin))
  
  value_k <- temp[lon_idx, lat_idx, 1]
  value_c <- ifelse(value_k == missing_value, NA, value_k - 273.15)
  
  return(list(
    date = date,
    lon = lon[lon_idx],
    lat = lat[lat_idx],
    value_K = value_k,
    value_C = value_c,
    units = "°C"
  ))
}

get_fapar_at_point <- function(nc_path, lon_val, lat_val, k = 0.5) {
  nc <- nc_open(nc_path)
  on.exit(nc_close(nc))
  
  lon <- ncvar_get(nc, "longitude")
  lat <- ncvar_get(nc, "latitude")
  lon_idx <- which.min(abs(lon - lon_val))
  lat_idx <- which.min(abs(lat - lat_val))
  
  lai_hv_raw <- ncvar_get(nc, "lai_hv")
  lai_lv_raw <- ncvar_get(nc, "lai_lv")
  
  dims <- dim(lai_hv_raw)
  
  if (length(dims) == 3) {
    lai_hv <- lai_hv_raw[lon_idx, lat_idx, 1]
    lai_lv <- lai_lv_raw[lon_idx, lat_idx, 1]
  } else if (length(dims) == 2) {
    lai_hv <- lai_hv_raw[lon_idx, lat_idx]
    lai_lv <- lai_lv_raw[lon_idx, lat_idx]
  } else {
    stop("变量维度不支持，dim: ", paste(dims, collapse = " x "))
  }
  
  # 修复：先判断是否为 NA，再判断是否大于缺失值阈值
  if (!is.na(lai_hv) && lai_hv > 1e+30) lai_hv <- NA
  if (!is.na(lai_lv) && lai_lv > 1e+30) lai_lv <- NA
  
  lai_total <- sum(c(lai_hv, lai_lv), na.rm = TRUE)
  fapar <- if (!is.na(lai_total)) 1 - exp(-k * lai_total) else NA
  
  time_raw <- ncvar_get(nc, "valid_time")
  time_units <- ncatt_get(nc, "valid_time", "units")$value
  time_origin <- sub("seconds since ", "", time_units)
  date <- if (length(time_raw) > 0) {
    as.character(as.POSIXct(time_raw[1], origin = time_origin, tz = "UTC"))
  } else {
    NA
  }
  
  return(list(
    date = date,
    lon = lon[lon_idx],
    lat = lat[lat_idx],
    lai_hv = lai_hv,
    lai_lv = lai_lv,
    lai_total = lai_total,
    fapar = fapar
  ))
}




