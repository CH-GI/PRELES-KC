library("ecmwfr")
# set a key to the keychain
wf_set_key(key = "1qaz2wsx3edc4rfv5tgb6yhn7usdfsafdsdfqefd")

# you can retrieve the key using
wf_get_key()

# the output should be the key you provided
# "abcd1234-foo-bar-98765431-XXXXXXXXXX"

# Alternatively you can input your login info with an interactive request
# if you do not put in the key directly
wf_set_key()

# you will get a command line request to provide the required details

request <- list(
  dataset_short_name = "derived-era5-single-levels-daily-statistics",
  product_type = "reanalysis",
  # variable = c("2m_temperature", "total_precipitation", "clear_sky_direct_solar_radiation_at_surface", "surface_net_solar_radiation", "surface_solar_radiation_downwards", "top_net_solar_radiation", "total_sky_direct_solar_radiation_at_surface", "soil_type", "high_vegetation_cover", "leaf_area_index_high_vegetation"),
  variable = c("2m_temperature"),
  year = "2023",
  month = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),
  day = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"),
  daily_statistic = "daily_mean",
  time_zone = "utc+00:00",
  frequency = "6_hourly",
  area = c(40, 107, 36, 112),
  target = "TMPFILE"
)
file <- wf_request(
  request  = request,  # the request
  transfer = TRUE,     # download the file
  path     = "."       # store data in current working directory
)

r <- terra::rast(file)
terra::plot(r, main = "ERA-5 Reanalysis Demo (2m Temperature)")
maps::map("world", add = TRUE)



