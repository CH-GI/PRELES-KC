library(shiny)
library(ncdf4)
library(Rprebasso)
library(ggplot2)

# ==== Function 1: Divide area into grid boxes ====
get_grid_boxes <- function(lon1, lon2, lat1, lat2, step) {
  lons <- seq(min(lon1, lon2), max(lon1, lon2) - step, by = step)
  lats <- seq(min(lat1, lat2), max(lat1, lat2) - step, by = step)
  expand.grid(lon = lons, lat = lats)
}

# ==== Function 2: Read NetCDF average over small box ====
read_box_avg <- function(ncfile, varname, lon_min, lon_max, lat_min, lat_max) {
  nc <- nc_open(ncfile)
  lon <- ncvar_get(nc, "lon")
  lat <- ncvar_get(nc, "lat")
  var <- ncvar_get(nc, varname)
  
  lon_idx <- which(lon >= lon_min & lon <= lon_max)
  lat_idx <- which(lat >= lat_min & lat <= lat_max)
  
  if (length(dim(var)) == 3) {
    data_slice <- var[lon_idx, lat_idx, 1]
  } else {
    data_slice <- var[lon_idx, lat_idx]
  }
  
  nc_close(nc)
  mean(data_slice, na.rm = TRUE)
}

# ==== Function 3: Extract preles input variables ====
get_preles_inputs <- function(grid_boxes, nc_paths, box_size) {
  results <- data.frame()
  for (i in 1:nrow(grid_boxes)) {
    lon0 <- grid_boxes$lon[i]
    lat0 <- grid_boxes$lat[i]
    
    row <- list(lon = lon0, lat = lat0)
    for (varname in names(nc_paths)) {
      value <- read_box_avg(
        ncfile = nc_paths[[varname]],
        varname = varname,
        lon_min = lon0, lon_max = lon0 + box_size,
        lat_min = lat0, lat_max = lat0 + box_size
      )
      row[[varname]] <- value
    }
    results <- rbind(results, as.data.frame(row))
  }
  return(results)
}

# ==== Function 4: Run preles ====
run_preles <- function(df_inputs) {
  df_inputs$DOY <- 180
  df_inputs$year <- 2020
  
  # Derived variables
  df_inputs$Precip <- df_inputs$precip_flux * df_inputs$precip_duration
  df_inputs$VPD <- 0.6108 * exp(17.27 * df_inputs$tair / (df_inputs$tair + 237.3)) *
    (1 - df_inputs$relative_humidity / 100)
  
  df_preles_input <- df_inputs[, c("co2", "fapar", "par", "Precip", "VPD", "tair", "lat", "lon", "DOY", "year")]
  output <- preles(df_preles_input)
  
  cbind(df_inputs[, c("lon", "lat")], GPP = output$GPP, ET = output$ET, SW = output$SW)
}

# ==== Function 5: Plot heatmap ====
plot_heatmap <- function(df, varname) {
  ggplot(df, aes(x = lon, y = lat, fill = .data[[varname]])) +
    geom_tile() +
    scale_fill_viridis_c(option = "C") +
    labs(title = paste("Heatmap of", varname), fill = varname) +
    coord_fixed()
}

# ==== UI ====
ui <- fluidPage(
  titlePanel("Preles Heatmap Generator"),
  sidebarLayout(
    sidebarPanel(
      numericInput("lon1", "Longitude 1:", 100),
      numericInput("lon2", "Longitude 2:", 110),
      numericInput("lat1", "Latitude 1:", 30),
      numericInput("lat2", "Latitude 2:", 35),
      numericInput("box_size", "Grid Box Size (degree):", 1),
      actionButton("run_btn", "Generate Heatmaps")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("GPP", plotOutput("gpp_plot")),
        tabPanel("ET", plotOutput("et_plot")),
        tabPanel("SW", plotOutput("sw_plot"))
      )
    )
  )
)

# ==== Server ====
server <- function(input, output) {
  observeEvent(input$run_btn, {
    grid <- get_grid_boxes(input$lon1, input$lon2, input$lat1, input$lat2, input$box_size)
    
    nc_paths <- list(
      co2 = "co2.nc",
      fapar = "fapar.nc",
      par = "par.nc",
      precip_duration = "precip_duration.nc",
      precip_flux = "path/to/precip_flux.nc",
      relative_humidity = "path/to/relative_humidity.nc",
      tair = "path/to/tair.nc"
    )
    
    df_inputs <- get_preles_inputs(grid, nc_paths, input$box_size)
    df_result <- run_preles(df_inputs)
    
    output$gpp_plot <- renderPlot({ plot_heatmap(df_result, "GPP") })
    output$et_plot <- renderPlot({ plot_heatmap(df_result, "ET") })
    output$sw_plot <- renderPlot({ plot_heatmap(df_result, "SW") })
  })
}

# ==== Run App ====
shinyApp(ui, server)


