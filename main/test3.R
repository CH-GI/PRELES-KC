library(shiny)
library(leaflet)
library(leaflet.extras)
library(ggplot2)
library(sf)
library(viridis)
library(rnaturalearth)
library(dplyr)

ui <- fluidPage(
  titlePanel("全球地理热力图 - 专业版"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("radius", "热力点半径(像素):", 
                  min = 5, max = 30, value = 15),
      sliderInput("blur", "边缘模糊度:", 
                  min = 5, max = 30, value = 20),
      selectInput("palette", "颜色方案:",
                  choices = c("plasma", "inferno", "magma", "viridis"),
                  selected = "plasma"),
      numericInput("min_value", "最小显示值:", 
                   min = 0, value = 10),
      actionButton("update", "更新热力图"),
      downloadButton("export", "导出高清图")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("交互地图", 
                 leafletOutput("heatmap", height = "700px")),
        tabPanel("静态分析图",
                 plotOutput("analyticPlot", height = "700px"))
      )
    )
  )
)

server <- function(input, output) {
  # 模拟数据生成（已修正语法错误）
  geo_data <- reactive({
    set.seed(123)
    df <- data.frame(
      lng = runif(1000, -180, 180),
      lat = runif(1000, -90, 90),
      value = abs(rnorm(1000, mean = 50, sd = 30))
    )  # 这里之前缺少右括号
    df %>% filter(value >= input$min_value)
  })
  
  # 交互式热力图
  output$heatmap <- renderLeaflet({
    input$update
    
    data <- geo_data()
    pal <- colorNumeric(
      palette = input$palette,
      domain = data$value
    )
    
    isolate({
      leaflet(data) %>%
        addProviderTiles(providers$CartoDB.DarkMatter) %>%
        addHeatmap(
          lng = ~lng, 
          lat = ~lat, 
          intensity = ~value,
          radius = input$radius,
          blur = input$blur,
          gradient = pal,
          cellSize = 10
        ) %>%
        setView(lng = 0, lat = 30, zoom = 2) %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = ~value,
          title = "强度值"
        )
    })
  })
  
  # 分析用静态图
  output$analyticPlot <- renderPlot({
    world <- ne_countries(scale = "medium", returnclass = "sf")
    data <- geo_data()
    
    ggplot() +
      geom_sf(data = world, fill = "gray20", color = NA) +
      stat_density_2d(
        data = data,
        aes(x = lng, y = lat, fill = after_stat(density)),
        geom = "raster",
        contour = FALSE,
        n = 300
      ) +
      scale_fill_viridis(option = input$palette) +
      coord_sf(crs = st_crs("ESRI:54030")) +
      labs(title = "ChatGPT API分析专用图",
           subtitle = paste("数据点:", nrow(data), "| 显示阈值:", input$min_value),
           caption = paste("生成时间:", Sys.time())) +
      theme_void()
  })
  
  # 高清图导出
  output$export <- downloadHandler(
    filename = function() {
      paste("heatmap_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png", sep = "")
    },
    content = function(file) {
      plt <- ggplot() +
        geom_sf(data = ne_countries(scale = "medium", returnclass = "sf"), 
                fill = "gray20", color = NA) +
        stat_density_2d(data = geo_data(), 
                        aes(x = lng, y = lat, fill = after_stat(density)),
                        geom = "raster", n = 500) +
        scale_fill_viridis(option = input$palette) +
        coord_sf(crs = st_crs("ESRI:54030")) +
        theme_void()
      
      ggsave(file, plot = plt, device = "png", 
             width = 16, height = 9, dpi = 300, bg = "white")
    }
  )
}

shinyApp(ui, server)