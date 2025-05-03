library(shiny)
library(shinythemes)
library(leaflet)
library(sf)
library(dplyr)
library(DT)

ui <- navbarPage(
  theme = shinytheme("flatly"),
  title = "森林生态系统碳平衡计量平台",
  tabPanel("区域分析",
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("1. 定义多边形区域"),
      numericInput("lat", "纬度:", value = NULL),
      numericInput("lng", "经度:", value = NULL),
      actionButton("add_point", "通过坐标添加点"),
      actionButton("clear_points", "清除所有点"),
      hr(),
      h4("当前多边形顶点:"),
      DTOutput("polygon_points_table"),
      actionButton("delete_selected", "删除选中点"),
      hr(),
      h4("2. 提取区域内数据"),
      actionButton("extract_data", "提取数据"),
      hr(),
      h4("3. 下载结果"),
      downloadButton("downloadData", "下载提取的数据"),
      hr(),
      h5("提示: 也可以直接在地图上点击添加点")
    ),
    mainPanel(
      width = 8,
      leafletOutput("map", height = "500px"),
      h4("提取的数据:"),
      DTOutput("extracted_data_table")
    )
  ))
)

server <- function(input, output, session) {
  # 示例数据 - 实际应用中替换为你的数据库
  sample_data <- reactive({
    set.seed(123)
    data.frame(
      id = 1:100,
      lat = runif(100, 30, 40),
      lng = runif(100, 110, 120),
      value = rnorm(100, 50, 10)
    )
  })
  
  # 存储多边形顶点
  polygon_points <- reactiveVal(data.frame(lat = numeric(), lng = numeric(), id = integer()))
  
  # 添加点到多边形 - 通过坐标输入
  observeEvent(input$add_point, {
    req(input$lat, input$lng)
    new_id <- ifelse(nrow(polygon_points()) == 0, 1, max(polygon_points()$id) + 1)
    new_point <- data.frame(lat = input$lat, lng = input$lng, id = new_id)
    updated_points <- rbind(polygon_points(), new_point)
    polygon_points(updated_points)
    
    # 清空输入框
    updateNumericInput(session, "lat", value = NA)
    updateNumericInput(session, "lng", value = NA)
  })
  
  # 添加点到多边形 - 通过地图点击
  observeEvent(input$map_click, {
    click <- input$map_click
    new_id <- ifelse(nrow(polygon_points()) == 0, 1, max(polygon_points()$id) + 1)
    new_point <- data.frame(lat = click$lat, lng = click$lng, id = new_id)
    updated_points <- rbind(polygon_points(), new_point)
    polygon_points(updated_points)
  })
  
  # 清除所有点
  observeEvent(input$clear_points, {
    polygon_points(data.frame(lat = numeric(), lng = numeric(), id = integer()))
  })
  
  # 删除选中的点
  observeEvent(input$delete_selected, {
    req(input$polygon_points_table_rows_selected)
    points <- polygon_points()
    updated_points <- points[-input$polygon_points_table_rows_selected, ]
    polygon_points(updated_points)
  })
  
  # 显示当前多边形顶点表格（可交互选择）
  output$polygon_points_table <- renderDT({
    req(nrow(polygon_points()) > 0)
    datatable(
      polygon_points() %>% select(-id),
      options = list(
        pageLength = 5,
        dom = 't',
        ordering = FALSE
      ),
      selection = 'single',
      rownames = FALSE
    )
  })
  
  # 创建地图
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 115, lat = 35, zoom = 5)
  })
  
  # 观察多边形顶点变化并更新地图
  observe({
    points <- polygon_points()
    leafletProxy("map") %>%
      clearShapes() %>%
      clearMarkers()
    
    if (nrow(points) > 0) {
      # 添加标记点（带编号）
      leafletProxy("map") %>%
        addMarkers(
          data = points,
          lng = ~lng, lat = ~lat,
          label = ~as.character(1:nrow(points)),
          labelOptions = labelOptions(noHide = TRUE, direction = "top")
        )
      
      # 如果至少有3个点，绘制多边形
      if (nrow(points) >= 3) {
        leafletProxy("map") %>%
          addPolygons(
            data = points,
            lng = ~lng, lat = ~lat,
            fillColor = "blue",
            fillOpacity = 0.2,
            stroke = TRUE,
            color = "blue",
            weight = 2
          )
      }
    }
    
    # 添加示例数据点
    leafletProxy("map") %>%
      addCircleMarkers(
        data = sample_data(),
        lng = ~lng, lat = ~lat,
        radius = 3,
        color = "red",
        fillOpacity = 0.8,
        group = "data_points"
      )
  })
  
  # 提取多边形内数据
  extracted_data <- reactiveVal(NULL)
  
  observeEvent(input$extract_data, {
    req(nrow(polygon_points()) >= 3)
    
    # 创建多边形sf对象
    poly_sf <- polygon_points() %>%
      st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
      summarise(geometry = st_combine(geometry)) %>%
      st_cast("POLYGON") %>%
      st_convex_hull()
    
    # 创建数据点sf对象
    data_sf <- sample_data() %>%
      st_as_sf(coords = c("lng", "lat"), crs = 4326)
    
    # 执行空间查询
    inside <- st_intersects(data_sf, poly_sf, sparse = FALSE)[,1]
    
    # 提取结果
    result <- sample_data()[inside, ]
    extracted_data(result)
    
    # 在地图上高亮显示选中的点
    leafletProxy("map") %>%
      clearGroup("selected_points") %>%
      addCircleMarkers(
        data = result,
        lng = ~lng, lat = ~lat,
        radius = 5,
        color = "green",
        fillOpacity = 1,
        group = "selected_points"
      )
  })
  
  # 显示提取的数据
  output$extracted_data_table <- renderDT({
    req(extracted_data())
    datatable(extracted_data(), options = list(pageLength = 5))
  })
  
  # 下载提取的数据
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("extracted_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(extracted_data(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)