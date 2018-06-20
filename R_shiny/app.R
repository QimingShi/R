library(shiny)
library(leaflet)
library(RColorBrewer)
library(raster)
library(shapefiles)
library(rgdal)
library(xtermStyle)
library(rgdal)
library(lattice)
library(Cairo)


# The input file geodatabase
fgdb = "Data/try2.gdb"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list = ogrListLayers(fgdb)

# Read the feature class
m = readOGR(dsn=fgdb,layer="final3")
d = read.csv("Data/change.csv")
fc = merge(m, d, by.x="TOWN", by.y = "city")
DF <- as.data.frame(fc)

dat = readLines("Data/icd_format.txt", encoding = "UTF-8")

vars <- c("Household Income"="Income",
          "Patient Ratio"="Ratio",
          "Population"="POP2010",
          "Median Age"="Median_Age",
          dat
)
vars1 <- c(
  dat
)
# llanos <- shapefile("C:/Users/shiq/Desktop/shape/join.shp")
# ##need to change the coordinate system when importing map
fc = spTransform(fc,CRS("+proj=longlat +datum=WGS84"))


ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(fixed = TRUE, raggable = TRUE,top = 10, right = 10,bottom = "auto",
                selectInput("color", "Color(X)", vars),
                selectInput("size", "Circle Size(Y)", vars1),
                # uiOutput("sliderIn"),
                checkboxInput("legend", "Show legend", TRUE),
                plotOutput('plots',height = 250,brush="plot_brush"),
                textOutput("text_r")
  )
)

server <- function(input, output, session) {
#use observeevent to listen to reactive input, once the input$range is triggered
#observeEvent will run the function inside.function as observe with isolate
  observeEvent(input$range,{
    colorBy <- input$color
    sizeBy <- input$size
    range_number <- input$range
    pop <- "POP2010"
    
    filteredData <- reactive({
      a = fc[sizeBy][[1]]
      #select the row and column
      fc[a >= range_number[1] & a <= range_number[2], sizeBy]})
    
    data2 = filteredData()
    
    
    filteredData1 <- reactive({
      a = fc[sizeBy][[1]]
      b = fc[[colorBy]]
      #select the row and column
      b[a >= range_number[1] & a <= range_number[2]]})
    
    data3 = filteredData1()
  
    pal <- colorBin("Spectral", data3, 7, pretty = FALSE)

    radius = fc[[sizeBy]]
    modify_function = function(x){
      if(x > 0.20){
        x = 0.20
      }
      else
        x
    }
    #give data2[[1]] / count_sum to data_71 to let it be a memory
    #if you do data2[[1]] / count_sum, it is temporary memory
    #you can not use it as a list
    #according to slider input change, data2 will coresponde,
    count_sum = sum(fc[sizeBy][[1]])
    if(count_sum==0){data_71=0
    }else{data_71 = data2[[1]] / count_sum}
    #the fc[[sizeBy]] has 71 features, you must use sapply to deal the bunch
    #of data inside fc[[sizeBy]]. or it will just deal with the first vector it meets.
    data_71 = sapply(data_71, FUN = modify_function)
    radius <- (data_71) * 35000

    
    if(nrow(filteredData())==0) { leafletProxy("map") %>% clearShapes()} 
    else {
      leafletProxy("map", data = data2) %>% clearShapes() %>%
      #the layerid should be llanos$TOWN2, cannot be llanos,if use llanos, the map_shape_click
      #gonna extract the whole information.
      #radius is decided by data_71 and data_71 is decided by data2 from slider input. Before, the circle
      #will jump around because the slider input shrank the data so the data2 is changing, but radius
      #does not change
      addCircles( radius=radius, layerId=fc$OBJECTID,
                  stroke=FALSE, fillOpacity=0.7, fillColor=pal(data3))
      }

  })
  
  observeEvent(input$color,{
    colorBy <- input$color
    sizeBy <- input$size
    range_number <- input$range
    pop <- "POP2010"
    colorData <- fc[[colorBy]]
    pal <- colorBin("Spectral", colorData, 7, pretty = FALSE)
    # Use a separate observer to recreate the legend as needed.
    proxy <- leafletProxy("map",data = fc)
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorBin("Spectral", colorData, 7, pretty = FALSE)
      proxy %>% addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                           layerId=fc)
      }
  })


  observe({
    colorBy <- input$color
    sizeBy <- input$size
    pop <- "POP2010"
    

    filteredData <- reactive({
      fc[sizeBy]})
    
    data2 = filteredData()
# just get the min and max from filterdata, the slider is controlled by observeevent
    # output$sliderIn <- renderUI({
    # 
    #   a <- fc[sizeBy][[1]]
    # 
    #   sliderInput("range", "Significant", min(a), max(a),
    #               value = range(a), step = 1)
    # })
    
    y = fc[][[sizeBy]]
    x = fc[][[colorBy]]

    output$plots <- renderPlot({
      print(xyplot(y ~ x,type = c("p", "r"),
                   col.line = "darkorange", lwd = 3))
    },bg="transparent")
    
    output$text_r <- renderText({ 
      paste("R:", round(cor(x,y),digits = 5),
            "R squared:",round(cor(x,y)*cor(x,y),digits = 5))
    })
    
    colorData <- fc[[colorBy]]
    pal <- colorBin("Spectral", colorData, 7, pretty = FALSE)
    
    radius = fc[[sizeBy]]
    modify_function = function(x){
      if(x > 0.20){
        x = 0.20
      }
      else
        x
    }
    #give fc[[sizeBy]] / cumsum(fc[[sizeBy]])[71] to data_71 to let it be a memory
    #if you do fc[[sizeBy]] / cumsum(fc[[sizeBy]])[71], it is temporary memory
    #you can not use it as a list
    if(cumsum(fc[[sizeBy]])[71]==0){data_71=0
    }else{data_71 = fc[[sizeBy]] / cumsum(fc[[sizeBy]])[71]}
    #the fc[[sizeBy]] has 71 features, you must use sapply to deal the bunch
    #of data inside fc[[sizeBy]]. or it will just deal with the first vector it meets.
    data_71 = sapply(data_71, FUN = modify_function)
    radius <- (data_71) * 35000
    
    
    leafletProxy("map", data = data2) %>%
      clearShapes() %>%
      #the layerid should be llanos$TOWN2, cannot be llanos,if use llanos, the map_shape_click
      #gonna extract the whole information.
      addCircles( radius=radius, layerId=fc$OBJECTID,
                  stroke=FALSE, fillOpacity=0.7, fillColor=pal(colorData))
  
  })

  
  # Show a popup at the given location
  showZipcodePopup <- function(UNIQUEid, lat, lng) {
    selectedZip <- fc[fc$OBJECTID == UNIQUEid,]
    content <- as.character(tagList(
      # tags$h4("ICD9/10:",input$size),
      tags$strong(HTML(sprintf("%s, %s",
                               selectedZip$TOWN, selectedZip$zip
      ))), tags$br(),
      #the automatic input$size is r dataframe, to access the data, 
      #you should find which bracket it locates
      sprintf("Number of Patients: %s", selectedZip[,input$size][[1]]), tags$br(),
      sprintf("Population: %s", as.integer(selectedZip$POP2010)), tags$br(),
      sprintf("Patient Ratio: %s%%", selectedZip[,input$size][[1]]*100/as.integer(selectedZip$POP2010)), tags$br(),
      # sprintf("Median Age: %s", as.character(selectedZip$Median_Age)), tags$br(),
      sprintf("Median Household Income: %s", as.integer(selectedZip$Income)), tags$br()
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId=UNIQUEid)
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
  #event tends to find the id in the layer, so the layer id in observeevent must be fc$object, also
  #in observe
    event <- input$map_shape_click
    if (is.null(event))
      return()
  #the click pass three variables in, it is used in showZipcodePopup <- function(UNIQUEid, lat, lng)
    isolate({
      showZipcodePopup(event$id, event$lat, event$lng)
    })
  })
  
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(fc) %>%
      addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
               attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
      setView(lng = -71.76, lat = 42.33, zoom = 10)
  })
  
}


shinyApp(ui, server)
