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
