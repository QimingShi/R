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