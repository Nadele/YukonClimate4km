# Yukon Climate Analog Explorer
# designed by Nadele Flynn (nadele@ualberta.ca)
# built by Ashton Drew (ashton.drew@kdv-decisions.com)
# with assistance form Eliot Dixon (eliot.dixon@kdv-decisions.com)
# Fall 2020

options("rgdal_show_exportToProj4_warnings"="none")

library(shiny)
library(shinyjs)
library(shinycssloaders)
#library(shinyEventLogger)
library(leaflet)
library(viridis)
library(leafem)
library(htmltools)
library(lwgeom)
library(sf)
library(readxl)
library(raster)
library(rgdal)
library(FNN)
library(dplyr)
library(ggplot2)

#set_logging()

source("custom_functions.R")

## LOAD DATA ----

# Create an easy method to switch paths when working within local project versus as Shiny app
test <- "shiny" # choice of "local" or "shiny"
rootPath <- ifelse(test=="local", "YukonClimateAnalogs/Data/", "Data/")

# Data for the base map
locations <- read_xlsx(paste(rootPath, "Basemaps/yukon_towns.xlsx", sep="")) 
yukon <- read_sf(paste(rootPath, "Basemaps/yukon_boundary.geojson", sep="")) %>% 
	st_make_valid() %>%
	st_transform(crs=4326)
ecoregions <- read_sf(paste(rootPath, "Basemaps/Ecoregions_2014_1M.shp", sep="")) %>% 
	st_transform(crs=4326)

# Lookup tables
analogLookup <- read_xlsx(paste(rootPath, "Lookups/yukon_analogs.xlsx", sep=""))

# Set scale using DEM
grid <- "DEM4km"
#testDem <- raster(paste(rootPath, "Geospatial/", grid, "_aster.tif", sep=""))

# Create basemap with ecoregions and communities as reference
baseMap <- leaflet(data = yukon) %>% 
	addTiles(group = "OSM") %>%
	addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>%
	addPolygons(data=yukon, fill=F) %>%
	addPolygons(data=ecoregions, color="#444444", fillColor = "transparent", weight=1, group="Ecoregions", 
							label=~htmlEscape(ECOREGION), labelOptions = labelOptions(noHide = F, textOnly = TRUE)) %>%
	addMarkers(data=locations, lng=~Long, lat=~Lat, label=~htmlEscape(Location), 
						 group="Communities",
						 clusterOptions = markerClusterOptions(), 
						 labelOptions = labelOptions(noHide = T, textOnly = TRUE, 
				 														style = list("color" = "orange", "font-family" = "serif", "font-size" = "14px"))) %>%
	addMouseCoordinates() %>%
	addMiniMap(tiles = providers$Esri.WorldImagery, zoomLevelOffset = -4, position="bottomleft",
						 toggleDisplay = TRUE) %>%
	addScaleBar()



C <- read.csv(paste(rootPath, "AttData/CRU_TS/X.stn_detrended.csv", sep="")) # ICV proxy data. Linearly detrended 1951-1990 annual time series at selected CRU TS3.23 climate stations. These time series are used as proxies for local interannual climate variability ("ICV proxies")
cId <- read.csv(paste(rootPath, "AttData/CRU_TS/A.stn_detrended.csv", sep=""))[,1] # the id number of the ICV proxy that belongs to each grid cell 

#climateVarList <-  as.matrix(read.csv(paste(rootPath, "AttData/climateVar.csv", sep=""),header = F)) #central year of the normal period, e.g., 2085 indicates the 2071-2100 normal period.
#climate.List <- as.matrix(read.csv(paste(rootPath, "AttData/climateList.csv", sep=""), header = F))

