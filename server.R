# Yukon Climate Analog Explorer
# designed by Nadele Flynn (nadele@ualberta.ca)
# built by Ashton Drew (ashton.drew@kdv-decisions.com) & Eliot Dixon (eliot.dixon@kdv-decisions.com)
# Fall 2020

# Define server logic required to draw a histogram
shinyServer(function(input, output, session){
	
	#shinyEventLogger::set_logging_session()

#SELECT A LOCATION OF INTEREST ----
	
	# Location choice defaults to whitehorse coordinates ----
	xyValues <- reactiveValues(Longitude=-135.05, Latitude=60.716667, Test=TRUE)

	# update coordinate choice with mapclick ----
	observeEvent(input$locationMap_click, {
		if (input$source == "map"){
			xyValues$Longitude <- input$locationMap_click$lng
			xyValues$Latitude <- input$locationMap_click$lat
			# yukonIntersect = st_intersection(x = st_transform(yukon, crs=3579),
			# 																 y = st_transform(data.frame(lon = as.vector(xyValues$Longitude),
			# 																 														lat = as.vector(xyValues$Latitude)) %>% 
			# 																 								 	st_as_sf(coords = c("lon", "lat")) %>% st_set_crs(4326)), crs=3579)
			# xyValues$Test <- ifelse(nrow(yukonIntersect==1), TRUE, FALSE)
		} else {
			xyValues$Longitude <- xyValues$Longitude
			xyValues$Latitude <- xyValues$Latitude
		}
	})
	
	# update coordinate choice with numeric entry ----
	observeEvent(input$coordinates, {
		if (input$source == "text"){
			xyValues$Longitude <- as.numeric(input$longitude)
			xyValues$Latitude <- as.numeric(input$latitude)
			# yukonIntersect = st_intersection(x = st_transform(yukon, crs=3579),
			# 																 y = st_transform(data.frame(lon = as.vector(xyValues$Longitude),
			# 																 														lat = as.vector(xyValues$Latitude)) %>% 
			# 																 								 	st_as_sf(coords = c("lon", "lat")) %>% st_set_crs(4326)), crs=3579)
			# xyValues$Test <- ifelse(nrow(yukonIntersect==1), TRUE, FALSE)
		} else {
			xyValues$Longitude <- xyValues$Longitude
			xyValues$Latitude <- xyValues$Latitude
		}
	})
	
	# Provide a message to help users enter coordinates ----
	output$chosenXY <- renderText({
		if(xyValues$Latitude == 60.716667 & xyValues$Longitude==-135.05){
			paste("The default location is Whitehorse (Latitude = ", round(xyValues$Latitude, 3), " Longitude = ", round(xyValues$Longitude, 3), "). To choose a different location either click on the map or enter new coordinates.  The chosen location appears as a red circle in the map.", sep="")}
		else if(xyValues$Latitude != 60.716667 | xyValues$Longitude != -135.05){
			paste("You have selected coordinates (Latitude = ", round(xyValues$Latitude, 3), " Longitude = ", round(xyValues$Longitude, 3), ").  The chosen location appears as a red circle in the map.", sep="")
		}
	})
	
#SELECT TIMEFRAME AND OUTPUT TEXT CHECK ----
	
	output$chosenTime <- renderText({
		refLabel <- analogLookup$LABEL[as.numeric(input$ref)]
		compLabel <- analogLookup$LABEL[as.numeric(input$comp)]
		cat(file=stderr(), "CHECK LOOKUP CLASS", class(input$ref), "\n")
		cat(file=stderr(), "CHECK LABELS ID", "input$ref=", input$ref, "; input$comp=", input$comp, "\n")
		cat(file=stderr(), "CHECK LABELS TEXT", "refLabel=", refLabel, "; compLabel=", compLabel, "\n")
		if (input$ref == input$comp) {
			timeMessage <- paste("You have chosen to view the analog landscape with a single time frame (", refLabel, ").  The output will illustrate similarity of the Yukon landscape to your chosen location in the selected time frame.", sep="")
		} else {
			if (input$ref > input$comp) {
				timeMessage <- paste("Your model will step backwards in time.  The output will illustrate similarity of the chosen location in the reference timeframe (", refLabel, ") to the Yukon landscape of the comparison timeframe (", compLabel, ").", sep="")
			} else {
				timeMessage <- paste("Your model will step forwards in time.  The output will illustrate similarity of the chosen location in the reference timeframe (", refLabel, ") to the Yukon landscape of the comparison timeframe (", compLabel, ").", sep="")
			}
		}
	})
	
#SELECT VARIABLES ----
	
	#Row names (directly calculated variables) ----
	directOptions <- c("Tmax","Tmin","PPT")
	#column names (season abbreviations)
	columnOptions <- c("wt","sp","sm","at")
	
	#'Generates html for each checkbox within datatable ----
	shinyInput <- function(FUN, colIds, rowIds, ...){
		inputs <- NULL
		for(rowId in rowIds){
			inputs <- append(inputs,
											 sapply(paste0(rowId,"_",colIds), function(x){
											 	as.character(FUN(inputId=x, label=NULL,width=80))
											 })
			)
		}
		return(inputs)
	}
	
	#1: Generate HTML for each checkbox ----
	inputs <- shinyInput(FUN=checkboxInput, colIds=columnOptions, rowIds=directOptions)
	
	#2: Construct dataframe from which table will be generated ----
	df <- data.frame(Variables = directOptions,
									 Winter = inputs[paste0(directOptions,"_wt")],
									 Spring = inputs[paste0(directOptions,"_sp")],
									 Summer = inputs[paste0(directOptions,"_sm")],
									 Autumn = inputs[paste0(directOptions,"_at")],
									 stringsAsFactors = FALSE)
	
	#3: generate table ----
	output$checkbox_table <- DT::renderDataTable(
		df,
		server = FALSE, escape = FALSE, selection = 'none',
		rownames = FALSE,
		options = list(
			dom = 't', paging = FALSE, ordering = FALSE,
			preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
			drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
		)
	)
	
	#reactive variable container ----
	varVector <- reactiveValues()
	
	#Store variable selection ----
	observeEvent(input$run, {
		varVector$chosenVarSet <- c()
		inputNames <- sapply(columnOptions, function(x){
			sapply(directOptions, paste0,"_",x)
		})
		
		for(var in inputNames){
			if(input[[var]]){
				varVector$chosenVarSet <- c(varVector$chosenVarSet, var)
			}
		}
		#This is debug output to check variable selection
		#output$testOut<- renderText(varVector$chosenVarSet)
	}) #ObserveEvent
	
	# Helpful variable selection messages ----
	output$chosenVars <- renderText({
		validate(
			need(input$run, "When you hit the run button, your variable choices will be noted here.")
		)
		#You must choose at least TWO variable to run a model.
			tmpString <- paste(strsplit(varVector$chosenVarSet, split=","), collapse=", ")
			paste("You have chosen the variables: ", tmpString)
			
	})
	
#RUN THE MODEL AND OUTPUT RASTER MAP ----
	
	newRaster <- eventReactive(input$run, {

		# Use analog lookup table to identify file name elements ----
		cat(file=stderr(), "SETTING ANALOGS IN NEWRASTER", "input$comp=", input$ref, "; input$comp=", input$comp, "\n")
		refValue <- analogLookup$VALUE[as.numeric(input$ref)]
		compValue <- analogLookup$VALUE[as.numeric(input$comp)]
		
		# Data source paths based on user choices ----
		cat(file=stderr(), "SETTING PATHS", "Reference=", refValue, "; Comparison=", compValue, "\n")
		refFile <- paste(rootPath, "AttData/", grid, "/X.", grid, "_630_", refValue, "MSY.csv", sep="")
		compFile <-  paste(rootPath, "AttData/", grid, "/X.", grid, "_630_", compValue, "MSY.csv", sep="") 
		landFile <-  paste(rootPath, "AttData/", grid, "/", grid, "_land.csv", sep="")
		proxyFile <-  paste(rootPath, "AttData/", grid, "/", grid, "_4nn.csv", sep="")
		aGridDistFile <- paste(rootPath, "AttData/", grid, "/", grid, "_4dist.csv", sep="")
		demFile <- paste(rootPath, "GeoSpatial/", grid, "_aster.tif", sep="")
		gridRefFile <- paste(rootPath, "AttData/", grid, "/", grid, ".csv",sep="")
		nonCnaFile <- paste(rootPath, "AttData/", grid, "/", grid, "_nonCNA.csv", sep="")
		
		# Read data based on paths ----
		cat(file=stderr(), "READING DATA", "\n")
		land <- read.csv(landFile)[,1] # DEM cells that make up the comparison (climate) matrix. this list includes a coastal buffer that is excluded from the reference climate (analog) pool (A matrix).  
		#shinyEventLogger::#shinyEventLogger::log_output(head(land))
		proxy <- read.csv(proxyFile)  # the ICV proxy used for each grid cell for 1km grid Yukon "grid1nn_Normal.csv"
		aGridDist <- read.csv(aGridDistFile) #distance of the 1..4 climate station for each grid cell,used for IDW
		dem <- raster(demFile) # the digital elevation model (DEM) used to generate the input data. This is used as a template raster for mapping the results. 
		nonCnaDem <- Which(!is.na(dem), cells = TRUE)
		#shinyEventLogger::log_output(str(dem))
		#shinyEventLogger::log_output(str(nonCnaDem))		
		
		
		
		# Run models based on user choice for analog ----
		cat(file=stderr(), "SET A and B MATRICES", "\n")
		#if (input$analog == "Forwards") #set up Forward model
		if (input$ref <= input$comp) #set up Forward model
		{
			A <- read.csv(refFile) # read in the reference data in the A matrix
			B <- read.csv(compFile) # read in the comparison climate data in the B matrix
			AName <- refValue # "Normal_1961_1990" 
			BName <- compValue # "Normal_1991_2019" 
		#} else if (input$analog == "Backwards") #set up Backward model
		} else if (input$ref > input$comp) #set up Backward model
		{
			B <- read.csv(refFile) # read in the reference data in the B matrix
			A <- read.csv(compFile) # read in the comparison climate data in the A matrix
			AName <- compValue
			BName <- refValue
		}
		
		# Read the gridref file ----
		cat(file=stderr(), "READING GRIDREF", "\n")
		gridRef <- read.csv(gridRefFile, strip.white = TRUE, na.strings = c("NA","",-9999) )
		nonCna <- as.vector(t(read.csv(nonCnaFile)))
		
		gridRef <- gridRef[-nonCna,which(names(gridRef) %in% names(gridRef)[grep("id|lat|lon",names(gridRef))])] #include all variables
		#shinyEventLogger::log_output(str(gridRef))	
		
		#Preserve rownames (for those nonCNA values) in both the A and B values so search that depend on a static ROW IDs, that matches the grid.ref2 file, can be used. Get rid of column after transfer to the rowname (i.e. it is a matrix)
		
		# Apply chosen climate variables ----
		cat(file=stderr(), "CHOOSING CLIMATE VARS", "\n")
		#climateVar <- strsplit(climateVarList[climateVar2],",")[[1]]
		climateVar <- varVector$chosenVarSet
		
		A <- A %>% dplyr::select("id1",all_of(climateVar))
		B <- B %>% dplyr::select("id1",all_of(climateVar))
		C <- C %>% dplyr::select(all_of(climateVar))
		
		rownames(A) <- A[,1]
		rownames(B) <- B[,1]
		
		A[,1] <- NULL 
		B[,1] <- NULL
		
		# Perform local calculations ----
		cat(file=stderr(), "PERFORM LOCAL CALCULATION", "\n")
		ARowID <- LocationCalc(as.vector(xyValues$Longitude),as.vector(xyValues$Latitude),gridRef,1) # the way this works is to find the closest point to the location which could be far away the larger the resolution. The A matrix can really be any scale (i.e. 1km grid) but takes a long time to read into R
		An <- A[as.character(ARowID),] #This search can also be more than one point, not tested on more than two locations but should work.
		
		if(input$ref == input$comp){
			outLabel <- "Same"
		} else{
			if(input$ref > input$comp){
				outLabel <- "Forwards"
			} else {
				outLabel <- "Backwards"
			}
		}
		
		climateVar2 <- 2
		outimage <- paste("AnalogIDW_BOLyn_", grid, "_", outLabel, "_", AName, "_", BName, "_Varset", climateVar2, sep="")
		
		# Principal component truncation rule
		truncSds <- 0.1 #truncation 
		
		#initiate the data frame to store the projected sigma dissimilarity of best analogs for each grid cell. 
		nnDist1 <- rep(NA,length(proxy[-nonCna,1]))
		nnDist2 <- rep(NA,length(proxy[-nonCna,1]))
		nnDist3 <- rep(NA,length(proxy[-nonCna,1]))
		nnDist4 <- rep(NA,length(proxy[-nonCna,1]))
		nnDist <- cbind(nnDist1,nnDist2,nnDist3,nnDist4)
		
		nnDist <- AnalogCalc(grid4nn = proxy, 
												 C.id = cId,
												 A = An, 
												 B = B,
												 C = C,
												 NN.dist = nnDist, 
												 non.CNA = nonCna, 
												 trunc.SDs = truncSds) # C and cID are loaded and defined in global.R
		nnChiProjIdwAve <- apply(cbind(nnDist, aGridDist[-nonCna,1:4]), 1, IDWMean) #apply inverse distance-weighted averaging function, otherwise you get "hard" lines between the climate stations
		
		# Create the raster output ----
		cat(file=stderr(), "CREATE RASTER \n")
		cat(file=stderr(), class(dem))
		out <- CreateRaster(dem,nonCnaDem,land,nnChiProjIdwAve,outimage)
		#shinyEventLogger::log_output(str(out))	
		return(out)
	})
	
	#MAP VIEWER ----
	
	# Create toggle for map with vs without model output ----
	showMap <- reactiveValues(
		view = "noModel"
	)
	
	# Trigger map to update to show model if "Run Model" hit
	observeEvent(input$run, {
		showMap$view <- "yesModel"
	})
	
	# renderLeaflet map ----
	output$locationMap <- renderLeaflet({
		cat(file=stderr(), "CREATE MAP \n")
		if(showMap$view == "yesModel"){
			pal <- colorBin(palette=c("#9e1e71", "#b55082", "#cc7e94", "#e0aea4", "#f5e3b5", "#ecedad", "#c8cc89", "#a4ab68", "#838c49", "#636e2b"), domain=c(0,100),	bins=c(0, 1, 1.5,2, 2.5,3, 3.5,4,4.5, 5, 100), na.color = "transparent")
			# pal <- colorBin(palette="magma", values(newRaster()),	bins=7, na.color = "transparent", reverse=TRUE)
			map <- baseMap %>%
				addRasterImage(newRaster(), colors = pal, group="Analog Model") %>%
				#addLegend(pal=pal, values = values(newRaster()), title = "Analog Values") %>%
				addLegend(pal=pal, values = 0:100, title = "Analog Values") %>%
				addCircleMarkers(lng=xyValues$Longitude, lat=xyValues$Latitude, color="red") %>%
				addLayersControl(baseGroups = c("OSM", "Esri World Imagery"), 
												 overlayGroups = c("Ecoregions", "Communities", "Analog Model"),
												 options = layersControlOptions(collapsed = FALSE))
			
		} else {
			map <- baseMap %>%
				addCircleMarkers(lng=xyValues$Longitude, lat=xyValues$Latitude, color="red") %>%
				addLayersControl(baseGroups = c("OSM", "Esri World Imagery"), 
												 overlayGroups = c("Ecoregions", "Communities"),
												 options = layersControlOptions(collapsed = FALSE))
		}
		map
	})
	
	# renderText mapLegend ----
	output$mapLegend <- renderText({
		if(showMap$view == "noModel"){
			legend <- ""
			
		} else {
			legend = "In the analog model color scale, low values (dark blue color) indicate similarity while high values (pale yellow) indicate difference.  For the calculations used here, values of 2 or less can be interpreted as analog climates."
		}
		legend
	})
	
	# TEST DATA ----
	output$test <- renderText({
		
	})
	
	# DOWNLOAD BUTTON ----
	output$ui_exportButton <- renderUI({
		if(showMap$view == "noModel"){return()}
		downloadButton('export', 'Download Model Raster')
	})


	output$export <- downloadHandler(
		filename = function() {
			"climateAnalog.tif"
		},
		content = function(con) {
			writeRaster(x=newRaster(), con)
		}
	)

	# RESET BUTTON ----
	# Trigger all user inputs to reset to default values if "Reset" hit.
	observeEvent(input$reset, {
		reset("allInputs")
		reset("allOutputs")
		xyValues$Longitude=-135.05
		xyValues$Latitude=60.716667
		showMap$view <- "noModel"
		reset("locationMap")
	})
	
})

