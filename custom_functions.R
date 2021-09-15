# Yukon Climate Analog Explorer
# designed by Nadele Flynn (nadele@ualberta.ca)
# built by Ashton Drew (ashton.drew@kdv-decisions.com) with assistance form Eliot Dixon (eliot.dixon@kdv-decisions.com)
# Fall 2020

# These custom functions were adapted by Nadele Flynn, University of Alberta, Renewable Resources, nadele@ualberta.ca  to include IDW and single location selection as well as user inputs.

# Colin Mahony
# Centre for Forest Conservation Genetics and Department of Forest and Conservation Sciences, 
# University of British Columbia, 3041-2424 Main Mall, Vancouver, British Columbia, Canada V6T1Z4
# c_mahony@alumni.ubc.ca

###Function get location of closest point to the entered value
LocationCalc <- function(long,lat,grid.ref2,kvalue) #Calc.Type = 1 is with lat lon id2 is rowid
{
	#lat <- 62.365165 
	#long <- -133.466754
	#kvalue <- 1
	get.loc <- get.knnx(data=grid.ref2[,3:4],query=as.data.frame(cbind(lat,long)),k=kvalue,algorithm="brute") #k value is normally 1 but can be larger.
	RowID <- as.vector(get.loc$nn.index[1,])
	i <- slice(grid.ref2,RowID)$id2
	return(i)
}

####Main function - calculate the analog####################
AnalogCalc <- function(grid4nn, A, B, C, NN.dist, non.CNA, C.id, trunc.SDs) #Run the novelty calculation
{ 
	###grid4nn <- proxy #this is the only unique variable in the function
	###proxy.i <- 1 # just to run out of the function
	###j <- 42 #test
	for (proxy.i in 1:4) 
	{
		#j <- ((grid4nn[,proxy.i]))
		proxy <- ((grid4nn[,proxy.i])[-non.CNA])
		for(j in sort(unique(proxy))){       # run the novelty calculation once for each ICV proxy. Takes about 1.5 sec/iteration (1 hour total) on a typical laptop. 
			## Select data relevant to ICV proxy j
			#Bj <- B
			Bj <- B[which(proxy==j),]   # select locations for which ICV proxy j is the closest ICV proxy. 
			Cj <- C[which(C.id==j),] # reference period (1951-1990) ICV at ICV proxy j
			
			## Step 1: express climate data as standardized anomalies of reference period (1951-1990) ICV at ICV proxy j. 
			Cj.sd <- apply(Cj,2,sd, na.rm=T)  #standard deviation of 1951-1990 interannual variability in each climate variable, ignoring missing years
			A.prime <- sweep(A,MARGIN=2,Cj.sd,`/`) #standardize the reference ICV    
			Bj.prime <- sweep(Bj,MARGIN=2,Cj.sd,`/`) #standardize the analog pool    
			Cj.prime <- sweep(Cj,MARGIN=2,Cj.sd,`/`) #standardize the projected future conditions of grid cells represented by ICV proxy j
			
			## Step 2: Extract the principal components (PCs) of the reference period ICV and project all data onto these PCs
			PCA <- prcomp(Cj.prime[!is.na(apply(Cj.prime,1,mean)),])   #Principal components analysis. The !is.na(apply(...)) term is there simply to select all years with complete observations in all variables. 
			PCs <- max(which(unlist(summary(PCA)[1])>trunc.SDs))    # find the number of PCs to retain using the PC truncation rule of eigenvector stdev > the truncation threshold
			X<- as.data.frame(predict(PCA,A.prime))   #project the analog pool onto the PCs
			Yj<- as.data.frame(predict(PCA,Bj.prime)) #project the projected future conditions onto the PCs
			Zj<- as.data.frame(predict(PCA,Cj.prime)) #project the reference ICV onto the PCs
			
			## Step 3a: express PC scores as standardized anomalies of reference interannual variability 
			Zj.sd <- apply(Zj,2,sd, na.rm=T)     #standard deviation of 1951-1990 interannual variability in each principal component, ignoring missing years
			X.prime <- sweep(X,MARGIN=2,Zj.sd,`/`) #standardize the analog pool    
			Yj.prime <- sweep(Yj,MARGIN=2,Zj.sd,`/`) #standardize the projected conditions   
			
			## Step 3b: find the sigma dissimilarity of each projected condition with its best analog (Euclidean nearest neighbour) in the observed analog pool. 
			NN.dist[which(proxy==j),proxy.i] <- as.vector(get.knnx(data=X.prime[,1:PCs],query=Yj.prime[,1:PCs],k=1,algorithm="brute")[[2]]) # Euclidean nearest neighbour distance in the z-standardized PCs of interannual climatic variability, i.e. the Mahalanobian nearest neighbour. 
		}
	}
	return(NN.dist)
} 

CreateRaster <- function(dem, non.CNADem,land, NN.dist, outimage){
	X <- dem
	values(X) <- NA
	values(X)[non.CNADem][land] <- NN.dist
	return(X)
}

IDWMean <- function(x){IDWAvg <- sum(x[1:(length(x)/2)]/x[(length(x)/2+1):length(x)])/sum(1/x[(length(x)/2+1):length(x)]); return(IDWAvg)} 

