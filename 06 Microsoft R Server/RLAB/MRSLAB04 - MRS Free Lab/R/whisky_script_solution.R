# Data import
txt <- RxTextData("whiskies.txt", delimiter=",", varsToDrop = "RowID")
whiskies <- RxXdfData("whiskies.xdf")
rxDataStep(txt,whiskies, overwrite = TRUE)

# Scale data --------------------------------------------------------------

scaleFunction <- function(lst) 
{
    mydf <- as.data.frame(lst)
    sk <- matrix(data = 0, nrow = NROW(mydf), ncol=NCOL(mydf))
    for(i in 1 : NCOL(mydf))
    {
        sk[,i] <- (mydf[,i]-centers[i])/scales[i]
    }
    
    sk <- data.frame(sk)
    names(sk) <- names(mydf)
    return(as.list(sk))
}

whiskies <- RxXdfData("whiskies.xdf",varsToKeep = names(whiskies)[2:13])
whiskies_scaled <- RxXdfData("whiskies_scaled.xdf")
xx <- rxSummary(~.,whiskies)$sDataFrame # we need mean and sd computed over entire dataset

rxDataStep(inData = whiskies,
           outFile = whiskies_scaled,
           varsToKeep = names(whiskies)[2:13],
           transformFunc = scaleFunction,
           transformObjects = list(centers = xx$Mean,scales = xx$StdDev),
           overwrite = TRUE)

# Sum within squares error ------------------------------------------------

ssPlot <- function(data, maxCluster = 9) 
{
    f <- formula(paste("~",paste(names(data), collapse = "+")))
    # Initialize within sum of squares
    z <- rxSummary(~.,data, reportProgress = 0)
    SSw <- double(maxCluster)
    SSw[1] <- (nrow(data) - 1) * sum(z$sDataFrame$StdDev^2)
    for (i in 2:maxCluster) 
    {
        SSw[i] <- sum(rxKmeans(f, data, numClusters = i, seed=1, reportProgress = 0)$withinss)
    }
    
    plot(1:maxCluster, SSw, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")
}

ssPlot(whiskies_scaled)

# Set up output file ------------------------------------------------------

fitFileName <- "cluster_fit.xdf"
clusterFit <- RxXdfData(fitFileName)
if(file.exists(fitFileName))
    file.remove(fitFileName)

# Run Kmeans --------------------------------------------------------------

f <- formula(paste("~",paste(names(whiskies_scaled), collapse = "+")))
fit <- rxKmeans(f, outFile = clusterFit,
                data = whiskies_scaled, numClusters = 4, 
                seed=1, outColName = "fitCluster")

# append cluster label to original data -----------------------------------

whiskies <- RxXdfData("whiskies.xdf")
rxDataStep(whiskies, outFile=clusterFit, append="cols")



# View cluster 2 ----------------------------------------------------------

# Laphroig (my favourite) sits in cluster 2. What other brands may I like?
View(rxDataStep(clusterFit, rowSelection=(fitCluster==2)))

        
