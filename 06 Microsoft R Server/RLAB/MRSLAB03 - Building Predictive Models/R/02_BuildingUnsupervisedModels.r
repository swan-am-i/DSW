rm(list=ls())
# replace the directory: Where did you store the csv file and xdf file
bigDataDir <- "data"
setwd(bigDataDir)
songs_15k <- RxXdfData("songs_15k.xdf")
rxGetInfo("songs_15k",getVarInfo = T)

# build 10 clusters
d# Since we have many variables, we build the forumla:
(avgs <- paste("timbre_avg",1:12,sep="_"))
covs <- paste("timbre_cov",1:78,sep="_")
(a <- paste(avgs, collapse=" + "))
(c <- paste(covs, collapse=" + "))
(form <- as.formula(paste(" ~ ",paste(a,c,sep=" + "))))

clust <- rxKmeans(form,data= songs_15k,numClusters = 9, algorithm = "lloyd", 
                  outFile = "song_clust", outColName = "Cluster", 
                  seed = 42,overwrite = TRUE)
song_clust <- RxXdfData( "song_clust.xdf" )
rxGetInfo(song_clust,getVarInfo = T)
# Create a factor for each interval of 10 years:
rxDataStep(inData = songs_15k, outFile = "decades.xdf",
           transforms = list(
             decade = cut(year,breaks = seq(1920,2010,10),labels=as.character(2:10))),
           overwrite = T)
rxGetInfo("decades.xdf",getVarInfo = T)

rxMerge(inData1="decades.xdf",inData2="song_clust",outFile="pairs",
        type="oneToOne",
        varsToKeep1 = "decade",varsToKeep2 = "Cluster", 
        overwrite = TRUE)
rxGetInfo("pairs",getVarInfo = T)
pairs <- RxXdfData( "pairs.xdf" )

clust_songs_tab <- rxCrossTabs(~decade:as.factor(Cluster), data = pairs)
print(clust_songs_tab,output="counts")

