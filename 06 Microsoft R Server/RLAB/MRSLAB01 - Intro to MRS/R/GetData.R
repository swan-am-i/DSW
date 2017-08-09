install.packages("curl")
library(curl)
baseurl <- "http://packages.revolutionanalytics.com/datasets/AirOnTimeCSV2012/"
numurl <- NULL

if (! dir.exists("data")) dir.create("data")

#getwd()

for (i in 1:12) {
  numurl[i] <- paste0("airOT2012",formatC(i, width = 2, flag = "0"),".csv")
  url <- paste0(baseurl,numurl[i])
  curl_download(url, file.path("data",numurl[i]))
}

inurl <- NULL
outurl <- NULL
appendFlag <- FALSE
outurl <- RxXdfData("airOT2012")
setwd("data")
for (i in 1:12) {
  inurl  <- RxTextData(paste0("airOT2012",formatC(i, width = 2, flag = "0"),".csv"),stringsAsFactors = TRUE)
  message("starting file ", inurl@file)
  rxImport(inData = inurl, outFile = outurl, append = appendFlag, overwrite = TRUE, rowsPerRead = 250000)
  append <- TRUE
  }
rxGetInfo(outurl)
rxGetVarInfo(outurl)


## ----airline-xdf, eval=FALSE---------------------------------------------
data.path <- getwd()
list.files(data.path)
airline.csv <- file.path(data.path, "2007.csv")
airline.xdf <- file.path("2007.xdf")

# Imports to XDF ~7.5m records for all flights in 2007
rxImport(inData = airline.csv, outFile = airline.xdf, overwrite = TRUE)

# Get head of the Xdf file - What will this do ?
head(airline.xdf)
# Define as an Rx Data Class
airline.xdf <- RxXdfData(airline.xdf)
# Now try
head(airline.xdf)
# ScaleR creates methodd for a number of commonly used R functions
methods(head)

## ----airline-info-1------------------------------------------------------
rxGetInfo(data=airline.xdf)


## ----airline-info-2, output.max=20---------------------------------------
rxGetVarInfo(data=airline.xdf)


## ----airline-info-3, output.max=20---------------------------------------
rxReadXdf(airline.xdf, numRows=3)


## ----airline-summary, output.max=20--------------------------------------
rxSummary(~ActualElapsedTime + AirTime + DepDelay + Distance, 
          data=airline.xdf)

summary(airline.xdf)

## ----airline-histogram---------------------------------------------------
rxHistogram(~DepDelay, data=airline.xdf)


## ----airline-histogram-advanced, tidy=FALSE------------------------------
rxHistogram(~DepDelay, data=airline.xdf, 
            xAxisMinMax=c(-100, 400), numBreaks=500, 
            xNumTicks=10)


## ----airline-transform-speed, tidy=FALSE---------------------------------
rxDataStep(inData=airline.xdf, 
           outFile=airline.xdf, 
           varsToKeep=c("AirTime", "Distance"), 
           transforms = list(AirSpeed = Distance / AirTime * 60), 
           append="cols", 
           overwrite=TRUE)


## ----airline-info-transform, output.max=20-------------------------------
rxGetInfo(data=airline.xdf, getVarInfo=TRUE)


## ----airline-info-speed--------------------------------------------------
rxGetInfo(data=airline.xdf, getVarInfo=TRUE, varsToKeep="AirSpeed") 


## ----airline-summary-speed, output.max=20--------------------------------
rxSummary(~AirSpeed, data=airline.xdf)


## ----airline-histogram-speed---------------------------------------------
rxHistogram(~AirSpeed, data=airline.xdf)


## ----airline-histogram-speed-subset, tidy=FALSE--------------------------
rxHistogram(~AirSpeed, data=airline.xdf, 
            rowSelection=(AirSpeed>50) & (AirSpeed<800), 
            numBreaks=5000,
            xNumTicks=20)


## ----airline-correlations------------------------------------------------
rxCor(formula=~DepDelay+ArrDelay+AirSpeed, data=airline.xdf)


## ----airline-correlation-subset, tidy=FALSE------------------------------
rxCor(formula=~DepDelay+ArrDelay+AirSpeed, data=airline.xdf,
      rowSelection=(AirSpeed>50) & (AirSpeed <800))


## ----airline-regression-speed, eval=TRUE, echo=TRUE----------------------
system.time({
  airline.model <- rxLinMod(formula=AirSpeed~DepDelay, data=airline.xdf, 
                            rowSelection=(AirSpeed>50) & (AirSpeed <800))
})


## ----airline-regression-output, size="tiny", output.max=30---------------
summary(airline.model)


## ----airline-datastep-factor-delay---------------------------------------
# create a factor variable for each 10 minutes of delay from -10 to 100 minutes
rxDataStep(inData = airline.xdf, 
           outFile = airline.xdf, 
           transforms = list(
             F_DepDelay = cut(DepDelay, breaks = seq(from = -10, to = 100, by = 10))
           ),
           append = "cols", 
           overwrite = TRUE)


## ----airline-factor-model, output.max=20---------------------------------
airline.factor.model <- rxLinMod(formula = AirSpeed ~ F_DepDelay, 
                                 data = airline.xdf, 
                                 rowSelection = (AirSpeed > 50) & (AirSpeed < 800))


## ----airline-factor-model-summary, output.max=20-------------------------
# model summary
summary(airline.factor.model)