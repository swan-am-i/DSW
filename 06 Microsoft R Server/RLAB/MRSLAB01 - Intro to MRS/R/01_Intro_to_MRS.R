## MRS LAB 01
# Analysing US Flight Departure Data
# Big Data Analytics with Microsoft R Server
# Intro to Microsoft R Server (MRS) functionality
#-------------------------------------------------------------------------------
# Version 1.0.   Author: Simon Field
#-------------------------------------------------------------------------------
## 

# Just as options() provides a set of parameters/defaults used by R
options()
# the rxOptions() function provides a set of additional parameters/defaults 
# used by MRS
rxOptions()
# NumCoresToUse defines the number of cores that MRS functions will use by 
# default to process parallelisable functions.  Defaults to total number of
# cores in the system.
rxOptions()[["numCoresToUse"]]


#  Microsoft R Server ships with some sampleData for exploration using code from
# help files, product documentation
sampleDataDir <- rxOptions()[["sampleDataDir"]]
list.files(sampleDataDir)
# Note: there are a mix of supported file-types provided

list.files(rxOptions()[["demoScriptsDir"]])
# Note: there are a mix of demo and installation test (IOQ) tests provided that
#       use this sample data

# The package containing the main MRS user functionality is RevoScaleR.
?RevoScaleR
# Note: Scroll down the help file to get an overview of the kind of functions 
#       available by area/use.
# Additional packages included in MRS : RevoIOQ, revolpe, RevoMods, RevoPemaR
# RevoRpeConnector, RevoRsrConnector, RevoScaleR, RevoTreeView, RevoUtils,
# RevoUtilsMath, doRSR

################################################################################
# Lets Explore some of the functionality through an example using US airline 
# flight data.  We will explore the data and ascertain if delayed flights on
# fly faster !
################################################################################

# Configure Data and Working Directories
projectDir <- getwd()
if(dir.exists(file.path(projectDir, "data"))==FALSE)
  {dir.create(file.path(projectDir, "data"))}

dataRead <- file.path(projectDir,"data")
dataWrite <- dataRead

# What would happen if we didn't use RevoScaleR "rx" functions and attempted to 
# run all of this in memory?   Go ahead and give it a shot - using only open 
# source R functions run a regression of air speed on departure delays on a
# subset with airSpeed between 50 and 800. If you don't have enough RAM you
# might not be able to work with the entire data set.
# 
## What would happen if we did this in memory?
## read the data into memory (limit to a couple of variables...)
## Which columns should we read in?

list.files(dataRead)
airlineCsv <- file.path(dataRead,"2007.csv")

## define a colInfo vector
myColInfo = rep("NULL", 29)
myColInfo[c(12, 14, 15, 16, 19)] <- "numeric"

# Build a data frame in memory for analysis
airlineDF <- read.table(file=airlineCsv, header = TRUE, sep = ",",
                        colClasses=myColInfo)
object.size(airlineDF)
# Note: The dataframe for 2007 year and a subset of columns is ~300MB.
# There is total of 25+ years available and 30+ columns in the total dataset.

dim(airlineDF)
str(airlineDF)

#  Summarise the data
summary(airlineDF)

# Create a new feature for airspeed
airlineDF$airSpeed <- airlineDF$Distance / airlineDF$AirTime * 60
head(airlineDF)

# Plot
hist(airlineDF$airSpeed,breaks = 100)

# Build a dataframe with the outliers removed 
airlineDF2 <- airlineDF[(airlineDF$airSpeed > 50) & (airlineDF$airSpeed < 800),]
dim(airlineDF2)
# make some room
rm(airlineDF)
gc()
hist(airlineDF2$airSpeed,breaks = 100)

system.time(reg3 <- lm(formula=airSpeed~DepDelay, data=airlineDF2))
summary(reg3)

################################################################################
# Now let's look at how we can achieve the same thing with MRS - RevoScaleR
################################################################################

# Redefine airlineCsv as a RxTextData object
airlineCsv <- RxTextData(file.path(dataRead,"2007.csv"))

# We can now run some basic R commands on it through functionality included in
# RevoScaleR
head(airlineCsv)

# Or MRS functions
rxGetInfo(data=airlineCsv)
rxGetVarInfo(data=airlineCsv)
rxGetInfo(data=airlineCsv, getVarInfo = TRUE, computeInfo = TRUE, numRows = 5 )

# All Microsoft R Server scalable functions can operate over text and other
#  external file formats. e.g. ODBC/SQL, SAS etc. 
# We also have the MRS External Data Frame (XDF) file format which is a 
# column-compressed, block-based R binary format created by Revolution to 
# optimise performance working with disk based datasets.

# We will import the data into XDF format
airlineXdf <- RxXdfData(file.path(dataWrite,"2007.xdf"))

# Import the data into xdf format for further exploratory analysis.
# We will time this one off import process
system.time(
  rxImport(inData=airlineCsv, outFile = airlineXdf, 
           overwrite = TRUE)
)

# Get some information about the file and data
rxGetInfo(airlineXdf)
# Get some information about the columns and data in the xdf 
rxGetVarInfo(data=airlineXdf)

object.size(airlineXdf)
# Note: the data is not stored in memory.  The airlineXdf is an object 
# (RxXdfData).
file.size(file.path(dataWrite,"2007.xdf"))
file.size(file.path(dataWrite,"2007.csv"))
# The XDF file is giving us approx 5x saving in space and I/O

# Best Practices for Import
# -------------------------
# Import performance will vary depending on the data types
# Character data is more "expensive" to import.
# Importing categorical data can be accelerated by providing colInfo describing
# the categorical levels in the data
# Aim for approx 10M cells per block (rows x columns)

# Let's define a colinfo clause to define factors and some data types

airlineColInfo <- list(
  Year = list(newName = "Year", type = "integer"),
  Month = list(newName = "Month", type = "factor",
              levels = as.character(1:12),
              newLevels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
  DayOfWeek = list(newName = "DayOfWeek", type = "factor",
                   levels = as.character(1:7),
                   newLevels = c("Mon", "Tues", "Wed", "Thur", "Fri", "Sat",
                                 "Sun")),
  UniqueCarrier = list(newName = "UniqueCarrier", type =
                         "factor"),
  TailNum = list(newName = "TailNum", type = "factor"),
  Origin = list(newName = "Origin", type = "factor"),
  Dest = list(newName = "Dest", type = "factor")
)

airlineCsv <- RxTextData(file.path(dataRead,"2007.csv"),
                         colInfo = airlineColInfo)

blockSize = 250000

system.time(
  rxImport(inData=airlineCsv, outFile = airlineXdf, 
           overwrite = TRUE, colInfo = airlineColInfo,
           rowsPerRead = blockSize,
           numRows = 100000)
)
# Get some information about the file and data
rxGetInfo(airlineXdf)
# Get some information about the columns and data in the xdf 
rxGetVarInfo(data=airlineXdf)


# We can also perform on the fly data transformations as we import and
# calculate new features. For this we will create a helper functions
ConvertToDecimalTime <- function( tm ){(tm %/% 100) + (tm %% 100)/60}

system.time(
  rxImport(inData=airlineCsv, outFile = airlineXdf, 
           overwrite = TRUE, colInfo = airlineColInfo,
           transforms = list(
             FlightDate = as.Date(as.character((as.numeric(Year)*10000)
                                  +(as.numeric(Month)*100)
                                  +as.integer((
                                    formatC(as.numeric(DayOfWeek), 
                                            width = 2, flag = "0")))),
                                  format = "%Y%m%d"),
             CRSDepTime = ConvertToDecimalTimeFn(CRSDepTime),
             DepTime = ConvertToDecimalTimeFn(DepTime),
             CRSArrTime = ConvertToDecimalTimeFn(CRSArrTime),
             ArrTime = ConvertToDecimalTimeFn(ArrTime),
             MonthsSince198710 = as.integer((as.numeric(Year)-1987)*12 + as.numeric(Month) - 10),
             DaysSince19871001 = as.integer(FlightDate - as.Date("1987-10-01", format = "%Y-%m-%d"))),
           transformObjects = list(ConvertToDecimalTimeFn = ConvertToDecimalTime),
           rowsPerRead = blockSize
  )
)
# Get some information about the file and data
rxGetInfo(airlineXdf)
# Note: the file blocks are compressed, and the data is stored in 15 blocks
# based on the default block size of 100000 rows per block.  Aim for
# each block to be circa 10M cells in size. cells = rows x columns

# Get some information about the columns and data in the xdf 
rxGetVarInfo(data=airlineXdf)
# Note: MRS pre-computes and stored the low/high values for numeric data which
# can be useful in optimising other processing.  Categorical data is stored as
# integer "levels" with metadata referencing the factor values.

# MRS provides methods for the following functions for supported data-sources
head(airlineXdf)
tail(airlineXdf)
names(airlineXdf)
dim(airlineXdf)
dimnames(airlineXdf)
nrow(airlineXdf)
ncol(airlineXdf)

# Read and display some rows from the file
rxReadXdf(airlineXdf, numRows=3)

# We can read from different points in the file
startPoint <- nrow(airlineXdf) - 5
rxReadXdf(airlineXdf, startRow=startPoint)

rxReadXdf(airlineXdf, startRow = 3983467, numRows=3) 

# Compute descriptive statistics using rxSummary()
rxSummary(~ActualElapsedTime + AirTime + DepDelay + Distance,
          data=airlineXdf)
# Note: MRS uses R's formula interface to enable a subset of columns
# to be analysed.
# For all numeric and factor columns you can use 
rxSummary(~., airlineXdf,reportProgress = 1)
# or MRS provides a method for summary that does the same thing
summary(airlineXdf)

# Plot some histograms of the data
rxHistogram(~DepDelay, data=airlineXdf, reportProgress = 1 )

# We can remove outliers by restricting the x Axis min and max
rxHistogram(~DepDelay, data=airlineXdf, reportProgress = 1, 
            xAxisMinMax=c(-100, 400), numBreaks=500,
            xNumTicks=10)

# We can see if the distribution is different by day of week
rxHistogram(~DepDelay | DayOfWeek, data=airlineXdf, reportProgress = 1, 
            xAxisMinMax=c(-100, 400), numBreaks=500,
            xNumTicks=10)

# To determine if delayed flights fly faster we will need the airspeed.
# Add a new feature (derived column) to the data
# rxDataStep processes data in blocks and applies the transforms to each block
rxDataStep(inData=airlineXdf,
           outFile=airlineXdf,
           varsToKeep=c("AirTime", "Distance"),
           transforms = list(AirSpeed = Distance / AirTime * 60),
           append="cols",
           overwrite=TRUE,
           reportProgress = 1)

# Check the new column is there. Couple of ways...
rxGetInfo(data=airlineXdf, getVarInfo=TRUE)
rxGetInfo(data=airlineXdf, getVarInfo=TRUE, varsToKeep="AirSpeed")

# Get summary information and histogram for AirSpeed
rxSummary(~AirSpeed, data=airlineXdf)
rxHistogram(~AirSpeed, data=airlineXdf)
# Clearly we have some outliers and caused by data quality issues

# Lets use the rowSelection argument to remove the obvious outliers!
rxHistogram(~AirSpeed, data=airlineXdf,
            rowSelection=(AirSpeed>50) & (AirSpeed<800),
            numBreaks=5000,
            xNumTicks=20,
            reportProgress = 1)

# Is there an obvious correlation between arrival and departure delay and airpspeed
rxCor(formula=~DepDelay+ArrDelay+AirSpeed, data=airlineXdf)
# What if we move the outliers ?
blah <- rxCor(formula=~DepDelay+ArrDelay+AirSpeed, data=airlineXdf,
      rowSelection=(AirSpeed>50) & (AirSpeed <800),
      reportProgress = 1)
str(blah)

# We can use open-source corrplot function to plot our correlation matrix

library(corrplot)
corrplot(blah, method="circle", type = c("upper"))
# As might be expected Arrival Delay is highly correlated to Departure Delay
# but much less correlated to Airspeed.  Nevertheless 
# lets create a linear regression model for airSpeed by Departure Delay
system.time({
  airline.model <- rxLinMod(formula=AirSpeed~DepDelay, data=airlineXdf,
                            rowSelection=(AirSpeed>50) & (AirSpeed <800),
                            reportProgress = 1)
})
airline.model
summary(airline.model)

# CONCLUSION
# Indeed! When you are late to leave, the plane flies faster
# Statistically significant faster
# But the coefficient is very low - 0.0439 - for each minute of delay
# A staggering 2.6336 mph per each hour of delay!
#
# Maybe the relationship is not monotonic?
# - When delay is small, it can be absorbed by the airlines safety margins
# - When delays are big, flying faster won't help
# - Maybe there is a range in between where it matters?
# Lets see

# create a factor variable for each 10 minutes of delay from -10 to 100 minutes
rxDataStep(inData = airlineXdf,
           outFile = airlineXdf,
           transforms = list(
             F_DepDelay = cut(DepDelay, breaks = seq(from = -10, to = 100, by = 10))
           ),
           append = "cols",
           overwrite = TRUE,
           reportProgress = 1)

# Create a linear regression factor model
airFactorMod <- rxLinMod(formula = AirSpeed ~ F_DepDelay + -1,
                                 data = airlineXdf,
                                 rowSelection = (AirSpeed > 50) & (AirSpeed < 800),
                         cube = TRUE,
                         cubePredictions = TRUE,
                         reportProgress = 1)

# model summary
summary(airFactorMod)
# Let's examine the structure of the model object
str(airFactorMod)

# We can extract the coefficients of the model to plot them
plotDF <- data.frame(seq(from = -10, to = 90, by = 10),airFactorMod$coefficients)
rownames(plotDF) <- NULL
names(plotDF)[1] <- "DepDelayRange"
rxLinePlot(AirSpeed ~ DepDelayRange,data = plotDF)
