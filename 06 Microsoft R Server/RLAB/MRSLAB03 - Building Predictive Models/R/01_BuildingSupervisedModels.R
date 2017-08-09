####################################################################
#
# Optionally: clean the workspace
rm(list=ls())
# replace the directory: Where did you store the data files, 
# e.g.; bigdatadir <- "data"
bigdatadir <- "data"
setwd(bigdatadir)
songs_15k <- RxXdfData("songs_15k.xdf")
rxGetInfo(songs_15k, getVarInfo = TRUE)
rxSummary(~ year10, 
          data=songs_15k,
          summaryStats=c("validObs"),
          transforms=list(year10 = cut(year,seq(1920,2010,10))))

# A first look into the data: Build a decision tree and look which variable influences the year. 
# to do this we need a formula. Since we have a lot of variables, we will create it programmatically
(avgs <- paste("timbre_avg",1:12,sep="_"))
covs <- paste("timbre_cov",1:78,sep="_")
(a <- paste(avgs, collapse=" + "))
c <- paste(covs, collapse=" + ")
(form1 <- as.formula(paste("year ~ ",paste(a,c,sep=" + "))))
tree <- rxDTree(form1,data=songs_15k,maxDepth = 3)
tree
# So we see that timbre_avg_6, avg_2 and avg_1 are the most influential variables

# Try prediction of the year of the song....
# Split into test and training set. Keep 20% for test data
# First create a random variable from 1 to 10 and select afterwards
rxDataStep(inData=songs_15k,
           outFile = "songs.xdf",
           transforms=list(splitvar = as.integer(runif(.rxNumRows,1,11))),
           overwrite=TRUE)
rxGetInfo("songs.xdf",getVarInfo = TRUE)
# Create training and test set
rxDataStep(inData="songs.xdf",
           outFile="train_15k",
           rowSelection = (splitvar < 9),
           varsToDrop = "splitvar",
           overwrite=TRUE)

rxDataStep(inData="songs.xdf",
           outFile="test_15k",
           rowSelection = (splitvar > 8),
           varsToDrop = "splitvar",
           overwrite=TRUE)

# try to approaches: Linear model and random forest to predict year
# Create a linear regression model
year_lin <- rxLinMod(form1, data = "train_15k")
year_lin

# predict
rxPredict(modelObject=year_lin,
          data="test_15k",
          outData="lin_test",
          predVarNames="Y_Pred",
          overwrite = TRUE)

rxMerge(inData1="test_15k",inData2="lin_test",outFile="test_r2",
        type="oneToOne",
        varsToKeep1 = "year",varsToKeep2 = "Y_Pred", 
        overwrite = TRUE)
rxGetInfo("test_r2",getVarInfo=TRUE,numRows=5)

# Calculate the errors (absolute and squared)
rxDataStep(inData="test_r2",
           outFile="reglin_error",
           transforms=list(
             abs_err = abs(Y_Pred-year),
             sq_err = (Y_Pred-year)^2
           ),
           varsToKeep = c("Y_Pred","year"),
           overwrite=TRUE)

rxGetInfo("reglin_error",getVarInfo=TRUE,numRows=5)

rxSummary(~ abs_err+sq_err, data="reglin_error",summaryStats=c("mean"))

# compare the mean squared error to the standard deviation (square it to compare)
rxSummary(~ year,data="test_15k")
# so we have a StdDev of 19 compared to a root mean squared error of 14. Better than guessing!


# Alternative: Random Forest -------------------------------------------------------------------
# Create a random Forest Model
# 20 trees and maxDepth = 3 are way too low! You just get an impression how it works
# Expect 5 minutes run-time, dependent on your ssd. 
year_regf <- rxDForest(form1, data = "train_15k", maxDepth = 3,nTree = 20)
rxPredict(modelObject=year_regf,
          data="test_15k",
          outData="regf_test",
          predVarNames="Y_Pred",
          overwrite = TRUE)

rxMerge(inData1="test_15k",inData2="regf_test",outFile="test_r2",
        type="oneToOne",
        varsToKeep1 = "year",varsToKeep2 = "Y_Pred", 
        overwrite = TRUE)
rxGetInfo("test_r2",getVarInfo=TRUE,numRows=5)

# Calculate the errors (absolute and squared)
rxDataStep(inData="test_r2",
           outFile="regregf_error",
           transforms=list(
             abs_err = abs(Y_Pred-year),
             sq_err = (Y_Pred-year)^2
           ),
           varsToKeep = c("Y_Pred","year"),
           overwrite=TRUE)

rxGetInfo("regregf_error",getVarInfo=TRUE,numRows=5)

rxSummary(~ abs_err+sq_err, data="regregf_error",summaryStats=c("mean"))

# compare the mean squared error to the standard deviation (square it to compare)
rxSummary(~ year,data="test_15k")
# so we have a StdDev of 19 compared to a root mean squared error of 16. Better than guessing!

#---------------------------------------------------------------------------------------------------
#
# Large Data Set
infile_csv <- "YearPredictionMSD.txt"

# Data preparation: --------------------------------------------
# Create xdf from csv. 

# Select the first 463715 rows as train, the last ones as test
# The file is structured, that in that way, we do not have the same artist in train and test. 
rxTextToXdf(inFile = infile_csv, outFile = "train.xdf",  stringsAsFactors = F, rowsPerRead = 50000, overwrite = T,numRows = 463715)
rxTextToXdf(inFile = infile_csv, outFile = "test.xdf",  stringsAsFactors = F, rowsPerRead = 10000, overwrite = T,rowsToSkip = 463715)
test <- RxXdfData( "test.xdf" )
train <- RxXdfData("train.xdf")
rxGetInfo(data = "train.xdf", getVarInfo = TRUE)
rxGetInfo(data = "test.xdf", getVarInfo = TRUE)

# rename the columns (may there is a simpler method in rx)
# First create column info: year, 12 timbre averages, 78 timbre covariances
(avgs <- paste("timbre_avg",1:12,sep="_"))
covs <- paste("timbre_cov",1:78,sep="_")
new_names <- c("year",avgs,covs)
names(test) <- new_names
names(train) <- new_names

# Look onto the data
rxGetInfo(data = "train.xdf", getVarInfo = TRUE)
rxGetInfo(data = "test.xdf", getVarInfo = TRUE)

# A histogram of the years
rxHistogram(~year,data=train)
rxHistogram(~year,data=test)
(quantiles <- rxQuantile(varName="year",data=train,probs=seq(0,1.0,0.1)))

# We are interested if we can predict if a song is before or after 2000
# We create a variable "new_millenium
rxDataStep(inData="train",
           outFile="train_millenium",
           transforms=list(new_millenium = (year > 1999)),
           overwrite=TRUE)
rxGetInfo(data="train_millenium",getVarInfo = TRUE)

rxSummary(~ new_millenium, data="train_millenium")

rxDataStep(inData="test",
           outFile="test_millenium",
           transforms=list(new_millenium = (year > 1999)),
           overwrite=TRUE)
rxGetInfo(data="test_millenium",getVarInfo = TRUE)

rxSummary(~ new_millenium, data="test_millenium")

# Build a boosted decision tree model from the training data set. 
# To save time, we limit nTree to 10. Larger nTree give better results!
# Expect 3 minutes with an SSD-based laptop
tmill <- RxXdfData("train_millenium")
form2 <- formula(tmill, depVar = "new_millenium")

bt_model <- rxBTrees(form2, data = "train_millenium",nTree = 10)
summary(bt_model)
plot(bt_model)
# predict
rxPredict(modelObject=bt_model,
          data="test_millenium",
          predVarNames="Y_Prob",
          overwrite = TRUE)
rxGetInfo("test_millenium",getVarInfo=TRUE)

# ROC curve
ROC <-rxRoc(actualVarName = "new_millenium", 
            predVarNames = "Y_Prob",
            data = "test_millenium")
rxAuc(ROC)      # Compute the AUC

rxRocCurve (actualVarName = "new_millenium", 
            predVarNames = "Y_Prob",
            data = "test_millenium",
            title ="ROC rxBTrees Model of Millenium Songs")

# Exercise 2: Do the same with a general linear model! It will perform better!
# Hint: use rxGlm
# Build a general linear model from the training data set. 
glm_model <- rxGlm(form2, data = "train_millenium")
summary(glm_model)
plot(glm_model)
# predict
rxPredict(modelObject=glm_model,
          data="test_millenium",
          predVarNames="Y_Prob",
          overwrite = TRUE)
rxGetInfo("test_millenium",getVarInfo=TRUE)

# ROC curve
ROC <-rxRoc(actualVarName = "new_millenium", 
            predVarNames = "Y_Prob",
            data = "test_millenium")
rxAuc(ROC)      # Compute the AUC

rxRocCurve (actualVarName = "new_millenium", 
            predVarNames = "Y_Prob",
            data = "test_millenium",
            title ="ROC rxGlm Model of Millenium Songs")

# ---------------------------------------------------------------