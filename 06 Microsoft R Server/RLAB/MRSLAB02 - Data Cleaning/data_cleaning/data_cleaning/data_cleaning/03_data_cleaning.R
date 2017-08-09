# An Example Computing Moving Averages

makeMoveAveTransFunc <- function(numDays = 10, varName = "", newVarName = "") {
    
    function(dataList) {
        numRowsToRead <- 0
        varForMoveAve <- 0
        # If no variable is named, use the first one sent
        # to the transformFunc
        if (is.null(varName) || nchar(varName) == 0) {
            varForMoveAve <- names(dataList)[1]
        } else {
            varForMoveAve <- varName
        }

        # Get the number of rows in the current chunk
        numRowsInChunk <- length(dataList[[varForMoveAve]])

        # .rxStartRow is the starting row number of the
        # chunk of data currently being processed
        # Read in previous data if we are not starting at row 1
        if (.rxStartRow > 1) {
            # Compute the number of lagged rows we'd like
            numRowsToRead <- numDays - 1
            # Check to see if enough data is available
            if (numRowsToRead >= .rxStartRow) {
                numRowsToRead <- .rxStartRow - 1
            }
            # Compute the starting row of previous data to read
            startRow <- .rxStartRow - numRowsToRead
            # Read previous rows from the .xdf file
            previousRowsDataList <- RevoScaleR::rxReadXdf(file = .rxReadFileName,
                                                     varsToKeep = names(dataList),
                                                     startRow = startRow, numRows = numRowsToRead,
                                                     returnDataFrame = FALSE)
            # Concatenate the previous rows with the existing rows
            dataList[[varForMoveAve]] <- c(previousRowsDataList[[varForMoveAve]],
                                            dataList[[varForMoveAve]])
        }
        # Create variable for simple moving average
        # It will be added as the last variable in the data list
        newVarIdx <- length(dataList) + 1
        # Initialize with NA's
        dataList[[newVarIdx]] <- rep(as.numeric(NA), times = numRowsInChunk)
        for (i in (numRowsToRead + 1):(numRowsInChunk + numRowsToRead)) {
            j <- i - numRowsToRead
            lowIdx <- i - numDays + 1
            if (lowIdx > 0 && ((lowIdx == 1) ||
                (j > 1 && (is.na(dataList[[newVarIdx]][j - 1]))))) {
                # If it's the first computation or the previous value
                # is missing, take the mean of all the relevant lagged data
                dataList[[newVarIdx]][j] <-
                mean(dataList[[varForMoveAve]][lowIdx:i])
            } else if (lowIdx > 1) {
                # Add and subtract from the last computation
                dataList[[newVarIdx]][j] <- dataList[[newVarIdx]][j - 1] -
                dataList[[varForMoveAve]][lowIdx - 1] / numDays +
                dataList[[varForMoveAve]][i] / numDays
            }
        }
        # Remove the extra rows we read in from the original variable
        dataList[[varForMoveAve]] <- dataList[[varForMoveAve]][(numRowsToRead + 1):(numRowsToRead + numRowsInChunk)]

        # Name the new variable
        if (is.null(newVarName) || (nchar(newVarName) == 0)) {
            # Use a default name if no name specified
            names(dataList)[newVarIdx] <- paste(varForMoveAve, "SMA", numDays, sep = ".")
        } else {
            names(dataList)[newVarIdx] <- newVarName
        }
        return(dataList)
    }
}

DJIAdaily <- file.path(rxGetOption("sampleDataDir"), "DJIAdaily.xdf")
rxLinePlot(Adj.Close + Adj.Close.SMA.360 ~ YearFrac, data = DJIAdaily,
           transformFunc = makeMoveAveTransFunc(numDays = 360),transformVars = c("Adj.Close"))