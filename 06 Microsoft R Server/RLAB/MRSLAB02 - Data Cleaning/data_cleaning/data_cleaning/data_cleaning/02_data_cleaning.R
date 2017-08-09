## set up pointer to xdf mortgage data
mortgages <- RxXdfData("mortgages.xdf")
head(mortgages)

# get the min and max credit scores
minScore <- rxGetVarInfo(mortgages)[["creditScore"]][["low"]]
maxScore <- rxGetVarInfo(mortgages)[["creditScore"]][["high"]]

# transform function to normalize
simply_normalize <- function(lst) {
    lst[["normCreditScore"]] <- as.numeric(
    (lst[["creditScore"]] - minCreditScore) / (maxCreditScore - minCreditScore))
    lst[["moreThanHalf"]] <- lst[["normCreditScore"]] > 0.5
    return(lst)
}

# run the transform function on each block in the datastep
rxDataStep(inData = mortgages,
           outFile = mortgages,
           transformFunc = simply_normalize,
           transformObjects = list(minCreditScore = minScore,
           maxCreditScore = maxScore),
           append = "cols",
           overwrite = TRUE)

# check the new variables
rxGetVarInfo(data = mortgages)

## THE WRONG WAY!! ###
wrong_normalization <- function(lst) {
    minCreditScore_bad <- min(lst[["creditScore"]], na.rm = TRUE)
    maxCreditScore_bad <- max(lst[["creditScore"]], na.rm = TRUE)
    lst[["normCreditScore2"]] <- (lst[["creditScore"]] - minCreditScore_bad) /
    (maxCreditScore_bad - minCreditScore_bad)
    lst[["normDiff"]] <- lst[["normCreditScore2"]] - lst[["normCreditScore"]]
    return(lst)
}

rxDataStep(inData = mortgages,
           outFile = mortgages,
           transformFunc = wrong_normalization,
           overwrite = TRUE)

rxGetVarInfo(mortgages)
