# set up a RxTextData object 
txt <- RxTextData("TextFileForExercise.txt", delimiter = "|")
xdf <- RxXdfData("exerciseOutput.xdf")

clean_text <- function(lst) {
    x <- data.frame(lst, stringsAsFactors = FALSE)
    ns <- strsplit(x[,1], " ")
    x$FirstName <- unlist(lapply(ns, function(x) return(x[1])))
    x$Surname <- unlist(lapply(ns, function(x) return(x[2])))
    x$NewDate <- as.Date(x$Date, "%m/%d/%Y")

    return(x)
}

rxDataStep(inData = txt,
           outFile = xdf,
           transformFunc = clean_text,
           transformPackages = "tm",
           overwrite = TRUE)
