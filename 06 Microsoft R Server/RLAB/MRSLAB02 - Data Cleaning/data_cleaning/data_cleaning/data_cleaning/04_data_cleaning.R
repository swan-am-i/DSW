library(tm)

# grab the text from a book
conanDoyle <- "ConanDoyle.txt"
textFile <- scan(conanDoyle, what="character", sep="\n")
str(textFile)

# work out where the book starts and finishes.
limits <- local({
  limitTerms <- c("START OF THIS PROJECT GUTENBERG EBOOK", "END OF THIS PROJECT GUTENBERG EBOOK")
  sapply(limitTerms, grep, textFile, USE.NAMES=FALSE)  + c(1, -1)
})
limits

# put the relevent part of the book into a data.frame
textFile <- textFile[limits[1]:limits[2]]

# The transform function to remove parts of the text e.g. punctuation, stop words
tmTransform <- function(dataList) {
  x <- dataList$text
  x <- tolower(x)
  x <- removePunctuation(x)
  x <- removeNumbers(x)
  x <- removeWords(x, stopwords("english"))
  dataList$text <- x
  return(dataList)
}

# create an XDF object for output
cdXdf <- RxXdfData("ConanDoyle.xdf")


# run the transformation on each chunk
rxDataStep(inData = data.frame(text=textFile, stringsAsFactors=FALSE), 
           outFile = cdXdf,
           overwrite=TRUE,
           transformFunc = tmTransform,
           transformPackages = "tm",
           rowsPerRead=1000)

head(textFile)
head(cdXdf)