#Remote Execution and Web Services for R Server
#help(mrsdeploy)

#****************************ENALBING WEBSERVICES PORT ACCESS OF YOUR VM
#add a rule to the NetworkInterface/Network security group:
#  +Add>Inbound security rule for port 12800 TCP protocol 
#(12800 is default after one-box configuration but can be changed).


#*********************************REMOTE EXECUTION AND DATA PREP
library(mrsdeploy)

# Remote Login, a prompt will show up to input user and pwd information
#replace hostanme with your VM's IP
endpoint <- "http://hostname:12800" #Your VM hostname or Public IP address

remoteLogin(endpoint, session = TRUE, diff = TRUE,commandline=TRUE)

#Load packages remotely
#If not already installed on the server install uncomment the code below and execute
#X <- c("stringr","plyr","lubridate","randomForest","reshape2","ggplot2")
#lapply(X, install.packages, character.only = T)
#lapply(X,library,character.only=T)


library(stringr)
library(plyr)
library(lubridate)
library(randomForest)
library(reshape2)
library(ggplot2)



pause()
getwd()
#Change your 'local_directory' to the output of getwd()
putLocalFile("local_directory/LoanStats3a.csv")

#Resume session on the server
resume()
#Check file is there
dir()
#Get exact directory path
getwd()
# Read from csv file and create your model remotely
#********************************************************************
#The below is a directory example , replace 'remote_diretory' with the output of the remote getwd()
loans <- read.csv("C:/Program Files/Microsoft/R Server/R_SERVER/DeployR/rserve/workdir/conn364/LoanStats3a.csv", h=T, stringsAsFactors=F, skip=1)

#take a peak...
head(loans,5)

#annoying column; just get rid of it
loans[,'desc'] <- NULL

summary(loans)
#almost all NA, so just get rid of it
loans[,'mths_since_last_record'] <- NULL

#get rid of fields that are mainly NA
poor_coverage <- sapply(loans, function(x) {
  coverage <- 1 - sum(is.na(x)) / length(x)
  coverage < 0.8
})
loans <- loans[,poor_coverage==FALSE]

bad_indicators <- c("Late (16-30 days)", "Late (31-120 days)", "Default", "Charged Off")

loans$is_bad <- ifelse(loans$loan_status %in% bad_indicators, 1,
                       ifelse(loans$loan_status=="", NA,0))

#Have a look at the loan status
table(loans$loan_status)
table(loans$is_bad)

head(loans,5)

#reformat dates and others
loans$issue_d <- as.Date(loans$issue_d,"%m/%d/%y")
loans$year_issued <- year(loans$issue_d)
loans$month_issued <- month(loans$issue_d)
loans$earliest_cr_line <- as.Date(loans$earliest_cr_line,"%m/%d/%y")
loans$revol_util <- str_replace_all(loans$revol_util, "[%]", "")
loans$revol_util <- as.numeric(loans$revol_util)
loans$int_rate<- as.numeric(loans$int_rate)


# Train the model
idx <- runif(nrow(loans)) > 0.75
train <- loans[idx == FALSE,]
names(train)
loanDataModel <- rxDForest(is_bad ~ revol_util + int_rate + annual_inc + total_rec_prncp , train)
pause()
snapshot<-createSnapshot("loanPredictSnapshot")
snapshot

# Scoring Function
loanPredictService <- function(revol_util, int_rate, annual_inc, total_rec_prncp) {
  inputData <- data.frame(revol_util = revol_util, int_rate = int_rate, annual_inc = annual_inc,  total_rec_prncp = total_rec_prncp)
  prediction <- rxPredict(loanDataModel, inputData)
  loanScore <- prediction$is_bad_Pred 
}
resume()
exit

# Remote Login, a prompt will show up to input user and pwd information
#Change the hostname to the VM IP
endpoint <- "http://hostname:12800" #Your VM hostname or Public IP address
remoteLogin(endpoint, session = FALSE, diff = FALSE)
#Please note, this remote session is set to false so no REMOTE prompt will show

# pseudo `unique` service name so no collision in example
service_name <- paste0("loanPredictService", round(as.numeric(Sys.time()), 0))

# Publish service
api <- publishService(
  service_name,
  code = loanPredictService,
  snapshot=snapshot,
  inputs = list(revol_util = 'numeric', int_rate = 'numeric', annual_inc = 'numeric', total_rec_prncp = 'numeric'),
  outputs = list(loanScore = 'numeric'),
  v = 'v2.0.0'
)


# Show API capabilities
api$capabilities()

#Consume the service/see execution time
system.time(loanScore <- api$loanPredictService(1, 1, 1, 1))

loanScore$output("loanScore")

#List all services
services <- listServices()
services

#Generate swagger json file
#Change remote_directory to your getwd() on your remote directory
cat(api$swagger(), file = "remote_directory/loanPredict.json")

#Logout
remoteLogout()

