#Remote Execution and Web Services for R Server
#help(mrsdeploy)

#****************************ENALBING WEBSERVICES PORT ACCESS OF YOUR VM
#add a rule to the NetworkInterface/Network security group:
#  +Add>Inbound security rule for port 12800 TCP protocol 
#(12800 is default after one-box configuration but can be changed).

#*********************************REMOTE EXECUTION AND DATA PREP
library(mrsdeploy)

# Remote Login, a prompt will show up to input user and pwd information
#Change hostname to the public IP on your VM

endpoint <- "http://hostname:12800" #Your VM hostname or Public IP address

remoteLogin(endpoint, session = TRUE, diff = TRUE,commandline=TRUE)


#******************************************TRANSFERRING FILES FROM LOCAL
#************************************************************

#create iris.csv in remote environment
write.csv(iris,file = "iris.csv")

#Other commands
listRemoteFiles()
deleteRemoteFile("iris.csv")
listRemoteFiles()

#create iris.csv locally
write.csv(iris,file = "iris.csv")
#Check file is there
dir()

#Transfer files form local environment to server
putLocalFile("iris.csv")

#Other commands
listRemoteFiles()
deleteRemoteFile("iris.csv")
listRemoteFiles()


#*****************************************TRANSFERRING OBJECTS FROM LOCAL
#*************************************************************
#Tranfer any local R object to server
IRIS<-iris

#Check IRIS is a data.frame
str(IRIS)

#Send local object to remote VM
putLocalObject("IRIS",name="remoteIRIS")

#Delete local object
rm(IRIS)

#get remote object to local environment

getRemoteObject("remoteIRIS")


#******************************************TRANFERRING WORKSPACES FROM LOCAL
#****************************************************************

#Take all objects form the local R session and load them into the remote R session

putLocalWorkspace()

#Take all objects from the remote R session and load them into the local R session.

getRemoteWorkspace()

#Logout
remoteLogout()
