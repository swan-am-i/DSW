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



# show that the server R environment is empty

ls()
pause()

#Create a snapshot - give it a name description
#please note snapshots are manipulated by ID

Snap1<-createSnapshot("SNAP1")

# list out the snapshots
#Snapshots can be loaded by ID not by Name
snaps <- do.call(rbind.data.frame, listSnapshots())

#Check your created snap is there
snaps

# load in a snapshot, resume the remote session and look at the R environment
loadSnapshot(snaps$id[1])
resume()
ls()
pause()

# download the snapshot from the server
downloadSnapshot(snaps$id[1], file = "mySnap.zip")
unzip("mySnap.zip")
load(paste0(snaps$id[1],".RData"))

#Logout
remoteLogout()

