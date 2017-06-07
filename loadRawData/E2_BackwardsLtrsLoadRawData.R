#Loads raw data for E2 Backwards letters

require(R.matlab)

userNameOnMachineOnWayToGoogleDrive<- "alexh" #"admin" on the machine that Chris Bush used
GoogleDrivePath<-"/Google\ Drive/Backwards\ paper/"
directoryOfRawData<- paste0("/Users/",userNameOnMachineOnWayToGoogleDrive,GoogleDrivePath,
                            "E2_Rotat/Experiment2RotateData/RawData/")
path <- file.path(directoryOfRawData)

col_headings <- c('participantID',
                  'block',
                  'randomSeed',
                  'letterSeparation',
                  'letterEccentricity',
                  'itemRate',
                  'condition', 
                  'expDegrees', #Angle the letter is drawn in 
                  'letterOrder1',
                  'target1',
                  'response1',
                  'responseLetter1',
                  'RT1',
                  'letterOrder2',
                  'target2',
                  'response2',
                  'responseLetter2',
                  'RT2',
                  'endTime')


             
E2 <- data.frame(matrix(ncol = 19, nrow = 0)) # Dataframe to hold all data
colnames(E2) <- col_headings #Assign column headings
tempData <- data.frame(matrix(ncol = 19, nrow = 100)) #Temp dataFrame for single data files
colnames(tempData) <- col_headings #Assign headings to temp dataFrame

for (fileName in list.files(path, pattern="*.mat")) #Loop through and read in each .mat data file
{
  singleSubjectData <- readMat(file.path(path, fileName)) #Read in single data file
  cat("reading",fileName,"\n")
  
  #Wrangle data into appropraite format
  tempData$participantID <- singleSubjectData$participantID[1]
  tempData$block <- substr(fileName, nchar(fileName) - 4, nchar(fileName) - 4) #Retrieve block number from file name
  tempData$randomSeed <- singleSubjectData$randomSeed[1]
  tempData$letterSeparation <- singleSubjectData$letterSeparation[1]
  tempData$letterEccentricity <- singleSubjectData$letterEccentricity[1]
  tempData$itemRate <- singleSubjectData$itemRate[1]
  tempData$condition <- sapply(singleSubjectData$allConditions, paste0)
  tempData$expDegrees <- paste(singleSubjectData$expDegrees[1,], collapse = ",")
  tempData$letterOrder1 <- apply(singleSubjectData$allLetterOrder[,1,], 1, paste, collapse=",") 
  tempData$target1 <- sapply(singleSubjectData$allTargets[,1], paste0)
  tempData$responseLetter1 <- sapply(singleSubjectData$allResponses[,1], paste0)
  tempData$RT1 <- sapply(singleSubjectData$allRTs[,1], paste0)
  tempData$letterOrder2 <- apply(singleSubjectData$allLetterOrder[,2,], 1, paste, collapse=",")
  tempData$target2 <- sapply(singleSubjectData$allTargets[,2], paste0)
  tempData$responseLetter2 <- sapply(singleSubjectData$allResponses[,2], paste0)
  tempData$RT2 <- sapply(singleSubjectData$allRTs[,2], paste0)
  tempData$endTime <- singleSubjectData$endTime[1]
  
  ##Add serial position columns for responses
  rowCount <- 1
  for (responses in tempData$response1)
  {
    tempData$response1[rowCount] <- match(tempData$responseLetter1[rowCount],singleSubjectData$allLetterOrder[rowCount,1,])
    tempData$response2[rowCount] <- match(tempData$responseLetter2[rowCount],singleSubjectData$allLetterOrder[rowCount,2,])
    rowCount <- rowCount + 1
  }
  
  E2 <- rbind(E2,tempData) # Add subjects data to main dataFrame
}

#Straight from Lizzy's mouth, meaning of condition and expDegrees variables
# 1: Canonical (0 degree rotation) 
# 2: downwards (90 deg clockwise rotate) 
# 3. Inverted (180 deg clockwise rotate
# 4. Upwards (270 degree rotation) 

E2$condName<-E2$condition
E2$condName[E2$condName == "1"] <- "Canonical"
E2$condName[E2$condName == "2"] <- "Downwards"
E2$condName[E2$condName == "3"] <- "Inverted"
E2$condName[E2$condName == "4"] <- "Upwards"

#What variable tells us the response order?
#PTGoodbourn: I think in some versions of the code there was an explicit response order variable, but here you can probably get it from:
#  firstResponse = ( allRTs(:,1) > allRTs(:,2) ) + 1;
#  % 1 = left first, 2 = right first
E2$rightFirst <- E2$RT1 > E2$RT2

#save dataframe as csv file
saveDataFramesPath<-"loadRawData/"
write.csv(E2,paste0(saveDataFramesPath, "E2_BwdsLtrs_Rotate_RawData.csv"),row.names=FALSE)