#Loads raw data for E1 Backwards letters. Script written by Chris Bush

require(R.matlab)
userNameOnMachineOnWayToGoogleDrive<- "alexh" #"admin" on the machine that Chris Bush used
GoogleDrivePath<-"/Google\ Drive/Backwards\ paper/"
subdirPath<-"E1_MirrRev/Experiment1MirrorRevData/"
directoryOfRawData<- paste0("/Users/",userNameOnMachineOnWayToGoogleDrive,
                            GoogleDrivePath,subdirPath,"matFilesForAnalysisExp1/")
  
path <- file.path(directoryOfRawData)
#Location of actual experiment code to run the experiment is unknown

#1 or a 2 at the end of each subject's raw data filename, e.g. RA_14-09-08_1.mat, are the two different blocks (conditions were blocked)
#There is a condition field with value 1 or 2. Lizzy sez: 1 is forward, 2 is mirror reversed

# Lizzy: Actually in all the conditions for the code the  ‘left stream’ or stream 1 is always the one that you would read first, and right stream or stream 2 is the one that you would read second.
#      Its because the way it is coded is that the streams really are just rotated visually by 0 degrees (i.e. left right), 180 degrees (i.e. ʇɥƃᴉɹ ʇɟǝl ), 90 degrees and 270 degrees.
#Meaning that target 1 and target 2 and presumably response 1 and response 2 are which you would read first

col_headings <- c('participantID',
                  'block',
                  'randomSeed',
                  'letterSeparation',
                  'letterEccentricity',
                  'itemRate',
                  'condition',
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


E1 <- data.frame(matrix(ncol = 18, nrow = 0)) #Dataframe to hold all data
colnames(E1) <- col_headings #Assign column headings
tempData <- data.frame(matrix(ncol = 18, nrow = 100)) #Temp dataFrame for single data files
colnames(tempData) <- col_headings #Assign headings to temp dataFrame

for (fileName in list.files(path, pattern="*.mat")) #Loop through and read in each .mat data file
{
  if (fileName != "II_15-04-20_1.mat") { #Mysterious file with only 12 trials that showed up after Chris did everything before March 1
    #Is II belong in pilotSs?_certainlyNotInFinalDataset?
    singleSubjectData <- readMat(file.path(path, fileName)) #Read in single data file
    cat("reading",fileName,"\n")
    #Wrangle data into appropriate format
    tempData$participantID <- singleSubjectData$participantID[1]
    tempData$block <- substr(fileName, nchar(fileName) - 4, nchar(fileName) - 4) #Retrieve block number from file name
    tempData$randomSeed <- singleSubjectData$randomSeed[1]
    tempData$letterSeparation <- singleSubjectData$letterSeparation[1]
    tempData$letterEccentricity <- singleSubjectData$letterEccentricity[1]
    tempData$itemRate <- singleSubjectData$itemRate[1]
    tempData$condition <- sapply(singleSubjectData$allConditions, paste0)
    tempData$letterOrder1 <- apply(singleSubjectData$allLetterOrder[,1,], 1, paste, collapse=",") 
    tempData$target1 <- sapply(singleSubjectData$allTargets[,1], paste0)
    tempData$responseLetter1 <- sapply(singleSubjectData$allResponses[,1], paste0)
    tempData$RT1 <- sapply(singleSubjectData$allRTs[,1], paste0)
    tempData$letterOrder2 <- apply(singleSubjectData$allLetterOrder[,2,], 1, paste, collapse=",")
    tempData$target2 <- sapply(singleSubjectData$allTargets[,2], paste0)
    tempData$responseLetter2 <- sapply(singleSubjectData$allResponses[,2], paste0)
    tempData$RT2 <- sapply(singleSubjectData$allRTs[,2], paste0)
    tempData$endTime <- singleSubjectData$endTime[1]
    
    #Add serial position columns for responses
    rowCount <- 1
    for (responses in tempData$response1)
    {
      tempData$response1[rowCount] <- match(tempData$responseLetter1[rowCount],singleSubjectData$allLetterOrder[rowCount,1,])
      tempData$response2[rowCount] <- match(tempData$responseLetter2[rowCount],singleSubjectData$allLetterOrder[rowCount,2,])
      rowCount <- rowCount + 1
    }
    
    E1 <- rbind(E1,tempData) #Add subjects data to main dataFrame
  }
}

#What variable tells us the response order?
#PTGoodbourn: I think in some versions of the code there was an explicit response order variable, but here you can probably get it from:
#  firstResponse = ( allRTs(:,1) > allRTs(:,2) ) + 1;
E1$oneQueriedFirst <- E1$RT1 > E1$RT2

#save dataframe as csv file
#saveDataFramesPath<- paste0("/Users/",userNameOnMachineOnWayToGoogleDrive,GoogleDrivePath,
#                            "analysisInR_postLizzy/loadRawData/")
saveDataFramesPath<-"loadRawData/"
write.csv(E1,paste0(saveDataFramesPath, "E1_BwdsLtrs_MirrRev_RawData.csv"),row.names=FALSE)