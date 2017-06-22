
#current directory should be analysisInR_postLizzy
setwd("~/Documents/attention_tempresltn/temp res of attn, binding/backwardsLetters/dataAnalysis")
#E1 is data.frame created by below file
#if (!exists('E1')) {
  source('loadRawData/E1_BackwardsLtrsLoadRawData.R')
#}
#if (!exists('E2')) {
  source('loadRawData/E2_BackwardsLtrsLoadRawData.R')
#}
#  rm(list=ls())
  
#Harmonise E1 and E2 so can combine into single data frame
E1$condName<- "Canonical"
E1$condName[E1$condition == "2"] <- "Backwards"
E1$exp<-1
  
E2$exp<-2
E2$expDegrees <- NULL

E<- rbind(E2,E1)
  
#get rid of some cluttering useless variables
E$endTime<-NULL #Don't know what this is
E$letterSeparation<-NULL
E$letterEccentricity<-NULL

#Work out the minimum and maximum SPE so can set histogram limits and num bins accordingly.
#not guaranteed to give right answer if small amount of data so not all possible values observed
minMaxTargetPos<-c( min(E$target1),max(E$target1) )
minRespPos<- min (1, min(E$response1) ) #AFAIK all my experiments use 1 as first serial position but just in case
minMaxRespPos<-c( minRespPos, max(E$response1) )
maxSPE<- minMaxTargetPos[2] - minMaxRespPos[1]
minSPE<- minMaxTargetPos[1] - minMaxRespPos[2]
numPossibleSPEs<- maxSPE-minSPE+1 #have to add 1 to count SPE=0
#Calculate the range that the guessing distribution is still ramping up. Try to add some color for it
#Certain values can occur for any targetPos
maxAlwaysPossibleSPE<- minMaxRespPos[2] - minMaxTargetPos[2] 
minAlwaysPossibleSPE<- minMaxRespPos[1] - minMaxTargetPos[1]

#Don't forget that E1 and E2 already excludes participants that were excluded at mixture modeling stage due to their
#efficacy being indistinguishable from zero.

# Lizzy: Actually in all the conditions for the code the  ‘left stream’ or stream 1 is always the one that you would read first, and right stream or stream 2 is the one that you would read second.
#      Its because the way it is coded is that the streams really are just rotated visually by 0 degrees (i.e. left right), 180 degrees (i.e. ʇɥƃᴉɹ ʇɟǝl ), 90 degrees and 270 degrees.

E$response1 <- as.numeric(E$response1)
E$response2 <- as.numeric(E$response2)
E$target1 <- as.numeric(E$target1) #left if canonical, generally first that would be read according to implied reading order
E$target2<- as.numeric(E$target2) #right if canonical, generally first that would be read according to implied reading order

#SANITY CHECK LATENCY before gathering
#Does response1 refer to the serial position in the stream, or is it a code for a letter,
#with serial position recoverable from letterOrder
require(ggplot2)
#sanity check
g=ggplot(E,   aes(x=response1-target1))  
g<-g+facet_grid(condName~exp)
g<-g+geom_histogram(bin_width=1) + xlim(minSPE,maxSPE)
g
#Looks good

#Make data long
#Melt data.frame to create a target number variable
#That way can include left/right in statistical analyses representing target number as factor
#and answer how is latency different for left and right.

#Change to long format instead of having target 1 and target 2 in different columns
require(tidyr)  #target will be name of key column, targetSP name of value column
gathered <- gather(E, key=target, value=targetSP,    target1,target2)
gathered$target[gathered$target=="target1"] <- 1
gathered$target[gathered$target=="target2"] <- 2

#response1, response2 still in wide format. Fix that
gathered$respSP <- gathered$response1
#replace those that are target2 with response2
gathered$respSP[ gathered$target == 2  ] <- gathered$response2[ gathered$target == 2 ]
#Fixed, now can delete response1 and response2
gathered$response1<-NULL
gathered$response2<-NULL

#resp1otherStream, resp2otherStream still in wide format. Fix that.
gathered$respSPwrongStream <- gathered$resp1otherStream #For target=1, resp1otherStream is the SPE in stream 2
#In the case of target=2, the response is response2 and resp2otherStream is SPE in stream 1
gathered$respSPwrongStream[ gathered$target == 2  ] <- gathered$resp2otherStream[ gathered$target == 2 ]
#Fixed, now can delete response1 and response2
gathered$resp1otherStream<-NULL
gathered$resp2otherStream<-NULL

head(gathered)
Elong<-gathered

Elong$SPE<- Elong$respSP - Elong$targetSP
Elong$correct <- Elong$SPE==0
Elong$approxCorr <- abs(Elong$SPE)<3 #Absolute value of the serial position error is less than 3

Elong$SPEother<- Elong$respSPwrongStream - Elong$targetSP #targetSP is same for both streams
Elong$correctWrongStream <- Elong$SPEother==0

require(dplyr)

#sanity check
g=ggplot(Elong,   aes(x=SPE))  
g<-g+facet_grid(condName~target)  +geom_histogram(bin_width=1) + xlim(minSPE,maxSPE)
g #looks good
g=ggplot(Elong,   aes(x=SPEother)) 
g<-g+facet_grid(condName~target)  +geom_histogram(bin_width=1) + xlim(minSPE,maxSPE)
#add line to show bit that should be flat if pure guessing
g<-g+geom_segment(aes(x = minAlwaysPossibleSPE, y = 0, xend = maxAlwaysPossibleSPE, yend = 0,
                   color = "flat"))
g #looks good thank you Chris. Some swaps can be seen. More for target 2, even for canonical condition
                  
# Also create straight up left and right, top bottom, because 1 and 2 correpsond to reading direction rather than mapping direclty on to positions
Elong$location<-Elong$condName
#Based on what Lizzy said, target=1 means top in downwards but bottom in upwards.
#filter(Elong,condName=="Inverted", target==1)$location  <- "right" 
Elong<- Elong %>% 
  mutate(location = replace(location, condName=="Inverted" & target==1, "right"),
         location = replace(location, condName=="Inverted" & target==2, "left"),
         location = replace(location, condName=="Canonical"& target==1, "left"),
         location = replace(location, condName=="Canonical"& target==2, "right"),
         location = replace(location, condName=="Backwards"& target==1, "right"),
         location = replace(location, condName=="Backwards"& target==2, "left"),
         location = replace(location, condName=="Downwards"& target==1, "top"),
         location = replace(location, condName=="Downwards"& target==2, "bottom"),
         location = replace(location, condName=="Upwards"  & target==1, "bottom"),
         location = replace(location, condName=="Upwards"  & target==2, "top"))
         
table(Elong$location,Elong$condName,Elong$target) #check it worked. 

# "In the ANOVA, stream was coded as dominant (top, left) or non-dominant (bottom, right)."
Elong$dominant<-FALSE
Elong$dominant[Elong$location=="left" | Elong$location=="top"] <- TRUE

#Calculate latency as function of condition

#First analyse participants individually, then collapse across participants
s <- Elong %>% 
  group_by(exp, condName,target, participantID) %>%
  summarise(corr=mean(correct), SPEmsec=mean(SPE*1000/itemRate))  #by participant
t <- s %>%  #collapse across participants
  group_by(exp, condName,target) %>% summarise_each(funs(mean,se=sd(.)/sqrt(n()))) %>%
      select(-participantID_mean, -participantID_se)
t #exp1,canonical left SPEmsec=31+/-7.5, right 24.8+/-15. exp1 backwards left 30.0+/-9.3 right 0.8 13.4
 #exp2, canonical left SPEms=41.3+/-9.6, right 18.9+/-17.5; inverted left 16.2+/18.5 right 11.1+/-14.6
 #exp2 downwards top SPEms=20.9 16.7, btm 17.9 22.4, upwards top 26.3 14.2 btm 52.5 17.4

#Do statistics on it
require(ez)
#factors = condName, target
plots = list()
expNum<-1
thisExp <- filter(Elong, exp==expNum)
latencyANOVA <- ezANOVA(data=thisExp, dv=SPE, within=.(location,condName), wid=participantID)
print(latencyANOVA)
cat("exp=",expNum,"F=", latencyANOVA$ANOVAF, " ps=", latencyANOVA$ANOVA$p, "\n", sep=",")
#These ANOVA details now reported on p.16 of revised manuscript

#Plot the interaction.
condName_by_target_plot = ezPlot(
  data = thisExp, dv = .(SPE)  , wid = .(participantID), within = .(location,condName)
  , x = .(location)
  , split = .(condName),
  print_code = FALSE
)
plots[[length(plots)+1]] <- condName_by_target_plot

#To get standard errors on means to report in manuscript, used dplyr because
#  ezPlot uses Tukey's LSD or something
print(plots[1])

####################
#Examine effect of response order RESPONSE ORDER

Elong$thisQueriedFirst  <- TRUE

Elong<- Elong %>% 
  mutate(thisQueriedFirst = replace(thisQueriedFirst, condName=="Canonical"  &  target==(oneQueriedFirst+1), TRUE),
         thisQueriedFirst = replace(thisQueriedFirst, condName=="Canonical"  &  target!=(oneQueriedFirst+1), FALSE),
         thisQueriedFirst = replace(thisQueriedFirst, condName=="Backwards"  &  target==(oneQueriedFirst+1), FALSE),
         thisQueriedFirst = replace(thisQueriedFirst, condName=="Backwards"  &  target!=(oneQueriedFirst+1), TRUE),
         thisQueriedFirst = replace(thisQueriedFirst, condName=="Upwards"  &  target==(oneQueriedFirst+1), FALSE),
         thisQueriedFirst = replace(thisQueriedFirst, condName=="Upwards"  &  target!=(oneQueriedFirst+1), TRUE),
         thisQueriedFirst = replace(thisQueriedFirst, condName=="Inverted"  &  target==(oneQueriedFirst+1), FALSE),
         thisQueriedFirst = replace(thisQueriedFirst, condName=="Inverted"  &  target!=(oneQueriedFirst+1), TRUE),
         thisQueriedFirst = replace(thisQueriedFirst, condName=="Downwards"  &  target==(oneQueriedFirst+1), TRUE),
         thisQueriedFirst = replace(thisQueriedFirst, condName=="Downwards"  &  target!=(oneQueriedFirst+1), FALSE))

table(Elong$condName, Elong$thisQueriedFirst, Elong$exp)

toSumm<- Elong[,c("exp","participantID","condName","target","thisQueriedFirst","oneQueriedFirst","SPE","correct")]
toSumm$SPEmsec <- toSumm$SPE*(1000/Elong$itemRate)
toSumm$correct<- as.numeric(toSumm$correct)
#There is a zero-order prediction that performance is better for side queried first. Forget second one more often because reported later.
                            #rightFirst+1 == target  means target is 2 and rightFirst, meaning that it works for canonical condition
xx <- toSumm %>% group_by(exp,thisQueriedFirst,participantID) %>%  summarise_each(funs(mean)) %>% 
  select(participantID, exp, thisQueriedFirst, SPEmsec, correct)
yy<- xx %>% group_by(exp,thisQueriedFirst) %>% summarise_each(funs(mean,se=sd(.)/sqrt(n()))) 
yy %>% select(-participantID_mean,-participantID_se)

#Spatial memory coding prediction. People encode the two targets spatially and prefer to read out memory left to right
vv <- toSumm %>% group_by(exp,oneQueriedFirst,participantID,condName) %>%  summarise_each(funs(mean)) %>% 
  select(participantID, condName, exp, oneQueriedFirst, SPEmsec, correct)
vv<- vv %>% group_by(exp,condName,oneQueriedFirst) %>% summarise_each(funs(mean,se=sd(.)/sqrt(n())))  %>% 
            select(-participantID_mean,-participantID_se)
vv #Reporting these values in the manuscript

#Probably should analyse as ANOVA
#First analyse participants individually, then collapse across participants

expNum<-1
whichFirstANOVAe1 <- ezANOVA(data= toSumm %>% filter(exp==expNum), 
                        dv=correct, within=.(thisQueriedFirst,condName,target), wid=participantID)
print(whichFirstANOVAe1)
#These ANOVA details now reported on p.17 of revised manuscript

expNum<-2
whichFirstANOVA <- ezANOVA(data= toSumm %>% filter(exp==expNum), 
                           dv=correct, within=.(thisQueriedFirst,condName,target), wid=participantID)
print(whichFirstANOVA)
#These ANOVA details now reported on p.23 of revised manuscript

#Close to significant for E1 is interaction of oneQueriedFirst and target
ee<- toSumm %>% group_by(exp,oneQueriedFirst,target,participantID) %>%  summarise_each(funs(mean)) %>% 
  select(participantID, exp, oneQueriedFirst, target, correct, SPEmsec )
ff<- ee %>% group_by(exp,oneQueriedFirst,target) %>% summarise_each(funs(mean,se=sd(.)/sqrt(n()))) %>%
              select(-participantID_mean,-participantID_se)
ff #very little going on

#Plot the interaction.
thisQueriedFirst_by_target_plot = ezPlot(
  data = toSumm %>% filter(exp==1), dv = .(correct)  , wid = .(participantID), within = .(thisQueriedFirst,target)
  , x = .(target)
  , split = .(thisQueriedFirst),
  print_code = FALSE
)
print(thisQueriedFirst_by_target_plot) #Not much going on

#Do a t-test
#Need to put back in wide format based on rightFirst participant_ID combination so
#  that have the two vectors as columns
#spreaded <- xx %>% select(-SPEmsec) %>% spread(rightFirst, correct) 
#spreaded
#t.test(spreaded$`FALSE`, spreaded$`TRUE`, alternative = "two.sided", var.equal = TRUE)  #perform the Student's t-test


#ALSO ANALYSE WHETHER THERE ARE MORE SPATIAL SWAPS IN THE BACKWARDS CONDITION

###########################################################################################
#E2 #######################################################################################

if (!exists('E2')) {
  source('loadRawData/E2_BackwardsLtrsLoadRawData.R')
}
E2wide<-E2
#24 participants which makes sense because 28 run, 4 excluded

#get rid of some cluttering useless variables
E2wide$letterSeparation<-NULL
E2wide$letterEccentricity<-NULL
E2wide$randomSeed<-NULL
E2wide$response1 <- as.numeric(E2wide$response1) #serial position
E2wide$response2 <- as.numeric(E2wide$response2) #seria position
E2wide$target1 <- as.numeric(E2wide$target1) #SP left?  First accoridng to reading direction?
E2wide$target2<- as.numeric(E2wide$target2) #SP right? Second according to rteading direction?

#Change to long format instead of having target 1 and target 2 in different columns
require(tidyr)  #target will be name of key column, targetSP name of value column
gathered <- gather(E2wide, target, targetSP,    target1,target2)
gathered$target[gathered$target=="target1"] <- 1
gathered$target[gathered$target=="target2"] <- 2

#response1, response2 still in wide format. Fix that
gathered$respSP <- gathered$response1
#replace those that are target2 with response2
gathered$respSP[ gathered$target == 2  ] <- gathered$response2[ gathered$target == 2 ]
#Fixed, now can delete response1 and response2
gathered$response1<-NULL
gathered$response2<-NULL

head(gathered)
E2<-gathered

E2$SPE<- E2$respSP - E2$targetSP
E2$correct <- E2$SPE==0

#sanity check
g=ggplot(E2,   aes(x=SPE))  
g<-g+facet_grid(target~condName)
g<-g+geom_histogram(bin_width=1)
g
#looks good

SOA = 89+36 #ms

SE=sd(SPEmsec)/sqrt(n())

toSummarise<- E2[,c("participantID","condName","target","SPE","correct")]
toSummarise$SPEmsec <- toSummarise$SPE*SOA
#First analyse participants individually, then collapse across participants
s <- toSummarise %>%
      group_by(condName,target, participantID) %>%
      summarise_each(funs(mean))

s$participantID<-NULL #collapse across participants
t <- s %>%  
  group_by(condName,target) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())))
t  

#Do statistics on it



# factors = condName, target
latencyANOVA_E2 <- ezANOVA(data=E2, dv=SPE, within=.(condName,dominant), wid=participantID)

print(latencyANOVA_E2)
cat("F=", latencyANOVA$F, " p=", latencyANOVA$ANOVA$p, "\n", sep="")
#These ANOVA details now reported on p.16 of revised manuscript

#Can I replicate accuracy we have in the text
cc <- t %>%  #collapse to print main effect of condName (orientation)
  group_by(condName) %>%
  summarise(msec=mean(msec),pCorr=mean(pCorr))
cc #Yes! Exactly same figures as Lizzy reported

#That's good but need left bias measure, for which the original wide data format is good because both targets on same row
E2wide$SPE1<- E2wide$response1 - E2wide$target1
E2wide$SPE2<- E2wide$response2 - E2wide$target2
E2wide$SPE<- E2wide$respSP - E2wide$targetSP
E2wide$correct1 <- E2wide$SPE1==0
E2wide$correct2 <- E2wide$SPE2==0
E2wide$target1bias <- E2wide$correct1 - E2wide$correct2 #Trying to get left bias eventually

#First analyse participants individually, only then collapse across participants (in case different participants different numbers of trials
toSummariseWide<- E2wide[,c("participantID","condName","SPE1","SPE2","correct1","correct2","target1bias")]
wideS <- toSummariseWide %>%
      group_by(condName, participantID) %>%
      summarise_each(funs(mean))
wideS
wideS$participantID<-NULL #collapse across participants
t <- wideS %>%  
  group_by(condName) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())))
t  #These bias values now reported at end of Accuracy section of E2

#Rather than doing anything Bayesian as suggested by Reviewer 2, we just report confidence intervals
