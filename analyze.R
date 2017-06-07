
#current directory should be analysisInR_postLizzy
#E1 is data.frame created by below file
#if (!exists('E1')) {
  source('loadRawData/E1_BackwardsLtrsLoadRawData.R')
#}
#if (!exists('E2')) {
  source('loadRawData/E2_BackwardsLtrsLoadRawData.R')
#}

#Harmonise E1 and E2 so can combine into single data frame
E2$expDegrees <- NULL
E1$condName<- "Canonical"
E1$condName[E1$condition == "2"] <- "Backwards"


#Don't forget that E1 and E2 already excludes participants that were excluded at mixture modeling stage due to their
#efficacy being indistinguishable from zero.

# Lizzy: Actually in all the conditions for the code the  ‘left stream’ or stream 1 is always the one that you would read first, and right stream or stream 2 is the one that you would read second.
#      Its because the way it is coded is that the streams really are just rotated visually by 0 degrees (i.e. left right), 180 degrees (i.e. ʇɥƃᴉɹ ʇɟǝl ), 90 degrees and 270 degrees.

#get rid of some cluttering useless variables
E$endTime<-NULL #Don't know what this is
E$letterSeparation<-NULL
E$letterEccentricity<-NULL
E$condName<-E$condition


E$response1 <- as.numeric(E$response1)
E$response2 <- as.numeric(E$response2)
E$target1 <- as.numeric(E$target1) #left?
E$target2<- as.numeric(E$target2) #right?

#SANITY CHECK LATENCY before gathering
#Does response1 refer to the serial position in the stream, or is it a code for a letter,
#with serial position recoverable from letterOrder
require(ggplot2)
#sanity check
g=ggplot(E,   aes(x=response1-target1))  
g<-g+facet_grid(condName~.)
g<-g+geom_histogram()
g
#Looks good

#Make data long
#Melt data.frame to create a target number variable
#That way can include left/right in statistical analyses representing target number as factor
#and answer how is latency different for left and right.

#Change to long format instead of having target 1 and target 2 in different columns
require(tidyr)  #target will be name of key column, targetSP name of value column
gathered <- gather(E, target, targetSP,    target1,target2)
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
Eg<-gathered

Eg$SPE<- Eg$respSP - Eg$targetSP

require(ggplot2)
#sanity check
g=ggplot(Eg,   aes(x=SPE))  
g<-g+facet_grid(condName~target)
g<-g+geom_histogram()
g
#looks good

SOA = 83 #ms
#Calculate latency as function of condition
require(dplyr)

#First analyse participants individually, then collapse across participants
s <- Eg %>%
  group_by(condName,target, participantID) %>%
  summarise(SPEmsec=mean(SPE*SOA))  #by participant
t <- s %>%  #collapse across participants
  group_by(condName,target) %>%
  summarise(msec=mean(SPEmsec),SE=sd(SPEmsec)/sqrt(n()))
t

#Do statistics on it
require(ez)

#factors = condName, target
latencyANOVA <- ezANOVA(data=Eg, dv=SPE, within=.(condName,target), wid=participantID)
print(latencyANOVA)
cat("F=", latencyANOVA$F, " ps=", latencyANOVA$ANOVA$p, "\n", sep=",")
#These ANOVA details now reported on p.16 of revised manuscript

########
#Examine effect of response order

Eg$correct <- Eg$SPE==0

toSumm<- Eg[,c("participantID","condName","target","rightFirst","SPE","correct")]
toSumm$SPEmsec <- toSumm$SPE*SOA

#There is a zero-order prediction that performance is better for side queried first. Forget second one more often because reported later.
toSumm$queriedFirst <- as.numeric(toSumm$rightFirst)+1 == toSumm$target
#Assume (checking with Lizzy to be sure) that target1 is actually right target in backwards condition. 
# So, need to flip backwards condition.
toSumm$queriedFirst[ toSumm$condName=="Backwards"] <- ! toSumm$queriedFirst[ toSumm$condName=="Backwards"]

vv <- toSumm %>% group_by(queriedFirst) %>% summarise_each(funs(mean))
vv

#Spatial memory coding prediction. People encode the two targets spatially and prefer to read out memory left to right
xx <- toSumm %>% group_by(rightFirst,participantID) %>%  summarise_each(funs(mean)) %>% 
      select(participantID,rightFirst, SPEmsec, correct)

yy<- xx %>% group_by(rightFirst) %>% summarise_each(funs(mean,se=sd(.)/sqrt(n()))) 
yy #21.5% and 21.8%

#Do a t-test
xx
#Need to put back in wide format based on rightFirst participant_ID combination so
#  that have the two vectors as columns
spreaded <- xx %>% select(-SPEmsec) %>% spread(rightFirst, correct) 
spreaded
t.test(spreaded$`FALSE`, spreaded$`TRUE`, alternative = "two.sided", var.equal = TRUE)  #perform the Student's t-test

#There is also a prediction of higher performance if order of report aligns with letter prioritised,
# even when first one queried is not the one reported. That is, in the canonical condition
# performance should be better for both targets if left one is queried first.
# And less so for the backwards condition.
toSumm$priorityAligned <- !toSumm$rightFirst & toSumm$condName=="Canonical"
#For backwards columns, good if rightFirst
toSumm$priorityAligned[toSumm$condName=="Backwards"] <- toSumm$rightFirst[toSumm$condName=="Backwards"]

table(toSumm$priorityAligned,toSumm$rightFirst,toSumm$condName, dnn=c("spatial","rightFirst","condName"))
ww <- toSumm %>% group_by(priorityAligned) %>%  summarise_each(funs(mean))
ww #2% correct difference in the wrong direction.

#Examine the canonical condition
zz <- toSumm %>% group_by(priorityAligned,condName) %>%  summarise_each(funs(mean))
zz #2% difference in the wrong direction

#Code the variable indicating what predicted higher performance if order of report aligns with letter prioritised,
#  left letter if canonical and right letter if backwards
toSumm$betterIfOrderMatters <- toSumm$target==1 & toSumm$rightFirst==FALSE
toSumm$betterIfOrderMatters[toSumm$target==2 & toSumm$rightFirst==TRUE ] <- TRUE
#If it is the backwards condition, it's the opposite value
toSumm$betterIfOrderMatters[toSumm$condName=="Backwards"] <- ! toSumm$betterIfOrderMatters[toSumm$condName=="Backwards"]
#verify I did this correctly
table(toSumm$betterIfOrderMatters,toSumm$target,toSumm$rightFirst,toSumm$condName, dnn=c("better","target","rightFirst","condName"))
#Confirmed, assuming that target 1 means left side when canonical and right side when backwards
toSumm$betterIfOrderMatters<- as.numeric(toSumm$betterIfOrderMatters)
bb <- toSumm %>%
      group_by(betterIfOrderMatters,participantID) %>%
      summarise_each(funs(mean)) 
bb$participantID<-NULL
cc <- bb %>%
      group_by(betterIfOrderMatters) %>%
      summarise_each(funs(mean,se=sd(.)/sqrt(n()))) 
cc  #That looks good, close to zero diff between the two groups

#Visualise report order effect
g=ggplot(ss, aes(x=target,color=rightFirst,y=correct))  
g<-g+facet_grid(condName~.)
g<-g+ stat_summary(fun.y=mean,geom="point") # geom_point()
g<-g + stat_summary(fun.data = mean_se, geom = "errorbar", width=.3)
g<-g+ theme_bw()
g

ss$participantID<-NULL #collapse across participants
tt <- ss %>%  
  group_by(condName,target,rightFirst) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())))
tt  

g=ggplot(tt, aes(x=rightFirst,color=target,y=correct_mean))  
g<-g+facet_grid(condName~.)
g<-g+geom_point()
g

#Probably should analyse as ANOVA
#First analyse participants individually, then collapse across participants
ss <- toSumm %>%
      group_by(condName,target,rightFirst,participantID) %>%
      summarise_each(funs(mean))


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
g<-g+geom_histogram()
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

# "In the ANOVA, stream was coded as dominant (top, left) or non-dominant (bottom, right)."
E2$streamName<-E2$condName
#Based on what Lizzy said in note at top, target=1 means top in downwards but bottom in upwards.
E2$streamName[E2$condName=="Canonical" & E2$target==1 ] <- "Left"
E2$streamName[E2$condName=="Canonical" & E2$target==2 ] <- "Right"
E2$streamName[E2$condName=="Inverted" & E2$target==1 ] <- "Right"
E2$streamName[E2$condName=="Inverted" & E2$target==2 ] <- "Left"
E2$streamName[E2$condName=="Downwards" & E2$target==1 ] <- "Top"
E2$streamName[E2$condName=="Downwards" & E2$target==2 ] <- "Bottom"
E2$streamName[E2$condName=="Upwards" & E2$target==1 ] <- "Bottom"
E2$streamName[E2$condName=="Upwards" & E2$target==2 ] <- "Top"

table(E2$streamName,E2$condName) #check it worked

E2$dominant<-FALSE
E2$dominant[E2$streamName=="Left"] <- TRUE
E2$dominant[E2$streamName=="Top"] <- TRUE

table(E2$dominant,E2$streamName) #check it worked

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
