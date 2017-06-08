
#current directory should be analysisInR_postLizzy
#E1 is data.frame created by below file
#if (!exists('E1')) {
  source('loadRawData/E1_BackwardsLtrsLoadRawData.R')
#}
#if (!exists('E2')) {
  source('loadRawData/E2_BackwardsLtrsLoadRawData.R')
#}

  
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
Elong<-gathered

Elong$SPE<- Elong$respSP - Elong$targetSP
Elong$correct <- Elong$SPE==0
Elong$approxCorr <- abs(Elong$SPE)<3

#sanity check
g=ggplot(Elong,   aes(x=SPE))  
g<-g+facet_grid(condName~target)
g<-g+geom_histogram()
g
#looks good

require(dplyr)
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
         thisQueriedFirst = replace(thisQueriedFirst, condName=="Inverted"  &  target!==(oneQueriedFirst+1), TRUE),
         thisQueriedFirst = replace(thisQueriedFirst, condName=="Downwards"  &  target==(oneQueriedFirst+1), TRUE),
         thisQueriedFirst = replace(thisQueriedFirst, condName=="Downwards"  &  target!=(oneQueriedFirst+1), FALSE))

table(Elong$condName, Elong$thisQueriedFirst, Elong$exp)

toSumm<- Elong[,c("exp","participantID","condName","target","thisQueriedFirst","SPE","correct")]
toSumm$SPEmsec <- toSumm$SPE*(1000/Elong$itemRate)

#There is a zero-order prediction that performance is better for side queried first. Forget second one more often because reported later.
                            #rightFirst+1 == target  means target is 2 and rightFirst, meaning that it works for canonical condition
toSumm$queriedFirst <- as.numeric(toSumm$rightFirst)+1 == toSumm$target
# Target1 is actually right target in backwards condition. (checking with Lizzy to be sure)
# So, need to flip backwards condition.
toSumm$queriedFirst[ toSumm$condName=="Backwards"] <- ! toSumm$queriedFirst[ toSumm$condName=="Backwards"]
#Experiment 2
toSumm$queriedFirst <- as.numeric(toSumm$rightFirst)+1 == toSumm$target
# Target1 is actually right target in backwards condition. (checking with Lizzy to be sure)
# So, need to flip backwards condition.
toSumm$queriedFirst[ toSumm$condName=="Backwards"] <- ! toSumm$queriedFirst[ toSumm$condName=="Backwards"]






#Spatial memory coding prediction. People encode the two targets spatially and prefer to read out memory left to right
xx <- toSumm %>% group_by(expNum,queriedFirst,participantID) %>%  summarise_each(funs(mean)) %>% 
  select(participantID, expNum, queriedFirst, SPEmsec, correct)
yy<- xx %>% group_by(expNum,queriedFirst) %>% summarise_each(funs(mean,se=sd(.)/sqrt(n()))) 
yy


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
