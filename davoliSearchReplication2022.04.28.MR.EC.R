#... UPDATED TO CALCULATE THE 20% CUT-OFF
#... UPDATED TO REMOVE EXTREME SCORES <100, > 1500ms


#LOAD PJ TRIMMING ROUTLINE
#source("//Users/michael/Desktop/rFolder/pjTrimmingV2.R")  
source("C:/Users/e-car/Desktop/standingstudy/Experiment 6/Data/Current Data scripts being used/SmithStroopSharing/SmithStroopSharing/pjTrimmingV2.R")

#...OPEN DATA
#setwd("~/Documents/Research/papers/in progress/SmithDavoliReplication/2022.03.16/smithDavoliReplication02/sjDataFiles/")
setwd("C:/Users/e-car/Desktop/standingstudy/Experiment 6/Data/Current Data scripts being used/SmithSearchSharing/SmithSearchSharing/sjDataFiles")

#...get the list of files in the data folder
library(plyr)
file_list = list.files()

#...only keep the filenames that are "SJ" and ".txt"
library(stringr)
file_list = file_list[str_detect(file_list,pattern="SJ") & str_detect(file_list,pattern=".txt")]

#...OPEN THE INDIVIDIDUAL DATA FILES AND COMBINE THEM
mergedDataSet <- ldply(file_list, 
                         read.delim, 
                         header=FALSE, 
                         stringsAsFactors = FALSE,
                         sep = "")  #for each item in the list apply the function read.delim

View(mergedDataSet)  #...CHECK TO MAKE SURE THE DATA OPENNED CORRECTLY

#..ADD HEADERS
names(mergedDataSet) = c("sj",
                         "cb",
                         "blockNumber",
                         "blockType",
                         "trialNum",
                         "target",
                         "targetImage",
                         "distractor",
                         "distractorImage",
                         "posture",
                         "setSize",
                         "rt",
                         "resp",
                         "cresp",
                         "ac")

#...look at unique values from both columns
unique(mergedDataSet[c('sj')])
unique(mergedDataSet[c('blockType')])

#..DOES EACH SUBJECT HAVE THE SAME NUMBER OF TRIALS
ftable(blockType~sj, mergedDataSet)


#...DO WE HAVE EQUAL OBSERVATIONS FOR EACH COUNTERBALANCE
ftable(blockType~cb, mergedDataSet)

#...LOOK FOR MISSING DATA
mergedDataSet[!complete.cases(mergedDataSet),]

#... GET RID OF PRACTICE TRIALS

mergedDataSet <- mergedDataSet[!mergedDataSet$blockType=="practice",]


#.... CHECK TRIALS PER CONDITION
ftable(posture+target+distractor+setSize~sj, mergedDataSet)

#... UNLIKE THE STROOP, PARTICIPANTS WERE ALLOWED TO TAKE LONGER THAN 1500MS BUT WERE GIVEN A WARNING
#... TRIALS LONGER THAN 1500 MS will be considered errors (i.e., they will be dropped in RT but kept in PE ANALYSES)
#... Set values in the ac column to 0 on trials where a response is > = 1500


#...check that only experimental trials are left
unique(mergedDataSet$blockType)

#...count trials
totalTrials = dim(mergedDataSet)[1]
observationData = data.frame(ftable(blockType~sj, mergedDataSet))[,c(1,3)]
View(observationData)

#...get the number of extreme trials <100 - anticipatory or fast responses 
mergedDataSet= mergedDataSet[!mergedDataSet$rt<=100,]
validRTTrials = dim(mergedDataSet)[1]
observationData$validTrials = data.frame(ftable(blockType~sj, mergedDataSet))[,c(3)]

print(paste("percent invalid trials = ", ((totalTrials-validRTTrials)/totalTrials)*100))

#...this code changes the 1550ms+ trials into errors
mergedDataSet$ac[mergedDataSet$rt>=1500] = 0
mergedDataSet

#...get mean error data 
sjDataPE = ddply(mergedDataSet, 
                 .(sj,cb,setSize, posture), 
                 summarise, 
                 meanPE = 100 - (mean(ac)*100))

head(sjDataPE)
#View(sjDataPE)


mergedDataSet = mergedDataSet[mergedDataSet$ac ==1,]
errorsRemoved = dim(mergedDataSet)[1]
observationData$correctTrials = data.frame(ftable(blockType~sj, mergedDataSet))[,c(3)]


trimInfo = data.frame(totalTrials, validRTTrials, errorsRemoved)
head(trimInfo)

#...percent of trials that timed out
print(paste("percent invalid trials (Fast/slow) out errors: ",round(((totalTrials-validRTTrials)/totalTrials)*100,3)))

#...percent of error trials lost
print(paste("percent of error trials lost: ",round(((validRTTrials-errorsRemoved)/totalTrials)*100,3)))

#write.table(trimInfo, "trimInfo.txt",row.names = FALSE)

#################################
#...CHECK 20% CRITERION
#################################
observationData$percentLoss = ((observationData$Freq-observationData$correctTrials)/observationData$Freq)*100
observationData$sj[observationData$percentLoss>20]
#...None!


#...GET RT DATA
#...RUN TRIMMING PROCEDURE
tempList = pjRecursiveTrim2(mergedDataSet, #...dataset
                            "rt", #...dependent variables
                            c("sj", 
                              "cb",
                              "setSize",
                              "posture")) #.independent variables

trimmedData=tempList[[1]]
totalN = tempList[[2]]
rejected = tempList[[3]]
percentTrimmed = tempList[[4]]
Ncells = tempList[[5]]

print(paste("Percent of outliers removed: ",round(percentTrimmed,3)))

#...get the trimming info
output.out= data.frame(totalN, rejected,percentTrimmed,Ncells)
head(output.out)

write.table(trimmedData,"davoliRepTrimmedData.txt",row.names = FALSE)

#...Get the cell means for each subject
library(plyr)
sjDataRT = ddply(trimmedData, 
                 .(sj, cb, setSize,posture), 
                 summarise, 
                 meanRT = mean(rt))

#...combine the RT and error data
sjData = cbind(sjDataRT,meanPE =sjDataPE$meanPE)


str(sjData)

#...set as factors
sjData$sj = factor(sjData$sj)
sjData$cb = factor(sjData$cb)
sjData$setSize = factor(sjData$setSize)
sjData$posterFactor = factor(sjData$posture)
summary(sjData$cb)

# ANOVA RT
options(scipen= 999)
library(ez)

rtModel <- ezANOVA(sjData, 
                   dv = .(meanRT),
                   wid=.(sj),
                   within=.(posterFactor,setSize), 
                   detailed=TRUE, 
                   type=3,
                   return_aov=TRUE)

rtModel$ANOVA
#...print ANOVA in nice format
paste(rtModel$ANOVA$Effect,":  F(",
      rtModel$ANOVA$DFn,
      ", ",
      rtModel$ANOVA$DFd,
      ") = ",
      round(rtModel$ANOVA$F,3),
      ", MSE = ",
      round(rtModel$ANOVA$SSd/rtModel$ANOVA$DFd,3),
      ", p = ",
      round(rtModel$ANOVA$p,3),
      ", partialEtaSq = ",
      round(rtModel$ANOVA$SSn/(rtModel$ANOVA$SSn+rtModel$ANOVA$SSd),4),sep="")

#ANOVA PE

rtModel <- ezANOVA(sjData, 
                   dv = .(meanPE),
                   wid=.(sj),
                   within=.(posterFactor,setSize), 
                   detailed=TRUE, 
                   type=3,
                   return_aov = TRUE)

paste(rtModel$ANOVA$Effect,":  F(",
      rtModel$ANOVA$DFn,
      ", ",
      rtModel$ANOVA$DFd,
      ") = ",
      round(rtModel$ANOVA$F,3),
      ", MSE = ",
      round(rtModel$ANOVA$SSd/rtModel$ANOVA$DFd,3),
      ", p = ",
      round(rtModel$ANOVA$p,3),
      ", partialEtaSq = ",
      round(rtModel$ANOVA$SSn/(rtModel$ANOVA$SSn+rtModel$ANOVA$SSd),4),sep="")


#...CALCULATE THE BAYES FACTORS FOR THE RT ANALYSIS
library(BayesFactor)
bfValues = anovaBF(meanRT~setSize*posterFactor+sj,
                   data = sjData,
                   whichRandom = "sj", 
                   method="laplace")
bfValues
warnings()
#...get the Bayes factor for the Null Interaction
bfValues[3]/bfValues[4]
1/(bfValues[3]/bfValues[4])

###############################################
# GET DIFFERENCE SCORES - SEARCH RATE
##############################################

library(reshape2)
str(sjData)
wideData = dcast(sjData, #the name of the dataframe you want to reshape
                 sj+cb #row variables 
                 ~posture+setSize, #row variables ~ column variables
                 value.var = "meanRT") 
View(wideData)

wideData$sittingEffect = (wideData$SITTING_8-wideData$SITTING_4)/4
wideData$standingEffect = (wideData$STANDING_8-wideData$STANDING_4)/4
wideData$interaction = wideData$sittingEffect - wideData$standingEffect

searchratestand = mean(wideData$standingEffect)  #...search rate in standing condition
searchratesit = mean(wideData$sittingEffect)  #...search rate in the sitting condition

searchratestand
searchratesit

t.test(wideData$standingEffect)
t.test(wideData$sittingEffect)

#...SIGN TEST
binom.test(length(wideData$interaction[wideData$interaction>=0]),length(unique(sjData$sj)))

#... for the errrors
wideData = dcast(sjData, #the name of the dataframe you want to reshape
                 sj+cb #row variables 
                 ~posture+setSize, #row variables ~ column variables
                 value.var = "meanPE") 
View(wideData)

wideData$sittingEffect = (wideData$SITTING_8-wideData$SITTING_4)/4
wideData$standingEffect = (wideData$STANDING_8-wideData$STANDING_4)/4
wideData$interaction = wideData$sittingEffect - wideData$standingEffect

searchratestand = mean(wideData$standingEffect)  #...search rate in standing condition
searchratesit = mean(wideData$sittingEffect)  #...search rate in the sitting condition

searchratestand
searchratesit

t.test(wideData$standingEffect)
t.test(wideData$sittingEffect)
###############################################
#Figure
##############################################

library(psych)

graphRT = describeBy(sjData$meanRT, list(sjData$posture,sjData$setSize), mat=TRUE, digits = 1)
graphPE = describeBy(sjData$meanPE, list(sjData$posture,sjData$setSize), mat=TRUE, digits = 1)

graphRT = graphRT[,c("group1","group2","mean","se")]
graphPE = graphPE[,c("group1","group2","mean","se")]

names(graphRT) = c("posture","setSize","mean","se")
names(graphPE) = c("posture","setSize","mean","se")

library(stringr)
graphRT$posture = str_to_upper(graphRT$posture)

###################################################
#..calculate the within subjects confidence intervals based on loftus and masson
#..the confidence intervals are based on the interaction term.
###################################################

graphRT$se = sqrt((727.64)/length(unique(sjData$sj)))  #..manually enter the values from the ANOVA.
graphPE$se= sqrt((4.452)/length(unique(sjData$sj))) #..manually enter the values from the ANOVA.

###################################################
#..calculate the within subjects confidence intervals based on loftus and masson
#..the confidence intervals are based on the interaction term.
###################################################



critT = qt(p=.025,df=length(unique(sjData$sj))-2,lower.tail =FALSE)

#---add the min and max for the confidence intervals
graphRT$min = graphRT$mean - (graphRT$se*critT)
graphRT$max = graphRT$mean + (graphRT$se*critT)

####GET AC DATA FROM twoAnimalWordsPRPac.R
graphRT$ac = paste("(",format(round(graphPE$mean,digits=1),nsmall = 1),")",sep="")
head(graphRT)
graphRT$vAdj = 35 #down
graphRT$vAdj[graphRT$setSize=="incongruent"]=35 #up
graphRT$hAdj = 0 #right
#graphRT$hAdj[graphRT$posture=="SITTING"]=-60 #left

graphRT$congruency = factor(graphRT$setSize,labels = c("4","8"))
View(graphRT)


library(ggplot2)
interactionPlot <- ggplot(graphRT, aes(setSize, mean, group=posture))
interactionPlot + 
  theme(legend.position = "none")+
  scale_fill_manual(values=c("#FFFFFF","#999999","#FFFFFF","#999999"))+
  coord_cartesian(ylim=c(500,950),expand=TRUE)+
  scale_y_continuous(breaks = round(seq(500, 950, by = 100),0))+
  geom_text(aes(label=ac),nudge_x=graphRT$hAdj,nudge_y =graphRT$vAdj,size=5)+
  geom_bar(stat="identity",
           aes(fill=interaction(setSize)),colour="black")+
  geom_errorbar(aes(ymin=min,ymax=max,group=interaction(posture,setSize)),
                width=.1
                #position =position_dodge(width=50)
  )+
  
  labs(x = "Set Size", y = "Response Time (ms)") +
  theme(axis.ticks.x = element_line(size = 1, 
                                    colour = "black",
                                    linetype = "solid"))+
  theme(axis.ticks.y = element_line(size = 1, 
                                    colour = "black",
                                    linetype = "solid"))+
  theme(axis.ticks.length = unit(.25,"cm"))+
  theme(axis.line.x = element_line(size = 1, 
                                   colour = "black",
                                   linetype = "solid"))+
  theme(axis.line.y = element_line(size = 1, 
                                   colour = "black",
                                   linetype = "solid"))+
  theme(panel.background = element_rect(fill = "white", 
                                        colour = "black", 
                                        size = 1))+
  facet_grid(~posture)+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=22,face="bold"))+
  theme(strip.text.x = element_text(size = 20, face = "bold",colour = "black", angle = 0),
        strip.text.y = element_text(size = 20, face = "bold",colour = "black", angle = 0),
        strip.background = element_rect(fill=NA,colour="NA",size = 2))



# 3-WAY ANOVA RT
options(scipen= 999)
library(ez)

rtModel <- ezANOVA(sjData, 
                   dv = .(meanRT),
                   wid=.(sj),
                   within=.(posterFactor,setSize),
                   between = .(cb),
                   detailed=TRUE, 
                   type=3,
                   return_aov=TRUE)

rtModel$ANOVA
#...print ANOVA in nice format
paste(rtModel$ANOVA$Effect,":  F(",
      rtModel$ANOVA$DFn,
      ", ",
      rtModel$ANOVA$DFd,
      ") = ",
      round(rtModel$ANOVA$F,3),
      ", MSE = ",
      round(rtModel$ANOVA$SSd/rtModel$ANOVA$DFd,3),
      ", p = ",
      round(rtModel$ANOVA$p,3),
      ", partialEtaSq = ",
      round(rtModel$ANOVA$SSn/(rtModel$ANOVA$SSn+rtModel$ANOVA$SSd),4),sep="")

#ANOVA PE

rtModel <- ezANOVA(sjData, 
                   dv = .(meanPE),
                   wid=.(sj),
                   within=.(posterFactor,setSize),
                   between = .(cb),
                   detailed=TRUE, 
                   type=3,
                   return_aov = TRUE)

paste(rtModel$ANOVA$Effect,":  F(",
      rtModel$ANOVA$DFn,
      ", ",
      rtModel$ANOVA$DFd,
      ") = ",
      round(rtModel$ANOVA$F,3),
      ", MSE = ",
      round(rtModel$ANOVA$SSd/rtModel$ANOVA$DFd,3),
      ", p = ",
      round(rtModel$ANOVA$p,3),
      ", partialEtaSq = ",
      round(rtModel$ANOVA$SSn/(rtModel$ANOVA$SSn+rtModel$ANOVA$SSd),4),sep="")


#...CALCULATE THE BAYES FACTORS FOR THE RT ANALYSIS
library(BayesFactor)
bfValues = anovaBF(meanRT~cb*setSize*posterFactor+sj,
                   data = sjData,
                   whichRandom = "sj", 
                   method="laplace")
bfValues
warnings()
#...get the Bayes factor for the Null Interaction
bfValues[7]/bfValues[13]
1/(bfValues[7]/bfValues[13])
