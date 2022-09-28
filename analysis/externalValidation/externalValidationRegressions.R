# Rouault*, Seow*, Gillan and Fleming. (2018) Biological Psychiatry
# Psychiatric symptom dimensions are associated with dissociable shifts in metacognition but not task performance.

# Figures for regression data and factor analysis in Experiment 2

################################################################################
## LOAD THE PREDICTED FACTOR SCORES FOR ROUAULT BASED ON THE REDUCED ITEMS    ##
## PUT THESE INTO THE REGRESSIONS AND SEE IF SAME RESULTS                     ##
################################################################################

## CLEAR ALL ##
rm(list=ls())

## LOADING LIBRARIES/TOOL ##
library(ggplot2) # for plotting graphs
library(gridExtra) # for ploting graphs
library(lme4) # for linear regression functions
library(plyr) # for collapse-and-mean functions like ddply
library(psych)
library(GPArotation)
library(paran)
library(reshape)
library(polycor)
library(nFactors)
library(R.matlab)
library(reshape)
library(doBy)
options(scipen = 999) # to display variable quantities in decimals (not in scientific notation format)

## SET DIRECTORY  ##
setwd('/Users/alexhopkins/Dropbox (Royal Holloway)/ESRC_NIG/RTMAT/')
baseDir <- '/Users/alexhopkins/Dropbox (Royal Holloway)/ESRC_NIG/RTMAT/'

## LOADING DATA ##
qnData = readMat("analysis/metacognition/ME_phase2_excludqnadata_all.mat") # load questionnaire data
taskData = readMat("analysis/metacognition/ME_phase2_excludanalyseddat_all.mat") # load task performance data
HDDM = read.csv('analysis/metacognition/subjParams_2k_3chain.csv') # load HDDM data
HDDMpara = data.frame(t(HDDM[1:nrow(HDDM),2:length(HDDM)]))
colnames(HDDMpara) <- c("a", "t", "v_inter", "v_delta")

## LOAD PREDICTED FACTOR SCORES
factorScores = read.csv('data/predictedFactorScoresFinal.csv') # load HDDM data
qnsRou <- factorScores[1827:2322,1:length(factorScores)]

rouaultItems = read.csv('data/raw/RouaultSeow_BP.csv') # load HDDM data
ids <- rouaultItems$subid
qnsRou$id <- ids
qnsRou$X <- NULL
qnsRou$Subject <- NULL

## CREATE EMPTY OBJECTS ##
# create objects for variables from task performance data
id<-matrix(0,length(taskData$analyseddata),1) # subject id
age<-matrix(0,length(taskData$analyseddata),1)
gender<-matrix(0,length(taskData$analyseddata),1)
accuracy<-matrix(0,length(taskData$analyseddata),1) # accuracy
mRatio<-matrix(0,length(taskData$analyseddata),1)   # metacognitive efficiency
confMean<-matrix(0,length(taskData$analyseddata),1) # mean confidence

# create objects for variables from task questionnaire data
qnid<-matrix(0,length(qnData$allqna),1) # subject id
zung<-matrix(0,length(qnData$allqna),1)
anxiety<-matrix(0,length(qnData$allqna),1)
ocir<-matrix(0,length(qnData$allqna),1)
leb<-matrix(0,length(qnData$allqna),1)
iq<-matrix(0,length(qnData$allqna),1)
bis<-matrix(0,length(qnData$allqna),1)
schizo<-matrix(0,length(qnData$allqna),1)
eat<-matrix(0,length(qnData$allqna),1)
apathy<-matrix(0,length(qnData$allqna),1)
alcohol<-matrix(0,length(qnData$allqna),1)

## EXTRACTING DATA ##
# extracting data from allqna data file
# loop over for all subjects
for (i in 1:length(qnData$allqna)) 
{
  qnid[i] = qnData$allqna[[i]][[1]][,,1]$id
  zung[i] = qnData$allqna[[i]][[1]][,,1]$zung[,,1]$score #first brackets is subject number
  anxiety[i] = qnData$allqna[[i]][[1]][,,1]$anxiety[,,1]$score
  ocir[i] = qnData$allqna[[i]][[1]][,,1]$ocir[,,1]$score
  leb[i] = qnData$allqna[[i]][[1]][,,1]$leb[,,1]$score
  iq[i] = qnData$allqna[[i]][[1]][,,1]$iq[,,1]$score
  bis[i] = qnData$allqna[[i]][[1]][,,1]$bis[,,1]$score[,,1]$total
  schizo[i] = qnData$allqna[[i]][[1]][,,1]$schizo[,,1]$score[,,1]$total
  eat[i] = qnData$allqna[[i]][[1]][,,1]$eat[,,1]$score[,,1]$total
  apathy[i] = qnData$allqna[[i]][[1]][,,1]$apathy[,,1]$score
  alcohol[i] = qnData$allqna[[i]][[1]][,,1]$alcohol[,,1]$score
}

# extracting data from analysed data
# loop over for all subjects
for (i in 1:length(taskData$analyseddata))
{
  id[i]=taskData$analyseddata[[i]][[1]][,,1]$data[1,4]
  age[i] =taskData$analyseddata[[i]][[1]][,,1]$data[1,2]
  gender[i]=taskData$analyseddata[[i]][[1]][,,1]$data[1,3]
  confMean[i] = mean(taskData$analyseddata[[i]][[1]][,,1]$data[,9])
  accuracy[i] = mean(taskData$analyseddata[[i]][[1]][,,1]$data[,6])
  mRatio[i] = taskData$analyseddata[[i]][[1]][,,1]$mratio
}

# set gender as factor (male or female)
gender <- factor(gender)

## MERGING DATA ##

# create dataframe to store questionnaire data
qnFrame = data.frame(qnid, anxiety, eat, apathy, alcohol, zung, ocir, leb, iq, bis, schizo)
# create dataframe to store task performance data
taskFrame = data.frame(id,age,gender,confMean,accuracy,mRatio)
# merge all data together into one data frame
allData =merge(taskFrame, qnFrame,by.x=c("id"), by.y=c("qnid"))
# join HDDM variables to existing dataframe
allData=data.frame(allData,HDDMpara)

## SCALING DATA ##
#scaling the task performance
allData$age.sc = scale(allData$age)
allData$confMean.sc = scale(allData$confMean)
allData$accuracy.sc = scale(allData$accuracy)

# scaling the questionnaire scores
allData$zung.sc = scale(log(allData$zung))
allData$anxiety.sc = scale(log(allData$anxiety))
allData$ocir.sc = scale(log(allData$ocir+1))
allData$leb.sc = scale(log(allData$leb+1))
allData$iq.sc = scale(allData$iq)
allData$schizo.sc = scale(log(allData$schizo+1))
allData$bis.sc = scale(log(allData$bis))
allData$eat.sc = scale(log(allData$eat+1))
allData$apathy.sc = scale(log(allData$apathy))
allData$alcohol.sc = scale(log(allData$alcohol+1))

# scale HDDM variables
allData$a.sc = scale(allData$a)
allData$t.sc = scale(allData$t)
allData$v_inter.sc = scale(allData$v_inter)
allData$v_delta.sc = scale(allData$v_delta)

#exclude negative mRatios and scale the mRatios of the subjects left
mrExcludedData <- allData[allData$mRatio>0,] 
mrExcludedData$mRatio.sc = scale(log(mrExcludedData$mRatio))

##  FACTOR ANALYSIS ##
# LOAD ALL QUESTIONNAIRE (individual questions) DATA
# create objects
qnIndivid<-matrix(0,length(qnData$allqna),1)
zungAll<-matrix(0,length(qnData$allqna),length(qnData$allqna[[1]][[1]][,,1]$zung[,,1]$raw))
anxietyAll<-matrix(0,length(qnData$allqna),length(qnData$allqna[[1]][[1]][,,1]$anxiety[,,1]$raw))
ocirAll<-matrix(0,length(qnData$allqna),length(qnData$allqna[[1]][[1]][,,1]$ocir[,,1]$raw))
lebAll<-matrix(0,length(qnData$allqna),length(qnData$allqna[[1]][[1]][,,1]$leb[,,1]$raw[,,1]$avg))
bisAll<-matrix(0,length(qnData$allqna),length(qnData$allqna[[1]][[1]][,,1]$bis[,,1]$raw))
schizoAll<-matrix(0,length(qnData$allqna),length(qnData$allqna[[1]][[1]][,,1]$schizo[,,1]$raw))
eatAll<-matrix(0,length(qnData$allqna),length(qnData$allqna[[1]][[1]][,,1]$eat[,,1]$raw))
apathyAll<-matrix(0,length(qnData$allqna),length(qnData$allqna[[1]][[1]][,,1]$apathy[,,1]$raw))
alcoholAll<-matrix(0,length(qnData$allqna),length(qnData$allqna[[1]][[1]][,,1]$alcohol[,,1]$raw))

# extracting data from allqna
for (i in 1:length(qnData$allqna))
{
  qnIndivid[i,]=qnData$allqna[[i]][[1]][,,1]$id
  zungAll[i,] = qnData$allqna[[i]][[1]][,,1]$zung[,,1]$raw #first brackets is subject number
  anxietyAll[i,] = t(qnData$allqna[[i]][[1]][,,1]$anxiety[,,1]$raw)
  ocirAll[i,] = qnData$allqna[[i]][[1]][,,1]$ocir[,,1]$raw
  lebAll[i,] = (qnData$allqna[[i]][[1]][,,1]$leb[,,1]$raw[,,1]$avg)
  bisAll[i,] = qnData$allqna[[i]][[1]][,,1]$bis[,,1]$raw
  schizoAll[i,] = qnData$allqna[[i]][[1]][,,1]$schizo[,,1]$raw
  eatAll[i,]=qnData$allqna[[i]][[1]][,,1]$eat[,,1]$raw
  apathyAll[i,]=qnData$allqna[[i]][[1]][,,1]$apathy[,,1]$raw
  alcoholAll[i,]=qnData$allqna[[i]][[1]][,,1]$alcohol[,,1]$raw
}

qns = data.frame("qnid"=qnIndivid,"zung"=zungAll, "anxiety"=anxietyAll,"ocir"= ocirAll, "leb" =lebAll,"bis"= bisAll,"schizo"= schizoAll, 'alcohol'=alcoholAll,'eat'=eatAll,'apathy'=apathyAll)

################################################################
################################################################
# REGRESSIONS 5) PERFORMANCE/METCOG/HDDM ~ FACTOR SCORES  ######
################################################################
################################################################
# Linear regressions for factor scores with task performance & HDDM variables of 
# 1) performance/ accuracy
# 2) mean confidence
# 3) m ratio

#centre the predicted scores as marions are centered
qnsRou$AD <- scale(qnsRou$AD)
qnsRou$Compul<- scale(qnsRou$Compul)
qnsRou$SW<- scale(qnsRou$SW)

# Add predicted factor scores to data frame 
factorData = merge(allData, qnsRou, by.x = c("id"), by.y = c("id"))

# exclude mRatio < 0 and scale it
mrExcludFactorData <- factorData[factorData$mRatio>0,]
mrExcludFactorData$mRatio.sc = scale(log(mrExcludFactorData$mRatio))

# linear regressions
accuFactorReg= lm(accuracy.sc~AD+Compul+SW+iq.sc+age.sc+gender,factorData) # accuracy
confMeanFactorReg= lm(confMean.sc~AD+Compul+SW+iq.sc+age.sc+gender,factorData) # mean confidence
mRatioFactorReg= lm(mRatio.sc~AD+Compul+SW+iq.sc+age.sc+gender,mrExcludFactorData) #mRatio
aFactorReg= lm(a.sc~AD+Compul+SW+iq.sc+age.sc+gender,factorData) # a
tFactorRreg= lm(t.sc~AD+Compul+SW+iq.sc+age.sc+gender,factorData) # t
vDeltaFactorReg= lm(v_delta.sc~AD+Compul+SW+iq.sc+age.sc+gender,factorData) # v delta
vInterFactorReg= lm(v_inter.sc~AD+Compul+SW+iq.sc+age.sc+gender,factorData)  #v intercept

# test magnitudes of contrasts for mean confidence
lambda1 <- c(0,1,0,0,0,0,0)
esticon(confMeanFactorReg, lambda1, beta0=0)

# extract coefficients into dataframes
accuFactorRegFig <- data.frame(summary(accuFactorReg)$coefficients[2:4,1:4])
confMeanFactorRegFig <- data.frame(summary(confMeanFactorReg)$coefficients[2:4,1:4])
mRatioFactorRegFig <- data.frame(summary(mRatioFactorReg)$coefficients[2:4,1:4])
tFactorRregFig <- data.frame(summary(tFactorRreg)$coefficients[2:4,1:4])
vDeltaFactorRegFig <- data.frame(summary(vDeltaFactorReg)$coefficients[2:4,1:4])
aFactorRegFig <- data.frame(summary(aFactorReg)$coefficients[2:4,1:4])
vInterFactorRegFig <- data.frame(summary(vInterFactorReg)$coefficients[2:4,1:4])


############################################################
############################################################
# PLOT 3) PERFORMANCE/METACOG/HDDM ~ FACTOR SCORES #########
############################################################
############################################################

# set and label dataframe to plot
accuFactorRegFig$Label<- 'Accuracy'
accuFactorRegFig$Type<-rownames(accuFactorRegFig)
summary(accuFactorReg)
tFactorRregFig$Label<- 't'
tFactorRregFig$Type<-rownames(tFactorRregFig)
vDeltaFactorRegFig$Label<- 'v delta'
vDeltaFactorRegFig$Type<-rownames(vDeltaFactorRegFig)
aFactorRegFig$Label<- 'a'
aFactorRegFig$Type<-rownames(aFactorRegFig)
vInterFactorRegFig$Label<- 'v intercept'
vInterFactorRegFig$Type<-rownames(vInterFactorRegFig)
confMeanFactorRegFig$Type<-rownames(confMeanFactorRegFig)
confMeanFactorRegFig$Label<- 'Confidence Level'
summary(confMeanFactorReg)
mRatioFactorRegFig$Type<-rownames(mRatioFactorRegFig)
mRatioFactorRegFig$Label<- 'Metacognitive Efficiency'
summary(mRatioFactorReg)


# Plot: Task performance + HDDM + Metacognition ~ Factor Scores
factorRegFig<-rbind(accuFactorRegFig,tFactorRregFig,vInterFactorRegFig,vDeltaFactorRegFig,aFactorRegFig,confMeanFactorRegFig,mRatioFactorRegFig)
factorRegFig$Label<-factor(factorRegFig$Label, levels=c("Accuracy","t","v intercept", "v delta",'a','Confidence Level','Metacognitive Efficiency'))

factorRegFig<-rbind(accuFactorRegFig,confMeanFactorRegFig,mRatioFactorRegFig)
factorRegFig$Label<-factor(factorRegFig$Label, levels=c("Accuracy",'Confidence Level','Metacognitive Efficiency'))
factorRegFig$Type[factorRegFig$Type=="AD"]<-"Anxious"
factorRegFig$Type[factorRegFig$Type=="Compul"]<-"Compulsivity"
factorRegFig$Type[factorRegFig$Type=="SW"]<-"Social Withdrawal"

factorFig <- ggplot(data = factorRegFig, aes(x = Label, y = Estimate, group=Type)) +       
  geom_bar(aes(fill = Type), colour="black",size=1.2,stat="identity", position = "dodge",width=0.8) +
  geom_errorbar(aes(ymin=factorRegFig$Estimate-factorRegFig$Std..Error, ymax=factorRegFig$Estimate+factorRegFig$Std..Error),colour="black", width=0.3, size=1.2, position=position_dodge(.8))+
  geom_hline(yintercept=0,size=1) + theme_classic() + labs(title=" ", x=" ", y = "Regression Coefficient") +
  theme(axis.title.y = element_text(size = rel(2.5), angle = 90, margin=margin(0,20,0,0)), axis.title.x = element_text(size = rel(3), angle = 00, margin=margin(20,0,0,0)))+
  theme(plot.title = element_text(size = rel(3), angle = 00),legend.text = element_text(size = 20),legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 00, size=20), axis.text.y = element_text(angle = 00, size=25))+ scale_x_discrete(expand = c(0,0.5)) +
  theme(axis.line.x = element_line(color="black", size = 1.2), axis.line.y = element_line(color="black", size = 1.2)) +
  theme(axis.ticks.y=element_line(size=(1.5)), axis.ticks.x=element_line(size=(1.5)), axis.ticks.length=unit(0.4, "cm")) +
  scale_fill_manual(values=c("#8dd3c7", "#ffffbc","#bebada")) + theme(legend.position="none") + ylim(-0.3,0.3)
factorFig
ggsave(factorFig, file=paste(baseDir, 'figs/marionRegsFinal.eps', sep=''), device="eps")

############################################################
############################################################
# ADD MARIONS ORIGINAL RESULTS TO PLOT #########
############################################################
############################################################

# DO FACTOR ANALYSIS ON RAW QUESTIONAIRRE SCORES
# Produce covariance matrix using hetcor to account for both continuous and binary correlations
het.mat <- hetcor(qns[,2:length(qns)])$cor

fa <- fa(r = het.mat, nfactors = 3, n.obs = nrow(qns), rotate = "oblimin", fm="ml", scores="regression")
fa.scores <- factor.scores(x=qns[,2:length(qns)], f=fa)
scoresOriginal = data.frame("id"=qns$qnid, fa.scores$scores)
loadings <- fa$loadings

# loadings plot m2= sa, m1 = a&d, m4 = impul, m3 = compul (THIS CAN CHANGE W THE FACTOR ANAYLSIS)
colnames(scoresOriginal) <- c("id", "AD", "Compul", "SW")
factorData2 =merge(allData, scoresOriginal,by.x=c("id"), by.y=c("id")) #join factor scores with main data matrix
factorData2<- subset(factorData2,id != 8355082 ) #remove subject excluded from predicted score data


# exclude mRatio < 0 and scale it
mrExcludFactorData2 <- factorData2[factorData2$mRatio>0,]
mrExcludFactorData2$mRatio.sc = scale(log(mrExcludFactorData2$mRatio))

# linear regressions
accuFactorReg2= lm(accuracy.sc~AD+Compul+SW+iq.sc+age.sc+gender,factorData2) # accuracy
confMeanFactorReg2= lm(confMean.sc~AD+Compul+SW+iq.sc+age.sc+gender,factorData2) # mean confidence
mRatioFactorReg2= lm(mRatio.sc~AD+Compul+SW+iq.sc+age.sc+gender,mrExcludFactorData2) #mRatio

# test magnitudes of contrasts for mean confidence
lambda1 <- c(0,1,0,0,0,0,0)
esticon(confMeanFactorReg2, lambda1, beta0=0)

# extract coefficients into dataframes
accuFactorRegFig2 <- data.frame(summary(accuFactorReg2)$coefficients[2:4,1:4])
confMeanFactorRegFig2 <- data.frame(summary(confMeanFactorReg2)$coefficients[2:4,1:4])
mRatioFactorRegFig2 <- data.frame(summary(mRatioFactorReg2)$coefficients[2:4,1:4])

# set and label dataframe to plot
accuFactorRegFig2$Label<- 'Accuracy'
accuFactorRegFig2$Type<-rownames(accuFactorRegFig2)
confMeanFactorRegFig2$Type<-rownames(confMeanFactorRegFig2)
confMeanFactorRegFig2$Label<- 'Confidence Level'
mRatioFactorRegFig2$Type<-rownames(mRatioFactorRegFig2)
mRatioFactorRegFig2$Label<- 'Metacognitive Efficiency'

# Plot: Task performance + HDDM + Metacognition ~ Factor Scores
factorRegFig2<-rbind(accuFactorRegFig2,confMeanFactorRegFig2,mRatioFactorRegFig2)
factorRegFig2$Label<-factor(factorRegFig2$Label, levels=c("Accuracy",'Confidence Level','Metacognitive Efficiency'))

factorRegFig2<-rbind(accuFactorRegFig2,confMeanFactorRegFig2,mRatioFactorRegFig2)
factorRegFig2$Label<-factor(factorRegFig2$Label, levels=c("Accuracy",'Confidence Level','Metacognitive Efficiency'))
factorRegFig2$Type[factorRegFig$Type=="AD"]<-"Anxious"
factorRegFig2$Type[factorRegFig$Type=="Compul"]<-"Compulsivity"
factorRegFig2$Type[factorRegFig$Type=="SW"]<-"Social Withdrawal"

factorFig2 <- ggplot(data = factorRegFig2, aes(x = Label, y = Estimate, group=Type)) +       
  geom_bar(aes(fill = Type), colour="black",size=1.2,stat="identity", position = "dodge",width=0.8) +
  geom_errorbar(aes(ymin=factorRegFig2$Estimate-factorRegFig2$Std..Error, ymax=factorRegFig2$Estimate+factorRegFig2$Std..Error),colour="black", width=0.3, size=1.2, position=position_dodge(.8))+
  geom_hline(yintercept=0,size=1) + theme_classic() + labs(title=" ", x=" ", y = "Regression Coefficient") +
  theme(axis.title.y = element_text(size = rel(2.5), angle = 90, margin=margin(0,20,0,0)), axis.title.x = element_text(size = rel(3), angle = 00, margin=margin(20,0,0,0)))+
  theme(plot.title = element_text(size = rel(3), angle = 00),legend.text = element_text(size = 20),legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 00, size=20), axis.text.y = element_text(angle = 00, size=25))+ scale_x_discrete(expand = c(0,0.5)) +
  theme(axis.line.x = element_line(color="black", size = 1.2), axis.line.y = element_line(color="black", size = 1.2)) +
  theme(axis.ticks.y=element_line(size=(1.5)), axis.ticks.x=element_line(size=(1.5)), axis.ticks.length=unit(0.4, "cm")) +
  scale_fill_manual(values=c("#8dd3c7", "#ffffbc","#bebada")) + theme(legend.position="none") + ylim(-0.3,0.3)
factorFig2
ggsave(factorFig2, file=paste(baseDir, 'figs/marionRegsOriginal.eps', sep=''), device="eps")

library(ggpubr)
ggarrange(factorFig, factorFig2)


