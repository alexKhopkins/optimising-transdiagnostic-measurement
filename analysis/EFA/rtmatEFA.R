##########
## CLEAR ALL
rm(list=ls())

##########
## LOADING LIBRARIES/TOOL
# loading tools
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
library(gridExtra)

library(psych)
library(missMDA) # This loads also required package FactoMineR
library(nFactors)
library(lavaan)
library(sem)
library(corrplot)
library(caret)
library(car)
library(ppcor)

options(scipen = 999) # to display variable quantities in decimals (not in scientific notation format)

##########
## SET DIRECTORY
sewd <- getwd();       # find out where we are
if (grepl('alex',sewd)){ whoami <- 'alexmac'}
if (grepl('hopkins', sewd)){whoami <- 'alexWork'}
# Adjust the base directdory accordingly.  
switch(whoami,
       alexmac= {basedir <- '/Users/alexxxxkh/Dropbox/postDocRH/RTMAT/';},
       alexWork= {basedir <- '/Users/alexhopkins/Dropbox (Royal Holloway)/ESRC_NIG/RTMAT/';});
setwd(basedir);

####################################
############ SETTINGS ##########
####################################
reHetCor = 0 #recompute hetcor or not

####################################
############ LOADING DATA ##########
####################################
RTMATitems = read.csv(paste(basedir, 'data/raw/RTMATitems.csv', sep='')) # load items
qns = RTMATitems


# DO FACTOR ANALYSIS ON RAW QUESTIONAIRE SCORES
# Produce covariance matrix using hetcor to account for both continuous and binary correlations
if (reHetCor == 1) {
  het.matRTMAT <- hetcor(qns[,3:length(qns)])$cor
  save(het.matRTMAT, file = paste(basedir, 'data/hetMats/hetMatRTMAT.Rdata', sep=''))

} else if (reHetCor ==0) {
  load(paste(basedir, 'data/hetMats/hetMatRTMAT.Rdata', sep=''))
  
}

ev <- eigen(het.matRTMAT) # get eigenvalues
ap <- parallel(subject=nrow(qns[,3:length(qns)]),var=ncol(qns[,3:length(qns)]),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
sPlot <- plotnScree(nS, main='Scree Test solutions'); 
nCng(ev$values)

evRTMAT <- ev$values
write.csv(evRTMAT, paste(basedir, 'data/EFAev/evRTMAT.csv', sep=''))

ggsave(paste(basedir, 'figs/screePlotRTMAT.png', sep=''), sPlot, width = 10, height = 5)

# DO FACTOR ANALYSIS ON ALL QUESTIONNAIRE DATA
faRTMAT <- fa(r = het.matRTMAT, nfactors = 3, n.obs = nrow(qns), rotate = "oblimin", fm="ml", scores="regression")
faRTMAT.scores <- factor.scores(x=qns[,3:length(qns)], f=faRTMAT)
scores = data.frame("id"=qns$subid, faRTMAT.scores$scores)
colnames(scores) <- c("id", "AD", "Compul", "SW")
# colnames(scores) <- c("id", "AD", "SW", "Compul")

loadingsRTMAT <- faRTMAT$loadings
loadingsRTMATmat <- loadingsRTMAT[1:209, 1:3]
loadingsDfRTMAT <- data.frame(loadingsRTMATmat)
# colnames(loadingsDfRTMAT) <- c("AD", "SW", "Compul")
colnames(loadingsDfRTMAT) <- c("AD", "Compul", "SW")

# PLOT LOADINGS
x = 1:209
itemLengths <- c(20, 20, 18, 24, 30, 43, 10, 26, 18)
ml1plot <- ggplot(data = loadingsDfRTMAT, aes(y = AD, x = x, fill = as.factor(x))) + geom_bar(stat = 'identity') +
  scale_fill_manual(values = c(rep("#3BB471", 20), rep("#B9860A", 20), rep("#1C90FE", 18), rep("#BA55D3", 24), rep("#FF2F93", 30), rep("#EEE8AC", 43), rep("#FE6447", 10), rep("#46D1CC", 26), rep("#0227CD", 18) )) + 
  theme(legend.position = "None", panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(title="AD",
        x ="Item number", y = "Loadings")

ml2plot<- ggplot(data = loadingsDfRTMAT, aes(y = Compul, x = x, fill = as.factor(x))) + geom_bar(stat = 'identity') +
  scale_fill_manual(values = c(rep("#3BB471", 20), rep("#B9860A", 20), rep("#1C90FE", 18), rep("#BA55D3", 24), rep("#FF2F93", 30), rep("#EEE8AC", 43), rep("#FE6447", 10), rep("#46D1CC", 26), rep("#0227CD", 18) )) + 
  theme(legend.position = "None", panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title="Compul",
       x ="Item number", y = "Loadings")

ml3plot<- ggplot(data = loadingsDfRTMAT, aes(y = SW, x = x, fill = as.factor(x))) + geom_bar(stat = 'identity') +
  scale_fill_manual(values = c(rep("#3BB471", 20), rep("#B9860A", 20), rep("#1C90FE", 18), rep("#BA55D3", 24), rep("#FF2F93", 30), rep("#EEE8AC", 43), rep("#FE6447", 10), rep("#46D1CC", 26), rep("#0227CD", 18) )) + 
  theme(legend.position = "None", panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(title="SW",
       x ="Item number", y = "Loadings")

library(ggpubr)
top <- text_grob("RTMAT", size = 20, col="black")
mlAll <- grid.arrange(ml1plot, ml2plot, ml3plot, nrow=3, top = top)
ggsave(paste(basedir, 'figs/loadingsRTMAT.png', sep=''), mlAll, width = 10, height = 5)


# PLOT SCORE HISTOGRAMS
g1 <- ggplot(data = scores, aes(x = AD)) + geom_histogram(bins = 20, col="black", 
                                                          fill="grey", 
                                                          alpha=.2)
g2 <- ggplot(data = scores, aes(x = Compul)) + geom_histogram(bins = 20, col="black", 
                                                              fill="red", 
                                                              alpha=.2)
g3 <- ggplot(data = scores, aes(x = SW)) + geom_histogram(bins = 20, col="black", 
                                                          fill="blue", 
                                                          alpha=.2)
top <- text_grob("RTMAT", size = 20, col="black")
gAll <- grid.arrange(g1, g2, g3, ncol=3, top = top)
ggsave(paste(basedir, 'figs/factoreScoresHistsRTMAT.png', sep=''), gAll, width = 10, height = 5)

# SAVE FACTOR SCORES AND LOADINGS
write.csv(scores, paste(basedir, 'data/EFAscores/RTMATscores.csv', sep=''))
write.csv(loadingsDfRTMAT, paste(basedir, 'data/loadings/RTMATloadings.csv', sep=''))


# DO FACTOR ANALYSIS ON RAW QUESTIONAIRRE SCORES FOR EACH STUDY SEPARATELY
# Produce covariance matrix using hetcor to account for both continuous and binary correlations
qnsKel <- qns[1:1006,3:length(qns)]
IDsKel <- qns$subid[1:1006]
het.matKel <- hetcor(qnsKel)$cor

evKel <- eigen(het.matKel) # get eigenvalues
ap <- parallel(subject=nrow(qnsKel),var=ncol(qnsKel),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
sPlot <- plotnScree(nS, main='Scree Test solutions'); 
nCng(evKel$values)
ggsave(paste(basedir, 'figs/kelley/screePlotKelley.png', sep=''), sPlot, width = 10, height = 5)


faKel <- fa(r = het.matKel, nfactors = 3, n.obs = nrow(qnsKel), rotate = "oblimin", fm="ml", scores="regression")
faKel.scores <- factor.scores(x=qnsKel, f=faKel)
scoresKel = data.frame("id"=IDsKel, faKel.scores$scores)

colnames(scoresKel) <- c("id", "AD", "Compul", "SW")
# colnames(scores) <- c("id", "AD", "SW", "Compul")

loadingsKel <- faKel$loadings
loadingsKelMat <- loadingsKel[1:209, 1:3]
loadingsDfKel <- data.frame(loadingsKelMat)
colnames(loadingsDfKel) <- c("AD", "Compul", "SW")

# PLOT LOADINGS
x = 1:209
itemLengths <- c(20, 20, 18, 24, 30, 43, 10, 26, 18)
ml1plot <- ggplot(data = loadingsDfKel, aes(y = AD, x = x, fill = as.factor(x))) + geom_bar(stat = 'identity') +
  scale_fill_manual(values = c(rep("#3BB471", 20), rep("#B9860A", 20), rep("#1C90FE", 18), rep("#BA55D3", 24), rep("#FF2F93", 30), rep("#EEE8AC", 43), rep("#FE6447", 10), rep("#46D1CC", 26), rep("#0227CD", 18) )) + 
  theme(legend.position = "None", panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(title="AD",
       x ="Item number", y = "Loadings")

ml2plot<- ggplot(data = loadingsDfKel, aes(y = Compul, x = x, fill = as.factor(x))) + geom_bar(stat = 'identity') +
  scale_fill_manual(values = c(rep("#3BB471", 20), rep("#B9860A", 20), rep("#1C90FE", 18), rep("#BA55D3", 24), rep("#FF2F93", 30), rep("#EEE8AC", 43), rep("#FE6447", 10), rep("#46D1CC", 26), rep("#0227CD", 18) )) + 
  theme(legend.position = "None", panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title="Compul",
       x ="Item number", y = "Loadings")

ml3plot<- ggplot(data = loadingsDfKel, aes(y = SW, x = x, fill = as.factor(x))) + geom_bar(stat = 'identity') +
  scale_fill_manual(values = c(rep("#3BB471", 20), rep("#B9860A", 20), rep("#1C90FE", 18), rep("#BA55D3", 24), rep("#FF2F93", 30), rep("#EEE8AC", 43), rep("#FE6447", 10), rep("#46D1CC", 26), rep("#0227CD", 18) )) + 
  theme(legend.position = "None", panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(title="SW",
       x ="Item number", y = "Loadings")

top <- text_grob("Kelley", size = 20, col="black")
mlAll <- grid.arrange(ml1plot, ml2plot, ml3plot, nrow=3, top = top)
ggsave(paste(basedir, 'figs/kelley/loadingsKelley.png', sep=''), mlAll, width = 10, height = 5)


# PLOT SCORE HISTOGRAMS
g1 <- ggplot(data = scoresKel, aes(x = AD)) + geom_histogram(bins = 20, col="black", 
                                                          fill="grey", 
                                                          alpha=.2)
g2 <- ggplot(data = scoresKel, aes(x = Compul)) + geom_histogram(bins = 20, col="black", 
                                                              fill="red", 
                                                              alpha=.2)
g3 <- ggplot(data = scoresKel, aes(x = SW)) + geom_histogram(bins = 20, col="black", 
                                                          fill="blue", 
                                                          alpha=.2)


gAll <- grid.arrange(g1, g2, g3, ncol=3, top = top)
ggsave(paste(basedir, 'figs/kelley/factoreScoresHistsKelley.png', sep=''), gAll, width = 10, height = 5)

# SAVE FACTOR SCORES AND LOADINGS
write.csv(scoresKel, paste(basedir, 'data/EFAscores/KelleyScores.csv', sep=''))
write.csv(loadingsDfKel, paste(basedir, 'data/loadings/KelleyLoadings.csv', sep=''))


x = 1:length(evKel$values)
evKelDF <- data.frame(evKel$values)
ggplot(data = evKelDF, aes(y = evKel.values, x = x)) + geom_col()

x = 1:20
evDF <- data.frame(ev$values[1:20])
g1 <- ggplot(data = evDF, aes(y = ev.values.1.20., x = x, fill = as.factor(x))) + geom_bar(stat = 'identity') + 
  scale_fill_manual(values = c(rep("#9CE2EE", 3), rep("grey", 17))) + 
  labs(x = 'Factor number', y = 'Eigenvalue') + 
  theme(legend.position = "None", panel.background = element_blank(), axis.line = element_line(colour = "black"))

###################################
######### PATZELT STUDY ##########
###################################

qnsPat <- qns[1007:1826,3:length(qns)]
IDsPat <- qns$subid[1007:1826]

# load(paste(basedir, 'data/hetMatPat.Rdata', sep=''))
het.matPat <- hetcor(qnsPat)$cor

evPat <- eigen(het.matPat) # get eigenvalues
ap <- parallel(subject=nrow(qnsPat),var=ncol(qnsPat),
               rep=100,cent=.05)
nSpat <- nScree(x=evPat$values, aparallel=ap$eigen$qevpea)
sPlot <- plotnScree(nSpat, main='Scree Test solutions'); 
nCng(evPat$values)
ggsave(paste(basedir, 'figs/patzelt/screePlotPat.png', sep=''), sPlot, width = 10, height = 5)
 
faPat <- fa(r = het.matPat, nfactors = 3, n.obs = nrow(qns), rotate = "oblimin", fm="ml", scores="regression")
faPat.scores <- factor.scores(x=qnsPat, f=faPat)
scoresPat = data.frame("id"=IDsPat, faPat.scores$scores)
colnames(scoresPat) <- c("id", "AD", "Compul", "SW")

loadingsPat <- faPat$loadings
loadingsPatMat <- loadingsPat[1:209, 1:3]
loadingsDfPat <- data.frame(loadingsPatMat)
colnames(loadingsDfPat) <- c("AD", "Compul", "SW")

x = 1:209
itemLengths <- c(20, 20, 18, 24, 30, 43, 10, 26, 18)
ml1plot <- ggplot(data = loadingsDfPat, aes(y = AD, x = x, fill = as.factor(x))) + geom_bar(stat = 'identity') +
  scale_fill_manual(values = c(rep("#3BB471", 20), rep("#B9860A", 20), rep("#1C90FE", 18), rep("#BA55D3", 24), rep("#FF2F93", 30), rep("#EEE8AC", 43), rep("#FE6447", 10), rep("#46D1CC", 26), rep("#0227CD", 18) )) + 
  theme(legend.position = "None", panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(title="AD",
       x ="Item number", y = "Loadings")

ml2plot<- ggplot(data = loadingsDfPat, aes(y = Compul, x = x, fill = as.factor(x))) + geom_bar(stat = 'identity') +
  scale_fill_manual(values = c(rep("#3BB471", 20), rep("#B9860A", 20), rep("#1C90FE", 18), rep("#BA55D3", 24), rep("#FF2F93", 30), rep("#EEE8AC", 43), rep("#FE6447", 10), rep("#46D1CC", 26), rep("#0227CD", 18) )) + 
  theme(legend.position = "None", panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title="Compul",
       x ="Item number", y = "Loadings")

ml3plot<- ggplot(data = loadingsDfPat, aes(y = SW, x = x, fill = as.factor(x))) + geom_bar(stat = 'identity') +
  scale_fill_manual(values = c(rep("#3BB471", 20), rep("#B9860A", 20), rep("#1C90FE", 18), rep("#BA55D3", 24), rep("#FF2F93", 30), rep("#EEE8AC", 43), rep("#FE6447", 10), rep("#46D1CC", 26), rep("#0227CD", 18) )) + 
  theme(legend.position = "None", panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(title="SW",
       x ="Item number", y = "Loadings")

top <- text_grob("Patzelt", size = 20, col="black")
mlAll <- grid.arrange(ml1plot, ml2plot, ml3plot, nrow=3, top = top)
ggsave(paste(basedir, 'figs/patzelt/loadingsPat.png', sep=''), mlAll, width = 10, height = 5)

# PLOT SCORE HISTOGRAMS
g1 <- ggplot(data = scoresPat, aes(x = AD)) + geom_histogram(bins = 20, col="black", 
                                                             fill="grey", 
                                                             alpha=.2)
g2 <- ggplot(data = scoresPat, aes(x = Compul)) + geom_histogram(bins = 20, col="black", 
                                                                 fill="red", 
                                                                 alpha=.2)
g3 <- ggplot(data = scoresPat, aes(x = SW)) + geom_histogram(bins = 20, col="black", 
                                                             fill="blue", 
                                                             alpha=.2)

gAll <- grid.arrange(g1, g2, g3, ncol=3, top = top)
ggsave(paste(basedir, 'figs/patzelt/factoreScoresHistsPat.png', sep=''), gAll, width = 10, height = 5)

# SAVE FACTOR SCORES AND LOADINGS
write.csv(scoresPat, paste(basedir, 'data/EFAscores/PatScores.csv', sep=''))
write.csv(loadingsDfPat, paste(basedir, 'data/loadings/PatLoadings.csv', sep=''))


x = 1:20
evDFpat <- data.frame(evPat$values[1:20])

g2<- ggplot(data = evDFpat, aes(y = evPat.values.1.20., x = x, fill = as.factor(x))) + geom_bar(stat = 'identity') + 
  scale_fill_manual(values = c(rep("#9CE2EE", 3), rep("grey", 17))) + 
  labs(x = 'Factor number', y = 'Eigenvalue') + 
  theme(legend.position = "None", panel.background = element_blank(), axis.line = element_line(colour = "black"))

###################################
######### ROUAULT ##########
###################################
qnsRou <- qns[1827:2322,3:length(qns)]
IDsRou <- qns$subid[1827:2322]

# load(paste(basedir, 'data/hetMatR.Rdata', sep=''))

het.matR <- hetcor(qnsRou)$cor

# FIRST LOOK AT SCREE PLOT 
evN <- eigen(het.matR) # get eigenvalues
ap <- parallel(subject=nrow(qnsRou),var=ncol(qnsRou),
               rep=100,cent=.05)
nSn <- nScree(x=evN$values, aparallel=ap$eigen$qevpea)
sPlot <- plotnScree(nSn, main='Scree Test solutions'); 
nCng(evN$values)
ggsave(paste(basedir, 'figs/rouault/screePlotRou.png', sep=''), sPlot, width = 10, height = 5)


# DO EFA
faR <- fa(r = het.matR, nfactors = 3, n.obs = nrow(qnsRou), rotate = "oblimin", fm="ml", scores="regression")

faR.scores <- factor.scores(x=qnsRou, f=faR)
scoresRou = data.frame("id"=IDsRou, faR.scores$scores)
colnames(scoresRou) <- c("id", "AD", "Compul", "SW")

loadingsR <- faR$loadings
loadingsRmat <- loadingsR[1:209, 1:3]
loadingsDfR <- data.frame(loadingsRmat)
colnames(loadingsDfR) <- c("AD", "Compul", "SW")

# SAVE FACTOR SCORES AND LOADINGS
write.csv(scoresRou, paste(basedir, 'data/EFAscores/RouScores.csv', sep=''))
write.csv(loadingsDfR, paste(basedir, 'data/loadings/RouLoadings.csv', sep=''))

# PLOT LOADINGS
x = 1:209
itemLengths <- c(20, 20, 18, 24, 30, 43, 10, 26, 18)
ml1plot <- ggplot(data = loadingsDfR, aes(y = AD, x = x, fill = as.factor(x))) + geom_bar(stat = 'identity') +
  scale_fill_manual(values = c(rep("#3BB471", 20), rep("#B9860A", 20), rep("#1C90FE", 18), rep("#BA55D3", 24), rep("#FF2F93", 30), rep("#EEE8AC", 43), rep("#FE6447", 10), rep("#46D1CC", 26), rep("#0227CD", 18) )) + 
  theme(legend.position = "None", panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(title="AD",
       x ="Item number", y = "Loadings")

ml2plot<- ggplot(data = loadingsDfR, aes(y = Compul, x = x, fill = as.factor(x))) + geom_bar(stat = 'identity') +
  scale_fill_manual(values = c(rep("#3BB471", 20), rep("#B9860A", 20), rep("#1C90FE", 18), rep("#BA55D3", 24), rep("#FF2F93", 30), rep("#EEE8AC", 43), rep("#FE6447", 10), rep("#46D1CC", 26), rep("#0227CD", 18) )) + 
  theme(legend.position = "None", panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title="Compul",
       x ="Item number", y = "Loadings")

ml3plot<- ggplot(data = loadingsDfR, aes(y = SW, x = x, fill = as.factor(x))) + geom_bar(stat = 'identity') +
  scale_fill_manual(values = c(rep("#3BB471", 20), rep("#B9860A", 20), rep("#1C90FE", 18), rep("#BA55D3", 24), rep("#FF2F93", 30), rep("#EEE8AC", 43), rep("#FE6447", 10), rep("#46D1CC", 26), rep("#0227CD", 18) )) + 
  theme(legend.position = "None", panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(title="SW",
       x ="Item number", y = "Loadings")

top <- text_grob("Rouault", size = 20, col="black")
mlAll <- grid.arrange(ml1plot, ml2plot, ml3plot, nrow=3, top = top)
ggsave(paste(basedir, 'figs/rouault/loadingsRou.png', sep=''), mlAll, width = 10, height = 5)

# PLOT SCORE HISTOGRAMS
g1 <- ggplot(data = scoresRou, aes(x = AD)) + geom_histogram(bins = 20, col="black", 
                                                             fill="grey", 
                                                             alpha=.2)
g2 <- ggplot(data = scoresRou, aes(x = Compul)) + geom_histogram(bins = 20, col="black", 
                                                                 fill="red", 
                                                                 alpha=.2)
g3 <- ggplot(data = scoresRou, aes(x = SW)) + geom_histogram(bins = 20, col="black", 
                                                             fill="blue", 
                                                             alpha=.2)

gAll <- grid.arrange(g1, g2, g3, ncol=3, top = top)
ggsave(paste(basedir, 'figs/rouault/factoreScoresHistsRouault.png', sep=''), gAll, width = 10, height = 5)


###################################
######### NEUREKA ##########
###################################
qnsNeur <- qns[2323:4782,3:length(qns)]
IDsNeur <- qns$subid[2323:4782]

# load(paste(basedir, 'data/hetMatN.Rdata', sep=''))
het.matN <- hetcor(qnsNeur)$cor

# FIRST LOOK AT SCREE PLOT 
evN <- eigen(het.matN) # get eigenvalues
ap <- parallel(subject=nrow(qnsNeur),var=ncol(qnsNeur),
               rep=100,cent=.05)
nSn <- nScree(x=evN$values, aparallel=ap$eigen$qevpea)
sPlot <- plotnScree(nSn, main='Scree Test solutions'); 
nCng(evN$values)
ggsave(paste(basedir, 'figs/neureka/screePlotNeur.png', sep=''), sPlot, width = 10, height = 5)


# DO EFA
faN <- fa(r = het.matN, nfactors = 3, n.obs = nrow(qnsNeur), rotate = "oblimin", fm="ml", scores="regression")

faN.scores <- factor.scores(x=qnsNeur, f=faN)
scoresNeur = data.frame("id"=IDsNeur, faN.scores$scores)
colnames(scoresNeur) <- c("id", "AD", "Compul", "SW")

loadingsN <- faN$loadings
loadingsNmat <- loadingsN[1:209, 1:3]
loadingsDfN <- data.frame(loadingsNmat)
colnames(loadingsDfN) <- c("AD", "Compul", "SW")

# SAVE FACTOR SCORES AND LOADINGS
write.csv(scoresNeur, paste(basedir, 'data/EFAscores/NeurScores.csv', sep=''))
write.csv(loadingsDfN, paste(basedir, 'data/loadings/NeurLoadings.csv', sep=''))

# ##CALCULATE POSTERIOR CREDIBLE INTERVALS
# mpposFNE <- mean()ppos_FNE
# sdpposFNE <- sd(ppos_FNE)
# 
# low_spos = qnorm(.025, msposFNE, sdsposFNE)
# high_spos = qnorm(.975, msposFNE, sdsposFNE)


# PLOT LOADINGS
x = 1:209
itemLengths <- c(20, 20, 18, 24, 30, 43, 10, 26, 18)
ml1plot <- ggplot(data = loadingsDfN, aes(y = AD, x = x, fill = as.factor(x))) + geom_bar(stat = 'identity') +
  scale_fill_manual(values = c(rep("#3BB471", 20), rep("#B9860A", 20), rep("#1C90FE", 18), rep("#BA55D3", 24), rep("#FF2F93", 30), rep("#EEE8AC", 43), rep("#FE6447", 10), rep("#46D1CC", 26), rep("#0227CD", 18) )) + 
  theme(legend.position = "None", panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(title="AD",
       x ="Item number", y = "Loadings")

ml2plot<- ggplot(data = loadingsDfN, aes(y = Compul, x = x, fill = as.factor(x))) + geom_bar(stat = 'identity') +
  scale_fill_manual(values = c(rep("#3BB471", 20), rep("#B9860A", 20), rep("#1C90FE", 18), rep("#BA55D3", 24), rep("#FF2F93", 30), rep("#EEE8AC", 43), rep("#FE6447", 10), rep("#46D1CC", 26), rep("#0227CD", 18) )) + 
  theme(legend.position = "None", panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title="Compul",
       x ="Item number", y = "Loadings")

ml3plot<- ggplot(data = loadingsDfN, aes(y = SW, x = x, fill = as.factor(x))) + geom_bar(stat = 'identity') +
  scale_fill_manual(values = c(rep("#3BB471", 20), rep("#B9860A", 20), rep("#1C90FE", 18), rep("#BA55D3", 24), rep("#FF2F93", 30), rep("#EEE8AC", 43), rep("#FE6447", 10), rep("#46D1CC", 26), rep("#0227CD", 18) )) + 
  theme(legend.position = "None", panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(title="SW",
       x ="Item number", y = "Loadings")

top <- text_grob("Neureka", size = 20, col="black")

mlAll <- grid.arrange(ml1plot, ml2plot, ml3plot, nrow=3, top = top)
ggsave(paste(basedir, 'figs/neureka/loadingsNeur.png', sep=''), mlAll, width = 10, height = 5)

# PLOT SCORE HISTOGRAMS
g1 <- ggplot(data = scoresNeur, aes(x = AD)) + geom_histogram(bins = 20, col="black", 
                                                             fill="grey", 
                                                             alpha=.2)
g2 <- ggplot(data = scoresNeur, aes(x = Compul)) + geom_histogram(bins = 20, col="black", 
                                                                 fill="red", 
                                                                 alpha=.2)
g3 <- ggplot(data = scoresNeur, aes(x = SW)) + geom_histogram(bins = 20, col="black", 
                                                             fill="blue", 
                                                             alpha=.2)
gAll <- grid.arrange(g1, g2, g3, ncol=3, top = top)
ggsave(paste(basedir, 'figs/neureka/factoreScoresHistsNeureka.png', sep=''), gAll, width = 10, height = 5)


x = 1:20
evDFn <- data.frame(evN$values[1:20])


g3 <- ggplot(data = evDFn, aes(y = evN.values.1.20., x = x, fill = as.factor(x))) + geom_bar(stat = 'identity') + 
  scale_fill_manual(values = c(rep("#9CE2EE", 3), rep("grey", 17))) + 
  labs(x = 'Factor number', y = 'Eigenvalue') + 
  theme(legend.position = "None", panel.background = element_blank(), axis.line = element_line(colour = "black"))

gAll <- grid.arrange(g1, g2, g3, ncol=3)
ggsave("Eigenvalues 3 study.png", gAll, width = 10, height = 5)







  