##### Tests for convergence using SURFACE: exectutes the SURFACE algorithm (Mahler et al 2013) using Oreinotinus morphological leaf data and summary trees. Input parameters include:
    # treetype: must = "mcc" or "map" to indicate the summary tree used
    # PCtype: must = "full" or "stripped" to indicate variable set used to generate PC values which are fed to SURFACE
    # numPCaxes: integer value between 2 and 4 indicating the number of PC axes to retain and feed to SURFACE
    # fwdaicthresh: numerical value, with smaller numbers indicating more stringent AIC (Akaike Information Criterion) requirements for the SURFACE algorithm's forward phase
    # bwdaicthresh: numerical value, with smaller numbers indicating more stringent AIC (Akaike Information Criterion) requirements for the SURFACE algorithm's backward phase
    # collapsemultiple: TRUE/FALSE logic for the algorithm's backward phase, indicating whether to allow more than one pair of regimes to be collapsed together in a single iteration
##### for more information see the SURFACE documentation

##### VARIABLES
treetype <- "mcc"
PCtype <- "stripped"
numPCaxes <- 3
fwdaicthresh <- 85
bwdaicthresh <- 0
collapsemultiple <- FALSE


##### install/update all packages for phylogenetics in single sweep
# install CRAN Task View for phylogenetics
#install.packages("ctv")
library("ctv")
#install.views("Phylogenetics")
update.views("Phylogenetics")
## packages
## load ape
library(ape)
## load phytools
require(phytools)
## load plyr
library(plyr)
library("ggplot2")
library(surface)
library(igraph)

#directory check
setwd("C:/Users/Jordan/Documents/Yale/Viburnum/R")

##### import dataset
leafdata.csv <- read.csv("oreinotinus_leaf_data_6-5-2018.csv", header = TRUE)
names(leafdata.csv)

##### take continuous variables as subset
leafdata.csv[,c(2,6,7,8,9,15,17,19,11,13,12,21,22,23)]->leafdata.cont
head(leafdata.cont)
names(leafdata.cont)

##### transform for skewness with log
log(leafdata.cont[,c(2:10)])->leafdata.cont[,c(2:10)]
# eccentricity left skewed so use alternate transformation:
hist(leafdata.cont$Eccentricity)
hist(log(2-leafdata.cont$Eccentricity))
log(2-leafdata.cont$Eccentricity) ->leafdata.cont$Eccentricity

##### read viburnum oreinotinus trees from nex files
vibtreemap <- read.nexus("latin_viburnum.no_biome.no_bg.no_fossil_unsequenced.map.tre")
vibtreemcc <- read.nexus("latin_viburnum.no_biome.no_bg.no_fossil_unsequenced.mcc.tre")

# TAKE PRUNED MCC OR MAP TREE AS 'MYTREE' BY INPUT VARIABLE
if(treetype == "mcc") {
  mytree<-drop.tip(vibtreemcc,c("V_anabaptista","V_glabratum")) ## no morphological data for these two species
  } else if(treetype == "map") {
  mytree<-drop.tip(vibtreemap,c("V_anabaptista","V_glabratum")) ## no morphological data for these two species
} else {
  print ("Error! treetype must = 'mcc' or 'map'!")
}

plotTree(mytree)

# capture species names from tree
tiplabels <- mytree$tip.label
# write.table(tiplabels, file = "mcc_tiplabels.csv") # write to table if needed

# take species in phylogeny from dataset as subset
leafdata.cont[leafdata.cont$Species %in% c(tiplabels), ]->leafphylo.cont
head(leafphylo.cont)


##### PCA - use either full set of continuous variables (11 metrics) or stripped subset of continuous variables (5 metrics) depending on input variable
if(PCtype == "stripped") {
  leafdata.PCfeed <- leafphylo.cont[,c(1,2,3,6,11,12)]
} else if(PCtype == "full") {
  leafdata.PCfeed <- leafphylo.cont
} else {
  print ("Error! PCtype must = 'stripped' or 'full'!")
}

head(leafdata.PCfeed)
names(leafdata.PCfeed)

# run pc using the variable set selected
if(PCtype == "stripped") {
  vb.pca <- prcomp(leafdata.PCfeed[, 2:6],
                   center = TRUE,
                   scale. = TRUE)} else if(PCtype == "full") {
  vb.pca <- prcomp(leafdata.PCfeed[, 2:12],
                   center = TRUE,
                   scale. = TRUE)} else {
  print ("Error! PCtype must = 'stripped' or 'full'!")
}

# PC stats
pcastats <- print(vb.pca)
summary(vb.pca)

# take as data frame
vib <- data.frame(vb.pca$x,Species=leafdata.PCfeed$Species)

## take mean PC values for each species - retain # of axes as specified by input variable
if(numPCaxes == 2) {
  meanPC <- ddply(vib, "Species", summarise, PC1=mean(PC1), PC2=mean(PC2))
  
} else if(numPCaxes == 3) {
  meanPC <- ddply(vib, "Species", summarise, PC1=mean(PC1), PC2=mean(PC2), PC3=mean(PC3))
  
} else if(numPCaxes == 4) {
  meanPC <- ddply(vib, "Species", summarise, PC1=mean(PC1), PC2=mean(PC2), PC3=mean(PC3), PC4=mean(PC4))
  
} else {
  print ("Error! numPCaxes must = 2 or 3 or 4")
}

# use species names as rownames
rownames(meanPC) <- meanPC[,1]
meanPC <- meanPC[,-1]
meanPC

#### ------- ERROR TESTING
#PCA FIRST THEN MEANS -> IS THIS BETTER?
## try to reduce number of variables
leafdata.stripped <- leafphylo.cont[,c(1,2,3,6,11,12)]
head(leafdata.stripped)
names(leafdata.stripped)

vb.pca <- prcomp(leafphylo.cont[, 2:12],
                 center = TRUE,
                 scale. = TRUE)
pcastats <- print(vb.pca)
summary(vb.pca)

vb.pca.stripped <- prcomp(leafdata.stripped[, 2:6],
                          center = TRUE,
                          scale. = TRUE)

print(vb.pca.stripped)
summary(vb.pca.stripped)

vib <- data.frame(vb.pca$x,Species=leafphylo.cont$Species)
vib.stripped <- data.frame(vb.pca.stripped$x,Species=leafdata.stripped$Species)

## take PC means for each species
meanPCfull <- ddply(vib, "Species", summarise, PC1=mean(PC1), PC2=mean(PC2))
meanPCfull

meanPCstripped <- ddply(vib.stripped, "Species", summarise, PC1=mean(PC1), PC2=mean(PC2), PC3=mean(PC3))
meanPCstripped

rownames(meanPCfull) <- meanPCfull[,1]
meanPCfull <- meanPCfull[,-1]
meanPCfull

rownames(meanPCstripped) <- meanPCstripped[,1]
meanPCstripped <- meanPCstripped[,-1]
meanPCstripped

mytree<-nameNodes(mytree)
# plot(tree)
olist<-convertTreeData(mytree,meanPCstripped) ## CHECK THAT THE DATA INPUT HERE IS USED THROUGHOUT!
otree<-olist[[1]]; odata<-olist[[2]]

fwd<-surfaceForward(otree, odata, aic_threshold = 85, exclude = 0, verbose = FALSE,plotaic = FALSE)




############# ______________


##### EXECUTE SURFACE
# prepare tree and data for SURFACE
mytree<-nameNodes(mytree)
olist<-convertTreeData(mytree,meanPC)
otree<-olist[[1]]; odata<-olist[[2]]

### run surface forward phase
fwd<-surfaceForward(otree, odata, aic_threshold = fwdaicthresh, exclude = 0, verbose = FALSE, plotaic = FALSE)

# capture surface fwd outputs for use in bwd phase
k<-length(fwd)
fsum<-surfaceSummary(fwd)
names(fsum)

# see aic values for forward iterations
fsum$aics

# record aic values and write to table if desired
fwdaicval <- fsum$aics
#write.table(fwdaicval, file = paste("fwdaicval",as.numeric(Sys.time()),".csv"))

## run surface backward phase using outputs from surface fwd
bwd<-surfaceBackward(otree, odata, starting_model = fwd[[k]], aic_threshold = bwdaicthresh, only_best = collapsemultiple, verbose = FALSE, plotaic = FALSE)

# capture surface bwd outputs
bsum<-surfaceSummary(bwd)
kk<-length(bwd)

# see surface bwd stats
bsum$alpha
bsum$sigma_squared
bsum$theta
bsum$n_regimes

## plots

## if designating custom colors:
    # mycol <- c("#A6761D", "#CC00FF", "#CCFF00", "#FF0000", "#D9D9D9", "#00FFFF", "#00FF66","#666666")
    # 
    # # plot for the SURFACE tree
    # surfaceTreePlot(mytree, bwd[[kk]], labelshifts = F, cols = mycol, convcol = T, cex=0.8)
    # 
    # # plots for the SURFACE PC morphospace (adjust 'whattraits' according to # retained PC axes)
    # surfaceTraitPlot(meanPC, bwd[[kk]], whattraits = c(1,2), cex = 3,cex.opt = 13, x.lim = c(-4,4), y.lim = c(-4,4), cols = mycol, cex.axis = 1.2, cex.lab = 1.5)  ## check dataset here matches olist above
    # surfaceTraitPlot(meanPC, bwd[[kk]], whattraits = c(1,3), cex = 3,cex.opt = 13, x.lim = c(-4,4), y.lim = c(-4,4), cols = mycol)
    # surfaceTraitPlot(meanPC, bwd[[kk]], whattraits = c(1,4), cex = 2,cex.opt = 10, x.lim = c(-4,4), y.lim = c(-4,4))
    # surfaceTraitPlot(meanPC, bwd[[kk]], whattraits = c(3,2), cex = 3,cex.opt = 13, x.lim = c(-4,4), y.lim = c(-4,4), cols = mycol)

## if allowing SURFACE to designate colors:
# plot for the SURFACE tree
surfaceTreePlot(mytree, bwd[[kk]], labelshifts = F, convcol = T, cex=0.8)

# plots for the SURFACE PC morphospace (adjust 'whattraits' according to # retained PC axes)
surfaceTraitPlot(meanPC, bwd[[kk]], whattraits = c(1,2), cex = 3,cex.opt = 13, cex.axis = 1.2, cex.lab = 1.5)
surfaceTraitPlot(meanPC, bwd[[kk]], whattraits = c(1,3), cex = 3,cex.opt = 13)
surfaceTraitPlot(meanPC, bwd[[kk]], whattraits = c(1,4), cex = 3,cex.opt = 13)
surfaceTraitPlot(meanPC, bwd[[kk]], whattraits = c(3,2), cex = 3,cex.opt = 13)


#Clean up
rm(list = ls())
