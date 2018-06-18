##### Tests for convergence using SURFACE: exectutes the SURFACE algorithm (Mahler et al 2013) using Oreinotinus morphological leaf data and summary trees. Input parameters include:
    # treetype: must = "mcc" or "map" to indicate the summary tree used
    # datatype: must = "morpho" or "PC" to indicate the data to feed to SURFACE
    # PCtype: must = "full" or "stripped" to indicate variable set used to generate PC values which are fed to SURFACE
    # numPCaxes: integer value between 2 and 4 indicating the number of PC axes to retain and feed to SURFACE
    # fwdaicthresh: numerical value, with smaller numbers indicating more stringent AIC (Akaike Information Criterion) requirements for the SURFACE algorithm's forward phase
    # bwdaicthresh: numerical value, with smaller numbers indicating more stringent AIC (Akaike Information Criterion) requirements for the SURFACE algorithm's backward phase
    # collapsemultiple: TRUE/FALSE logic for the algorithm's backward phase, indicating whether to allow more than one pair of regimes to be collapsed together in a single iteration
##### for more information see the SURFACE documentation

##### VARIABLES
treetype <- "mcc"
datatype <- "PC"
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
leafdata.csv <- read.csv("oreinotinus_leaf_data_6-18-2018.csv", header = TRUE)
names(leafdata.csv)

##### take continuous variables as subset
leafdata.csv[,c(2,6,7,8,9,15,17,19,11,13,12,21,22,23)]->leafdata.cont
head(leafdata.cont)
names(leafdata.cont)

##### transform for skewness with log
log(leafdata.cont[,c(2:10)])->leafdata.cont[,c(2:10)]
# eccentricity left skewed so use alternate transformation:
#hist(leafdata.cont$Eccentricity)
#hist(log(2-leafdata.cont$Eccentricity))
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


##### prepare morphological data as input, if datatype = "morpho"
if(datatype == "morpho") {
  ## take means for each species
  mdat <- ddply(leafphylo.cont, "Species", summarise, length=mean(Blade.length), width=mean(Blade.width), area=mean(Blade.area), perimeter=mean(Blade.perimeter), cog=mean(Center.of.gravity), teeth=mean(Num.teeth.adj), base.angle=mean(Angle.of.base), tip.angle=mean(Angle.of.tip), AR=mean(Aspect.ratio), eccentricity=mean(Eccentricity), hairiness=mean(Hairiness), region_exp=mean(Region.code.exp), region_sim=mean(Region.code.simple))
  mdat
  
  ## convert to matrix
  #meandata <- data.matrix(mdat, rownames.force = TRUE)
  meandata <- as.matrix(mdat[,-1])
  rownames(meandata) <- mdat[,1]
  meandata
  
  xbar <- as.data.frame(meandata[,c(-12,-13)])
  
  ## take subsets of trait mean data for SURFACE (area, teeth, aspect ratio, hair)
  xbar_subset <- xbar[,c(3,6,9,11)]
  xbar_subset
} ### end if for datatype = morpho


##### PCA - use either full set of continuous variables (11 metrics) or stripped subset of continuous variables (5 metrics) depending on input variable
if(PCtype == "stripped") {
  leafdata.PCfeed <- leafphylo.cont[,c(1,4,6,10,12)]
} else if(PCtype == "full") {
  leafdata.PCfeed <- leafphylo.cont
} else {
  print ("Error! PCtype must = 'stripped' or 'full'!")
}

head(leafdata.PCfeed)
names(leafdata.PCfeed)

# run pc using the variable set selected
if(PCtype == "stripped") {
  vb.pca <- prcomp(leafdata.PCfeed[, 2:5],
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

##### EXECUTE SURFACE
# prepare tree for SURFACE
mytree<-nameNodes(mytree)

# prepare data for SURFACE, using either PC data or morpho data per input variable 'datatype'
if(datatype == "PC") {
  olist<-convertTreeData(mytree,meanPC)
} else if(datatype == "morpho"){
  olist<-convertTreeData(mytree,xbar_subset)
} else {
  print("Error! datatype must = 'PC' or 'morpho'")
}

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
if(datatype == "PC"){
  surfaceTraitPlot(meanPC, bwd[[kk]], whattraits = c(1,2), cex = 3,cex.opt = 13, cex.axis = 1.2, cex.lab = 1.5)
  surfaceTraitPlot(meanPC, bwd[[kk]], whattraits = c(1,3), cex = 3,cex.opt = 13)
  #surfaceTraitPlot(meanPC, bwd[[kk]], whattraits = c(1,4), cex = 3,cex.opt = 13)
  surfaceTraitPlot(meanPC, bwd[[kk]], whattraits = c(3,2), cex = 3,cex.opt = 13)
} else if(datatype == "morpho"){
  surfaceTraitPlot(xbar_subset, bwd[[kk]], whattraits = c(1,2), cex = 3,cex.opt = 13, cex.axis = 1.2, cex.lab = 1.5)
  surfaceTraitPlot(xbar_subset, bwd[[kk]], whattraits = c(1,3), cex = 3,cex.opt = 13)
  surfaceTraitPlot(xbar_subset, bwd[[kk]], whattraits = c(1,4), cex = 3,cex.opt = 13)
  surfaceTraitPlot(xbar_subset, bwd[[kk]], whattraits = c(2,3), cex = 3,cex.opt = 13)
}


setwd("C:/Users/Jordan/Documents/Yale/Viburnum/Figures/SURFACE") ## change path as needed

pdf("SURFACE_xbar_subset-area-aspect_aic-25_6-18-2018.pdf", width = 10, height = 10) ## change name as needed!!
surfaceTreePlot(mytree, bwd[[kk]], labelshifts = F, convcol = T, cex=0.8)
if(datatype == "PC"){
  surfaceTraitPlot(meanPC, bwd[[kk]], whattraits = c(1,2), cex = 3,cex.opt = 13, cex.axis = 1.2, cex.lab = 1.5)
  surfaceTraitPlot(meanPC, bwd[[kk]], whattraits = c(1,3), cex = 3,cex.opt = 13)
  #surfaceTraitPlot(meanPC, bwd[[kk]], whattraits = c(1,4), cex = 3,cex.opt = 13)
  surfaceTraitPlot(meanPC, bwd[[kk]], whattraits = c(3,2), cex = 3,cex.opt = 13)
} else if(datatype == "morpho"){
  surfaceTraitPlot(xbar_subset, bwd[[kk]], whattraits = c(1,2), cex = 3,cex.opt = 13, cex.axis = 1.2, cex.lab = 1.5)
  surfaceTraitPlot(xbar_subset, bwd[[kk]], whattraits = c(1,3), cex = 3,cex.opt = 13)
  surfaceTraitPlot(xbar_subset, bwd[[kk]], whattraits = c(1,4), cex = 3,cex.opt = 13)
  surfaceTraitPlot(xbar_subset, bwd[[kk]], whattraits = c(2,3), cex = 3,cex.opt = 13)
}
dev.off()

# pdf("SURFACE_PCstripped-area-aspect_PCspace.pdf", width = 10, height = 10)
# surfaceTraitPlot(meanPC, bwd[[kk]], whattraits = c(1,2), cex = 3,cex.opt = 13)
# surfaceTraitPlot(meanPC, bwd[[kk]], whattraits = c(1,3), cex = 3,cex.opt = 13)
# surfaceTraitPlot(meanPC, bwd[[kk]], whattraits = c(2,3), cex = 3,cex.opt = 13)
# dev.off()

#Clean up
rm(list = ls())
