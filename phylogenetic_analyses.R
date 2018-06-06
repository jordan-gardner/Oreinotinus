##### Phylogenetic analyses using mcc and map summary Oreinotinus trees, including character mapping, phenogram, phylomorphospace. Uses packages APE and PHYTOOLS

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
library(ggplot2)

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

# plot trees
plot(vibtreemap)
plot(vibtreemcc)

# TAKE PRUNED MCC TREE AS 'MYTREE'
mytree<-drop.tip(vibtreemcc,c("V_anabaptista","V_glabratum")) ## no morphological data for these two species
plotTree(mytree)

# capture species names from tree
mcctiplabels <- mytree$tip.label
# write.table(mcctiplabels, file = "mcc_tiplabels.csv") # write to table if needed

# take species in phylogeny from dataset as subset
leafdata.cont[leafdata.cont$Species %in% c(mcctiplabels), ]->leafphylo.cont
head(leafphylo.cont)

# take means of each morphological metric for each species
mdat <- ddply(leafphylo.cont, "Species", summarise, length=mean(Blade.length), width=mean(Blade.width), area=mean(Blade.area), perimeter=mean(Blade.perimeter), cog=mean(Center.of.gravity), teeth=mean(Num.teeth.adj), base.angle=mean(Angle.of.base), tip.angle=mean(Angle.of.tip), AR=mean(Aspect.ratio), eccentricity=mean(Eccentricity), hairiness=mean(Hairiness), region_exp=mean(Region.code.exp), region_sim=mean(Region.code.simple))
mdat

## convert to matrix (for use with character mapping)
meandata <- as.matrix(mdat[,-1])
rownames(meandata) <- mdat[,1]
meandata


##### PCA - try using full set of continuous variables (11 metrics) and using stripped subset of continuous variables (5 metrics)
## capture reduced number of variables
leafdata.stripped <- leafphylo.cont[,c(1,2,3,6,11,12)]
head(leafdata.stripped)
names(leafdata.stripped)

# using full set
vb.pca <- prcomp(leafphylo.cont[, 2:12],
                 center = TRUE,
                 scale. = TRUE)
pcastats <- print(vb.pca)
summary(vb.pca)

# using stripped set
vb.pca.stripped <- prcomp(leafdata.stripped[, 2:6],
                          center = TRUE,
                          scale. = TRUE)

print(vb.pca.stripped)
summary(vb.pca.stripped)

# take both as data frames
vib <- data.frame(vb.pca$x,Species=leafphylo.cont$Species)
vib.stripped <- data.frame(vb.pca.stripped$x,Species=leafdata.stripped$Species)

## take mean PC values for each species - first for full set of continuous variables
meanPCfull <- ddply(vib, "Species", summarise, PC1=mean(PC1), PC2=mean(PC2), PC3=mean(PC3), PC4=mean(PC4))
meanPCfull

## take mean PC values for each species - second for stripped set of continuous variables
meanPCstripped <- ddply(vib.stripped, "Species", summarise, PC1=mean(PC1), PC2=mean(PC2), PC3=mean(PC3), PC4=mean(PC4))
meanPCstripped

# convert to matrix
PCfull <- as.matrix(meanPCfull[,-1])
rownames(PCfull) <- meanPCfull[,1]
PCfull

PCstripped <- as.matrix(meanPCstripped[,-1])
rownames(PCstripped) <- meanPCstripped[,1]
PCstripped

##### CHARACTER MAPPING
## try a single variable (blade area) mean as a character
meanarea <- meandata[,3]
objarea<-contMap(mytree, meanarea, outline = FALSE)
plot(objarea,legend=FALSE,ylim=c(1-0.09*(Ntip(objarea$tree)-1),Ntip(objarea$tree)),
     mar=c(5.1,0.4,0.4,0.4), outline=FALSE)
add.color.bar(8,objarea$cols,title="log(Blade area)",
              lims=objarea$lims,digits=3,prompt=FALSE,x=0,
              y=1-0.08*(Ntip(objarea$tree)-1),lwd=4,fsize=1,subtitle="")

## try a single variable (blade length) mean as a character
meanlength <- meandata[,1]
objlength<-contMap(mytree, meanlength, outline = FALSE)
plot(objlength,legend=FALSE,ylim=c(1-0.09*(Ntip(objlength$tree)-1),Ntip(objlength$tree)),
     mar=c(5.1,0.4,0.4,0.4), outline=FALSE)
add.color.bar(6,objlength$cols,title="log(Blade length)",
              lims=objlength$lims,digits=3,prompt=FALSE,x=0,
              y=1-0.08*(Ntip(objlength$tree)-1),lwd=4,fsize=1,subtitle="")

## try a single variable (blade width) mean as a character
meanwidth <- meandata[,2]
objwidth<-contMap(mytree, meanwidth, outline = FALSE)
plot(objwidth,legend=FALSE,ylim=c(1-0.09*(Ntip(objwidth$tree)-1),Ntip(objwidth$tree)),
     mar=c(5.1,0.4,0.4,0.4), outline=FALSE)
add.color.bar(6,objwidth$cols,title="log(Blade width)",
              lims=objwidth$lims,digits=3,prompt=FALSE,x=0,
              y=1-0.08*(Ntip(objwidth$tree)-1),lwd=4,fsize=1,subtitle="")

## try a single variable (teeth) mean as a character
meanteeth <- meandata[,6]
objteeth<-contMap(mytree, meanteeth, outline = FALSE)
plot(objteeth,legend=FALSE,ylim=c(1-0.09*(Ntip(objteeth$tree)-1),Ntip(objteeth$tree)),
     mar=c(5.1,0.4,0.4,0.4), outline=FALSE)
add.color.bar(6,objteeth$cols,title="log(Adjusted teeth)",
              lims=objteeth$lims,digits=3,prompt=FALSE,x=0,
              y=1-0.08*(Ntip(objteeth$tree)-1),lwd=4,fsize=1,subtitle="")

## try a single variable (COG) mean as a character
meanCOG <- meandata[,5]
objCOG<-contMap(mytree, meanCOG, outline = FALSE)
plot(objCOG,legend=FALSE,ylim=c(1-0.09*(Ntip(objCOG$tree)-1),Ntip(objCOG$tree)),
     mar=c(5.1,0.4,0.4,0.4), outline=FALSE)
add.color.bar(6,objCOG$cols,title="Center of gravity",
              lims=objCOG$lims,digits=3,prompt=FALSE,x=0,
              y=1-0.08*(Ntip(objCOG$tree)-1),lwd=4,fsize=1,subtitle="")

## try a single variable (hair) mean as a character
meanhair <- meandata[,11]
objhair<-contMap(mytree, meanhair, outline = FALSE)
plot(objhair,legend=FALSE,ylim=c(1-0.09*(Ntip(objhair$tree)-1),Ntip(objhair$tree)),
     mar=c(5.1,0.4,0.4,0.4), outline=FALSE)
add.color.bar(6,objhair$cols,title="Hairiness",
              lims=objhair$lims,digits=3,prompt=FALSE,x=0,
              y=1-0.08*(Ntip(objhair$tree)-1),lwd=4,fsize=1,subtitle="")

## try a principle component (PC1) mean as a character ## NOTE USING PC FROM FULL VARIABLE SET
meanpc1 <- PCfull[,1]
objpc1<-contMap(mytree, meanpc1, outline = FALSE)
plot(objpc1,legend=FALSE,ylim=c(1-0.09*(Ntip(objpc1$tree)-1),Ntip(objpc1$tree)),
     mar=c(5.1,0.4,0.4,0.4), outline=FALSE)
add.color.bar(6,objpc1$cols,title="PC1",
              lims=objpc1$lims,digits=3,prompt=FALSE,x=0,
              y=1-0.08*(Ntip(objpc1$tree)-1),lwd=4,fsize=1,subtitle="")

## try a principle component (PC2) mean as a character ## NOTE USING PC FROM FULL VARIABLE SET
meanpc2 <- PCfull[,2]
objpc2<-contMap(mytree, meanpc2, outline = FALSE)
plot(objpc2,legend=FALSE,ylim=c(1-0.09*(Ntip(objpc2$tree)-1),Ntip(objpc2$tree)),
     mar=c(5.1,0.4,0.4,0.4), outline=FALSE)
add.color.bar(6,objpc2$cols,title="PC2",
              lims=objpc2$lims,digits=3,prompt=FALSE,x=0,
              y=1-0.08*(Ntip(objpc2$tree)-1),lwd=4,fsize=1,subtitle="")

## map regions continuously
# expanded region set
regionexp <- meandata[,12]
objregionexp<-contMap(mytree, regionexp)
plot(objregionexp,legend=FALSE,ylim=c(1-0.09*(Ntip(objregionexp$tree)-1),Ntip(objregionexp$tree)),
     mar=c(5.1,0.4,0.4,0.4))
add.color.bar(6,objregionexp$cols,title="Region",
              lims=objregionexp$lims,digits=3,prompt=FALSE,x=0,
              y=1-0.08*(Ntip(objregionexp$tree)-1),lwd=4,fsize=1,subtitle="")

# simple region set
regionsim <- meandata[,13]
objregionsim<-contMap(mytree, regionsim)
plot(objregionsim,legend=FALSE,ylim=c(1-0.09*(Ntip(objregionsim$tree)-1),Ntip(objregionsim$tree)),
     mar=c(5.1,0.4,0.4,0.4))
add.color.bar(6,objregionsim$cols,title="Region",
              lims=objregionsim$lims,digits=3,prompt=FALSE,x=0,
              y=1-0.08*(Ntip(objregionsim$tree)-1),lwd=4,fsize=1,subtitle="")


##### PHENOGRAMS using PC values (from both full and stripped)
## PHENOGRAMS PCfull
phenogram(mytree,PCfull[,1],spread.labels=TRUE,spread.cost=c(1,0),ylab="PC1")
phenogram(mytree,PCfull[,2],spread.labels=TRUE,spread.cost=c(1,0),ylab="PC2")
phenogram(mytree,PCfull[,3],spread.labels=TRUE,spread.cost=c(1,0),ylab="PC3")

## PHENOGRAMS PCstripped
phenogram(mytree,PCstripped[,1],spread.labels=TRUE,spread.cost=c(1,0),ylab="PC1")
phenogram(mytree,PCstripped[,2],spread.labels=TRUE,spread.cost=c(1,0),ylab="PC2")
phenogram(mytree,PCstripped[,3],spread.labels=TRUE,spread.cost=c(1,0),ylab="PC3")


##### PHYLOMORPHOSPACE
# with PC values from FULL variable set
# PC 1 and 2
phylomorphospace(mytree,PCfull[,c(1,2)],xlab="PC 1",ylab="PC 2", node.size=1)
# alternate labeling
phylomorphospace(mytree,PCfull[,c(1,2)],xlab="PC 1",ylab="PC 2", label = "horizontal", node.size=1)
# PC 1 and 3
phylomorphospace(mytree,PCfull[,c(1,3)],xlab="PC 1",ylab="PC 3")
# 3d
phylomorphospace3d(mytree,PCfull[,c(1,2,3)],method="static")
phylomorphospace3d(mytree,PCfull[,c(1,2,3)])

# with PC values from STRIPPED variable set
# PC 1 and 2
phylomorphospace(mytree,PCstripped[,c(1,2)],xlab="PC 1",ylab="PC 2", xlim=c(-4,4), ylim=c(-3,3), node.size=1)
# alternate labeling
phylomorphospace(mytree,PCstripped[,c(1,2)],xlab="PC 1",ylab="PC 2", xlim=c(-4,4), ylim=c(-3,3), label = "horizontal", node.size=1)
# PC 1 and 3
phylomorphospace(mytree,PCstripped[,c(1,3)],xlab="PC 1",ylab="PC 3")
# 3d
phylomorphospace3d(mytree,PCstripped[,c(1,2,3)],method="static")
phylomorphospace3d(mytree,PCstripped[,c(1,2,3)])


##### Clean up
rm(list = ls())