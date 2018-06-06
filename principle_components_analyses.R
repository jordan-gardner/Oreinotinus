##### PCA analyses in several stages:
    # I. using full continuous variable set (11 metrics)
    # II. using stripped continuous variable set (5 metrics)
    # III. by pairs (jucundum-lautum, acutifolium-sulcatum, microcarpum-caudatum)

##### directory check
setwd("C:/Users/Jordan/Documents/Yale/Viburnum/R")

##### packages
require(ggplot2)


##### import dataset
leafdata.csv <- read.csv("oreinotinus_leaf_data_6-5-2018.csv", header = TRUE)
names(leafdata.csv)

##### take continuous variables as subset
leafdata.csv[,c(2,6,7,8,9,15,17,19,11,13,12,21)]->leafdata.cont
head(leafdata.cont)
names(leafdata.cont)

##### transform for skewness with log
log(leafdata.cont[,c(2:10)])->leafdata.cont[,c(2:10)]
# eccentricity left skewed so use alternate transformation:
hist(leafdata.cont$Eccentricity)
hist(log(2-leafdata.cont$Eccentricity))
log(2-leafdata.cont$Eccentricity) ->leafdata.cont$Eccentricity

##### pull out leaf pairs as subset; order into respective pairs
leafdata.cont[leafdata.cont$Species %in% c("V_jucundum", "V_lautum", "V_acutifolium", "V_sulcatum", "V_microcarpum", "V_caudatum"), ]->leafpairs.cont
leafpairs.cont$Species <- factor(leafpairs.cont$Species, levels = c("V_jucundum", "V_lautum", "V_sulcatum", "V_acutifolium", "V_microcarpum", "V_caudatum"))
head(leafpairs.cont)

##### pull out leaf triplets as subset; order into respective triplets
leafdata.cont[leafdata.cont$Species %in% c("V_jucundum", "V_lautum", "V_acutifolium", "V_sulcatum", "V_hartwegii", "V_fuscum"), ]->leaftrip.cont
leaftrip.cont$Species <- factor(leaftrip.cont$Species, levels = c("V_jucundum", "V_lautum", "V_hartwegii" ,"V_sulcatum", "V_acutifolium", "V_fuscum"))
head(leaftrip.cont)


##### PCA ##### 

##### Stage I. full continuous variable set
vb.pca <- prcomp(leafdata.cont[, 2:12],
                 center = TRUE,
                 scale. = TRUE)
# PCA I stats
pcastats <- print(vb.pca)
summary(vb.pca)

vib <- data.frame(vb.pca$x,Species=leafdata.cont$Species)

## PCA I plots
#pairs Chiapas, Oaxaca, Puebla highlighted
p1A <- ggplot(vib, aes(x=PC1, y=PC2, col=vib$Species, size=vib$Species))+geom_point()+scale_color_manual(values=c("#3EDD63", rep("lightgray", 2), "#6dc4ff", rep("lightgray", 11),"#9000E0","lightgray", "#E0A8FF", "lightgray", "#0077e0", rep("lightgray",12), "#008206", rep("lightgray",7)))+theme(legend.direction="horizontal",legend.position = "top")+stat_ellipse(level = .8)+scale_size_manual(values = c(1.5, rep(.5,2), 1.5, rep(.5,11), 1.5, .5, 1.5, .5, 1.5, rep(.5,12), 1.5, rep(.5,7)))+theme_bw(base_size = 20)

# p1A <- p1A + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#                  panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position="none")   ## use this to hide figure legend ##

p1A

# niche model pairs highlighted
p1N <- ggplot(vib, aes(x=PC1, y=PC2, col=vib$Species, size=vib$Species))+geom_point()+scale_color_manual(values=c("lightgray", "#D80012", rep("lightgray", 13),"#9000E0","lightgray", "#E0A8FF", rep("lightgray",9),"#ffaab2", rep("lightgray",12)))+theme(legend.direction="horizontal",legend.position = "top")+stat_ellipse(level = .8)+scale_size_manual(values = c(0.5, 1.5, rep(.5,13), 1.5, .5, 1.5, rep(.5,9), 1.5, rep(.5,12)))+theme_bw(base_size = 20)

# p1N <- p1N + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#                  panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position="none")   ## use this to hide figure legend ##

p1N

# third ecotype (hartwegii-fuscum) highlighted
p1B <- ggplot(vib, aes(x=PC1, y=PC2, col=vib$Species, size=vib$Species))+geom_point()+scale_color_manual(values=c(rep("lightgray", 10), "#12e5c2", "lightgray","#ff60e2", rep("lightgray", 27)))+theme(legend.direction="horizontal",legend.position = "top")+stat_ellipse(level = .8)+scale_size_manual(values = c(rep(.5,10),1.5, 0.5, 1.5,rep(.5,27)))+theme_bw(base_size = 20)

# p1B <- p1B + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#                  panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position="none")   ## use this to hide figure legend ##

p1B

# dentatum complex highlighted
p1C <- ggplot(vib, aes(x=PC1, y=PC2, col=vib$Species, size=vib$Species))+geom_point()+scale_color_manual(values=c(rep("lightgray", 6), "#ffbb2b",rep("lightgray", 17), "#ff6a00", "lightgray", "#ffe438",rep("lightgray",14)))+theme(legend.direction="horizontal",legend.position = "top")+stat_ellipse(level = .8)+scale_size_manual(values = c(rep(.5,6), 1.5, rep(.5,17), 1.5, 0.5, 1.5,rep(0.5,14)))+theme_bw(base_size = 20)

# p1C <- p1C + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#                  panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position="none")   ## use this to hide figure legend ##

p1C

# Bolivian species highlighted
p1D <- ggplot(vib, aes(x=PC1, y=PC2, col=vib$Species, size=vib$Species))+geom_point()+scale_color_manual(values=c("lightgray", "#D80012", rep("lightgray", 25), "#ffaab2", rep("lightgray", 25)))+theme(legend.direction="horizontal",legend.position = "top")+stat_ellipse(level = .8)+scale_size_manual(values = c(0.5, 1.5, rep(.5,25),1.5, rep(.5,12)))+theme_bw(base_size = 20)

# p1D <- p1D + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#                  panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position="none")   ## use this to hide figure legend ##

p1D

# plot loadings for PC2 over PC1
theta <- seq(0,2*pi,length.out = 100)
circle <- data.frame(x = cos(theta), y = sin(theta))
pv <- ggplot()+theme_bw() + geom_path(aes(x,y), data=circle, color="grey70")

loadings <- data.frame(vb.pca$rotation, 
                       .names = row.names(vb.pca$rotation))
pv <- pv + geom_text(data=loadings, 
                     mapping=aes(x = PC1, y = PC2, label = .names, colour = .names)) +
  coord_fixed(ratio=1) +
  labs(x = "PC1", y = "PC2")

pv <- pv + geom_segment(data=loadings, aes(x = 0, y= 0, xend = 0.9*PC1, yend = 0.9*PC2), arrow=arrow(length=unit(1/2, 'picas')))

pv 


##### Stage II. stripped continuous variable set (try to reduce number of variables)
leafdata.cont[,c(1,2,3,6,11,12)]->leafdata.stripped
head(leafdata.stripped)
names(leafdata.stripped)

vb.pca.stripped <- prcomp(leafdata.stripped[, 2:6],
                          center = TRUE,
                          scale. = TRUE)

print(vb.pca.stripped)
summary(vb.pca.stripped)

vib.stripped <- data.frame(vb.pca.stripped$x,Species=leafdata.stripped$Species)

## PCA II plots
#pairs Chiapas, Oaxaca, Puebla highlighted
p2A <- ggplot(vib.stripped, aes(x=PC1, y=PC2, col=vib.stripped$Species, size=vib.stripped$Species))+geom_point()+scale_color_manual(values=c("#3EDD63", rep("lightgray", 2), "#6dc4ff", rep("lightgray", 11),"#9000E0","lightgray", "#E0A8FF", "lightgray", "#0077e0", rep("lightgray",12), "#008206", rep("lightgray",7)))+theme(legend.direction="horizontal",legend.position = "top")+stat_ellipse(level = .8)+scale_size_manual(values = c(1.5, rep(.5,2), 1.5, rep(.5,11), 1.5, .5, 1.5, .5, 1.5, rep(.5,12), 1.5, rep(.5,7)))+theme_bw(base_size = 20)

# p2A <- p2A + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#                  panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position="none")   ## use this to hide figure legend ##

p2A

# niche model pairs highlighted
p2N <- ggplot(vib.stripped, aes(x=PC1, y=PC2, col=vib.stripped$Species, size=vib.stripped$Species))+geom_point()+scale_color_manual(values=c("lightgray", "#D80012", rep("lightgray", 13),"#9000E0","lightgray", "#E0A8FF", rep("lightgray",9),"#ffaab2", rep("lightgray",12)))+theme(legend.direction="horizontal",legend.position = "top")+stat_ellipse(level = .8)+scale_size_manual(values = c(0.5, 1.5, rep(.5,13), 1.5, .5, 1.5, rep(.5,9), 1.5, rep(.5,12)))+theme_bw(base_size = 20)

# p2N <- p2N + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#                  panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position="none")   ## use this to hide figure legend ##

p2N

# third ecotype (hartwegii-fuscum) highlighted
p2B <- ggplot(vib.stripped, aes(x=PC1, y=PC2, col=vib.stripped$Species, size=vib.stripped$Species))+geom_point()+scale_color_manual(values=c(rep("lightgray", 10), "#12e5c2", "lightgray","#ff60e2", rep("lightgray", 27)))+theme(legend.direction="horizontal",legend.position = "top")+stat_ellipse(level = .8)+scale_size_manual(values = c(rep(.5,10),1.5, 0.5, 1.5,rep(.5,27)))+theme_bw(base_size = 20)

# p2B <- p2B + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#                  panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position="none")   ## use this to hide figure legend ##

p2B

# dentatum complex highlighted
p2C <- ggplot(vib.stripped, aes(x=PC1, y=PC2, col=vib.stripped$Species, size=vib.stripped$Species))+geom_point()+scale_color_manual(values=c(rep("lightgray", 6), "#ffbb2b",rep("lightgray", 17), "#ff6a00", "lightgray", "#ffe438",rep("lightgray",14)))+theme(legend.direction="horizontal",legend.position = "top")+stat_ellipse(level = .8)+scale_size_manual(values = c(rep(.5,6), 1.5, rep(.5,17), 1.5, 0.5, 1.5,rep(0.5,14)))+theme_bw(base_size = 20)

# p2C <- p2C + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#                  panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position="none")   ## use this to hide figure legend ##

p2C

# Bolivian species highlighted
p2D <- ggplot(vib.stripped, aes(x=PC1, y=PC2, col=vib.stripped$Species, size=vib.stripped$Species))+geom_point()+scale_color_manual(values=c("lightgray", "#D80012", rep("lightgray", 25), "#ffaab2", rep("lightgray", 25)))+theme(legend.direction="horizontal",legend.position = "top")+stat_ellipse(level = .8)+scale_size_manual(values = c(0.5, 1.5, rep(.5,25),1.5, rep(.5,12)))+theme_bw(base_size = 20)

# p2D <- p2D + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#                  panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position="none")   ## use this to hide figure legend ##

p2D

# plot loadings for PC2 over PC1
theta <- seq(0,2*pi,length.out = 100)
circle <- data.frame(x = cos(theta), y = sin(theta))
pv <- ggplot()+theme_bw() + geom_path(aes(x,y), data=circle, color="grey70")

loadings <- data.frame(vb.pca.stripped$rotation, 
                       .names = row.names(vb.pca.stripped$rotation))
pv <- pv + geom_text(data=loadings, 
                     mapping=aes(x = PC1, y = PC2, label = .names, colour = .names)) +
  coord_fixed(ratio=1) +
  labs(x = "PC1", y = "PC2")

pv <- pv + geom_segment(data=loadings, aes(x = 0, y= 0, xend = 0.9*PC1, yend = 0.9*PC2), arrow=arrow(length=unit(1/2, 'picas')))

pv


##### Stage III. PCA using only species pairs (and stripped continuous variable set)
## try to reduce number of variables
leafpairs.cont[,c(1,2,3,6,11,12)]->leafpairs.stripped
#leaftrip.cont[,c(1,2,3,6,9,11,12)]->leaftrip.cont
head(leafpairs.stripped)
names(leafpairs.stripped)

vb.pca.pairs <- prcomp(leafpairs.stripped[, 2:6],
                 center = TRUE,
                 scale. = TRUE)

vib.pairs <- data.frame(vb.pca.pairs$x,Species=leafpairs.stripped$Species)

## PCA III plots
ggplot(vib.pairs, aes(x=PC1, y=PC2, col=Species, size=Species))+scale_color_manual(values=c("#9000E0","#E0A8FF","#008206","#3EDD63", "#0077e0", "#6dc4ff"))+geom_point()+theme(legend.position = "right")+stat_ellipse(level = 0.8)+scale_size_manual(values = c(rep(1.5,6)))+theme_bw(base_size = 20) 

#plot loadings for PC2 over PC1
theta <- seq(0,2*pi,length.out = 100)
circle <- data.frame(x = cos(theta), y = sin(theta))
pv <- ggplot()+theme_bw() + geom_path(aes(x,y), data=circle, color="grey70")

loadings <- data.frame(vb.pca.pairs$rotation, 
                       .names = row.names(vb.pca.pairs$rotation))
pv <- pv + geom_text(data=loadings, 
              mapping=aes(x = PC1, y = PC2, label = .names, colour = .names)) +
  coord_fixed(ratio=1) +
  labs(x = "PC1", y = "PC2")

pv <- pv + geom_segment(data=loadings, aes(x = 0, y= 0, xend = 0.9*PC1, yend = 0.9*PC2), arrow=arrow(length=unit(1/2, 'picas')))

pv


##### Stage IV. PCA using only species triplets (and stripped continuous variable set)
## try to reduce number of variables
leaftrip.cont[,c(1,2,3,6,11,12)]->leaftrip.stripped
head(leaftrip.stripped)
names(leaftrip.stripped)

vb.pca.trip <- prcomp(leaftrip.stripped[, 2:6],
                       center = TRUE,
                       scale. = TRUE)

vib.trip <- data.frame(vb.pca.trip$x,Species=leaftrip.stripped$Species)

## PCA IV plots
ggplot(vib.trip, aes(x=PC1, y=PC2, col=Species, size=Species))+scale_color_manual(values=c("#9000E0","#E0A8FF","#ff60e2","#008206","#3EDD63", "#12e5c2"))+geom_point()+theme(legend.position = "right")+stat_ellipse(level = 0.8)+scale_size_manual(values = c(rep(1.5,6)))+theme_bw(base_size = 20) 

#plot loadings for PC2 over PC1
theta <- seq(0,2*pi,length.out = 100)
circle <- data.frame(x = cos(theta), y = sin(theta))
pv <- ggplot()+theme_bw() + geom_path(aes(x,y), data=circle, color="grey70")

loadings <- data.frame(vb.pca.trip$rotation, 
                       .names = row.names(vb.pca.trip$rotation))
pv <- pv + geom_text(data=loadings, 
                     mapping=aes(x = PC1, y = PC2, label = .names, colour = .names)) +
  coord_fixed(ratio=1) +
  labs(x = "PC1", y = "PC2")

pv <- pv + geom_segment(data=loadings, aes(x = 0, y= 0, xend = 0.9*PC1, yend = 0.9*PC2), arrow=arrow(length=unit(1/2, 'picas')))

pv


##### Clean up
detach("package:ggplot2", unload=TRUE)
rm(list = ls())
