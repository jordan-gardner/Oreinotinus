##### single variable and bivariate analyses by identified pairs (jucundum-lautum, acutifolium-sulcatum, microcarpum-caudatum)

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

##### pull out leaf pairs as subset; order into respective pairs
leafdata.cont[leafdata.cont$Species %in% c("V_jucundum", "V_lautum", "V_acutifolium", "V_sulcatum", "V_microcarpum", "V_caudatum"), ]->leafpairs.cont
leafpairs.cont$Species <- factor(leafpairs.cont$Species, levels = c("V_jucundum", "V_lautum", "V_sulcatum", "V_acutifolium", "V_microcarpum", "V_caudatum"))
head(leafpairs.cont)

##### pull out leaf triplets as subset; order into respective triplets
leafdata.cont[leafdata.cont$Species %in% c("V_jucundum", "V_lautum", "V_acutifolium", "V_sulcatum", "V_hartwegii", "V_fuscum"), ]->leaftrip.cont
leaftrip.cont$Species <- factor(leaftrip.cont$Species, levels = c("V_jucundum", "V_lautum", "V_hartwegii" ,"V_sulcatum", "V_acutifolium", "V_fuscum"))
head(leaftrip.cont)


##### Untransformed boxplots: PAIRS
# boxplots for SIZE (blade area, blade length, blade width), TEETH, SHAPE (eccentricity, aspect ratio, center of gravity, angle of tip, angle of base)
ggplot(leafpairs.cont, aes(x=Species, y=Blade.area))+geom_boxplot()+theme_bw(base_size = 20)
ggplot(leafpairs.cont, aes(x=Species, y=Blade.length))+geom_boxplot()+theme_bw(base_size = 20)
ggplot(leafpairs.cont, aes(x=Species, y=Blade.width))+geom_boxplot()+theme_bw(base_size = 20)
ggplot(leafpairs.cont, aes(x=Species, y=Num.teeth.adj))+geom_boxplot()+theme_bw(base_size = 20)
ggplot(leafpairs.cont, aes(x=Species, y=Eccentricity))+geom_boxplot()+theme_bw(base_size = 20)
ggplot(leafpairs.cont, aes(x=Species, y=Aspect.ratio))+geom_boxplot()+theme_bw(base_size = 20)
ggplot(leafpairs.cont, aes(x=Species, y=Center.of.gravity))+geom_boxplot()+theme_bw(base_size = 20)
ggplot(leafpairs.cont, aes(x=Species, y=Angle.of.tip))+geom_boxplot()+theme_bw(base_size = 20)
ggplot(leafpairs.cont, aes(x=Species, y=Angle.of.base))+geom_boxplot()+theme_bw(base_size = 20)


##### Untransformed boxplots: TRIPLETS
# boxplots for SIZE (blade area, blade length, blade width), TEETH, SHAPE (eccentricity, aspect ratio, center of gravity, angle of tip, angle of base)
ggplot(leaftrip.cont, aes(x=Species, y=Blade.area))+geom_boxplot()+theme_bw(base_size = 20)
ggplot(leaftrip.cont, aes(x=Species, y=Blade.length))+geom_boxplot()+theme_bw(base_size = 20)
ggplot(leaftrip.cont, aes(x=Species, y=Blade.width))+geom_boxplot()+theme_bw(base_size = 20)
ggplot(leaftrip.cont, aes(x=Species, y=Num.teeth.adj))+geom_boxplot()+theme_bw(base_size = 20)
ggplot(leaftrip.cont, aes(x=Species, y=Eccentricity))+geom_boxplot()+theme_bw(base_size = 20)
ggplot(leaftrip.cont, aes(x=Species, y=Aspect.ratio))+geom_boxplot()+theme_bw(base_size = 20)
ggplot(leaftrip.cont, aes(x=Species, y=Center.of.gravity))+geom_boxplot()+theme_bw(base_size = 20)
ggplot(leaftrip.cont, aes(x=Species, y=Angle.of.tip))+geom_boxplot()+theme_bw(base_size = 20)
ggplot(leaftrip.cont, aes(x=Species, y=Angle.of.base))+geom_boxplot()+theme_bw(base_size = 20)

##### transform for skewness with log
# main
log(leafdata.cont[,c(2:10)])->leafdata.cont[,c(2:10)]
# eccentricity left skewed so use alternate transformation:
hist(leafdata.cont$Eccentricity)
hist(log(1-leafdata.cont$Eccentricity))
log(1-leafdata.cont$Eccentricity) ->leafdata.cont$Eccentricity

# pairs
log(leafpairs.cont[,c(2:10)])->leafpairs.cont[,c(2:10)]
log(1-leafpairs.cont$Eccentricity) ->leafpairs.cont$Eccentricity

# triplets
log(leaftrip.cont[,c(2:10)])->leaftrip.cont[,c(2:10)]
log(1-leaftrip.cont$Eccentricity) ->leaftrip.cont$Eccentricity


##### Transformed boxplots: PAIRS
# boxplots for SIZE (blade area, blade length, blade width), TEETH, SHAPE (eccentricity, aspect ratio, center of gravity, angle of tip, angle of base)
ggplot(leafpairs.cont, aes(x=Species, y=Blade.area))+geom_boxplot()+theme_bw(base_size = 20)
ggplot(leafpairs.cont, aes(x=Species, y=Blade.length))+geom_boxplot()+theme_bw(base_size = 20)
ggplot(leafpairs.cont, aes(x=Species, y=Blade.width))+geom_boxplot()+theme_bw(base_size = 20)
ggplot(leafpairs.cont, aes(x=Species, y=Num.teeth.adj))+geom_boxplot()+theme_bw(base_size = 20)
ggplot(leafpairs.cont, aes(x=Species, y=Eccentricity))+geom_boxplot()+theme_bw(base_size = 20)
ggplot(leafpairs.cont, aes(x=Species, y=Aspect.ratio))+geom_boxplot()+theme_bw(base_size = 20)
ggplot(leafpairs.cont, aes(x=Species, y=Center.of.gravity))+geom_boxplot()+theme_bw(base_size = 20)
ggplot(leafpairs.cont, aes(x=Species, y=Angle.of.tip))+geom_boxplot()+theme_bw(base_size = 20)
ggplot(leafpairs.cont, aes(x=Species, y=Angle.of.base))+geom_boxplot()+theme_bw(base_size = 20)

##### Transformed boxplots: TRIPLETS
# boxplots for SIZE (blade area, blade length, blade width), TEETH, SHAPE (eccentricity, aspect ratio, center of gravity, angle of tip, angle of base)
ggplot(leaftrip.cont, aes(x=Species, y=Blade.area))+geom_boxplot()+theme_bw(base_size = 20)
ggplot(leaftrip.cont, aes(x=Species, y=Blade.length))+geom_boxplot()+theme_bw(base_size = 20)
ggplot(leaftrip.cont, aes(x=Species, y=Blade.width))+geom_boxplot()+theme_bw(base_size = 20)
ggplot(leaftrip.cont, aes(x=Species, y=Num.teeth.adj))+geom_boxplot()+theme_bw(base_size = 20)
ggplot(leaftrip.cont, aes(x=Species, y=Eccentricity))+geom_boxplot()+theme_bw(base_size = 20)
ggplot(leaftrip.cont, aes(x=Species, y=Aspect.ratio))+geom_boxplot()+theme_bw(base_size = 20)
ggplot(leaftrip.cont, aes(x=Species, y=Center.of.gravity))+geom_boxplot()+theme_bw(base_size = 20)
ggplot(leaftrip.cont, aes(x=Species, y=Angle.of.tip))+geom_boxplot()+theme_bw(base_size = 20)
ggplot(leaftrip.cont, aes(x=Species, y=Angle.of.base))+geom_boxplot()+theme_bw(base_size = 20)


##### Bivariates
# size: blade width x blade length
ggplot(leafpairs.cont, aes(x=Blade.length, y=Blade.width, col=Species))+geom_point()+theme(legend.direction="horizontal",legend.position = "top")+stat_ellipse(level = .8)+theme_bw(base_size = 20)
# size/teeth: blade area x teeth
ggplot(leafpairs.cont, aes(x=Blade.area, y=Num.teeth.adj, col=Species))+geom_point()+theme(legend.direction="horizontal",legend.position = "top")+stat_ellipse(level = .8)+theme_bw(base_size = 20)
# size/shape: blade area x eccentricity
ggplot(leafpairs.cont, aes(x=Blade.area, y=Eccentricity, col=Species))+geom_point()+theme(legend.direction="horizontal",legend.position = "top")+stat_ellipse(level = .8)+theme_bw(base_size = 20)


##### Clean up
detach("package:ggplot2", unload=TRUE)
rm(list = ls())