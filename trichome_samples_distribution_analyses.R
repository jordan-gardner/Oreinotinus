##### analyses of sampled trichome counts under the four category model

##### directory check
setwd("C:/Users/Jordan/Documents/Yale/Viburnum/R")

##### import and check data
trichome.csv <- read.csv("trichomecounts.csv", header = TRUE)
head(trichome.csv)
names(trichome.csv)

##### packages
require(ggplot2)

##### untransformed plots

  # density distribution by assigned category for ALL trichomes
ggplot(trichome.csv, aes(x=Trichomes, fill=Hairiness.category))+geom_density(alpha=.3)+theme_bw()

  # density distribution by assigned category for lower lamina trichomes (indicated by column "Lamina side" = 1)
ggplot(trichome.csv[trichome.csv$Lamina.side %in% "1", ], aes(x=Trichomes, fill=Hairiness.category))+geom_density(alpha=.3)+theme_bw()

  # density distribution by assigned category for upper lamina trichomes (indicated by column "Lamina side" = 0)
ggplot(trichome.csv[trichome.csv$Lamina.side %in% "0", ], aes(x=Trichomes, fill=Hairiness.category))+geom_density(alpha=.3)+theme_bw()

  # boxplot for ALL trichomes
ggplot(trichome.csv, aes(x=Hairiness.category, y=Trichomes))+geom_boxplot()+theme_bw()

  # boxplot for lower lamina trichomes
ggplot(trichome.csv[trichome.csv$Lamina.side %in% "1", ], aes(x=Hairiness.category, y=Trichomes))+geom_boxplot()+theme_bw(base_size = 20)

  # boxplot for upper lamina trichomes 
ggplot(trichome.csv[trichome.csv$Lamina.side %in% "0", ], aes(x=Hairiness.category, y=Trichomes))+geom_boxplot()+theme_bw(base_size = 20)

  # histogram for ALL trichomes
ggplot(trichome.csv, aes(x=Trichomes))+geom_histogram(binwidth = 2)+theme_bw(base_size = 20)

  # histogram for lower lamina trichomes
ggplot(trichome.csv[trichome.csv$Lamina.side %in% "1", ], aes(x=Trichomes))+geom_histogram(binwidth = 2)+theme_bw(base_size = 20)

  # histogram for upper lamina trichomes
ggplot(trichome.csv[trichome.csv$Lamina.side %in% "0", ], aes(x=Trichomes))+geom_histogram(binwidth = 2)+theme_bw(base_size = 20)

  # categorized histogram for ALL trichomes
ggplot(trichome.csv, aes(x=Trichomes, fill=Hairiness.category))+geom_histogram(binwidth = 2)+theme_bw(base_size = 20)

  # categorized histogram for lower lamina trichomes
ggplot(trichome.csv[trichome.csv$Lamina.side %in% "1", ], aes(x=Trichomes, fill=Hairiness.category))+geom_histogram(binwidth = 2)+theme_bw(base_size = 20)

  # categorized histogram for upper lamina trichomes
ggplot(trichome.csv[trichome.csv$Lamina.side %in% "0", ], aes(x=Trichomes, fill=Hairiness.category))+geom_histogram(binwidth = 2)+theme_bw(base_size = 20)


##### transform for skewness with log (add 1 first since some trichome counts = 0):
log(trichome.csv[,5]+1)->trichome.csv[,5]


##### transformed plots

  # density distribution by assigned category for ALL trichomes
ggplot(trichome.csv, aes(x=Trichomes, fill=Hairiness.category))+geom_density(alpha=.3)+theme_bw()

  # density distribution by assigned category for lower lamina trichomes
ggplot(trichome.csv[trichome.csv$Lamina.side %in% "1", ], aes(x=Trichomes, fill=Hairiness.category))+geom_density(alpha=.3)+theme_bw()

  # density distribution by assigned category for upper lamina trichomes
ggplot(trichome.csv[trichome.csv$Lamina.side %in% "0", ], aes(x=Trichomes, fill=Hairiness.category))+geom_density(alpha=.3)+theme_bw()

  # boxplot for ALL trichomes
ggplot(trichome.csv, aes(x=Hairiness.category, y=Trichomes))+geom_boxplot()+theme_bw()

  # boxplot for lower lamina trichomes (indicated by column "Lamina side" = 1)
ggplot(trichome.csv[trichome.csv$Lamina.side %in% "1", ], aes(x=Hairiness.category, y=Trichomes))+geom_boxplot()+theme_bw(base_size = 20)

  # boxplot for upper lamina trichomes (indicated by column "Lamina side" = 0)
ggplot(trichome.csv[trichome.csv$Lamina.side %in% "0", ], aes(x=Hairiness.category, y=Trichomes))+geom_boxplot()+theme_bw(base_size = 20)

  # histogram for ALL trichomes
ggplot(trichome.csv, aes(x=Trichomes))+geom_histogram(binwidth = 2)+theme_bw(base_size = 20)

  # histogram for lower lamina trichomes
ggplot(trichome.csv[trichome.csv$Lamina.side %in% "1", ], aes(x=Trichomes))+geom_histogram(binwidth = 2)+theme_bw(base_size = 20)

  # histogram for upper lamina trichomes
ggplot(trichome.csv[trichome.csv$Lamina.side %in% "0", ], aes(x=Trichomes))+geom_histogram(binwidth = 2)+theme_bw(base_size = 20)

  # categorized histogram for ALL trichomes
ggplot(trichome.csv, aes(x=Trichomes, fill=Hairiness.category))+geom_histogram(binwidth = 2)+theme_bw(base_size = 20)

  # categorized histogram for lower lamina trichomes
ggplot(trichome.csv[trichome.csv$Lamina.side %in% "1", ], aes(x=Trichomes, fill=Hairiness.category))+geom_histogram(binwidth = 2)+theme_bw(base_size = 20)

  # categorized histogram for upper lamina trichomes
ggplot(trichome.csv[trichome.csv$Lamina.side %in% "0", ], aes(x=Trichomes, fill=Hairiness.category))+geom_histogram(binwidth = 2)+theme_bw(base_size = 20)


##### Clean up
detach("package:ggplot2", unload=TRUE)
rm(list = ls())
