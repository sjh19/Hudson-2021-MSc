#Sarah_Hudson_CID01785398_CombinedRScript_26Aug2021.R

#INDEX OF CODE
#lines 49-193 BEE SIZE ANALYSIS
#line 120 linear model to test correlation of hive distance and ITD for Bufftails only using total numbers of buffs
#line 123 Is correlation between hive distance and ITD significant?
#line 129 #Is correlation between hive density and ITD significant?
#line 140 shapiro test for normalcy of size data
#line 154 PCA variables in linear model
#line 169 hierarchical regression of PCA variables
#line 193 PCA analysis
#LINE 285 ANALYSIS OF BEE TRANSECT AND FITT COUNT DATA
#LINE 311 TRANSECT DATA
#line 521 multivariate linear models of frequency of Buff against landscape and floral variables in Bees4
#line 528 stepwise elimination and best fit models
#line 560 Multivariate Linear Model using PCA variables Rail, Floral.Units, Hive.Distance and Building Vol
#LINE 580 FITT COUNT DATA
#line 636 Calculate basic stats for FITT data from FITT4
#line 680 QUESTION:Is bee frequency correlated to distance from honey bee hive?
#line 745 Multivariate regression of BuffFrequency against variables for FITT COUNT data
#line 759 Best model suggested by step backwards
#line 779 Multivariate Linear Model using PCA variables Rail, Floral Units, Hive.Distance and Building.Vol
#line 796 TESTING DIFFERENCE IN TRANSECT AND FITT COUNT DATA 
#line 830 Shapiro-Wilk normality test for TRANSECT and FITT data
#LINE 851 BIPARTITE ANALYSIS
#LINE 915 ANALYSIS OF FLORAL DATA
#Line 1001 Forage Preference Index 



#Get applications
library(dplyr)
library(ggplot2)
library(ggfortify)
library(lubridate)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(usdm)
library(psych)
library(vegan)
library(factoextra)
library(pls)

#BEE SIZE
#QUESTION: Is bee size correlated to distance to honey bee hive?

rm(list=ls())
getwd()
setwd("C:/Users/sarah/OneDrive/Reseach/BeeAnalysis/2021")
read.csv("BeeSize2021.csv")

#Create working file "Size"
Size <- read.csv("BeeSize2021.csv", header=TRUE)
names(Size)
head(Size)

as_tibble(Size)

#make Date a recognisable date 

Size<-mutate(Size,Date=dmy(Date))
glimpse(Size)

as_tibble(Size)

Size

#calculate mean and sd for each species and caste
SizeStats<-Size%>%
  group_by(Species,Caste, Hive.Distance, Hive.Density)%>% summarise(SumBees=sum(Number),MeanSize=mean(ITD),sdSize=sd(ITD))
SizeStats

#Summarise size stats
SizeStatsSummary<-Size%>%
  group_by(Species,Caste)%>% summarise(SumBees=sum(Number),MeanSize=mean(ITD),sdSize=sd(ITD))
SizeStatsSummary

#Select worker caste only
WorkerSize<-filter(SizeStats, Caste == "W" )
WorkerSize

#Select Buff and common carder only
WorkerSizeMain<-filter(WorkerSize, Species=="Bombus terrestris"| Species=="Bombus pascuorum")
WorkerSizeMain

#Sample size of common carder too small - Select Bufftail for for further analysis
WorkerSizeBuff<-filter(WorkerSize, Species=="Bombus terrestris")
WorkerSizeBuff

#Is Bufftail worker size correlated to distance from hive?

#select  Buff workers only from Size.csv
AllBuffWorkerSize<-filter(Size, Caste == "W", Species=="Bombus terrestris")
AllBuffWorkerSize


ggplot(AllBuffWorkerSize, aes(Hive.Distance,ITD, shape=Species, colour=Species, fill=Species)) +
  geom_smooth(method="lm") +
  geom_point(size=3) +
  theme_bw() + 
  xlab("Distance from hive") +
  ylab("Size in mm") +   
  ggtitle("Size Buff Worker bees distance from honey bee Hive")

#Is Buff worker size (ITD) correlated to hive density?
ggplot(AllBuffWorkerSize, aes(Hive.Density,ITD, shape=Species, colour=Species, fill=Species)) +
  geom_smooth(method="lm") +
  geom_point(size=3) +
  theme_bw() + 
  xlab("Hive Density per km sq.") +
  ylab("Size in mm") +   
  ggtitle("Size Buff Worker bees with hive density")


#linear model to test correlation of hive distance and ITD for Bufftails only using total numbers of buffs
#not mean size

#Is correlation between hive distance and ITD significant?

BuffSizeMod1<-lm(ITD ~ Hive.Distance, data=AllBuffWorkerSize)
anova(BuffSizeMod1)
summary(BuffSizeMod1)

#Is correlation between hive density and ITD significant?
  
BuffSizeMod2<-lm(ITD ~ Hive.Density, data=AllBuffWorkerSize)
anova(BuffSizeMod2)
summary(BuffSizeMod2)

#calculate basic stats of mean and sd for Bufftail size
SizeSummary<-Size%>%
  group_by(Species, Caste)%>% summarise(SumBees=sum(Number),MeanSize=mean(ITD),sdSize=sd(ITD))
SizeSummary 

#use shapiro test for normalcy on AllBUffWorkerSize
#select Buff worker Size

shapiro.test(AllBuffWorkerSize$ITD)
#W = 0.9917, p-value = 0.2738
#distribution is normal as test is not significant

#show graphically
ggdensity(AllBuffWorkerSize$ITD, 
          main = "Density plot of Size",
          xlab = "Size")

#plot is normally distributed so not skewed to larger speciments Therefore queens not misclassified

#QUESTION - IS SIZE CORRELATED WITH LANDSCAPE VARIABLES IDENTIFIED BY PCA?

#Linear regression with 4 PCA explanatory variables Rail, Building.Vol, Floral Units, Hive.Distance
#Scale data
AllBuffWorkerSizez<-as.data.frame(scale(AllBuffWorkerSize[14:22],center = TRUE, scale=TRUE))

#Model Size (ITD) with 4 explanatory variables
LmPCABuffSize <- lm(ITD ~Hive.Distance + Building.Vol+ FloralUnits.Site +Rail, data = AllBuffWorkerSizez)
anova(LmPCABuffSize)
summary(LmPCABuffSize)

#p-value: 0.01046 Adjusted R-squared:  0.04344
#Model shows significant positive correlation of hive distance, building volume and floral units
#Model is significant but individual variables are not. This suggests there is still an issue with collinearity

#Check results using hierarchical linear regression
#Hierarchical Linear Regression https://data.library.virginia.edu/hierarchical-linear-regression/
#using AllBuffWorkerSizez
#To obtain total SS

Model0 <- lm(ITD ~ 1, data=AllBuffWorkerSizez)  # to obtain Total SS

anova(Model0)
summary(Model0)
Model1<- lm(ITD ~ Hive.Distance, data=AllBuffWorkerSizez)
summary(Model1)
Model2<- lm(ITD ~ Hive.Distance+Building.Vol, data=AllBuffWorkerSizez)
summary(Model2)
Model3<- lm(ITD ~ Hive.Distance+Building.Vol+FloralUnits.Site, data=AllBuffWorkerSizez)
summary(Model3)
Model4<- lm(ITD ~ Hive.Distance+Building.Vol+FloralUnits.Site+Rail, data=AllBuffWorkerSizez)
summary(Model4)
#model comparison
anova(Model1, Model2,Model3,Model4)


#Hierarchical model suggests best fit is hive Distance and building volume Pr(>F) = 0.002672


#PCA AND LANDSCAPE VARIABLES

#Prepare landscape data for PCA

#clear directory
rm(list=ls())
getwd()
setwd("C:/Users/sarah/OneDrive/Reseach/BeeAnalysis/2021")
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(usdm)
library(psych)
library(vegan)
library(factoextra)

#load landscape data file

LandPCA <- read.csv("AllLandscapeVar.csv", header=TRUE)
names(LandPCA)
head(LandPCA)
as_tibble(LandPCA)

#scale variables
LandPCAz<-as.data.frame(scale(LandPCA[2:21],center = TRUE, scale=TRUE))
LandPCAz

#calculate pairs panels for 
pairs.panels(LandPCAz[,c(3,4,15,16,17,19,18,20)])

#calculate p values
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.6, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.4, txt2)
}

pairs(LandPCAz[,c(3,4,15,16,17,19,18,20)], upper.panel = panel.cor)

#PCA analysis calculate VIF for Impervious, Building vol, Rail, Florally Rich, Hive Distance, Total Water, Floral units


vif(LandPCAz[,c(2,4,6,15,16,17,20)])

#eliminate Flower.rich

vif(LandPCAz[,c(2,4,15,16,17,20)])

#eliminate water
vif(LandPCAz[,c(2,4,16,17,20)])

#eliminate impervious
vif(LandPCAz[,c(4,16,17,20)])

#PCA2 components for Building volume, Rail, Hive Distance and Floral Units
PCA2<-princomp(LandPCAz [,c(4,16,17,20)], cor=TRUE)

summary(PCA2) 

loadings(PCA2)

#The Latent Root Criterion
eigenvalues <- PCA2$sdev^2
eigenvalues

#The Scree Plot Criterion
plot(PCA2, type="lines", ylim=c(0,2))

#The Relative Percent Variance Criterion
summary(PCA2)

#plot components
fviz_pca_biplot(PCA2, repel = TRUE, geom.ind = "point", ellipse.level=0.95, col.var = "black", labelsize=4)



#ANALYSIS OF BEE TRANSECT AND FITT COUNT DATA

#Analysis of Bee Transect data and FITT count data 2021
#NB Multiple linear regression of PCA variables is on line 357 (Transect data) and line 575 (FITT Count)

rm(list=ls())
getwd()
setwd("C:/Users/sarah/OneDrive/Reseach/BeeAnalysis/2021")

library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(ggpubr)
library(rstatix)
library(tidyverse)
library(raster)
library(ggfortify)
library(tidyverse)
library(usdm)
library(psych)
library(vegan)
library(factoextra)
library(pls)


#BEE TRANSECT DATA
#Get bee transect data into R

read.csv("BeeTransect.csv")

BeeTrans <- read.csv("BeeTransect.csv", header=TRUE)
names(BeeTrans)
head(BeeTrans) 

#make Date a recognisable date 

BeeTrans<-mutate(BeeTrans,Date=dmy(Date))
glimpse(BeeTrans)
as_tibble(BeeTrans)

#Species richness and abundance per site from BeeTransect data
summary(BeeTrans)

#Create basic analysis of Bee Transect data. Species, caste, site with totals

TransectAnal<-BeeTrans%>%
  group_by(Caste, Species.1)%>% summarise(SumBees=sum(Number))
TransectAnal

#Spread TransectAnal into Species columns
SpreadTransectAnal<-TransectAnal%>%spread(key=Species.1, value=SumBees, fill= 0)
SpreadTransectAnal

#add totals to SpreadTransectAnal
SpreadTransectAnal<-mutate(SpreadTransectAnal,Caste, TotalBumblebees=sum(Buff+Ccarder+Early+Garden+Red+Tree+Vestal),
                           Totalbees=sum(TotalBumblebees+Honey+Solitary))
SpreadTransectAnal

#Create a table of number Species by Site and Date "BeeTransSpecies"

BeeTransSpecies<-BeeTrans%>%
  group_by(Site,Visit, Date, Method, Species.1, Hive.Distance, Hive.Density,Buildings, Impervious, Rail,
           Total.Water,Building.Vol,Bee.habitat, Floral.Units)%>% summarise(SumBees=sum(Number))
BeeTransSpecies

#spread BeeTransSpecies into separate columns for each species

SpreadBeeTrans<-BeeTransSpecies%>%spread(key=Species.1, value=SumBees, fill= 0)
SpreadBeeTrans

#change visit to character
SpreadBeeTrans$Visit<-as.character(SpreadBeeTrans$Visit)
SpreadBeeTrans                     

#add totals
Bees<-mutate(SpreadBeeTrans,Date, Method, TotalNo=sum(Buff+Ccarder+Early+Garden+Honey+Red+Solitary+Tree+Vestal)) 
Bees
summary(Bees)

Bees2<-Bees%>%
  group_by(Site, Date, Method, Hive.Distance, Hive.Density, Buildings, Impervious, Bee.habitat, 
           Floral.Units, Rail,Total.Water,Building.Vol)%>% summarise(SumBuff=sum(Buff), SumCcarder=sum(Ccarder), 
               SumEarly=sum(Early), SumGarden=sum(Garden),SumHoney=sum(Honey),SumRed=sum(Red),SumSolitary=sum(Solitary),
               SumTree=sum(Tree),SumVestal=sum(Vestal), SumBees=sum(TotalNo))
Bees2

#Select main species
#Create a table of Buff, CC and honey bees per site and visit
#Calculate relative frequency of Buff Honey and CC to total

BeesMainSpecies<-Bees2%>% dplyr::select(Site, Date, Method, Hive.Distance, Hive.Density,Buildings, Impervious,Rail,
                                        Total.Water,Building.Vol,Bee.habitat, Floral.Units, SumBuff, SumCcarder, 
                                        SumHoney)
BeesMainSpecies
#rename columns

Bees3<-BeesMainSpecies%>%
  rename(Buff=SumBuff, Ccarder=SumCcarder, Honey=SumHoney)
Bees3

#calculate total, total per Date, frequency of each species

Bees4<-mutate(Bees3,Date,Method, Total=(Buff+Ccarder+Honey), BuffFreq=(Buff/Total),
              CcarderFreq=(Ccarder/Total),HoneyFreq=Honey/Total)
Bees4
summary(Bees4)   

#Calculate basic stats for Transect data from Bees4

BeesStats<-Bees4%>%
  group_by(Method)%>% summarise(MeanBuffFreq=mean(BuffFreq), MeanCcarderFreq=mean(CcarderFreq), 
                                MeanHoneyFreq=mean(HoneyFreq), sdBuffFreq=sd(BuffFreq), sdCcarderFreq=sd(CcarderFreq), 
                                sdHoneyFreq=sd(HoneyFreq))
BeesStats

#gather Bees4 into long format for graphing

Bees5<-Bees4%>%pivot_longer(c(BuffFreq, CcarderFreq,HoneyFreq),names_to="Species",
                            values_to="Frequency")
Bees5
summary(Bees5)


#QUESTION: Does bee frequency change by date?

#plot a graph of frequency of species by date
ggplot(Bees5, aes(x=Date, y=Frequency, color=Species))+
  geom_point(size=3)+theme_bw()+ 
  scale_color_manual(values = c("BuffFreq" = "blue", "CcarderFreq" = "green", "HoneyFreq" = "orange"))+
  xlab("Date")+
  ylab("Frequency")+
  ggtitle("Transect: Bee relative frequency by date")


#linear model of frequency of frequency of Bufftails against date

BeesMod1<-lm(BuffFreq ~ Date, data=Bees4)
anova(BeesMod1)
summary(BeesMod1)

#Not significant p=0.2646, adj R squared is 6.6%

#linear model of honey bee freqency by date
BeesModHoney<-lm(HoneyFreq ~ Date, data=Bees4)
anova(BeesModHoney)
summary(BeesModHoney)

#no significant correlation adj r is -2.4%

#linear model of Ccarder bee freqency by date
BeesModCC<-lm(CcarderFreq ~ Date, data=Bees4)
anova(BeesModCC)
summary(BeesModCC)

#Weakly significant adj R=6.4%

Plot1 <- ggplot(Bees5, aes(x = Date, y=Frequency))+
  xlab("Date")+
  ylab("Frequency")+  
  geom_point(aes(color = Species, shape = Species))+
  geom_smooth(aes(color = Species, fill = Species), 
              method = "lm", fullrange = TRUE) +
  theme_bw()+
  ggtitle("Transect: Bee frequency by date")
Plot1

# QUESTION:Is bee frequency correlated to distance from honey bee hive?

#Buff tail

BeesMod2<-lm(BuffFreq ~ Hive.Distance, data=Bees4)
anova(BeesMod2)
summary(BeesMod2)

#weakly significant p-value: 0.0532, Adjusted R-squared: 0.06592  

#Common carder
  
BeesMod3<-lm(CcarderFreq ~ Hive.Distance, data=Bees4)
anova(BeesMod3)
summary(BeesMod3)

#not significant p-value: 0.2023, Adjusted R-squared:  0.01591 

#honey

BeesMod4<-lm(HoneyFreq ~ Hive.Distance, data=Bees4)
anova(BeesMod4)
summary(BeesMod4)

#significant  p-value: 0.004181, Adjusted R-squared:  0.1634

#Graph frequency against distance from hive
  
  Plot2 <- ggplot(Bees5, aes(x = Hive.Distance, y=Frequency))+
  xlab("Distance")+
  ylab("Frequency")+
  geom_point(aes(color = Species, shape = Species))+
  geom_smooth(aes(color = Species, fill = Species), 
              method = "lm", fullrange = TRUE) +
  theme_bw()+
  ggtitle("Transect: Bee frequency by distance from honey bee hive")
Plot2

# QUESTION: Is bee frequency correlated to density of honey bee hives

#Buff tail

BeesMod5<-lm(BuffFreq ~ Hive.Density, data=Bees4)
anova(BeesMod5)
summary(BeesMod5)

#Common carder

BeesMod6<-lm(CcarderFreq ~ Hive.Density, data=Bees4)
anova(BeesMod6)
summary(BeesMod6)

#honey

BeesMod7<-lm(HoneyFreq ~ Hive.Density, data=Bees4)
anova(BeesMod7)
summary(BeesMod7)

#Graph frequency against distance from hive
  
  Plot3 <- ggplot(Bees5, aes(x = Hive.Density, y=Frequency))+
  xlab("Hive density per sq. km")+
  ylab("Frequency")+
  geom_point(aes(color = Species, shape = Species))+
  geom_smooth(aes(color = Species, fill = Species), 
              method = "lm", fullrange = TRUE) +
  theme_bw()+
  ggtitle("Bee frequency by density of honey bee hives per sq km")

Plot3

#multivariate linear models of frequency of Buff against landscape and floral variables in Bees4

#scale variables

Bees4z<-as.data.frame(scale(Bees4[4:19],center = TRUE, scale=TRUE))
Bees4z

#do stepwise elimination
lm_totalBuffTransect <- lm(BuffFreq ~Hive.Distance + Hive.Density +Buildings+ Impervious+ Building.Vol+Total.Water+Rail+
                             Bee.habitat+ Floral.Units, data = Bees4z)
summary(lm_totalBuffTransect)
ModelStep1<-step(lm_totalBuffTransect, direction="backward")

ModelStep1

#Fit best model suggested by step backwards
#Bufftail

BestFitModelBuffTransect<-lm(formula= BuffFreq ~ Rail +Impervious + Buildings+Building.Vol, data=Bees4z)

summary(BestFitModelBuffTransect)

#Common carder

lm_totalCCTransect <- lm(CcarderFreq ~Hive.Distance + Hive.Density +Buildings+ Impervious+ 
                           Bee.habitat+ Floral.Units, data = Bees4z)
summary(lm_totalCCTransect)

ModelStep2<-step(lm_totalCCTransect, direction="backward")

ModelStep2

#Fit best model suggested by step backwards

BestFitModelCCTransect<-lm(formula= CcarderFreq ~ Impervious, data=Bees4z)

summary(BestFitModelCCTransect)


#Multivariate Linear Model using variables derived from PCA, Rail, Floral.Units Hive.Distance and Building Vol

#Buff
LmPCABuffTransect <- lm(BuffFreq ~Hive.Distance + Building.Vol+ Floral.Units+Rail, data = Bees4z)
anova(LmPCABuffTransect)
summary(LmPCABuffTransect)
#CC
LmPCACCTransect <- lm(CcarderFreq ~Hive.Distance + Building.Vol+ Floral.Units+Rail, data = Bees4z)
anova(LmPCACCTransect)
summary(LmPCACCTransect)
#Honey
LmPCAHoneyTransect <- lm(HoneyFreq ~Hive.Distance + Building.Vol+ Floral.Units+Rail, data = Bees4z)
anova(LmPCAHoneyTransect)
summary(LmPCAHoneyTransect)

#Is honey freq correlated to building vol?
LmHonTransectBuildingVol<- lm(Hive.Density ~ Building.Vol, data = Bees4z)
anova(LmHonTransectBuildingVol)
summary(LmHonTransectBuildingVol)

#FITT COUNT DATA

#Get FITT count data into R

read.csv("FITTCount.csv")

FITTCount <- read.csv("FITTCount.csv", header=TRUE)
names(FITTCount)
head(FITTCount) 

#make Date a recognisable date 
unique(FITTCount$Date)

FITTCount<-mutate(FITTCount,Date=dmy(Date))
glimpse(FITTCount)

as_tibble(FITTCount)

#summarise by site

FITT2<-FITTCount%>%
  group_by(Site, Date, Method, Hive.Distance, Hive.Density,Buildings, Impervious, 
           Bee.habitat, Floral.Units, Building.Vol, Rail, Total.Water )%>% summarise(SumBuff=sum(Buff), 
                                      SumCcarder=sum(Ccarder), SumEarly=sum(Early), SumHoney=sum(Honey),
                                      SumRedtail=sum(Redtail),SumSolitary=sum(Solitary),SumTree=sum(Tree),
                                      SumBumblebees=sum(SumBuff+SumCcarder+SumEarly+SumTree),SumBees=sum(TotalBees))
FITT2

#Summarise for FITT analysis

FITTAnal<-FITTCount%>%
  group_by(Site)%>% summarise(SumBuff=sum(Buff), SumCcarder=sum(Ccarder), SumEarly=sum(Early), TotalHoney=sum(Honey),
                              SumRedtail=sum(Redtail),TotalSolitary=sum(Solitary), SumTree=sum(Tree),
                              TotalBumblebees=sum(SumBuff+SumCcarder+SumEarly+SumTree),TotalBees=sum(TotalBumblebees+TotalSolitary
                                                                                                     +TotalHoney))
FITTAnal

#Select main species

FITTMainSpecies<-FITT2%>% dplyr::select(Site, Date,Method,Hive.Density, Hive.Distance,Buildings, Impervious, 
                                        Bee.habitat, Floral.Units,Building.Vol, Rail, Total.Water, SumBuff,
                                        SumCcarder, SumHoney)
FITTMainSpecies
#rename columns

FITT3<-FITTMainSpecies%>%
  rename(Buff=SumBuff, Ccarder=SumCcarder, Honey=SumHoney)
FITT3

#calculate total, total per date, frequency of each species

FITT4<-mutate(FITT3,Date,Method, Hive.Density, Hive.Distance,Buildings, Impervious, 
              Bee.habitat, Floral.Units,Building.Vol, Rail, Total.Water,Total=(Buff+Ccarder+Honey), BuffFreq=(Buff/Total),
              CcarderFreq=(Ccarder/Total),HoneyFreq=(Honey/Total))
FITT4

#Calculate basic stats for FITT data from FITT4
#remove NAs

FITT4<-na.omit(FITT4)
FITT4

FITTStats<-FITT4%>%
  group_by(Method)%>% summarise(MeanBuffFreq=mean(BuffFreq), MeanCcarderFreq=mean(CcarderFreq), 
                                MeanHoneyFreq=mean(HoneyFreq), sdBuffFreq=sd(BuffFreq), sdCcarderFreq=sd(CcarderFreq), 
                                sdHoneyFreq=sd(HoneyFreq))
FITTStats

#gather FITT4 into long format for graphing

FITT5<-FITT4%>%pivot_longer(c(BuffFreq, CcarderFreq,HoneyFreq),names_to="Species",
                            values_to="Frequency")
FITT5

#Does bee frequency change by date on FITT Counts?

#plot a graph of frequency of species by date
ggplot(FITT5, aes(x=Date, y=Frequency, color=Species))+
  geom_point(size=3)+theme_bw()+ 
  scale_color_manual(values = c("BuffFreq" = "blue", "CcarderFreq" = "green", "HoneyFreq" = "orange"))+
  xlab("Date")+
  ylab("Frequency")+
  ggtitle("FITT Count:Bee relative frequency by date")

#linear model of frequency of honeybees against Date

FITTBeesMod1<-lm(BuffFreq ~ Date, data=Bees4)
anova(FITTBeesMod1)
summary(FITTBeesMod1)

FITTPlot1 <- ggplot(FITT5, aes(x = Date, y=Frequency))+
  xlab("Date")+
  ylab("Frequency")+  
  geom_point(aes(color = Species, shape = Species))+
  geom_smooth(aes(color = Species, fill = Species), 
              method = "lm", fullrange = TRUE) +
  theme_bw()+
  ggtitle("FITT Count: Bee frequency by date")
FITTPlot1

# QUESTION:Is bee frequency correlated to distance from honey bee hive?

#Buff tail

FITTBeesMod2<-lm(BuffFreq ~ Hive.Distance, data=FITT4)
anova(FITTBeesMod2)
summary(FITTBeesMod2)

#Common carder

FITTBeesMod3<-lm(CcarderFreq ~ Hive.Distance, data=FITT4)
anova(FITTBeesMod3)
summary(FITTBeesMod3)

#honey

FITTBeesMod4<-lm(HoneyFreq ~ Hive.Distance, data=FITT4)
anova(FITTBeesMod4)
summary(FITTBeesMod4)

#Graph frequency against distance from hive

FITTPlot2 <- ggplot(FITT5, aes(x = Hive.Distance, y=Frequency))+
  xlab("Distance")+
  ylab("Frequency")+
  geom_point(aes(color = Species, shape = Species))+
  geom_smooth(aes(color = Species, fill = Species), 
              method = "lm", fullrange = TRUE) +
  theme_bw()+
  ggtitle("FITT Count: Bee frequency by distance from honey bee hive")
FITTPlot2

# Is bee frequency correlated to density of honey bee hives

#Buff tail

FITTBeesMod5<-lm(BuffFreq ~ Hive.Density, data=FITT4)
anova(FITTBeesMod5)
summary(FITTBeesMod5)

#Common carder
  
FITTBeesMod6<-lm(CcarderFreq ~ Hive.Density, data=FITT4)
anova(FITTBeesMod6)
summary(FITTBeesMod6)

#honey

FITTBeesMod7<-lm(HoneyFreq ~ Hive.Density, data=FITT4)
anova(FITTBeesMod7)
summary(FITTBeesMod7)

#Graph frequency against hive density

FITTPlot3 <- ggplot(FITT5, aes(x = Hive.Density, y=Frequency))+
  xlab("Hive density per sq. km")+
  ylab("Frequency")+
  geom_point(aes(color = Species, shape = Species))+
  geom_smooth(aes(color = Species, fill = Species), 
              method = "lm", fullrange = TRUE) +
  theme_bw()+
  ggtitle("FITT Count: Bee frequency by density of honey bee hives per sq km")
FITTPlot3


#Multivariate regression of BuffFrequency against variables for FITT COUNT data
#scale variables

FITT4z<-as.data.frame(scale(FITT4[4:19],center = TRUE, scale=TRUE))
FITT4z

#do stepwise elimination
lm_totalBuffFITT <- lm(BuffFreq ~Hive.Distance + Hive.Density +Buildings+ Impervious+ Building.Vol+Rail+Total.Water+
                         Bee.habitat+ Floral.Units, data = FITT4z)
summary(lm_totalBuffFITT)
ModelStep3<-step(lm_totalBuffFITT, direction="backward")

ModelStep3

#Fit best model suggested by step backwards

BestFitModelBuffFITT<-lm(formula= BuffFreq ~ Hive.Distance+Hive.Density + Buildings + Total.Water, data=FITT4z)

summary(BestFitModelBuffFITT)

#Find best model for Common carder

#do stepwise elimination
lm_totalCCFITT <- lm(CcarderFreq ~Hive.Distance + Hive.Density +Buildings+ Impervious+ 
                       Bee.habitat+ Floral.Units, data = FITT4z)
summary(lm_totalCCFITT)
ModelStep4<-step(lm_totalCCFITT, direction="backward")
ModelStep4

#Fit best model suggested by step backwards

BestFitModelFITTCC<-lm(formula= CcarderFreq ~ Hive.Distance, data=FITT4z)
summary(BestFitModelFITTCC)

#Multivariate Linear Model using variables derived from PCA, Rail, Floral Units, Hive.Distance and Building.Vol
#BuffFreq

LmPCABuffFITT <- lm(BuffFreq ~Hive.Distance + Building.Vol+ Floral.Units+Rail, data = FITT4z)
anova(LmPCABuffFITT)
summary(LmPCABuffFITT)

LmPCACCFITT <- lm(CcarderFreq ~Hive.Distance + Building.Vol+ Floral.Units+Rail, data = FITT4z)
anova(LmPCACCFITT)
summary(LmPCACCFITT)


#Is frequency of honey bees affected by PCA factors
LmPCAHoneyFITT <- lm(HoneyFreq ~Hive.Distance + Building.Vol+ Total.Water+Rail, data = FITT4z)
anova(LmPCAHoneyFITT)
summary(LmPCAHoneyFITT)

#TESTING DIFFERENCE IN TRANSECT AND FITT COUNT DATA http://www.sthda.com/english/wiki/unpaired-two-samples-t-test-in-r

#t test to show results are significantly different
#save Transect and FITT data for relative frequency of buff tails at each site

#From transect
Beettest<-Bees4%>%dplyr::select(Site, Method,BuffFreq)
Beettest

#rename column
#from FITT
Beettest2<-FITT4%>%dplyr::select(Site,Method, BuffFreq)
Beettest2

#combine 

BeeTest3<-rbind(Beettest,Beettest2)            
BeeTest3  

#unpaired t test
#summary stats
group_by(BeeTest3, Method) %>%
  summarise(
    count = n(),
    mean = mean(BuffFreq, na.rm = TRUE),
    sd = sd(BuffFreq, na.rm = TRUE)
  )

ggboxplot(BeeTest3, x = "Method", y = "BuffFreq", 
          color = "Method", palette = c("Blue", "Orange"),
          ylab = "Relative Frequency B.terrestris agg.", xlab = "Observations")+
  ggtitle("Mean and sd of B. terrestris agg: Transect and FITT Count data")


#Shapiro-Wilk normality test for transect and FITT data

with(BeeTest3, shapiro.test(BuffFreq[Method == "Transect"])) #0.07458
with(BeeTest3, shapiro.test(BuffFreq[Method == "FITT"])) #p-value = 0.001301

#FITT data is not normally distributed Therefore use use the non parametric two-samples Wilcoxon rank test.

#compare variances F-test to test for homogeneity in variances

res.ftest <- var.test(BuffFreq ~ Method, data = BeeTest3)
res.ftest

#p-value = 0.96 is greater than 0.05 therefore variances are not significantly different 

#Compute t-test
res <- t.test(BuffFreq~Method, data=BeeTest3,var.equal = TRUE)
res

#p-value = 0.02135 is less than significance level 0.05. 
#Therefore FITT and Transect Buff frequencies are significantly different.

#BIPARTITE ANALYSIS

rm(list=ls())
getwd()
setwd("C:/Users/sarah/OneDrive/Reseach/BeeAnalysis/2021")

library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(bipartite)

BeeForage<- read.csv("ForagePrefIndex.csv", header=TRUE)
names(BeeForage)
head(BeeForage)

as_tibble(BeeForage)

#Find most visited plants
#Group by Species of bee and Plant Species

BeeForSum<-BeeForage%>%
  group_by(Species,PlantSpecies)%>% summarise(TotalBeeVisits=sum(Total.Bee.Visits))

BeeForSum

#Calculate total most popular plants
BeeForSumTot<-BeeForage%>%
  group_by(PlantSpecies)%>% summarise(TotalBeeVisits=sum(Total.Bee.Visits))
BeeForSumTot

#select plants from BeeForSum that make up 95% of total bee visits

#TopPlants

TopPlants<-filter(BeeForSum, TotalBeeVisits > 6)
TopPlants

#spread TopPlants for bipartite analysis

SpreadTopPlants<-TopPlants%>%spread(key=Species, value=TotalBeeVisits, fill= 0)
SpreadTopPlants

#read in Bipartite plants
BipartiteBee<- read.csv("BipartiteBee.csv", header=TRUE)


colnames(BipartiteBee)

matrix_bipartite <- BipartiteBee %>%
  
  # use column 'PlantSpecies' as row name
  column_to_rownames(var = "PlantSpecies") 

View(matrix_bipartite)
plotweb(matrix_bipartite, col.high = "lightgoldenrod", col.low = "lightgoldenrod", adj.low = .95,text.rot=90, 
        adj.high = .05)

Stats<-networklevel(matrix_bipartite)
Stats

#ANALYSIS OF FLORAL DATA

rm(list=ls())
getwd()
setwd("C:/Users/sarah/OneDrive/Reseach/BeeAnalysis/2021")

library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(tidyverse)
library(ggpubr)
library(rstatix)

#Get floral resources data into R

read.csv("Floral Resources2021.csv")

FloralRes <- read.csv("Floral Resources2021.csv", header=TRUE)
names(FloralRes)
head(FloralRes)
as_tibble(FloralRes)

#PlantSpecies richness and cover per site 
summary(FloralRes)

#summarise floral units by Site
FloralunitsbySite<-FloralRes%>%
  group_by(Site)%>%summarise(SumFloralUnits=sum(TotalMsq))
FloralunitsbySite

#add no of visits

#Create a table of Plant Species by Msq for all sites in total - Total Floral density

FloralDensity<-FloralRes%>%
  group_by(PlantSpecies)%>%summarise(SumTotalMsq=sum(TotalMsq))
FloralDensity
#calculate column sum
colSums(FloralDensity[,-1])#total sq meters of forage plants = SumTotalMsq Floral.density = 159528              1 


#Calculate floral density
FloralDensity<-FloralRes%>%
  group_by(PlantSpecies)%>%summarise(SumTotalMsq=sum(TotalMsq),Floral.density=(SumTotalMsq/159528))
FloralDensity

#compare bee floral preference to total available plants

#Get bee floral forage plants "BeeFloral" from Transect data only
read.csv("BeeFloral.csv")

BeeFloral <- read.csv("BeeFloral.csv", header=TRUE)
names(BeeFloral)
head(BeeFloral)

#make Date a recognisable date 

BeeFloral<-mutate(BeeFloral,Date=dmy(Date))
glimpse(BeeFloral)

as_tibble(BeeFloral)

#Create a summary table of Forage plants per species
BeeFloralSummary<-BeeFloral%>%
  group_by(PlantSpecies, Species)%>% summarise(TotalVisits=sum(Number))

BeeFloralSummary

#SpreadBeeFloralSummary
SpreadBeeFloral<-BeeFloralSummary%>%spread(key=Species, value=TotalVisits, fill= 0)
SpreadBeeFloral

sum(BeeFloralSummary$TotalVisits)
#1660 bee visits to plants
mean((BeeFloralSummary$TotalVisits)) #13.49593 
sd(BeeFloralSummary$TotalVisits)#69.95302

#create a summary per site
BeeFloralSummary2<-BeeFloral%>%
  group_by(Site, Species,Caste, PlantSpecies)%>% summarise(Total=sum(Number))

BeeFloralSummary2

#Create a table of total number of species 

#Get Forage Preference Index data 

read.csv("ForagePrefIndex.csv")

FPI <- read.csv("ForagePrefIndex.csv", header=TRUE)
names(FPI)
head(FPI)

as_tibble(FPI)

#group and average flower density per site
FPISummary<-FPI%>%
  group_by(Species,PlantSpecies, Group)%>% summarise(MeanBeeVisitFreq=mean(Bee.Visit.Freq.Alpha), 
                                                     MeanFlowerDensity=mean(FlowerDensity.Beta), 
                                                     FPI=((MeanBeeVisitFreq-MeanFlowerDensity)/MeanFlowerDensity)+1)
FPISummary

#compare the data of 3 groups, first Apis mellifera (group1) with short tongues (group2)
#OK to use Mann Whitney U test with unequal sample sizes

#summarise data into groups

FPIGroup<-group_by(FPISummary, Group) %>%
  summarise(
    count = n(),
    median = median(FPI, na.rm = TRUE),
    IQR = IQR(FPI, na.rm = TRUE))

FPIGroup

#visualise with boxplots

ggboxplot(FPISummary, x = "Group", y = "FPI", 
          color = "Group", 
          ylab = "FPI", xlab = "Groups")

#Select Group 1 and Group 2
FPIGroup1and2<-filter(FPISummary,Group<=2)
FPIGroup1and2

#Is there any significant difference in FPI of Group1 and Group2
FPIWillcox1and2 <- wilcox.test(FPI ~ Group, data =FPIGroup1and2 ,
                               exact = FALSE)
FPIWillcox1and2

#Wilcoxon rank sum test with continuity correction

#data:  FPI by Group
#W = 1139.5, p-value = 0.5862
#alternative hypothesis: true location shift is not equal to 0

#As the p-value is 0.59 the difference is not significant.

#Is there any significant difference in FPI of Group1 and Group3
#select Groups 1 and 3

FPIGroup1and3<-filter(FPISummary,Group==1|Group==3)
FPIGroup1and3

#Is there any significant difference in FPI of Group1 and Group3
FPIWillcox1and3 <- wilcox.test(FPI ~ Group, data =FPIGroup1and3,
                               exact = FALSE)
FPIWillcox1and3

#Wilcoxon rank sum test with continuity correction

#data:  FPI by Group
#W = 489, p-value = 0.01538
#alternative hypothesis: true location shift is not equal to 0
#as p-value is 0.01 it is significantly different 

#Is there a significant difference between Group2 and 3
#select Groups 2 and 3

FPIGroup2and3<-filter(FPISummary,Group>=2)
FPIGroup2and3

#Is there any significant difference in FPI of Group2 and Group3?

FPIWillcox2and3 <- wilcox.test(FPI ~ Group, data =FPIGroup2and3,
                               exact = FALSE)
FPIWillcox2and3

#Wilcoxon rank sum test with continuity correction

#data:  FPI by Group
#W = 772, p-value = 0.0769
#alternative hypothesis: true location shift is not equal to 0

#as p-value is 0.08 the difference is not significant

#visualise top forage plants FPI

read.csv("ForagePrefIndex.csv")

TopForage <- read.csv("TopForage.csv", header=TRUE)
names(TopForage)
head(TopForage)

as_tibble(TopForage)

#calculate mean, sd, SE

TopForageSummary<- TopForage %>%
  group_by(Species, PlantSpecies) %>%
  summarise( 
    n=n(),
    SumVisits=sum(Total.Bee.Visits),
    mean=mean(FPI),
    sd=sd(FPI)
  ) %>%
  mutate( se=sd/sqrt(n))  

TopForageSummary

#turn NAs into zeros
TopForageSummary[is.na(TopForageSummary)] <- 0

#plot into boxplot

TopForageBar<-ggplot(TopForageSummary, aes(fill=Species, y=mean, x=PlantSpecies )) +
  geom_bar(position="dodge", stat="identity")+
  ggtitle("Mean Forage Preference Index: Main Species: Transect Data")+
  theme_bw()+
  xlab("Plant Species")+
  ylab("Mean Forage Preference Index")+ 
  geom_text(aes(y=mean+sd,label=SumVisits), position=position_dodge2(width=0.9), vjust=-.5, size=3)+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,position=position_dodge(.9))

TopForageBar

