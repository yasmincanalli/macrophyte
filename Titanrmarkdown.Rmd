# Titan for macrophytes

\ 

***

\ 

## Titan analyse

We derived species and community response thresholds to depth and transparency applying the Threshold Indicator Taxa ANalysis (TITAN). This analysis detects simultaneous changes in abundance and frequency and correlates with abiotic data. The method uses the **Indicator Values** (IndVal; Dufrêne and Legendre, 1997) to describe species’ associations with the environmental gradient. IndVal is an index which the maximum value is retrieved when all individuals of a species is in the same group and in all sites of this group. TITAN’s innovation is the partition of the abiotic variation to search the maximum IndVal per group, and provides information for the community and each species. Communities can respond negatively (Z-) or positively (Z+) to the environmental gradient and the change points can show a **non-linear response for the community**. The result expresses **purity** (bootstrap of response directionality) and **reliability** (bootstrap of IndVal), both as an index of uncertainty of the location of the taxa and community change points. We utilized **1000 bootstraps** and **confidence intervals of 5% and 95%**, **purity and reliability indices > 0.95**. Species with less than five occurrences in the dataset were removed due to method limitations (see Baker and King, 2010). The analyzes were performed in the 'TITAN2' package (Baker, King and Kahle, 2019) in the **R environment** (R Core Team, 2020).
\ 

***

\ 

## Data

The species were measured from the percentage of coverage of each plot. Depth measured used using weighted tape measure and transparecy with Secchi disc.

**sp1** | **sp2** | **depth** | **transparency**
:--- | :---: | :---: | :---:
0 | 100 | 1.7 | 0.10 |
5 | 50 | 1.40 | 0.70 |
95 | 40 | 3.0 | 0.55 |

## Script
```{r}
################
#Titam         #
################

#Reading the package TITAN2
library(TITAN2)

#Loading data
setwd("E:/analyse") #search the directory
dir()
data<-read.table("data.txt", h=T, sep="\t", stringsAsFactors = T)
summary(data)
str(data)

#Break biotic and abiotic data
abiot<-data[,19:20]
parc<-data[,5:18]


#Removing plots with occorrence of species equal to 0
parc2<-parc[rowSums(parc[, -1])>0, ]
abiot2<-abiot[rowSums(parc[, -1])>0, ]

#Removing species with less than 5 occurrences
apply(parc2,2,function(x) sum(x > 0)) #Frequency of species

parc2<-parc2[, which(apply(parc2,2,function(x) sum(x > 0))!=1)]
parc2<-parc2[, which(apply(parc2,2,function(x) sum(x > 0))!=2)]
parc2<-parc2[, which(apply(parc2,2,function(x) sum(x > 0))!=3)]
parc2<-parc2[, which(apply(parc2,2,function(x) sum(x > 0))!=4)]
parc2<-parc2[, which(apply(parc2,2,function(x) sum(x > 0))!=5)]

apply(parc2,2,function(x) sum(x > 0)) #Frequency of species with more 5 occurrences

###########################################################################################

#Analyzing depth
resdep=titan(abiot2$Depth,parc2,numPerm=1000,nBoot=1000,ncpus=6,pur.cut=0.95,rel.cut=0.95)

#Depth graphic
par(mar=c(5,5,5,5))
par(cex=1.3)
plot_sumz(resdep,cex.axis=2, cex.lab=2.5) #Use sumz1=FALSE or sumz2=FALSE, for exclude positive or negative response
par(cex=1.5)
plot_taxa(resdep) #Use z1=FALSE or 2=FALSE, for exclude positive or negative response
resdep$sppmax
resdep$sumz.cp

###########################################################################################
#Analyzing transparency
restra=titan(abiot2$MedTransp,parc2,numPerm=1000,nBoot=1000,ncpus=6,pur.cut=0.95,rel.cut=0.95)

#Transparency graphic
par(mar=c(5,5,5,5))
par(cex=1.3)
plot_sumz(restra,cex.axis=2, cex.lab=2.5) #Use sumz1=FALSE or sumz2=FALSE, for exclude positive or negative response
par(cex=1.5)
plot_taxa(restra) #Use z1=FALSE or 2=FALSE, for exclude positive or negative response
restra$sppmax
restra$sumz.cp
```

