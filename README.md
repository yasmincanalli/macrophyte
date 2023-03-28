# Macrophyte Thresholds in a Atlantic Forest wetland: first release

## Titan analyse

Data and script to estimate the response of macrophyte species and assemblages to variation in transparency and depth in a Atlantic Forest wetland using a threshold indicator analysis (TITAN)l. These results are discussed in the manuscript "Increasing depth reduces macrophyte coverage but increasing transparency promotes composition turnover through environmental thresholds", accepted for publication in the journal Marine & Freshwater Research.

# Data analysis for the manuscript: Increasing depth reduces macrophyte coverage but increasing transparency promotes composition turnover through environmental thresholds
 
## Authors
Yasmin Canalli, Bruno E. Soares, and Cássia M. Sakuragui

## Published in
Accepted in Marine & Freshwater Research.

## To cite this data
Data and R coding available in Zenodo: XXXX.
Cite as: XXX

## Description
Data and R coding to analyze the response of macrophytes to variation in transparency and depth in a Atlantic Forest wetland. These results are discussed in the manuscript  "Increasing depth reduces macrophyte coverage but increasing transparency promotes composition turnover through environmental thresholds", accepted for publication in Marine & Freshwater Research.

## How to use this directory
In R, you may download all the data contained in this repository by using the download.file() function, then use the function unzip(). Example:

`
download.file(url="https://github.com/bruno-soares/MS_MacrophyteThresholds/archive/master.zip", destfile = "MS_MacrophyteThresholds.zip")
`

`
unzip(zipfile = "MS_MacrophyteThresholds.zip")
`

Analysis is summarized in a single R script. All the data necessary to run analyses are availagle in /data. In this example, we used 100 permutations for all analysis for examplifying the coding, but published analysis used 1,000 permutations.

## Contact information
Please, feel free to contact me in my personal e-mail: soares.e.bruno@gmail.com or yasmincanalli@hotmail.com

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

