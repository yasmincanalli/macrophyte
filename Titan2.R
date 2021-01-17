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