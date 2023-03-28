# Loading required packages
require(TITAN2)
require(ggplot2)
require(tibble)
require(vegan)
require(tidyverse)

#Loading and formatting data
data<-read.table("data/data.txt", h=T, row.names=1)
names(data)
abiotic.data<-data[,19:20]
biotic.data<-data[,5:18]
abiotic.data<-abiotic.data[rowSums(biotic.data[, -1])>0, ] #Removing plots with occurrence of species equal to 0
biotic.data<-biotic.data[rowSums(biotic.data[, -1])>0, ] #Removing plots with occurrence of species equal to 0
biotic.data<-biotic.data[, which(apply(biotic.data,2,function(x) sum(x > 0))!=1)] #Removing species with less than 5 occurrences
biotic.data<-biotic.data[, which(apply(biotic.data,2,function(x) sum(x > 0))!=2)] #Removing species with less than 5 occurrences
biotic.data<-biotic.data[, which(apply(biotic.data,2,function(x) sum(x > 0))!=3)] #Removing species with less than 5 occurrences
biotic.data<-biotic.data[, which(apply(biotic.data,2,function(x) sum(x > 0))!=4)] #Removing species with less than 5 occurrences
biotic.data<-biotic.data[, which(apply(biotic.data,2,function(x) sum(x > 0))!=5)] #Removing species with less than 5 occurrences

# TITAN - Depth #
# Change the number of permutations for deriving reliable results #
# We are using 10 in this example given the large size of the matrix #
titan.depth=titan(abiotic.data$Depth,biotic.data,numPerm=100,nBoot=100,ncpus=6,pur.cut=0.95,rel.cut=0.95) 
write.table(titan.depth$sppmax,"results/titan_depth_species.txt")
write.table(titan.depth$sumz.cp,"results/titan_depth_communities.txt")

png("figures/titan_depth_community.png")
par(mar=c(5,5,5,5))
par(cex=1.0)
plot_sumz(titan.depth, pch1=16, pch2=16, col1="blue", col2="red") #Use sumz1=FALSE or sumz2=FALSE, for exclude positive or negative response
dev.off()

png("figures/titan_depth_species.png")
par(mar=c(5,5,5,5))
par(cex=1.0)
plot_taxa(titan.depth) #Use z1=FALSE or z2=FALSE, for exclude positive or negative responses.
dev.off()

# TITAN - Transparency #
# Change the number of permutations for deriving reliable results #
# We are using 100 in this example given the large size of the matrix #
titan.transparency=titan(abiotic.data$MeanTransp,biotic.data,numPerm=100,nBoot=100,ncpus=6,pur.cut=0.95,rel.cut=0.95)
write.table(titan.transparency$sppmax,"results/titan_transparency_species.txt")
write.table(titan.transparency$sumz.cp,"results/titan_transparency_communities.txt")

png("figures/titan_transparency_community.png")
par(mar=c(5,5,5,5))
par(cex=1.0)
plot_sumz(titan.transparency, pch1=16, pch2=16, col1="blue", col2="red") #Use sumz1=FALSE or sumz2=FALSE, for exclude positive or negative response
dev.off()

png("figures/titan_transparency_species.png")
par(mar=c(5,5,5,5))
par(cex=1.0)
plot_taxa(titan.transparency) #Use z1=FALSE or z2=FALSE, for exclude positive or negative responses.
dev.off()
