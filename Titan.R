################
#Titam         #
################

library(TITAN2)

#Carregando dados
setwd("E:/analises/doutorado")
dir()
dados.completos<-read.table("dados.completos.txt", h=T, sep="\t", stringsAsFactors = T)
summary(dados.completos)
str(dados.completos)

#Separando biótico e abiótico
abiot<-dados.completos[,19:30]
parc<-dados.completos[,5:18]


#Tirando os dados com 0
parc2<-parc[rowSums(parc[, -1])>0, ]
abiot2<-abiot[rowSums(parc[, -1])>0, ]

#Tirando espécies com ocorrência menor que 5
apply(parc2,2,function(x) sum(x > 0)) #Frequência

parc2<-parc2[, which(apply(parc2,2,function(x) sum(x > 0))!=1)]
parc2<-parc2[, which(apply(parc2,2,function(x) sum(x > 0))!=2)]
parc2<-parc2[, which(apply(parc2,2,function(x) sum(x > 0))!=3)]
parc2<-parc2[, which(apply(parc2,2,function(x) sum(x > 0))!=4)]
parc2<-parc2[, which(apply(parc2,2,function(x) sum(x > 0))!=5)]

apply(parc2,2,function(x) sum(x > 0)) #Frequência

###########################################################################################

#Analisando profundidade

resprof=titan(abiot2$Depth,parc2,numPerm=1000,nBoot=1000,ncpus=6,pur.cut=0.95,rel.cut=0.95)

#Gráficos profundidade
par(mar=c(5,5,5,5))
par(cex=1.3)
plot_sumz(resprof,cex.axis=2, cex.lab=2.5)
par(cex=1.5)
plot_taxa(resprof)
resprof$sppmax
resprof$sumz.cp

###########################################################################################
#Analisando transparência
restr=titan(abiot2$MedTransp,parc2,numPerm=1000,nBoot=1000,ncpus=6,pur.cut=0.95,rel.cut=0.95)

#Gráficos transparência
par(mar=c(5,5,5,5))
par(cex=1.3)
plot_sumz(restr,cex.axis=2, cex.lab=2.5)
par(cex=1.5)
plot_taxa(restr)
restr$sppmax
restr$sumz.cp

###########################################################################################
#Analisando pH
resph=titan(abiot2$pH,parc2,numPerm=1000,nBoot=1000,ncpus=6,pur.cut=0.95,rel.cut=0.95)

#Gráficos pH
par(mar=c(5,5,5,5))
par(cex=1.3)
plot_sumz(resph,cex.axis=2, cex.lab=2.5)
par(cex=1.8)
plot_taxa(resph)
resph$sppmax
resph$sumz.cp


###########################################################################################

#Analisando Hum
reshum=titan(abiot2$Hum,parc2,numPerm=1000,nBoot=1000,ncpus=6,pur.cut=0.95,rel.cut=0.95)

#Gráficos Hum
par(mar=c(5,5,5,5))
par(cex=1.3)
plot_sumz(reshum,cex.axis=2, cex.lab=2.5)
par(cex=1.8)
plot_taxa(reshum)
reshum$sppmax
reshum$sumz.cp

###########################################################################################

#Analisando Wtemp
reswtemp=titan(abiot2$WatTemp,parc2,numPerm=1000,nBoot=1000,ncpus=6,pur.cut=0.95,rel.cut=0.95)

#Gráficos Wtemp
par(mar=c(5,5,5,5))
par(cex=1.3)
plot_sumz(reswtemp,cex.axis=2, cex.lab=2.5)
par(cex=1.8)
plot_taxa(reswtemp)
reswtemp$sppmax
reswtemp$sumz.cp

###########################################################################################

#Analisando Rtemp
resrtemp=titan(abiot2$Rtemp,parc2,numPerm=1000,nBoot=1000,ncpus=6,pur.cut=0.95,rel.cut=0.95)

#Gráficos Rtemp
par(mar=c(5,5,5,5))
par(cex=1.3)
plot_sumz(resrtemp,cex.axis=2, cex.lab=2.5)
par(cex=1.8)
plot_taxa(resrtemp)
resrtemp$sppmax
resrtemp$sumz.cp

###########################################################################################
#Analisando Rfall
resrfall=titan(abiot2$Rfall,parc2,numPerm=1000,nBoot=1000,ncpus=6,pur.cut=0.95,rel.cut=0.95)

#Gráficos Rfall
par(mar=c(5,5,5,5))
par(cex=1.3)
plot_sumz(resrfall,cex.axis=2, cex.lab=2.5)
par(cex=1.8)
plot_taxa(resrfall,z2=FALSE)
resrfall$sppmax
resrfall$sumz.cp

###########################################################################################

#Analisando Wspeed
reswspeed=titan(abiot2$Wspeed,parc2,numPerm=1000,nBoot=1000,ncpus=6,pur.cut=0.95,rel.cut=0.95)

#Gráficos Wspeed
par(mar=c(5,5,5,5))
par(cex=1.3)
plot_sumz(reswspeed,cex.axis=2, cex.lab=2.5)
par(cex=1.8)
plot_taxa(reswspeed)
reswspeed$sppmax
reswspeed$sumz.cp

###########################################################################################

#Analisando Phperiod
resphperiod=titan(abiot2$Phperiod,parc2,numPerm=1000,nBoot=1000,ncpus=6,pur.cut=0.95,rel.cut=0.95)

#Gráficos Phperiod
par(mar=c(5,5,5,5))
par(cex=1.3)
plot_sumz(resphperiod,cex.axis=2, cex.lab=2.5)
par(cex=1.8)
plot_taxa(resphperiod)
resphperiod$sppmax
resphperiod$sumz.cp

###########################################################################################
#Só transecto 5
cinco<-droplevels(dados.completos[dados.completos$UA=="5E",])
abiot<-cinco[,c(19:29)]
parc<-cinco[,c(5:18)]

parc2<-parc[rowSums(parc[, -1])>0, ]
parc2<-parc2[, which(apply(parc2,2,function(x) sum(x > 0))!=0)]
parc2<-parc2[, which(apply(parc2,2,function(x) sum(x > 0))!=1)]
parc2<-parc2[, which(apply(parc2,2,function(x) sum(x > 0))!=2)]

abiot2<-abiot[rowSums(parc[, -1])>0, ]
abiot2

res=titan(abiot2$MedTransp,parc2,numPerm=1000,nBoot=1000,ncpus=6,pur.cut=0.95,rel.cut=0.95)
plot_taxa(res)
plot_sumz(res)
res$sppmax
res$sumz.cp


###########################################################################################

par(mar=c(5,5,5,5))
par(cex=1.3)
plot_sumz(resprof,cex.axis=2, cex.lab=2.5,sumz2=FALSE)
par(cex=1.5)
plot_taxa(resprof,z2=FALSE)
resprof$sppmax
resprof$sumz.cp
