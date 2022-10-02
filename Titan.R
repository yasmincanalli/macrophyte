################
#Titam         #
################

#Reading the package TITAN2
library(TITAN2)
library(ggplot2)
library(tibble)
library(vegan)
library(tidyverse)

#Loading data
setwd("E:/analises/doutorado")
dir()
dados.completos<-read.table("dados.completos.txt", h=T, sep="\t", stringsAsFactors = T)
summary(dados.completos)
str(dados.completos)

#Break biotic and abiotic data
abiot<-dados.completos[,19:30]
parc<-dados.completos[,5:18]


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
resprof=titan(abiot2$Depth,parc2,numPerm=1000,nBoot=1000,ncpus=6,pur.cut=0.95,rel.cut=0.95)

#Depth graphic
par(mar=c(5,5,5,5))
par(cex=1.0)
plot_sumz(resprof, pch1=16, pch2=16, , col1="blue", col2="red") #Use sumz1=FALSE or sumz2=FALSE, for exclude positive or negative response
par(cex=1.5)
plot_taxa(resprof) #Use z1=FALSE or 2=FALSE, for exclude positive or negative response
resprof$sppmax
resprof$sumz.cp


resprof_resultados<-as.data.frame(resprof$sppmax)
row.names(resprof_resultados)
species_ND_resultados<-data.frame(código=row.names(resprof_resultados),species=c("Cabfur", "Cypgar", "Cypble", "Egeden", "Eicazu", "Eiccra", "Hydnym", "Nymcae", "Poa1", "Salaur", "Utrbre", "Utrfol"))
row.names(species_ND_resultados)<-species_ND_resultados$código
names(resprof_resultados)[names(resprof_resultados)=='5%']<-'X5.'
names(resprof_resultados)[names(resprof_resultados)=='95%']<-'X95.'
resprof_resultados
species_ND_resultados

(63.14+73.37+24.69+63.14)/4
(243.61+237.07+73.37+48.40)/4

titan_prof <- resprof_resultados %>%
  rownames_to_column(var = "código") %>% # agora o nome das linhas eh uma coluna da planilha de dados
  filter(purity > 0.95 & reliability > 0.95) %>% # filtrando apenas
  mutate(maxgrp = ifelse(maxgrp == 1, "z-", "z+")) %>% # mudando os codigos para z- e z+
  left_join(species_ND_resultados, by = "código") %>% # juntando com uma planilha que contem nomes e codigos das especies
  mutate(species = gsub(pattern = "Desconhecido", replacement = "", x = species)) # removendo a palavra desconhecido de tudo
titan_prof


ggplot(data = titan_prof, mapping = aes(x = reorder(species, -zenv.cp), y = zenv.cp, fill = maxgrp)) + # criando grafico do ggplot
  geom_errorbar(mapping = aes(ymin = X5., ymax = X95., color = maxgrp), width = 0, size = 1.3) + # adicionando barras de erro
  geom_point(shape = 21, size = 6, colour = "black", stroke = 0.8) + # adicionando pontos
  geom_hline(yintercept=1.90,color="darkblue",linetype="dashed",size=1.2)+
  geom_hline(yintercept=1.53,color="red",linetype="dashed",size=1.2)+
  scale_fill_manual(values = c("red", "darkblue")) + # mudando cores dos pontos
  scale_color_manual(values = c("red", "darkblue")) + # mudando cores das barras de erro
  coord_flip() + # invertendo eixos
  ylab("Depth") + # titulo do eixo y
  xlab("Species") + # titulo do eixo x
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 0.5), # opcoes graficas
        panel.grid.major = element_line(colour = NA),
        panel.grid.minor = element_line(colour = NA),
        axis.text.x = element_text(colour = "black", size = 22),
        axis.text.y = element_text(colour = "black", size = 22,face = "italic"),
        axis.title = element_text(colour = "black", size = 24, face = "bold"),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.text = element_text(face = "bold", colour = "black", size = 24))



###########################################################################################
#Analyzing transparency
restr=titan(abiot2$MedTransp,parc2,numPerm=100,nBoot=100,ncpus=6,pur.cut=0.95,rel.cut=0.95)

#Transparency graphic
par(mar=c(5,5,5,5))
par(cex=1.3)
plot_sumz(restr,cex.axis=2, cex.lab=2.5)
par(cex=1.5)
plot_taxa(restr)
restr$sppmax
restr$sumz.cp

restr_resultados<-as.data.frame(restr$sppmax)
row.names(restr_resultados)
species_ND_resultados<-data.frame(código=row.names(restr_resultados),species=c("Cabfur", "Cypgar", "Cypble", "Egeden", "Eicazu", "Eiccra", "Hydnym", "Nymcae", "Poa1", "Salaur", "Utrbre", "Utrfol"))
row.names(species_ND_resultados)<-species_ND_resultados$código
names(restr_resultados)[names(restr_resultados)=='5%']<-'X5.'
names(restr_resultados)[names(restr_resultados)=='95%']<-'X95.'
restr_resultados
species_ND_resultados

(63.14+73.37+24.69+63.14)/4
(243.61+237.07+73.37+48.40)/4

titan_tr <- restr_resultados %>%
  rownames_to_column(var = "código") %>% # agora o nome das linhas eh uma coluna da planilha de dados
  filter(purity > 0.95 & reliability > 0.95) %>% # filtrando apenas
  mutate(maxgrp = ifelse(maxgrp == 1, "z-", "z+")) %>% # mudando os codigos para z- e z+
  left_join(species_ND_resultados, by = "código") %>% # juntando com uma planilha que contem nomes e codigos das especies
  mutate(species = gsub(pattern = "Desconhecido", replacement = "", x = species)) # removendo a palavra desconhecido de tudo
titan_tr


ggplot(data = titan_tr, mapping = aes(x = reorder(species, -zenv.cp), y = zenv.cp, fill = maxgrp)) + # criando grafico do ggplot
  geom_errorbar(mapping = aes(ymin = X5., ymax = X95., color = maxgrp), width = 0, size = 1.3) + # adicionando barras de erro
  geom_point(shape = 21, size = 6, colour = "black", stroke = 0.8) + # adicionando pontos
  geom_hline(yintercept=0.24,color="darkblue",linetype="dashed",size=1.2)+
  geom_hline(yintercept=0.23,color="red",linetype="dashed",size=1.2)+
  scale_fill_manual(values = c("red", "darkblue")) + # mudando cores dos pontos
  scale_color_manual(values = c("red", "darkblue")) + # mudando cores das barras de erro
  coord_flip() + # invertendo eixos
  ylab("Transparency") + # titulo do eixo y
  xlab("Species") + # titulo do eixo x
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 0.5), # opcoes graficas
        panel.grid.major = element_line(colour = NA),
        panel.grid.minor = element_line(colour = NA),
        axis.text.x = element_text(colour = "black", size = 26),
        axis.text.y = element_text(colour = "black", size = 26,face = "italic"),
        axis.title = element_text(colour = "black", size = 28, face = "bold"),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.text = element_text(face = "bold", colour = "black", size = 28))

###########################################################################################
