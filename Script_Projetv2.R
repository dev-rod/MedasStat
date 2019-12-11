# Import des librairie 
if("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")};library(lubridate)
if("FactoMineR" %in% rownames(installed.packages()) == FALSE) {install.packages("FactoMineR")};library(FactoMineR)
if("factoextra" %in% rownames(installed.packages()) == FALSE) {install.packages("factoextra")};library(factoextra)
if("corrplot" %in% rownames(installed.packages()) == FALSE) {install.packages("corrplot")};library(corrplot)
if("stringr" %in% rownames(installed.packages()) == FALSE) {install.packages("stringr")};library(stringr)
if("rgl" %in% rownames(installed.packages()) == FALSE) {install.packages("rgl")};library(rgl)

if("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")};library(dplyr)
if("gsubfn" %in% rownames(installed.packages()) == FALSE) {install.packages("gsubfn")};library(gsubfn)
if("proto" %in% rownames(installed.packages()) == FALSE) {install.packages("proto")};library(proto)
if("RSQLite" %in% rownames(installed.packages()) == FALSE) {install.packages("RSQLite")};library(RSQLite)
if("readxl" %in% rownames(installed.packages()) == FALSE) {install.packages("readxl")};library(readxl)
if("data.table" %in% rownames(installed.packages()) == FALSE) {install.packages("data.table")};library(data.table)
if("sqldf" %in% rownames(installed.packages()) == FALSE) {install.packages("sqldf")};library(sqldf)
if("readxl" %in% rownames(installed.packages()) == FALSE) {install.packages("readxl")};library(readxl)


#Import des fichier 
setwd("C:/Users/medas/Documents/Medas - Pour étudiant/Data/Projet")

basesirene<-read.csv2("120027016_base-sirene-v3-ss.csv",sep=";",dec='.', encoding='UTF-8')
baserestaurant<-read.csv2("234400034_070-008_offre-touristique-restaurants-rpdl.csv",sep=";",dec='.', encoding='UTF-8')
basefetes<-read.csv2("234400034_070-002_offre-touristique-fetes_et_manifestations-rpdl.csv",sep=";",dec='.', encoding='UTF-8')
basepopulation<-read.csv2("234400034_004-005_population_en_rpdl.csv",sep=";",dec='.', encoding='UTF-8')
baseinternet<-read.csv2("234400034_001-002_deploiement-de-la-fibre-optique-ftth-en-pays-de-la-loire.csv",sep=";",dec='.', encoding='UTF-8')
basegare<-read.csv2("referentiel-gares-voyageurs.csv",sep=";",dec='.', encoding='UTF-8')


#Rennomage variable Code_Postal
basesirene <- rename.variable(basesirene, "Code.postal.de.l.établissement", "codePostal")
baserestaurant <- rename.variable(baserestaurant, "Code.postal", "codePostal")
basefetes <- rename.variable(basefetes, "Code.postal", "codePostal")
basepopulation <- rename.variable(basepopulation, "Commune.active..code.", "codePostal")
baseinternet <- rename.variable(baseinternet, "Code.INSEE", "codePostal")
basegare <- rename.variable(basegare, "Code.postal", "codePostal")
  
#Modificarion des base
baseassociation<-subset(basesirene, Nature.juridique.de.l.unité.légale == "Association déclarée")
basegareLoire <- subset(basegare, Code.département == 44 | Code.département == 49 | Code.département ==72 | Code.département==53 | Code.département == 85)
baseentreprise50salariesetplus <- subset(basesirene, Etat.administratif.de.l.établissement== "Actif" | Nature.juridique.de.l.unité.légale != "Association déclarée" | Tranche.de.l.effectif.de.l.établissement == "50 à 99 salariés" | Tranche.de.l.effectif.de.l.établissement == "100 à 199 salariés"| Tranche.de.l.effectif.de.l.établissement == "200 à 249 salariés" | Tranche.de.l.effectif.de.l.établissement == "200 à 499 salariés" | Tranche.de.l.effectif.de.l.établissement == "500 à 999 salariés"| Tranche.de.l.effectif.de.l.établissement == "1000 à 1999 salariés" | Tranche.de.l.effectif.de.l.établissement == "2000 à 4999 salariés" | Tranche.de.l.effectif.de.l.établissement == "5000 à 9999 salariés" | Tranche.de.l.effectif.de.l.établissement == "10000 salariés et plus")





population_2015<-basepopulation[which(basepopulation==2015),]
population_2014<-basepopulation[which(basepopulation==2014),]
population_2013<-basepopulation[which(basepopulation==2013),]
population_2012<-basepopulation[which(basepopulation==2012),]
population_2011<-basepopulation[which(basepopulation==2011),]

# Base CP distinct
base_CP <- distinct(basesirene, codePostal, Commune.de.l.établissement)


 

#Création d'indicateurs

Nb_assos<-aggregate(baseassociation$SIREN,by=list(baseassociation$codePostal),FUN=length)
Nb_fetes<-aggregate(basefetes$Nom.de.la.fête.ou.manifestation,by=list(basefetes$codePostal),FUN=length)
Nb_gare <- aggregate(basegareLoire$Code.plate.forme,by=list(basegareLoire$codePostal),FUN=length)
Nb_resto <- aggregate(baserestaurant$Nom.de.l.offre.touristique,by=list(baserestaurant$codePostal),FUN=length)
#basDébit <- aggregate(baseinternetbasdébit$generation2,by=list(baseinternetbasdébit$codePostal),FUN=length)
#hautDébit <- aggregate(baseinternethautdébit$generation2,by=list(baseinternethautdébit$codePostal),FUN=length)

Nb_population2015<-aggregate(population_2015$Nombre.de.personnes,by=list(population_2015$codePostal),FUN=sum)
Nb_population2011<-aggregate(population_2011$Nombre.de.personnes,by=list(population_2011$codePostal),FUN=sum)
Nb_population2015_15ansemploi<-aggregate(population_2015$Nombre.de.personnes.actives.de.15.ans.ou.plus.ayant.un.emploi,by=list(population_2015$codePostal),FUN=sum)
Nb_population2011_15ansemploi<-aggregate(population_2011$Nombre.de.personnes.actives.de.15.ans.ou.plus.ayant.un.emploi,by=list(population_2011$codePostal),FUN=sum)


POPULATIONTEST <- cbind(Nb_population2015, Nb_population2015_15ansemploi$x, Nb_population2011$x, Nb_population2011_15ansemploi$x)
POP <- cbind(population_2011, population_2015)

pp <- left_join(population_2011, population_2015, by = "codePostal")
pp <- right_join(population_2011, population_2015, by = "codePostal")
ppp <- setdiff(population_2015, population_2011, by = "codePostal")

dim(population_2011)
dim(population_2015)

xx <- merge(population_2011, population_2015, by="codePostal",all.x= T , all.y=T)
donnees_manquante<-xx[which(is.na(xx$Année.x)),]

Indicateurs <-cbind(codePostal, $x, $x)
names(Indicateurs) <- c("Metier","camoyen","resmoyen", "txresmoyen", "caec","resec","txresec", "caq10", "caq90", "txresq10", "txresq90","txresq025","txresq975","NbparMetier","txvaq10", "txvaq90", "txebeq10", "txebeq90",  "txvamoyen", "txebemoyen", "txvaec", "txebeec")

xx <- merge(baseassociation, basesirene, by="Code_Postal")


##


baseinternet$generation3[baseinternet$generation =="2G"] <- 2
baseinternet$generation3[baseinternet$generation =="3G"] <- 3
baseinternet$generation3[baseinternet$generation =="4G"] <- 4

tapply(baseinternet,baseinternet$generation3,max)

baseinternet$generation2[baseinternet$generation =="2G"] <- "basDébit"
baseinternet$generation2[baseinternet$generation =="3G"] <- "hautDébit"
baseinternet$generation2[baseinternet$generation =="4G"] <- "hautDébit"

baseinternetbasdébit <- subset(baseinternet, generation2 = "basDébit")
baseinternethautdébit <- subset(baseinternet, generation2 = "hautDébit")



str(baseinternet)
baseinternet$Date.de.mise.en.service<-as.Date(baseinternet$Date.de.mise.en.service)
baseinternet$generation3
generation_max<-aggregate(generation3 ~ codePostal, data = baseinternet, max)
test<-data.frame(table(baseinternet$codePostal,baseinternet$generation, baseinternet$Operateur))
install.packages("reshape")
library(reshape)
colnames(test)
subjmeans <- cast(test, Var1~Var2, mean)