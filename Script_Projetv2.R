# Import des librairies
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
if("questionr" %in% rownames(installed.packages()) == FALSE) {install.packages("questionr")};library(questionr)

# Import des fichiers
setwd("C:/Transway/rodrigue/MEDAS/stat_josse/Projet/data_asso")
#setwd("C:/Users/medas/Documents/Medas - Pour étudiant/Data/Projet")

basesirene<-read.csv2("120027016_base-sirene-v3-ss.csv",sep=";",dec='.', encoding='UTF-8')
baserestaurant<-read.csv2("234400034_070-008_offre-touristique-restaurants-rpdl.csv",sep=";",dec='.', encoding='UTF-8')
basefetes<-read.csv2("234400034_070-002_offre-touristique-fetes_et_manifestations-rpdl.csv",sep=";",dec='.', encoding='UTF-8')
basepopulation<-read.csv2("234400034_004-005_population_en_rpdl.csv",sep=";",dec='.', encoding='UTF-8')
baseinternet<-read.csv2("234400034_001-002_deploiement-de-la-fibre-optique-ftth-en-pays-de-la-loire.csv",sep=";",dec='.', encoding='UTF-8')
basegare<-read.csv2("referentiel-gares-voyageurs.csv",sep=";",dec='.', encoding='UTF-8')

# Renommage variable Code_Postal
basesirene <- rename.variable(basesirene, "Code.commune.de.l.établissement", "codeInsee")
baserestaurant <- rename.variable(baserestaurant, "Code.Insee.de.la.Commune", "codeInsee")
basefetes <- rename.variable(basefetes, "Code.Insee.de.la.Commune", "codeInsee")
basepopulation <- rename.variable(basepopulation, "Commune.active..code.", "codeInsee")
baseinternet <- rename.variable(baseinternet, "Code.INSEE", "codeInsee")
basegare <- rename.variable(basegare, "Code.postal", "codeInsee")

# Création code insee par rapport au code departement et code commune (padding de 0)
basegare$Code.commune2<-str_pad(basegare$Code.commune,3,side = c("left"),pad = "0" )
basegare$codeInsee<-paste(basegare$Code.département,basegare$Code.commune2, sep = "")

###### Modification des bases
baseassociation<-subset(basesirene, Nature.juridique.de.l.unité.légale == "Association déclarée"& Etat.administratif.de.l.établissement== "Actif")
basegareLoire <- subset(basegare, Code.département  %in% c(44,49,72,53,85))
baseentreprise50salariesetplus <- subset(basesirene, Etat.administratif.de.l.établissement== "Actif" & Nature.juridique.de.l.unité.légale != "Association déclarée" &  Tranche.de.l.effectif.de.l.établissement %in% c("50 à 99 salariés","5000 à 9999 salariés","10000 salariés et plus"))
basebar <- subset(basesirene, Activité.principale.de.l.établissement == "56.30Z" & Etat.administratif.de.l.établissement== "Actif")
# Population par Année
population_2015<-basepopulation[which(basepopulation==2015),]
population_2014<-basepopulation[which(basepopulation==2014),]
population_2013<-basepopulation[which(basepopulation==2013),]
population_2012<-basepopulation[which(basepopulation==2012),]
population_2011<-basepopulation[which(basepopulation==2011),]

# Base CP distinct
base_CP <- distinct(basesirene, codeInsee, Commune.de.l.établissement)

###### Création des indicateurs ######
baseinternet$Locaux.raccordables.2eme.trimestre.2019[baseinternet$Locaux.raccordables.2eme.trimestre.2019 ==0] <- 0
baseinternet$Locaux.raccordables.2eme.trimestre.2019[baseinternet$Locaux.raccordables.2eme.trimestre.2019 !=0] <- 1
Fibreoupas <- aggregate(baseinternet$Locaux.raccordables.2eme.trimestre.2019,by=list(baseinternet$codeInsee),FUN=sum)
names(Fibreoupas) <- c("codeInsee","fibreoupas")
Fibreoupas$fibreoupas[Fibreoupas$fibreoupas ==0] <- "pasfibre"
Fibreoupas$fibreoupas[Fibreoupas$fibreoupas !=0] <- "fibre"

Nb_assos<-aggregate(baseassociation$SIREN,by=list(baseassociation$codeInsee),FUN=length)
Nb_fetes<-aggregate(basefetes$Nom.de.la.fête.ou.manifestation,by=list(basefetes$codeInsee),FUN=length)
Nb_gare <- aggregate(basegareLoire$Code.plate.forme,by=list(basegareLoire$codeInsee),FUN=length)
Nb_resto <- aggregate(baserestaurant$Nom.de.l.offre.touristique,by=list(baserestaurant$codeInsee),FUN=length)
Nb_entrepriseplusde50s <- aggregate(baseentreprise50salariesetplus$SIREN,by=list(baseentreprise50salariesetplus$codeInsee),FUN=length)
Nb_bar <- aggregate(basebar$SIREN,by=list(basebar$codeInsee),FUN=length)



# Différence de ligne entre 2 bdd
xx <- merge(population_2011, population_2015, by="codeInsee",all.x= T , all.y=T)
donnees_manquante<-xx[which(is.na(xx$Année.x)),]
population_2015 <- subset(population_2015, ! codeInsee %in% c(44000,49000,72000,85000,53000,49382,NA))

Nb_population2015<-aggregate(population_2015$Nombre.de.personnes,by=list(population_2015$codeInsee),FUN=sum)
Nb_population2011<-aggregate(population_2011$Nombre.de.personnes,by=list(population_2011$codeInsee),FUN=sum)
Nb_population2015_15ansemploi<-aggregate(population_2015$Nombre.de.personnes.actives.de.15.ans.ou.plus.ayant.un.emploi,by=list(population_2015$codeInsee),FUN=sum)
Nb_population2011_15ansemploi<-aggregate(population_2011$Nombre.de.personnes.actives.de.15.ans.ou.plus.ayant.un.emploi,by=list(population_2011$codeInsee),FUN=sum)

EvolPop <- cbind(Nb_population2015, Nb_population2015_15ansemploi$x, Nb_population2011$x, Nb_population2011_15ansemploi$x)
names(EvolPop) <- c("codeInsee","Nb_population2015","Nb_population2015_15ansemploi","Nb_population2011", "Nb_population2011_15ansemploi")
EvolPop$EvolutionPop20112015 <- (((EvolPop$Nb_population2011-EvolPop$Nb_population2015)/EvolPop$Nb_population2015)*100)
EvolPop$EvolutionPop20112015plus15ansetemploi <- (((EvolPop$Nb_population2011_15ansemploi-EvolPop$Nb_population2015_15ansemploi)/EvolPop$Nb_population2015_15ansemploi)*100)

EvolPop[EvolPop$Nb_population2015==0,"EvolutionPop20112015"]<-2.804959
mean(EvolPop[EvolPop$Nb_population2015!=0,"EvolutionPop20112015"])

EvolPop[EvolPop$Nb_population2015==0,"EvolutionPop20112015plus15ansetemploi"]<-1.038999
mean(EvolPop[EvolPop$Nb_population2015!=0,"EvolutionPop20112015plus15ansetemploi"])

EvolPop <- subset(EvolPop, select=c("codeInsee","EvolutionPop20112015","EvolutionPop20112015plus15ansetemploi"))

names(Nb_assos) <- c("codeInsee","nbasso")
names(Nb_fetes) <- c("codeInsee","nbfetes")
names(Nb_gare) <- c("codeInsee","nbgare")
names(Nb_resto) <- c("codeInsee","nbresto")
names(Nb_entrepriseplusde50s) <- c("codeInsee","nbentrepriseplusde50s")
names(Fibreoupas) <- c("codeInsee","fibre")
names(Nb_bar) <- c("codeInsee","nbbar")

base1 <- merge(base_CP, Nb_assos, by="codeInsee",all.x= T , all.y=T)
base2 <- merge(base1,Nb_fetes, by="codeInsee",all.x= T , all.y=T)
base3 <- merge(base2, Nb_gare, by="codeInsee",all.x= T , all.y=T)
base4 <- merge(base3, Nb_resto, by="codeInsee",all.x= T , all.y=T)
base5 <- merge(base4, EvolPop, by="codeInsee",all.x= T , all.y=T)
base6 <- merge(base5, Nb_entrepriseplusde50s, by="codeInsee",all.x= T , all.y=T)
base7 <- merge(base6, Nb_bar, by="codeInsee",all.x= T , all.y=T)

base8 <- merge(base7, Fibreoupas, by="codeInsee",all.x= T , all.y=T)


base7 <- subset(base7, ! codeInsee %in% c(0,2498,4007,21711,25056,35236,37170,37213,37231))
base7[which(is.na(base7$nbfetes) == TRUE)] <- 0
base7[is.na(base7[,"nbasso"]),"nbasso"]<-0
base7[is.na(base7[,"nbfetes"]),"nbfetes"]<-0
base7[is.na(base7[,"nbgare"]),"nbgare"]<-0
base7[is.na(base7[,"nbresto"]),"nbresto"]<-0
base7[is.na(base7[,"nbentrepriseplusde50s"]),"nbentrepriseplusde50s"]<-0
base7[is.na(base7[,"EvolutionPop20112015"]),"EvolutionPop20112015"]<-0
base7[is.na(base7[,"EvolutionPop20112015plus15ansetemploi"]),"EvolutionPop20112015plus15ansetemploi"]<-0
base7[is.na(base7[,"nbbar"]),"nbbar"]<-0


base7<-base7[,c("nbasso","nbfetes","nbgare","nbresto","nbentrepriseplusde50s","EvolutionPop20112015","EvolutionPop20112015plus15ansetemploi","nbbar")]

res.pca<-PCA(base7, scale.unit = TRUE, ncp = 5)


summary(base7)

res.pca$var$coord
res.pca$var$contrib

#Mise en classe
hist(jeux_acm$height,breaks = unique(jeux_acm$height))
jeux_acm$height_classe<-cut(jeux_acm$height,breaks = c(0,180,200,215,300))

hist(jeux_acm$weight,breaks = unique(jeux_acm$weight))
jeux_acm$weight_classe<-cut(jeux_acm$weight,breaks = c(0,80,100,120,200))


#Calcul de l'age à la fin de carrière
jeux_acm$annee<-as.integer(substr(jeux_acm$birth_date,nchar(jeux_acm$birth_date)-4,nchar(jeux_acm$birth_date)))
jeux_acm$age_fin<-jeux_acm$year_end-jeux_acm$annee
hist(jeux_acm$age_fin)
jeux_acm$age_fin_classe<-cut(jeux_acm$age_fin,breaks = c(0,22,28,31,35,50))

jeux_acm2<-jeux_acm[,c("age_fin_classe","weight_classe","height_classe","position")]



res.acm<-MCA(jeux_acm2, ncp = 5, ind.sup = NULL, quanti.sup = NULL, 
             quali.sup = NULL, excl=NULL, graph = TRUE, 
             level.ventil = 0, axes = c(1,2), row.w = NULL, 
             method="Indicator", na.method="NA", tab.disj=NULL)
#Nombre d'axe
fviz_screeplot (res.acm, addlabels = TRUE, ylim = c (0, 45))

#Analyse variable
var <- get_mca_var(res.acm)

res.acm$var$coord
res.acm$var$contrib
res.acm$var$cos2

#Graphique variable
fviz_mca_var (res.acm, choice = "mca.cor",
              repel = TRUE, 
              ggtheme = theme_minimal ())

#Graphique modalité
fviz_mca_var (res.acm,
              repel = TRUE, 
              ggtheme = theme_minimal ())

#Graphique individu
fviz_mca_ind(res.acm, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             #repel = TRUE, 
             ggtheme = theme_minimal())







##BROUILLON

names(Indicateurs) <- c("Metier","camoyen","resmoyen", "txresmoyen", "caec","resec","txresec", "caq10", "caq90", "txresq10", "txresq90","txresq025","txresq975","NbparMetier","txvaq10", "txvaq90", "txebeq10", "txebeq90",  "txvamoyen", "txebemoyen", "txvaec", "txebeec")

xx <- merge(baseassociation, basesirene, by="Code_Postal")

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
generation_max<-aggregate(generation3 ~ codeInsee, data = baseinternet, max)
test<-data.frame(table(baseinternet$codeInsee,baseinternet$generation, baseinternet$Operateur))
install.packages("reshape")
library(reshape)
colnames(test)
subjmeans <- cast(test, Var1~Var2, mean)