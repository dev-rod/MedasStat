# Import des librairies
if("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")};library(lubridate)
if("FactoMineR" %in% rownames(installed.packages()) == FALSE) {install.packages("FactoMineR")};library(FactoMineR)
if("factoextra" %in% rownames(installed.packages()) == FALSE) {install.packages("factoextra")};library(factoextra)
if("corrplot" %in% rownames(installed.packages()) == FALSE) {install.packages("corrplot")};library(corrplot)
if("rgl" %in% rownames(installed.packages()) == FALSE) {install.packages("rgl")};library(rgl)

if("gsubfn" %in% rownames(installed.packages()) == FALSE) {install.packages("gsubfn")};library(gsubfn)
if("proto" %in% rownames(installed.packages()) == FALSE) {install.packages("proto")};library(proto)
if("RSQLite" %in% rownames(installed.packages()) == FALSE) {install.packages("RSQLite")};library(RSQLite)
if("readxl" %in% rownames(installed.packages()) == FALSE) {install.packages("readxl")};library(readxl)
if("data.table" %in% rownames(installed.packages()) == FALSE) {install.packages("data.table")};library(data.table)
if("sqldf" %in% rownames(installed.packages()) == FALSE) {install.packages("sqldf")};library(sqldf)
if("readxl" %in% rownames(installed.packages()) == FALSE) {install.packages("readxl")};library(readxl)

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
if("questionr" %in% rownames(installed.packages()) == FALSE) {install.packages("questionr")};library(questionr)
basesirene <- rename.variable(basesirene, "Code.commune.de.l.établissement", "codeInsee")
basesirene <- rename.variable(basesirene, "Commune.de.l.établissement", "commune")
baserestaurant <- rename.variable(baserestaurant, "Code.Insee.de.la.Commune", "codeInsee")
basefetes <- rename.variable(basefetes, "Code.Insee.de.la.Commune", "codeInsee")
basepopulation <- rename.variable(basepopulation, "Commune.active..code.", "codeInsee")
baseinternet <- rename.variable(baseinternet, "Code.INSEE", "codeInsee")
basegare <- subset(basegare, is.na(Code.département) == FALSE)

# basegare : Création code insee par rapport au code departement et code commune (padding de 0)
if("stringr" %in% rownames(installed.packages()) == FALSE) {install.packages("stringr")};library(stringr)
basegare$Code.département<-str_pad(basegare$Code.département,2,side = c("left"),pad = "0" )
basegare$Code.commune2<-str_pad(basegare$Code.commune,3,side = c("left"),pad = "0" )
basegare$codeInsee<-paste(basegare$Code.département,basegare$Code.commune2, sep = "")

# Initialisation des bases complémentaires depuis la base sirene
baseassociation<-subset(basesirene, Nature.juridique.de.l.unité.légale == "Association déclarée" & Etat.administratif.de.l.établissement== "Actif")
basegareLoire <- subset(basegare, Code.département  %in% c(44,49,72,53,85))
baseentreprise50salariesetplus <- subset(basesirene, Etat.administratif.de.l.établissement== "Actif" & Nature.juridique.de.l.unité.légale != "Association déclarée" &  Tranche.de.l.effectif.de.l.établissement %in% c("50 à 99 salariés","5000 à 9999 salariés","10000 salariés et plus"))
basebar <- subset(basesirene, Activité.principale.de.l.établissement == "56.30Z" & Etat.administratif.de.l.établissement== "Actif")

# Population par Année
population_2011<-basepopulation[which(basepopulation==2011),]
population_2012<-basepopulation[which(basepopulation==2012),]
population_2013<-basepopulation[which(basepopulation==2013),]
population_2014<-basepopulation[which(basepopulation==2014),]
population_2015<-basepopulation[which(basepopulation==2015),]

# Base des codes communes uniques
if("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")};library(dplyr)
basesirene$commune <- str_replace_all(basesirene$commune, "-", " ")
basesirene$commune <- str_replace_all(basesirene$commune, 'È', 'E')
base_CP <- distinct(basesirene, codeInsee, commune)

# Création des indicateurs

# Variables qualitatives
baseinternet$Locaux.raccordables.2eme.trimestre.2019[baseinternet$Locaux.raccordables.2eme.trimestre.2019 ==0] <- 0
baseinternet$Locaux.raccordables.2eme.trimestre.2019[baseinternet$Locaux.raccordables.2eme.trimestre.2019 !=0] <- 1
Fibreoupas <- aggregate(baseinternet$Locaux.raccordables.2eme.trimestre.2019,by=list(baseinternet$codeInsee),FUN=sum)
names(Fibreoupas) <- c("codeInsee","fibreoupas")
Fibreoupas$fibreoupas[Fibreoupas$fibreoupas ==0] <- FALSE
Fibreoupas$fibreoupas[Fibreoupas$fibreoupas !=0] <- TRUE

# Variables quantitatives
Nb_fetes<-aggregate(basefetes$Nom.de.la.fête.ou.manifestation,by=list(basefetes$codeInsee),FUN=length)
Nb_assos<-aggregate(baseassociation$SIREN,by=list(baseassociation$codeInsee),FUN=length)
Nb_gare <- aggregate(basegareLoire$Code.plate.forme,by=list(basegareLoire$codeInsee),FUN=length)
Nb_resto <- aggregate(baserestaurant$Nom.de.l.offre.touristique,by=list(baserestaurant$codeInsee),FUN=length)
Nb_entrepriseplusde50s <- aggregate(baseentreprise50salariesetplus$SIREN,by=list(baseentreprise50salariesetplus$codeInsee),FUN=length)
Nb_bar <- aggregate(basebar$SIREN,by=list(basebar$codeInsee),FUN=length)
Nb_population<-aggregate(population_2015$Nombre.de.personnes,by=list(population_2015$codeInsee),FUN=sum)

# Nettoyage des population_2015 de ligne entre 2 bdd
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
names(Nb_resto) <- c("codeInsee","nbresto")
names(Nb_entrepriseplusde50s) <- c("codeInsee","nbentrepriseplusde50s")
names(Fibreoupas) <- c("codeInsee","fibre")
names(Nb_bar) <- c("codeInsee","nbbar")
names(Nb_gare) <- c("codeInsee","nbgare")
names(Nb_population) <- c("codeInsee","nbpopulation")

base1 <- merge(base_CP, Nb_assos, by="codeInsee",all.x= T , all.y=T)
base1$nbasso[is.na(base1$nbasso) == T] <- median(base1$nbasso, na.rm=T)

base2 <- merge(base1, Nb_fetes, by="codeInsee", all.x= T, all.y=T)
base2 <- subset(base2, is.na(commune) == F)
base2$nbfetes[is.na(base2$nbfetes) == T] <- 0

base3 <- merge(base2, Nb_gare, by="codeInsee", all.x= T, all.y=T)
base3$nbgare[is.na(base3$nbgare) == T] <- 0

base4 <- merge(base3, Nb_resto, by="codeInsee", all.x= T, all.y=T)
base4$nbresto[is.na(base4$nbresto) == T] <- 0

base5 <- merge(base4, Nb_entrepriseplusde50s, by="codeInsee", all.x= T, all.y=T)
base5 <- subset(base5, is.na(commune) == F)

base6 <- merge(base5, Nb_bar, by="codeInsee", all.x= T, all.y=T)

# base61 <- merge(base6, EvolPop, by="codeInsee", all.x= T, all.y=T)
# base61 <- subset(base61, is.na(commune) == F)

base7 <- merge(base6, Nb_population, by="codeInsee", all.x= T, all.y=T)
base7$nbentrepriseplusde50s[is.na(base7$nbentrepriseplusde50s) == T] <- 0


base7[is.na(base7[,"nbasso"]),"nbasso"]<-0
base7[is.na(base7[,"nbfetes"]),"nbfetes"]<-0
base7[is.na(base7[,"nbgare"]),"nbgare"]<-0
base7[is.na(base7[,"nbresto"]),"nbresto"]<-0
base7[is.na(base7[,"nbentrepriseplusde50s"]),"nbentrepriseplusde50s"]<-0
base7[is.na(base7[,"nbbar"]),"nbbar"]<-0
base7 <- subset(base7,! codeInsee %in% c(0,2498,4007,21711,25056,35236,37170,37213,37231))

basepetitecommune <- subset(base7,nbpopulation <=10000 )
basemoyennecommune <- subset(base7,nbpopulation >=10000 & nbpopulation <=50000)
basegrossecommune <- subset(base7,nbpopulation >=50000)
View(basepetitecommune)

write.csv(basepetitecommune,"basepetitecommune.csv", quote=F, row.names = F)
write.csv(basemoyennecommune,"basemoyennecommune.csv", quote=F, row.names = F)
write.csv(basegrossecommune,"basegrossecommune.csv", quote=F, row.names = F)


basepetitecommune<-basepetitecommune[,c("nbasso","nbfetes","nbgare","nbresto","nbentrepriseplusde50s","nbbar")]
basemoyennecommune<-basemoyennecommune[,c("nbasso","nbfetes","nbgare","nbresto","nbentrepriseplusde50s","nbbar")]
basegrossecommune<-basegrossecommune[,c("nbasso","nbfetes","nbgare","nbresto","nbentrepriseplusde50s","nbbar")]


res.pca<-PCA(basepetitecommune, scale.unit = TRUE, ncp = 5)
res.pca<-PCA(basemoyennecommune, scale.unit = TRUE, ncp = 5)
res.pca<-PCA(basegrossecommune, scale.unit = TRUE, ncp = 5)


basepetiteetmoyennecommune <- subset(base7,nbpopulation <=50000)
basepetiteetmoyennecommune<-basepetiteetmoyennecommune[,c("nbasso","nbfetes","nbgare","nbresto","nbentrepriseplusde50s","nbbar")]





##### -> à modifier base selon l'acp
res.pca<-PCA(basepetiteetmoyennecommune, scale.unit = TRUE, ncp = 5)
#Analyse contribution par axe
corrplot(var$contrib, is.corr=FALSE)    


#Cercle des corrélations 
fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Évite le chevauchement de texte
)

#Analyse des individus
fviz_pca_ind (res.pca, col.ind = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
              # repel = TRUE # Évite le chevauchement de texte
)

plot(res.pca$eig[,2])

res.pca$var$coord

res.pca$var$contrib

plot(res.pca$ind$coord[,1:2])



#Réaliser une classification grâce à la méthode Kmeans à partir de l'ACM réalisé précédemment
coord_individu<-res.pca$ind$coord
#Calcul d'une segmentation à 5 classes
res_kmeans<-kmeans(coord_individu,center=5,iter.max=100,nstart=50)

#Centre 
res_kmeans$centers

str(individu_clust)

individu_clust<-data.frame(coord_individu,as.factor(res_kmeans$cluster))
colnames(individu_clust)<-c("dim1","dim2","dim3","dim4","dim5","cluster")
table(individu_clust$cluster)

#Variance totale
res_kmeans$totss
#Variance intra
res_kmeans$tot.withinss
#Variance intra par groupe
res_kmeans$withinss
#Variance inter
res_kmeans$betweenss

# Visualisation sur les 2 premiers axes
ggplot(individu_clust, aes(dim1, dim2, col = factor(cluster))) + 
  geom_point(size = 2, alpha = 0.8, position = "jitter")
# Visualisation sur l'axe 1 et 3 
ggplot(individu_clust, aes(dim1, dim3, col = factor(cluster))) + 
  geom_point(size = 2, alpha = 0.8, position = "jitter")
# Visualisation sur les 3 premiers axes
with(individu_clust, plot3d(dim1,dim2,dim3, col = cluster,size=4))
legend3d("topright", legend = levels(individu_clust$cluster), col = levels(individu_clust$cluster), pch=19)


#Calcul du nombre optimal de groupe
res<-matrix(nrow=14,ncol=2)
for(i in 2:15){
  
  res_kmeans<-kmeans(coord_individu,center=i,iter.max=100,nstart=50)
  res[i-1,2]<-res_kmeans$tot.withinss/res_kmeans$totss
  res[i-1,1]<-i
}
plot(res)


res.pca<-PCA(base7, scale.unit = TRUE, ncp = 5)

plot(res.pca$eig[,2])

res.pca$var$coord

res.pca$var$contrib

plot(res.pca$ind$coord[,1:2])
