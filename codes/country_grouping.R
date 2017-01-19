# Code for country grouping
setwd("F:/NORMES TECHNIQUES/PERIMETRE EXPATRIES/12 - Tarification GLM 2016/06-Modèle GLM")
source("./Codes dans R/utils.R")
library(ggplot2)

######################################## preparation of distance matrix ######################################## 
# creating the data_all which contains clients and claims
data_all <- panel_ass

data_all <- data.table::data.table(data_all)

data_all <- data_all[,.(annee,ident_personne,pays_expat,presence,age)]

for (claim in database)
{
  name_claim <- strsplit(claim, split = ".", fixed = T)[[1]][1]
  
  data_all <- data_preprocessing(claim, panel_ass = data_all)
  
  names(data_all)[names(data_all) == 'somme_quantite'] <-
    paste0(name_claim, "_freq")
  
  names(data_all)[names(data_all) == 'cout_moyen'] <- 
    paste0(name_claim, "_cout")
}
data_all$ident_personne <- NULL


# write data_all
write.csv(data_all, file = "Base de donnée après traitement/data_all.csv", row.names = FALSE)


# constructing data_cout, data_freq and data_frais for the repartition of country
presence <- data_all$presence

data_all$presence <- NULL

data_all$age <- NULL

data_freq <- data_all[,c(1:2,seq(3,49,2))]

data_cout <- data_all[,c(1:2,seq(4,50,2))]

data_frais <- data_cout

for (i in 3:26)
{
  data_frais[[i]] <- data_frais[[i]] * data_freq[[i]]
}

data_freq[,3:26] <- lapply(data_freq[,3:26],
                           f<-function(x){return(x/presence)})

# frequence moyen par pays par acte(en considerant l'exposition)
data_freq <- data_freq[, -"annee",
                       with = F][, lapply(.SD,mean),
                                 by = pays_expat] 

# cout moyen de consultation (sur les consommeur) par pays par acte
data_cout <- data_cout[, -"annee",
                       with = F][, lapply(.SD,
                                          f <- function(x){return(mean(x[x!=0]))}),
                                 by = pays_expat]

data_cout[is.na(data_cout)] <- 0

# prime pure estime (frais reel par tete)
data_frais <- data_frais[, -"annee",
                       with = F][, lapply(.SD,mean),
                                 by = pays_expat]

database <- list.files(path = "./Base de donnée après traitement/Sinistres/")

col_name <-
  as.character(sapply(database,
                      f <- function(x){return(strsplit(x,
                                                       split = ".", 
                                                       fixed = T)[[1]][1])}))

names(data_frais) <- c("pays_expat",paste(col_name, "_frais", sep = ""))

data_frais[is.na(data_frais)] <- 0


# constructing data_cout_i (after considering medical inflation)
infla <- read.csv("Complement_donnees/Inflation.csv", 
                  header = T,
                  check.names = FALSE)

data_cout_i <- 
  data.frame(data_all[,c(1:2,seq(4,50,2))][,lapply(.SD,
                              f <- function(x){return(mean(x[x!=0]))}),
                      by = c("pays_expat","annee")])

data_cout_i[is.na(data_cout_i)] <- 0

data_cout_i$iso <- 
  countrycode::countrycode(data_cout_i$pays_expat, 
                           origin = "country.name", 
                           destination = "iso3c")

pays_expat_i <- data_cout_i$pays_expat

data_cout_i$pays_expat <- NULL

for (i in 2007:2012)
{
  for (j in na.omit(unique(data_cout_i$iso)))
  {
    if (j %in% infla$iso)
    {
      k <- i
      while (k != 2006)
      {
          data_cout_i[data_cout_i$annee == i & data_cout_i$iso == j,][,-c(1,26)] <-
            data_cout_i[data_cout_i$annee == i & data_cout_i$iso == j,
                        ][,-c(1,26)]/infla[infla$iso==j,][[paste0("X",k)]]
        k = k - 1
      }
    }
  }
}

data_cout_i$annee <- NULL

data_cout_i$pays_expat <- pays_expat_i

data_cout_i$iso <- NULL

data_cout_i <- data.table::data.table(data_cout_i)

data_cout_i <- data_cout_i[,lapply(.SD,mean),by = pays_expat]


# constructing data_frais_i (after considering medical inflation)
data_frais_i <- data_all[,c(1:2,seq(3,49,2))][, -"annee",
                                              with = F][, lapply(.SD,sum),
                                                        by = pays_expat] 


for (i in 1:length(col_name))
{
  data_frais_i[[i+1]] <- data_frais_i[[i+1]]*data_cout_i[[i+1]]
}

names(data_frais_i) <- c("pays_expat",paste(col_name, "_frais", sep = ""))


# proporstion de garanti par pays
garanti <- c("Xautres_protheses", 
             "Xauxiliaires_medicaux",
             "Xbilan_de_sante",
             "Xchambre_particuliere",
             "Xconsultation", 
             "Xcures_thermales", 
             "Xdivers", 
             "Xfiv", 
             "Xhospitalisation_sauf_chambre_particuliere",
             "Ximplants_dentaires", 
             "Xkeratotomie", 
             "Xlentilles", 
             "Xmaternite_sauf_fiv",
             "Xmedecine_alternative",
             "Xmontures",
             "Xorthodontie",
             "Xparadontologie",
             "Xpetit_risque",
             "Xpharmacie", 
             "Xprothese_dentaire",
             "Xpsychiatrie", 
             "Xsoins_dentaires",
             "Xvaccination", 
             "Xverres")

panel_ass <- data.table::data.table(panel_ass)

garanti_prop <- 
  panel_ass[,c(garanti,"pays_expat"),
            with = F][,lapply(.SD,
                              f <- function(x){sum(x)/.N}),
                      by = pays_expat]


# Nombre total du sinistre et d'exposition
sum_sini <- data_all[,c(2,seq(3,49,2))][,lapply(.SD,sum), by = pays_expat]

data_all$presence <- presence

sum_expo <- data_all[,.(somme_expo = sum(presence)), by = pays_expat]

data_all$presence <- NULL

# write data_cout, data_frais, data_freq, data_cout_i
write.csv(data_cout, 
          file = "Base de donnée après traitement/grouping/data_cout.csv", 
          row.names = FALSE)

write.csv(data_freq,
          file = "Base de donnée après traitement/grouping/data_freq.csv", 
          row.names = FALSE)

write.csv(data_frais, 
          file = "Base de donnée après traitement/grouping/data_frais.csv",
          row.names = FALSE)

write.csv(data_cout_i,
          file = "Base de donnée après traitement/grouping/data_cout_i.csv",
          row.names = FALSE)

write.csv(garanti_prop,
          file = "Base de donnée après traitement/grouping/garanti_prop.csv",
          row.names = FALSE)

write.csv(sum_sini,
          file = "Base de donnée après traitement/grouping/sum_sini.csv", 
          row.names = FALSE)

write.csv(sum_expo, 
          file = "Base de donnée après traitement/grouping/sum_expo.csv",
          row.names = FALSE)
######################################## preparation of distance matrix ######################################## 


######################################## load in distance matrix ######################################## 
setwd("F:/NORMES TECHNIQUES/PERIMETRE EXPATRIES/12 - Tarification GLM 2016/06-Modèle GLM")

data_cout <- read.csv("Base de donnée après traitement/grouping/data_cout.csv")

data_freq <- read.csv("Base de donnée après traitement/grouping/data_freq.csv")

data_frais <- read.csv("Base de donnée après traitement/grouping/data_frais.csv")

data_cout_i <- read.csv("Base de donnée après traitement/grouping/data_cout_i.csv")

garanti_prop <- read.csv("Base de donnée après traitement/grouping/garanti_prop.csv")
  
sum_sini <- read.csv("Base de donnée après traitement/grouping/sum_sini.csv")
  
sum_expo <- read.csv("Base de donnée après traitement/grouping/sum_expo.csv")


# convert all distance matrix into data.table object
data_freq <- data.table::data.table(data_freq)

data_cout <- data.table::data.table(data_cout)

data_frais <- data.table::data.table(data_frais)

data_cout_i <- data.table::data.table(data_cout_i)

garanti_prop <- data.table::data.table(garanti_prop)

sum_sini <- data.table::data.table(sum_sini)
######################################## load in distance matrix ######################################## 


######################################## building kmeans models ######################################## 
# building kmeans models
# cluster by using frequency distance matrix
set.seed(54)

pays_expat <- data_freq$pays_expat

mydata <- data_freq[,-"pays_expat",with = F]

mydata <- mydata[,sapply(.SD,
                         f <- function(x){return((x-mean(x))/sd(x))})]

wss <- c()

for (i in 1:187) 
{wss[i] <- sum(kmeans(mydata,
                        centers=i)$withinss)}

plot(1:187,
     wss,
     type="b",
     xlab="Number of Clusters",
     ylab="Within groups sum of squares")


# cluster by using average cost distance matrix
mydata <- data_cout[,-"pays_expat",with = F]

mydata <- mydata[,sapply(.SD,
                         f <- function(x){return((x-mean(x))/sd(x))})]

wss <- c()

for (i in 1:187) 
{wss[i] <- sum(kmeans(mydata,
                        centers=i)$withinss)}

plot(1:187,
     wss, 
     type="b",
     xlab="Number of Clusters",
     ylab="Within groups sum of squares")

cluster <- kmeans(mydata, centers = 8)

cluster <- data.frame(pays_expat = data_cout$pays_expat, cluster = cluster$cluster)


# cluster by using total cost distance matrix
data_frais_i <- data.table::data.table(data_frais_i)
mydata <- data_frais_i[,-"pays_expat",with = F]
sum_sini <- read.csv("Base de donnée après traitement/grouping/sum_sini.csv")
sum_sini <- data.table::data.table(sum_sini)
sum_sini <- transform(sum_sini, sum = rowSums(sum_sini[,-"pays_expat",with=F]))
pays_expat <- sum_sini$pays_expat
sum_sini$X <- NULL
sum_sini$pays_expat <- NULL
poids <- sum_sini[,-"sum",with= F] 
poids <- poids[,lapply(.SD,f<-function(x){return(x/(sum_sini$sum))})]

mydata <- data.frame(mydata)
poids <- data.frame(poids)

for (i in 1:24)
{mydata[,i] <- mydata[,i]*poids[,i]}
mydata$hospitalisation_sauf_chambre_particuliere_frais <- NULL

#mydata <- mydata[,sapply(.SD, f <- function(x){return(x/sum(x))})]
wss <- c()
for (i in 1:187) wss[i] <- sum(kmeans(mydata,
                                      centers=i)$withinss)
plot(1:187, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
cluster <- kmeans(mydata, centers = 20)
cluster <- data.frame(pays_expat = data_cout$pays_expat, cluster = cluster$cluster)
data_frais_i$cluster <- cluster$cluster
data_frais_i$pays_expat <- NULL
data_frais_av<- data_frais_i[,-"hospitalisation_sauf_chambre_particuliere_frais",
                             with = F][,lapply(.SD,mean),by = cluster]
data_frais_av <- transform(data_frais_av, mean = rowSums(data_frais_av[,-"cluster",with=F])/23)
write.csv(data_frais_av,file = "test_infl.csv", row.names = FALSE)


for (i in 1:20)
{
  print(i)
  print(paste0(cluster[cluster$cluster == i,]$pays_expat,", ",collapse = ""))
}

data_frais_av <- data_frais_av[order(data_frais_av$cluster)]
data_frais_av$cluster <- paste("G",data_frais_av$cluster,sep = "")
ggplot(data = data_frais_av, aes(x = cluster ,y=mean)) + geom_col()
######################################## building kmeans models ######################################## 
