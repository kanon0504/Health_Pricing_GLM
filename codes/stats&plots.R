library(ggplot2)

panel_ass <- data.table::data.table(panel_ass)
# l'age moyen (assurés principaux, bénéficiaire d'adult, bénéficiaire d'enfant)
data <- panel_ass[,.(age_mean = mean(age)
                     , nb_enfants = mean(nb_enfants)),
                  by = c("sexe","type_assure","annee")]

levels(data$type_assure) <- c("assuré", 
                              "conjoint", 
                              "enfant", 
                              "parent")
                  
ggplot(data = data[,.(age_mean = mean(age_mean)), 
                   by = c("type_assure","annee")], 
       aes(x = as.factor(annee) ,
           y = age_mean,
           fill = type_assure)) + 
  geom_bar(position="dodge",
           stat="identity") + 
  xlab("Année") + 
  ylab("Age moyen")


# nombre moyen d'enfant par assuré principal
ggplot(data = data[type_assure == "assuré",], 
       aes(x = as.factor(annee) , 
           y = nb_enfants)) + 
  geom_bar(position="dodge",
           stat="identity") + 
  xlab("Année") + 
  ylab("Nombre d'enfants(de principaux)")


# age moyen du groupe ( femme, homme, assuré, conjoint, parent et enfant)
data$fill <- paste(data$type_assure, data$sexe)

ggplot(data = data[type_assure != "enfant" & type_assure != "parent",], 
       aes(x = as.factor(annee) , 
           y = age_mean,
           fill = fill)) + 
  geom_bar(position="dodge",
           stat="identity") + 
  xlab("Année") + 
  ylab("Age moyen")


# creating the dataset which contains clients and claims
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


# constructing data_cout, data_freq and data_frais
data_frais <- data_all[,c(1:4,seq(6,52,2))]

data_freq <- data_all[,c(1:4,seq(5,51,2))]

presence <- data_all$presence

data_freq[,5:28] <- lapply(data_freq[,5:28],
                           f<-function(x){return(x/presence)})

data_cout <- data_frais

data_cout[,5:28] <- lapply(data_cout[,5:28],
                           f<-function(x){return(x/data_cout$presence)})

data_cout$presence <- NULL


# free the memory of data_all
rm(data_all)


# plot of frais reel par age
data_frais <- transform(data_frais, sum = rowSums(data_frais[,-(1:4)]))

ggplot(data = data_frais[,.(sum = sum(sum)),
                         by = age], 
       aes(x = age, 
           y = sum)) + 
  geom_col() +
  xlab("Age") + 
  ylab("Somme de frais réel")

ggplot(data = data_frais[,.(sum = mean(sum)), 
                         by = age], 
       aes(x = age,
           y = sum)) + 
  geom_col() + 
  xlab("Age") + 
  ylab("Moyen de frais réel")


# plot of frequence par age
data_freq <- transform(data_freq, sum = rowSums(data_freq[,-(1:4)]))

ggplot(data = data_freq[,.(sum = mean(sum)), 
                        by = age], 
       aes(x = age,
           y = sum)) + 
  geom_col() +
  xlab("Age") +
  ylab("Moyen de fréquence")

ggplot(data = data_freq[,.(sum = sum(sum)), 
                        by = age], 
       aes(x = age,
           y = sum)) + 
  geom_col() + 
  xlab("Age") + 
  ylab("Somme de fréquence")


# plot of cout moyen par age
data_cout <- transform(data_cout,
                       sum = rowSums(data_cout[,-(1:4)]))

ggplot(data = data_cout[,.(sum = mean(sum)),
                        by = age],
       aes(x = age,
           y = sum)) + 
  geom_col() + 
  xlab("Age") + 
  ylab("cout par tete en considerant l'exposition")

ggplot(data = data_cout[,.(sum = sum(sum)), 
                        by = age], 
       aes(x = age, 
           y = sum)) + 
  geom_col() +
  xlab("Age") +
  ylab("Somme de cout moyen")


# repartition de pays
library(countrycode)


# par le nombre de person dans pays de destination
data_frais$head_count <- 1
data_frais$iso <- 
  countrycode::countrycode(data_frais$pays_expat, 
                           origin = "country.name",
                           destination = "iso3c")

map <- rworldmap::joinCountryData2Map(data_frais[,.(count = sum(head_count),
                                                     sum = sum(sum),
                                                     mean = mean(sum)), 
                                                  by = iso]
                                       ,joinCode = "ISO3"
                                       ,nameJoinColumn = "iso") 

rworldmap::mapCountryData(map, 
                          nameColumnToPlot = "mean",
                          mapTitle = "Moyen de frais réel",
                          catMethod = "quantiles",
                          colourPalette = "heat")


# Somme de frais reel par beneficiaire par acte
data_frais_reshape <- melt(data_frais, 
                           id.vars = c("annee",
                                       "pays_expat",
                                       "age",
                                       "presence",
                                       "sum",
                                       "head_count",
                                       "iso"))

data_frais_reshape$variable <- 
  as.character(data_frais_reshape$variable)

data_frais_reshape$variable <- 
  substr(data_frais_reshape$variable,
         1,
         nchar(data_frais_reshape$variable) - 5)

data_frais_reshape <- 
  data.table::data.table(data_frais_reshape)

ggplot(data_frais_reshape[,.(sum = sum(value)),
                          by = variable]
       , aes(x = reorder(variable,sum),
             y = sum)) +
  geom_col() +
  xlab("Acte") + 
  ylab("Somme de frais réel") + 
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1)) +
  coord_flip()


# Moyen de frais reel par beneficiaire par acte
ggplot(data_frais_reshape[,.(sum = mean(value[value != 0])),
                          by = variable]
       , aes(x = reorder(variable,
                         sum), 
             y = sum)) + 
  geom_col() +
  xlab("Acte") + 
  ylab("Moyen de frais réel par tete") + 
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1)) + 
  coord_flip()

rm(data_frais_reshape)


# somme frais reel d'hospitalisation, petit risque
# montures, consultation, soins dentaires par annee
ggplot(data_frais_reshape[variable %in% c("hospitalisation_sauf_chambre_particuliere",
                                          "petit_risque", 
                                          "montures", 
                                          "consultation", 
                                          "soins_dentaires"),
                          .(sum = sum(value)),
                  by = c("annee"
                         ,"variable")],
       aes(x = as.factor(annee),
           y = sum,
           fill = variable)) +
  geom_col() + 
  xlab("Année") + 
  ylab("Somme de frais réel de 5 acte qui sont plus lourd dans consommation")


# moyen de frais reel d'implant dentaire, keratotomie
# fiv, protheses dentaires, maternite par annee
ggplot(data_frais_reshape[variable %in% c("implants_dentaires",
                                          "keratotomie",
                                          "fiv", 
                                          "prothese_dentaire", 
                                          "maternite_sauf_fiv"),
                          .(sum = mean(value[value != 0])),
                          by = c("annee",
                                 "variable")], 
       aes(x = as.factor(annee),
           y = sum,
           fill = variable)) +
  geom_col() + 
  xlab("Année") +
  ylab("Moyen de frais réel de les 5 actes qui coutent le plus cher")


# tendence de frequence par l'age pour maternite
ggplot(data_freq[,.(mean = mean(consultation_freq)), 
                 by = age],
       aes(x = age,
           y = mean)) +
  geom_col() + 
  xlab("Age") + 
  ylab("Fréquence pour maternité")


# plot l'evolution du pourcentage de nombre pour type assure

panel_ass <- data.table::data.table(panel_ass)

df <- panel_ass[,
                .N,
                by = c("annee","type_assure")]

df$type_assure = factor(df$type_assure, levels = c("A","C","E","P"))

df <- arrange(df, annee, type_assure)

levels(df$type_assure) <- c("Assuré principal","Conjoint","Enfant","Parent")

df <- ddply(df, .(annee), transform, percent = N/sum(N) * 100)

df <- ddply(df, .(annee), transform, pos = (cumsum(N) - 0.5 * N))

df$label = paste0(sprintf("%.0f", df$percent), "%")

ggplot(df,
       aes(x = factor(annee),
           y = N, 
           fill = type_assure)) +
  geom_bar(stat = "identity", width = .7) +
  scale_y_continuous(labels=prettyNum) +
  coord_flip()+
  geom_text(aes(label = label), 
            size = 4,
            position = position_stack(vjust = 0.5)) +
  xlab("Année") +
  ylab("Nombre de bénéficiaires") + 
  labs(fill = "Type d'assuré") +
  ggtitle("L'évolution de type d'assuré(par nombre et pourcentage)")


# plot l'evolution du pourcentage de nombre pour sexe

panel_ass <- data.table::data.table(panel_ass)

df <- panel_ass[,
                .N,
                by = c("annee","type_assure","sexe")]

df[df$type_assure == "E",]$sexe <- "E"

df <- df[,
         .(N = sum(N)),
         by = c("annee","sexe")]

df$sexe = factor(df$sexe, levels = c("F","M","E"))

df <- arrange(df, annee, sexe)

levels(df$sexe) <- c("Femme","Homme","Enfant")

df <- ddply(df, .(annee), transform, percent = N/sum(N) * 100)

df <- ddply(df, .(annee), transform, pos = (cumsum(N) - 0.5 * N))

df$label = paste0(sprintf("%.0f", df$percent), "%")

ggplot(df,
       aes(x = factor(annee),
           y = N, 
           fill = sexe)) +
  geom_bar(stat = "identity", width = .7) +
  scale_y_continuous(labels=prettyNum) +
  coord_flip()+
  geom_text(aes(label = label), 
            size = 4,
            position = position_stack(vjust = 0.5)) +
  xlab("Année") +
  ylab("Nombre de bénéficiaires") + 
  labs(fill = "Sexe") +
  ggtitle("L'évolution par sexe(par nombre et pourcentage)")

# plot pour chacun type assure
ggplot(df[df$type_assure == "Parent",],
       aes(x = factor(annee),
           y = N)) +
  geom_bar(stat = "identity", width = .7, fill="mediumorchid1") +
  xlab("Année") +
  ylab("Nombre de parents") + 
  ggtitle("L'évolution de type d'assuré parent")

ggplot(df[df$type_assure == "Assuré principal",],
       aes(x = factor(annee),
           y = N)) +
  geom_bar(stat = "identity", width = .7, fill="coral1") +
  xlab("Année") +
  ylab("Nombre d'assuré principal") + 
  ggtitle("L'évolution de type d'assuré principal")

ggplot(df[df$type_assure == "Conjoint",],
       aes(x = factor(annee),
           y = N)) +
  geom_bar(stat = "identity", width = .7, fill="olivedrab") +
  xlab("Année") +
  ylab("Nombre de conjoints") + 
  ggtitle("L'évolution de type d'assuré conjoint")

ggplot(df[df$type_assure == "Enfant",],
       aes(x = factor(annee),
           y = N)) +
  geom_bar(stat = "identity", width = .7, fill="darkturquoise") +
  xlab("Année") +
  ylab("Nombre de enfants") + 
  ggtitle("L'évolution de type d'assuré enfant")

# evolution de frais reel
data_cout <- data_all[,c(1:2,seq(4,50,2))]

data_cout$pays_expat <- NULL

data_cout <- transform(data_cout, sum = rowSums(data_cout[,-1]))

ggplot(data_cout[,
                 .(sum = sum(sum)),
                 by = annee],
       aes(x = factor(annee),
           y = sum)) + 
  geom_bar(stat = "identity", width = .7, fill="grey50") +
  xlab("Année") +
  ylab("Somme de frais réel") + 
  ggtitle("L'évolution de somme de frais réel")


# pourcentage de homme et femme pour les assurees principaux
df <- panel_ass[type_assure == "A",
                .N,
                by = c("annee","type_assure","sexe")]

df$sexe = factor(df$sexe, levels = c("F","M"))

df <- arrange(df, annee, sexe)

levels(df$sexe) <- c("Femme","Homme")

df <- ddply(df, .(annee), transform, percent = N/sum(N) * 100)

df <- ddply(df, .(annee), transform, pos = (cumsum(N) - 0.5 * N))

df$label = paste0(sprintf("%.0f", df$percent), "%")

ggplot(df,
       aes(x = factor(annee),
           y = N, 
           fill = sexe)) +
  geom_bar(stat = "identity", width = .7) +
  scale_y_continuous(labels=prettyNum) +
  coord_flip()+
  geom_text(aes(label = label), 
            size = 4,
            position = position_stack(vjust = 0.5)) +
  xlab("Année") +
  ylab("Nombre de bénéficiaires principaux") + 
  labs(fill = "Sexe") +
  ggtitle("L'évolution de pourcentage par sexe pour les assurés principaux")


# pourcentage de homme et femme pour les assurees conjoints
panel_ass[panel_ass$nb_conjoints == 2,]$nb_conjoints <- 1

df <- panel_ass[type_assure == "A",
                .N,
                by = c("annee","nb_conjoints")]

df$status <- factor(df$nb_conjoints)

levels(df$status) <- c("CVD","Marié(e)")

df <- arrange(df, annee, status)

df <- ddply(df, .(annee), transform, percent = N/sum(N) * 100)

df <- ddply(df, .(annee), transform, pos = (cumsum(N) - 0.5 * N))

df$label = paste0(sprintf("%.0f", df$percent), "%")

ggplot(df,
       aes(x = factor(annee),
           y = N, 
           fill = status)) +
  geom_bar(stat = "identity", width = .7) +
  scale_y_continuous(labels=prettyNum) +
  coord_flip()+
  geom_text(aes(label = label), 
            size = 4,
            position = position_stack(vjust = 0.5)) +
  xlab("Année") +
  ylab("Nombre de bénéficiaires") + 
  labs(fill = "Status") +
  ggtitle("L'évolution de status mariage pour les assurés principaux")


# top ten pays avec les frais reels les plus hauts
data_frais <- transform(data_frais, sum = rowSums(data_frais[,-c(1,26)]))

data_frais <- data_frais[order(sum, decreasing = T),]

# count <- data_frais[, -"annee",
#                     with = F][, .N,
#                                 by = pays_expat]$N
# 
# data_frais$count <- count
# 
# rm(count)
# 
# data_frais$pays_expat <-
#   paste(data_frais$pays_expat,
#         "(",
#         data_frais$count,
#         ")",
#         sep = "")

ggplot(data_frais[1:30,],
       aes(x = reorder(pays_expat,-sum),
           y = sum)) +
  geom_bar(stat = "identity", width = .7) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Pays") +
  ylab("Frais réel moyen") + 
  ggtitle("Les top trente pays avec le frais réel le plus élevé")


# cout moyen par forfait
data_cout$maternite <- 
  rowSums(data_cout[,c("maternite_sauf_fiv_cout",
                       "fiv_cout"),
                    with = F])

data_cout$maternite_sauf_fiv_cout <- NULL

data_cout$fiv_cout <- NULL

data_cout$dentaire <- 
  rowSums(data_cout[,c("soins_dentaires_cout",
                       "orthodontie_cout",
                       "prothese_dentaire_cout",
                       "implants_dentaires_cout"),
                    with = F])

data_cout$soins_dentaires_cout <- NULL

data_cout$orthodontie_cout <- NULL

data_cout$prothese_dentaire_cout <- NULL

data_cout$implants_dentaires_cout <- NULL

data_cout$optique <- 
  rowSums(data_cout[,c("verres_cout",
                       "lentilles_cout",
                       "montures_cout",
                       "keratotomie_cout"),
                    with = F])

data_cout$verres_cout <- NULL

data_cout$lentilles_cout <- NULL

data_cout$montures_cout <- NULL

data_cout$keratotomie_cout <- NULL

data_cout$hospi <- 
  rowSums(data_cout[,c("hospitalisation_sauf_chambre_particuliere_cout",
                       "chambre_particuliere_cout"),
                    with = F])

data_cout$hospitalisation_sauf_chambre_particuliere_cout <- NULL

data_cout$chambre_particuliere_cout <- NULL

data_cout <- data_cout[,lapply(.SD,mean), b= c("annee","pays_expat")]

