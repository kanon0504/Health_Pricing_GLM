setwd("F:/NORMES TECHNIQUES/PERIMETRE EXPATRIES/12 - Tarification GLM 2016/06-Modèle GLM")
source("./Codes dans R/utils.R")




############################ Load in all database ############################ 
# read processed exposure data
panel_ass <- read.csv("./Base de donnée après traitement/Assurées/panel_ass_clean.csv")

# read in 24 claim data
database <- list.files(path = "./Base de donnée après traitement/Sinistres/")
for (name_claim_data in database)
{
  name <- strsplit(name_claim_data,split = ".",fix =T)[[1]][1]
  print(paste0("reading ", name))
  data <- read.csv(paste0("./Base de donnée après traitement/Sinistres/"
                                    , name_claim_data))
  data$X <- NULL
  assign(name, data)
}
rm(database)
rm(name)
rm(name_claim_data)
############################ Load in all database ############################ 


############################ Data Pre-processing ############################ 
merged_data <- data_preprocessing("divers" , panel_ass = panel_ass)
############################ Data Pre-processing ############################ 


############################ Group levels ############################ 
merged_data <- group_pays_freq(merged_data, name, print.csv = T)
merged_data <- group_pays_cout(merged_data, name, print.csv = T)
merged_data <- group_age_freq(merged_data, name, print.csv = F)
merged_data <- group_age_cout(merged_data, name, print.csv = F)
merged_data <- group_nationalite_freq(merged_data, name, print.csv = T)
merged_data <- group_nationalite_cout(merged_data, name, print.csv = T)
############################ Group levels ############################ 


############################ Generate training and testing data ############################ 
returnlist <- random_split(merged_data)
tr <- returnlist$train.set # training dataset
te <- returnlist$test.set # testing dataset
rm(returnlist)

# Create a formula object for fitting a glm model as the baseline
xnames <- names(tr)
to_remove <- c("presence", "somme_quantite", "ident_police", "ident_famille", "IDENT_CONV"
               , "pointeur_origine", "date_sortie", "ident_personne", "somme_frais", "categorie")
#xnames <- setdiff(xnames, to_remove)
xnames <- c("type_assure", "sexe","group_age_freq")

fmla_freq <- as.formula(paste("tr$somme_quantite ~ ",paste(xnames,collapse = '+')))


glm_freq <- glm(fmla_freq, offset(tr$presence), family = poisson(), data = tr)
############################ Generate training and testing data ############################ 


############################ Testing of glm_model model ############################ 

prediction <- predict(object = glm_model, newdata = te[,xnames], type = "response")
prediction <- exp(prediction) - 1
  
kpi <- kpi_gini(predrisk = prediction, truerisk = te$somme_quantite
                , exposure = te$presence, significance = 6)
print(kpi)
############################ Testing of glm_model model ############################ 

pays_expat <- levels(panel_ass$pays_expat)
pays_dist <- panel_ass[,.(nb_assure = .N), by = pays_expat]

xnames <- names(panel_ass)
to_remove <- c("X","presence",  "ident_police", "ident_famille", "ident_personne", "date_naissance",
               "IDENT_CONV", "date_entree", "date_sortie", "nationalite", "nationalite_2", "pays_expat",
               "pays_expat_2","pays", "pays_2", "pointeur_origine", "date_sortie_obs", "temps_de_presence",
               "temps_de_presence_2","age","presence", "categorie")
xnames <- setdiff(xnames, to_remove)
xnames <- c("type_assure","sexe","taille_famille","nb_conjoints","nb_enfants","annee")
xnames_freq <- c(xnames, "group_pays_freq","group_age_freq","group_nationalite_freq")
xnames_cout <- c(xnames, "group_pays_cout","group_age_cout","group_nationalite_cout")
fmla_freq <- as.formula(paste("somme_quantite ~ ",paste(xnames_freq,collapse = '+')))
fmla_cout <- as.formula(paste("cout_moyen ~ ",paste(xnames_cout,collapse = '+')))

models_list <- list()
for (name_claim in database[c(c(1:5),c(8:24))])
{
  name_claim <- strsplit(name_claim, split = ".", fixed = T)[[1]][1]
  print(name_claim)
  merged_data <- data_preprocessing(name_claim , panel_ass = panel_ass, verbose = FALSE)
  merged_data <- data.table::data.table(merged_data)
  merged_data <- group_pays_freq(merged_data, name_claim, print.csv = F)
  merged_data <- group_pays_cout(merged_data, name_claim, print.csv = F)
  merged_data <- group_age_freq(merged_data, name_claim, print.csv = F)
  merged_data <- group_age_cout(merged_data, name_claim, print.csv = F)
  merged_data <- group_nationalite_freq(merged_data, name_claim, print.csv = F)
  merged_data <- group_nationalite_cout(merged_data, name_claim, print.csv = F)
  train_freq <- merged_data[annee < 2012,]
  test_freq <- merged_data[annee == 2012,]
  
  merged_cout <- merged_data[cout_moyen>0,]
  varchars = names(merged_cout)[which(sapply(merged_cout,is.factor))]
  for (j in varchars) 
  { data.table::set(merged_cout,j=j,value = as.factor(merged_cout[[j]]))}
  train_cout <- merged_cout[annee < 2012,]
  test_cout <- merged_cout[annee == 2012,]
  
 
  glm_freq <- glm(fmla_freq, offset(train_freq$presence), family = poisson(link = "log"), data = train_freq)
  glm_cout <- glm(fmla_cout, offset(train_cout$presence),family = Gamma(link = "log"), data = train_cout)
  
  pred_freq <- predict.glm(object = glm_freq, newdata = test_freq, type = "response")
  pred_cout <- predict.glm(object = glm_cout, newdata = test_cout, type = "response")
  
  real_freq <- test_freq$somme_quantite
  real_cout <- test_cout$cout_moyen
  presence <- test_freq$presence
  
  element <- list(pred_freq = pred_freq, pred_cout = pred_cout,
                  real_freq = real_freq, real_cout = real_cout, presence <- presence)
  
  models_list[[name_claim]] <- element
  # s_freq <- summary.glm(glm_freq)$coefficients
  # s_cout <- summary.glm(glm_cout)$coefficients
  # 
  # write.csv(s_freq, file = paste0("./Models/freq/freq_",name_claim))
  # write.csv(s_cout, file = paste0("./Models/cout/cout_",name_claim))
  #merged_data <- data.table::data.table(merged_data)
  # temp <- merged_data[,.(nb_sinistre = sum(somme_quantite)), by = pays_expat]
  # names(temp)[2] <- name_claim
  # pays_dist <- merge(pays_dist, temp, by = "pays_expat", x.all = T)
  # setwd("/Users/Kanon/Documents/Health_Pricing_GLM/saved_plots//")
  # plot_claim(name_claim, panel_ass)
  # setwd("/Users/Kanon/Documents/Health_Pricing_GLM/saved_data/")
  # save_data(name_claim, panel_ass)
}

name_list <- names(models_list)
for (i in 1:length(models_list))
{
  name <- name_list[i]
  png(filename = paste0("./plots/gain_freq/",name,"_gain_freq.png"), width = 6, height = 3.25,
      units = "in",res = 400, pointsize = 2)
  plotgain(predrisk = models_list[[i]]$pred_freq, truerisk = models_list[[i]]$real_freq, 
           exposure = models_list[[i]]$presence, significance = 5)
  dev.off()
  png(filename = paste0("./plots/gain_cout/",name,"_gain_cout.png"), width = 6, height = 3.25,
      units = "in",res = 400, pointsize = 2)
  plotgain(predrisk = models_list[[i]]$pred_cout, truerisk = models_list[[i]]$real_cout, 
           exposure = NULL, significance = 5)
  dev.off()
}




Xautres_protheses <- c("autres_prothese","prothese_audio")
Xauxiliaires_medicaux <- c("auxi_medical","kine")
Xbilan_de_sante <- c("bilan","med_prevent")
Xchambre_particuliere <- "chambre"
Xconsultation <- c("specialiste","generaliste")
Xcures_thermales <- "cures"
Xdivers <- "divers"
Xfiv <- "fertilite"
Xhospitalisation_sauf_chambre_particuliere <- c("hospi","hospi_jour","chimio","lit_accompagnant","transport")
Ximplants_dentaires <- "implants"
Xkeratotomie <- "keratomie"
Xlentilles <- "lentilles"
Xmaternite_sauf_fiv <- c("cesarienne","maternite")
Xmedecine_alternative <- c("acuponcture", "chiropractie","med_alter","osteopathie")
Xmontures <- c("monture", "optique")
Xorthodontie <- "orthodontie"
Xparadontologie <- "paradontologie"
Xpetit_risque <- c("petit_risque", "radiotherapie")
Xpharmacie <- "pharmacie"
Xprothese_dentaire <- "prothese_dentaire"
Xpsychiatrie <- "psychiatrie"
Xsoins_dentaires <- "dentaire_gen"
Xvaccination <- "vaccins"
Xverres <- "verres"
