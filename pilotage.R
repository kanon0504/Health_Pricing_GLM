setwd("/Users/Kanon/Documents/Health_Pricing_GLM")
source('utils.R')


for(name_claim_data in database)
{
  name <- name_claim_data
  #claim_data <- sas7bdat::read.sas7bdat(paste0("/Users/Kanon/Google Drive/AXA/data/MSH/"
  #                                                      , name_claim_data))
  #claim_data$pk <- as.numeric(paste(claim_data$annee,claim_data$ident_personne,sep=""))
  #check_dup(claim_data, name, verbose = verbose)
  print(name)
  print(dim(eval(parse(text = name)))[1])
  eliminate_negative(eval(parse(text = name)))
  rm(name)
}

features <- c("autres_prothese", "protheses_auditives_pharmacie", "auxiliaire_medical", "kinesitherapie"
              , "bilan_de_sante", "cures_thermales", "dentaire_general", "implants", "orthodontie"
              , "protheses_dentaires", "parodontologie", "divers", "generaliste", "specialiste"
              , "hospitalisation", "chambre_particuliere", "hospitalisation_de_jour", "lit_accompagnant"
              , "maternite_generale", "cesarienne", "chiropractie", "osteopathie", "acuponcture"
              , "autres_medecines_alternatives", "medecine_preventive", "radiotherapie", "chimiotherapie"
              , "keratomie", "lentilles", "montures", "verres", "optique", "petit_risque", "pharmacie"
              , "psychiatrie", "vaccins", "traitement_de_la_fertilite", "ambulance_transport")

database <- list.files(path = '/Users/Kanon/Google Drive/AXA/data/MSH/')

setwd("/Users/Kanon/Google Drive/AXA/data/Merged_data")
database <- list.files(path = '/Users/Kanon/Google Drive/AXA/data/Merged_data')


############################ Data Pre-processing ############################ 
panel_ass <- sas7bdat::read.sas7bdat("/Users/Kanon/Google Drive/AXA/data/MSH/exposure/panel_ass.sas7bdat")
for (name_claim_data in database)
{
  name <- get_name(name_claim_data)
  print(paste0("reading ", name))
  data <- sas7bdat::read.sas7bdat(paste0("/Users/Kanon/Google Drive/AXA/data/MSH/Sinistre/"
                                    , name_claim_data))
  assign(name, data)
}

name_claim_data <- database[13]
merged_data <- data_preprocessing("consultation" , panel_ass = panel_ass)
merged_data <- data.table::data.table(merged_data)
merged_data$annee <- as.factor(merged_data$annee)
############################ Data Pre-processing ############################ 

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
xnames <- c("type_assure", "sexe","annee","categorie")
fmla <- as.formula(paste("tr$somme_quantite ~ ",paste(xnames,collapse = '+')))


glm_model <- glm(fmla, offset(tr$presence), family = poisson(link = log), data = tr)
############################ Generate training and testing data ############################ 


############################ Testing of glm_model model ############################ 

prediction <- predict(object = glm_model, newdata = te[,xnames], type = "response")
prediction <- exp(prediction) - 1
  
kpi <- kpi_gini(predrisk = prediction, truerisk = te$somme_quantite
                , exposure = te$presence, significance = 6)
print(kpi)
############################ Testing of glm_model model ############################ 


mean_var <- data.frame(poste = c(1), mean = c(1), var = c(1), logmean = c(1), logvar = c(1))

for (name_claim in database)
{
  merged_data <- data_preprocessing(name_claim , panel_ass = panel_ass, verbose = FALSE)
  temp <- check_dist(merged_data)
  mean_var <- rbind(mean_var, c(strsplit(name_claim, split = ".", fixed = T)[[1]][1], temp))
  # setwd("/Users/Kanon/Documents/Health_Pricing_GLM/saved_plots//")
  # plot_claim(name_claim, panel_ass)
  # setwd("/Users/Kanon/Documents/Health_Pricing_GLM/saved_data/")
  # save_data(name_claim, panel_ass)
}





merge_data(Xautres_protheses, "Xautres_protheses")
merge_data(Xauxiliaires_medicaux,"Xauxiliaires_medicaux")
merge_data(Xbilan_de_sante,"Xbilan_de_sante")
merge_data(Xchambre_particuliere,"Xchambre_particuliere")
merge_data(Xconsultation,"Xconsultation")
merge_data(Xcures_thermales,"Xcures_thermales")
merge_data(Xdivers,"Xdivers")
merge_data(Xfiv,"Xfiv")
merge_data(Xhospitalisation_sauf_chambre_particuliere,"Xhospitalisation_sauf_chambre_particuliere")
merge_data(Ximplants_dentaires,"Ximplants_dentaires")
merge_data(Xkeratotomie,"Xkeratotomie")
merge_data(Xlentilles,"Xlentilles")
merge_data(Xmaternite_sauf_fiv,"Xmaternite_sauf_fiv")
merge_data(Xmedecine_alternative,"Xmedecine_alternative")
merge_data(Xmontures,"Xmontures")
merge_data(Xorthodontie,"Xorthodontie")
merge_data(Xparadontologie,"Xparadontologie")
merge_data(Xpetit_risque,"Xpetit_risque")
merge_data(Xpharmacie,"Xpharmacie")
merge_data(Xprothese_dentaire,"Xprothese_dentaire")
merge_data(Xpsychiatrie,"Xpsychiatrie")
merge_data(Xsoins_dentaires,"Xsoins_dentaires")
merge_data(Xvaccination,"Xvaccination")
merge_data(Xverres,"Xverres")



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
