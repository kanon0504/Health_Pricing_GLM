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
# remove file exposure
database <- database[-1]

############################ Data Pre-processing ############################ 
name_claim_data <- database[13]
merged_data <- data_preprocessing(name_claim_data, panel_ass = panel_ass)

############################ Data Pre-processing ############################ 

############################ Generate training and testing data ############################ 

returnlist <- random_split(merged_data)
tr <- returnlist$train.set # training dataset
te <- returnlist$test.set # testing dataset
rm(returnlist)

# Create a formula object for fitting a glm model as the baseline
xnames <- names(tr)
to_remove <- c("presence", "somme_quantite", "ident_police", "ident_famille", "IDENT_CONV"
               , "pointeur_origine", "date_sortie", "ident_personne")
xnames <- setdiff(xnames, to_remove)
fmla <- as.formula(paste("tr$somme_quantite ~ ",paste(xnames,collapse = '+')))


glm_model <- glm(fmla, family = poisson(link = log), offset = offset(tr$presence), data = tr)
############################ Generate training and testing data ############################ 


############################ Testing of glm_model model ############################ 

prediction <- predict.glm(glm_model, newdata = te[,xnames], type = "response")

kpi <- kpi_gini(predrisk = prediction, truerisk = te$somme_quantite
                , exposure = te$presence, significance = 6)

############################ Testing of glm_model model ############################ 

