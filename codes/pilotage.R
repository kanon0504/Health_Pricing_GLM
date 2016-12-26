setwd("/Users/Kanon/Documents/Health_Pricing_GLM")
source('utils.R')


features <- c("autres_prothese", "protheses_auditives_pharmacie", "auxiliaire_medical", "kinesitherapie"
              , "bilan_de_sante", "cures_thermales", "dentaire_general", "implants", "orthodontie"
              , "protheses_dentaires", "parodontologie", "divers", "generaliste", "specialiste"
              , "hospitalisation", "chambre_particuliere", "hospitalisation_de_jour", "lit_accompagnant"
              , "maternite_generale", "cesarienne", "chiropractie", "osteopathie", "acuponcture"
              , "autres_medecines_alternatives", "medecine_preventive", "radiotherapie", "chimiotherapie"
              , "keratomie", "lentilles", "montures", "verres", "optique", "petit_risque", "pharmacie"
              , "psychiatrie", "vaccins", "traitement_de_la_fertilite", "ambulance_transport")

database <- list.files(path = '/Users/Kanon/Google Drive/AXA/data/MSH/')
database <- list.files(path = '/Users/Kanon/Google Drive/AXA/data/Merged_data')


############################ Load in all database ############################ 
panel_ass <- sas7bdat::read.sas7bdat("/Users/Kanon/Google Drive/AXA/data/MSH/exposure/panel_ass.sas7bdat")
panel_ass <- binary_to_factor(panel_ass)
levels(panel_ass$pays_expat)[levels(panel_ass$pays_expat)=="ZZ_FRANCE"] <-
  "FRANCE"
levels(panel_ass$pays_expat)[levels(panel_ass$pays_expat)=="DEM. REP. CONGO"] <- 
  "DEMOCRATIC REPUBLIC OF THE CONGO"
levels(panel_ass$pays_expat)[levels(panel_ass$pays_expat)=="BOSNIA HERZEGOVINA"] <- 
  "Bosnia and Herzegovina"
levels(panel_ass$pays_expat)[levels(panel_ass$pays_expat)=="CENTRAL AFRICAN REP"] <- 
  "Central African Republic"
levels(panel_ass$pays_expat)[levels(panel_ass$pays_expat)=="MACEDONIA (FYROM)"] <-
  "MACEDONIA"

for (name_claim_data in database)
{
  name <- strsplit(name_claim_data,split = ".",fix =T)[[1]][1]
  print(paste0("reading ", name))
  data <- read.csv(paste0("/Users/Kanon/Google Drive/AXA/data/Merged_data//"
                                    , name_claim_data))
  assign(name, data)
}
############################ Load in all database ############################ 


############################ Data Pre-processing ############################ 
merged_data <- data_preprocessing("consultation" , panel_ass = panel_ass)
############################ Data Pre-processing ############################ 


############################ Group levels ############################ 
merged_data <- group_pays_freq(merged_data, name, print.csv = T)
merged_data <- group_pays_cout(merged_data, name, cp = 0.0001, print.csv = T)
merged_data <- group_age_freq(merged_data, name, cp = 0.0002, print.csv = T)
merged_data <- group_age_cout(merged_data, name, cp = 0.00003, print.csv = T)
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

pays_expat <- levels(panel_ass$pays_expat)
pays_dist <- panel_ass[,.(nb_assure = .N), by = pays_expat]

for (name_claim in database)
{
  merged_data <- data_preprocessing(name_claim , panel_ass = panel_ass, verbose = FALSE)
  #merged_data <- data.table::data.table(merged_data)
  # temp <- merged_data[,.(nb_sinistre = sum(somme_quantite)), by = pays_expat]
  # names(temp)[2] <- name_claim
  # pays_dist <- merge(pays_dist, temp, by = "pays_expat", x.all = T)
  group_pays_cout(merged_data, name = name_claim)  
  # setwd("/Users/Kanon/Documents/Health_Pricing_GLM/saved_plots//")
  # plot_claim(name_claim, panel_ass)
  # setwd("/Users/Kanon/Documents/Health_Pricing_GLM/saved_data/")
  # save_data(name_claim, panel_ass)
}

library(rworldmap)
library(WDI)
library(RColorBrewer)
pays_dist <- merge(pays_dist, replace, by = "pays_expat", all.x = T)
mymap <- joinCountryData2Map( pays_dist, joinCode = "NAME",
                              suggestForFailedCodes = T,nameJoinColumn = "pays_expat") 
setwd("/Users/Kanon/Documents/Health_Pricing_GLM")
png(filename = paste0("Manque_d'observation_pays.png"), width = 6, height = 3.25,
    units = "in",res = 400, pointsize = 2)

mapCountryData(map, nameColumnToPlot = "sum",mapTitle = "Manque_d'observation", 
                catMethod = "categorical", numCats = )
dev.off()



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
