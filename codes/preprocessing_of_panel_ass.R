setwd("F:/NORMES TECHNIQUES/PERIMETRE EXPATRIES/12 - Tarification GLM 2016/06-Modèle GLM")
source("./Codes dans R/utils.R")

# load in data set
panel_ass <- sas7bdat::read.sas7bdat("./Base de donnée brute/Assurées/panel_ass.sas7bdat")

panel_ass <- data.table::data.table(panel_ass)

# remove constant variable
panel_ass$nb_adherents <- NULL

# country name replacement
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

levels(panel_ass$pays_expat)[levels(panel_ass$pays_expat)=="BR. VIRGIN ISLAND"] <-
  "British Virgin Islands"

levels(panel_ass$pays_expat)[levels(panel_ass$pays_expat)=="SERBIA MONTENEGRO"] <-
  "Serbia"
 
# removal of countries with little impact
panel_ass <- panel_ass[pays_expat != "DOMINICA",]

panel_ass <- panel_ass[pays_expat != "FIJI",]

panel_ass <- panel_ass[pays_expat != "FR. SOUTHERN TERRIT.",]

panel_ass <- panel_ass[pays_expat != "GREENLAND",]

panel_ass <- panel_ass[pays_expat != "ISLE OF MAN",]

panel_ass <- panel_ass[pays_expat != "KYRGYZSTAN",]

panel_ass <- panel_ass[pays_expat != "PITCAIRN ISLANDS",]

panel_ass <- panel_ass[pays_expat != "SAINT LUCIA",]

panel_ass <- panel_ass[pays_expat != "SAMOA",]

panel_ass <- panel_ass[pays_expat != "SOLOMON ISLANDS",]

panel_ass <- panel_ass[pays_expat != "SOMALIA",]

panel_ass <- panel_ass[pays_expat != "SWAZILAND",]

panel_ass <- panel_ass[pays_expat != "TAJIKISTAN",]

panel_ass <- panel_ass[pays_expat != "TIMOR-LESTE",]

panel_ass <- panel_ass[pays_expat != "TONGA",]

panel_ass <- panel_ass[pays_expat != "US VIRIN ISLANDS",]

panel_ass <- panel_ass[pays_expat != "ST VINCENT & GREN.",]

panel_ass <- panel_ass[pays_expat != "ARUBA",]

panel_ass <- panel_ass[pays_expat != "BHUTAN",]

panel_ass <- panel_ass[pays_expat != "KOSOVO",]

panel_ass <- panel_ass[pays_expat != "ERITREA",]

panel_ass <- panel_ass[pays_expat != "MALDIVES",]

panel_ass <- panel_ass[pays_expat != "GUINEA-BISSAU",]

panel_ass <- panel_ass[pays_expat != "SERIA MONTENEGRO",]

panel_ass <- panel_ass[pays_expat != "CAPE VERDE",]

panel_ass <- panel_ass[pays_expat != "SOUTH SUDAN",]

panel_ass <- panel_ass[pays_expat != "US MINOR OUT. ISL.",]

panel_ass <- panel_ass[pays_expat != "INCONNU",]

panel_ass$pays_expat <- factor(panel_ass$pays_expat)

# merge from 38 actes to 24 actes
panel_ass$Xautres_protheses <- 
  rowSums(panel_ass[,.(Autres_protheses,
                       Protheses_auditives_pharmacie)])

panel_ass$Autres_protheses <- NULL

panel_ass$Protheses_auditives_pharmacie <- NULL

panel_ass$Xauxiliaires_medicaux <- 
  rowSums(panel_ass[,.(Auxiliaire_medical,
                       Kinesitherapie)])

panel_ass$Auxiliaire_medical <- NULL

panel_ass$Kinesitherapie <- NULL

panel_ass$Xbilan_de_sante <- 
  rowSums(panel_ass[,.(Bilan_de_sante,
                       Medecine_preventive)])

panel_ass$Bilan_de_sante <- NULL

panel_ass$Medecine_preventive <- NULL

panel_ass$Xchambre_particuliere <- 
  panel_ass[,Chambre_particuliere]

panel_ass$Chambre_particuliere <- NULL

panel_ass$Xconsultation <- 
  rowSums(panel_ass[,.(Specialiste,
                       Generaliste)])

panel_ass$Specialiste <- NULL

panel_ass$Generaliste <- NULL

panel_ass$Xcures_thermales <- 
  panel_ass[,Cures_thermales]

panel_ass$Cures_thermales <- NULL

panel_ass$Xdivers <- 
  panel_ass[,Divers]

panel_ass$Divers<- NULL

panel_ass$Xfiv <- 
  panel_ass[,Traitement_de_la_fertilite]

panel_ass$Traitement_de_la_fertilite <- NULL

panel_ass$Xhospitalisation_sauf_chambre_particuliere <- 
  rowSums(panel_ass[,.(Hospitalisation,
                       Hospitalisation_de_jour,
                       Chimiotherapie,
                       Lit_accompagnant,
                       Ambulance_transport)])

panel_ass$Hospitalisation <- NULL

panel_ass$Hospitalisation_de_jour <- NULL

panel_ass$Chimiotherapie <- NULL

panel_ass$Lit_accompagnant <- NULL

panel_ass$Ambulance_transport <- NULL

panel_ass$Ximplants_dentaires <- 
  panel_ass[,Implants]

panel_ass$Implants <- NULL

panel_ass$Xkeratotomie <- 
  panel_ass[,Keratomie]

panel_ass$Keratomie <- NULL

panel_ass$Xlentilles <- 
  panel_ass[,Lentilles]

panel_ass$Lentilles <- NULL

panel_ass$Xmaternite_sauf_fiv <- 
  rowSums(panel_ass[,.(Cesarienne,
                       Maternite_generale)])

panel_ass$Cesarienne <- NULL

panel_ass$Maternite_generale <- NULL

panel_ass$Xmedecine_alternative <- 
  rowSums(panel_ass[,.(Acuponcture,
                       Chiropractie,
                       Autres_medecines_alternatives,
                       Osteopathie)])

panel_ass$Acuponcture <- NULL

panel_ass$Chiropractie <- NULL

panel_ass$Autres_medecines_alternatives <- NULL

panel_ass$Osteopathie <- NULL

panel_ass$Xmontures <- 
  rowSums(panel_ass[,.(Montures, 
                       Optique)])

panel_ass$Montures <- NULL

panel_ass$Optique <- NULL

panel_ass$Xorthodontie <- 
  panel_ass[,Orthodontie]

panel_ass$Orthodontie <- NULL

panel_ass$Xparadontologie <- 
  panel_ass[,Parodontologie]

panel_ass$Parodontologie <- NULL

panel_ass$Xpetit_risque <- 
  rowSums(panel_ass[,.(Petit_risque,
                       Radiotherapie)])

panel_ass$Petit_risque <- NULL

panel_ass$Radiotherapie <- NULL

panel_ass$Xpharmacie <- 
  panel_ass[,Pharmacie]

panel_ass$Pharmacie <- NULL

panel_ass$Xprothese_dentaire <- 
  panel_ass[,Protheses_dentaires]

panel_ass$Protheses_dentaires <- NULL

panel_ass$Xpsychiatrie <-
  panel_ass[,Psychiatrie]

panel_ass$Psychiatrie <- NULL

panel_ass$Xsoins_dentaires <- 
  panel_ass[,Dentaire_general]

panel_ass$Dentaire_general <- NULL

panel_ass$Xvaccination <-
  panel_ass[,Vaccins]

panel_ass$Vaccins <- NULL

panel_ass$Xverres <- 
  panel_ass[,Verres]

panel_ass$Verres <- NULL

# assign each garanti to binary variable
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

for (act in garanti)
{ 
  panel_ass[[act]] <- 
    as.numeric(as.logical(panel_ass[[act]]))
}

# binary numeric to factor transformation
panel_ass <- binary_to_factor(panel_ass)

# remove sex type "I"(inconnu)
panel_ass <- panel_ass[sexe != "I",]

# remove all observation from 2013
panel_ass <- panel_ass[annee != 2013,]

# Replace some of the missing entries(30%) of date_sortie by date_sortie_obs #
panel_ass$date_sortie[is.na(panel_ass$date_sortie)] <- 
  panel_ass$date_sortie_obs[which(is.na(panel_ass$date_sortie))]


# delete observations whose age is below 0
panel_ass <- panel_ass[age >= 0,]

# convert date sortie obs and date entree into timestamp object
panel_ass$date_entree <- as.Date(panel_ass$date_entree, origin = '1960-01-01')

panel_ass$date_sortie <- as.Date(panel_ass$date_sortie, origin = '1960-01-01')

panel_ass$date_sortie_obs <- as.Date(panel_ass$date_sortie_obs, origin = '1960-01-01')

# Furthuer removal of useless or unidentified variables from panel_ass
panel_ass$ident_famille <- NULL

panel_ass$ident_police <- NULL

# Create a feature which indicates whether the date_sortie equals to date_sortie_obs
panel_ass$sortie_prevu <- 
  (panel_ass$date_sortie == panel_ass$date_sortie_obs)

# Furthuer removal of useless or unidentified variables from panel_ass
panel_ass$date_sortie <- NULL

panel_ass$pointeur_origine <- NULL

panel_ass$temps_de_presence <- NULL

panel_ass$IDENT_CONV <- NULL

panel_ass$categorie <- NULL

# Delete variables that has numerous levels which have few information
panel_ass$nationalite_2 <- NULL

panel_ass$pays_expat_2 <- NULL

panel_ass$pays <- NULL

panel_ass$pays_2 <- NULL

# convert panel_ass to data.frame object for writing
panel_ass <- data.frame(panel_ass)

write.csv(panel_ass,
          file = "./Base de donnée après traitement/Assurées/panel_ass_clean.csv", 
          row.names = FALSE)
