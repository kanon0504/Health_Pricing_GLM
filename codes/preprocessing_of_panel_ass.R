setwd("F:/NORMES TECHNIQUES/PERIMETRE EXPATRIES/12 - Tarification GLM 2016/06-Modèle GLM")
source("./Codes dans R/utils.R")

# load in data set
panel_ass <- sas7bdat::read.sas7bdat("./Base de donnée brute/Assurées/panel_ass.sas7bdat")
panel_ass <- data.table::data.table(panel_ass)

# remove constant variable
panel_ass$nb_adherents <- NULL

# binary numeric to factor transformation
panel_ass <- binary_to_factor(panel_ass)

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
panel_ass$pays_expat <- factor(panel_ass$pays_expat)

# remove sex type "I"(inconnu)
panel_ass <- panel_ass[sexe != "I",]

# remove all observation from 2013
panel_ass <- panel_ass[annee != 2013,]

# Replace some of the missing entries(30%) of date_sortie by date_sortie_obs #
panel_ass$date_sortie[is.na(panel_ass$date_sortie)] <- 
  panel_ass$date_sortie_obs[which(is.na(panel_ass$date_sortie))]

# Delete entries whose date_naissance(age at the same time) is NA(very few, 8 cases in panel_ass) #
panel_ass <- panel_ass[-which(is.na(panel_ass$age)),]

# delete observations whose age is below 0
panel_ass <- panel_ass[age >= 0,]

# convert panel_ass to data.frame object for writing
panel_ass <- data.frame(panel_ass)
write.csv(panel_ass, file = "./Base de donnée après traitement/Assurées/panel_ass_clean.csv")
