library(sas7bdat)
library(utils)

############################ Data Pre-processing ############################ 

# Load datasets on Windows #
# panel_ass <- read.sas7bdat("C:/Users/s636000/Documents/Expat/data/MSH/panel_ass.sas7bdat")
# panel_generaliste <- read.sas7bdat("C:/Users/s636000/Documents/Expat/data/MSH/panel_generaliste_decompressed.sas7bdat")
# panel_hospi <- read.sas7bdat("C:/Users/s636000/Documents/Expat/data/MSH/panel_hospi_decompressed.sas7bdat")
# panel_bilan <- read.sas7bdat("C:/Users/s636000/Documents/Expat/data/MSH/panel_bilan_decompressed.sas7bdat")

# Load datasets on Macbook Pro #
panel_ass <- read.sas7bdat("/Users/Kanon/Google Drive/AXA/data/MSH/panel_ass.sas7bdat")
panel_generaliste <- read.sas7bdat("/Users/Kanon/Google Drive/AXA/data/MSH/panel_generaliste_decompressed.sas7bdat")
# panel_hospi <- read.sas7bdat("/Users/Kanon/Google Drive/AXA/data/MSH/panel_hospi_decompressed.sas7bdat")
# panel_bilan <- read.sas7bdat("/Users/Kanon/Google Drive/AXA/data/MSH/panel_bilan_decompressed.sas7bdat")

# Check the format of each data set #
is.data.frame(panel_ass)
is.data.frame(panel_generaliste)
# is.data.frame(panel_hospi)
# is.data.frame(panel_bilan)

# Set the pk (primary key) equals to paste(annee,ident_personne) in preparation for left join #
panel_ass$pk <- as.numeric(paste(panel_ass$annee,panel_ass$ident_personne,sep=""))

# Preparation with dataset panel_generaliste for consistancy #
panel_generaliste$pk <- as.numeric(paste(panel_generaliste$annee,panel_generaliste$ident_personne,sep=""))
dedup_panel_generaliste <- unique(panel_generaliste)
rm(panel_generaliste)

# ident_personne and annee_soin are included in pk #
dedup_panel_generaliste$ident_personne <- NULL
dedup_panel_generaliste$annee_soin <- NULL

# somme_frais is not applicable in frequency modelling #
dedup_panel_generaliste$somme_frais <- NULL

# A test has been performed to check weather the presence in panel_generaliste corresponds to #
# the presence in panel_ass. Results turned out to be positive, thus no need in keeping both #
dedup_panel_generaliste$presence <- NULL

# Left outer join of panel_ass and dedup_panel_generaliste #
data_generaliste <- merge(panel_ass,dedup_panel_generaliste, by = "pk", all.x = TRUE)


## Deal with the NA values in data_generaliste ##
# Detect all the columns that contain NA value and the number of them #
detection_NA(data_generaliste)

# Assgin 0 to the frequency column for those who havn't had claims during exposure #
data_generaliste$somme_quantite[is.na(data_generaliste$somme_quantite)] <- 0

# Replace some of the missing entries(30%) of date_sortie by date_sortie_obs #
data_generaliste$date_sortie[is.na(data_generaliste$date_sortie)] <- 
  data_generaliste$date_sortie_obs[which(is.na(data_generaliste$date_sortie))]

# Delete entries whose date_naissance(age at the same time) is NA(very few, 8 cases in panel_ass) #
data_generaliste <- data_generaliste[-which(is.na(data_generaliste$age)),]

# Detect all the columns that contain NA value and the number of them #
detection_NA(data_generaliste)

# Delete primary key
data_generaliste$pk <- NULL

# Delete variables that has numerous levels which have few information
data_generaliste$nationalite_2 <- NULL
data_generaliste$pays_expat_2 <- NULL
data_generaliste$pays <- NULL
data_generaliste$pays_2 <- NULL
data_generaliste$nb_adherents <- NULL
data_generaliste <- binary_to_factor(data_generaliste)
data_generaliste <- eliminate_negative(data_generaliste)

############################ Data Pre-processing ############################ 


############################ Generate training and testing data ############################ 

returnlist <- random_split(data_generaliste)
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

kpi <- kpi_gini(predrisk = prediction, truerisk = te$somme_quantite, exposure = te$presence, significance = 6)
############################ Testing of glm_model model ############################ 

