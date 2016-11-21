setwd("/Users/Kanon/Documents/Health_Pricing_GLM")
source('utils.R')


# A function that loads in polices and claims dataset then performs merging and selection
data_preprocessing <- function(name_claim_data, verbose = TRUE)
{
  # Load datasets on Macbook Pro #
  claim_data <- sas7bdat::read.sas7bdat(paste0("/Users/Kanon/Google Drive/AXA/data/MSH/",name_claim_data))
  panel_ass <- sas7bdat::read.sas7bdat("/Users/Kanon/Google Drive/AXA/data/MSH/panel_ass.sas7bdat")
  # Load datasets on Windows #
  # panel_ass <- read.sas7bdat("C:/Users/s636000/Documents/Expat/data/MSH/panel_ass.sas7bdat")
  # claim_data <- read.sas7bdat(paste0("C:/Users/s636000/Documents/Expat/data/MSH/",name_claim_data))
  
  # Check the format of each data set #
  if (verbose == TRUE)
  {print(paste0("Check weather ", name_claim_data, " is a data.frame object: "))}
  is.data.frame(claim_data)
  
  # Set the pk (primary key) equals to paste(annee,ident_personne) in preparation for left join #
  if (verbose == TRUE)
  {print(paste0("Adding a new variable serves as the primary key 'pk' in panel_ass") )}
  panel_ass$pk <- as.numeric(paste(panel_ass$annee,panel_ass$ident_personne,sep=""))
  
  # Check duplication in the loaded claims dataset. If exist, remove all duplications #
  if (verbose == TRUE)
  {print(paste0("Checking duplication in ", deparse(substitute(claim_data)), "..."))}
  claim_data <- check_dup(claim_data, verbose = verbose)
  
  # Preparation with dataset claim_data for consistancy #
  if (verbose == TRUE)
  {print(paste0("Adding a new variable serves as the primary key 'pk' in ", name_claim_data) )}
  claim_data$pk <- as.numeric(paste(claim_data$annee,claim_data$ident_personne,sep=""))
  
  # In preparation for the left outer join, some redundant variables in claim_data #
  # are removed #
  # ident_personne and annee_soin are included in pk #
  # A test has been performed to check weather the presence in claim_data corresponds to #
  # the presence in panel_ass. Results turned out to be positive, thus no need in keeping both #
  if (verbose == TRUE)
  {print(paste0("Variables 'ident_personne', 'annee_soin', 'presence' are removed from ", name_claim_data))}
  claim_data$ident_personne <- NULL
  claim_data$annee_soin <- NULL
  claim_data$presence <- NULL
  
  # Left outer join of panel_ass and claim_data #
  if (verbose == TRUE)
  {print(paste0("Left outer joining ",name_claim_data, " to panel_ass by key 'pk'..."))}
  merged_data <- merge(panel_ass,claim_data, by = "pk", all.x = TRUE)
  
  ## Deal with the NA values in data_generaliste ##
  # Detect all the columns that contain NA value and the number of them #
  detection_NA(merged_data)
  
}


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

