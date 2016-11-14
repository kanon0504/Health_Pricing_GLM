library(sas7bdat)

############################ Functions and tests ############################ 

# A function that allows to show the schema of a dataframe with all variables converted #
# to factors #
show_data_as_factor <- function(x)
{
  if(length(levels(as.factor(x))) <= 50)
  {return(levels(as.factor(x)))}
  else
  {return(length(levels(as.factor(x))))}
}

# A function which returns the names of columns that contains NA value and the number of#
# NA values it contains #
detection_NA <- function(DATASET)
{
  f<- function(x)
  {return(length(which(is.na(x))))}
  raw <- sapply(DATASET, f)
  return(raw[which(raw != 0)])
}

# See the discrete conditional distribution given some condition #
sort(table(head(panel_ass[panel_ass$sexe == "I",],918)$type_assure))

############################ Functions and tests ############################ 

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

# Convert all data.frame objects into data.table objects #
panel_ass <- as.data.frame.table(panel_ass)
panel_generaliste <- as.data.frame.table(panel_generaliste)
# panel_hospi <- as.data.frame.table(panel_hospi)
# panel_bilan <- as.data.frame.table(panel_bilan)

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

# Delete primary key
data_generaliste$pk <- NULL

# Delete variables that has numerous levels which have few information
data_generaliste$nationalite_2 <- NULL
data_generaliste$pays_expat_2 <- NULL
data_generaliste$pays_2 <- NULL

############################ Data Pre-processing ############################ 


############################ Generate training and testing data ############################ 

set.seed(5)
size <- dim(data_generaliste)[1]
index <- c(1:size)
training_index <- sample(index, size = 0.8*size, replace = FALSE)
testing_index <- setdiff(index, training_index)

############################ Generate training and testing data ############################ 



