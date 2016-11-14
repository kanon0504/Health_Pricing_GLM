library(sas7bdat)

###################### Functions and tests ###################### 

# A function that allows to show the schema of a dataframe with all variables converted to factors
show_data_as_factor <- function(x)
{
  if(length(levels(as.factor(x))) <= 50)
  {return(levels(as.factor(x)))}
  else
  {return(length(levels(as.factor(x))))}
}

# See the discrete conditional distribution given some condition
sort(table(head(panel_ass[panel_ass$sexe == "I",],918)$type_assure))

###################### Functions and tests ###################### 



# Load datasets on Windows
# panel_ass <- read.sas7bdat("C:/Users/s636000/Documents/Expat/data/MSH/panel_ass.sas7bdat")
# panel_generaliste <- read.sas7bdat("C:/Users/s636000/Documents/Expat/data/MSH/panel_generaliste_decompressed.sas7bdat")
# panel_hospi <- read.sas7bdat("C:/Users/s636000/Documents/Expat/data/MSH/panel_hospi_decompressed.sas7bdat")
# panel_bilan <- read.sas7bdat("C:/Users/s636000/Documents/Expat/data/MSH/panel_bilan_decompressed.sas7bdat")


# Load datasets on Macbook Pro
panel_ass <- read.sas7bdat("/Users/Kanon/Google Drive/AXA/data/MSH/panel_ass.sas7bdat")
panel_generaliste <- read.sas7bdat("/Users/Kanon/Google Drive/AXA/data/MSH/panel_generaliste_decompressed.sas7bdat")
panel_hospi <- read.sas7bdat("/Users/Kanon/Google Drive/AXA/data/MSH/panel_hospi_decompressed.sas7bdat")
panel_bilan <- read.sas7bdat("/Users/Kanon/Google Drive/AXA/data/MSH/panel_bilan_decompressed.sas7bdat")

# Check the format of each data set
is.data.frame(panel_ass)
is.data.frame(panel_generaliste)

# Convert all data.frame objects into data.table objects
panel_ass <- as.data.frame.table(panel_ass)
panel_generaliste <- as.data.frame.table(panel_generaliste)

# Set the pk (primary key) equals to paste(annee,ident_personne) in preparation for left join
panel_ass$pk <- as.numeric(paste(panel_ass$annee,panel_ass$ident_personne,sep=""))

# Preparation with dataset panel_generaliste for consistancy
panel_generaliste$Var1 <- NULL
panel_generaliste$Var2 <- NULL
names(panel_generaliste) <- c("ident_personne", "presence", "annee_soin", "somme_frais", "somme_quantite")
panel_generaliste$fk <- as.numeric(paste(panel_generaliste$annee,panel_generaliste$ident_personne,sep=""))




