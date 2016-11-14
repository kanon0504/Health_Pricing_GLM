library(sas7bdat)

# Load datasets
panel_ass <- read.sas7bdat("/Users/Kanon/Google Drive/AXA/data/MSH/panel_ass.sas7bdat")
panel_generaliste <- read.sas7bdat("/Users/Kanon/Google Drive/AXA/data/MSH/panel_generaliste_decompressed.sas7bdat")

# Check the format of each data set
is.data.frame(panel_ass)
is.data.frame(panel_generaliste)

# Convert all data.frame objects into data.table objects
panel_ass <- as.data.frame.table(panel_ass)
panel_generaliste <- as.data.frame.table(panel_generaliste)

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

# Check the consistancy of the relationship of multiple variables(logical check)
logical_check <- function()
  