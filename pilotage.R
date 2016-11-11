library(sas7bdat)

panel_ass <- read.sas7bdat("/Users/Kanon/Downloads/panel_ass.sas7bdat")

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

