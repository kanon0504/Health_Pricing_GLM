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
  if(sum(raw) == 0)
  {return("None of the entry contains NA value!")}
  else
  {return(raw[which(raw != 0)])}
}

# A function that randomly samples the training dataset and testing dataset, a vector in the size of 
# dataset is created to index all data entries. We allocate with a designate propotion the training
# dataset(tr) and the testing dataset(te) with respect to the index
allocate_dataset <- function(DATASET, seed = 5, training_prop = 0.8)
{
  # Set the seed for the psudo random function sample
  set.seed(seed)
  # Get the size of the dataset(number of entries)
  size <- dim(DATASET)[1]
  index <- c(1:size)
  training_index <- sample(index, size = training_prop*size, replace = FALSE)
  testing_index <- setdiff(index, training_index)
  tr <- DATASET[training_index,]
  te <- DATASET[testing_index,]
  print(dim(tr))
  return_list <- list(tr, te)
  return(return_list)
}

# A random split function
# method pseudo: Subset firstly a dataset that contains all different levels, randomly fill in the rest.
# method omit: Remove observations which contain levels that are not appeared in training dataset
random_split <- function(dt, train_portion = 0.8, method = 'omit')
{
  len <- dim(dt)[1]
  if (method == 'pseudo')
  {
    fcts <- names(dt)[which(sapply(dt,is.factor))]
    train <- data.frame()
    pre.select <- c()
    for (i in fcts)
    {
      name <- levels(dt[[i]])
      index <- c()
      while(length(name) != 0)
      {
        index <- c(index, which(dt[[i]]==name[1])[2])
        name <- name[-1]
      }
      pre.select <- c(pre.select,index)
    }
    pre.select <- unique(pre.select)
    reserve.len <- length(pre.select)
    train.index <- pre.select
    rest.index <- setdiff(c(1:len),pre.select)
    add.train <- sample(rest.index,((train_portion*len)-reserve.len))
    train.index <- c(train.index,rest.index[add.train])
    test.index <- setdiff(c(1:len),train.index)
    train.set <- dt[train.index,]
    train.set <- train.set[complete.cases(train.set),]
    test.set <- dt[test.index,]
    test.set <- test.set[complete.cases(test.set),]
    returnlist <- list(train.set,test.set)
    return(returnlist)
  }
  
  
  if (method == 'omit')
  {
    fcts.index <- sapply(dt,is.factor)
    train.index <- sample(c(1:len),round(train_portion*len))
    test.index <- setdiff(c(1:len),train.index)
    train.set <- dt[train.index,]
    #    train.set <- train.set[complete.cases(train.set),]
    test.set <- dt[test.index,]
    #    test.set <- test.set[complete.cases(test.set),]
    fcts <- names(train.set)[which(sapply(train.set,is.factor))]
    name <- c()
    for (i in fcts) {name <- c(name,levels(train.set[[i]]))}
    f <- function(x) 
    {
      if (length(intersect(x,name))==0)
        return(F)
      else
        return(T)
    }
    fact.test.set <- test.set[,which(sapply(test.set,is.factor))]
    clean.index <- which(apply(fact.test.set,1,f))
    test.set <- test.set[clean.index,]
    returnlist <- list('train.set' = train.set, 'test.set' = test.set)
    return(returnlist)
  }
}

# A function that detects all the binary variables and turns then into factors
binary_to_factor <- function(DATASET)
{
  class <- sapply(DATASET,class)
  n_set <- names(DATASET)[which(class == 'numeric')]
  for(name in n_set)
  {
    levels <- unique(get(name,DATASET))
    if(identical(levels,c(1,0))|identical(levels,c(0,1)))
    { DATASET[[name]] <- factor(DATASET[[name]])}
  }
  return(DATASET)
}

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
data_generaliste$pays_2 <- NULL
data_generaliste$nb_adherents <- NULL
data_generaliste <- binary_to_factor(data_generaliste)

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


# glm_model <- 
############################ Generate training and testing data ############################ 



