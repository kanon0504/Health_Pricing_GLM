############################ Functions and tests ############################ 

# group variable nationalite with respect to freqency
group_nationalite_freq <- function(merged_data, name, print.csv = TRUE)
{
  r <- rpart::rpart(somme_quantite ~ nationalite, data = merged_data, cp = 0)
  cp <- r$cptable[10]
  r <- rpart::prune(r, cp = cp)
  replace <- data.frame(nationalite = merged_data$nationalite, group_nationalite_freq =  r$where)
  replace <- unique(replace)
  replace$group_nationalite_freq <- factor(replace$group_nationalite_freq)
  for (i in 1:length(levels(replace$group_nationalite_freq)))
  {levels(replace$group_nationalite_freq)[i] <- paste0("G",i)}
  if (print.csv == TRUE)
  {
    write.csv(replace, file = paste0("./grouping/nationalite_freq/",name,"_nationalite_freq.csv"))
    map <- rworldmap::joinCountryData2Map( replace
                                           ,joinCode = "NAME"
                                           ,nameJoinColumn = "nationalite") 
  }
  merged_data <- merge(merged_data,replace,by = "nationalite", all.x =T)
  return(merged_data)
}

# group variable nationalite with respect to cout
group_nationalite_cout <- function(merged_data, name, cp = 0.0001, print.csv = T, verbose = T)
{
  data_cout <- merged_data[cout_moyen > 0,]
  data_cout$nationalite <- factor(data_cout$nationalite)
  level0 <- setdiff(levels(merged_data$nationalite),levels(factor(data_cout$nationalite)))
  left_out <- data.frame(nationalite = level0, group_nationalite_cout = 0)
  r <- rpart::rpart(cout_moyen ~ nationalite, data = data_cout, cp = 0)
  cp <- r$cptable[10]
  r <- rpart::prune(r, cp = cp)
  replace <- data.frame(nationalite = data_cout$nationalite, group_nationalite_cout = r$where)
  replace <- unique(replace)
  replace <- rbind(replace,left_out)
  replace$group_nationalite_cout <- factor(replace$group_nationalite_cout)
  for (i in 1:length(levels(replace$group_nationalite_cout)))
  {levels(replace$group_nationalite_cout)[i] <- paste0("G",i)}
  replace <- replace[order(replace$group_nationalite_cout),]
  if (print.csv == TRUE)
  {
    write.csv(replace, file = paste0("./grouping/nationalite_cout/",name,"_nationalite_cout.csv"))
    map <- rworldmap::joinCountryData2Map( replace
                                           ,joinCode = "NAME"
                                           ,nameJoinColumn = "nationalite") 
  }
  merged_data <- merge(merged_data,replace,by = "nationalite", all.x =T)
  return(merged_data)
}

# group variable pays with respect to freqency
group_pays_freq <- function(merged_data, name, print.csv = TRUE, minsplit = 15)
{
  r <- rpart::rpart(somme_quantite ~ pays_expat, data = merged_data, cp = 0)
  cp <- r$cptable[10]
  r <- rpart::prune(r, cp = cp)
  replace <- data.frame(pays_expat = merged_data$pays_expat, group_pays_freq =  r$where)
  replace <- unique(replace)
  replace$group_pays_freq <- factor(replace$group_pays_freq)
  for (i in 1:length(levels(replace$group_pays_freq)))
  {levels(replace$group_pays_freq)[i] <- paste0("G",i)}
  if (print.csv == TRUE)
  {
    write.csv(replace, file = paste0("./grouping/pays_freq/",name,"_pays_freq.csv"))
    map <- rworldmap::joinCountryData2Map( replace
                                ,joinCode = "NAME"
                                ,nameJoinColumn = "pays_expat") 
    png(filename = paste0("./plots/pays_freq/",name,"_group_freq.png"), width = 6, height = 3.25,
        units = "in",res = 400, pointsize = 2)
    rworldmap::mapCountryData(map, nameColumnToPlot = "group_pays_freq",mapTitle = paste0("Group_en_fonction_de_freq_",name),
                   catMethod = "categorical")
    dev.off()
  }
  merged_data <- merge(merged_data,replace,by = "pays_expat", all.x =T)
  return(merged_data)
}


# group variable pays with respect to cout
group_pays_cout <- function(merged_data, name, print.csv = T, verbose = T)
{
  data_cout <- merged_data[cout_moyen > 0,]
  data_cout$pays_expat <- factor(data_cout$pays_expat)
  level0 <- setdiff(levels(merged_data$pays_expat),levels(factor(data_cout$pays_expat)))
  left_out <- data.frame(pays_expat = level0, group_pays_cout = 0)
  r <- rpart::rpart(cout_moyen ~ pays_expat, data = data_cout, cp = 0)
  cp <- r$cptable[10]
  r <- rpart::prune(r, cp = cp)
  replace <- data.frame(pays_expat = data_cout$pays_expat, group_pays_cout = r$where)
  replace <- unique(replace)
  replace <- rbind(replace,left_out)
  replace$group_pays_cout <- factor(replace$group_pays_cout)
  for (i in 1:length(levels(replace$group_pays_cout)))
  {levels(replace$group_pays_cout)[i] <- paste0("G",i)}
  replace <- replace[order(replace$group_pays_cout),]
  if (print.csv == TRUE)
  {
    write.csv(replace, file = paste0("./grouping/pays_cout/",name,"_pays_cout.csv"))
    map <- rworldmap::joinCountryData2Map( replace
                                           ,joinCode = "NAME"
                                           ,nameJoinColumn = "pays_expat") 
    png(filename = paste0("./plots/pays_cout/",name,"_group_cout.png"), width = 6, height = 3.25,
        units = "in",res = 400, pointsize = 2)
    rworldmap::mapCountryData(map, nameColumnToPlot = "group_pays_cout",mapTitle = paste0("Group_en_fonction_de_cout_",name),
                              catMethod = "categorical")
    dev.off()
  }
  merged_data <- merge(merged_data,replace,by = "pays_expat", all.x =T)
  return(merged_data)
}

# group variable age with respect to cout
group_age_cout <- function(merged_data, name, print.csv = TRUE)
{
  data_cout <- merged_data[cout_moyen > 0,]
  level0 <- setdiff(unique(merged_data$age),unique(factor(data_cout$age)))
  left_out <- data.frame(age = level0, group_age_cout = 0)
  r <- rpart::rpart(cout_moyen ~ age, data = data_cout, cp = 0)
  cp <- r$cptable[10]
  r <- rpart::prune(r, cp = cp)
  rpart.plot::rpart.plot(r)
  replace <- data.frame(age = data_cout$age, group_age_cout =  r$where)
  replace <- unique(replace)
  group_name <- levels(as.factor(replace$group_age_cout))
  level_name <- c()
  for (i in 1:length(group_name))
  {
    ma <- max(replace[replace$group_age_cout == group_name[i],]$age)
    mi <- min(replace[replace$group_age_cout == group_name[i],]$age)
    level_name <- c(level_name, paste0(mi,"-",ma))
  }
  replace$group_age_cout <- as.factor(replace$group_age_cout)
  levels(replace$group_age_cout) <- level_name
  if (print.csv == TRUE)
  {
    setwd("/Users/Kanon/Documents/Health_Pricing_GLM/saved_groups/")
    write.csv(replace, file = paste0(name,"_age_cout.csv"))
  }
  merged_data <- merge(merged_data,replace,by = "age", all.x =T)
  return(merged_data)
}

# group variable age with respect to frequency
group_age_freq <- function(merged_data, name, print.csv = TRUE)
{
  r <- rpart::rpart(somme_quantite ~ age, data = merged_data, cp = 0)
  cp <- r$cptable[10]
  r <- rpart::prune(r, cp = cp)
  rpart.plot::rpart.plot(r)
  replace <- data.frame(age = merged_data$age, group_age_freq =  r$where)
  replace <- unique(replace)
  group_name <- levels(as.factor(replace$group_age_freq))
  level_name <- c()
  for (i in 1:length(group_name))
  {
    ma <- max(replace[replace$group_age_freq == group_name[i],]$age)
    mi <- min(replace[replace$group_age_freq == group_name[i],]$age)
    level_name <- c(level_name, paste0(mi,"-",ma))
  }
  replace$group_age_freq <- as.factor(replace$group_age_freq)
  levels(replace$group_age_freq) <- level_name
  if (print.csv == TRUE)
  {
    setwd("/Users/Kanon/Documents/Health_Pricing_GLM/saved_groups/")
    write.csv(replace, file = paste0(name,"_age_freq.csv"))
  }
  merged_data <- merge(merged_data,replace,by = "age", all.x =T)
  return(merged_data)
}

# for merging 
merge_data <- function(namelist,merged_name)
{
  len <- length(namelist)
  if (len > 1)
  {
    test <- eval(parse(text = namelist[1]))
    for (i in 1:(len -1))
    {
      test <- rbind(test,eval(parse(text = namelist[i+1])))
    }
    test$pk <- as.character(paste(test$annee,test$ident_personne,test$presence,sep="_"))
    test <- data.table::data.table(test)
    merged_test <- test[,.( somme_frais = sum(somme_frais), somme_quantite = sum(somme_quantite)),by = pk]
    print(sum(merged_test$somme_quantite) == sum(test$somme_quantite))
    merged_test <- merged_test %>% separate(pk, c('annee_soin','ident_personne','presence'),'_')
    setwd("/Users/Kanon/Google Drive/AXA/data/Merged_data")
    merged_name <- paste0(substring(merged_name,2), ".csv")
    write.csv(merged_test, file = merged_name)
    return(merged_test)
  }
  else
  {
    unchanged <- eval(parse(text = namelist))
    merged_name <- paste0(substring(merged_name,2), ".csv")
    write.csv(unchanged, file = merged_name)
    return(unchanged)
  }
}

# check distribution
check_dist <- function(data, verbose = TRUE)
{
  #data <- eliminate_negative(data, verbose = FALSE)
  name <- deparse(substitute(data))
  mean <- mean(data$somme_quantite)
  var <- var(data$somme_quantite)
  logmean <- mean(log(data$somme_quantite + 1))
  logvar <- var(log(data$somme_quantite + 1))
  if (verbose == TRUE)
  {
    print(paste0("the mean frequency of ",name, " is ",mean))
    print(paste0("the variance of frequency of ",name, " is ",var))
    print(paste0("the log mean frequency of ",name, " is ",logmean))
    print(paste0("the log variance of frequency of ",name, " is ",logvar))
  }
  return(c(mean,var,logmean,logvar))
}
  
# get name
get_name <- function(name_claim_data)
{
  name <- strsplit(name_claim_data, split = "_", fixed = T)[[1]]
  name <- name[-length(name)]
  name <- name[-1]
  if (length(name) > 1)
  {name <- paste(name, collapse = '_')}
  return(name)
}
  
  
# save data
save_data <- function(name_claim, panel_ass)
{
  merged_data <- data_preprocessing(name_claim, panel_ass = panel_ass)
  merged_data <- data.table::data.table(merged_data)
  name <- get_name(name_claim)
  plot_data <- merged_data[sexe != 'I', .(frais_par_tete = sum(somme_frais)/sum(presence),
                                          freq = sum(somme_quantite)/sum(presence),
                                          cout = sum(somme_frais)/sum(somme_quantite),
                                          sum_claim = sum(somme_quantite)),
                           by = "annee"]
  plot_data$nb_obv = merged_data[sexe != 'I'][somme_quantite != 0 ,
                                             .(nb_obv = .N), by = "annee"]$nb_obv
  names(plot_data) <- c("annee", paste0(name,"_frais_par_tete"), paste0(name, "_freq"),
                        paste0(name, "_cout"), paste0(name, "_sum_claim"),
                        paste0(name, "_nb_observation"))
  write.csv(plot_data, paste0(name, ".csv"))
}

# plot
plot_claim <- function(name_claim, panel_ass)
{
  merged_data <- data_preprocessing(name_claim, panel_ass = panel_ass)
  merged_data <- data.table::data.table(merged_data)
  name <- get_name(name_claim)
  
  plot_data <- merged_data[sexe != 'I', .(frais_par_tete = mean(somme_frais),
                                          freq = mean(somme_quantite),
                                          cout = sum(somme_frais)/sum(somme_quantite)),
                           by = "annee"]
  freq_plot <- ggplot(plot_data, aes(x = annee, y = freq)) + ylab(paste0(name,"_freq")) + 
    geom_point() + geom_line()
  cout_plot <- ggplot(plot_data, aes(x = annee, y = cout)) + ylab(paste0(name,"_cout")) + 
    geom_point() + geom_line()
  par_tete_plot <- ggplot(plot_data, aes(x = annee, y = frais_par_tete)) + 
    ylab(paste0(name,"_frais_par_tete")) + geom_point() + geom_line()
  png(filename = paste0(name,"_freq.png"))
  plot(freq_plot)
  dev.off()
  png(filename = paste0(name,"_cout.png"))
  plot(cout_plot)
  dev.off()
  png(filename = paste0(name,"_frais_par_tete.png"))
  plot(par_tete_plot)
  dev.off()
}

# A function enables to check weather two datasets possess equivalent levels
check_levels <- function(tr,te)
{
  tr_f <- tr[,which(sapply(tr,is.factor))]
  te_f <- te[,which(sapply(te,is.factor))]
  names <- names(tr_f)
  for (name in names)
  {
    tr_levels <- levels(tr_f[,name])
    te_levels <- levels(te_f[,name])
    if (setequal(tr_levels,te_levels))
    {print(paste0("Levels are equivalent in factor ",name))}
    else
    {
        re_te <- setdiff(te_levels, tr_levels)
        print(paste0(re_te, " in ", name, "new level in testing dataset!"))
    }
  }
}

# A function that loads in polices and claims dataset then performs merging and selection
data_preprocessing <- function(name_claim_data, verbose = TRUE, panel_ass = panel_ass)
{
  name <- strsplit(name_claim_data, split = ".", fixed = T)[[1]][1]
  # Load datasets on Macbook Pro #
  if (!exists(name, envir = globalenv()))
  {
    if (verbose == T)
    {print(paste0("loading ",name))}
    claim_data <- sas7bdat::read.sas7bdat(paste0("/Users/Kanon/Google Drive/AXA/data/Merged_data"
                                                 , name_claim_data))
  }
  else(claim_data <- eval(parse(text = name)))
  
  # Check the format of each data set #
  if (verbose == TRUE)
  {print(paste0("Check whether ", name, " is a data.frame object: "))}
  is.data.frame(claim_data)
  
  # Set the pk (primary key) equals to paste(annee,ident_personne) in preparation for left join #
  if (verbose == TRUE)
  {print(paste0("Adding a new variable serves as the primary key 'pk' in panel_ass") )}
  panel_ass$pk <- as.numeric(paste(panel_ass$annee,panel_ass$ident_personne,sep=""))
  
  # Preparation with dataset claim_data for consistancy #
  if (verbose == TRUE)
  {print(paste0("Adding a new variable serves as the primary key 'pk' in ", name) )}
  claim_data$pk <- as.numeric(paste(claim_data$annee,claim_data$ident_personne,sep=""))
  
  # Check duplication in the loaded claims dataset. If exist, remove all duplications #
  if (verbose == TRUE)
  {print(paste0("Checking duplication in ", name, "..."))}
  claim_data <- check_dup(claim_data, name, verbose = verbose)
  
  # In preparation for the left outer join, some redundant variables in claim_data #
  # are removed #
  # ident_personne and annee_soin are included in pk #
  # A test has been performed to check weather the presence in claim_data corresponds to #
  # the presence in panel_ass. Results turned out to be positive, thus no need in keeping both #
  if (verbose == TRUE)
  {print(paste0("Variables 'ident_personne', 'annee_soin'
                , 'presence' are removed from ", name))}
  claim_data$ident_personne <- NULL
  claim_data$annee_soin <- NULL
  claim_data$presence <- NULL
  
  # Left outer join of panel_ass and claim_data #
  if (verbose == TRUE)
  {print(paste0("Left outer joining ",name, " to panel_ass by key 'pk'..."))}
  merged_data <- merge(panel_ass,claim_data, by = "pk", all.x = TRUE)
  
  ## Deal with the NA values in merged_data ##
  # Detect all the columns that contain NA value and the number of them #
  if (verbose == TRUE)
  {detection_NA(merged_data)}
  # Assign 0 to the frequency column for those who haven't had claims during exposure #
  merged_data$somme_quantite[is.na(merged_data$somme_quantite)] <- 0
  # Assign 0 to the cost column for those who haven't had claims during exposure #
  merged_data$somme_frais[is.na(merged_data$somme_frais)] <- 0
  # Detect all the columns that contain NA value and the number of them #
  if (verbose == TRUE)
  {detection_NA(merged_data)}
  
  # Delete primary key
  merged_data$pk <- NULL
  
  # Check the targets, negative values are listed and eliminated
  if (verbose == TRUE)
  {print("Eliminate observations whose target is negative")}
  merged_data <- eliminate_negative(merged_data, verbose = verbose)
  
  # Converting merged_data into data.table object
  merged_data <- data.table::data.table(merged_data)
  
  # convert somme_frois to cout_moyen
  merged_data$cout_moyen <- 0
  happened <- which(merged_data$somme_quantite != 0)
  merged_data$cout_moyen[happened] <- merged_data$somme_frais[happened]/merged_data$somme_quantite[happened]
  merged_data$somme_frais <- NULL
  
  # Return the processed clean dataset
  return(merged_data)
  }

# A function that checks and removes duplication of a loaded dataset #
check_dup <- function(DATASET, name, verbose = TRUE)
{
  size <- dim(DATASET)[1]
  dedup_size <- dim(unique(DATASET))[1]
  if (size == dedup_size)
  {
    size_pk <- length(levels(as.factor(DATASET$pk)))
    if (size_pk == size)
    {
      if (verbose == TRUE)
      {print(paste0("No duplication in ", name, "!"))}
      return(DATASET)
    }
    else
    {
      if (verbose == TRUE)
      {print(paste0("No duplication in ", name, "!", " But ", size - size_pk
                    , " duplicates with respect to pk!"))}
      return(DATASET)
    }
  }
  else if(size < dedup_size)
  {
    if (verbose == TRUE)
    {print("ERROR!")}
  }
  else if(size > dedup_size)
  {
    DATASET <- unique(DATASET)
    if (verbose == TRUE)
    {print(paste0(size-dedup_size, " duplication(s) have(s) been removed from "
                  , name, "!"))}
    return(DATASET)
  }
}

# A function that allows to show the schema of a dataframe with all variables converted #
# to factors #
show_data_as_factor <- function(x)
{
  if(length(levels(as.factor(x))) <= 50)
  {return(levels(as.factor(x)))}
  else
  {return(length(levels(as.factor(x))))}
}

fancy_scientific <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l)
  # return this as an expression
  parse(text=l)
}

# A function which returns the names of columns that contains NA value and the number of #
# NA values it contains #
detection_NA <- function(DATASET)
{
  f<- function(x)
  {return(length(which(is.na(x))))}
  raw <- sapply(DATASET, f)
  if(sum(raw) == 0)
  {print("None of the entry contains NA value!")}
  else
  {
    print("NA values detected:")
    print(raw[which(raw != 0)])
    return(raw[which(raw != 0)])
  }
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
    returnlist <- list('train.set' = train.set,'test.set' = test.set)
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
  n_set <- names(DATASET)[which(class != 'factor')]
  for(name in n_set)
  {
    levels <- unique(get(name,DATASET))
    if(identical(levels,c(1,0))|identical(levels,c(0,1)))
    { DATASET[[name]] <- factor(DATASET[[name]])}
  }
  return(DATASET)
}

# A function that detects negative responding values in a dataset
# As negative values are not allowed for the 'Poisson' family
# Those entries must be eliminated
eliminate_negative <- function(DATASET, verbose = TRUE)
{
  size_o <- dim(DATASET)[1]
  
  # Find the observations who contain negative value of somme_quantite
  quantite_to_eliminate <- which(DATASET$somme_quantite < 0)
  size_q <- length(quantite_to_eliminate)
  if (verbose == TRUE)
  {print(paste0(size_q," observations contain negative value in 'somme_quantite'!"))}
  
  # Find the observations who contain negative value of somme_frais
  frais_to_eliminate <- which(DATASET$somme_frais < 0)
  size_f <- length(frais_to_eliminate)
  if (verbose == TRUE)
  {print(paste0(size_f," observations contain negative value in 'somme_frais'!"))}
  
  # Merge to get the set of observations to eliminate from DATASET
  to_eliminate <- c(quantite_to_eliminate,frais_to_eliminate)
  if (length(to_eliminate) > 0)
  {DATASET <- DATASET[-to_eliminate,]}
  size <- dim(DATASET)[1]
  if (verbose == TRUE)
  {print(paste0(size_o - size, " observations are eliminated for containing negative value!"))}
  return(DATASET)
}

## EXTERNALS ###########################################################
########################################################################
# Gain Curve
# plots the gain (lorenz) curve for the given data and returns the gini index
plotgain <- function(predrisk, truerisk = NULL, exposure = NULL, nbpts = 1000,
                     significance = 2, normalize = F, return_val = F,
                     with_optimal = F)
{
  ## check argument value
  predrisk      = check_predrisk_gain(predrisk)
  truerisk      = check_truerisk_gain(predrisk, truerisk)
  exposure      = check_exposure_gain(predrisk, exposure)
  significance  = check_significance_gain(significance)
  nm_mod        = names(predrisk)
  
  ##plot
  #colors
  clrs = colorRampPalette(c("#3f88c5","#00b34d", "#ff3300","#ffd500","#7d00b3"))(length(predrisk))
  
  #other options
  op <- list()
  op$chart <- list(zoomType = "xy")
  op$title <- list(text = 'Gain Curve', x = -20)
  op$tooltip = F
  op$xAxis <- list(list(min           = 0,
                        max           = 1,
                        title         = list(text = "population"),
                        gridLineWidth = 0.5,
                        gridLineDashStyle =  'longdash'))
  op$yAxis <- list(list(min           = 0,
                        max           = 1,
                        title         = list(text = "risk"),
                        gridLineWidth = 0.5,
                        gridLineDashStyle =  'longdash'))
  op$series <- list(list(name = "bisector",
                         data   = list(c(0,0), c(1,1)),
                         marker = list(enabled = F),
                         showInLegend =  F,
                         lineWidth = 1,
                         color = "grey"))
  
  tit_legend = ifelse(normalize, "gini index (normalized)", "gini index")
  op$legend <- list(title = list(text = tit_legend,
                                 style = list("font-size"= "120%",
                                              "text-align" = "center")),
                    layout = 'vertical',
                    align = 'right',
                    verticalAlign = 'middle',
                    backgroundColor = '#FCFFC5',
                    borderColor = '#C98657',
                    itemStyle = list(fontWeight = "normal"))
  
  
  perfect_list = list()
  for(i in 1:length(predrisk))
  {
    gaincd   = get_gaincoord(predrisk[[i]], truerisk[[i]],
                             exposure[[i]], nbpts)
    perfect_gaincd = get_gaincoord(truerisk[[i]], truerisk[[i]],
                                   exposure[[i]], nbpts)
    
    perfect_list[[i]] = perfect_gaincd
    
    ## gini index
    add_gini = calc_gini(gaincd)
    if(normalize)
      add_gini = add_gini/calc_gini(perfect_gaincd)
    add_gini = signif(add_gini, significance)
    if(i == 1)
      gini = add_gini
    else
      gini = c(gini, add_gini)
    
    if(length(grep("^#[0-9]*$", nm_mod[i]) >=1))
      nm_serie = paste0(i, " (", add_gini,")")
    else
      nm_serie = paste0(i, ": ", nm_mod[i], " (", add_gini,")")
    add_series <- list(list(name   = nm_serie,
                            data   = hc_listpts(gaincd[["x"]], gaincd[["y"]]),
                            marker = list(enabled = F),
                            color  = clrs[i]))
    op$series = append(op$series, add_series)
  }
  ## perfect list cleanup
  if(with_optimal)
  {
    names(perfect_list) = nm_mod
    ret_list            = group_identicals(perfect_list)
    perfect_list        = ret_list[['perfect_list']]
    colors_ID           = ret_list[['colors_ID']]
    
    for(k in 1:length(perfect_list))
    {
      nms = names(perfect_list)
      coord = perfect_list[[k]]
      colID = colors_ID[k]
      
      perfect_gini = signif(calc_gini(coord), significance)
      nm_serie = paste0("optimal_",nms[k], " (", perfect_gini,")")
      add_series <- list(list(name   = nm_serie,
                              data   = hc_listpts(coord[["x"]], coord[["y"]]),
                              marker = list(enabled = F),
                              color = clrs[colID],
                              dashStyle = "ShortDot"))
      op$series = append(op$series, add_series)
    }
  }
  
  ## plot
  plt = highcharter::highchart(op)
  print(plt)
  
  if(return_val)
  {
    ret = list(plot = plt, gini = gini)
    return(ret)
  }
}


# Gini Index
# Calculate the AUC for a binary classifier
kpi_gini <- function(predrisk, truerisk = NULL, exposure = NULL, significance = 2,
                     normalize = F, nbpts = 1000)
{
  predrisk      = check_predrisk_gain(predrisk)
  truerisk      = check_truerisk_gain(predrisk, truerisk)
  exposure      = check_exposure_gain(predrisk, exposure)
  significance  = check_significance_gain(significance)
  nm_mod        = names(predrisk)
  
  gini <- sapply(1:length(predrisk), function(i){
    gaincd = get_gaincoord(predrisk[[i]], truerisk[[i]],
                           exposure[[i]], nbpts)
    return(calc_gini(gaincd))
  })
  
  if(normalize)
  {
    giniPerfect <- sapply(1:length(predrisk), function(i){
      gaincd = get_gaincoord(truerisk[[i]], truerisk[[i]], exposure[[i]],
                             nbpts)
      return(calc_gini(gaincd))
    })
    gini = gini / giniPerfect
  }
  
  gini = signif(gini, significance)
  names(gini) = nm_mod
  
  return(gini)
}


### INTERNALS ##########################################################
########################################################################

## non-vectorized function for calculating the (x,y) coordinates of a
## gain curve given thre predicted risk, true risk and exposure
get_gaincoord <- function(predrisk_vec, truerisk_vec, exposure_vec,
                          nbpts = 1000)
{
  ordpred = order(predrisk_vec, decreasing = T)
  cumrisk = c(0, cumsum(truerisk_vec[ordpred])/sum(truerisk_vec))
  
  if(is.null(exposure_vec))
  {
    x        = (1:nbpts)/nbpts
    y        = quantile(cumrisk, x)
    names(y) = NULL
  }
  else
  {
    cumexpo = c(0, cumsum(exposure_vec[ordpred])/sum(exposure_vec))
    ind     = sample(x = 1:length(cumrisk), size = nbpts)
    ind     = ind[order(ind)]
    x       = cumexpo[ind]
    y       = cumrisk[ind]
  }
  return(list(x = x, y = y))
}


## non-vectorized function for calculating the gini index given
## (x,y) coordinates of the gain curve
calc_gini <- function(x, y = NULL, significance = NULL)
{
  if(is.null(y))
  {
    temp = x
    x = temp[[1]]
    y = temp[[2]]
  }
  gini = (pracma::trapz(x, y)-0.5)/0.5
  if(is.null(significance))
    return(gini)
  else
    return(signif(gini, significance))
}

### CHECKARGS ##########################################################
########################################################################

check_significance_gain <- function(significance)
{
  if(!is.numeric(significance))
    stop("significance should be a numeric")
  if(length(significance) != 1)
    stop("significance should be of length 1")
  if(as.integer(significance) != significance)
    warning("converting significance to integer")
  significance = as.integer(significance)
  if(!(significance >=0))
    stop("significance should be a positive integer")
  
  return(significance)
}

check_predrisk_gain <- function(predrisk)
{
  ## check predrisk
  if(class(predrisk)[1] != "list" & !is.numeric(predrisk))
    stop("predrisk should be of type list or numeric")
  if(is.numeric(predrisk))
    predrisk = list(predrisk)
  for(i in 1:length(predrisk))
  {
    if(!is.numeric(predrisk[[i]]))
      stop(paste0("predrisk[[", i, "]] is not numeric"))
    if(length(predrisk[[i]]) == 0)
      stop(paste0("predrisk[[", i, "]] is of length 0"))
  }
  
  ## check model name
  if(is.null(names(predrisk)))
    names(predrisk) = paste0("#", 1:length(predrisk))
  else if(length(grep("^$", names(predrisk))) > 0)
    for(i in grep("^$", names(predrisk)))
      names(predrisk)[i] = paste0("#",i)
    
    return(predrisk)
}

check_truerisk_gain <- function(predrisk, truerisk)
{
  ## check truerisk
  if(is.null(truerisk))
  {
    truerisk = predrisk
    return(truerisk)
  }
  if(class(truerisk)[1] != "list" & !is.numeric(truerisk))
    stop("truerisk should be of type list or numeric")
  if(is.numeric(truerisk))
    truerisk = list(truerisk)
  if(length(truerisk) > length(predrisk))
    stop("length(truerisk) > length(predrisk)")
  else if(length(truerisk) < length(predrisk))
  {
    if(length(truerisk) == 1)
      for(i in 2:length(predrisk))
        truerisk[[i]] = truerisk[[1]]
      else
        stop("length of truerisk > 1 but doesnt match the length of predrisk")
  }
  
  for(i in 1:length(truerisk))
  {
    if(!is.numeric(truerisk[[i]]))
      stop(paste0("truerisk[[", i, "]] is not numeric"))
    if(length(truerisk[[i]]) != length(predrisk[[i]]))
      stop(paste0("length(predrisk[[", i, "]]) != length(truerisk[[",i, "]])"))
  }
  
  ## check model name
  if(!is.null(names(truerisk)))
  {
    warning("names are not considered for argument 'truerisk'. Matching with
            'predrisk' is done only by order.")
  }
  
  return(truerisk)
  }

check_exposure_gain <- function(predrisk, exposure)
{
  ## check exposure
  if(is.null(exposure))
    return(exposure)
  if(class(exposure)[1] != "list" & class(exposure)[1] != "numeric")
    stop("exposure should be of type list or numeric")
  if(is.numeric(exposure))
    exposure = list(exposure)
  if(length(exposure) > length(predrisk))
    stop("length(exposure) > length(predrisk)")
  else if(length(exposure) < length(predrisk))
  {
    if(length(exposure) == 1)
      for(i in 2:length(predrisk))
        exposure[[i]] = exposure[[1]]
      else
        stop("length of exposure > 1 but doesnt match the length of predrisk")
  }
  for(i in 1:length(exposure))
  {
    if(!is.numeric(exposure[[i]]))
      stop(paste0("exposure[[", i, "]] is not numeric"))
    if(length(exposure[[i]]) != length(predrisk[[i]]))
      stop(paste0("length(predrisk[[", i, "]]) != length(exposure[[",i, "]])"))
    if(!all(exposure[[i]] >= 0 & exposure[[i]] <= 1))
      stop(paste0("some elements of exposure[[", i, "]] are not in [0,1]"))
  }
  
  ## check model name
  if(!is.null(names(exposure)))
  {
    warning("names are not considered for argument 'exposure'. Matching with
            'predrisk' is done only by order.")
  }
  
  return(exposure)
  }


#' Group levels
#'
#' Replace old levels in a factor with the new grouped levels
#'
#' Steps:
#'
#' 1. Create a data table that contains all the reduced features extracted from nfeature_creation
#' (all numerical features) in order to cluster on the levels of group_factor
#'
#' 2. Merge the second data table that contains all the reduced features extracted from cfeature_creation
#' (all categorical features) with the first data table
#'
#' 3. Given the training data set and its labels, a vector with all the labels with thier clustered tag
#' is returned
#'
#' @param dt [data.table] Input data table, ready to be modolized
#' @param group_factor [character]  a character that indicates the factor on whose levels
#' we want to perform clustering
#' @param loose_ends [character] features that are in numeric or integer format however
#' should be processed as factors
#' @param frequency [character] a string of the name of the frequency(predictive target)
#' @param exposure [character] default set to 'POL_Exp', a string of the name of the exposure
#' @param treecut [integer] default set to 5, the maximum number of non-leaf nodes selected
#' from the regression tree
#' @param cp [numeric] default set to 0.01, the complexity parameter in regression tree,
#' which introduce the penalty of the size of the tree
#' @param col [numeric] default set to 0.99, any correlation above this quantity between
#' two vectors, one of the vector will be removed
#' @param gbm.rank [logical] A logical object to determine whether the importance ranking realized
#' by gbm algorithm will be performed
#' @param nbclusters [integer] The number of clusters returned from hierarchical clustering
#' @param bulk [numeric] The percentage of variance to retain in Singular Value Decomposition
#' @param pc [integer] The number of principle components retained
#' @param verbose [logical] if TRUE, group_levels will print out progress and performance indication.
#' If this option is left unspecified for gbm.more then it uses verbose from object.
#'
#' @return return_column [factor] The grouped factor with new levels
#' @export


group_levels <- function(dt, group_factor, loose_ends = NULL, frequency, exposure, verbose = T
                         , treecut = 5, cp = 0.01, col = 0.98, gbm.rank = T, nbclusters = 30
                         , bulk = 0.6, pc = 3)
{
  ##################################### split the data table into two parts ####################################
  # Specify all the categorical and numerical variables
  # Convert numerical variables into factors (in loose_ends)
  # Return list contains three data table: Categorical Variables, Numerical Variables
  # and Grouping Factor
  split <- function(dt, group_factor, loose_ends)
  {
    varchars = names(dt)[which(sapply(dt,is.character))]
    if (!is.null(loose_ends))
      varchars <- append(varchars,loose_ends)
    for (j in varchars) data.table::set(dt,j=j,value = as.factor(dt[[j]]))
    integers <- names(dt)[which(sapply(dt,is.integer))]
    for (j in integers) data.table::set(dt,j=j,value = as.numeric(dt[[j]]))
    # Catagorical variables
    c_name = names(dt)[which(sapply(dt,is.factor))]
    c_variables = dt[,c_name, with = F]
    # Numeric and integer variables
    n_name = names(dt)[which(!sapply(dt,is.factor))]
    n_variables = dt[,n_name, with = F]
    group_factor <- c_variables[,group_factor,with = F]
    return_list <- list('c_variables' = c_variables,'n_variables' = n_variables
                        ,'group_factor' = group_factor)
    return(return_list)
  }
  ##############################################################################################################
  
  
  
  ######################################## Numerical features creation #########################################
  # Generate statistical features from Numerical variables
  nfeature_creation <- function(column,group_factor,index)
  {
    name = names(column)
    # create data table object for the prepration of training set
    column[[names(group_factor)]] <- group_factor[[names(group_factor)]]
    column <- data.table::data.table(column)
    dt = column[,.(mean(get(name),na.rm=T)
                   ,sd(get(name),na.rm=T)
                   ,e1071::skewness(get(name),na.rm=T)
                   ,e1071::kurtosis(get(name),na.rm=T)
                   ,quantile(get(name),.01,na.rm=T)
                   ,quantile(get(name),.1,na.rm=T)
                   ,quantile(get(name),.25,na.rm=T)
                   ,quantile(get(name),.5,na.rm=T)
                   ,quantile(get(name),.75,na.rm=T)
                   ,quantile(get(name),.9,na.rm=T)
                   ,quantile(get(name),.99,na.rm=T)
                   ,length(unique(get(name)))/length(get(name))
                   ,as.double(median(get(name)))
                   ,as.double(mad(get(name),na.rm=T))
                   ,as.double(min(get(name),na.rm=T))
                   ,as.double(max(get(name),na.rm=T))
                   ,sd(get(name),na.rm=T)/sqrt(length(get(name)))
                   ,.N)
                ,by = group_factor]
    
    names(dt) <- c(names(group_factor)
                   ,paste0(index,'m')
                   ,paste0(index,'sd')
                   ,paste0(index,'sk')
                   ,paste0(index,'kt')
                   ,paste0(index,'q01')
                   ,paste0(index,'q10')
                   ,paste0(index,'q25')
                   ,paste0(index,'q50')
                   ,paste0(index,'q75')
                   ,paste0(index,'q90')
                   ,paste0(index,'q99')
                   ,paste0(index,'uq')
                   ,paste0(index,'med')
                   ,paste0(index,'mad')
                   ,paste0(index,'min')
                   ,paste0(index,'max')
                   ,paste0(index,'sem')
                   ,paste0(index,'N'))
    
    return(dt)
  }
  ##############################################################################################################
  
  
  
  ####################################### Categorical features creation ########################################
  cfeature_creation <- function(column,group_factor,index)
  {
    # Shannon entropy calculation
    entropy <- function(p)
    {
      # if (min(p) < 0 || sum(p) <= 0)
      #  return(NA)
      p.norm <- p[p>0]/sum(p)
      -sum(log2(p.norm)*p.norm)
    }
    # Replace all the NA by zero in a data frame
    f_dowle <- function(dt)
    {
      for (j in names(dt))
        set(dt,which(is.na(dt[[j]])),j,0)
      for (j in seq_len(ncol(dt)))
        set(dt,which(is.na(dt[[j]])),j,0)
    }
    
    # Replace all the empty column name by V1, V2, ...
    r_empty <- function(cols)
    {
      ncols <- c()
      index <- 1
      for (name in cols)
      {
        if (name == '')
        {
          ncols <- c(ncols,paste0('V',index))
          index <- index + 1
        }
        else
          ncols <- c(ncols,name)
      }
      return(ncols)
    }
    temp <- data.table::data.table(c(column,group_factor))
    dt <- table(group_factor[[names(group_factor)]],column[[names(column)]])
    dt <- as.data.frame.matrix(dt)
    # Assign column names
    cols <- levels(column[[names(column)]])
    # Replace empty column names by 'V1', 'V2', etc...
    ncols <- paste0(index,r_empty(cols))
    names(dt) <- ncols
    # Normalize the data frame row-wise
    dt <- sweep(dt, 1, rowSums(dt), FUN="/")
    # Assign the standard deviation of the distribution to the column standd
    standd <- c()
    for (i in 1:dim(dt)[1]) {standd <- c(standd,sd(dt[i,]))}
    # Assign the shannon entropy of the distribution to the column sentropy
    sentropy <- c()
    for (i in 1:dim(dt)[1])
    {
      sentropy <- c(sentropy,entropy(dt[i,]))
    }
    dt[,paste0(index,'sd')] <- standd
    dt[,paste0(index,'entropy')] <- sentropy
    dt <- data.table::data.table(dt)
    return(dt)
  }
  ##############################################################################################################
  
  
  
  ############################################ feature selection ###############################################
  feature_selection <- function(sdt,treecut,cp)
  {
    mycontrol <- rpart::rpart.control(cp = cp, xval = 10)
    tree = rpart::rpart(formula = 100*(sdt$tgt-mean(sdt$tgt))~.
                 ,data = sdt,control = mycontrol)
    split = as.character(unique(tree$frame$var))
    split = split[split != '<leaf>']
    split = split[c(1:treecut)]
    split <- split[!is.na(split)]
    temp = sdt[,split,with=F]
    #  temp[['VEH_Brand']] = sdt$VEH_Brand
    #  index = c(1:length(sdt$tgt))
    #  temp[['index']] = index
    return(temp)
  }
  ##############################################################################################################
  
  
  
  ########################################### Training set creation ############################################
  get_training_set <- function(n_variables, c_variables, group_factor, frequency, exposure = exposure
                               , treecut=treecut, cp = cp, col = col, gbm.rank = gbm.rank, verbose = verbose)
  {
    ### Remove strong correlation
    remove.col <- function(combined_features,group_factor, col, verbose)
    {
      cor <- cor(combined_features[,-names(group_factor),with = F])
      cor[upper.tri(cor)] <- 0
      diag(cor) <- 0
      new_data <- !apply(cor,2,function(x){any(x>col)})
      new_data <- c(TRUE,new_data)
      combined_features <- combined_features[,new_data,with = F]
      if (verbose == T)
      {
        return_list = paste('Features left after removing high correlations among them are: ')
        cat(return_list,'\n',names(combined_features[,-names(group_factor),with =F]),'\n')
      }
      return(combined_features)
    }
    ### normalize function
    # Normalize with respect to the importance that returned by gbm algorithm
    normalize <- function(combined_features, tgt, group_factor, n.trees, verbose)
    {
      combined_features[['tgt']] = tgt[['tgt']]
      combined_features[[names(group_factor)]] <- NULL
      mygbm <- gbm::gbm(formula = combined_features$tgt ~ ., data = combined_features, interaction.depth = 3
                   , distribution = 'gaussian', shrinkage = 0.001, bag.fraction = 0.5, n.trees = 100, 
                   verbose = verbose)
      if (verbose == T)
      {
        print(summary(mygbm))
      }
      combined_features$tgt <- NULL
      # Get the ordered features(by its importance) and the importance vector
      features <- summary(mygbm)[,1]
      importance <- summary(mygbm)[,2]
      # Normalize coloum-wise the data set with respect to its importance
      len <- length(combined_features)
      for (i in 1:len)
      {
        column <- combined_features[[features[i]]]
        combined_features[[features[i]]] <- ((column-mean(column))/sd(column))
      }
      # Remove all zero columns
      combined_features <- combined_features[,colSums(combined_features != 0) > 0,with = F]
      if (verbose == T)
      {
        returnlist <- ' Features left after removing all zero columns are: '
        len <- length(names(combined_features))
        cat(len, return_list,'\n', names(combined_features),'\n')
      }
      return(combined_features)
    }
    # Given one group_factor, the target of all the regression trees that we would run on
    # each numerical features is the same.
    # Prepare the target for the regression tree
    target <- n_variables[,c(frequency,exposure),with=F]
    names(target) <- c('frequency','exposure')
    target[[names(group_factor)]] <- group_factor[[names(group_factor)]]
    tgt = target[,sum(get('frequency'),na.rm=T)/sum(get('exposure'),na.rm=T),by=group_factor]
    #target$tgt <- target$frequency*target$exposure
    #tgt = target[,sum(tgt,na.rm = T),by=group_factor]
    names(tgt) <- c(names(group_factor),'tgt')
    tgt[[names(group_factor)]] = NULL
    
    # Remove the target from the generating matrix
    n_variables[[frequency]] = NULL
    n_variables[[exposure]] = NULL
    
    # Initialize the training data for final grouping
    index = 1
    combined_features = group_factor[,.('i'=1),by = names(group_factor)]
    combined_features[['i']] = NULL
    names(combined_features) <- names(group_factor)
    
    for (column in n_variables)
    {
      column <- data.table::data.table(column)
      
      # create data table s_table by merging column and bonus
      # s_table contains exposure, risk frequency and one feature from n_variables
      # at each iteration
      #    s_table <- bonus
      #    s_table[[names(column)]] = column[[names(column)]]
      # data table with features extracted from one numeric feature
      # 19 features ready to be trimmed
      sdt = nfeature_creation(column = column,group_factor=group_factor, index = index)
      sdt[['tgt']] = tgt[['tgt']]
      sdt[[names(group_factor)]] = NULL
      # pass dt to the function nfeature_reduction
      # returns the reduced features considered to be correlated with risk
      dt = feature_selection(sdt,treecut,cp = cp)
      if (length(dt) != 0)
      {
        dt[[names(group_factor)]] <- combined_features[[names(group_factor)]]
        # show the features selected at each stage
        if (verbose == T)
        {
          return_list = paste('Features selected from the ', names(n_variables)[index],' variable: ')
          len <- length(dt) - 1
          cat(len,return_list,c(names(dt))[!c(names(dt)) %in% names(group_factor)],'.','\n')
        }
        # merge the features selected into the training data for final grouping 'combined_features'
        combined_features <- merge(combined_features,dt,by = names(group_factor),sort = F)
      }
      else
      {
        if (verbose == T)
        {
          return_list = paste('No feature was selected from the ',names(n_variables)[index],' variable.')
          cat(return_list,'\n')
        }
      }
      index <- index + 1
    }
    name_count <- 1
    index <- 1
    cname <- names(c_variables)
    for (column in c_variables)
    {
      column <- data.table::data.table(column)
      names(column) <- cname[name_count]
      name_count <- name_count + 1
      if (names(column) != names(group_factor))
      {
        sdt = cfeature_creation(column = column,group_factor=group_factor, index = index)
        sdt[['tgt']] = tgt[['tgt']]
        # pass dt to the function nfeature_reduction
        # returns the reduced features considered to be correlated with risk
        dt = feature_selection(sdt,treecut,cp = cp)
        if (length(dt) != 0)
        {
          dt[[names(group_factor)]] <- combined_features[[names(group_factor)]]
          # show the features selected at each stage
          if (verbose == T)
          {
            return_list = paste('Features selected from the ', names(column),' variable: ')
            len <- length(dt) - 1
            cat(len,return_list,c(names(dt))[!c(names(dt)) %in% names(group_factor)],'.','\n')
          }
          # merge the features selected into the training data for final grouping 'combined_features'
          combined_features <- merge(combined_features,dt,by = names(group_factor),sort = F)
        }
        else
        {
          if (verbose == T)
          {
            return_list = paste('No feature was selected from the ',names(c_variables)[index],' variable.')
            cat(return_list,'\n')
          }
        }
        index <- index + 1
      }
    }
    # Remove strong correlations from combined_features
    combined_features <- remove.col(combined_features, group_factor, col, verbose = verbose)
    # Normalize the data table combined_features for the sake of builiding distance
    # (or similarity) matrixs
    if (gbm.rank)  {combined_features <- normalize(combined_features, verbose = verbose
                                                   , tgt, group_factor, 3*length(combined_features))}
    combined_features[[names(group_factor)]] <- group_factor[,.('i'=1)
                                                             ,by = names(group_factor)][[names(group_factor)]]
    # Return combined_features as the final training dataset
    return(combined_features)
  }
  ###############################################################################################################
  
  
  
  ########################################### Clustering function ###############################################
  clustering <- function(combined_features, group_factor, method = 'hclust', nbclusters = nbclusters
                         , bulk = bulk, pc = pc, verbose = verbose)
  {
    #combined_features = preProcess(combined_features,method=c('BoxCox','center','scale','pca'))
    # Convert combined_features into data frame thus add row names as the VEH_Brand
    combined_features = data.frame(combined_features, row.names=
                                     as.vector(combined_features[[names(group_factor)]]))
    principal.component <- function(pca, bulk)
    {
      sdev <- pca$sdev
      len <- length(sdev)
      s <- sum(sdev)
      for (i in 1:len)
      {
        if (sum(sdev[1:i])/s >= bulk)
        {
          return(i)
          break
        }
      }
    }
    # Hierarchical clustering much depends on the distance matrix which could be easily
    # dominated by the biggiest number in the vector
    if (method == 'hclust')
    {
      dup <- combined_features
      dup[names(group_factor)] <- NULL
      pca <- prcomp(dup,center = T, scale. = T)
      index <- principal.component(pca, bulk = bulk)
      appr <- pca$x[,c(1:min(pc,index))]
      # Create the distance object from combined_features except for VEH_Brand for clustering
      dist <- dist(appr)
      # Apply the hierarical clustering
      hc <- hclust(dist, method = 'complete')
      # Choose the number of times of grouping
      clusterCut <- cutree(hc, nbclusters)
      #table(clusterCut,combined_features$get(names(group_factor)))
      if (verbose == T)
      {
        for (i in c(1:nbclusters))
        {
          temp = as.vector(combined_features[[names(group_factor)]])[unname(clusterCut) == i]
          cat('The ', i, suffix(i),'cluster contains',length(temp),'element(s)','\n')
        }
      }
      return(clusterCut)
    }
  }
  ###############################################################################################################
  
  
  
  ################################################ Suffix function ##############################################
  suffix <- function(index)
  {
    if (index%%10 == 1)
    {
      if (index%%100 == 11)
        suffix = 'th'
      else
        suffix = 'st'
    }
    else if (index%%10 == 2)
    {
      if (index%%100 == 12)
        suffix = 'th'
      else
        suffix = 'nd'
    }
    else if (index%%10 == 3)
    {
      if (index%%100 == 13)
        suffix = 'th'
      else
        suffix = 'rd'
    }
    else
      suffix = 'th'
    return(suffix)
  }
  ###############################################################################################################
  
  
  
  ################################################ Replace levels ###############################################
  group_factors <- function(clustercut, column)
  {
    new.levels <- c()
    for (i in levels(column))
    {
      new.level <- paste0('level',unname(clustercut[i]))
      new.levels <- append(new.levels,new.level)
    }
    levels(column) <- new.levels
    return(column)
  }
  ###############################################################################################################
  
  return_list <- split(dt, group_factor = group_factor, loose_ends = loose_ends)
  n_variables <- return_list$n_variables
  c_variables <- return_list$c_variables
  group_factor <- return_list$group_factor
  combined_features <- get_training_set(n_variables = n_variables, c_variables = c_variables
                                        , group_factor = group_factor, frequency = frequency
                                        , exposure = exposure, treecut = treecut, cp = cp
                                        , col = col, gbm.rank = gbm.rank, verbose = verbose)
  clustercut = clustering(combined_features,group_factor,nbclusters = nbclusters, bulk = bulk
                          ,verbose = verbose, pc = pc)
  return_column <- group_factors(clustercut, group_factor[[names(group_factor)]])
  
  return(return_column)
}

############################ Functions and tests ############################ 


# INTERNALS #########################################################
#####################################################################

## area under curve
trapz <- function(x,y)
{
  # x and y must be vectors
  idx = 2:length(x)
  return (((x[idx] - x[idx-1]) %*% (y[idx] + y[idx-1])) / 2)
}

custom_summarise <- function(tbl, v, args, model)
{
  c0 = args$type == "summary"
  if(is.null(v)){c1 = args$global}else{c1 = T}
  if(is.null(v) || !v %in% model$facpred){c2 = !args$fim}else{c2 = T}
  
  summ_args = args$formula[c0 & c1 & c2]
  
  if(length(summ_args) == 0)
    stop("error: no summary statistics provided")
  
  summ_args = paste0(summ_args, collapse = ", ")
  
  str <- paste0("tbl %>% dplyr::summarise(", summ_args,")")
  
  tbl <- eval(parse(text = str))
  ## transform the table var column to a correctly-ordered factor
  if(!is.null(v))
    tbl[[v]] = w_tidyFactor(tbl[[v]], NA_as_level = T)
  
  return(tbl)
}

custom_add <- function(tbl, v, args, add_tbls, DT, model, predictions, envStats)
{
  c0 = args$type == "add"
  if(is.null(v)){c1 = args$global}else{c1 = T}
  if(is.null(v) || !v %in% model$facpred){c2 = !args$fim}else{c2 = T}
  
  add_args = args$formula[c0 & c1 & c2]
  
  ## add tables to add_tbls list
  if(length(add_args) > 0)
    for(aa in add_args)
      add_tbls[[length(add_tbls)+1]] = eval(parse(text = aa))
  
  if(length(add_tbls) == 0)
    return(tbl)
  
  ## flatten the list of lists of tables to a list of tables
  flist = flatten_tblist(add_tbls)
  
  ## add tables
  ## global criterion: first col not named levels and nrow = 1
  if(!is.null(v))
  {
    for(i in 1:length(flist))
      if(v %in% names(flist[[i]]))
        tbl = merge(x = tbl, y = flist[[i]], by = v, all.x = T)
  }
  else
  {
    for(i in 1:length(flist))
      if(nrow(flist[[i]]) == 1)
        tbl = cbind(tbl, flist[[i]])
  }
  
  return(tbl)
}

## this mutate function allows to refer to a base level for a given
## variable, in a dplyr::mutate call
custom_mutate <- function(tbl, v, args, base_levels, order_by, order, model)
{
  c0 = args$type == "mutate"
  if(is.null(v)){c1 = args$global} else{c1 = T}
  if(is.null(v) || !v %in% model$facpred){c2 = !args$fim}else{c2 = T}
  
  mutate_args = args$formula[c0 & c1 & c2]
  
  if(length(mutate_args) == 0)
    return(tbl)
  
  ##replace true base levels
  if(!is.null(v))
    mutate_args = gsub(pt_blvlind, base_levels[[v]], mutate_args)
  
  mutate_args = paste0(mutate_args, collapse = ", ")
  
  tbl = tryCatch({eval(parse(text = paste0("tbl %>% dplyr::mutate(", mutate_args,")")))},
                 error = function(e){
                   e = gsub("^.*:", "", e)
                   stop(paste0("Error in mutate:", e, ". Check validity of argument 'base_levels'."))
                 })
  
  return(tbl)
}

rename_col1_to_levels <- function(tbl)
{
  data.table::setnames(tbl, names(tbl)[1], "levels")
  return(tbl)
}

arrange_rows <- function(tbl, v, order_by, order)
{
  ## handling order_by and order arguments
  if(is.null(order_by))
    ordvar = "levels"
  else if(!order_by %in% names(tbl))
    ordvar = "levels"
  else
    ordvar = order_by
  
  if(order == -1)
    tbl = tbl[rev(order(tbl[[ordvar]]))]
  else
    tbl = tbl[order(tbl[[ordvar]])]
  
  return(tbl)
}

arrange_columns <- function(tbl)
{
  data.table::setDT(tbl)
  
  currcols = colnames(tbl)
  ord      = all_stats$ordercols
  exrem = gsub("\\|.*", "", currcols)
  final = c(ord[ord %in% exrem], exrem[!exrem %in% ord])
  vecwithmod = unique(gsub("\\|.*", "", currcols[grep("\\|.*", currcols)]))
  
  if(length(vecwithmod) > 0)
    for(v in vecwithmod)
    {
      toputin = ex[grep(paste0(v, "\\|"), ex)]
      final = replaceInVec(vec = final, what = v, replacement = toputin)
    }
  data.table::setcolorder(tbl, final)
  return(tbl)
}


flatten_tblist <- function(add_tbls)
{
  
  ret = list()
  if(length(add_tbls) == 0)
    return(ret)
  
  for(i in 1:length(add_tbls))
  {
    if(is.list(add_tbls[[i]]) & !is.data.frame(add_tbls[[i]]))
    {
      for(elem in add_tbls[[i]])
      {
        if(is.data.frame(elem))
          ret[[length(ret) + 1]] = elem
        else
          stop("add_tbls[[", i,"]] contains non-data-frame elements")
      }
    }
    else if(is.data.frame(add_tbls[[i]]))
      ret[[length(ret) + 1]] = add_tbls[[i]]
    else
      stop("objects in add_tbls argument should be tables or list of tables")
  }
  
  return(ret)
}

get_baselvl_index <- function(tbl, v, base_levels, order_by, order)
{
  if(is.null(base_levels[[v]]))
  {
    if(is.null(order_by))
      ordvar = v
    else if(!order_by %in% names(tbl))
      ordvar = v
    else
      ordvar = order_by
    
    if(order == -1)
      base_level_index = which(order(tbl[[ordvar]]) == length(tbl[[ordvar]]))
    else
      base_level_index = which(order(tbl[[ordvar]]) == 1)
  }
  else
    base_level_index = which(tbl[[v]] == base_levels[[v]])
  
  return(base_level_index)
}


group_identicals <- function(lt)
{
  new_lt = list(lt[[1]])
  names(new_lt)[1] = "1"
  colors_ID = 1
  if(length(lt) > 1)
  {
    for(i in (2:length(lt)))
    {
      found_identical = F
      for(k in 1:length(new_lt))
        if(identical(lt[[i]], new_lt[[k]]))
        {
          names(new_lt)[k] = paste0(names(new_lt)[k], "&", i)
          found_identical = T
        }
      if(!found_identical)
      {
        new_lt[[length(new_lt)+1]] = lt[[i]]
        names(new_lt)[length(new_lt)] = i
        colors_ID = c(colors_ID, i)
      }
    }
  }
  
  return(list(perfect_list = new_lt, colors_ID = colors_ID))
}

##### ERROR MESSAGES
# taken from link =
# https://github.com/rstudio/addinexamples/blob/master/R/utils.R
isErrorMessage <- function(object) {
  inherits(object, "error_message")
}

errorMessage <- function(type, message) {
  structure(
    list(type = type, message = message),
    class = "error_message"
  )
}


muffleWarning <- function(expr, warnings = NULL)
{
  h <- function(w){
    w <- as.character(w)
    for(wns in warnings)
      if(grepl(pattern = wns, x = w))
        invokeRestart("muffleWarning")
  }
  withCallingHandlers(eval(expr), warning = h)
}


# is_ ###############################################################
#####################################################################

## check if variable is in DT
is_notindt <- function(DT, vars, type_message = NULL,
                       message_suffix = "", verbose = T)
{
  message_function = get_message_function(type_message)
  
  if(is.null(vars))
  {
    if(verbose)
      message_function(paste0("variable NULL is not in DT.", message_suffix))
    return(T)
  }
  
  ret = sapply(vars, function(v){
    pb = !(v %in% names(DT))
    if(pb & verbose)
      message_function(paste0("variable '", v, "' is not in DT.", message_suffix))
    return(pb)
  })
  
  return(ret)
}

## check if a variable has unique values or a single value in a dataset
is_single <- function(DT, vars = NULL, type_message = NULL,
                      message_suffix = "", verbose = T)
{
  message_function = get_message_function(type_message)
  if(is.null(vars))
    vars = names(DT)
  
  ret = sapply(vars, function(v){
    pb = length(unique(DT[[v]])) == 1
    if(pb & verbose)
      message_function(paste0("single value for variable '", v, "'", message_suffix))
    return(pb)
  })
  
  return(ret)
}

## check if a variable has unique values for each row of the dataset
is_unique <- function(DT, vars = NULL, type_message = NULL,
                      message_suffix = "", verbose = T,
                      thresh_unique = 0.99)
{
  message_function = get_message_function(type_message)
  if(is.null(vars))
    vars = names(DT)
  
  ret = sapply(vars, function(v){
    pb = length(unique(DT[[v]]))/nrow(DT) >= thresh_unique
    if(pb & verbose)
      message_function(paste0("> ",thresh_unique,
                              "% of unique values for variable '", v, "'", message_suffix))
    return(pb)
  })
  
  return(ret)
}


## util function to get a specific message function
get_message_function <- function(type_message)
{
  if(is.null(type_message))
    message_function = function(...){}
  else if(type_message == "error")
    message_function = base::stop
  else if(type_message == "warning")
    message_function = base::warning
  else if(type_message == "message")
    message_function = base::message
  else
    message_function = function(...){}
  
  return(message_function)
}

# formula parsing #####################################################
#######################################################################




# regex ###############################################################
#######################################################################

## pattern to detec valid names in R
pt_valid = "((\\.[[:alpha:]])|[[:alpha:]])([[:alnum:]]|_|\\.)*"
pt_valid_quoted = "('|\")((\\.[[:alpha:]])|[[:alpha:]])([[:alnum:]]|_|\\.)*('|\")"
#valid = c("bb", ".ala", ".ala_za3", ".a3", "bb34", "bb45_yu", "bala.fri")
#invalid = c(".", "3a", "_olo", ".3z")
#grep(pt_valid, invn)

pt_blvlind = "\\.blvl_ind"
#valid = c("[.blvl_ind]", "(.blvl_ind)", " .blvl_ind 2", ".blvl_ind34 + a[.blvl_ind]")
#invalid = c("ablvl_ind", ".blvl_ind", "blvl_ind_qsd", "blvl_ind3", "blvl_ind.")
#gsub(pt_blvlind, "@@",valid)

# EXTERNALS ####################################################################
################################################################################


# Factor Manipulation ###############################################
#####################################################################

#' get boolean vector of NAs in factor
#'
#' Works as is.na() but more flexible.
#'
#' Useful for detecting NAs after NA has been set as a level using addNA
#'
#' @param f factor
#' @export
w_isNA <- function(f)
{
  is.na(as.character(f))
}

#' Tidy the levels of a factor
#'
#' Does 3 potential operations:
#' - order the levels of the factor, putting numerics in numerical order
#' first then characters in alphabetical order
#' - removes levels that are missing in the factor
#' - add NA as a level of the factor
#' Based on w_ordLvlFactor and w_cleanLvlFactor
#'
#' @param f factor
#' @param NA_as_level [boolean] is NA is to be set as a level if the
#' factor contains NAs
#' @param ord [boolean] if levels should be ordered as described above
#' @param clean [boolean] if missing levels should be removed
#'
#' @export
w_tidyFactor <- function(f, NA_as_level = F, ord = T, clean = T)
{
  if(ord)
    f = w_ordLvlFactor(f)
  if(clean)
    f = w_cleanLvlFactor(f)
  if(NA_as_level)
    f = addNA(f, ifany = T)
  
  return(f)
}

#' Order the levels of a factor
#'
#' Order the levels of the factor, putting numerics in numerical order
#' first then characters in alphabetical order
#'
#' @param f factor
#'
#' @export
w_ordLvlFactor <- function(f)
{
  if(!is.factor(f))
    f = factor(f)
  
  lvls = levels(f)[!is.na(levels(f))]
  isnum = suppressWarnings(!is.na(as.numeric(lvls)))
  num_lvls = lvls[isnum]
  oth_lvls = lvls[!isnum]
  nlvls = c(num_lvls[order(as.numeric(num_lvls))],
            oth_lvls[order(oth_lvls)])
  
  ret = factor(f, levels = nlvls)
  
  if(NA %in% levels(f))
    ret = addNA(ret, ifany = T)
  
  return(ret)
}

#' Clean the levels of a factor
#'
#' Removes levels that are missing in the factor
#'
#' @param f factor
#'
#' @export
w_cleanLvlFactor <- function(f)
{
  if(!is.factor(f))
    stop("argument is not a factor")
  nlvls = levels(f)
  
  ## removing levels that aren't present in the factor
  for(l in levels(f))
  {
    if(!l %in% as.character(f))
    {
      message(paste0("removing level '", l,"' : missing in factor"))
      nlvls = nlvls[!nlvls %in% l]
    }
  }
  ## this trick is needed because factor(f, levels = nlvls) doesn't
  ## put NA as factor event though it is in nlvls...
  if(NA %in% nlvls)
    ret = addNA(factor(f, levels = nlvls))
  else
    ret = factor(f, levels = nlvls)
  
  return(ret)
}


#' Replace empty or all-white strings by NAs
#'
#' For character and factor variables
#'
#' @param DT data.frame or data.table
#' @param byref if true, columns are changed by reference
#'
#' @return the modified dataset
#'
#' @export
w_replaceEmptyByNA <- function(DT, byref = T)
{
  ## get only the name of variables that are of type factor/character
  ## and containing blank elements
  vars = names(DT)[sapply(DT, function(x) class(x) %in% c("factor", "character"))]
  vars = vars[sapply(vars, function(v) length(grep("^[[:blank:]]*$",unique(DT[[v]])))>0)]
  if(is.data.table(DT) | byref)
  {
    for(v in vars)
      DT[, (v):=gsub(pattern = "^[[:blank:]]*$", replacement = NA, x = DT[[v]])]
    return(DT)
  }
  else if(is.data.frame(DT))
  {
    for(v in vars)
      DT[[v]] = gsub(pattern = "^[[:blank:]]*$", replacement = NA, x = DT[[v]])
    return(DT)
  }
  else
    stop("DT is not a data.frame")
}

#' get all the variables having a given number of NA in the dataset
#'
#' @param DT the dataset
#'
#' @return a data.table with 3 column: NB_NA (number of NA), NB_VARS
#' (number of variables having this amount of NA), VARS (name of the
#' corresponding variables)
#'
#' @export
w_varByNBNA <- function(DT)
{
  inf = info(DT)
  
  vars_by_nbna = data.table(NB_NA = unique(inf$NB_NA),
                            VARS = lapply(unique(inf$NB_NA), function(x){
                              inf$COL[inf$NB_NA == x]
                            }))
  
  vars_by_nbna$NB_VARS = sapply(vars_by_nbna$VARS, length)
  
  data.table::setcolorder(vars_by_nbna, c("NB_NA", "NB_VARS", "VARS"))
  
  return(vars_by_nbna)
}

# Data Manipulation and Info ########################################
#####################################################################




#' Information about a dataset
#'
#' @param DT [data.table] dataset
#' @return if global = F (default):
#' for each column of the dataset:
#' - the number of NAs
#' - the proportion of NAs
#' - the number of unique values
#' - the proportion of unique values
#' - the column type (continuous or discrete)
#' - the R variable type
#' else if global = T
#' - tocomplete
#'
#' @export
info <- function(DT, global = FALSE)
{
  if(global)
    ret = data.table::data.table(
      NB_QUOTES       = nrow(DT),
      NB_DUP          = sum(duplicated(DT)),
      PROP_DUP        = round(1000*sum(duplicated(DT))/nrow(DT))/1000,
      NB_INCOMPLETE   = sum(!complete.cases(DT)),
      PROP_INCOMPLETE = round(1000*sum(!complete.cases(DT))/nrow(DT))/1000)
  else
  {
    NB_NA = sapply(DT, function(x) { sum(is.na(x))})
    PROP_NA = sapply(DT, function(x) {
      round(1000*sum(is.na(x))/nrow(DT))/1000})
    NB_UNIQUE = sapply(DT, function(x) {
      length(unique(na.omit(x))) })
    PROP_UNIQUE = sapply(DT, function(x) {
      round(1000*length(unique(na.omit(x)))/nrow(DT))/1000})
    R_TYPE = sapply(colnames(DT), function(x){class(DT[[x]])})
    
    COL = colnames(DT)
    ret = data.table::data.table(COL, NB_NA, PROP_NA, NB_UNIQUE,
                                 PROP_UNIQUE, R_TYPE,
                                 keep.rownames = T)
  }
  return(ret)
}

#' Shorthand for accessing particular rows of an info() table
#'
#' @param info info table produced by info()
#' @param varnames variable names
#'
#' @export
ci <- function(info, varnames)
{
  info[info$COL %in% varnames]
}


#' getnum
#'
#' Get the name of numeric variables in a dataset
#'
#' @param DT the dataset
#' @export
getnum <- function(DT)
{
  names(which(sapply(names(DT), function(v){ is.numeric(DT[[v]])})))
}

#' getfac
#'
#' Get the name of factor variables in a dataset
#'
#' @param DT the dataset
#' @export
getfac <- function(DT)
{
  names(which(sapply(names(DT), function(v){ is.factor(DT[[v]])})))
}

#' getother
#'
#' Get the name of non-numeric, non-factor variables in a dataset
#'
#' @param DT the dataset
#' @export
getother <- function(DT)
{
  names(which(sapply(names(DT), function(v){
    !is.numeric(DT[[v]]) & !is.factor(DT[[v]])
  })))
}



#' Correspondency between a column in a dataa.table and a dictionary
#'
#' Description
#'
#' @param tbl d
#' @param col d
#' @param dictionary d
#' @param by_reference d
#'
#' @export
correspond <- function(tbl, col, dictionary, by_reference = F)
{
  #! to improve: remove trailing spaces
  #! check that not multiple names in dictionary
  
  newcol = tbl[[col]]
  for(nm in names(dictionary))
    newcol[newcol == nm] = dictionary[[nm]]
  
  if(by_reference)
    if(data.table::is.data.table(tbl))
      tbl[, (col):= newcol]
  else
    stop("tbl is not a data.table, not possible to modify by reference")
  else
    tbl[[col]] = newcol
  
  return(tbl)
}

#' transform a table into a list of tables.
#'
#' One of the input table column values should be of the form
#' varname(separator)level
#'
#' @param tbl d
#' @param col d
#' @param vars d
#' @param sep d
#'
#' @export
dtlist <- function(tbl, col, vars, sep)
{
  tbl_list = list()
  if(length(sep) != 1 | !is.character(sep))
    stop("sep should be a character of length 1")
  if(sep %in% c("|","^","(",")",".","$","[","]"))
    sep = paste0("\\",sep)
  
  for(v in vars)
  {
    ind = grep(pattern = paste0("^",v,sep), x = tbl[[col]])
    lvls = sub(pattern = paste0("^",v, sep),"",tbl[[col]][ind])
    tbl_list[[v]] = data.table::data.table(tbl[ind])
    tbl_list[[v]][, (col):=lvls]
    data.table::setnames(tbl_list[[v]], col, v)
  }
  return(tbl_list)
}

# col: unquoted name of the variable
# value: string (level to put first)
putfirst <- function(tbl, col, value)
{
  namecol = deparse(substitute(col))
  if(!value %in% tbl[[namecol]])
    stop(paste0(value, " not in ", namecol))
  
  x = c(value, tbl[[namecol]][tbl[[namecol]] != value])
  
  tbl %>% slice(match(x, tbl[[namecol]]))
}

#' Diagonalize
#'
#' Create a diagonal data.table from a vector of value
#'
#' @param vec a vector of numerical values
#'
#' @return the diagonalized data table (1 line = 1 value)
#'
#' @export
diagonalize <- function(vec)
{
  ret = data.table::data.table(diag(vec, nrow = length(vec), ncol = length(vec)))
  names(ret) = names(vec)
  return(ret)
}


#' returns string w/o leading or trailing whitespace
#' @export
trim <- function(x)
{
  gsub("^\\s+|\\s+$", "", x)
}

#' swaps values in a vector
#' @export
swap <- function(vec, val1, val2)
{
  if(length(val1) != 1 || length(val2) != 1 || !all(c(val1, val2) %in% vec))
    stop(paste0("val1 and val2 should be of length 1 and in vec"))
  indval1 = which(vec == val1)
  indval2 = which(vec == val2)
  vec[indval1] = val2
  vec[indval2] = val1
  return(vec)
}


#' swap rows of a data frame
#' @export
swaprow <- function(dt, ind1, ind2)
{
  if(ind1 <= 0 || ind1 > nrow(dt) ||ind2 <= 0 || ind2 > nrow(dt) || !is_integer(ind1) || !is_integer(ind2))
    stop("illegal values for ind1 / ind2")
  newind = swap(1:nrow(dt), ind1, ind2)
  
  if(!is.data.frame(dt))
    stop("dt should be a data.frame or data.table")
  if(ncol(dt) == 1 & class(dt)[1] == "data.frame")
  {
    namecol = colnames(dt)
    dt = data.frame(dt[newind,])
    colnames(dt) = namecol
    rownames(dt) = 1:nrow(dt)
  }
  else if(class(dt)[1] == "data.frame")
  {
    dt = dt[newind,]
    rownames(dt) = 1:nrow(dt)
  }
  else
    dt = dt[newind]
  return(dt)
}

#' check if values in a vector are integers
#' @export
is_integer <- function(x)
{
  if(is.integer(x))
    return(rep(T, length(x)))
  if(!is.numeric(x))
    stop("x should be numeric")
  
  asint = as.integer(x)
  sapply(1:length(x), function(i) x[i] == asint[i])
}

## 2 modes
## l1 and l2 should contain characters
## if l1 and l2 are list, returns TRUE if all elements of l1 are equal to those of l2
## if l1 is a list of character and l2 a character vector, returns a logical vector
## comparing each element of l1 with l2
charlist_equal <- function(l1, l2)
{
  if(is.list(l1) && is.list(l2))
  {
    if(length(l1) != length(l2))
      return(F)
    if(!(all(sapply(l1, is.character)) && all(sapply(l2, is.character))))
      stop("l1 and l2 should be lists of character vectors")
    return(all(sapply(1:length(l1), function(i){
      (length(l1[[i]]) == length(l2[[i]]) && all(l1[[i]] == l2[[i]]))
    })))
  }
  else if(is.list(l1) && is.character(l2))
    sapply(l1, function(l){ length(l) == length(l2) && all(l == l2)})
  else
    stop("invalid type for l1 / l2")
  
}



## replace a character string of length 1 in a vector by a character string of length >=1
## ex: replace "bb" in c("a", "bb", "c") --> c("a", "SDF", "KJ34", "c")
replaceInVec <- function(vec, what, replacement)
{
  while(what %in% vec)
  {
    i = which(vec == what)[1]
    if(i == 1)
      before = character(0)
    else
      before = vec[1:(i-1)]
    
    if(i == length(vec))
      after = character(0)
    else
      after = vec[(i+1):length(vec)]
    
    vec = c(before, replacement, after)
  }
  
  return(vec)
}




# Tools for api authentification ########################################
#####################################################################


### Function to code/decode in base 64
# x a string
# encode Encode if True (default), decode if False
# return the string encoded or decoded in base64
w_base64_string = function(x, encode=T)
{
  x_safe = x
  if(!encode)
  {
    x_safe = gsub("_","/", x_safe, fixed=T)
    x_safe = gsub("-","+", x_safe, fixed=T)
    x_64 = RCurl::base64Decode(x_safe, mode="raw")
  } else {
    x_64 = RCurl::base64Encode(x_safe)
    x_64 = gsub("/","_", x_64, fixed=T)
    x_64 = gsub("+","-", x_64, fixed=T)
  }
  return(x_64)
}

# convert a list of vector (with names) to a data.table
w_list2dt <- function(l) {
  require(data.table)
  return(rbindlist(lapply(l, function(x) as.data.table(matrix(x, nrow=1,dimnames = list("",names(x))))),fill = T, use.names = T))
}

#' Lookup into a shape file the location of the coordinate provided in the data file
#'
#' @param data datatable of coordinates. The columns must be named lat and lng for latitude and longitude
#' @param shape shapefile
#' @return the data merged with the info provided in the shape file for each coordinates.
#'
#' @export
overFunc = function(data, shape){
  
  if(!(sum(colnames(data) %in% c('lat','lng')) == 2))
    stop('You must include your coordinates in a dataframe with column name lat & lng')
  data.table::setDT(data)
  # Convert to numeric.
  for (j in c('lat','lng')) data.table::set(data,j=j,value=as.numeric(data[[j]]))
  sp::coordinates(data) <- ~lng+lat
  sp::proj4string(data) <- sp::proj4string(shape)
  
  data_coord_tmp = sp::over(x = data, shape)
  data = cbind(data, data_coord_tmp)
  return(data.table::setDT(data))
}

#' Function to compute the distance in meters between 2 points gien by their coordinates
#'
#' @param latA,latB,lngA,lngB: vector of coordinates of the point A and B
#' @return vector of distances
#'
#' @export
GeoDist <- function(latA,latB,lngA,lngB) {
  
  if(missing(latA) | missing(latB) | missing(lngA) | missing(lngB)) stop('You need to provide 4 coordinates.')
  
  latA = as.numeric(as.character(latA))
  latB = as.numeric(as.character(latB))
  lngA = as.numeric(as.character(lngA))
  lngB = as.numeric(as.character(lngB))
  # convert to radian
  latA = pi/180*latA
  latB = pi/180*latB
  lngA = pi/180*lngA
  lngB = pi/180*lngB
  # Radius of the earth
  R = 6378000
  # Distance in meters
  d = R*acos(sin(latA)*sin(latB)+cos(latB)*cos(latA)*cos((lngA-lngB)))
  return(d)
}

#' Function to return the difference in latitude and longitude between a point and its boundary radius
#'
#' @param clat, clng : coordinates of the center of the circle
#' @param radius: in meter of the circle
#' @return vector of difference of lat and lng
#'
#' @export
circleBounds <- function(clat, clng, radius) {
  
  if(missing(clat) | missing(clng) | missing(radius)) stop('You need to provide the coordinates of the center and the radius')
  clat = as.numeric(as.character(clat))
  clng = as.numeric(as.character(clng))
  # convert to radian
  clat = pi/180*clat
  clng = pi/180*clng
  # Radius of the earth
  R = 6378000
  # Compute the max latitude and max longitude
  mlat <- (clat - radius/R) * 180/pi
  mlng <- (clng - acos(1/(cos(clat)^2)*(cos(radius/R) - sin(clat)^2))) * 180 /pi
  
  diffLng = abs(mlng - (clng * 180/pi))
  diffLat = abs(mlat - (clat * 180/pi))
  return(c(diffLat,diffLng))
}

# Others ############################################################
#####################################################################

#' hc_listpts
#'
#' get (x,y) data into the right format to be added to a highcharts line series
#'
#' @param x x coordinates
#' @param y y coordinates
#'
#' @export
hc_listpts <- function(x,y)
{
  ret = list()
  for(i in 1:length(x))
    ret[[i]] = c(x[i], y[i])
  return(ret)
}

#' Export Description
#'
#' Export the description of a dataset to an excel file
#'
#' @param DT the dataset
#' @param description string describing the dataset
#' @param file path to the file. Defaults to "./name_of_dataset_object.xlsx"
#' @param overwrite overwrite or not the file
#'
#' @return sheet 2 is basically the result of axaml::info(DT).
#' Column "IS_PERSONAL" is added with defualt value "?", to reference personal
#' or non personal data
#' @export
export_description <- function(DT, file = NULL, overwrite = F, DESCRIPTION = T, IS_ANONYMIZED = F, IS_PERSONAL = F)
{
  ## Description not used yet
  
  if(!is.data.frame(DT))
    stop("DT should be a data frame")
  
  if(is.null(file))
  {
    name_DT = deparse(substitute(DT))
    file = paste0("./", name_DT, ".xlsx")
    message(paste0("writing description file by default to: ", file))
  }
  if(file.exists(file) && !overwrite)
    stop(paste0("file '", file,"' already exists and overwrite = F"))
  
  ## verify that file is in xlsx format
  ## verify that file has a correct path
  
  ## create structure
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheetName = "Data Description", gridLines = FALSE)
  openxlsx::addWorksheet(wb, sheetName = "Variable Breakdown", gridLines = FALSE)
  
  ## first sheet
  openxlsx::writeData(wb = wb, sheet = 1, x = "number of rows: ", startRow = 1, startCol = 1)
  openxlsx::writeData(wb = wb, sheet = 1, x = nrow(DT), startRow = 1, startCol = 2)
  
  openxlsx::writeData(wb = wb, sheet = 1, x = "number of columns: ", startRow = 2, startCol = 1)
  openxlsx::writeData(wb = wb, sheet = 1, x = ncol(DT), startRow = 2, startCol = 2)
  
  openxlsx::writeData(wb = wb, sheet = 1, x = "In-memory size: ", startRow = 3, startCol = 1)
  openxlsx::writeData(wb = wb, sheet = 1, x = capture.output(pryr::object_size(DT)), startRow = 3, startCol = 2)
  
  openxlsx::setColWidths(wb = wb, sheet = 1, widths = 20, cols = 1:2)
  
  
  ## second sheet: variable breakdown
  inf = axaml::info(DT)
  if(is.logical(DESCRIPTION) & DESCRIPTION)
    inf$DESCRIPTION   = "?"
  if(is.logical(IS_PERSONAL) & IS_PERSONAL)
    inf$IS_PERSONAL   = "?"
  if(is.logical(IS_ANONYMIZED) & IS_ANONYMIZED)
    inf$IS_ANONYMIZED   = "?"
  
  openxlsx::writeDataTable(wb = wb, sheet = 2, x = inf)
  openxlsx::setColWidths(wb = wb, sheet = 2, widths = 20, cols = 1:ncol(inf))
  
  ## save workbook
  openxlsx::saveWorkbook(wb, file, overwrite = overwrite) ## save to working directory
}

#' ''Sniffs' a dataset to look for variable names that could have a special 'meaning'
#'
#' @param type type of variable to look for.
#' type = "exposure" --> look for variables that are in [0,1] or have "Expo" in their name.
#' Currently, only type = 'exposure' is implemented.
#' @param DT dataset
#' @export
sniff <- function(type = NULL, DT = NULL, verbose = T)
{
  ## sniff types implemented
  alltypes <- c("exposure", "claimcount", "age")
  
  ## argument check
  if(is.null(type) && is.null(DT))
  {
    cat()
    return(NULL)
  }
  if(!is.null(type) && is.null(DT))
    stop("DT is NULL")
  if(!type %in% alltypes)
    stop("invalid type")
  
  ## sniff implementations
  if(type == "exposure")
  {
    candidates_bool <- sapply(names(DT), function(col){
      if(verbose)
        cat("examining '", col,"' ...\n")
      is.numeric(DT[[col]]) &&
        ((max(DT[[col]], na.rm = T) <= 1 && min(DT[[col]], na.rm = T) >= 0) ||
           grepl("(Expo)|(expo)", col))
    })
    return(names(DT)[candidates_bool])
  }
  else if(type == "claimcount")
  {
    candidates_bool <- sapply(names(DT), function(col){
      if(verbose)
        cat("examining '", col,"' ...\n")
      is.numeric(DT[[col]]) &&
        min(DT[[col]], na.rm = T) >= 0 &&
        all(DT[[col]]%%1==0) &&
        names(sort(table(DT[[col]]),decreasing=TRUE))[1] == "0"
    })
    return(names(DT)[candidates_bool])
  }
  else if(type == "age")
  {
    candidates_bool <- sapply(names(DT), function(col){
      if(verbose)
        cat("examining '", col,"' ...\n")
      is.numeric(DT[[col]]) &&
        grepl("^Age|^age|^AGE", col)
    })
    return(names(DT)[candidates_bool])
  }
  else
    return(NULL)
}


# Data Anonymization ################################################
#####################################################################

#' Anonymize
#'
#' Anonymize a dataset
#'
#' @param DT the dataset
#' @param vars variables to anonymize
#' @param method method of anonymization. Currently only support hashing with
#' the crc32 algorithm.
#'
#' @export
anonymize <- function(DT, vars, method = "crc32")
{
  if(method == "crc32")
  {
    for(v in vars)
    {
      unq_hashes <- vapply(unique(DT[[v]]), function(object) digest::digest(object, algo=method),
                           FUN.VALUE="", USE.NAMES=TRUE)
      DT[[v]] = unname(unq_hashes[DT[[v]]])
    }
  }
  else
    stop(paste0("method '", method, "' not implemented."))
  
  return(DT)
}

#' Anonymize Interactively
#'
#' Addin interface to anonymize()
#'
#'@importFrom miniUI miniPage miniContentPanel gadgetTitleBar
#'@importFrom shiny textInput uiOutput radioButtons conditionalPanel reactive renderUI checkboxGroupInput dialogViewer runGadget observeEvent
#'@export
anonymizeI <- function() {
  
  # Get the document context.
  context <- rstudioapi::getActiveDocumentContext()
  
  # Set the default data to use based on the selection.
  text <- context$selection[[1]]$text
  defaultData <- text
  
  # Generate UI for the gadget.
  ui <- miniPage(
    gadgetTitleBar("anonymize"),
    miniContentPanel(
      textInput("data", "Data", value = defaultData),
      uiOutput("pending"),
      conditionalPanel('output$ok()', uiOutput('varSelect'))
    )
  )
  
  # Server code for the gadget.
  server <- function(input, output, session) {
    
    reactiveData <- reactive({
      # Collect inputs.
      dataString <- input$data
      
      # Check to see if there is data called 'data',
      # and access it if possible.
      if (!nzchar(dataString))
        return(errorMessage("data", "No dataset available."))
      
      if (!exists(dataString, envir = .GlobalEnv))
        return(errorMessage("data", paste("No dataset named '", dataString, "' available.")))
      
      data <- get(dataString, envir = .GlobalEnv)
    })
    
    ok <- reactive({
      is.data.frame(reactiveData())
    })
    
    output$varSelect <- renderUI({
      data <- reactiveData()
      checkboxGroupInput('showVars', 'Variables to anonymize:',
                         names(data), selected = NULL)
    })
    
    output$pending <- renderUI({
      data <- reactiveData()
      if (isErrorMessage(data))
        h4(style = "color: #AA7732;", data$message)
    })
    
    # Listen for 'done'.
    observeEvent(input$done, {
      
      # Emit a subset call if a dataset has been specified.
      if(nzchar(input$data) & length(input$showVars) > 0){
        
        varstr = paste0("c('",
                        paste0(input$showVars, collapse = "','"), "')")
        code <- paste0("\nanonym_",input$data, " = anonymize(DT = ",
                       input$data, ", vars = ", varstr, ")")
        rstudioapi::insertText(text = code)
      }
      
      invisible(stopApp())
    })
  }
  
  # Use a modal dialog as a viewr.
  viewer <- dialogViewer("Hello World", width = 1000, height = 800)
  runGadget(ui, server, viewer = viewer)
}
