############################ Functions and tests ############################ 

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
  name <- name_claim_data
  # Load datasets on Macbook Pro #
  if (!exists(name, envir = globalenv()))
  {
    if (verbose == T)
    {print(paste0("loading ",name))}
    claim_data <- sas7bdat::read.sas7bdat(paste0("/Users/Kanon/Google Drive/AXA/data/Merged_data"
                                                 , name_claim_data))
  }
  else(claim_data <- eval(parse(text = name)))

  if (!exists("panel_ass", envir = globalenv()))
  {
    if (verbose == T)
    {print("Reading in panel_ass, please take a coffee... ")}
    panel_ass <- sas7bdat::read.sas7bdat("/Users/Kanon/Google Drive/AXA/data/MSH/exposure/panel_ass.sas7bdat")
  }
  
  # Load datasets on Windows #
  # panel_ass <- read.sas7bdat("C:/Users/s636000/Documents/Expat/data/MSH/panel_ass.sas7bdat")
  # claim_data <- read.sas7bdat(paste0("C:/Users/s636000/Documents/Expat/data/MSH/",name_claim_data))
  
  # Check the format of each data set #
  if (verbose == TRUE)
  {print(paste0("Check whether ", name_claim_data, " is a data.frame object: "))}
  is.data.frame(claim_data)
  
  # Set the pk (primary key) equals to paste(annee,ident_personne) in preparation for left join #
  if (verbose == TRUE)
  {print(paste0("Adding a new variable serves as the primary key 'pk' in panel_ass") )}
  panel_ass$pk <- as.numeric(paste(panel_ass$annee,panel_ass$ident_personne,sep=""))
  
  # Preparation with dataset claim_data for consistancy #
  if (verbose == TRUE)
  {print(paste0("Adding a new variable serves as the primary key 'pk' in ", name_claim_data) )}
  claim_data$pk <- as.numeric(paste(claim_data$annee,claim_data$ident_personne,sep=""))
  
  # Check duplication in the loaded claims dataset. If exist, remove all duplications #
  if (verbose == TRUE)
  {print(paste0("Checking duplication in ", name_claim_data, "..."))}
  claim_data <- check_dup(claim_data, name_claim_data, verbose = verbose)
  
  # In preparation for the left outer join, some redundant variables in claim_data #
  # are removed #
  # ident_personne and annee_soin are included in pk #
  # A test has been performed to check weather the presence in claim_data corresponds to #
  # the presence in panel_ass. Results turned out to be positive, thus no need in keeping both #
  if (verbose == TRUE)
  {print(paste0("Variables 'ident_personne', 'annee_soin'
                , 'presence' are removed from ", name_claim_data))}
  claim_data$ident_personne <- NULL
  claim_data$annee_soin <- NULL
  claim_data$presence <- NULL
  
  # Left outer join of panel_ass and claim_data #
  if (verbose == TRUE)
  {print(paste0("Left outer joining ",name_claim_data, " to panel_ass by key 'pk'..."))}
  merged_data <- merge(panel_ass,claim_data, by = "pk", all.x = TRUE)
  
  ## Deal with the NA values in merged_data ##
  # Detect all the columns that contain NA value and the number of them #
  if (verbose == TRUE)
  {detection_NA(merged_data)}
  # Assign 0 to the frequency column for those who haven't had claims during exposure #
  merged_data$somme_quantite[is.na(merged_data$somme_quantite)] <- 0
  # Assign 0 to the cost column for those who haven't had claims during exposure #
  merged_data$somme_frais[is.na(merged_data$somme_frais)] <- 0
  # Replace some of the missing entries(30%) of date_sortie by date_sortie_obs #
  merged_data$date_sortie[is.na(merged_data$date_sortie)] <- 
    merged_data$date_sortie_obs[which(is.na(merged_data$date_sortie))]
  # Delete entries whose date_naissance(age at the same time) is NA(very few, 8 cases in panel_ass) #
  merged_data <- merged_data[-which(is.na(merged_data$age)),]
  # Detect all the columns that contain NA value and the number of them #
  if (verbose == TRUE)
  {detection_NA(merged_data)}
  
  # Delete primary key
  merged_data$pk <- NULL
  
  # Delete variables that has numerous levels which have few information
  merged_data$nationalite_2 <- NULL
  merged_data$pays_expat_2 <- NULL
  merged_data$pays <- NULL
  merged_data$pays_2 <- NULL
  
  # Uniform variable which doesn't bring in any signal
  merged_data$nb_adherents <- NULL
  
  # Transform all numerical binary variables into factors
  if (verbose == TRUE)
  {print("Transfering binary variables into factor objects")}
  merged_data <- binary_to_factor(merged_data)
  
  # Check the targets, negative values are listed and eliminated
  if (verbose == TRUE)
  {print("Eliminate observations whose target is negative")}
  merged_data <- eliminate_negative(merged_data, verbose = verbose)
  
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
    column <- as.data.table(column)
    dt = column[,.(mean(get(name),na.rm=T)
                   ,sd(get(name),na.rm=T)
                   ,skewness(get(name),na.rm=T)
                   ,kurtosis(get(name),na.rm=T)
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
    temp <- as.data.table(c(column,group_factor))
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
    dt <- as.data.table(dt)
    return(dt)
  }
  ##############################################################################################################
  
  
  
  ############################################ feature selection ###############################################
  feature_selection <- function(sdt,treecut,cp)
  {
    mycontrol <- rpart.control(cp = cp, xval = 10)
    tree = rpart(formula = 100*(sdt$tgt-mean(sdt$tgt))~.
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
      mygbm <- gbm(formula = combined_features$tgt ~ ., data = combined_features, interaction.depth = 3
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
      column <- as.data.table(column)
      
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
      column <- as.data.table(column)
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