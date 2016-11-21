############################ Functions and tests ############################ 

# A function that checks duplication of a loaded dataset #
check_dup <- function(DATASET)
{
  size <- dim(DATASET)[1]
  dedup_size <- dim(unique(DATASET))[1]
  if (size == dedup_size)
  {
    print(paste0("No duplication in ",deparse(substitute(DATASET)), "!"))
    return(DATASET)
  }
  else if(size < dedup_size)
  {print("ERROR!")}
  else if(size > dedup_size)
  {
    DATASET <- unique(DATASET)
    print(paste0(size-dedup_size, " duplication(s) have(s) been removed from ", deparse(substitute(DATASET)), "!"))
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

# A function that detects negative responding values in a dataset
# As negative values are not allowed for the 'Poisson' family
# Those entries must be eliminated

eliminate_negative <- function(DATASET)
{
  to_eliminate <- which(DATASET$somme_quantite < 0)
  DATASET <- DATASET[-to_eliminate,]
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
  gini = (trapz(x, y)-0.5)/0.5
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

############################ Functions and tests ############################ 