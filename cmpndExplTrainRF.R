setwd("/home/kareande/lchl-mhw-events")
library("ranger") #randomForest package
library("tidymodels") #tidyverse models
library("foreach") #parallel processing
library("doParallel") #parallel processing
library("ggplot2") #aesthetic plotting
#install.packages()

##################################### Exploratory Train LChl RF #####################################
vars_lag = c(2, 7, 14, 180) #2 days, 1 wk, 2 wk, 6 mo
set.seed(3939)
doParallel::registerDoParallel(32)
for(i in 1:length(vars_lag)){
    file_name <- gsub(" ", "", paste("cmpnd_blncd_",vars_lag[i],"lag.csv")) #create dyamic df name
    workingset <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)),sep=",") #get cmpnd df
    workingset <- workingset[,-c(1:2,31:32)] #remove day, mo, lchlCat, and mhwCat columns
    workingset[workingset$cmpCat == 3,]$cmpCat="Compound"
    workingset[workingset$cmpCat == 2,]$cmpCat="LChl Event"
    workingset[workingset$cmpCat == 1,]$cmpCat="MHW Event"
    workingset[workingset$cmpCat == 0,]$cmpCat="No event"
    workingset$cmpCat = as.factor(workingset$cmpCat)
    
    # Split data into training and testing sets
    cmp_split <- initial_split(workingset, strata = cmpCat)
    cmp_train <- training(cmp_split)
    cmp_test <- testing(cmp_split)
    cmp_rec <- recipe(cmpCat ~ ., data = cmp_train)
    
    # Create model specification
    n_trees <- 200
    tune_spec <- rand_forest(
      mtry = tune(),      #number of variables sampled
      trees = n_trees,    #change number of trees
      min_n = tune()) %>% #min number of datapoints for node to split
      set_mode("classification") %>%
      set_engine("ranger")
    tune_wf <- workflow() %>%
      add_recipe(cmp_rec) %>%
      add_model(tune_spec)
    
    # Training model
    start_time <- Sys.time()
    cmp_folds <- vfold_cv(cmp_train)
    tune_res <- tune_grid(
      tune_wf,
      resamples = cmp_folds,
      grid = 10)

    # Visualize results of k-fold analysis; accuracy
    pdf(gsub(" ", "", paste("cmpndFigs/preTrainCmpnd",vars_lag[i],"lag",n_trees,"T.pdf")))
    tune_res %>%
      collect_metrics() %>%
      filter(.metric == "accuracy") %>%
      select(mean, min_n, mtry) %>%
      pivot_longer(min_n:mtry,
                   values_to = "value",
                   names_to = "parameter") %>%
      ggplot(aes(value, mean, color = parameter)) +
      geom_point(show.legend = FALSE) +
      facet_wrap(~parameter, scales = "free_x") +
      labs(x = NULL, y = "Accuracy")
    dev.off()
    }
doParallel::stopImplicitCluster()
