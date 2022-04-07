setwd("/home/kareande/lchl-mhw-events")
library("ranger") #randomForest package
library("vip") #variable importance plots
library("tidymodels") #tidyverse models
library("foreach") #parallel processing
library("doParallel") #parallel processing
library("ggplot2") #aesthetic plotting
#install.packages()

############################## Refined Train LChl RF ##############################
# Provide refined ranges for parameters
n_min_range <- c()
n_max_range <- c()
mtry_min_range <- c()
mtry_max_range <- c()
n_min_range[1] <- 1 #2 days lag
n_max_range[1] <- 6
mtry_min_range[1] <- 26
mtry_max_range[1] <- 28
n_min_range[2] <- 1 #7 days lag
n_max_range[2] <- 9
mtry_min_range[2] <- 5
mtry_max_range[2] <- 9
n_min_range[3] <- 1 #14 days lag
n_max_range[3] <- 5
mtry_min_range[3] <- 6
mtry_max_range[3] <- 14
n_min_range[4] <- 1 #180 days lag
n_max_range[4] <- 8
mtry_min_range[4] <- 7
mtry_max_range[4] <- 10
n_min_range[5] <- 1 #365 days lag
n_max_range[5] <- 5
mtry_min_range[5] <- 3
mtry_max_range[5] <- 7
n_min_range[6] <- 1 #730 days lag
n_max_range[6] <- 7
mtry_min_range[6] <- 11
mtry_max_range[6] <- 16

vars_lag = c(2, 7, 14, 180, 365, 730) #2 days, 1 wk, 2 wk, 6 mo
n_trees <- 200 #designate number of trees
final_rfs <- c(rep(NA, 4))
set.seed(3939)
doParallel::registerDoParallel(32)
#for(i in 1:length(vars_lag)){
for(i in 5:6){
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
    tune_spec <- rand_forest(
      mtry = tune(),      #number of variables sampled
      trees = n_trees,    #change number of trees
      min_n = tune()) %>% #min number of datapoints for node to split
      set_mode("classification") %>%
      set_engine("ranger")
    tune_wf <- workflow() %>%
      add_recipe(cmp_rec) %>%
      add_model(tune_spec)
    
    # Refine model specs for re-training based on previous results
    rf_grid <- grid_regular(
      mtry(range = c(mtry_min_range[i], mtry_max_range[i])),
      min_n(range = c(n_min_range[i], n_max_range[i])),
      levels = 10)

    # Train models using refined specs
    cmp_folds <- vfold_cv(cmp_train)
    regular_res <- tune_grid(
      tune_wf,
      resamples = cmp_folds,
      grid = rf_grid)

    # Visualize refined results
    pdf(gsub(" ", "", paste("cmpndFigs/refTrnCmpnd",vars_lag[i],"Lag",n_trees,"T.pdf")))
    pltimg <- regular_res %>%
      collect_metrics() %>%
      filter(.metric == "accuracy") %>% #or roc_auc
      mutate(min_n = factor(min_n)) %>%
      ggplot(aes(mtry, mean, color = min_n)) +
      geom_line(alpha = 0.5, size = 1.5) +
      geom_point() +
      labs(y = "Accuracy")
    print(pltimg)
    dev.off()

    # Deciding on best model
    best_rf <- select_best(regular_res, "roc_auc")
    final_rf <- finalize_model(
      tune_spec,
      best_rf)
    final_rfs[i] <- gsub(" ", "", paste(final_rf," for ",vars_lag[i]," days lag..."))

    # Check variable importance for training data
    cmp_prep <- prep(cmp_rec)
    juiced <- juice(cmp_prep)
    pdf(gsub(" ", "", paste("cmpndFigs/VarImpTrnCmpnd",vars_lag[i],"Lag",n_trees,"T.pdf")))
    pltimg <- final_rf %>%
      set_engine("ranger", importance = "permutation") %>%
      fit(cmpCat ~ .,
        data = juice(cmp_prep)) %>%
      vip(geom = "point")
    print(pltimg)
    dev.off()
    rm(list= ls()[!(ls() %in% c('n_min_range','n_max_range','mtry_min_range','mtry_max_range','vars_lag','n_trees','final_rfs'))])
    }
doParallel::stopImplicitCluster()
final_rfs
