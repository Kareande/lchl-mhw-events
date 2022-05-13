setwd("/home/kareande/lchl-mhw-events")
library("ranger") #randomForest package
library("tidymodels") #tidyverse models
library("foreach") #parallel processing
library("doParallel") #parallel processing
library("ggplot2") #aesthetic plotting
#install.packages()

##################################### Exploratory Train LChl RF #####################################
vars_lag = c(2, 7, 14, 180, 365, 730) #2 days, 1 wk, 2 wk, 6 mo, 1 yr, 2 yr
n_trees <- 200
set.seed(3939)
doParallel::registerDoParallel(32)
for(i in 1:length(vars_lag)){
    # Load training data
    file_name <- gsub(" ", "", paste("cmpnd_trn_",vars_lag[i],"lag.csv")) #create dyamic df name
    cmp_train <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)),sep=",") #get cmpnd df
    cmp_train[cmp_train$cmpCat == 3,]$cmpCat="Compound"
    cmp_train[cmp_train$cmpCat == 2,]$cmpCat="LChl Event"
    cmp_train[cmp_train$cmpCat == 1,]$cmpCat="MHW Event"
    cmp_train[cmp_train$cmpCat == 0,]$cmpCat="No event"
    cmp_train$cmpCat = as.factor(cmp_train$cmpCat)
        
    # Create model specification
    cmp_rec <- recipe(cmpCat ~ ., data = cmp_train)
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
    cmp_folds <- vfold_cv(cmp_train)
    tune_res <- tune_grid(
      tune_wf,
      resamples = cmp_folds,
      grid = 10)

    # Visualize results of k-fold analysis; accuracy
    pdf(gsub(" ", "", paste("cmpndFigs/preTrainCmpnd",vars_lag[i],"lag",n_trees,"T.pdf")))
    pltimg <- tune_res %>%
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
    print(pltimg)
    dev.off()
    rm(list= ls()[!(ls() %in% c('vars_lag','n_trees'))])
    }
doParallel::stopImplicitCluster()

