setwd("/home/kareande/lchl-mhw-events")
library("ranger") #randomForest package
library("vip") #variable importance plots
library("tidymodels") #tidyverse models
library("foreach") #parallel processing
library("doParallel") #parallel processing
library("ggplot2") #aesthetic plotting
library("plot.matrix") #confusion matrix
library("caret") #proportional matrix
#install.packages()

############################## Refined Train LChl RF ##############################
# Provide refined ranges for parameters
n_min_range <- c()
n_max_range <- c()
mtry_min_range <- c()
mtry_max_range <- c()
n_min_range[1] <- 5 #2 days lag
n_max_range[1] <- 12
mtry_min_range[1] <- 12
mtry_max_range[1] <- 14
n_min_range[2] <- 1 #7 days lag
n_max_range[2] <- 6
mtry_min_range[2] <- 9
mtry_max_range[2] <- 10
n_min_range[3] <- 1 #14 days lag
n_max_range[3] <- 8
mtry_min_range[3] <- 12
mtry_max_range[3] <- 14
n_min_range[4] <- 1 #180 days lag
n_max_range[4] <- 7
mtry_min_range[4] <- 11
mtry_max_range[4] <- 12
n_min_range[5] <- 1 #365 days lag
n_max_range[5] <- 10
mtry_min_range[5] <- 7
mtry_max_range[5] <- 11
n_min_range[6] <- 10 #730 days lag
n_max_range[6] <- 14
mtry_min_range[6] <- 13
mtry_max_range[6] <- 14

vars_lag = c(2, 7, 14, 180, 365, 730) #2 days, 1 wk, 2 wk, 6 mo, 1 yr, 2 yr
n_trees <- 200 #designate number of trees
final_rfs <- c(rep(NA, 4))
set.seed(3939)
doParallel::registerDoParallel(32)
for(i in 1:length(vars_lag)){
    file_name <- gsub(" ", "", paste("cmpnd_blncd_",vars_lag[i],"lag.csv")) #create dyamic df name
    workingset <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)),sep=",") #get cmpnd df
    workingset <- workingset[ , ! names(workingset) %in% 
                             c("day","mo","lchlCat","mhwCat", #remove day, mo, lchlCat, and mhwCat columns
                               "nit","oxy","pho","chl","sil","npp", #remove unlagged lchl vars
                               "qnet","slp","sat","wndsp","sst","sstRoC")] #remove unlagged mhw vars
    workingset[workingset$cmpCat == 3,]$cmpCat="Compound"
    workingset[workingset$cmpCat == 2,]$cmpCat="LChl Event"
    workingset[workingset$cmpCat == 1,]$cmpCat="MHW Event"
    workingset[workingset$cmpCat == 0,]$cmpCat="No Event"
    workingset$cmpCat = as.factor(workingset$cmpCat)
    
    # Split data into training and testing sets
    cmp_split <- initial_split(workingset, strata = cmpCat)
    cmp_train <- training(cmp_split)
    cmp_test <- testing(cmp_split)
    cmp_rec <- recipe(cmpCat ~ ., data = cmp_train)
    cmp_prep <- prep(cmp_rec)
    juiced <- juice(cmp_prep)
    
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
    final_rfs[i] <- final_rf

    # Check variable importance for training data
    pdf(gsub(" ", "", paste("cmpndFigs/VarImpTrnCmpnd",vars_lag[i],"Lag",n_trees,"T.pdf")))
    pltimg <- final_rf %>%
      set_engine("ranger", importance = "permutation") %>%
      fit(cmpCat ~ .,
        data = juice(cmp_prep)) %>%
      vip(geom = "point")
    print(pltimg)
    dev.off()
    
    # Use testing data in model
    final_wf <- workflow() %>%
      add_recipe(cmp_rec) %>%
      add_model(final_rf)
    final_res <- final_wf %>%
      last_fit(cmp_split)

    # Produce confusion matrix
    pdf(gsub(" ", "", paste("cmpndFigs/confMatCmpnd",vars_lag[i],"Lag",n_trees,"T.pdf")))
    pltimg <- final_res %>%
      collect_predictions() %>%
      conf_mat(cmpCat, .pred_class) %>%
      autoplot(type = "heatmap")
    print(pltimg)
    dev.off()
    
    # Produce proportional confusion matrix
    cmat <- as.table(confusionMatrix(collect_predictions(final_res)$.pred_class,collect_predictions(final_res)$cmpCat))
    cmpt <- sum(cmat[,1])
    chlt <- sum(cmat[,2])
    mhwt <- sum(cmat[,3])
    noet <- sum(cmat[,4])
    c11 <- cmat[1,1]/cmpt
    c21 <- cmat[2,1]/cmpt
    c31 <- cmat[3,1]/cmpt
    c41 <- cmat[4,1]/cmpt
    c12 <- cmat[1,2]/chlt
    c22 <- cmat[2,2]/chlt
    c32 <- cmat[3,2]/chlt
    c42 <- cmat[4,2]/chlt
    c13 <- cmat[1,3]/mhwt
    c23 <- cmat[2,3]/mhwt
    c33 <- cmat[3,3]/mhwt
    c43 <- cmat[4,3]/mhwt
    c14 <- cmat[1,4]/noet
    c24 <- cmat[2,4]/noet
    c34 <- cmat[3,4]/noet
    c44 <- cmat[4,4]/noet
    lab <- c("Compound", "LChl Event", "MHW Event", "None")
    cnfs <- matrix(c(c11, c21, c31, c41, c12, c22, c32, c42, c13, c23, c33, c43, c14, c24, c34, c44),
                        nrow = 4, ncol = 4,
                        dimnames = list(lab, lab))
    pdf(gsub(" ", "", paste("cmpndFigs/confMatPrptnCmpnd",vars_lag[i],"Lag",n_trees,"T.pdf")))
    par(mar=c(5.1, 4.1, 4.1, 4.1)) # adapt margins
    pltimg <- plot(cnfs,
         xlab="",
         ylab="",
         main="RF Prediction Accuracy",
         digits=2,
         fmt.cell='%.2f',
         cex=1,
         col=topo.colors,
         breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1),
         spacing.key=c(0.75,0.3,-.5),
         fmt.key="%.3f")
    print(pltimg)
    dev.off()
    #rm(list= ls()[!(ls() %in% c('n_min_range','n_max_range','mtry_min_range','mtry_max_range','vars_lag','n_trees','final_rfs'))])
    }
doParallel::stopImplicitCluster()
final_rfs
