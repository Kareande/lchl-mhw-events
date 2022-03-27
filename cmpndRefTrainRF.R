setwd("/home/kareande/lchl-mhw-events")
library("ranger") #randomForest package
library("vip") #variable importance plots
library("tidymodels") #tidyverse models
library("foreach") #parallel processing
library("doParallel") #parallel processing
library("ggplot2") #aesthetic plotting
#install.packages()

##################################### Refined Train LChl RF #####################################
# Provide refined ranges for parameters
n_min_range <- c()
n_max_range <- c()
mtry_min_range <- c()
mtry_max_range <- c()
n_min_range[1] <- #2 days lag
n_max_range[1] <-
mtry_min_range[1] <-
mtry_max_range[1] <- 
n_min_range[2] <- #7 days lag
n_max_range[2] <- 
mtry_min_range[2] <-
mtry_max_range[2] <- 
n_min_range[3] <- #14 days lag
n_max_range[3] <- 
mtry_min_range[3] <-
mtry_max_range[3] <- 
n_min_range[4] <- #180 days lag
n_max_range[4] <- 
mtry_min_range[4] <-
mtry_max_range[4] <- 

vars_lag = c(2, 7, 14, 180) #2 days, 1 wk, 2 wk, 6 mo
final_rfs <- c()
set.seed(3939)
doParallel::registerDoParallel(32)
for(i in 1:length(vars_lag)){
    # Refine model specs for re-training based on previous results
    rf_grid <- grid_regular(
      mtry(range = c(mtry_min_range[i], mtry_max_range[i])),
      min_n(range = c(n_min_range[i], n_max_range[i])),
      levels = 10)

    # Train models using refined specs
    regular_res <- tune_grid(
      tune_wf,
      resamples = cmp_folds,
      grid = rf_grid)

    # Visualize refined results
    pdf(gsub(" ", "", paste("cmpndFigs/refTrnCmpnd",vars_lag[i],"Lag",n_trees,"T.pdf")))
    regular_res %>%
      collect_metrics() %>%
      filter(.metric == "accuracy") %>% #or roc_auc
      mutate(min_n = factor(min_n)) %>%
      ggplot(aes(mtry, mean, color = min_n)) +
      geom_line(alpha = 0.5, size = 1.5) +
      geom_point() +
      labs(y = "Accuracy")
    dev.off()

    # Deciding on best model
    best_rf <- select_best(regular_res, "roc_auc")
    final_rf <- finalize_model(
      tune_spec,
      best_rf)
    final_rfs[i] <- gsub(" ", "", paste(final_rf," for ",vars_lag[i]," days lag..."))

    # Check variable importance for training data
    pdf(gsub(" ", "", paste("cmpndFigs/VarImpTrnCmpnd",vars_lag[i],"Lag",n_trees,"T.pdf")))
    cmp_prep <- prep(cmp_rec)
    juiced <- juice(cmp_prep)
    library(vip)
    final_rf %>%
      set_engine("ranger", importance = "permutation") %>%
      fit(cmpCat ~ .,
        data = juice(cmp_prep)) %>%
      vip(geom = "point")
    dev.off()
    }
doParallel::stopImplicitCluster()
final_rfs