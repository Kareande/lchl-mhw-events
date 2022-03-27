setwd("/home/kareande/lchl-mhw-events")
library("ranger") #randomForest package
library("tidymodels") #tidyverse models
library("foreach") #parallel processing
library("doParallel") #parallel processing
library("ggplot2") #aesthetic plotting
library("plot.matrix") #confusion matrix
#install.packages()

##################################### Test Compound RF Model #####################################
# Provide optimal parameters for the final model
best_mtry <- c()
best_minn <- c()
best_mtry[1] <- #2 days lag
best_minn[1] <-
best_mtry[2] <- #7 days lag
best_minn[2] <- 
best_mtry[3] <- #14 days lag
best_minn[3] <- 
best_mtry[4] <- #180 days lag
best_minn[4] <- 

vars_lag = c(2, 7, 14, 180) #2 days, 1 wk, 2 wk, 6 mo
set.seed(3939)
doParallel::registerDoParallel(32)
for(i in 1:length(vars_lag)){
    # Reload and split Lchl data: MAKE SURE SEED IS SAME FROM TRAINING
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

    # Create final model specification
    final_spec <- rand_forest(
      mtry = best_mtry[i], #number of variables sampled
      trees = n_trees, #number of decision trees
      min_n = best_minn[i]) %>% #min number of datapoints for node to split
      set_mode("classification") %>%
      set_engine("ranger")

    # Use testing data in model
    final_wf <- workflow() %>%
      add_recipe(cmp_rec) %>%
      add_model(final_rf)
    final_res <- final_wf %>%
      last_fit(cmp_split)
    final_res %>%
      collect_metrics()

    # Produce confusion matrix
    pdf(gsub(" ", "", paste("cmpndFigs/confMatCmpnd",vars_lag[i],"Lag",n_trees,"T.pdf")))
    final_res %>%
      collect_predictions() %>%
      conf_mat(cmpCat, .pred_class) %>%
      autoplot(type = "heatmap")
    dev.off()
    }
doParallel::stopImplicitCluster()