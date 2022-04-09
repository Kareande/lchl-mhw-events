setwd("/home/kareande/lchl-mhw-events")
library("ranger") #randomForest package
library("tidymodels") #tidyverse models
library("foreach") #parallel processing
library("doParallel") #parallel processing
library("ggplot2") #aesthetic plotting
library("plot.matrix") #confusion matrix
library("caret") #proportional matrix
#install.packages()

##################################### Test Compound RF Model #####################################
# Provide optimal parameters for the final model
best_mtry <- c()
best_minn <- c()
best_mtry[1] <- 26 #2 days lag
best_minn[1] <- 2
best_mtry[2] <- 5 #7 days lag
best_minn[2] <- 1
best_mtry[3] <- 6 #14 days lag
best_minn[3] <- 1
best_mtry[4] <- 7 #180 days lag
best_minn[4] <- 1
best_mtry[5] <- 4 #365 days lag
best_minn[5] <- 1
best_mtry[6] <- 11 #730 days lag
best_minn[6] <- 1

vars_lag = c(2, 7, 14, 180, 365, 730) #2 days, 1 wk, 2 wk, 6 mo
n_trees <- 200
set.seed(3939) #SET SEED AS SAME FROM TRAINING
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
    cmp_rec <- recipe(cmpCat ~ ., data = cmp_test)

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
      add_model(final_spec)
    final_res <- final_wf %>%
      last_fit(cmp_split)
    final_res %>%
      collect_metrics()

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
         fmt.key="%.3f",)
    print(pltimg)
    dev.off()
    rm(list= ls()[!(ls() %in% c('best_mtry','best_minn','vars_lag','n_trees'))])
    }
doParallel::stopImplicitCluster()