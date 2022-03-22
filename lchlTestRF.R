setwd("/home/kareande/lchl-mhw-events")
library("ranger") #randomForest package
library("tidymodels") #tidyverse models
library("foreach") #parallel processing
library("doParallel") #parallel processing
library("ggplot2") #aesthetic plotting
library("plot.matrix") #confusion matrix
#install.packages()

##################################### Test LChl RF Model #####################################
# Reload and split Lchl data: MAKE SURE SEED IS SAME FROM TRAINING
set.seed(5)
file_name <- paste("chl3_balanced_df.csv")
workingset <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)), sep=",", header=TRUE)
workingset <- workingset[,-9] #remove lchl category
workingset[workingset$lchlCat == 0,]$lchlCat <- "negligable"
workingset[workingset$lchlCat == 1,]$lchlCat <- "moderate"
workingset[workingset$lchlCat == 2,]$lchlCat <- "severe"
workingset$lchlCat = as.factor(workingset$lchlCat)

lchl_train <- workingset[sample(nrow(workingset), 633346), ]
remove_r <- which(lchl_train == TRUE)
lchl_test <- workingset[-remove_r, ]
lchl_split <- initial_split(lchl_test, strata = lchlCat)
lchl_test <- testing(lchl_split)
lchl_rec <- recipe(lchlCat ~ ., data = lchl_train)

head(lchl_test)

# Create final model specification
n_trees <- 200
best_mtry <- 3
best_minn <- 10

final_spec <- rand_forest(
  mtry = best_mtry, #number of variables sampled
  trees = n_trees, #number of trees
  min_n = best_minn #min number of datapoints for node to split
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

# Use testing data in final model
final_wf <- workflow() %>%
  add_recipe(lchl_rec) %>%
  add_model(final_spec)

final_res <- final_wf %>%
  last_fit(lchl_split)

final_res %>%
  collect_metrics()
    
# Produce confusion matrix
pdf(gsub(" ", "", paste("cmpndFigs/confMatLchl",n_trees,"T.pdf")))

final_res %>%
  collect_predictions() %>%
  conf_mat(lchlCat, .pred_class) %>%
  autoplot(type = "heatmap")

dev.off()

print("#############################################################
        #############################################################

        Testing of RF Model complete. Confusion matrix saved as PDF.

        #############################################################
        #############################################################")
