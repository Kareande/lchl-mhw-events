setwd("/home/kareande/lchl-mhw-events")
library("ranger") #randomForest package
library("tidymodels") #tidyverse models
library("foreach") #parallel processing
library("doParallel") #parallel processing
library("ggplot2") #aesthetic plotting
library("plot.matrix") #confusion matrix
#install.packages()

##################################### Test Compound RF Model #####################################
# Reload and split Lchl data: MAKE SURE SEED IS SAME FROM TRAINING
set.seed(3939)
file_name <- paste("cmpnd_balanced_df.csv")
workingset <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)), header=TRUE)
workingset <- workingset[,c(-9,-18,-19)] #remove the lchl, mhwCat, and lchlCat columns
workingset[workingset$cmpCat == 3,]$cmpCat="Compound"
workingset[workingset$cmpCat == 2,]$cmpCat="LChl Event"
workingset[workingset$cmpCat == 1,]$cmpCat="MHW Event"
workingset[workingset$cmpCat == 0,]$cmpCat="No event"
workingset$cmpCat = as.factor(workingset$cmpCat)

cmp_split <- initial_split(workingset, strata = cmpCat)
cmp_train <- training(cmp_split)
cmp_test <- testing(cmp_split)
cmp_rec <- recipe(cmpCat ~ ., data = cmp_train)

head(cmp_test)

# Create final model specification
n_trees <- 200
best_mtry <- 6
best_minn <- 2

final_spec <- rand_forest(
  mtry = best_mtry, #number of variables sampled
  trees = n_trees, #number of decision trees
  min_n = best_minn, #min number of datapoints for node to split
) %>%
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
pdf(gsub(" ", "", paste("cmpndFigs/confMatCmpnd",n_lag,"Lag",n_trees,"T.pdf")))

final_res %>%
  collect_predictions() %>%
  conf_mat(cmpCat, .pred_class) %>%
  autoplot(type = "heatmap")

dev.off()

print("#############################################################
	#############################################################
	
	Testing of RF Model complete. Confusion matrix saved as .pdf.

	#############################################################
	#############################################################")
