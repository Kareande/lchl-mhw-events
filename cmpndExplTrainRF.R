setwd("/home/kareande/lchl-mhw-events")
library("ranger") #randomForest package
library("tidymodels") #tidyverse models
library("foreach") #parallel processing
library("doParallel") #parallel processing
library("ggplot2") #aesthetic plotting
#install.packages()

##################################### Exploratory Train LChl RF #####################################
# Prepare processed Lchl data
file_name <- paste("cmpnd_balanced_df.csv")
workingset <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)), header=TRUE)
workingset <- workingset[,c(-9,-18,-19)] #remove the lchl, mhwCat, and lchlCat columns
workingset[workingset$cmpCat == 3,]$cmpCat="Compound"
workingset[workingset$cmpCat == 2,]$cmpCat="LChl Event"
workingset[workingset$cmpCat == 1,]$cmpCat="MHW Event"
workingset[workingset$cmpCat == 0,]$cmpCat="No event"
workingset$cmpCat = as.factor(workingset$cmpCat)

head(workingset, n=10)

# Split data into training and testing sets
set.seed(3939)
cmp_split <- initial_split(workingset, strata = cmpCat)
cmp_train <- training(cmp_split)
cmp_test <- testing(cmp_split)
cmp_rec <- recipe(cmpCat ~ ., data = cmp_train)

head(cmp_train, n=10)

# Designate number of trees
n_trees <- 200

# Create model specification
tune_spec <- rand_forest(
  mtry = tune(), #number of variables sampled
  trees = n_trees, #change number of trees
  min_n = tune(), #min number of datapoints for node to split
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_wf <- workflow() %>%
  add_recipe(cmp_rec) %>%
  add_model(tune_spec)

# Training model
doParallel::registerDoParallel(8)
start_time <- Sys.time()
cmp_folds <- vfold_cv(cmp_train)

tune_res <- tune_grid(
  tune_wf,
  resamples = cmp_folds,
  grid = 10)

end_time <- Sys.time()
doParallel::stopImplicitCluster()
time_diff <- end_time - start_time

time_diff
tune_res

# Visualize results of k-fold analysis; accuracy
pdf(gsub(" ", "", paste("cmpndFigs/preTrainCmpnd",n_lag,"Lag",n_trees,"T.pdf")))

tune_res %>%
  collect_metrics() %>%
  filter(.metric == "accuracy") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "Accuracy")

dev.off()

print("#############################################################
	#############################################################

	Initial Training of RF model complete. Results saved as PDF.
	
	#############################################################
	#############################################################")
