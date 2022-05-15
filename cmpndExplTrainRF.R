setwd("/home/kareande/lchl-mhw-events")
library("ranger") #randomForest package
library("tidymodels") #tidyverse models
library("foreach") #parallel processing
library("doParallel") #parallel processing
library("ggplot2") #aesthetic plotting
#install.packages()

##################################### Exploratory Train LChl RF #####################################
# Load training data
file_name <- gsub(" ", "", paste("cmpnd_trn.csv"))
cmp_train <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)),sep=",") #get training df
cmp_train[cmp_train$cmpCat == 3,]$cmpCat="Compound"
cmp_train[cmp_train$cmpCat == 2,]$cmpCat="LChl Event"
cmp_train[cmp_train$cmpCat == 1,]$cmpCat="MHW Event"
cmp_train[cmp_train$cmpCat == 0,]$cmpCat="No event"
cmp_train$cmpCat = as.factor(cmp_train$cmpCat)

# Create model specification
n_trees <- 200
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
png(gsub(" ", "", paste("cmpndFigs/preTrainCmpnd",n_trees,"T.png")))
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

