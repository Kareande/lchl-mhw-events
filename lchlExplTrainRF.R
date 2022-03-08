##################################### Exploratory Train LChl RF #####################################
# Prepare processed Lchl data
file_name <- paste("chl3_balanced_df.csv")
file_path <- gsub(" ", "", paste("/home/kareande/mhwData/",file_name)) #csv file
workingset <- read.csv(file_path, sep=",", header=TRUE) #load file
workingset <- workingset[,-9] #remove lchl category
workingset[workingset$lchlCat == 0,]$lchlCat <- "negligable"
workingset[workingset$lchlCat == 1,]$lchlCat <- "moderate"
workingset[workingset$lchlCat == 2,]$lchlCat <- "severe"
workingset$lchlCat = as.factor(workingset$lchlCat)

head(workingset, n=10)

# Split data into training and testing sets (broken)
# Causes training data only contain category "moderate"
#set.seed(5)
#lchl_split <- initial_split(workingset, strata = lchlCat)
#lchl_train <- training(lchl_split)
#lchl_test <- testing(lchl_split)

# Split data into training and testing sets
set.seed(5)
lchl_train <- workingset[sample(nrow(workingset), 633346), ]
remove_r <- which(lchl_train == TRUE)
lchl_test <- workingset[-remove_r, ]
lchl_test <- lchl_test[sample(nrow(lchl_test), 211118), ]
lchl_rec <- recipe(lchlCat ~ ., data = lchl_train)

head(lchl_train)
unique(lchl_train$lchlCat)

# Create model specification using vip package and ranger engine
tune_spec <- rand_forest(
  mtry = tune(), #number of variables sampled
  trees = 200, #number of trees
  min_n = tune() #min number of datapoints for node to split
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

lchl_folds <- vfold_cv(lchl_train)

tune_wf <- workflow() %>%
  add_recipe(lchl_rec) %>%
  add_model(tune_spec)

# Training model
doParallel::registerDoParallel(8)
system.time({
    tune_res <- tune_grid(
      tune_wf,
      resamples = lchl_folds,
      grid = 10)
})
doParallel::stopImplicitCluster()

tune_res

# Visualize results of k-fold analysis training
pdf(gsub(" ", "", paste("/cmpndFigs/preTrainLchl",n_trees,"T.pdf")))

tune_res %>%
  collect_metrics() %>%
  filter(.metric == "accuracy") %>% #or roc_auc
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

tune_res


print("#############################################################
        #############################################################

        Initial Training of RF model complete. Results saved as PDF.

        #############################################################
        #############################################################")
