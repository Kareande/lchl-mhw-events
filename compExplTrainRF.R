##################################### Exploratory Train LChl RF #####################################
# Prepare processed Lchl data
workingset=read.csv("comp_balanced_df.csv", header=TRUE)
workingset <- workingset[,c(-9,-18,-19)] #remove the lchl, mhwCat, and lchlCat columns
workingset[workingset$compCat == 3,]$compCat="Compound"
workingset[workingset$compCat == 2,]$compCat="LChl Event"
workingset[workingset$compCat == 1,]$compCat="MHW Event"
workingset[workingset$compCat == 0,]$compCat="No event"
workingset$compCat = as.factor(workingset$compCat)

head(workingset, n=10)

# Split data into training and testing sets
set.seed(3939)
comp_split <- initial_split(workingset, strata = compCat)
comp_train <- training(comp_split)
comp_test <- testing(comp_split)
comp_rec <- recipe(compCat ~ ., data = comp_train)

head(comp_train, n=10)

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
  add_recipe(comp_rec) %>%
  add_model(tune_spec)

# Training model
doParallel::registerDoParallel(8)
start_time <- Sys.time()
comp_folds <- vfold_cv(comp_train)

tune_res <- tune_grid(
  tune_wf,
  resamples = comp_folds,
  grid = 10)

end_time <- Sys.time()
doParallel::stopImplicitCluster()
end_time - start_time

tune_res

# Visualize results of k-fold analysis; accuracy
pdf(gsub(" ", "", paste("/auto/home/kareande/lchl-mhw-events/cmpndFigs/preTrainCompNoChl",n_trees,"T.pdf")))

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


print("################################################################
	################################################################

	Initial Training of RF model complete. Results saved as .pdf
	
	################################################################
	################################################################")
