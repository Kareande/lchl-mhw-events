setwd("/auto/home/kareande/mhwData")
library(randomForest)
library(ranger)
library(vip)
library(tidymodels)

##################################### Define LChl categories #####################################
# Get unbalanced and uncategorized lchl df
lchl_df <- read.csv("master_with_contime_df.csv", sep=",", header=TRUE)
lchl_df <- na.omit(lchl_df) #remove NA values
head(lchl_df)

# Select chl column
chl_col <- lchl_df[,9] 

# Define LChl categories
cat1 <- quantile(chl_col,probs=0.1) #what value is the 10th percentile cutoff, or category 1 minimum value
cat2 <- quantile(chl_col,probs=0.075) #category 2 minimum
cat3 <- quantile(chl_col,probs=0.05) #category 3 minimum
cat4 <- quantile(chl_col,probs=0.025) #category 4 minimum
cat1 #0.37448388308533
cat2 #0.302988503263755
cat3 #0.237521889263933
cat4 #0.177686934912008

# Check chl values
max(chl_col) #350.995544433594 mg/m^3
min(chl_col) #9.74359052937264e-06 mg/m^3

# Divide LChl events into 3 cats
lchl_cat3 <- vector() #create empty vector to fill with categories
x <- length(chl_col) #get number of datapoints
#doParallel::registerDoParallel()
for(i in 1:x) { #loop to fill out categories
    if (is.na(chl_col[i])) { #if value i of chl column is NA,
        lchl_cat3[i] = 0       #then the category is 0
    } else if(chl_col[i] <= cat3) { #if value i of chl column is <= the bottom 5%,
        lchl_cat3[i] = 2       #then the category is 2
    } else if(chl_col[i] <= cat1) { #if chl value is <= the bottom 10%,
        lchl_cat3[i] = 1              #then the category is 1
    } else {           #if the chl value is > than the 10th percentile,
        lchl_cat3[i] = 0 #then the category is 0
    }
        }
#doParallel::stopImplicitCluster()
length(lchl_cat3) #5324099
nrow(lchl_df) #5324099

# Add category column to df w/ 3 cats
lchl3_df <- cbind(lchl_df,lchl_cat3)
colnames(lchl3_df) <- c("day","mo","yr","lon","lat","nit","oxy","pho","chl","sil","npp","lchlCat")

# Save df w/ 3 cats
csvfile <- "chl3_unbalanced_df.csv"
write.table(lchl3_df,csvfile,sep=",")

# Divide LChl events into 5 cats
#lchl_cat5 <- vector() #create empty vector to fill with categories
#x <- length(chl_col) #get number of datapoints
#doParallel::registerDoParallel()
#for(i in 1:x) { #loop to fill out categories
#    if (is.na(chl_col[i])) { #if value i of chl column is NA,
#        lchl_cat5[i] = 0       #then the category is 0
#    } else if(chl_col[i] <= cat4) { #if value i of chl column is <= the category 4 cut-off,
#        lchl_cat5[i] = 4            #then the category is 4
#    } else if(chl_col[i] <= cat3) { #if chl value is <= category 3 cutoff,
#        lchl_cat5[i] = 3            #then the category is 3
#    } else if(chl_col[i] <= cat2) { #if chl value is <= category 2 cutoff,
#        lchl_cat5[i] = 2            #then the category is 2
#    } else if(chl_col[i] <= cat1) { #if chl value is <= category 1 cutoff,
#        lchl_cat5[i] = 1            #then the category is 1
#    } else {           #if the chl value is > than the 10th percentile,
#        lchl_cat5[i] = 0 #then the category is 0
#    }
#        }
#doParallel::stopImplicitCluster()
#length(lchl_cat5) #5324099
#nrow(lchl_df) #5324099

# Add category column to df w/ 5 cats
#lchl5_df <- cbind(lchl_df,lchl_cat5)
#colnames(lchl5_df) <- c("day","mo","yr","lon","lat","nit","oxy","pho","chl","sil","npp","lchlCat")

# Save df w/ 5 cats
#csvfile <- "chl5_unbalanced_df.csv"
#write.table(lchl5_df,csvfile,sep=",")


##################################### Balance LChl dataset #####################################
# Read unbalanced lchl df w/ multiple cats
lchl_df <- read.csv("chl3_unbalanced_df.csv", sep=",", header=TRUE)
head(lchl_df)

# Find the percentages of each category
cat0 <- nrow(subset(lchl_df, lchlCat=="0")) #4791689
cat1 <- nrow(subset(lchl_df, lchlCat=="1")) #266205
cat2 <- nrow(subset(lchl_df, lchlCat=="2")) #266205
catsTot <- cat0 +cat1 + cat2 #5324099

cat0_per <- (cat0/(catsTot))*100 #89.9999981217479
cat1_per <- (cat1/(catsTot))*100 #5.00000093912604
cat2_per <- (cat2/(catsTot))*100 #5.00000093912604
cat0_per
cat1_per
cat2_per

# Isolate the most recent datapoints for viewing
lchl_2019_df <- (subset(lchl_df, yr=="2019")) #Create df of all 2019 datapoints, length 185997
lchl_2019_cat0_len <- nrow(subset(lchl_2019_df, lchlCat=="0")) #cat0 events in 2019, 173509
lchl_2019_cat1_len <- nrow(subset(lchl_2019_df, lchlCat=="1")) #cat1 events in 2019, 7839
lchl_2019_cat2_len <- nrow(subset(lchl_2019_df, lchlCat=="2")) #cat2 events in 2019, 4649

# Randomly remove n rows where year<2019 and cat=="0" to balance dataset
lchl_x19_df <- lchl_df[lchl_df$yr !=2019, ] #length 5138102
find.button.5 <- lchl_x19_df$lchlCat == 0 # find row numbers where button == 5
perc.30 <- round(sum(find.button.5) * 0.97) # find 30% of button 5
button.5 <- which(find.button.5 == TRUE)
sampled.30 <- sample(button.5, perc.30) # row numbers of 30% of button 5
lchl_bal_df <- lchl_df[-sampled.30, ] # in your final output, include all but the 30%

# Verify the percentages of each category; negligible (0), moderate (1), severe (2)
bal_cat0 <- nrow(subset(lchl_bal_df, lchlCat=="0")) # 312054
bal_cat1 <- nrow(subset(lchl_bal_df, lchlCat=="1")) # 266205
bal_cat2 <- nrow(subset(lchl_bal_df, lchlCat=="2")) # 266205
bal_catsTot <- bal_cat0 + bal_cat1 + bal_cat2 # 844464

bal_cat0_per <- (bal_cat0/(bal_catsTot))*100 # 36.9529074063548
bal_cat1_per <- (bal_cat1/(bal_catsTot))*100 # 31.5235462968226
bal_cat2_per <- (bal_cat2/(bal_catsTot))*100 # 31.5235462968226
bal_cat0_per
bal_cat1_per
bal_cat2_per

# Save balanced compound event df
csvfile <- "chl3_balanced_df.csv"
write.table(lchl_bal_df,csvfile,sep=",")
    

##################################### Exploratory Train LChl RF #####################################
# Prepare processed Lchl data
workingset=read.csv("chl3_balanced_df.csv", header=TRUE)
workingset[workingset$lchlCat == 0,]$lchlCat="negligable"
workingset[workingset$lchlCat == 1,]$lchlCat="moderate"
workingset[workingset$lchlCat == 2,]$lchlCat="severe"
#workingset[workingset$lchlCat == 2,]$lchlCat="extreme"
workingset$lchlCat = as.factor(workingset$lchlCat)

head(workingset, n=10)

# Split data into training and testing sets
set.seed(3939)
lchl_split <- initial_split(workingset, strata = lchlCat)
lchl_train <- training(lchl_split)
lchl_test <- testing(lchl_split)
lchl_rec <- recipe(lchlCat ~ ., data = lchl_train)

head(lchl_train, n=10)

# Create model specification
tune_spec <- rand_forest(
  mtry = tune(),
  trees = 200, #change number of trees
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_wf <- workflow() %>%
  add_recipe(lchl_rec) %>%
  add_model(tune_spec)

# Training model
doParallel::registerDoParallel()
start_time <- Sys.time()
lchl_folds <- vfold_cv(lchl_train)

tune_res <- tune_grid(
  tune_wf,
  resamples = lchl_folds,
  grid = 10)

end_time <- Sys.time()
doParallel::stopImplicitCluster()
end_time - start_time

tune_res

# Visualize results of k-fold analysis; accuracy
pdf("/auto/home/kareande/lchl-mhw-events/mtryMinN200Acc.pdf")

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

# Visualize results of k-fold analysis; AUC
pdf("/auto/home/kareande/lchl-mhw-events/mtryMinN200AUC.pdf")
tune_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

dev.off()

tune_res

    
##################################### Refined Train LChl RF #####################################
# New model specs for re-training based on previous results
mtry_min_range <- 9
mtry_max_range <- 11
n_min_range <- 1
n_max_range <- 10

rf_grid <- grid_regular(
  mtry(range = c(mtry_min_range, mtry_max_range)),
  min_n(range = c(n_min_range, n_max_range)),
  levels = 10
)

rf_grid
    
# Train refined models
start_time <- Sys.time()
doParallel::registerDoParallel()

regular_res <- tune_grid(
  tune_wf,
  resamples = lchl_folds,
  grid = rf_grid
)

end_time <- Sys.time()
doParallel::stopImplicitCluster()
end_time - start_time

save(regular_res, file="200TRangerTune2.RData")
    
# Deciding on best model, roc_auc or accuracy
best_rf <- select_best(regular_res, "roc_auc")

final_rf <- finalize_model(
  tune_spec,
  best_rf
)

final_rf

lchl_prep <- prep(lchl_rec)
juiced <- juice(lchl_prep)

# Check variable importance for training data
pdf("/auto/home/kareande/workspace/VarImpor200T.pdf")

library(vip)
final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(lchlCat ~ .,
    data = juice(lchl_prep)
  ) %>%
  vip(geom = "point")

dev.off()
    
    
##################################### Test LChl RF Model #####################################
# Use testing data in model
final_wf <- workflow() %>%
  add_recipe(lchl_rec) %>%
  add_model(final_rf)

final_res <- final_wf %>%
  last_fit(lchl_split)

final_res %>%
  collect_metrics()

# Add column to data with "Correct" and "Incorrect" predictions

final_res %>%
  collect_predictions() %>%
  mutate(correct = case_when(
    lchlCat == .pred_class ~ "Correct",
    TRUE ~ "Incorrect"
  )) %>%
  bind_cols(lchl_test)

final_res