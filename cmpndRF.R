## The nature of this codebase requires the code to be run in pieces. This entire script
## is avaliable for reference, but it is broken up into data processing, exploratory
## training, refined training, and testing. Those individual scripts can be run as-is.

setwd("/home/kareande/lchl-mhw-events")
library("foreach") #parallel processing
library("doParallel") #parallel processing
#install.packages()

##################################### Define LChl categories #####################################
# Get unbalanced and uncategorized lchl df
file_name <- paste("chl_unprocessed.csv") #name of df
lchl_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)), sep=",", header=TRUE) #read df path
lchl_df <- na.omit(lchl_df) #remove NA values
head(lchl_df)

# Select chl column
chl_col <- lchl_df[,9] 

# Calculate percentiles
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

# Divide LChl events into 2 categories
lchl_cat <- vector() #create empty vector to fill with categories
x <- length(chl_col) #get number of datapoints

doParallel::registerDoParallel()
for(i in 1:x) { #loop to fill out categories
    if (is.na(chl_col[i])) { #if value i of chl column is NA,
        lchl_cat[i] = 0       #then the category is 0
    } else if(chl_col[i] <= cat1) { #if chl value is <= the bottom 10%,
        lchl_cat[i] = 1              #then the category is 1
    } else {           #if the chl value is > than the 10th percentile,
        lchl_cat[i] = 0 #then the category is 0
    }
        }
doParallel::stopImplicitCluster()

# Add category column to df
lchl_df <- cbind(lchl_df,lchl_cat)
colnames(lchl_df) <- c("day","mo","yr","lon","lat","nit","oxy","pho","chl","sil","npp","lchlCat")

# Save df
file_name <- paste("chl_unbalanced.csv")
file_path <- gsub(" ", "", paste("cmpndData/",file_name))
write.table(lchl_df,file_path,sep=",")


##################################### Combine LChl and MHW dataframes #####################################
# Get LChl df
file_name <- paste("chl_unbalanced.csv")
lchl_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)), sep=",", header=TRUE)
head(lchl_df)
#tail(lchl_df)

# Get MHW df
file_name <- paste("mhw_unbalanced.csv")
mhw_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)), sep=",", header=TRUE)
mhw_df$mhwCat[mhw_df$mhwCat > 0] <- 1 #condense MHW cats into two categories (presence or absence)
head(mhw_df)
#tail(mhw_df)

# Correct longitudes for lchl df
lchl_df$lon <- abs(lchl_df$lon)

# Correct ranges of lat/lon to be equal on both datasets
lchl_df <- lchl_df[lchl_df$lon<170, ] #length 5138102
mhw_df <- mhw_df[mhw_df$lon>=102.5, ] #length 5138102
lchl_df <- lchl_df[lchl_df$lat>10.625, ] #length 5138102

# Find range of lon and lat for each df (verify same range)
print(paste("lchl_df longitudes goes from",toString(min(lchl_df$lon)),"to",toString(max(lchl_df$lon)))) #102.5 to 167.5
print(paste("mhw_df longitudes goes from",toString(min(mhw_df$lon)),"to",toString(max(mhw_df$lon)))) #102.5 to 167.5
print(paste("lchl_df latitudes goes from",toString(min(lchl_df$lat)),"to",toString(max(lchl_df$lat)))) #12.5 to 62.5
print(paste("mhw_df latitudes goes from",toString(min(mhw_df$lat)),"to",toString(max(mhw_df$lat)))) #12.5 to 62.5

# Extract unique values of lon and lat (veryify same values)
unique(lchl_df$lon) #27 vals
unique(mhw_df$lon) #27 vals 
unique(lchl_df$lat) #21 vals
unique(mhw_df$lat) #20 vals, missing 60.0

# Combine dfs into compound df
cmp_df <- merge(lchl_df, mhw_df, c("day","mo","yr","lon","lat"))
colnames(cmp_df) <- c("day","mo","yr","lon","lat","nit","oxy","pho","chl","sil","npp","lchlCat",
                       "qnet","slp","sat","wndSp","sst","sstRoC","mhwCat")
lchlCat <- cmp_df$lchlCat #isolate the lchl category column
mhwCat <- cmp_df$mhwCat #isolate the mhw cat column
cmp_df <- cmp_df[,-12] #remove lchl from current spot
cmp_df <- cmp_df[,-18] #remove mhw from current spot
cmp_df <- cbind(cmp_df, lchlCat, mhwCat) #put the category columns at end of dataset
head(cmp_df)
nrow(cmp_df)

# Save unbalanced compound df
cmp_df <- na.omit(cmp_df) #remove NA values
file_name <- paste("cmpnd_unbalanced.csv")
write.table(cmp_df,gsub(" ", "", paste("cmpndData/",file_name)),sep=",")


##################################### Add Compound Cats #####################################
# Get unbalanced, uncategorized compound df
file_name <- paste("cmpnd_unbalanced.csv")
cmp_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)), sep=",", header=TRUE)
chl_col <- cmp_df[,18] #select chl cat column
mhw_col <- cmp_df[,19] #select mhw cat column

# Create a compound category column of either no events (0), mhw event (1), lchl event (2), compound event (3)
cmp_cat <- vector() #create empty vector to fill with categories
x <- length(chl_col) #get number of datapoints
doParallel::registerDoParallel()
for(i in 1:x) { #loop to fill out categories
    if (is.na(chl_col[i]) || is.na(mhw_col[i])) { #if value i of chl column OR mhw column is NA,
        cmp_cat[i] = 0                           #then the category is 0
    } else if(chl_col[i]==1 && mhw_col[i]==1) { #if chl cat AND mhw cat is 1,
        cmp_cat[i] = 3                         #then the category is 3
    } else if(mhw_col[i]==1) { #if only the mhw cat is 1,
        cmp_cat[i] = 1        #then the category is 1
    } else if(chl_col[i]==1) { #if only the chl cat is 1,
        cmp_cat[i] = 2        #then the category is 2
    } else {            #if the chl or mhw cat is not 1,
        cmp_cat[i] = 0 #then the category is 0
    }
        }
doParallel::stopImplicitCluster()
length(cmp_cat) #2718254

# Create a compound category column of either both events (1) or not (0)
#cmp_cat <- vector() #create empty vector to fill with categories
#x <- length(chl_col) #get number of datapoints
#doParallel::registerDoParallel()
#for(i in 1:x) { #loop to fill out categories
#    if (is.na(chl_col[i]) || is.na(mhw_col[i])) { #if value i of chl column OR mhw column is NA,
#        cmp_cat[i] = 0                           #then the category is 0
#    } else if(chl_col[i]==1 && mhw_col[i]==1) { #if chl cat AND mhw cat is 1,
#        cmp_cat[i] = 1                         #then the category is 1
#    } else {            #if the chl or mhw cat is not 1,
#        cmp_cat[i] = 0 #then the category is 0
#    }
#        }
#doParallel::stopImplicitCluster()
#length(lchl_cat)

# Add compound cats to df
cmp_df <- cbind(cmp_df,cmp_cat)
colnames(cmp_df) <- c("day","mo","yr","lon","lat","nit","oxy","pho","chl","sil","npp",
                       "qnet","slp","sat","wndSp","sst","sstRoC","lchlCat","mhwCat","cmpCat")
head(cmp_df)

# Save df with compound event categories
file_name <- paste("cmpnd_unbalanced.csv")
write.table(cmp_df,gsub(" ", "", paste("cmpndData/",file_name)),sep=",")


##################################### Balance Compound DF #####################################
# Get df with mhw and lchl compound events
file_name <- paste("cmpnd_unbalanced.csv")
cmp_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)), sep=",", header=TRUE)
head(cmp_df)
#tail(cmp_df)

# Find the unbalanced percentages of each category; no events (0), mhw event (1), lchl event (2), compound event (3)
cmp_ev <- nrow(subset(cmp_df, cmpCat=="3")) #number of compound events, 31474
chl_ev <- nrow(subset(cmp_df, cmpCat=="2")) #lchl only events, 406426
mhw_ev <- nrow(subset(cmp_df, cmpCat=="1")) #mhw only events, 232268
no_ev <- nrow(subset(cmp_df, cmpCat=="0")) #no events, 2048086
tot_ev <- cmp_ev+chl_ev+mhw_ev+no_ev #total events, 2718254

cmp_ev_per <- (cmp_ev/tot_ev)*100 #1.15% compound events (3)
chl_ev_per <- (chl_ev/tot_ev)*100 #14.9% lchl only (2)
mhw_ev_per <- (mhw_ev/tot_ev)*100 #8.54% mhw only (1)
no_ev_per <- (no_ev/tot_ev)*100 #75.34% no event (0)
cmp_ev_per
chl_ev_per
mhw_ev_per
no_ev_per
    
# Calculate how many rows of each category to remove
cmp_x19_df <- subset(cmp_df, yr!=2019) #no 2019
cmp_19_df <- subset(cmp_df, yr==2019) #only 2019

x19_cmp_ev <- nrow(subset(cmp_x19_df, cmpCat=="3")) #28594, least observations
x19_chl_ev <- nrow(subset(cmp_x19_df, cmpCat=="2")) #399222
x19_mhw_ev <- nrow(subset(cmp_x19_df, cmpCat=="1")) #197840
x19_no_ev <- nrow(subset(cmp_x19_df, cmpCat=="0")) #1997636

w19_cmp_ev <- nrow(subset(cmp_19_df, cmpCat=="3")) #2880, least observations
w19_chl_ev <- nrow(subset(cmp_19_df, cmpCat=="2")) #7204
w19_mhw_ev <- nrow(subset(cmp_19_df, cmpCat=="1")) #34428
w19_no_ev <- nrow(subset(cmp_19_df, cmpCat=="0")) #50450

cat3_tot = x19_cmp_ev + w19_cmp_ev #31474
n_2rows_remov = x19_chl_ev + w19_chl_ev - cat3_tot #how many rows to remove from cat2
n_1rows_remov = x19_mhw_ev + w19_mhw_ev - cat3_tot #how many rows to remove from cat1
n_0rows_remov = x19_no_ev + w19_no_ev - cat3_tot #how many rows to remove from cat0
    
# Isolate and remove n rows where year<=2019 and cmpCat==0
set.seed(313)
cmp_x19_df <- subset(cmp_df, yr!=2019) #exclude 2019
conditions <- cmp_df$cmpCat==0 #set conditions for removed rows, cat==0
rows_remov <- which(conditions==TRUE) #only remove rows where conditions true
n_rows_remov <- n_0rows_remov
sampled.cats <- sample(rows_remov, n_rows_remov) #sample to remove
cmp_bal_df <- cmp_df[-sampled.cats, ]
    
# Isolate and remove n rows where year<=2019 and cmpCat==2
cmp_x19_df <- subset(cmp_bal_df, yr!=2019)
conditions <- cmp_bal_df$cmpCat==2 #set conditions for removed rows, cat==2
rows_remov <- which(conditions==TRUE) #only remove rows where conditions true
n_rows_remov <- n_2rows_remov
sampled.cats <- sample(rows_remov, n_rows_remov)
cmp_bal_df <- cmp_bal_df[-sampled.cats, ]
    
# Isolate and remove n rows where year<=2019 and cmpCat==1
cmp_x19_df <- subset(cmp_bal_df, yr!=2019)
conditions <- cmp_bal_df$cmpCat==1 #set conditions for removed rows, cat==1
rows_remov <- which(conditions==TRUE) #only remove rows where conditions true
n_rows_remov <- n_1rows_remov
sampled.cats <- sample(rows_remov, n_rows_remov)
cmp_bal_df <- cmp_bal_df[-sampled.cats, ]

# Find the balanced percentages of each category; no events (0), mhw event (1), lchl event (2), compound event (3)
bal_cmp_ev <- nrow(subset(cmp_bal_df, cmpCat=="3")) #number of compound events, 31474
bal_chl_ev <- nrow(subset(cmp_bal_df, cmpCat=="2")) #lchl only events, 31474
bal_mhw_ev <- nrow(subset(cmp_bal_df, cmpCat=="1")) #mhw only events, 31474
bal_no_ev <- nrow(subset(cmp_bal_df, cmpCat=="0")) #no events, 31474
bal_tot_ev <- bal_cmp_ev+bal_chl_ev+bal_mhw_ev+bal_no_ev #125896 total observations

bal_cmp_ev_per <- (bal_cmp_ev/bal_tot_ev)*100 #25% compound events (3)
bal_chl_ev_per <- (bal_chl_ev/bal_tot_ev)*100 #25% lchl only (2)
bal_mhw_ev_per <- (bal_mhw_ev/bal_tot_ev)*100 #25% mhw only (1)
bal_no_ev_per <- (bal_no_ev/bal_tot_ev)*100 #25% no event (0)
bal_cmp_ev_per
bal_chl_ev_per
bal_mhw_ev_per
bal_no_ev_per

# Save balanced compound df
file_name <- paste("cmpnd_balanced.csv")
write.table(cmp_bal_df,gsub(" ", "", paste("cmpndData/",file_name)),sep=",") #save balanced lchl dataset



##################################### Exploratory Train LChl RF #####################################
setwd("/home/kareande/lchl-mhw-events")
library("ranger") #randomForest package
library("tidymodels") #tidyverse models
library("foreach") #parallel processing
library("doParallel") #parallel processing
library("ggplot2") #aesthetic plotting
#install.packages()

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


##################################### Refined Train LChl RF #####################################
setwd("/home/kareande/lchl-mhw-events")
library("ranger") #randomForest package
library("vip") #variable importance plots
library("tidymodels") #tidyverse models
library("foreach") #parallel processing
library("doParallel") #parallel processing
library("ggplot2") #aesthetic plotting
#install.packages()

# Refine model specs for re-training based on previous results
#for 200 trees; mtry 1:9, min_n 3,6,7
n_min_range <- 2
n_max_range <- 9
mtry_min_range <- 6
mtry_max_range <- 7

rf_grid <- grid_regular(
  mtry(range = c(mtry_min_range, mtry_max_range)),
  min_n(range = c(n_min_range, n_max_range)),
  levels = 10
)

rf_grid

# Train models using refined specs
doParallel::registerDoParallel(8)
start_time <- Sys.time()

regular_res <- tune_grid(
  tune_wf,
  resamples = cmp_folds,
  grid = rf_grid
)

end_time <- Sys.time()
doParallel::stopImplicitCluster()
end_time - start_time

# Visualize refined results
pdf(gsub(" ", "", paste("cmpndFigs/refTrnCmpnd",n_lag,"Lag",n_trees,"T.pdf")))

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
  best_rf
)

final_rf #mtry = 6, min_n = 2 

# Check variable importance for training data
pdf(gsub(" ", "", paste("cmpndFigs/VarImpTrnCmpnd",n_lag,"Lag",n_trees,"T.pdf")))
cmp_prep <- prep(cmp_rec)
juiced <- juice(cmp_prep)

library(vip)
final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(cmpCat ~ .,
    data = juice(cmp_prep)
  ) %>%
  vip(geom = "point")

dev.off()



##################################### Test Compound RF Model #####################################
setwd("/home/kareande/lchl-mhw-events")
library("ranger") #randomForest package
library("tidymodels") #tidyverse models
library("foreach") #parallel processing
library("doParallel") #parallel processing
library("ggplot2") #aesthetic plotting
library("plot.matrix") #confusion matrix
#install.packages()

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

