## The nature of this codebase requires the code to be run in pieces. This entire script
## is avaliable for reference, but it is broken up into data processing, exploratory
## training, refined training, and testing. Those individual scripts can be run as-is.

setwd("/home/kareande/lchl-mhw-events")
library("foreach") #parallel processing
library("doParallel") #parallel processing
library("lubridate") #calculating day of year
#install.packages()

##################################### Define LChl categories #####################################
# Get unbalanced and uncategorized lchl df
file_name <- paste("chl_raw.csv") #name of df
lchl_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)), sep=",", header=TRUE) #read df path
lchl_df <- na.omit(lchl_df) #remove NA values
head(lchl_df)

# Calculate percentiles
cat1 <- quantile(lchl_df$chl,probs=0.1) #what value is the 10th percentile cutoff, or category 1 minimum value
cat2 <- quantile(lchl_df$chl,probs=0.075) #category 2 minimum
cat3 <- quantile(lchl_df$chl,probs=0.05) #category 3 minimum
cat4 <- quantile(lchl_df$chl,probs=0.025) #category 4 minimum
cat1 #0.37448388308533
cat2 #0.302988503263755
cat3 #0.237521889263933
cat4 #0.177686934912008

# Check chl values
max(lchl_df$chl) #350.995544433594 mg/m^3
min(lchl_df$chl) #9.74359052937264e-06 mg/m^3

# Divide LChl events into 2 categories
x <- length(lchl_df$chl) #get number of datapoints
doParallel::registerDoParallel()
for(i in 1:x) { #loop to fill out categories
    if (is.na(lchl_df$chl[i])) { #if value i of chl column is NA,
        lchl_df$lchlCat2[i] = 0       #then the category is 0
    } else if(lchl_df$chl[i] <= cat1) { #if chl value is <= the bottom 10%,
        lchl_df$lchlCat2[i] = 1              #then the category is 1
    } else {           #if the chl value is > than the 10th percentile,
        lchl_df$lchlCat2[i] = 0 #then the category is 0
    }
        }

# Divide LChl events into 5 cats
x <- length(lchl_df$chl) #get number of datapoints
doParallel::registerDoParallel()
for(i in 1:x) { #loop to fill out categories
    if (is.na(lchl_df$chl[i])) { #if value i of chl column is NA,
        lchl_df$lchlCat5[i] = 0       #then the category is 0
    } else if(lchl_df$chl[i] <= cat4) { #if value i of chl column is <= the category 4 cut-off,
        lchl_df$lchlCat5[i] = 4            #then the category is 4
    } else if(lchl_df$chl[i] <= cat3) { #if chl value is <= category 3 cutoff,
        lchl_df$lchlCat5[i] = 3            #then the category is 3
    } else if(lchl_df$chl[i] <= cat2) { #if chl value is <= category 2 cutoff,
        lchl_df$lchlCat5[i] = 2            #then the category is 2
    } else if(lchl_df$chl[i] <= cat1) { #if chl value is <= category 1 cutoff,
        lchl_df$lchlCat5[i] = 1            #then the category is 1
    } else {           #if the chl value is > than the 10th percentile,
        lchl_df$lchlCat5[i] = 0 #then the category is 0
    }
        }
doParallel::stopImplicitCluster()
head(lchl_df) #5324099

# Create date columns
lchl_df$date <- gsub(" ", "", paste(lchl_df$yr, "-", lchl_df$mo, "-",lchl_df$day)) #merge yr, mo, da columns
lchl_df <- lchl_df %>% relocate(date, .before = lon) #move date to be in front of lon
head(lchl_df)

# Calculate doy from date
names(lchl_df)[names(lchl_df) == "date"] <- "doy" #change name of date column
lchl_df$doy <- yday(lchl_df$doy) #calculate doy
lchl_df <- lchl_df[ , ! names(lchl_df) %in% c("day","mo")] #remove day and mo columns
head(lchl_df)

# Save df w/ chl cats
file_name <- paste("chl_categorized.csv")
write.table(lchl_df,gsub(" ", "", paste("cmpndData/",file_name)),sep=",")


############################# Split into Testing and Training DFs #############################
set.seed(313)
file_name <- gsub(" ", "", paste("chl_categorized.csv")) #create dyamic df name
chl_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)),sep=",") #get chl data
conditions <- chl_df$yr<2011 #set conditions for sampled rows
smpld_rows <- which(conditions==TRUE) #only sample rows where conditions true
n_smpld <- nrow(chl_df)*(2/3) #set how many rows to sample
sampled.cats <- sample(smpld_rows, n_smpld) #create sample
chl_train <- chl_df[sampled.cats,] #dataset with sample
chl_test <- chl_df[-sampled.cats, ] #dataset excluding sample
tst_name <- gsub(" ", "", paste("chl_tst_unlag.csv")) #create dyamic df name
trn_name <- gsub(" ", "", paste("chl_trn_unbal_unlag.csv")) #create dyamic df name
write.table(chl_test,gsub(" ", "", paste("cmpndData/",tst_name)),sep=",")
write.table(chl_train,gsub(" ", "", paste("cmpndData/",trn_name)),sep=",")

    
##################################### Balance Cat2 Training DF #####################################
# Read Chl2 training DF
file_name <- paste("chl_trn_unbal_unlag.csv")
lchl2_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)), sep=",", header=TRUE) #load file
lchl2_df <- lchl2_df[ , ! names(lchl2_df) %in% c("lchlCat5")] #remove lchlCat5
names(lchl_df)[names(lchl_df) == "lchlCat2"] <- "lchlCat" #change name of lchlCat2 column
    
# Find the percentages of each category
cat0 <- nrow(subset(lchl_df, lchlCat=="0")) #
cat1 <- nrow(subset(lchl_df, lchlCat=="1")) #
catsTot <- cat0 +cat1 #

cat0_per <- (cat0/(catsTot))*100 #
cat1_per <- (cat1/(catsTot))*100 #
cat0_per
cat1_per

# Isolate and remove n rows where chlCat==0
conditions <- lchl_df$chlCat==0 #set conditions for removed rows, cat==0
rows_remov <- which(conditions==TRUE) #only remove rows where conditions true
n_rows_remov <- n_1rows_remov
sampled.cats <- sample(rows_remov, n_rows_remov)
chl_bal_df <- lchl_df[-sampled.cats, ]

# Verify the percentages of each category; No Event (0), LChl (1)
bal_cat0 <- nrow(subset(lchl_bal_df, lchlCat=="0"))
bal_cat1 <- nrow(subset(lchl_bal_df, lchlCat=="1"))
bal_catsTot <- bal_cat0 + bal_cat1

bal_cat0_per <- (bal_cat0/(bal_catsTot))*100 # 36.9529074063548
bal_cat1_per <- (bal_cat1/(bal_catsTot))*100 # 31.5235462968226
bal_cat0_per
bal_cat1_per

# Save balanced compound event df
file_name <- paste("chl2_trn_unlag.csv")
write.table(lchl_bal_df,gsub(" ", "", paste("cmpndData/",file_name)),sep=",")


##################################### Balance Cat5 Training DF #####################################
# Read Chl5 training DF
file_name <- paste("chl_trn_unbal_unlag.csv")
lchl5_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)), sep=",", header=TRUE) #load file
lchl5_df <- lchl5_df[ , ! names(lchl5_df) %in% c("lchlCat2")] #remove lchlCat2
names(lchl_df)[names(lchl_df) == "lchlCat5"] <- "lchlCat" #change name of lchlCat5 column
head(lchl_df)
    
# Find the percentages of each category
cat0 <- nrow(subset(lchl_df, lchlCat=="0")) #
cat1 <- nrow(subset(lchl_df, lchlCat=="1")) #
cat2 <- nrow(subset(lchl_df, lchlCat=="2")) #
cat3 <- nrow(subset(lchl_df, lchlCat=="3")) #
cat4 <- nrow(subset(lchl_df, lchlCat=="4")) #

catsTot <- cat0 + cat1 + cat2 + cat3 + cat4 #

cat0_per <- (cat0/(catsTot))*100 #
cat1_per <- (cat1/(catsTot))*100 #
cat1_per <- (cat2/(catsTot))*100 #
cat1_per <- (cat3/(catsTot))*100 #
cat1_per <- (cat4/(catsTot))*100 #
cat0_per
cat1_per
cat2_per
cat3_per
cat4_per

# Isolate and remove n rows where chlCat==0
conditions <- lchl_df$chlCat==1 #set conditions for removed rows, cat==1
rows_remov <- which(conditions==TRUE) #only remove rows where conditions true
n_rows_remov <- n_1rows_remov
sampled.cats <- sample(rows_remov, n_rows_remov)
chl_bal_df <- lchl_df[-sampled.cats, ]

# Isolate and remove n rows where chlCat==0
conditions <- lchl_df$chlCat==0 #set conditions for removed rows, cat==0
rows_remov <- which(conditions==TRUE) #only remove rows where conditions true
n_rows_remov <- n_0rows_remov
sampled.cats <- sample(rows_remov, n_rows_remov)
chl_bal_df <- lchl_df[-sampled.cats, ]

# Isolate and remove n rows where chlCat2==1
conditions <- lchl_df$chlCat==1 #set conditions for removed rows, cat==1
rows_remov <- which(conditions==TRUE) #only remove rows where conditions true
n_rows_remov <- n_1rows_remov
sampled.cats <- sample(rows_remov, n_rows_remov)
chl_bal_df <- lchl_df[-sampled.cats, ]

# Isolate and remove n rows where chlCat2==2
conditions <- lchl_df$chlCat==2 #set conditions for removed rows, cat==2
rows_remov <- which(conditions==TRUE) #only remove rows where conditions true
n_rows_remov <- n_2rows_remov
sampled.cats <- sample(rows_remov, n_rows_remov)
chl_bal_df <- lchl_df[-sampled.cats, ]

# Isolate and remove n rows where chlCat2==3
conditions <- lchl_df$chlCat==3 #set conditions for removed rows, cat==3
rows_remov <- which(conditions==TRUE) #only remove rows where conditions true
n_rows_remov <- n_3rows_remov
sampled.cats <- sample(rows_remov, n_rows_remov)
chl_bal_df <- lchl_df[-sampled.cats, ]

# Verify the percentages of each category; negligible (0), moderate (1), strong (2), severe (3), extreme (4)
bal_cat0 <- nrow(subset(lchl_bal_df, lchlCat=="0"))
bal_cat1 <- nrow(subset(lchl_bal_df, lchlCat=="1"))
bal_cat2 <- nrow(subset(lchl_bal_df, lchlCat=="2"))
bal_cat3 <- nrow(subset(lchl_bal_df, lchlCat=="3"))
bal_cat4 <- nrow(subset(lchl_bal_df, lchlCat=="4"))
bal_catsTot <- bal_cat0 + bal_cat1 + bal_cat2 + bal_cat3 + bal_cat4

bal_cat0_per <- (bal_cat0/(bal_catsTot))*100 # 
bal_cat1_per <- (bal_cat1/(bal_catsTot))*100 # 
bal_cat2_per <- (bal_cat2/(bal_catsTot))*100 # 
bal_cat3_per <- (bal_cat3/(bal_catsTot))*100 # 
bal_cat4_per <- (bal_cat4/(bal_catsTot))*100 # 
bal_cat0_per
bal_cat1_per
bal_cat2_per
bal_cat3_per
bal_cat4_per

# Save balanced compound event df
file_name <- paste("chl5_trn_unlag.csv")
write.table(lchl_bal_df,gsub(" ", "", paste("cmpndData/",file_name)),sep=",")


##################################### Exploratory Train LChl RF #####################################
setwd("/home/kareande/lchl-mhw-events")
library("ranger") #randomForest package
library("tidymodels") #tidyverse models
library("foreach") #parallel processing
library("doParallel") #parallel processing
library("ggplot2") #aesthetic plotting
#install.packages()
    
# Prepare training data
file_name <- paste("chl5_trn_unlag.csv")
lchl_train <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)), sep=",", header=TRUE) #load file
lchl_train <- lchl_train[ , ! names(lchl_train) %in% c("chl", "npp")] #remove chl and npp variable
lchl_train[lchl_train$lchlCat == 0,]$lchlCat <- "Negligable"
#lchl_train[lchl_train$lchlCat == 0,]$lchlCat <- "No Event"
#lchl_train[lchl_train$lchlCat == 1,]$lchlCat <- "LChl Event"
lchl_train[lchl_train$lchlCat == 1,]$lchlCat <- "Moderate"
lchl_train[lchl_train$lchlCat == 2,]$lchlCat <- "Strong"
lchl_train[lchl_train$lchlCat == 3,]$lchlCat <- "Severe"
lchl_train[lchl_train$lchlCat == 4,]$lchlCat <- "Extreme"
lchl_train$lchlCat = as.factor(lchl_train$lchlCat)
head(lchl_train, n=10)

# Create model specification using vip package and ranger engine
n_trees <- 200
lchl_rec <- recipe(lchlCat ~ ., data = lchl_train)
tune_spec <- rand_forest(
  mtry = tune(), #number of variables sampled
  trees = n_trees, #number of trees
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
pdf(gsub(" ", "", paste("cmpndFigs/preTrainLChlUnlag",n_trees,"T.pdf")))
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


##################################### Refined Train LChl RF #####################################
setwd("/home/kareande/lchl-mhw-events")
library("ranger") #randomForest package
library("vip") #variable importance plots
library("tidymodels") #tidyverse models
library("foreach") #parallel processing
library("doParallel") #parallel processing
library("ggplot2") #aesthetic plotting
#install.packages()

# Re-load training data
file_name <- paste("chl5_trn_unlag.csv")
lchl_train <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)), sep=",", header=TRUE) #load file
lchl_train <- lchl_train[ , ! names(lchl_train) %in% c("chl", "npp")] #remove chl and npp variable
lchl_train[lchl_train$lchlCat == 0,]$lchlCat <- "Negligable"
#lchl_train[lchl_train$lchlCat == 0,]$lchlCat <- "No Event"
#lchl_train[lchl_train$lchlCat == 1,]$lchlCat <- "LChl Event"
lchl_train[lchl_train$lchlCat == 1,]$lchlCat <- "Moderate"
lchl_train[lchl_train$lchlCat == 2,]$lchlCat <- "Strong"
lchl_train[lchl_train$lchlCat == 3,]$lchlCat <- "Severe"
lchl_train[lchl_train$lchlCat == 4,]$lchlCat <- "Extreme"
lchl_train$lchlCat = as.factor(lchl_train$lchlCat)
head(lchl_train, n=10)

# Refine model specs for re-training based on previous results
n_min_range <- 10
n_max_range <- 13
mtry_min_range <- 3
mtry_max_range <- 4
lchl_rec <- recipe(lchlCat ~ ., data = lchl_train)
tune_spec <- rand_forest(
  mtry = tune(), #number of variables sampled
  trees = n_trees, #number of trees
  min_n = tune() #min number of datapoints for node to split
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

rf_grid <- grid_regular(
  mtry(range = c(mtry_min_range, mtry_max_range)),
  min_n(range = c(n_min_range, n_max_range)),
  levels = 10)
rf_grid
    
lchl_folds <- vfold_cv(lchl_train)
tune_wf <- workflow() %>%
  add_recipe(lchl_rec) %>%
  add_model(tune_spec)
    
# Train models using refined specs
doParallel::registerDoParallel(8);
system.time({
    regular_res <- tune_grid(
      tune_wf,
      resamples = lchl_folds,
      grid = rf_grid
    )
})
doParallel::stopImplicitCluster()
     
# Visualize refined results
png(gsub(" ", "", paste("cmpndFigs/refTrnLChlUnlag",n_trees,"T.png")))
regular_res %>%
  collect_metrics() %>%
  filter(.metric == "accuracy") %>% #or roc_auc
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "Accuracy") +
  ggtitle(title) +
  theme(plot.title = element_text(face="bold", size=18, hjust = 0.5),
  axis.title.x = element_text(face="bold", size=16),
  axis.title.y = element_text(face="bold", size=14),
  legend.title=element_text(size=14))
  labs(y = "Accuracy")
dev.off()

# Deciding on best model
best_rf <- select_best(regular_res, "accuracy") #or roc_auc
final_rf <- finalize_model(
  tune_spec,
  best_rf
)
final_rf

# Save final model
file_name <- gsub(" ", "", paste("finalLchl",n_trees,"TRF.RData")))
file_path <- gsub(" ", "", paste("cmpndData/",file_name))
save(final_rf, file=file_path)

# Check variable importance for trained model
png(gsub(" ", "", paste("cmpndFigs/VarImpTrnLChlUnlag",n_trees,"T.png")))
title <- gsub(" ", " ", paste("Variable Importance"))
lchl_prep <- prep(lchl_rec)
juiced <- juice(lchl_prep)
library(vip)
final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(lchlCat ~ .,
    data = juice(lchl_prep)
  ) %>%
  vip(geom = "point") +
  ggtitle(title) +
  theme(plot.title = element_text(face="bold", size=18, hjust = 0.5),
  axis.title.x = element_text(face="bold", size=16),
  axis.title.y = element_text(face="bold", size=14))
dev.off()


##################################### Test LChl RF Model #####################################
setwd("/home/kareande/lchl-mhw-events")
library("ranger") #randomForest package
library("tidymodels") #tidyverse models
library("foreach") #parallel processing
library("doParallel") #parallel processing
library("ggplot2") #aesthetic plotting
library("plot.matrix") #confusion matrix
#install.packages()

# Re-load training data
file_name <- paste("chl5_trn_unlag.csv")
lchl_train <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)), sep=",", header=TRUE) #load file
lchl_train <- lchl_train[ , ! names(lchl_train) %in% c("chl", "npp")] #remove chl and npp variable
lchl_train[lchl_train$lchlCat == 0,]$lchlCat <- "Negligable"
#lchl_train[lchl_train$lchlCat == 0,]$lchlCat <- "No Event"
#lchl_train[lchl_train$lchlCat == 1,]$lchlCat <- "LChl Event"
lchl_train[lchl_train$lchlCat == 1,]$lchlCat <- "Moderate"
lchl_train[lchl_train$lchlCat == 2,]$lchlCat <- "Strong"
lchl_train[lchl_train$lchlCat == 3,]$lchlCat <- "Severe"
lchl_train[lchl_train$lchlCat == 4,]$lchlCat <- "Extreme"
lchl_train$lchlCat = as.factor(lchl_train$lchlCat)
head(lchl_train, n=10)

# Get testing dataset
file_name <- paste("chl_tst_unlag.csv")
lchl_test <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)), sep=",", header=TRUE) #load file
lchl_test <- lchl_test[ , ! names(lchl_test) %in% c("chl", "npp")] #remove chl and npp variable
lchl_test[lchl_test$lchlCat == 0,]$lchlCat <- "Negligable"
#lchl_test[lchl_test$lchlCat == 0,]$lchlCat <- "No Event"
#lchl_test[lchl_test$lchlCat == 1,]$lchlCat <- "LChl Event"
lchl_test[lchl_test$lchlCat == 1,]$lchlCat <- "Moderate"
lchl_test[lchl_test$lchlCat == 2,]$lchlCat <- "Strong"
lchl_test[lchl_test$lchlCat == 3,]$lchlCat <- "Severe"
lchl_test[lchl_test$lchlCat == 4,]$lchlCat <- "Extreme"
lchl_test$lchlCat = as.factor(lchl_test$lchlCat)
head(lchl_test, n=10)

# Create final model specification
n_trees <- 200
best_mtry <- 
best_minn <- 

# Use testing data in model
rand_rf <- randomForest(lchlCat ~ ., num.trees=n_trees,mtry=as.integer(best_mtry),
                    nodesize=as.integer(best_minn), data=lchl_train)
pred_rf <- predict(rand_rf, lchl_test)
pred_rf

# Produce proportional confusion matrix
levels(lchl_test$lchlCat) <- levels(pred_rf)
cmat <- as.table(confusionMatrix(pred_rf, lchl_test$lchlCat))

extt <- sum(cmat[,1])
sevt <- sum(cmat[,2])
strt <- sum(cmat[,3])
modt <- sum(cmat[,4])
negt <- sum(cmat[,5])
c11 <- cmat[1,1]/extt
c21 <- cmat[2,1]/extt
c31 <- cmat[3,1]/extt
c41 <- cmat[4,1]/extt
c51 <- cmat[5,1]/extt
c12 <- cmat[1,2]/sevt
c22 <- cmat[2,2]/sevt
c32 <- cmat[3,2]/sevt
c42 <- cmat[4,2]/sevt
c52 <- cmat[5,2]/sevt
c13 <- cmat[1,3]/strt
c23 <- cmat[2,3]/strt
c33 <- cmat[3,3]/strt
c43 <- cmat[4,3]/strt
c53 <- cmat[5,3]/strt
c14 <- cmat[1,4]/modt
c24 <- cmat[2,4]/modt
c34 <- cmat[3,4]/modt
c44 <- cmat[4,4]/modt
c54 <- cmat[5,4]/modt
c14 <- cmat[1,5]/negt
c24 <- cmat[2,5]/negt
c34 <- cmat[3,5]/negt
c44 <- cmat[4,5]/negt
c54 <- cmat[5,5]/negt
lab <- c("Extreme", "Severe", "Strong", "Moderate", "Negligable")
cnfs <- matrix(c(c11, c21, c31, c41, c51,
                 c12, c22, c32, c42, c52,
                 c13, c23, c33, c43, c53,
                 c14, c24, c34, c44, c54,
                 c15, c25, c35, c45, c55),
                    nrow = 5, ncol = 5,
                    dimnames = list(lab, lab))
png(gsub(" ", "", paste("cmpndFigs/confMatLChlUnlag",n_trees,"T.png")))
title <- gsub(" ", " ", paste("RF Prediction Accuracy"))
par(mar=c(5.1, 4.1, 4.1, 4.1)) # adapt margins
pltimg <- plot(cnfs,
     xlab="",
     ylab="",
     cex.lab=1.5,
     cex.main=1.5,
     main="",
     digits=2,
     fmt.cell='%.2f',
     cex=1,
     col=topo.colors,
     breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1),
     spacing.key=c(0.75,0.3,-.5),
     fmt.key="%.3f")
mtext(side=1, line=2.4, "True Category", font=2, cex=1.5)
mtext(side=2, line=2.4, "Predicted Category", font=2, cex=1.5)
mtext(side=3, line=0.5, title, font=2, cex=1.75)
print(pltimg)
dev.off()
