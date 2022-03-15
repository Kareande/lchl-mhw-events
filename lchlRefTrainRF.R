setwd("/home/kareande/lchl-mhw-events")
library("ranger") #randomForest package
library("vip") #variable importance plots
library("tidymodels") #tidyverse models
library("foreach") #parallel processing
library("doParallel") #parallel processing
library("ggplot2") #aesthetic plotting
#install.packages()

##################################### Refined Train LChl RF #####################################
# Refine model specs for re-training based on previous results
#for 150 trees; mtry 3:4, min_n 5:10 REDO WITH 150T---------------------------
#for 200 trees; mtry x:x, min_n x:x
mtry_min_range <- 3
mtry_max_range <- 4
n_min_range <- 10
n_max_range <- 13

rf_grid <- grid_regular(
  mtry(range = c(mtry_min_range, mtry_max_range)),
  min_n(range = c(n_min_range, n_max_range)),
  levels = 10
)
    
nrow(rf_grid)
rf_grid
    
# Train models using refined specs
doParallel::registerDoParallel(10);
system.time({
    regular_res <- tune_grid(
      tune_wf,
      resamples = lchl_folds,
      grid = rf_grid
    )
})
doParallel::stopImplicitCluster()
     
# Visualize refined results
pdf(gsub(" ", "", paste("/cmpndFigs/refTrnLchl",n_trees,"T.pdf")))

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
pdf(gsub(" ", "", paste("/cmpndFigs/VarImpTrnLchl",n_trees,"T.pdf")))

lchl_prep <- prep(lchl_rec)
juiced <- juice(lchl_prep)

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(lchlCat ~ .,
    data = juiced
  ) %>%
  vip(geom = "point")

dev.off()


print("###################################################################
        ###################################################################

        Refined Train complete. Results and Var Importance saved as PDF's.

        ###################################################################
        ###################################################################")
