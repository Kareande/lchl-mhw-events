##################################### Refined Train LChl RF #####################################
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
  resamples = comp_folds,
  grid = rf_grid
)

end_time <- Sys.time()
doParallel::stopImplicitCluster()
end_time - start_time

# Visualize refined results
pdf(gsub(" ", "", paste("/auto/home/kareande/lchl-mhw-events/cmpndFigs/refTrnCompNoChl",n_trees,"T.pdf")))

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

final_rf

#save final model
file_name <- gsub(" ", "", paste("finalCompNoChl",n_trees,"TRF.RData"))
save(final_rf, file=file_name)

# Check variable importance for training data
pdf(gsub(" ", "", paste("/auto/home/kareande/lchl-mhw-events/cmpndFigs/VarImpTrnCompNoChl",n_trees,"T.pdf")))
comp_prep <- prep(comp_rec)
juiced <- juice(comp_prep)

library(vip)
final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(compCat ~ .,
    data = juice(comp_prep)
  ) %>%
  vip(geom = "point")

dev.off()


print("##############################################################
	##############################################################

	Refined training of RF model complete. Results and variable
	importance saved as .pdf's.

	##############################################################
	##############################################################")
