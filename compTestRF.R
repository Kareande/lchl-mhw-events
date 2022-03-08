##################################### Test Compound RF Model #####################################
# Load final model
file_name <- gsub(" ", "", paste("finalCompNoChl",n_trees,"TRF.RData")))
file_path <- gsub(" ", "", paste("/home/kareande/mhwData/",file_name)) #csv file
final_rf <- load(file_path)

# Use testing data in model
final_wf <- workflow() %>%
  add_recipe(comp_rec) %>%
  add_model(final_rf)

final_res <- final_wf %>%
  last_fit(comp_split)

final_res %>%
  collect_metrics()

# Produce confusion matrix
pdf(gsub(" ", "", paste("/cmpndFigs/confMatCompNoChl",n_trees,"T.pdf")))

final_res %>%
  collect_predictions() %>%
  conf_mat(compCat, .pred_class) %>%
  autoplot(type = "heatmap")

dev.off()

print("#############################################################
	#############################################################
	
	Testing of RF Model complete. Confusion matrix saved as .pdf.

	#############################################################
	#############################################################")
