setwd("/home/kareande/lchl-mhw-events")
library("ranger") #randomForest package
library("tidymodels") #tidyverse models
library("foreach") #parallel processing
library("doParallel") #parallel processing
library("ggplot2") #aesthetic plotting
library("plot.matrix") #confusion matrix
#install.packages()

##################################### Test LChl RF Model #####################################
# Load final model
file_name <- gsub(" ", "", paste("finalLchl",n_trees,"TRF.RData")))
final_rf <- load(gsub(" ", "", paste("cmpndData/",file_name)))

# Use testing data in final model
final_wf <- workflow() %>%
  add_recipe(lchl_rec) %>%
  add_model(final_rf)

final_res <- final_wf %>%
  last_fit(lchl_split)

final_res %>%
  collect_metrics()
    
# Produce confusion matrix
pdf(gsub(" ", "", paste("/cmpndFigs/confMatLchl",n_trees,"T.pdf")))

final_res %>%
  collect_predictions() %>%
  conf_mat(lchlCat, .pred_class) %>%
  autoplot(type = "heatmap")

dev.off()

print("#############################################################
        #############################################################

        Testing of RF Model complete. Confusion matrix saved as PDF.

        #############################################################
        #############################################################")
