setwd("/home/kareande/lchl-mhw-events")
library("foreach") #parallel processing
library("doParallel") #parallel processing
library("dplyr") #aesthetic plotting
#install.packages()

##################################### Add lags to LChl DF #####################################
# Load LChl DF
file_name <- paste("master_with_contime_df.csv")
lchl_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)), sep=",", header=TRUE)
head(lchl_df)



##################################### Add lags to MHW DF #####################################
file_name <- paste("mhw_unbalanced_df.csv")
mhw_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)), sep=",", header=TRUE)
mhw_df$mhwCat[mhw_df$mhwCat > 0] <- 1 #condense MHW cats into two categories (presence or absence)
head(mhw_df)