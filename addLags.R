setwd("/home/kareande/lchl-mhw-events")
library("foreach") #parallel processing
library("doParallel") #parallel processing
library("dplyr") #aesthetic plotting
#install.packages()

##################################### Condense dates #####################################
# Load unbalanced lChl df
file_name <- paste("chl_unbalanced_df.csv")
lchl_unb_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)), sep=",", header=TRUE)
head(lchl_unb_df)

# Replace day and month (mo) and yr w/ date
lchl_unb_df$date <- gsub(" ", "", paste(lchl_unb_df$yr, "-", lchl_unb_df$mo, "-",lchl_unb_df$day)) #merge yr, mo, da columns
lchl_unb_df <- lchl_unb_df %>% relocate(date, .before = yr) #move doy to be in front of year
lchl_unb_df <- lchl_unb_df[,-c(1,2,4)] #remove da, mo, yr columns
head(lchl_unb_df)

# Calculate days since
date_1<-as.Date(lchl_unb_df$date[1])
doParallel::registerDoParallel(32)
for(i in 1:nrow(lchl_unb_df)) {
    date_2 <- as.Date(lchl_unb_df$date[i])
    lchl_unb_df$days_since[i] <- difftime(date_1 ,date_2 , units = c("days"))
    }
doParallel::stopImplicitCluster()
lchl_unb_df <- lchl_unb_df %>% relocate(date, .before = lon) #move date column before year
head(lchl_unb_df)

# Save df
file_name <- paste("chl_unb_lags_ds_df.csv")
write.table(lchl_unb_df,gsub(" ", "", paste("cmpndData/",file_name)),sep=",")

rm(lchl_unb_df)

# Load unbalanced MHW df
file_name <- paste("mhw_unbalanced_df.csv")
mhw_unb_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)), sep=",", header=TRUE)
#head(cmp_unb_df)

# Replace day and month (mo) and yr w/ date
mhw_unb_df$date <- gsub(" ", "", paste(mhw_unb_df$yr, "-", mhw_unb_df$mo, "-",mhw_unb_df$day)) #merge yr, mo, da columns
mhw_unb_df <- mhw_unb_df %>% relocate(date, .before = yr) #move doy to be in front of year
mhw_unb_df <- mhw_unb_df[,-c(1,2,4)] #remove da, mo, yr columns
head(mhw_unb_df)

# Calculate days since
date_1<-as.Date(mhw_unb_df$date[1])
doParallel::registerDoParallel(32)
for(i in 1:nrow(mhw_unb_df)) {
    date_2 <- as.Date(mhw_unb_df$date[i])
    mhw_unb_df$days_since[i] <- difftime(date_1 ,date_2 , units = c("days"))
    }
doParallel::stopImplicitCluster()
mhw_unb_df <- mhw_unb_df %>% relocate(date, .before = lon) #move date column before year
head(mhw_unb_df)

# Save df
file_name <- paste("mhw_unb_lags_ds_df.csv")
write.table(mhw_unb_df,gsub(" ", "", paste("cmpndData/",file_name)),sep=",")


##################################### Correct and condense lat/lon #####################################
# Get LChl df
file_name <- paste("chl_unb_lags_ds_df.csv")
lchl_unb_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)),sep=",")
head(lchl_unb_df)

# Correct longitudes for lchl df
lchl_unb_df$lon <- abs(lchl_unb_df$lon)

# Correct ranges of lat/lon to be equal on both datasets
lchl_unb_df <- lchl_unb_df[lchl_unb_df$lon<170, ] #length 5138102
mhw_unb_df <- mhw_unb_df[mhw_unb_df$lon>=102.5, ] #length 5138102
lchl_unb_df <- lchl_unb_df[lchl_unb_df$lat>10.625, ] #length 5138102

# Find range of lon and lat for each df (verify same range)
print(paste("lchl_df longitudes goes from",toString(min(lchl_unb_df$lon)),"to",toString(max(lchl_unb_df$lon)))) #102.5 to 167.5
print(paste("mhw_df longitudes goes from",toString(min(mhw_unb_df$lon)),"to",toString(max(mhw_unb_df$lon)))) #102.5 to 167.5
print(paste("lchl_df latitudes goes from",toString(min(lchl_unb_df$lat)),"to",toString(max(lchl_unb_df$lat)))) #12.5 to 62.5
print(paste("mhw_df latitudes goes from",toString(min(mhw_unb_df$lat)),"to",toString(max(mhw_unb_df$lat)))) #12.5 to 62.5

# Extract unique values of lon and lat (veryify same values)
unique(lchl_unb_df$lon) #27 vals
unique(mhw_unb_df$lon) #27 vals 
unique(lchl_unb_df$lat) #21 vals
unique(mhw_unb_df$lat) #20 vals, missing 60.0

#Replace lat/lon with location
lchl_unb_df$location <- gsub(" ", "", paste(lchl_unb_df$lat, "-", lchl_unb_df$lon)) #merge lat and lon
lchl_unb_df <- lchl_unb_df %>% relocate(location, .before = nit) #move location column before nit
lchl_unb_df <- lchl_unb_df[,-c(1,2,3)] #remove date, lat, lon columns

lchl_unb_df <- lchl_unb_df[order(lchl_unb_df$location),] #reorder dataset by location instead of date
lchl_unb_df$days_since <- abs(lchl_unb_df$days_since) #make days since a positive number
lchl_unb_df <- lchl_unb_df %>% relocate(days_since, .before = location) #move days_since to be in front of location
head(lchl_unb_df)

# Save df
file_name <- paste("chl_unb_lags_ll_df.csv")
write.table(lchl_unb_df,gsub(" ", "", paste("cmpndData/",file_name)),sep=",")

# Get MHW df
file_name <- paste("mhw_unb_lags_ds_df.csv")
mhw_unb_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)),sep=",")
head(mhw_unb_df)

#Replace lat/lon with location
mhw_unb_df$location <- gsub(" ", "", paste(mhw_unb_df$lat, "-", mhw_unb_df$lon)) #merge lat and lon
mhw_unb_df <- mhw_unb_df %>% relocate(location, .before = qnet) #move location column before nit
mhw_unb_df <- mhw_unb_df[,-c(1,2,3)] #remove date, lat, lon columns

mhw_unb_df <- mhw_unb_df[order(mhw_unb_df$location),] #reorder dataset by location instead of date
mhw_unb_df$days_since <- abs(mhw_unb_df$days_since) #make days since a positive number
mhw_unb_df <- mhw_unb_df %>% relocate(days_since, .before = location) #move days_since to be in front of location
head(mhw_unb_df)

# Save df
file_name <- paste("mhw_unb_lags_ll_df.csv")
write.table(mhw_unb_df,gsub(" ", "", paste("cmpndData/",file_name)),sep=",")

unique(lchl_unb_df$location) #are they the same?
unique(mhw_unb_df$location) #are they the same?


