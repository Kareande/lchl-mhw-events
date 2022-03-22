setwd("/home/kareande/lchl-mhw-events")
library("foreach") #parallel processing
library("doParallel") #parallel processing
library("dplyr") #lags
#install.packages()

##################################### Define LChl categories #####################################
# Get unbalanced and uncategorized lchl df
file_name <- paste("master_with_contime_df.csv") #name of df
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
file_name <- paste("chl_unbalanced_df.csv")
file_path <- gsub(" ", "", paste("cmpndData/",file_name))
write.table(lchl_df,file_path,sep=",")

##################################### Condense date columns #####################################
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
#date_1<-as.Date(mhw_unb_df$date[1]) #date_1 value from lchl
doParallel::registerDoParallel(32)
for(i in 1:nrow(mhw_unb_df)) {
    date_2 <- as.Date(mhw_unb_df$date[i])
    mhw_unb_df$days_since[i] <- difftime(date_1 ,date_2 , units = c("days"))
    }
doParallel::stopImplicitCluster()
mhw_unb_df <- mhw_unb_df %>% relocate(date, .before = lon) #move date column before year
head(mhw_unb_df)

# Save df !!!!*****************LEFT OFF HERE, SAVE DF VERIFY DATES ARE SAME**********************!!!!
file_name <- paste("mhw_unb_lags_ds_df.csv")
write.table(mhw_unb_df,gsub(" ", "", paste("cmpndData/",file_name)),sep=",")


##################################### Condense lat/lon columns #####################################
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


##################################### Add LChl Lags #####################################
# Get LChl df
file_name <- paste("chl_unb_lags_ll_df.csv")
lchl_unb_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)),sep=",")
head(lchl_unb_df)

# Prepare to add lags
#vars_lag_k = c(2, 7, 14, 30, 50, 70, 100, 140, 170, 200, 230, 270, 310, 365, 390, 450, 500, 560, 600, 660, 730) #Kat's lags in days
#vars_lag = c(2, 7, 14, 180) #2 days, 1 wk, 2 wk, 6 mo
vars_lchl = c(colnames(lchl_unb_df)) #make list of column names
vars_lchl = vars_lchl[-c(1,2,9)] #remove days, location, and category from list

# Add 2, 7, 14, and 180 days of lags using dplyr
doParallel::registerDoParallel(32)
lchl_unb_2lag180_df <- lchl_unb_df %>%                            # Add lagged column
    group_by(days_since) %>%
    dplyr::mutate(nitlag2 = dplyr::lag(nit, n = 2, default = NA)) %>% 
    dplyr::mutate(oxylag2 = dplyr::lag(oxy, n = 2, default = NA)) %>% 
    dplyr::mutate(pholag2 = dplyr::lag(pho, n = 2, default = NA)) %>% 
    dplyr::mutate(chllag2 = dplyr::lag(chl, n = 2, default = NA)) %>% 
    dplyr::mutate(sillag2 = dplyr::lag(sil, n = 2, default = NA)) %>% 
    dplyr::mutate(npplag2 = dplyr::lag(npp, n = 2, default = NA)) %>% 
    dplyr::mutate(nitlag7 = dplyr::lag(nit, n = 7, default = NA)) %>% 
    dplyr::mutate(oxylag7 = dplyr::lag(oxy, n = 7, default = NA)) %>% 
    dplyr::mutate(pholag7 = dplyr::lag(pho, n = 7, default = NA)) %>% 
    dplyr::mutate(chllag7 = dplyr::lag(chl, n = 7, default = NA)) %>% 
    dplyr::mutate(sillag7 = dplyr::lag(sil, n = 7, default = NA)) %>% 
    dplyr::mutate(npplag7 = dplyr::lag(npp, n = 7, default = NA)) %>% 
    dplyr::mutate(nitlag14 = dplyr::lag(nit, n = 14, default = NA)) %>% 
    dplyr::mutate(oxylag14 = dplyr::lag(oxy, n = 14, default = NA)) %>% 
    dplyr::mutate(pholag14 = dplyr::lag(pho, n = 14, default = NA)) %>% 
    dplyr::mutate(chllag14 = dplyr::lag(chl, n = 14, default = NA)) %>% 
    dplyr::mutate(sillag14 = dplyr::lag(sil, n = 14, default = NA)) %>% 
    dplyr::mutate(npplag14 = dplyr::lag(npp, n = 14, default = NA)) %>%
    dplyr::mutate(nitlag180 = dplyr::lag(nit, n = 180, default = NA)) %>% 
    dplyr::mutate(oxylag180 = dplyr::lag(oxy, n = 180, default = NA)) %>% 
    dplyr::mutate(pholag180 = dplyr::lag(pho, n = 180, default = NA)) %>% 
    dplyr::mutate(chllag180 = dplyr::lag(chl, n = 180, default = NA)) %>% 
    dplyr::mutate(sillag180 = dplyr::lag(sil, n = 180, default = NA)) %>% 
    dplyr::mutate(npplag180 = dplyr::lag(npp, n = 180, default = NA)) %>%
    as.data.frame()
doParallel::stopImplicitCluster()

# Save df
file_name <- paste("lchl_unb_2lags180_df.csv")
write.table(lchl_unb_2lag180_df,gsub(" ", "", paste("cmpndData/",file_name)),sep=",")
rm(lchl_unb_2lag180_df)


##################################### Add MHW Lags #####################################
# Get MHW df
file_name <- paste("mhw_unb_lags_ll_df.csv")
mhw_unb_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)),sep=",")
head(mhw_unb_df)

# Add 2, 7, 14, and 180 days of lags using dplyr
doParallel::registerDoParallel(32)
mhw_unb_2lag180_df <- mhw_unb_df %>%
    group_by(days_since) %>%
    dplyr::mutate(qnetlag2 = dplyr::lag(qnet, n = 2, default = NA)) %>% 
    dplyr::mutate(slplag2 = dplyr::lag(slp, n = 2, default = NA)) %>% 
    dplyr::mutate(satlag2 = dplyr::lag(sat, n = 2, default = NA)) %>% 
    dplyr::mutate(wndsplag2 = dplyr::lag(wndsp, n = 2, default = NA)) %>% 
    dplyr::mutate(sstlag2 = dplyr::lag(sst, n = 2, default = NA)) %>% 
    dplyr::mutate(sstRoClag2 = dplyr::lag(sstRoC, n = 2, default = NA)) %>% 
    dplyr::mutate(qnetlag7 = dplyr::lag(qnet, n = 7, default = NA)) %>% 
    dplyr::mutate(slplag7 = dplyr::lag(slp, n = 7, default = NA)) %>% 
    dplyr::mutate(satlag7 = dplyr::lag(sat, n = 7, default = NA)) %>% 
    dplyr::mutate(wndsplag7 = dplyr::lag(wndsp, n = 7, default = NA)) %>% 
    dplyr::mutate(sstlag7 = dplyr::lag(sst, n = 7, default = NA)) %>% 
    dplyr::mutate(sstRoClag7 = dplyr::lag(sstRoC, n = 7, default = NA)) %>%
    dplyr::mutate(qnetlag14 = dplyr::lag(qnet, n = 14, default = NA)) %>% 
    dplyr::mutate(slplag14 = dplyr::lag(slp, n = 14, default = NA)) %>% 
    dplyr::mutate(satlag14 = dplyr::lag(sat, n = 14, default = NA)) %>% 
    dplyr::mutate(wndsplag14 = dplyr::lag(wndsp, n = 14, default = NA)) %>% 
    dplyr::mutate(sstlag14 = dplyr::lag(sst, n = 14, default = NA)) %>% 
    dplyr::mutate(sstRoClag14 = dplyr::lag(sstRoC, n = 14, default = NA)) %>%
    dplyr::mutate(qnetlag180 = dplyr::lag(qnet, n = 180, default = NA)) %>% 
    dplyr::mutate(slplag180 = dplyr::lag(slp, n = 180, default = NA)) %>% 
    dplyr::mutate(satlag180 = dplyr::lag(sat, n = 180, default = NA)) %>% 
    dplyr::mutate(wndsplag180 = dplyr::lag(wndsp, n = 180, default = NA)) %>% 
    dplyr::mutate(sstlag180 = dplyr::lag(sst, n = 180, default = NA)) %>% 
    dplyr::mutate(sstRoClag180 = dplyr::lag(sstRoC, n = 180, default = NA)) %>%
    as.data.frame()
doParallel::stopImplicitCluster()

# Save df
file_name <- paste("mhw_unb_2lags180_df.csv")
write.table(mhw_unb_2lag180_df,gsub(" ", "", paste("cmpndData/",file_name)),sep=",")
rm(mhw_unb_2lag180_df)

##################################### Convert and lat/lon back #####################################


##################################### Combine DF's #####################################
# Get LChl df w/ lags
file_name <- paste("lchl_unb_2lags180_df.csv")
lchl_unb_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)),sep=",")
head(lchl_unb_df)

# Get MHW df w/ lags
file_name <- paste("mhw_unb_2lags180_df.csv")
mhw_unb_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)),sep=",")
head(mhw_unb_df)

# Combine dfs into compound df
cmp_df <- merge(lchl_unb_df, mhw_unb_df, c("days_since","location"))
head(cmp_df)
nrow(cmp_df)

# Save unbalanced compound df
cmp_df <- na.omit(cmp_df) #remove NA values
file_name <- paste("cmpnd_unb_2lags180_df.csv")
write.table(cmp_df,gsub(" ", "", paste("cmpndData/",file_name)),sep=",")

