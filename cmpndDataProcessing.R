setwd("/home/kareande/lchl-mhw-events")
library("foreach") #parallel processing
library("doParallel") #parallel processing
library("dplyr") #lags
library("lubridate") #calculating day of year
#install.packages()

##################################### Define LChl categories #####################################
# Get uncategorized Lchl df
file_name <- paste("lchl_raw.csv") #name of df
lchl_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)), sep=",", header=TRUE) #read df path
lchl_df <- na.omit(lchl_df) #remove NA values
head(lchl_df)

# Calculate percentile
cat <- quantile(lchl_df$chl,probs=0.1) #what value is LChl maximum value
cat #0.37448388308533

# Check chl values
max(lchl_df$chl) #350.995544433594 mg/m^3
min(lchl_df$chl) #9.74359052937264e-06 mg/m^3

# Divide LChl events into 2 categories
x <- length(lchl_df$chl) #get number of datapoints
doParallel::registerDoParallel()
for(i in 1:x) { #loop to fill out categories
    if (is.na(lchl_df$chl[i])) { #if value i of chl column is NA,
        lchl_df$lchlCat[i] = 0       #then the category is 0
    } else if(lchl_df$chl[i] <= cat) { #if chl value is <= the bottom 10%,
        lchl_df$lchlCat[i] = 1              #then the category is 1
    } else {           #if the chl value is > than the 10th percentile,
        lchl_df$lchlCat[i] = 0 #then the category is 0
    }
        }
doParallel::stopImplicitCluster()
head(lchl_df)

# Save df
file_name <- paste("lchl_unprocessed.csv")
file_path <- gsub(" ", "", paste("cmpndData/",file_name))
write.table(lchl_df,file_path,sep=",")

################################ Condense date and location data ################################
# Load lChl df
file_name <- paste("lchl_unprocessed.csv")
lchl_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)), sep=",", header=TRUE)
#head(lchl_df)

# Load MHW df
file_name <- paste("mhw_unprocessed.csv")
mhw_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)), sep=",", header=TRUE)
mhw_df$mhwCat[mhw_df$mhwCat > 0] <- 1 #condense MHW cats into two categories (presence or absence)
#head(mhw_df)

# Create date columns
lchl_df$date <- gsub(" ", "", paste(lchl_df$yr, "-", lchl_df$mo, "-",lchl_df$day)) #merge yr, mo, da columns
mhw_df$date <- gsub(" ", "", paste(mhw_df$yr, "-", mhw_df$mo, "-",mhw_df$day))
lchl_df <- lchl_df %>% relocate(date, .before = lon) #move date to be in front of lon
mhw_df <- mhw_df %>% relocate(date, .before = lon)
head(lchl_df)
head(mhw_df)

# Calculate days since for LChl DF
date_1 <- as.Date(lchl_df$date[1])
date_2 <- as.Date(lchl_df$date)
lchl_df$ds <- difftime(date_1 ,date_2 , units = c("days"))
lchl_df <- lchl_df %>% relocate(ds, .before = lon) #move days since column before lon
head(lchl_df)

# Calculate days since for MHW DF
date_1 <- as.Date(mhw_df$date[1])
date_2 <- as.Date(mhw_df$date)
mhw_df$ds <- difftime(date_1 ,date_2 , units = c("days"))
mhw_df <- mhw_df %>% relocate(ds, .before = lon) #move days since column before lon
head(mhw_df)

# Calculate doy from date for LChl DF
names(lchl_df)[names(lchl_df) == "date"] <- "doy" #rename date to doy
lchl_df$doy <- yday(lchl_df$doy) #calculate doy
lchl_df <- lchl_df[ , ! names(lchl_df) %in% c("day","mo")] #remove day and month columns
head(lchl_df)

# Calculate doy from date for MHW DF
names(mhw_df)[names(mhw_df) == "date"] <- "doy" #rename date to doy
mhw_df$doy <- yday(mhw_df$doy) #calculate doy
mhw_df <- mhw_df[ , ! names(mhw_df) %in% c("day","mo")] #remove day and month columns
head(mhw_df)

# Make lat/lon ranges equal on both datasets
lchl_df$lon <- abs(lchl_df$lon) #make lchl lons positive
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

# Create location column (to aid in lagging)
lchl_df$location <- gsub(" ", "", paste(lchl_df$lat, "-", lchl_df$lon)) #merge lat and lon
mhw_df$location <- gsub(" ", "", paste(mhw_df$lat, "-", mhw_df$lon))
lchl_df <- lchl_df %>% relocate(location, .before = nit) #move location column before nit
mhw_df <- mhw_df %>% relocate(location, .before = qnet) #move location column before qnet

# Reorder datasets to aid lags grouping
lchl_df <- lchl_df[order(lchl_df$location),] #reorder dataset by location instead of date
mhw_df <- mhw_df[order(mhw_df$location),]
lchl_df$ds <- abs(lchl_df$ds) #make days since a positive number
mhw_df$ds <- abs(mhw_df$ds)
head(mhw_df)
head(lchl_df)

# Save DF's
file_name <- paste("lchl_lagsprep.csv")
write.table(lchl_df,gsub(" ", "", paste("cmpndData/",file_name)),sep=",")
file_name <- paste("mhw_lagsprep.csv")
write.table(mhw_df,gsub(" ", "", paste("cmpndData/",file_name)),sep=",")

############################# Split into Testing and Training DFs #############################
# Get LChl DF
file_name <- gsub(" ", "", paste("lchl_lagsprep.csv"))
lchl_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)),sep=",") #get lchl df
head(lchl_df)

# Split LChl DF
conditions <- lchl_df$yr<2011 #set conditions for sampled rows
smpld_rows <- which(conditions==TRUE) #only sample rows where conditions true
n_smpld <- nrow(lchl_df)*(2/3) #set how many rows to sample
sampled.cats <- sample(smpld_rows, n_smpld) #create sample
lchl_train <- lchl_df[sampled.cats,] #dataset with sample
lchl_test <- lchl_df[-sampled.cats, ] #dataset excluding sample
tst_name <- gsub(" ", "", paste("lchl_tst_unlag.csv")) 
write.table(lchl_test,gsub(" ", "", paste("cmpndData/",tst_name)),sep=",") #save testing set
trn_name <- gsub(" ", "", paste("lchl_trn_unbal.csv")) 
write.table(lchl_train,gsub(" ", "", paste("cmpndData/",trn_name)),sep=",") #save training set

# Get MHW DF
file_name <- gsub(" ", "", paste("mhw_lagsprep.csv")) #create dyamic df name
mhw_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)),sep=",") #get mhw df
head(mhw_df)

# Split MHW DF
conditions <- mhw_df$yr<2011 #set conditions for sampled rows
smpld_rows <- which(conditions==TRUE) #only sample rows where conditions true
n_smpld <- nrow(mhw_df)*(2/3) #set how many rows to sample
sampled.cats <- sample(smpld_rows, n_smpld) #create sample
mhw_train <- mhw_df[sampled.cats,] #dataset with sample
mhw_test <- mhw_df[-sampled.cats, ] #dataset excluding sample
tst_name <- gsub(" ", "", paste("mhw_tst_unlag.csv"))
write.table(mhw_test,gsub(" ", "", paste("cmpndData/",tst_name)),sep=",") #save testing set
trn_name <- gsub(" ", "", paste("mhw_trn_unbal.csv"))
write.table(mhw_train,gsub(" ", "", paste("cmpndData/",trn_name)),sep=",") #save training set


##################################### Add lags to Testing DFs #####################################
# Get unlagged LChl testing df
file_name <- paste("lchl_tst_unlag.csv")
lchl_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)),sep=",")
head(lchl_df)

# Add various lags to LChl testing DF
vars_lag = c(2, 7, 14, 180, 365, 730) #2 days, 1 wk, 2 wk, 6 mo, 1yr, 2yr
vars_lchl = c(colnames(lchl_df))[-c(1:6,13)] #var names
doParallel::registerDoParallel(32)
for(i in 1:length(vars_lag)) { #for each lag value...
    dyn_col <- c()
    for(v in 1:length(vars_lchl)) {
        dyn_col[v] <- gsub(" ", "", paste(vars_lchl[v],vars_lag[i])) #create iterative lagged col name
        }
    lchl_lag_df <- lchl_df %>% #create lagged df
        group_by(location) %>% #limit shifting of rows to be by date
        dplyr::mutate(!!dyn_col[1] := dplyr::lead(nit, n = vars_lag[i], default = NA)) %>% #add lag to nit-- CANNOT LOOP
        dplyr::mutate(!!dyn_col[2] := dplyr::lead(oxy, n = vars_lag[i], default = NA)) %>% #add lag to oxy-- B/C 1ST ARG
        dplyr::mutate(!!dyn_col[3] := dplyr::lead(pho, n = vars_lag[i], default = NA)) %>% #add lag to pho-- IN lead()
        dplyr::mutate(!!dyn_col[4] := dplyr::lead(chl, n = vars_lag[i], default = NA)) %>% #add lag to chl-- CANT BE
        dplyr::mutate(!!dyn_col[5] := dplyr::lead(sil, n = vars_lag[i], default = NA)) %>% #add lag to sil-- CHANGED TO
        dplyr::mutate(!!dyn_col[6] := dplyr::lead(npp, n = vars_lag[i], default = NA)) %>% #add lag to npp-- vars_lchl[v]
        as.data.frame()
    for(v in 1:length(vars_lchl)) {
        lchl_lag_df <- lchl_lag_df %>% relocate(gsub(" ", "", paste(vars_lchl[v],vars_lag[i])), .after = vars_lchl[v])
        }
    lchl_lag_df <- lchl_lag_df[ , ! names(lchl_lag_df) %in% c("ds","location")] #remove days since and location
    df_name <- gsub(" ", "", paste("lchl_tst_",vars_lag[i],"lag.csv")) #create dyamic lag df name
    write.table(lchl_lag_df,gsub(" ", "", paste("cmpndData/",df_name)),sep=",") #save df w/ each lag value
    }
doParallel::stopImplicitCluster()
head(lchl_lag_df)

# Get unlagged MHW testing DF
file_name <- paste("mhw_tst_unlag.csv")
mhw_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)),sep=",")

# Add various lags to MHW testing DF
vars_mhw = c(colnames(mhw_df))[-c(1:6,13)] #var names
doParallel::registerDoParallel(32)
for(i in 1:length(vars_lag)) { #for each lag value...
    dyn_col <- c()
    for(v in 1:length(vars_mhw)) {
        dyn_col[v] <- gsub(" ", "", paste(vars_mhw[v],vars_lag[i])) #create iterative lagged col name
        }
    mhw_lag_df <- mhw_df %>% #create lagged df
        group_by(location) %>% #limit shifting of rows to be by date
        dplyr::mutate(!!dyn_col[1] := dplyr::lead(qnet, n = vars_lag[i], default = NA)) %>%   #add var1 lag-- CANNOT LOOP
        dplyr::mutate(!!dyn_col[2] := dplyr::lead(slp, n = vars_lag[i], default = NA)) %>%    #add var2 lag-- B/C 1ST ARG
        dplyr::mutate(!!dyn_col[3] := dplyr::lead(sat, n = vars_lag[i], default = NA)) %>%    #add var3 lag-- IN LAG()
        dplyr::mutate(!!dyn_col[4] := dplyr::lead(wndsp, n = vars_lag[i], default = NA)) %>%  #add var4 lag-- CANT BE
        dplyr::mutate(!!dyn_col[5] := dplyr::lead(sst, n = vars_lag[i], default = NA)) %>%    #add var5 lag-- CHANGED TO
        dplyr::mutate(!!dyn_col[6] := dplyr::lead(sstRoC, n = vars_lag[i], default = NA)) %>% #add var6 lag-- vars_mhw[v]
        as.data.frame()
    for(v in 1:length(vars_mhw)) {
        mhw_lag_df <- mhw_lag_df %>% relocate(gsub(" ", "", paste(vars_mhw[v],vars_lag[i])), .after = vars_mhw[v])
        }
    mhw_lag_df <- mhw_lag_df[ , ! names(mhw_lag_df) %in% c("ds","location")] #remove days since and location
    df_name <- gsub(" ", "", paste("mhw_tst_",vars_lag[i],"lag.csv")) #create dyamic lag df name
    write.table(mhw_lag_df,gsub(" ", "", paste("cmpndData/",df_name)),sep=",") #save df w/ each lag value
    }
doParallel::stopImplicitCluster()
head(mhw_lag_df)


############################### Add Compound Cats to Testing DF's ###############################
for(i in 1:length(vars_lag)){
    # Get lagged LChl testing DFs
    file_name <- gsub(" ", "", paste("lchl_tst_",vars_lag[i],"lag.csv")) #create dyamic df name
    lchl_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)),sep=",")
    
    # Get lagged MHW testing DFs
    file_name <- gsub(" ", "", paste("mhw_tst_",vars_lag[i],"lag.csv")) #create dyamic df name
    mhw_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)),sep=",")
    
    # Combine LChl and MHW testing DFs
    cmp_df <- merge(lchl_df, mhw_df, c("yr", "doy", "lon", "lat")) #merge based on these columns
    cmp_df <- na.omit(cmp_df) #remove NA values
    cmp_df <- cmp_df %>% relocate(lchlCat, .before = mhwCat)
    file_name <- gsub(" ", "", paste("cmpnd_tst_",vars_lag[i],"lag.csv")) #create dyamic df name
    write.table(cmp_df,gsub(" ", "", paste("cmpndData/",file_name)),sep=",")
    }
head(cmp_df)
    
# Add cats to compound testing DF
doParallel::registerDoParallel(32)
for(i in 1:length(vars_lag)){
    file_name <- gsub(" ", "", paste("cmpnd_tst_",vars_lag[i],"lag.csv")) #create dyamic df name
    cmp_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)),sep=",") #get cmpnd df
    cmp_df <- na.omit(cmp_df) #make sure NA values removed
    x <- nrow(cmp_df) #get number of datapoints
    for(n in 1:x) { #loop to fill out categories
        if (is.na(cmp_df$lchlCat[n]) || is.na(cmp_df$mhwCat[n])) { #if value i of chl column OR mhw column is NA,
            cmp_df$cmpCat[n] = 0                           #then the category is 0
        } else if(cmp_df$lchlCat[n]==1 && cmp_df$mhwCat[n]==1) { #if chl cat AND mhw cat is 1,
            cmp_df$cmpCat[n] = 3                         #then the category is 3
        } else if(cmp_df$mhwCat[n]==1) { #if only the mhw cat is 1,
            cmp_df$cmpCat[n] = 1        #then the category is 1
        } else if(cmp_df$lchlCat[n]==1) { #if only the chl cat is 1,
            cmp_df$cmpCat[n] = 2        #then the category is 2
        } else {            #if the chl or mhw cat is not 1,
            cmp_df$cmpCat[n] = 0 #then the category is 0
        }
            }
    cmp_df <- cmp_df[ , ! names(cmp_df) %in% c("lchlCat","mhwCat")] #remove lchlCat & mhwCat columns
    file_name <- gsub(" ", "", paste("cmpnd_tst_",vars_lag[i],"lag.csv")) #create dyamic df name
    write.table(cmp_df,gsub(" ", "", paste("cmpndData/",file_name)),sep=",")
    }
doParallel::stopImplicitCluster()
head(cmp_df)
    
    
############################### Add Compound Cats to Training DF ###############################
# Get LChl training DF
file_name <- gsub(" ", "", paste("lchl_trn_unbal.csv"))
lchl_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)),sep=",")
lchl_df <- lchl_df[ , ! names(lchl_df) %in% c("ds","location")] #remove days since and location

# Get MHW training DF
file_name <- gsub(" ", "", paste("mhw_trn_unbal.csv"))
mhw_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)),sep=",")
mhw_df <- mhw_df[ , ! names(mhw_df) %in% c("ds","location")] #remove days since and location

# Combine LChl and MHW training DF's 
cmp_df <- merge(lchl_df, mhw_df, c("yr", "doy", "lon", "lat")) #merge based on these columns
cmp_df <- na.omit(cmp_df) #remove NA values
cmp_df <- cmp_df %>% relocate(lchlCat, .before = mhwCat)
head(cmp_df)

# Add cats to compound training DF
x <- nrow(cmp_df) #get number of observations
for(n in 1:x) { #loop to fill out categories
    if (is.na(cmp_df$lchlCat[n]) || is.na(cmp_df$mhwCat[n])) { #if value i of chl column OR mhw column is NA,
        cmp_df$cmpCat[n] = 0                           #then the category is 0
    } else if(cmp_df$lchlCat[n]==1 && cmp_df$mhwCat[n]==1) { #if chl cat AND mhw cat is 1,
        cmp_df$cmpCat[n] = 3                         #then the category is 3
    } else if(cmp_df$mhwCat[n]==1) { #if only the mhw cat is 1,
        cmp_df$cmpCat[n] = 1        #then the category is 1
    } else if(cmp_df$lchlCat[n]==1) { #if only the chl cat is 1,
        cmp_df$cmpCat[n] = 2        #then the category is 2
    } else {            #if the chl or mhw cat is not 1,
        cmp_df$cmpCat[n] = 0 #then the category is 0
    }
        }
cmp_df <- cmp_df[ , ! names(cmp_df) %in% c("lchlCat","mhwCat")] #remove lchlCat & mhwCat columns
file_name <- gsub(" ", "", paste("cmpnd_trn_unbal.csv"))
write.table(cmp_df,gsub(" ", "", paste("cmpndData/",file_name)),sep=",")
head(cmp_df)

##################################### Balance Training DF #####################################
# Get unbalanced compound training DF
file_name <- gsub(" ", "", paste("cmpnd_trn_unbal.csv"))
cmp_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)),sep=",")

# Find the unbalanced percentages of each category
cmp_ev <- nrow(subset(cmp_df, cmpCat=="3")) #number of compound events
chl_ev <- nrow(subset(cmp_df, cmpCat=="2")) #lchl only events
mhw_ev <- nrow(subset(cmp_df, cmpCat=="1")) #mhw only events
no_ev <- nrow(subset(cmp_df, cmpCat=="0")) #no event
tot_ev <- cmp_ev+chl_ev+mhw_ev+no_ev #total events
cmp_ev_per <- (cmp_ev/tot_ev)*100 #1.15% compound events (3)
chl_ev_per <- (chl_ev/tot_ev)*100 #14.9% lchl only (2)
mhw_ev_per <- (mhw_ev/tot_ev)*100 #8.54% mhw only (1)
no_ev_per <- (no_ev/tot_ev)*100 #75.34% no event (0)

# Calculate how many rows of each category to remove
cmp_ev <- nrow(subset(cmp_df, cmpCat=="3")) #least observations
chl_ev <- nrow(subset(cmp_df, cmpCat=="2"))
mhw_ev <- nrow(subset(cmp_df, cmpCat=="1"))
no_ev <- nrow(subset(cmp_df, cmpCat=="0"))
n_2rows_remov = chl_ev - cmp_ev #how many rows to remove from cat2
n_1rows_remov = mhw_ev - cmp_ev #how many rows to remove from cat1
n_0rows_remov = no_ev - cmp_ev #how many rows to remove from cat0

# Isolate and remove n rows where cmpCat==0
conditions <- cmp_df$cmpCat==0 #set conditions for removed rows, cat==0
rows_remov <- which(conditions==TRUE) #only remove rows where conditions true
n_rows_remov <- n_0rows_remov
sampled.cats <- sample(rows_remov, n_rows_remov) #sample to remove
cmp_bal_df <- cmp_df[-sampled.cats, ]

# Isolate and remove n rows where cmpCat==2
conditions <- cmp_bal_df$cmpCat==2 #set conditions for removed rows, cat==2
rows_remov <- which(conditions==TRUE) #only remove rows where conditions true
n_rows_remov <- n_2rows_remov
sampled.cats <- sample(rows_remov, n_rows_remov)
cmp_bal_df <- cmp_bal_df[-sampled.cats, ]

# Isolate and remove n rows where cmpCat==1
conditions <- cmp_bal_df$cmpCat==1 #set conditions for removed rows, cat==1
rows_remov <- which(conditions==TRUE) #only remove rows where conditions true
n_rows_remov <- n_1rows_remov
sampled.cats <- sample(rows_remov, n_rows_remov)
cmp_bal_df <- cmp_bal_df[-sampled.cats, ]

# Save balanced compound training df
file_name <- gsub(" ", "", paste("cmpnd_trn.csv"))
write.table(cmp_bal_df,gsub(" ", "", paste("cmpndData/",file_name)),sep=",") #save balanced lchl dataset
    
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
