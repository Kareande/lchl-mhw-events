setwd("/home/kareande/lchl-mhw-events")
library("foreach") #parallel processing
library("doParallel") #parallel processing
library("dplyr") #lags
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
        lchl_df$lchlCat[i] = 0       #then the category is 0
    } else if(lchl_df$chl[i] <= cat1) { #if chl value is <= the bottom 10%,
        lchl_df$lchlCat[i] = 1              #then the category is 1
    } else {           #if the chl value is > than the 10th percentile,
        lchl_df$lchlCat[i] = 0 #then the category is 0
    }
        }
doParallel::stopImplicitCluster()
head(lchl_df)

# Save df
file_name <- paste("chl_unprocessed.csv")
file_path <- gsub(" ", "", paste("cmpndData/",file_name))
write.table(lchl_df,file_path,sep=",")


################################ Condense date and location data ################################
# Load unbalanced lChl df
file_name <- paste("chl_unprocessed.csv")
lchl_unb_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)), sep=",", header=TRUE)
#head(lchl_unb_df)

# Load unbalanced MHW df
file_name <- paste("mhw_unprocessed.csv")
mhw_unb_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)), sep=",", header=TRUE)
mhw_unb_df$mhwCat[mhw_unb_df$mhwCat > 0] <- 1 #condense MHW cats into two categories (presence or absence)
#head(mhw_unb_df)

# Create date columns
lchl_unb_df$date <- gsub(" ", "", paste(lchl_unb_df$yr, "-", lchl_unb_df$mo, "-",lchl_unb_df$day)) #merge yr, mo, da columns
mhw_unb_df$date <- gsub(" ", "", paste(mhw_unb_df$yr, "-", mhw_unb_df$mo, "-",mhw_unb_df$day))
lchl_unb_df <- lchl_unb_df %>% relocate(date, .before = lon) #move date to be in front of lon
mhw_unb_df <- mhw_unb_df %>% relocate(date, .before = lon)
head(lchl_unb_df)
head(mhw_unb_df)

# Calculate days since for LChl DF
date_1 <- as.Date(lchl_unb_df$date[1])
date_2 <- as.Date(lchl_unb_df$date)
lchl_unb_df$ds <- difftime(date_1 ,date_2 , units = c("days"))
lchl_unb_df <- lchl_unb_df %>% relocate(ds, .before = lon) #move days since column before lon
head(lchl_unb_df)

# Calculate days since for MHW DF
date_1 <- as.Date(mhw_unb_df$date[1])
date_2 <- as.Date(mhw_unb_df$date)
mhw_unb_df$ds <- difftime(date_1 ,date_2 , units = c("days"))
mhw_unb_df <- mhw_unb_df %>% relocate(ds, .before = lon) #move days since column before lon
head(mhw_unb_df)

# Calculate doy from date for LChl DF
names(lchl_unb_df)[names(lchl_unb_df) == "date"] <- "doy"
lchl_unb_df$doy <- yday(lchl_unb_df$doy)
head(lchl_unb_df)

# Calculate doy from date for MHW DF
names(mhw_unb_df)[names(mhw_unb_df) == "date"] <- "doy"
mhw_unb_df$doy <- yday(mhw_unb_df$doy)
head(mhw_unb_df)

# Make lat/lon ranges equal on both datasets
lchl_unb_df$lon <- abs(lchl_unb_df$lon) #make lchl lons positive
lchl_unb_df <- lchl_unb_df[lchl_unb_df$lon<170, ] #length 5138102
mhw_unb_df <- mhw_unb_df[mhw_unb_df$lon>=102.5, ] #length 5138102
lchl_unb_df <- lchl_unb_df[lchl_unb_df$lat>10.625, ] #length 5138102

# Find range of lon and lat for each df (verify same range)
print(paste("lchl_unb_df longitudes goes from",toString(min(lchl_unb_df$lon)),"to",toString(max(lchl_unb_df$lon)))) #102.5 to 167.5
print(paste("mhw_unb_df longitudes goes from",toString(min(mhw_unb_df$lon)),"to",toString(max(mhw_unb_df$lon)))) #102.5 to 167.5
print(paste("lchl_df latitudes goes from",toString(min(lchl_unb_df$lat)),"to",toString(max(lchl_unb_df$lat)))) #12.5 to 62.5
print(paste("mhw_unb_df latitudes goes from",toString(min(mhw_unb_df$lat)),"to",toString(max(mhw_unb_df$lat)))) #12.5 to 62.5

# Extract unique values of lon and lat (veryify same values)
unique(lchl_unb_df$lon) #27 vals
unique(mhw_unb_df$lon) #27 vals 
unique(lchl_unb_df$lat) #21 vals
unique(mhw_unb_df$lat) #20 vals, missing 60.0

# Create location column
lchl_unb_df$location <- gsub(" ", "", paste(lchl_unb_df$lat, "-", lchl_unb_df$lon)) #merge lat and lon
mhw_unb_df$location <- gsub(" ", "", paste(mhw_unb_df$lat, "-", mhw_unb_df$lon))
lchl_unb_df <- lchl_unb_df %>% relocate(location, .before = nit) #move location column before nit
mhw_unb_df <- mhw_unb_df %>% relocate(location, .before = qnet) #move location column before qnet

# Reorder datasets to aid lags grouping
lchl_unb_df <- lchl_unb_df[order(lchl_unb_df$location),] #reorder dataset by location instead of date
mhw_unb_df <- mhw_unb_df[order(mhw_unb_df$location),]
lchl_unb_df$ds <- abs(lchl_unb_df$ds) #make days since a positive number
mhw_unb_df$ds <- abs(mhw_unb_df$ds)
head(mhw_unb_df)
head(lchl_unb_df)

# Save DF's
file_name <- paste("chl_lagsprep.csv")
write.table(lchl_unb_df,gsub(" ", "", paste("cmpndData/",file_name)),sep=",")
file_name <- paste("mhw_lagsprep.csv")
write.table(mhw_unb_df,gsub(" ", "", paste("cmpndData/",file_name)),sep=",")


##################################### Add Leads #####################################
# Get LChl df
file_name <- paste("chl_lagsprep.csv")
lchl_unb_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)),sep=",")
head(lchl_unb_df)

# Create LChl DF's with various values of lags
vars_lag = c(2, 7, 14, 180, 365, 730) #2 days, 1 wk, 2 wk, 6 mo, 1yr, 2yr
vars_lchl = c(colnames(lchl_unb_df))[-c(1:8,15)] #var names
doParallel::registerDoParallel(32)
for(i in 1:length(vars_lag)) { #for each lag value...
    dyn_col <- c()
    for(v in 1:length(vars_lchl)) {
        dyn_col[v] <- gsub(" ", "", paste(vars_lchl[v],vars_lag[i])) #create dynamic lagged col name
        }
    lchl_unb_lag_df <- lchl_unb_df %>% #create lagged df
        group_by(location) %>% #limit shifting of rows to be by date
        dplyr::mutate(!!dyn_col[1] := dplyr::lead(nit, n = vars_lag[i], default = NA)) %>% #add lag to nit-- CANNOT LOOP
        dplyr::mutate(!!dyn_col[2] := dplyr::lead(oxy, n = vars_lag[i], default = NA)) %>% #add lag to oxy-- B/C 1ST ARG
        dplyr::mutate(!!dyn_col[3] := dplyr::lead(pho, n = vars_lag[i], default = NA)) %>% #add lag to pho-- IN lead()
        dplyr::mutate(!!dyn_col[4] := dplyr::lead(chl, n = vars_lag[i], default = NA)) %>% #add lag to chl-- CANT BE
        dplyr::mutate(!!dyn_col[5] := dplyr::lead(sil, n = vars_lag[i], default = NA)) %>% #add lag to sil-- CHANGED TO
        dplyr::mutate(!!dyn_col[6] := dplyr::lead(npp, n = vars_lag[i], default = NA)) %>% #add lag to npp-- vars_lchl[v]
        as.data.frame()
    for(v in 1:length(vars_lchl)) {
        lchl_unb_lag_df <- lchl_unb_lag_df %>% relocate(gsub(" ", "", paste(vars_lchl[v],vars_lag[i])), .after = vars_lchl[v])
        }
    df_name <- gsub(" ", "", paste("lchl_",vars_lag[i],"lag.csv")) #create dyamic lag df name
    write.table(lchl_unb_lag_df,gsub(" ", "", paste("cmpndData/",df_name)),sep=",") #save df w/ each lag value
    }
doParallel::stopImplicitCluster()
head(lchl_unb_lag_df)
rm(lchl_unb_lag_df, vars_lchl, lchl_unb_df)

# Get MHW df
file_name <- paste("mhw_lagsprep.csv")
mhw_unb_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)),sep=",")

# Create MHW DF's with various values of lags
vars_mhw = c(colnames(mhw_unb_df))[-c(1:8,15)] #var names
doParallel::registerDoParallel(32)
for(i in 1:length(vars_lag)) { #for each lag value...
    dyn_col <- c()
    for(v in 1:length(vars_mhw)) {
        dyn_col[v] <- gsub(" ", "", paste(vars_mhw[v],vars_lag[i])) #create dynamic lagged col name
        }
    mhw_unb_lag_df <- mhw_unb_df %>% #create lagged df
        group_by(location) %>% #limit shifting of rows to be by date
        dplyr::mutate(!!dyn_col[1] := dplyr::lead(qnet, n = vars_lag[i], default = NA)) %>%   #add var1 lag-- CANNOT LOOP
        dplyr::mutate(!!dyn_col[2] := dplyr::lead(slp, n = vars_lag[i], default = NA)) %>%    #add var2 lag-- B/C 1ST ARG
        dplyr::mutate(!!dyn_col[3] := dplyr::lead(sat, n = vars_lag[i], default = NA)) %>%    #add var3 lag-- IN LAG()
        dplyr::mutate(!!dyn_col[4] := dplyr::lead(wndsp, n = vars_lag[i], default = NA)) %>%  #add var4 lag-- CANT BE
        dplyr::mutate(!!dyn_col[5] := dplyr::lead(sst, n = vars_lag[i], default = NA)) %>%    #add var5 lag-- CHANGED TO
        dplyr::mutate(!!dyn_col[6] := dplyr::lead(sstRoC, n = vars_lag[i], default = NA)) %>% #add var6 lag-- vars_mhw[v]
        as.data.frame()
    for(v in 1:length(vars_mhw)) {
        mhw_unb_lag_df <- mhw_unb_lag_df %>% relocate(gsub(" ", "", paste(vars_mhw[v],vars_lag[i])), .after = vars_mhw[v])
        }
    df_name <- gsub(" ", "", paste("mhw_",vars_lag[i],"lag.csv")) #create dyamic lag df name
    write.table(mhw_unb_lag_df,gsub(" ", "", paste("cmpndData/",df_name)),sep=",") #save df w/ each lag value
    }
doParallel::stopImplicitCluster()
head(mhw_unb_lag_df)

# Combine DF's 
for(i in 1:length(vars_lag)){
    file_name <- gsub(" ", "", paste("lchl_",vars_lag[i],"lag.csv")) #create dyamic df name
    lchl_unb_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)),sep=",")
    lchl_unb_df <- lchl_unb_df[,-c(5,8)] #remove days since and location
    file_name <- gsub(" ", "", paste("mhw_",vars_lag[i],"lag.csv")) #create dyamic df name
    mhw_unb_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)),sep=",")
    mhw_unb_df <- mhw_unb_df[,-c(5,8)] #remove days since and location
    cmp_df <- merge(lchl_unb_df, mhw_unb_df, c("day", "mo", "yr", "doy", "lon", "lat")) #merge based on these columns
    cmp_df <- na.omit(cmp_df) #remove NA values
    file_name <- gsub(" ", "", paste("cmpnd_uncat_",vars_lag[i],"lag.csv")) #create dyamic df name
    write.table(cmp_df,gsub(" ", "", paste("cmpndData/",file_name)),sep=",")
    }
head(cmp_df)
rm(lchl_unb_df,mhw_unb_df,cmp_df)


##################################### Add Compound Cats #####################################
doParallel::registerDoParallel(32)
for(i in 1:length(vars_lag)){
    file_name <- gsub(" ", "", paste("cmpnd_uncat_",vars_lag[i],"lag.csv")) #create dyamic df name
    cmp_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)),sep=",") #get cmpnd df
    cmp_df <- cmp_df %>% relocate(lchlCat, .before = mhwCat)
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
    file_name <- gsub(" ", "", paste("cmpnd_",vars_lag[i],"lag.csv")) #create dyamic df name
    write.table(cmp_df,gsub(" ", "", paste("cmpndData/",file_name)),sep=",")
    }
doParallel::stopImplicitCluster()
head(cmp_df)
rm(cmp_df)
    

############################# Split into Testing and Training DFs #############################
set.seed(313)
doParallel::registerDoParallel(32)
for(i in 1:length(vars_lag)){
    file_name <- gsub(" ", "", paste("cmpnd_",vars_lag[i],"lag.csv")) #create dyamic df name
    cmp_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)),sep=",") #get cmpnd df
    cmp_df <- cmp_df[ , ! names(cmp_df) %in% 
                             c("day","mo","lchlCat","mhwCat","nit", #remove day, mo, lchlCat, and mhwCat columns
                               "oxy","pho","chl","sil","npp","qnet", #remove unlagged lchl vars
                               "slp","sat","wndsp","sst","sstRoC")] #remove unlagged mhw vars
    conditions <- cmp_df$yr<2018 #set conditions for sampled rows
    smpld_rows <- which(conditions==TRUE) #only sample rows where conditions true
    n_smpld <- nrow(cmp_df)*(2/3) #set how many rows to sample
    sampled.cats <- sample(smpld_rows, n_smpld) #create sample
    cmp_train <- cmp_df[sampled.cats,] #dataset with sample
    cmp_test <- cmp_df[-sampled.cats, ] #dataset excluding sample
    tst_name <- gsub(" ", "", paste("cmpnd_tst_",vars_lag[i],"lag.csv")) #create dyamic df name
    trn_name <- gsub(" ", "", paste("cmpnd_trn_unb_",vars_lag[i],"lag.csv")) #create dyamic df name
    write.table(cmp_test,gsub(" ", "", paste("cmpndData/",tst_name)),sep=",")
    write.table(cmp_train,gsub(" ", "", paste("cmpndData/",trn_name)),sep=",")
    }
doParallel::stopImplicitCluster()

    
##################################### Balance Training DF #####################################
set.seed(313)
doParallel::registerDoParallel(32)
for(i in 1:length(vars_lag)){
    file_name <- gsub(" ", "", paste("cmpnd_trn_unb_",vars_lag[i],"lag.csv")) #create dyamic df name
    cmp_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)),sep=",") #get cmpnd df
    
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
    cmp_x19_df <- subset(cmp_df, yr!=2019) #no 2019
    x19_cmp_ev <- nrow(subset(cmp_x19_df, cmpCat=="3")) #least observations
    x19_chl_ev <- nrow(subset(cmp_x19_df, cmpCat=="2"))
    x19_mhw_ev <- nrow(subset(cmp_x19_df, cmpCat=="1"))
    x19_no_ev <- nrow(subset(cmp_x19_df, cmpCat=="0"))
    cmp_19_df <- subset(cmp_df, yr==2019) #only 2019
    w19_cmp_ev <- nrow(subset(cmp_19_df, cmpCat=="3")) #least observations
    w19_chl_ev <- nrow(subset(cmp_19_df, cmpCat=="2"))
    w19_mhw_ev <- nrow(subset(cmp_19_df, cmpCat=="1"))
    w19_no_ev <- nrow(subset(cmp_19_df, cmpCat=="0"))
    cat3_tot = x19_cmp_ev + w19_cmp_ev
    n_2rows_remov = x19_chl_ev + w19_chl_ev - cat3_tot #how many rows to remove from cat2
    n_1rows_remov = x19_mhw_ev + w19_mhw_ev - cat3_tot #how many rows to remove from cat1
    n_0rows_remov = x19_no_ev + w19_no_ev - cat3_tot #how many rows to remove from cat0
    
    # Isolate and remove n rows where year<=2019 and cmpCat==0
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

    # Save balanced compound df
    file_name <- gsub(" ", "", paste("cmpnd_trn_",vars_lag[i],"lag.csv")) #create dyamic df name
    write.table(cmp_bal_df,gsub(" ", "", paste("cmpndData/",file_name)),sep=",") #save balanced lchl dataset
    }
doParallel::stopImplicitCluster()

# Verify the balanced percentages of each category for each balanced DF
bal_cmp_ev_pers <- c()
bal_chl_ev_pers <- c()
bal_mhw_ev_pers <- c()
bal_no_ev_pers <- c()

set.seed(313)
doParallel::registerDoParallel(8)
for(i in 1:length(vars_lag)){
    file_name <- gsub(" ", "", paste("cmpnd_trn_",vars_lag[i],"lag.csv")) #create dyamic df name
    cmp_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)),sep=",") #get cmpnd df
    bal_cmp_ev <- nrow(subset(cmp_bal_df, cmpCat=="3")) #number of compound events
    bal_chl_ev <- nrow(subset(cmp_bal_df, cmpCat=="2")) #lchl only events
    bal_mhw_ev <- nrow(subset(cmp_bal_df, cmpCat=="1")) #mhw only events
    bal_no_ev <- nrow(subset(cmp_bal_df, cmpCat=="0")) #no events
    bal_tot_ev <- bal_cmp_ev+bal_chl_ev+bal_mhw_ev+bal_no_ev #total observations
    bal_cmp_ev_pers[i] <- (bal_cmp_ev/bal_tot_ev)*100 #compound events (3)
    bal_chl_ev_pers[i] <- (bal_chl_ev/bal_tot_ev)*100 #lchl only (2)
    bal_mhw_ev_pers[i] <- (bal_mhw_ev/bal_tot_ev)*100 #mhw only (1)
    bal_no_ev_pers[i] <- (bal_no_ev/bal_tot_ev)*100 #no event (0)
    }
doParallel::stopImplicitCluster()
    
bal_cmp_ev_pers #25, 25, 25, 25
bal_chl_ev_pers #25, 25, 25, 25
bal_mhw_ev_pers #25, 25, 25, 25
bal_no_ev_pers #25, 25, 25, 25
