setwd("/home/kareande/lchl-mhw-events")
library("foreach") #parallel processing
library("doParallel") #parallel processing
library("dplyr") #lags
library("lubridate") #calculating day of year
#install.packages()

##################################### Define LChl categories #####################################
# Get unbalanced and uncategorized lchl df
file_name <- paste("chl_unprocessed.csv") #name of df
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
file_name <- paste("chl_unbalanced.csv")
file_path <- gsub(" ", "", paste("cmpndData/",file_name))
write.table(lchl_df,file_path,sep=",")

################################ Condense date and location data ################################
# Load unbalanced lChl df
file_name <- paste("chl_unbalanced.csv")
lchl_unb_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)), sep=",", header=TRUE)
#head(lchl_unb_df)

# Load unbalanced MHW df
file_name <- paste("mhw_unbalanced.csv")
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
date_1<-as.Date(lchl_unb_df$date[1])
doParallel::registerDoParallel(32)
for(i in 1:nrow(lchl_unb_df)) {
    date_2 <- as.Date(lchl_unb_df$date[i])
    lchl_unb_df$ds[i] <- difftime(date_1 ,date_2 , units = c("days"))
    }
doParallel::stopImplicitCluster()
lchl_unb_df <- lchl_unb_df %>% relocate(ds, .before = lon) #move days since column before lon
head(lchl_unb_df)

# Calculate days since for MHW DF
#date_1<-as.Date(mhw_unb_df$date[1]) #use date_1 value from lchl calc.
doParallel::registerDoParallel(32)
for(i in 1:nrow(mhw_unb_df)) {
    date_2 <- as.Date(mhw_unb_df$date[i])
    mhw_unb_df$ds[i] <- difftime(date_1 ,date_2 , units = c("days"))
    }
doParallel::stopImplicitCluster()
mhw_unb_df <- mhw_unb_df %>% relocate(ds, .before = lon) #move days since column before lon
head(mhw_unb_df)

# Calculate doy from date for LChl DF
names(lchl_unb_df)[names(lchl_unb_df) == "date"] <- "doy"
doParallel::registerDoParallel(16)
for(i in 1:nrow(lchl_unb_df)) {
    lchl_unb_df$doy[i] <- yday(lchl_unb_df$doy[i])
    }
doParallel::stopImplicitCluster()
head(lchl_unb_df)

# Calculate doy from date for MHW DF
names(mhw_unb_df)[names(mhw_unb_df) == "date"] <- "doy"
doParallel::registerDoParallel(16)
for(i in 1:nrow(mhw_unb_df)) {
    mhw_unb_df$doy[i] <- yday(mhw_unb_df$doy[i])
    }
doParallel::stopImplicitCluster()
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
lchl_unb_df$days_since <- abs(lchl_unb_df$days_since) #make days since a positive number
mhw_unb_df$days_since <- abs(mhw_unb_df$days_since)
head(mhw_unb_df)
head(lchl_unb_df)

# Save DF's
file_name <- paste("chl_unb_lagsprep.csv")
write.table(lchl_unb_df,gsub(" ", "", paste("cmpndData/",file_name)),sep=",")
file_name <- paste("mhw_unb_lagsprep.csv")
write.table(mhw_unb_df,gsub(" ", "", paste("cmpndData/",file_name)),sep=",")


##################################### Add Leads #####################################
# Get LChl df
file_name <- paste("chl_unb_lagsprep.csv")
lchl_unb_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)),sep=",")
head(lchl_unb_df)

# Create LChl DF's with 2, 7, 14, and 180 days of lags
vars_lag = c(2, 7, 14, 180) #2 days, 1 wk, 2 wk, 6 mo
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
        dplyr::mutate(!!dyn_col[3] := dplyr::lead(pho, n = vars_lag[i], default = NA)) %>% #add lag to pho-- IN LAG()
        dplyr::mutate(!!dyn_col[4] := dplyr::lead(chl, n = vars_lag[i], default = NA)) %>% #add lag to chl-- CANT BE
        dplyr::mutate(!!dyn_col[5] := dplyr::lead(sil, n = vars_lag[i], default = NA)) %>% #add lag to sil-- CHANGED TO
        dplyr::mutate(!!dyn_col[6] := dplyr::lead(npp, n = vars_lag[i], default = NA)) %>% #add lag to npp-- vars_lchl[v]
        as.data.frame()
    for(v in 1:length(vars_lchl)) {
        lchl_unb_lag_df <- lchl_unb_lag_df %>% relocate(gsub(" ", "", paste(vars_lchl[v],vars_lag[i])), .after = vars_lchl[v])
        }
    df_name <- gsub(" ", "", paste("lchl_unb_",vars_lag[i],"lag.csv")) #create dyamic lag df name
    write.table(lchl_unb_lag_df,gsub(" ", "", paste("cmpndData/",df_name)),sep=",") #save df w/ each lag value
    }
doParallel::stopImplicitCluster()
head(lchl_unb_lag_df)
rm(lchl_unb_lag_df, vars_lchl, lchl_unb_df)

# Get MHW df
file_name <- paste("mhw_unb_lagsprep.csv")
mhw_unb_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)),sep=",")
head(mhw_unb_df)

# Create MHW DF's with 2, 7, 14, and 180 days of lags
vars_lag = c(2, 7, 14, 180) #2 days, 1 wk, 2 wk, 6 mo
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
    df_name <- gsub(" ", "", paste("mhw_unb_",vars_lag[i],"lag.csv")) #create dyamic lag df name
    write.table(mhw_unb_lag_df,gsub(" ", "", paste("cmpndData/",df_name)),sep=",") #save df w/ each lag value
    }
doParallel::stopImplicitCluster()
head(mhw_unb_lag_df)

# Combine DF's
vars_lag = c(2, 7, 14, 180) #2 days, 1 wk, 2 wk, 6 mo
for(i in 1:length(vars_lag)){
    file_name <- gsub(" ", "", paste("lchl_unb_",vars_lag[i],"lag.csv")) #create dyamic df name
    lchl_unb_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)),sep=",")
    lchl_unb_df <- lchl_unb_df[,-c(5,8)] #remove days since and location
    file_name <- gsub(" ", "", paste("lchl_unb_",vars_lag[i],"lag.csv")) #create dyamic df name
    mhw_unb_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)),sep=",")
    mhw_unb_df <- mhw_unb_df[,-c(5,8)] #remove days since and location
    cmp_df <- merge(lchl_unb_df, mhw_unb_df, c("day", "mo", "yr", "doy", "lon", "lat")) #merge based on these columns
    file_name <- gsub(" ", "", paste("cmpnd_unb_",vars_lag[i],"lag.csv")) #create dyamic df name
    write.table(cmp_df,gsub(" ", "", paste("cmpndData/",file_name)),sep=",")
    head(cmp_df)
    }
rm(lchl_unb_df,mhw_unb_df,cmp_df)


##################################### Add Compound Cats #####################################
# Get lagged, unbalanced compound df
file_name <- paste("cmpnd_unb_2lags180.csv")
cmpnd_unb_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)),sep=",")
head(cmpnd_unb_df)

# Decide which lag to continue with
cols_cmp = c(colnames(cmpnd_unb_df)) #make list of mhw col_names
#cmp_df <- select(.data, contains(cols_cmp[,1:4, 6, 10, 14, 18, 22, 26, 30])) #columns to use for no lags
cmp_df <- select(.data, contains(cols_cmp[,1:4, 6, 10, 14, 18, 22, 26, 30])) #columns to use for 2d lags
#cmp_df <- select(.data, contains(cols_cmp[,1:4, 6, 10, 14, 18, 22, 26, 30])) #columns to use for 7d lags
#cmp_df <- select(.data, contains(cols_cmp[,1:4, 6, 10, 14, 18, 22, 26, 30])) #columns to use for 14d lags
#cmp_df <- select(.data, contains(cols_cmp[,1:4, 6, 10, 14, 18, 22, 26, 30])) #columns to use for 180 lags
chl_col <- cmpnd_unb_df$lchlCat #select chl cat column
mhw_col <- cmpnd_unb_df$mhwCats #select mhw cat column

# Create a compound category column of either no events (0), mhw event (1), lchl event (2), compound event (3)
cmp_cat <- vector() #create empty vector to fill with categories
x <- nrow(cmp_df) #get number of datapoints
doParallel::registerDoParallel()
for(i in 1:x) { #loop to fill out categories
    if (is.na(chl_col[i]) || is.na(mhw_col[i])) { #if value i of chl column OR mhw column is NA,
        cmp_cat[i] = 0                           #then the category is 0
    } else if(chl_col[i]==1 && mhw_col[i]==1) { #if chl cat AND mhw cat is 1,
        cmp_cat[i] = 3                         #then the category is 3
    } else if(mhw_col[i]==1) { #if only the mhw cat is 1,
        cmp_cat[i] = 1        #then the category is 1
    } else if(chl_col[i]==1) { #if only the chl cat is 1,
        cmp_cat[i] = 2        #then the category is 2
    } else {            #if the chl or mhw cat is not 1,
        cmp_cat[i] = 0 #then the category is 0
    }
        }
doParallel::stopImplicitCluster()
length(cmp_cat) #2718254

# Add compound cats to df
cmp_df <- cbind(cmp_df,cmp_cat)
colnames(cmp_df) <- c("day","mo","yr","lon","lat","nit","oxy","pho","chl","sil","npp",
                           "qnet","slp","sat","wndSp","sst","sstRoC","lchlCat","mhwCat","cmpCat") #CHANGE VARS TO MATCH DF******
head(cmp_df)

# Save df with compound event categories
file_name <- paste("cmpnd_cats_unb.csv")
write.table(cmp_df,gsub(" ", "", paste("cmpndData/",file_name)),sep=",")


##################################### Balance Compound DF #####################################
# Get df with mhw and lchl compound events
file_name <- paste("cmpnd_cats_unb.csv")
cmp_df <- read.csv(gsub(" ", "", paste("cmpndData/",file_name)), sep=",", header=TRUE)
head(cmp_df)

# Find the unbalanced percentages of each category; no events (0), mhw event (1), lchl event (2), compound event (3)
cmp_ev <- nrow(subset(cmp_df, cmpCat=="3")) #number of compound events, 31474
chl_ev <- nrow(subset(cmp_df, cmpCat=="2")) #lchl only events, 406426
mhw_ev <- nrow(subset(cmp_df, cmpCat=="1")) #mhw only events, 232268
no_ev <- nrow(subset(cmp_df, cmpCat=="0")) #no events, 2048086
tot_ev <- cmp_ev+chl_ev+mhw_ev+no_ev #total events, 2718254

cmp_ev_per <- (cmp_ev/tot_ev)*100 #1.15% compound events (3)
chl_ev_per <- (chl_ev/tot_ev)*100 #14.9% lchl only (2)
mhw_ev_per <- (mhw_ev/tot_ev)*100 #8.54% mhw only (1)
no_ev_per <- (no_ev/tot_ev)*100 #75.34% no event (0)
cmp_ev_per
chl_ev_per
mhw_ev_per
no_ev_per
    
# Calculate how many rows of each category to remove
cmp_x19_df <- subset(cmp_df, yr!=2019) #no 2019
cmp_19_df <- subset(cmp_df, yr==2019) #only 2019

x19_cmp_ev <- nrow(subset(cmp_x19_df, cmpCat=="3")) #28594, least observations
x19_chl_ev <- nrow(subset(cmp_x19_df, cmpCat=="2")) #399222
x19_mhw_ev <- nrow(subset(cmp_x19_df, cmpCat=="1")) #197840
x19_no_ev <- nrow(subset(cmp_x19_df, cmpCat=="0")) #1997636

w19_cmp_ev <- nrow(subset(cmp_19_df, cmpCat=="3")) #2880, least observations
w19_chl_ev <- nrow(subset(cmp_19_df, cmpCat=="2")) #7204
w19_mhw_ev <- nrow(subset(cmp_19_df, cmpCat=="1")) #34428
w19_no_ev <- nrow(subset(cmp_19_df, cmpCat=="0")) #50450

cat3_tot = x19_cmp_ev + w19_cmp_ev #31474
n_2rows_remov = x19_chl_ev + w19_chl_ev - cat3_tot #how many rows to remove from cat2
n_1rows_remov = x19_mhw_ev + w19_mhw_ev - cat3_tot #how many rows to remove from cat1
n_0rows_remov = x19_no_ev + w19_no_ev - cat3_tot #how many rows to remove from cat0
    
# Isolate and remove n rows where year<=2019 and cmpCat==0
set.seed(313)
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

# Save balanced compound df
file_name <- paste("cmpnd_balanced.csv")
write.table(cmp_bal_df,gsub(" ", "", paste("cmpndData/",file_name)),sep=",") #save balanced lchl dataset


