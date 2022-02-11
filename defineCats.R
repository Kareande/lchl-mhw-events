setwd("/auto/home/kareande/mhwData") #working directory with datasets
#library()
#install.packages()

##################################### Define LChl categories #####################################
# Get dataset
lchl_df <- read.csv("/master_with_contime_df.csv", sep=",", header=TRUE) #get chl df
lchl_df <- na.omit(lchl_df) #remove NA values
head(lchl_df)

# Select chl column
chl_col <- lchl_df[,9] 

# Calculate percentiles
cat1 <- quantile(qchl_col,probs=0.1) #what value is the 10th percentile cutoff, or category 1 minimum value
cat2 <- quantile(qchl_col,probs=0.075) #category 2 minimum
cat3 <- quantile(qchl_col,probs=0.05) #category 3 minimum
cat4 <- quantile(qchl_col,probs=0.025) #category 4 minimum
cat1 #0.37448388308533
cat2 #0.302988503263755
cat3 #0.237521889263933
cat4 #0.177686934912008

# Check chl values
max(qchl_col) #350.995544433594 mg/m^3
min(qchl_col) #9.74359052937264e-06 mg/m^3

# Divide chl events into 2 categories
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
length(lchl_cat)

# Divide LChl events into 3 cats
#lchl_cat <- vector() #create empty vector to fill with categories
#x <- length(chl_col) #get number of datapoints
#doParallel::registerDoParallel()
#for(i in 1:x) { #loop to fill out categories
#    if (is.na(chl_col[i])) { #if value i of chl column is NA,
#        lchl_cat[i] = 0       #then the category is 0
#    } else if(chl_col[i] <= cat3) { #if value i of chl column is <= the bottom 5%,
#        lchl_cat[i] = 2       #then the category is 2
#    } else if(chl_col[i] <= cat1) { #if chl value is <= the bottom 10%,
#        lchl_cat[i] = 1              #then the category is 1
#    } else {           #if the chl value is > than the 10th percentile,
#        lchl_cat[i] = 0 #then the category is 0
#    }
        }
#doParallel::stopImplicitCluster()
#length(lchl_cat)

# Divide LChl events into 5 cats
#lchl_cat <- vector() #create empty vector to fill with categories
#x <- length(chl_col) #get number of datapoints
#doParallel::registerDoParallel()
#for(i in 1:x) { #loop to fill out categories
#    if (is.na(chl_col[i])) { #if value i of chl column is NA, then FALSE
#        lchl_cat[i] = 0       #then the category is 0
#    } else if(chl_col[i] <= cat4) { #if value i of chl column is <= the category 4 cut-off,
#      lchl_cat[i] == 4       #then the category is 4
#    } else if(chl_col[i] <= cat3) { #if chl value is <= category 3 cutoff,
#      lchl_cat[i] == 3              #then the category is 3
#    } else if(chl_col[i] <= cat2) {#if chl value is <= category 2 cutoff,
#      lchl_cat[i] == 2             #then the category is 2
#    } else if(chl_col[i] <= cat1) {#if chl value is <= category 1 cutoff,
#      lchl_cat[i] == 1             #then the category is 1
#    } else {           #if the chl value is greater than the 10th percentile,
#      lchl_cat[i] == 0 #then the category is 0
#    }
#        }
#doParallel::stopImplicitCluster()
#length(lchl_cat)

# Add category column to df
lchl_df <- cbind(lchl_df,lchl_cat)
colnames(lchl_df) <- c("day","mo","yr","lon","lat","nit","oxy","pho","chl","sil","npp","lchlCat")

# Save df
csvfile <- "/chl_unbalanced_df.csv"
write.table(lchl_df,csvfile,sep=",") #save as csv


##################################### Combine LChl and MHW dataframes #####################################
# Get MHW df
mhw_df <- read.csv("/mhw_unbalanced_df.csv", sep=",", header=TRUE) #get df with mhw
#head(mhw_df)
#tail(mhw_df)

# Get LChl df
lchl_df <- read.csv("/chl_unbalanced_df.csv", sep=",", header=TRUE) #get df with chl
#head(lchl_df)
#tail(lchl_df)

# Correct ranges of lat/lon to be equal on both datasets
#lchl_df2 <- lchl_df[lchl_df$lon<170, ] #length 5138102
#mhw_df2 <- mhw_df[mhw_df$lon>102.5, ] #length 5138102
#lchl_df2 <- lchl_df[lchl_df$lat>10.625, ] #length 5138102

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

# Combine df's
comp_df <- merge(lchl_df, mhw_df, c("day","mo","yr","lon","lat"))
colnames(comp_df) <- c("day","mo","yr","lon","lat","nit","oxy","pho","chl","sil","npp","lchlCat",
                       "qnet","slp","sat","wndSp","sst","sstRoC","mhwCat")
lchlCat <- comp_df$lchlCat #isolate the lchl category column
mhwCat <- comp_df$mhwCat #isolate the mhw cat column
comp_df <- comp_df[,-12] #remove lchl from current spot
comp_df <- comp_df[,-18] #remove mhw from current spot
comp_df <- cbind(comp_df, lchlCat, mhwCat) #put the category columns at end of dataset
head(comp_df)
nrow(comp_df)

# Save compound df
csvfile <- "/comp_unbalanced_df.csv"
write.table(comp_df,csvfile,sep=",") #create master csv with day/month/year


##################################### Define Compound Cats #####################################
comp_df <- read.csv("/comp_unbalanced_df.csv", sep=",", header=TRUE) #get df with chl
head(comp_df)
#tail(comp_df)

chl_col <- comp_df[,18] #select chl cat column
mhw_col <- comp_df[,19] #select mhw cat column

# Create a compound category column of either no events (0), mhw event (1), lchl event (2), compound event (4)
comp_cat <- vector() #create empty vector to fill with categories
x <- length(chl_col) #get number of datapoints

#doParallel::registerDoParallel()
for(i in 1:x) { #loop to fill out categories
    if (is.na(chl_col[i]) || is.na(mhw_col[i])) { #if value i of chl column OR mhw column is NA,
        comp_cat[i] = 0                           #then the category is 0
    } else if(chl_col[i]==1 && mhw_col[i]==1) { #if chl cat AND mhw cat is 1,
        comp_cat[i] = 4                         #then the category is 4
    } else if(mhw_col[i]==1) { #if only the mhw cat is 1,
        comp_cat[i] = 1        #then the category is 1
    } else if(chl_col[i]==1) { #if only the chl cat is 1,
        comp_cat = 2           #then the category is 2
    } else {            #if the chl or mhw cat is not 1,
        comp_cat[i] = 0 #then the category is 0
    }
        }
#doParallel::stopImplicitCluster()
length(comp_cat)

# Create a compound category column of either both events (1) or not (0)
#comp_cat <- vector() #create empty vector to fill with categories
#x <- length(chl_col) #get number of datapoints

#doParallel::registerDoParallel()
#for(i in 1:x) { #loop to fill out categories
#    if (is.na(chl_col[i]) || is.na(mhw_col[i])) { #if value i of chl column OR mhw column is NA,
#        comp_cat[i] = 0                           #then the category is 0
#    } else if(chl_col[i]==1 && mhw_col[i]==1) { #if chl cat AND mhw cat is 1,
#        comp_cat[i] = 1                         #then the category is 1
#    } else {            #if the chl or mhw cat is not 1,
#        comp_cat[i] = 0 #then the category is 0
#    }
#        }
#doParallel::stopImplicitCluster()
#length(lchl_cat)
    
comp_df <- cbind(comp_df,comp_cat)
colnames(comp_df) <- c("day","mo","yr","lon","lat","nit","oxy","pho","chl","sil","npp",
                       "qnet","slp","sat","wndSp","sst","sstRoC","lchlCat","mhwCat","compCat")
    
csvfile <- "/comp_unbalanced_df.csv"
write.table(comp_df,csvfile,sep=",") #save df with compound event categories
