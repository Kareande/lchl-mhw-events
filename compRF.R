setwd("/auto/home/kareande/mhwData") #working directory with datasets
library("foreach")
#install.packages()

##################################### Define LChl categories #####################################
# Get unbalanced and uncategorized lchl df
lchl_df <- read.csv("/master_with_contime_df.csv", sep=",", header=TRUE)
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
lchl_cat2 <- vector() #create empty vector to fill with categories
x <- length(chl_col) #get number of datapoints
doParallel::registerDoParallel()
for(i in 1:x) { #loop to fill out categories
    if (is.na(chl_col[i])) { #if value i of chl column is NA,
        lchl_cat2[i] = 0       #then the category is 0
    } else if(chl_col[i] <= cat1) { #if chl value is <= the bottom 10%,
        lchl_cat2[i] = 1              #then the category is 1
    } else {           #if the chl value is > than the 10th percentile,
        lchl_cat2[i] = 0 #then the category is 0
    }
        }
doParallel::stopImplicitCluster()
length(lchl_cat2)

# Add category column to df
lchl2_df <- cbind(lchl_df,lchl_cat2)
colnames(lchl2_df) <- c("day","mo","yr","lon","lat","nit","oxy","pho","chl","sil","npp","lchlCat")

# Save df
csvfile <- "/chl_unbalanced_df.csv"
write.table(lchl2_df,csvfile,sep=",") #save as csv


##################################### Combine LChl and MHW dataframes #####################################
# Get MHW df
mhw_df <- read.csv("/mhw_unbalanced_df.csv", sep=",", header=TRUE) #get df with mhw cats
#head(mhw_df)
#tail(mhw_df)

# Get LChl df
lchl_df <- read.csv("/chl_unbalanced_df.csv", sep=",", header=TRUE) #get df with chl cats
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
# Get unbalanced, uncategorized compound df
comp_df <- read.csv("/comp_unbalanced_df.csv", sep=",", header=TRUE)
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
    
# Save df with compound event categories
csvfile <- "/comp_unbalanced_df.csv"
write.table(comp_df,csvfile,sep=",")
    
##################################### Balance Compound Df #####################################
# Get df with mhw and lchl compound events
comp_df <- read.csv("/comp_unbalanced_df.csv", sep=",", header=TRUE) #get df with categorized mhw and lchl
head(comp_df)
    
# Find the percentages of each category; no events (0), mhw event (1), lchl event (2), compound event (4)
comp_ev <- nrow(subset(comp_df, compCat=="4")) #number of compound events, 437900
chl_ev <- nrow(subset(comp_df, compCat=="2")) #lchl only events, 0
mhw_ev <- nrow(subset(comp_df, compCat=="1")) #mhw only events, 2280354
no_ev <- nrow(subset(comp_df, compCat=="0")) #no events, 0
tot_ev <- comp_ev+chl_ev+mhw_ev+no_ev

comp_ev_per <- (comp_ev/tot_ev)*100 #16% compound events (4)
chl_ev_per <- (chl_ev/tot_ev)*100 #0% lchl only (2)
mhw_ev_per <- (mhw_ev/tot_ev)*100 #83% mhw only (1)
no_ev_per <- (no_ev/tot_ev)*100 #0% no event (0)
comp_ev_per
chl_ev_per
mhw_ev_per
no_ev_per
    
# Randomly remove n rows where year<2019 and cat=="1" to balance dataset
comp_x19_df <- comp_df[comp_df$yr !=2019, ] #length 2623292
find.cat.1 <- comp_x19_df$compCat == 1 # find row numbers where cat == 1
perc.cat.1 <- round(sum(find.cat.1) * 0.97) # find X% of cat.1
cat.1 <- which(find.cat.1 == TRUE)
sampled.perc <- sample(cat.1, perc.cat.1) # row numbers of X% of cat.1
comp_bal_df <- comp_df[-sampled.perc, ] # in your final output, include all but the X%, length 588642

# Find the percentages of each category; no events (0), mhw event (1), lchl event (2), compound event (4)
bal_comp_ev <- nrow(subset(comp_bal_df, compCat=="4")) #number of compound events, 280290
bal_chl_ev <- nrow(subset(comp_bal_df, compCat=="2")) #lchl only events, 0
bal_mhw_ev <- nrow(subset(comp_bal_df, compCat=="1")) #mhw only events, 308352
bal_no_ev <- nrow(subset(comp_bal_df, compCat=="0")) #no events, 0
bal_tot_ev <- comp_ev+chl_ev+mhw_ev+no_ev

bal_comp_ev_per <- (bal_comp_ev/bal_tot_ev)*100 #10.31% compound events (4)
bal_chl_ev_per <- (bal_chl_ev/bal_tot_ev)*100 #0% lchl only (2)
bal_mhw_ev_per <- (bal_mhw_ev/bal_tot_ev)*100 #11.34% mhw only (1)
bal_no_ev_per <- (bal_no_ev/bal_tot_ev)*100 #0% no event (0)
bal_comp_ev_per
bal_chl_ev_per
bal_mhw_ev_per
bal_no_ev_per

# Save balanced compound df
csvfile <- "/comp_balanced_df.csv"
write.table(comp_bal_df,csvfile,sep=",") #save balanced lchl dataset
    

##################################### Train Compound RF Model #####################################
