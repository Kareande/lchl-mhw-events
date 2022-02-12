setwd("/auto/home/kareande/mhwData") #working directory with datasets
library("foreach")
#install.packages()

##################################### Define LChl categories #####################################
# Get unbalanced and uncategorized lchl df
lchl_df <- read.csv("master_with_contime_df.csv", sep=",", header=TRUE)
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
length(lchl_cat)

# Add category column to df
lchl_df <- cbind(lchl_df,lchl_cat)
colnames(lchl_df) <- c("day","mo","yr","lon","lat","nit","oxy","pho","chl","sil","npp","lchlCat")

# Save df
csvfile <- "chl_unbalanced_df.csv"
write.table(lchl_df,csvfile,sep=",")


##################################### Combine LChl and MHW dataframes #####################################
# Get LChl df
lchl_df <- read.csv("chl_unbalanced_df.csv", sep=",", header=TRUE) #get df with chl cats
head(lchl_df)
#tail(lchl_df)

# Get MHW df
mhw_df <- read.csv("mhw_unbalanced_df.csv", sep=",", header=TRUE) #get df with mhw cats
mhw_df$mhwCat[mhw_df$mhwCat > 0] <- 1 #condense MHW cats into two categories
head(lchl_df)
#tail(mhw_df)

# Correct ranges of lat/lon to be equal on both datasets
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

# Combine dfs into compound df
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
comp_df <- na.omit(comp_df) #remove NA values
csvfile <- "comp_unbalanced_df.csv"
write.table(comp_df,csvfile,sep=",")


##################################### Add Compound Cats #####################################
# Get unbalanced, uncategorized compound df
comp_df <- read.csv("comp_unbalanced_df.csv", sep=",", header=TRUE)
chl_col <- comp_df[,18] #select chl cat column
mhw_col <- comp_df[,19] #select mhw cat column

# Create a compound category column of either no events (0), mhw event (1), lchl event (2), compound event (3)
comp_cat <- vector() #create empty vector to fill with categories
x <- length(chl_col) #get number of datapoints
#doParallel::registerDoParallel()
for(i in 1:x) { #loop to fill out categories
    if (is.na(chl_col[i]) || is.na(mhw_col[i])) { #if value i of chl column OR mhw column is NA,
        comp_cat[i] = 0                           #then the category is 0
    } else if(chl_col[i]==1 && mhw_col[i]==1) { #if chl cat AND mhw cat is 1,
        comp_cat[i] = 3                         #then the category is 3
    } else if(mhw_col[i]==1) { #if only the mhw cat is 1,
        comp_cat[i] = 1        #then the category is 1
    } else if(chl_col[i]==1) { #if only the chl cat is 1,
        comp_cat[i] = 2        #then the category is 2
    } else {            #if the chl or mhw cat is not 1,
        comp_cat[i] = 0 #then the category is 0
    }
        }
#doParallel::stopImplicitCluster()
length(comp_cat) #2718254

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

# Add compound cats to df
comp_df <- cbind(comp_df,comp_cat)
colnames(comp_df) <- c("day","mo","yr","lon","lat","nit","oxy","pho","chl","sil","npp",
                       "qnet","slp","sat","wndSp","sst","sstRoC","lchlCat","mhwCat","compCat")
    
# Save df with compound event categories
csvfile <- "comp_unbalanced_df.csv"
write.table(comp_df,csvfile,sep=",")
    
##################################### Balance Compound Df #####################################
# Get df with mhw and lchl compound events
comp_df <- read.csv("comp_unbalanced_df.csv", sep=",", header=TRUE) #get df with categorized mhw and lchl
head(comp_df)
#tail(comp_df)
    
# Find the unbalanced percentages of each category; no events (0), mhw event (1), lchl event (2), compound event (3)
comp_ev <- nrow(subset(comp_df, compCat=="3")) #number of compound events, 31474
chl_ev <- nrow(subset(comp_df, compCat=="2")) #lchl only events, 406426
mhw_ev <- nrow(subset(comp_df, compCat=="1")) #mhw only events, 232268
no_ev <- nrow(subset(comp_df, compCat=="0")) #no events, 2048086
tot_ev <- comp_ev+chl_ev+mhw_ev+no_ev #2718254

comp_ev_per <- (comp_ev/tot_ev)*100 #1.15% compound events (3)
chl_ev_per <- (chl_ev/tot_ev)*100 #14.9% lchl only (2)
mhw_ev_per <- (mhw_ev/tot_ev)*100 #8.54% mhw only (1)
no_ev_per <- (no_ev/tot_ev)*100 #75.34% no event (0)
comp_ev_per
chl_ev_per
mhw_ev_per
no_ev_per
    
# Calculate how many rows of each category to remove
comp_x19_df <- subset(comp_df, yr!=2019)
comp_19_df <- subset(comp_df, yr==2019)

x19_comp_ev <- nrow(subset(comp_x19_df, compCat=="3")) #28594
x19_chl_ev <- nrow(subset(comp_x19_df, compCat=="2")) #399222
x19_mhw_ev <- nrow(subset(comp_x19_df, compCat=="1")) #197840
x19_no_ev <- nrow(subset(comp_x19_df, compCat=="0")) #1997636

w19_comp_ev <- nrow(subset(comp_19_df, compCat=="3")) #2880
w19_chl_ev <- nrow(subset(comp_19_df, compCat=="2")) #7204
w19_mhw_ev <- nrow(subset(comp_19_df, compCat=="1")) #34428
w19_no_ev <- nrow(subset(comp_19_df, compCat=="0")) #50450

cat3_tot = x19_comp_ev + w19_comp_ev
n_2rows_remov = x19_chl_ev + w19_chl_ev - cat3_tot
n_1rows_remov = x19_mhw_ev + w19_mhw_ev - cat3_tot
n_0rows_remov = x19_no_ev + w19_no_ev - cat3_tot
    
# Isolate and remove n rows where year<=2019 and compCat==0
set.seed(313)
comp_x19_df <- subset(comp_df, yr!=2019) #exclude 2019
conditions <- comp_df$compCat==0 #set conditions for removed rows, cat==0
rows_remov <- which(conditions==TRUE) #only remove rows where conditions true
n_rows_remov <- n_0rows_remov
sampled.cats <- sample(rows_remov, n_rows_remov) #sample to remove
comp_bal_df <- comp_df[-sampled.cats, ]
    
# Isolate and remove n rows where year<=2019 and compCat==2
comp_x19_df <- subset(comp_bal_df, yr!=2019)
conditions <- comp_bal_df$compCat==2 #set conditions for removed rows, cat==2
rows_remov <- which(conditions==TRUE) #only remove rows where conditions true
n_rows_remov <- n_2rows_remov
sampled.cats <- sample(rows_remov, n_rows_remov)
comp_bal_df <- comp_bal_df[-sampled.cats, ]
    
# Isolate and remove n rows where year<=2019 and compCat==1
comp_x19_df <- subset(comp_bal_df, yr!=2019)
conditions <- comp_bal_df$compCat==1 #set conditions for removed rows, cat==1
rows_remov <- which(conditions==TRUE) #only remove rows where conditions true
n_rows_remov <- n_1rows_remov
sampled.cats <- sample(rows_remov, n_rows_remov)
comp_bal_df <- comp_bal_df[-sampled.cats, ]

# Find the balanced percentages of each category; no events (0), mhw event (1), lchl event (2), compound event (3)
bal_comp_ev <- nrow(subset(comp_bal_df, compCat=="3")) #number of compound events, 31474
bal_chl_ev <- nrow(subset(comp_bal_df, compCat=="2")) #lchl only events, 35798
bal_mhw_ev <- nrow(subset(comp_bal_df, compCat=="1")) #mhw only events, 63022
bal_no_ev <- nrow(subset(comp_bal_df, compCat=="0")) #no events, 79044
bal_tot_ev <- bal_comp_ev+bal_chl_ev+bal_mhw_ev+bal_no_ev

bal_comp_ev_per <- (bal_comp_ev/bal_tot_ev)*100 #25% compound events (3)
bal_chl_ev_per <- (bal_chl_ev/bal_tot_ev)*100 #25% lchl only (2)
bal_mhw_ev_per <- (bal_mhw_ev/bal_tot_ev)*100 #25% mhw only (1)
bal_no_ev_per <- (bal_no_ev/bal_tot_ev)*100 #25% no event (0)
bal_comp_ev_per
bal_chl_ev_per
bal_mhw_ev_per
bal_no_ev_per

# Save balanced compound df
csvfile <- "comp_balanced_df.csv"
write.table(comp_bal_df,csvfile,sep=",") #save balanced lchl dataset
    

##################################### Train Compound RF Model #####################################
