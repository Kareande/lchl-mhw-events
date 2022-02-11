setwd("/auto/home/kareande/mhwData/pacificData")
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(chron)

# Save the print(nc) dump to a text file
nc_data <- nc_open('global-reanalysis-bio-001-029-daily_1993.nc') #open nc file
#{
#    sink('global-reanalysis-bio-001-029-daily_1629509748397_metadata.txt') #sink data to a viewable .txt file
# print(nc_data)
#    sink()
#}

# Get variables from .NC file, see dimensions
lon <- ncvar_get(nc_data, "longitude")
nlon <- dim(lon)
lat <- ncvar_get(nc_data, "latitude")
nlat <- dim(lat)
print(c(nlon,nlat))

dep <- ncvar_get(nc_data, "depth")
ndep <- dim(dep)

time <- ncvar_get(nc_data, "time")
nt <- dim(time)
tunits <- ncatt_get(nc_data,"time","units")

# Create arrays of variables
nit.array <- ncvar_get(nc_data, "no3") # store the data in a 3-dimensional array
oxy.array <- ncvar_get(nc_data, "o2") # store the data in a 3-dimensional array
pho.array <- ncvar_get(nc_data, "po4") # store the data in a 3-dimensional array
chl.array <- ncvar_get(nc_data, "chl") # store the data in a 3-dimensional array
sil.array <- ncvar_get(nc_data, "si") # store the data in a 3-dimensional array
npp.array <- ncvar_get(nc_data, "nppv") # store the data in a 3-dimensional array
dim(npp.array)
fillvalue <- ncatt_get(nc_data, "nppv", "_FillValue")
fillvalue

nc_close(nc_data)

# Turn the fill values into NA and omit them
nit.array[nit.array == fillvalue$value] <- NA
oxy.array[oxy.array == fillvalue$value] <- NA
pho.array[pho.array == fillvalue$value] <- NA
chl.array[chl.array == fillvalue$value] <- NA
sil.array[sil.array == fillvalue$value] <- NA
npp.array[npp.array == fillvalue$value] <- NA

length(na.omit(as.vector(nit.array[,,1])))
length(as.vector(nit.array[,,1]))

# Convert nc time into date time
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1])
chron(time/24,origin=c(tmonth, tday, tyear))

nday <- length(chron(time/24,origin=c(tmonth, tday, tyear)))

# Take slice w/ all time steps
nit.slice <- nit.array[,,1:nday]
oxy.slice <- oxy.array[,,1:nday]
pho.slice <- pho.array[,,1:nday]
chl.slice <- chl.array[,,1:nday]
sil.slice <- sil.array[,,1:nday]
npp.slice <- npp.array[,,1:nday]

length(na.omit(as.vector(nit.slice[,,1])))
length(na.omit(as.vector(nit.slice[,,1:nday])))
lentest <- 46851*nday
lentest

# Create vectors of variable slices
nit_vec <- as.vector(nit.slice)
oxy_vec <- as.vector(oxy.slice)
pho_vec <- as.vector(pho.slice)
chl_vec <- as.vector(chl.slice)
sil_vec <- as.vector(sil.slice)
npp_vec <- as.vector(npp.slice)

length(nit_vec)

# Get lat/lon to be same length as variables
lonlat <- as.matrix(expand.grid(lon,lat))
dim(lonlat)

lennitvec <- length(nit_vec)
lenlonlat <- length(lonlat)
nmult <- (lennitvec/lenlonlat)*2

lonlat <- matrix(rep(t(lonlat),nmult),ncol=ncol(lonlat),byrow=TRUE)

# Turn vectors into dataframes, verify dimensions
nit_df01 <- data.frame(cbind(lonlat,nit_vec))
oxy_df01 <- data.frame(oxy_vec)
pho_df01 <- data.frame(pho_vec)
chl_df01 <- data.frame(chl_vec)
sil_df01 <- data.frame(sil_vec)
npp_df01 <- data.frame(npp_vec)

dim(nit_df01)
dim(oxy_df01)
dim(pho_df01)
dim(chl_df01)
dim(sil_df01)
dim(npp_df01)
dim(lonlat)

# Remove old variable forms (keep dataframes)
rm(nit_vec,oxy_vec,pho_vec,chl_vec,sil_vec,npp_vec,lonlat)
rm(nit.slice,oxy.slice,pho.slice,chl.slice,sil.slice,npp.slice)
rm(nit.array,oxy.array,pho.array,chl.array,sil.array,npp.array)

# Compile vectors into data frame
comp_df01 <- cbind(nit_df01,oxy_df01)
rm(nit_df01,oxy_df01)
comp_df01 <- cbind(comp_df01,pho_df01)
rm(pho_df01)
comp_df01 <- cbind(comp_df01,chl_df01)
rm(chl_df01)
comp_df01 <- cbind(comp_df01,sil_df01)
rm(sil_df01)
comp_df01 <- cbind(comp_df01,npp_df01)
rm(npp_df01)

# Save dataframe as csv file
names(comp_df01) <- c("lon","lat","no3","o2","po4","chl","si","nppv")
comp_df01 <- na.omit(comp_df01)
csvfile <- "/auto/home/kareande/mhwData/pacificData/lchl_1.csv"
write.table(comp_df01,csvfile, row.names=FALSE, sep=",")

# Will need to combine all dataframed into one master df

doParallel::registerDoParallel()
for(i in 1993:2019) { #loop to compile each yearly array file
    df <- paste("array_time",i,".csv", sep="") #create array file name to be read through each i, 1993:2019
    comp_df <- read.csv(df, sep="", header=FALSE) #read each array file from 1993:2019
    rm(df)  #remove var for cpu space
    master_with_time_df <- rbind(master_with_time_df, comp_df) #add on each array file by rows
    rm(comp_df) #remove var for cpu space
}
doParallel::stopImplicitCluster()