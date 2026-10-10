#NOTE: mak sure the Maxent in in the right folder dismo
#"C:\Users\HP\AppData\Local\R\win-library\4.4\dismo\maxent.jar"

rm(list = ls())
# Install and load important packages

library(dismo)

library(raster)

library(rJava)

library(jsonlite)

library(terra)

library(sf)


##### 2nd part ### 

# Handling data that has been downloaded manually

setwd("D:/Master2BiostatisticsLABEF/Species distribution model/Practical_Prof/Species distribution")

zebra <- read.table("zebra.txt", fill=T, sep="\t", header=T, stringsAsFactors=F, quote="")
#lion <- read.table("varanus.txt", fill=T, sep="\t", header=T, stringsAsFactors=F, quote="")
# the text file can change to your desired country
#the text file can be lion,occurances
dim(zebra )

colnames(zebra )

zebra1 <- subset(zebra, !is.na(decimalLongitude) & !is.na(decimalLatitude))



# Handling duplicates

dups <- duplicated(zebra1[,c("decimalLongitude","decimalLatitude")])

sum(dups)

zebra2 <-zebra1[!dups,]



# Extract longitude and latitude data

zebra3 <- zebra2[, c(99,98)]

head(zebra3,5)

names(zebra3) <- c("lon","lat")



# Visualise your data on a map

library(geodata)

wrld <- world(path=".")

#this are for kenya
plot(wrld, xlim=c(33,42), ylim=c(-5,5), axes=TRUE,col="light yellow")
#this cordinates is for benin you can change the cordinates as you want
#plot(wrld, xlim=c(0.8, 3.9), ylim=c(6.2, 12.5), axes=TRUE,col="light yellow")

points(zebra3$lon, zebra3$lat, col="orange",pch=20, cex=0.75)



# Save your data

write.csv(zebra3, file="zebra.csv", row.names=F)


# Import your data manually
getwd()

library(raster)

#options(java.parameters = "-Xmx4g")  # Set max memory for Java, adjust as needed

#initilize jvm manally
#library(rJava)
#.jinit()  # This explicitly starts the JVM

datassp <- read.csv("D:/Master2BiostatisticsLABEF/Species distribution model/Practical_Prof/Species distribution/zebra.csv")


datassp1 <- datassp

#get this files from tbios and mbios for 30' from AFRICLIM website newyoork
pet=raster("D:/Master2BiostatisticsLABEF/Species distribution model/Practical_Prof/Species distribution/pet_wc30s.tif")
bio12=raster("D:/Master2BiostatisticsLABEF/Species distribution model/Practical_Prof/Species distribution/bio12_wc30s.tif")
bio4=raster("D:/Master2BiostatisticsLABEF/Species distribution model/Practical_Prof/Species distribution/bio4_wc30s.tif")
mimq=raster("D:/Master2BiostatisticsLABEF/Species distribution model/Practical_Prof/Species distribution/mimq_wc30s.tif")
llds=raster("D:/Master2BiostatisticsLABEF/Species distribution model/Practical_Prof/Species distribution/llds_wc30s.tif")
bio1=raster("D:/Master2BiostatisticsLABEF/Species distribution model/Practical_Prof/Species distribution/bio1_wc30s.tif")
bio3=raster("D:/Master2BiostatisticsLABEF/Species distribution model/Practical_Prof/Species distribution/bio3_wc30s.tif")
bio5=raster("D:/Master2BiostatisticsLABEF/Species distribution model/Practical_Prof/Species distribution/bio5_wc30s.tif")
bio10=raster("D:/Master2BiostatisticsLABEF/Species distribution model/Practical_Prof/Species distribution/bio10_wc30s.tif")
bio13=raster("D:/Master2BiostatisticsLABEF/Species distribution model/Practical_Prof/Species distribution/bio13_wc30s.tif")
bio14=raster("D:/Master2BiostatisticsLABEF/Species distribution model/Practical_Prof/Species distribution/bio14_wc30s.tif")
bio15=raster("D:/Master2BiostatisticsLABEF/Species distribution model/Practical_Prof/Species distribution/bio15_wc30s.tif")
bio16=raster("D:/Master2BiostatisticsLABEF/Species distribution model/Practical_Prof/Species distribution/bio16_wc30s.tif")
bio17=raster("D:/Master2BiostatisticsLABEF/Species distribution model/Practical_Prof/Species distribution/bio17_wc30s.tif")
dm=raster("D:/Master2BiostatisticsLABEF/Species distribution model/Practical_Prof/Species distribution/dm_wc30s.tif")
mi=raster("D:/Master2BiostatisticsLABEF/Species distribution model/Practical_Prof/Species distribution/mi_wc30s.tif")
miaq=raster("D:/Master2BiostatisticsLABEF/Species distribution model/Practical_Prof/Species distribution/miaq_wc30s.tif")
bio2=raster("D:/Master2BiostatisticsLABEF/Species distribution model/Practical_Prof/Species distribution/bio2_wc30s.tif")
bio6=raster("D:/Master2BiostatisticsLABEF/Species distribution model/Practical_Prof/Species distribution/bio6_wc30s.tif")
bio7=raster("D:/Master2BiostatisticsLABEF/Species distribution model/Practical_Prof/Species distribution/bio7_wc30s.tif")
bio11=raster("D:/Master2BiostatisticsLABEF/Species distribution model/Practical_Prof/Species distribution/bio11_wc30s.tif")



#plot(llds)            
historical=stack(pet,bio12,bio4,mimq,llds,bio1,bio3,bio5,bio10,bio13,bio14,bio15,bio16,bio17,dm,mi,miaq,bio2,bio6,bio7,bio11)
windows()
plot(historical)



#### prof  code part #####

# Extract information describing the environmental conditions at the locations of our study species
env_occur <- extract(historical,cbind(datassp1$lon,datassp1$lat))

# Transform the obtained matrix into a dataframe
env_occur <- as.data.frame(env_occur)
env_occur <- cbind.data.frame(datassp1$lon,datassp1$lat,env_occur)

# Delete "na" from the database if any
env_occur1 <- as.data.frame(na.omit(env_occur))
summary(env_occur1)

# Rename the columns
names(env_occur1) <- c("lon","lat","pet","bio12","bio4","mimq","llds","bio1","bio3","bio5","bio10","bio13","bio14","bio15","bio16","bio17","dm","mi","miaq","bio2","bio6","bio7","bio11")

#pet -evapotranspiration
# Bio 12—Annual Precipitation
#Bio 4—Temperature Seasonality (Standard Deviation) 
#mimq - moisture index
#llds - duration of long dry season
#################
######

# Crop the African maps of bioclimate to Benin
library(sf)

# go to DIVA-GIS to download te shapefile for administrative copy and
#paste in the working directory the specofic 6 files with adm0 ending
Kenya_shp <- sf::st_read("KEN_adm0.shp")
windows()
plot(Kenya_shp)
plot(Kenya_shp$geometry)


historical1 <- crop(historical,extent(Kenya_shp))
plot(historical1)
historical1<- mask(historical1,Kenya_shp)
plot(historical1)
names(historical1) <- c("pet","bio12","bio4","mimq","llds","bio1","bio3","bio5","bio10","bio13","bio14","bio15","bio16","bio17","dm","mi","miaq","bio2","bio6","bio7","bio11")

# # Getting all data from the rasters of study area
myxy <- xyFromCell(historical1[[1]],cell=1:ncell(historical1[[1]]))
myvar <- getValues(historical1)
study_area <-cbind.data.frame(myxy, myvar)

# Delete na from the database
study_area <- as.data.frame(na.omit(study_area))
names(study_area) <- c("lon","lat","pet","bio12","bio4","mimq","llds","bio1","bio3","bio5","bio10","bio13","bio14","bio15","bio16","bio17","dm","mi","miaq","bio2","bio6","bio7","bio11")

# generate a matrix containing 1 to depict presence and 0 to depict background (the study area)
pa <-c(rep(1,nrow(env_occur1)),rep(0,nrow(study_area)))

# Note that we excluded the x and y information.
model_data <- rbind(env_occur1[,3:23],study_area[,3:23]) 

# Specify where you want to store the results
getwd()
setwd("D:/Master2BiostatisticsLABEF/Species distribution model/Practical_Prof/Species distribution")
wd <- getwd()

# Call packages that are necessary to run MaxEnt in R (rJava)


# Call packages that are necessary to run MaxEnt in R (rJava)

options(java.parameters = "-Xmx4g")  # Sets Java heap space to 4 GB
library(rJava)  # Load Java-dependent package AFTER setting the option
#file.edit("~/.Rprofile")

jar="C:/Users/HP/AppData/Local/R/win-library/4.4/dismo/maxent.jar"

# Model
#MSWD <- maxent(x=model_data, p=pa, path=paste(wd, "/thmaxSWD", sep=""), args=c("-P", "noautofeature", "nothreshold", "noproduct", "maximumbackground=136960", "noaddsamplestobackground"))

library(dismo)
# Model
MSWD <- maxent(x=model_data, p=pa, path=paste(wd, "/thmaxSWD", sep=""), args=c("-P", "noautofeature", "nothreshold", "noproduct", "maximumbackground=680657", "noaddsamplestobackground"))

# visualization
windows()
plot(MSWD)
response(MSWD)

MSWD.pred1 <- predict(MSWD, historical1, args="outputformat=raw", sep="", progress="text")                                  
windows()
plot(MSWD.pred1)




## Projecting to the future bioclimatic environment

petf=raster("D:/Master2BiostatisticsLABEF/Species distribution model/Practical_Prof/Species distribution/pet_rcp45_2055_ensemble-mean-v3_wc30s.tif")
bio12f=raster("D:/Master2BiostatisticsLABEF/Species distribution model/Practical_Prof/Species distribution/bio12_rcp45_2055_ensemble-mean-v3_wc30s.tif")
bio4f=raster("D:/Master2BiostatisticsLABEF/Species distribution model/Practical_Prof/Species distribution/bio4_rcp45_2055_ensemble-mean-v3_wc30s.tif")
mimqf=raster("D:/Master2BiostatisticsLABEF/Species distribution model/Practical_Prof/Species distribution/mimq_rcp45_2055_ensemble-mean-v3_wc30s.tif")
lldsf=raster("D:/Master2BiostatisticsLABEF/Species distribution model/Practical_Prof/Species distribution/llds_rcp45_2055_ensemble-mean-v3_wc30s.tif")
bio1f=raster("D:/Master2BiostatisticsLABEF/Species distribution model/Practical_Prof/Species distribution/bio1_rcp45_2055_ensemble-mean-v3_wc30s.tif")
bio3f=raster("D:/Master2BiostatisticsLABEF/Species distribution model/Practical_Prof/Species distribution/bio3_rcp45_2055_ensemble-mean-v3_wc30s.tif")
bio5f=raster("D:/Master2BiostatisticsLABEF/Species distribution model/Practical_Prof/Species distribution/bio5_rcp45_2055_ensemble-mean-v3_wc30s.tif")
bio10f=raster("D:/Master2BiostatisticsLABEF/Species distribution model/Practical_Prof/Species distribution/bio10_rcp45_2055_ensemble-mean-v3_wc30s.tif")
bio13f=raster("D:/Master2BiostatisticsLABEF/Species distribution model/Practical_Prof/Species distribution/bio13_rcp45_2055_ensemble-mean-v3_wc30s.tif")
bio14f=raster("D:/Master2BiostatisticsLABEF/Species distribution model/Practical_Prof/Species distribution/bio14_rcp45_2055_ensemble-mean-v3_wc30s.tif")
bio15f=raster("D:/Master2BiostatisticsLABEF/Species distribution model/Practical_Prof/Species distribution/bio15_rcp45_2055_ensemble-mean-v3_wc30s.tif")
bio16f=raster("D:/Master2BiostatisticsLABEF/Species distribution model/Practical_Prof/Species distribution/bio16_rcp45_2055_ensemble-mean-v3_wc30s.tif")
bio17f=raster("D:/Master2BiostatisticsLABEF/Species distribution model/Practical_Prof/Species distribution/bio17_rcp45_2055_ensemble-mean-v3_wc30s.tif")
dmf=raster("D:/Master2BiostatisticsLABEF/Species distribution model/Practical_Prof/Species distribution/dm_rcp45_2055_ensemble-mean-v3_wc30s.tif")
mif=raster("D:/Master2BiostatisticsLABEF/Species distribution model/Practical_Prof/Species distribution/mi_rcp45_2055_ensemble-mean-v3_wc30s.tif")
miaqf=raster("D:/Master2BiostatisticsLABEF/Species distribution model/Practical_Prof/Species distribution/miaq_rcp45_2055_ensemble-mean-v3_wc30s.tif")
bio2f=raster("D:/Master2BiostatisticsLABEF/Species distribution model/Practical_Prof/Species distribution/bio2_rcp45_2055_ensemble-mean-v3_wc30s.tif")
bio6f=raster("D:/Master2BiostatisticsLABEF/Species distribution model/Practical_Prof/Species distribution/bio6_rcp45_2055_ensemble-mean-v3_wc30s.tif")
bio7f=raster("D:/Master2BiostatisticsLABEF/Species distribution model/Practical_Prof/Species distribution/bio7_rcp45_2055_ensemble-mean-v3_wc30s.tif")
bio11f=raster("D:/Master2BiostatisticsLABEF/Species distribution model/Practical_Prof/Species distribution/bio11_rcp45_2055_ensemble-mean-v3_wc30s.tif")

#plot(lldsf)

future <- stack(petf,bio12f,bio4f,mimqf,lldsf,bio1f,bio3f,bio5f,bio10f,bio13f,bio14f,bio15f,bio16f,bio17f,dmf,mif,miaqf,bio2f,bio6f,bio7f,bio11f)
plot(future)

# Crop the African maps of the future bioclimate to Benin
library(sf)
Kenya_shp <- sf::st_read("KEN_adm0.shp")
plot(Kenya_shp)
plot(Kenya_shp$geometry)

future1 <- crop(future,extent(Kenya_shp))
plot(future1)
future1<- mask(future1,Kenya_shp)
plot(future1)
names(future1) <- c("pet","bio12","bio4","mimq","llds","bio1","bio3","bio5","bio10","bio13","bio14","bio15","bio16","bio17","dm","mi","miaq","bio2","bio6","bio7","bio11")

# predicting the future
MSWD.pred2 <- predict(MSWD, future1, args="outputformat=raw", sep="", progress="text")
plot(MSWD.pred2)

# visualise the modeled habitat suitability maps
MSWD.pred2 <- crop(MSWD.pred2,extent(MSWD.pred1))
MSWD.pred2 <- resample(MSWD.pred2,MSWD.pred1, method="bilinear")
MSWD.pred2 <- mask(MSWD.pred2,MSWD.pred1)
results <- stack(MSWD.pred1,MSWD.pred2)
windows()
plot(results)

names(results) <- c("historical Equus grevyi","future Equus grevyi")
windows()
plot(results)

# Save your results for further handling in ArcMap

writeRaster(MSWD.pred1,filename="zebra1f.tif")
writeRaster(MSWD.pred2,filename="zebra2f.tif")
writeRaster(results,filename="zebra3f.tif")
