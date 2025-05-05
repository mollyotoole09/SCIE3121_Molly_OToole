#2/05 - 10 populations of salmo trutta at different latitudes ---- 
##upload studies with salmo trutta records >5 years 
library(tidyverse)

salmotruttaStudies <- read_csv("data/SalmoTruttaData.csv")
salmotruttaStudies <- salmotruttaStudies |> 
    dplyr::select(2:3) |> 
    separate(tsGroup, c("TimeSeriesID", "group"), sep = "-")

##upload summary table for rivfish studies 

RivFishStudies <- read_csv("data/RivFishTIME_STUDIES.csv") 
RivFishStudies <- RivFishStudies |> 
    dplyr::select(TimeSeriesID, Latitude, Longitude, Country, Region, Province)

##join 2 tables to attach latitudes to salmo trutta studies and create 15 bins for latitude groups
salmoStudies_latitude <- salmotruttaStudies |> 
    left_join(RivFishStudies, by = "TimeSeriesID") 

latitude_bins <- salmoStudies_latitude |>  
    filter(Latitude > 40) |> 
    mutate(lat_bin = cut(Latitude, breaks = 10)) 

long_studies <- latitude_bins |> 
    group_by(lat_bin) |> 
    slice_max(order_by = year, n = 1, with_ties = FALSE) |> 
    ungroup() |> 
    dplyr::select(!group)

longest_ids <- c("G8647", "G8637", "G7358", "G7343", "G2961", "G10556", "G11473", "G404", "G9542", "G10191")

##now, create a table with the abundance data for salmo trutta in each of the given studies
#include latitude so that it can be used as a fixed effect in the model 
fishData <- read_csv("data/RivFishTIME_DATA.csv")

salmotruttaData <- fishData |> 
    left_join(RivFishStudies, by = c("TimeSeriesID")) |>
    dplyr::select(TimeSeriesID, Year, Quarter, Species, Abundance, UnitAbundance, Latitude, Longitude, Country) |> 
    filter(Species == "Salmo trutta", TimeSeriesID %in% longest_ids) |> 
    group_by(TimeSeriesID, Year)
    

#5/05 - Europe climate analysis ----
install.packages("raster")
library(raster)
library(ggplot2)
library(terra)

#Fupload the 12 climate data files from ERA5 and select coordinates for the 10 sampling sites
list_coords <- list(c(6.00, 43.2),
                    c(6.28, 44.3),
                    c(-0.744, 48.7),
                    c(-1.40, 49.6),
                    c(-2.44, 54.0),
                    c(12.7, 57.2),
                    c(11.4, 58.9), 
                    c(25.1, 61.7),
                    c(20.1, 63.9), 
                    c(20.6, 67.7))

#file 1
data1 <- rast("data/new_climate_data/europeData1.grib")
print(data1)

n_layers1 <- nlyr(data1)

temp_layers1 <- seq(1, n_layers1, by = 2)
precip_layers1 <- seq(2, n_layers1, by = 2)

temp_data1 <- data1[[temp_layers1]]
precip_data1 <- data1[[precip_layers1]]

results1 <- vector("list", length = length(list_coords))

start_time1 <- as.POSIXct("1950-01-01 00:00:00", tz = "UTC")
n_timepoints1 <- nlyr(temp_data1)
datetimes1 <- seq(start_time1, by = "hour", length.out = n_timepoints1)

for (i in seq_along(list_coords)) {

  # Convert the i-th coordinate into a single-point SpatVector (as matrix)
  coord_mat <- matrix(list_coords[[i]], nrow = 1)
  coords_sf <- vect(coord_mat, type = "points", crs = crs(data1))
  
  # Extract temperature and precipitation
  temp_vals1 <- extract(temp_data1, coords_sf)[, -1]  # remove ID column
  precip_vals1 <- extract(precip_data1, coords_sf)[, -1]
  
  # Create data frame for this location
  df <- data.frame(
    Longitude = list_coords[[i]][1],
    Latitude = list_coords[[i]][2],
    DateTime = datetimes1,
    Temperature = as.numeric(temp_vals1),
    Precipitation = as.numeric(precip_vals1)
  )
  
  # Save to list
  results1[[i]] <- df
}

#add site id to each data table 
for (i in seq_along(results1)) {
  results1[[i]]$SiteID <- i
}

write.csv(results1, file = "data/new_climate_data/europeData1_coords.csv", row.names = FALSE)

#file 2
data2 <- rast("data/new_climate_data/europeData2.grib")
print(data2)

n_layers2 <- nlyr(data2)

temp_layers2 <- seq(1, n_layers2, by = 2)
precip_layers2 <- seq(2, n_layers2, by = 2)

temp_data2 <- data2[[temp_layers2]]
precip_data2 <- data2[[precip_layers2]]

results2 <- vector("list", length = length(list_coords))

start_time2 <- as.POSIXct("1956-01-01 00:00:00", tz = "UTC")
n_timepoints2 <- nlyr(temp_data2)
datetimes2 <- seq(start_time2, by = "hour", length.out = n_timepoints2)

for (i in seq_along(list_coords)) {

  # Convert the i-th coordinate into a single-point SpatVector (as matrix)
  coord_mat <- matrix(list_coords[[i]], nrow = 1)
  coords_sf <- vect(coord_mat, type = "points", crs = crs(data2))
  
  # Extract temperature and precipitation
  temp_vals2 <- extract(temp_data2, coords_sf)[, -1]  # remove ID column
  precip_vals2 <- extract(precip_data2, coords_sf)[, -1]
  
  # Create data frame for this location
  df <- data.frame(
    Longitude = list_coords[[i]][1],
    Latitude = list_coords[[i]][2],
    DateTime = datetimes2,
    Temperature = as.numeric(temp_vals2),
    Precipitation = as.numeric(precip_vals2)
  )
  
  # Save to list
  results2[[i]] <- df
}

#add site id to each data table 
for (i in seq_along(results2)) {
  results2[[i]]$SiteID <- i
}

write.csv(results2, file = "data/new_climate_data/europeData2_coords.csv", row.names = FALSE)

#file 3
data3 <- rast("data/new_climate_data/europeData3.grib")
print(data3)

n_layers3 <- nlyr(data3)

temp_layers3 <- seq(1, n_layers3, by = 2)
precip_layers3 <- seq(2, n_layers3, by = 2)

temp_data3 <- data3[[temp_layers3]]
precip_data3 <- data3[[precip_layers3]]

results3 <- vector("list", length = length(list_coords))

start_time3 <- as.POSIXct("1962-01-01 00:00:00", tz = "UTC")
n_timepoints3 <- nlyr(temp_data3)
datetimes3 <- seq(start_time3, by = "hour", length.out = n_timepoints3)

for (i in seq_along(list_coords)) {

  # Convert the i-th coordinate into a single-point SpatVector (as matrix)
  coord_mat <- matrix(list_coords[[i]], nrow = 1)
  coords_sf <- vect(coord_mat, type = "points", crs = crs(data3))
  
  # Extract temperature and precipitation
  temp_vals3 <- extract(temp_data3, coords_sf)[, -1]  # remove ID column
  precip_vals3 <- extract(precip_data3, coords_sf)[, -1]
  
  # Create data frame for this location
  df <- data.frame(
    Longitude = list_coords[[i]][1],
    Latitude = list_coords[[i]][2],
    DateTime = datetimes3,
    Temperature = as.numeric(temp_vals3),
    Precipitation = as.numeric(precip_vals3)
  )
  
  # Save to list
  results3[[i]] <- df
}

#add site id to each data table 
for (i in seq_along(results3)) {
  results3[[i]]$SiteID <- i
}

write.csv(results3, file = "data/new_climate_data/europeData3_coords.csv", row.names = FALSE)

#file 4
data4 <- rast("data/new_climate_data/europeData4.grib")
print(data4)

n_layers4 <- nlyr(data4)

temp_layers4 <- seq(1, n_layers4, by = 2)
precip_layers4 <- seq(2, n_layers4, by = 2)

temp_data4 <- data4[[temp_layers4]]
precip_data4 <- data4[[precip_layers4]]

results4 <- vector("list", length = length(list_coords))

start_time4 <- as.POSIXct("1968-01-01 00:00:00", tz = "UTC")
n_timepoints4 <- nlyr(temp_data4)
datetimes4 <- seq(start_time4, by = "hour", length.out = n_timepoints4)

for (i in seq_along(list_coords)) {

  # Convert the i-th coordinate into a single-point SpatVector (as matrix)
  coord_mat <- matrix(list_coords[[i]], nrow = 1)
  coords_sf <- vect(coord_mat, type = "points", crs = crs(data4))
  
  # Extract temperature and precipitation
  temp_vals4 <- extract(temp_data4, coords_sf)[, -1]  # remove ID column
  precip_vals4 <- extract(precip_data4, coords_sf)[, -1]
  
  # Create data frame for this location
  df <- data.frame(
    Longitude = list_coords[[i]][1],
    Latitude = list_coords[[i]][2],
    DateTime = datetimes4,
    Temperature = as.numeric(temp_vals4),
    Precipitation = as.numeric(precip_vals4)
  )
  
  # Save to list
  results4[[i]] <- df
}

#add site id to each data table 
for (i in seq_along(results4)) {
  results4[[i]]$SiteID <- i
}

write.csv(results4, file = "data/new_climate_data/europeData4_coords.csv", row.names = FALSE)

#file 5
data5 <- rast("data/new_climate_data/europeData5.grib")
print(data5)

n_layers5 <- nlyr(data5)

temp_layers5 <- seq(1, n_layers5, by = 2)
precip_layers5 <- seq(2, n_layers5, by = 2)

temp_data5 <- data5[[temp_layers5]]
precip_data5 <- data5[[precip_layers5]]

results5 <- vector("list", length = length(list_coords))

start_time5 <- as.POSIXct("1974-01-01 00:00:00", tz = "UTC")
n_timepoints5 <- nlyr(temp_data5)
datetimes5 <- seq(start_time5, by = "hour", length.out = n_timepoints5)

for (i in seq_along(list_coords)) {

  # Convert the i-th coordinate into a single-point SpatVector (as matrix)
  coord_mat <- matrix(list_coords[[i]], nrow = 1)
  coords_sf <- vect(coord_mat, type = "points", crs = crs(data5))
  
  # Extract temperature and precipitation
  temp_vals5 <- extract(temp_data5, coords_sf)[, -1]  # remove ID column
  precip_vals5 <- extract(precip_data5, coords_sf)[, -1]
  
  # Create data frame for this location
  df <- data.frame(
    Longitude = list_coords[[i]][1],
    Latitude = list_coords[[i]][2],
    DateTime = datetimes5,
    Temperature = as.numeric(temp_vals5),
    Precipitation = as.numeric(precip_vals5)
  )
  
  # Save to list
  results5[[i]] <- df
}

#add site id to each data table 
for (i in seq_along(results5)) {
  results5[[i]]$SiteID <- i
}

write.csv(results5, file = "data/new_climate_data/europeData5_coords.csv", row.names = FALSE)

#file 6
data6 <- rast("data/new_climate_data/europeData6.grib")
print(data6)

n_layers6 <- nlyr(data6)

temp_layers6 <- seq(1, n_layers6, by = 2)
precip_layers6 <- seq(2, n_layers6, by = 2)

temp_data6 <- data6[[temp_layers6]]
precip_data6 <- data6[[precip_layers6]]

results6 <- vector("list", length = length(list_coords))

start_time6 <- as.POSIXct("1980-01-01 00:00:00", tz = "UTC")
n_timepoints6 <- nlyr(temp_data6)
datetimes6 <- seq(start_time6, by = "hour", length.out = n_timepoints6)

for (i in seq_along(list_coords)) {

  # Convert the i-th coordinate into a single-point SpatVector (as matrix)
  coord_mat <- matrix(list_coords[[i]], nrow = 1)
  coords_sf <- vect(coord_mat, type = "points", crs = crs(data6))
  
  # Extract temperature and precipitation
  temp_vals6 <- extract(temp_data6, coords_sf)[, -1]  # remove ID column
  precip_vals6 <- extract(precip_data6, coords_sf)[, -1]
  
  # Create data frame for this location
  df <- data.frame(
    Longitude = list_coords[[i]][1],
    Latitude = list_coords[[i]][2],
    DateTime = datetimes6,
    Temperature = as.numeric(temp_vals6),
    Precipitation = as.numeric(precip_vals6)
  )
  
  # Save to list
  results6[[i]] <- df
}

#add site id to each data table 
for (i in seq_along(results6)) {
  results6[[i]]$SiteID <- i
}

write.csv(results6, file = "data/new_climate_data/europeData6_coords.csv", row.names = FALSE)

#file 7
data7 <- rast("data/new_climate_data/europeData7.grib")
print(data7)

n_layers7 <- nlyr(data7)

temp_layers7 <- seq(1, n_layers7, by = 2)
precip_layers7 <- seq(2, n_layers7, by = 2)

temp_data7 <- data7[[temp_layers7]]
precip_data7 <- data7[[precip_layers7]]

results7 <- vector("list", length = length(list_coords))

start_time7 <- as.POSIXct("1986-01-01 00:00:00", tz = "UTC")
n_timepoints7 <- nlyr(temp_data7)
datetimes7 <- seq(start_time7, by = "hour", length.out = n_timepoints7)

for (i in seq_along(list_coords)) {

  # Convert the i-th coordinate into a single-point SpatVector (as matrix)
  coord_mat <- matrix(list_coords[[i]], nrow = 1)
  coords_sf <- vect(coord_mat, type = "points", crs = crs(data7))
  
  # Extract temperature and precipitation
  temp_vals7 <- extract(temp_data7, coords_sf)[, -1]  # remove ID column
  precip_vals7 <- extract(precip_data7, coords_sf)[, -1]
  
  # Create data frame for this location
  df <- data.frame(
    Longitude = list_coords[[i]][1],
    Latitude = list_coords[[i]][2],
    DateTime = datetimes7,
    Temperature = as.numeric(temp_vals7),
    Precipitation = as.numeric(precip_vals7)
  )
  
  # Save to list
  results7[[i]] <- df
}

#add site id to each data table 
for (i in seq_along(results7)) {
  results7[[i]]$SiteID <- i
}

write.csv(results7, file = "data/new_climate_data/europeData7_coords.csv", row.names = FALSE)

#file 8
data8 <- rast("data/new_climate_data/europeData8.grib")
print(data8)

n_layers8 <- nlyr(data8)

temp_layers8 <- seq(1, n_layers8, by = 2)
precip_layers8 <- seq(2, n_layers8, by = 2)

temp_data8 <- data8[[temp_layers8]]
precip_data8 <- data8[[precip_layers8]]

results8 <- vector("list", length = length(list_coords))

start_time8 <- as.POSIXct("1992-01-01 00:00:00", tz = "UTC")
n_timepoints8 <- nlyr(temp_data8)
datetimes8 <- seq(start_time8, by = "hour", length.out = n_timepoints8)

for (i in seq_along(list_coords)) {

  # Convert the i-th coordinate into a single-point SpatVector (as matrix)
  coord_mat <- matrix(list_coords[[i]], nrow = 1)
  coords_sf <- vect(coord_mat, type = "points", crs = crs(data8))
  
  # Extract temperature and precipitation
  temp_vals8 <- extract(temp_data8, coords_sf)[, -1]  # remove ID column
  precip_vals8 <- extract(precip_data8, coords_sf)[, -1]
  
  # Create data frame for this location
  df <- data.frame(
    Longitude = list_coords[[i]][1],
    Latitude = list_coords[[i]][2],
    DateTime = datetimes8,
    Temperature = as.numeric(temp_vals8),
    Precipitation = as.numeric(precip_vals8)
  )
  
  # Save to list
  results8[[i]] <- df
}

#add site id to each data table 
for (i in seq_along(results8)) {
  results8[[i]]$SiteID <- i
}

write.csv(results8, file = "data/new_climate_data/europeData8_coords.csv", row.names = FALSE)

#file 9
data9 <- rast("data/new_climate_data/europeData9.grib")
print(data9)

n_layers9 <- nlyr(data9)

temp_layers9 <- seq(1, n_layers9, by = 2)
precip_layers9 <- seq(2, n_layers9, by = 2)

temp_data9 <- data9[[temp_layers9]]
precip_data9 <- data9[[precip_layers9]]

results9 <- vector("list", length = length(list_coords))

start_time9 <- as.POSIXct("1998-01-01 00:00:00", tz = "UTC")
n_timepoints9 <- nlyr(temp_data9)
datetimes9 <- seq(start_time9, by = "hour", length.out = n_timepoints9)

for (i in seq_along(list_coords)) {

  # Convert the i-th coordinate into a single-point SpatVector (as matrix)
  coord_mat <- matrix(list_coords[[i]], nrow = 1)
  coords_sf <- vect(coord_mat, type = "points", crs = crs(data9))
  
  # Extract temperature and precipitation
  temp_vals9 <- extract(temp_data9, coords_sf)[, -1]  # remove ID column
  precip_vals9 <- extract(precip_data9, coords_sf)[, -1]
  
  # Create data frame for this location
  df <- data.frame(
    Longitude = list_coords[[i]][1],
    Latitude = list_coords[[i]][2],
    DateTime = datetimes9,
    Temperature = as.numeric(temp_vals9),
    Precipitation = as.numeric(precip_vals9)
  )
  
  # Save to list
  results9[[i]] <- df
}

#add site id to each data table 
for (i in seq_along(results9)) {
  results9[[i]]$SiteID <- i
}

write.csv(results9, file = "data/new_climate_data/europeData9_coords.csv", row.names = FALSE)

#file 10
data10 <- rast("data/new_climate_data/europeData10.grib")
print(data10)

n_layers10 <- nlyr(data10)

temp_layers10 <- seq(1, n_layers10, by = 2)
precip_layers10 <- seq(2, n_layers10, by = 2)

temp_data10 <- data10[[temp_layers10]]
precip_data10 <- data10[[precip_layers10]]

results10 <- vector("list", length = length(list_coords))

start_time10 <- as.POSIXct("2004-01-01 00:00:00", tz = "UTC")
n_timepoints10 <- nlyr(temp_data10)
datetimes10 <- seq(start_time10, by = "hour", length.out = n_timepoints10)

for (i in seq_along(list_coords)) {

  # Convert the i-th coordinate into a single-point SpatVector (as matrix)
  coord_mat <- matrix(list_coords[[i]], nrow = 1)
  coords_sf <- vect(coord_mat, type = "points", crs = crs(data10))
  
  # Extract temperature and precipitation
  temp_vals10 <- extract(temp_data10, coords_sf)[, -1]  # remove ID column
  precip_vals10 <- extract(precip_data10, coords_sf)[, -1]
  
  # Create data frame for this location
  df <- data.frame(
    Longitude = list_coords[[i]][1],
    Latitude = list_coords[[i]][2],
    DateTime = datetimes10,
    Temperature = as.numeric(temp_vals10),
    Precipitation = as.numeric(precip_vals10)
  )
  
  # Save to list
  results10[[i]] <- df
}

#add site id to each data table 
for (i in seq_along(results10)) {
  results10[[i]]$SiteID <- i
}

write.csv(results10, file = "data/new_climate_data/europeData10_coords.csv", row.names = FALSE)

#file 11
data11 <- rast("data/new_climate_data/europeData11.grib")
print(data11)

n_layers11 <- nlyr(data11)

temp_layers11 <- seq(1, n_layers11, by = 2)
precip_layers11 <- seq(2, n_layers11, by = 2)

temp_data11 <- data11[[temp_layers11]]
precip_data11 <- data11[[precip_layers11]]

results11 <- vector("list", length = length(list_coords))

start_time11 <- as.POSIXct("2010-01-01 00:00:00", tz = "UTC")
n_timepoints11 <- nlyr(temp_data11)
datetimes11 <- seq(start_time11, by = "hour", length.out = n_timepoints11)

for (i in seq_along(list_coords)) {

  # Convert the i-th coordinate into a single-point SpatVector (as matrix)
  coord_mat <- matrix(list_coords[[i]], nrow = 1)
  coords_sf <- vect(coord_mat, type = "points", crs = crs(data11))
  
  # Extract temperature and precipitation
  temp_vals11 <- extract(temp_data11, coords_sf)[, -1]  # remove ID column
  precip_vals11 <- extract(precip_data11, coords_sf)[, -1]
  
  # Create data frame for this location
  df <- data.frame(
    Longitude = list_coords[[i]][1],
    Latitude = list_coords[[i]][2],
    DateTime = datetimes11,
    Temperature = as.numeric(temp_vals11),
    Precipitation = as.numeric(precip_vals11)
  )
  
  # Save to list
  results11[[i]] <- df
}

#add site id to each data table 
for (i in seq_along(results11)) {
  results11[[i]]$SiteID <- i
}

write.csv(results11, file = "data/new_climate_data/europeData11_coords.csv", row.names = FALSE)

#file 12
data12 <- rast("data/new_climate_data/europeData12.grib")
print(data12)

n_layers12 <- nlyr(data12)

temp_layers12 <- seq(1, n_layers12, by = 2)
precip_layers12 <- seq(2, n_layers12, by = 2)

temp_data12 <- data12[[temp_layers12]]
precip_data12 <- data12[[precip_layers12]]

results12 <- vector("list", length = length(list_coords))

start_time12 <- as.POSIXct("2016-01-01 00:00:00", tz = "UTC")
n_timepoints12 <- nlyr(temp_data12)
datetimes12 <- seq(start_time12, by = "hour", length.out = n_timepoints12)

for (i in seq_along(list_coords)) {

  # Convert the i-th coordinate into a single-point SpatVector (as matrix)
  coord_mat <- matrix(list_coords[[i]], nrow = 1)
  coords_sf <- vect(coord_mat, type = "points", crs = crs(data12))
  
  # Extract temperature and precipitation
  temp_vals12 <- extract(temp_data12, coords_sf)[, -1]  # remove ID column
  precip_vals12 <- extract(precip_data12, coords_sf)[, -1]
  
  # Create data frame for this location
  df <- data.frame(
    Longitude = list_coords[[i]][1],
    Latitude = list_coords[[i]][2],
    DateTime = datetimes12,
    Temperature = as.numeric(temp_vals12),
    Precipitation = as.numeric(precip_vals12)
  )
  
  # Save to list
  results12[[i]] <- df
}

#add site id to each data table 
for (i in seq_along(results12)) {
  results12[[i]]$SiteID <- i
}

write.csv(results12, file = "data/new_climate_data/europeData12_coords.csv", row.names = FALSE)

#Now that I have all of the data imported, I need to calculate daily average temperatures and total daily precipitation 

combinedData <- bind_rows(results1, results2, results3, results4, results5, results6, results7, results8, results9, results10, results11, results12)

europeData <- combinedData |> 
  separate(DateTime, into = c("Date", "Time"), sep = " ") |> 
  group_by(SiteID, Date, Latitude, Longitude) |> 
  summarise(
    Temp = mean(Temperature, na.rm = TRUE),
    Precip = sum(Precipitation, na.rm = TRUE),
    .groups = "drop") |> 
  rename(t = Date)

#save the table as a file for future use 
write.csv(europeData, file = "data/new_climate_data/europeData.csv", row.names = FALSE)

europeData <- read_csv("data/new_climate_data/europeData.csv")
europeData <- europeData |> 
  mutate(Temp = Temp - 273.15) #convert to celsius

view(europeData)

#get rid of extra variables to save space
rm(ls = "data1", "data2", "data3", "data4", "data5", "data6", "data7", "data8", "data9", "data10", "data11", "data12")
rm(ls = "n_layers1", "n_layers2", "n_layers3", "n_layers4", "n_layers5", "n_layers6", "n_layers7", "n_layers8", "n_layers9", "n_layers10", "n_layers11", "n_layers12")
rm(ls = "temp_layers1", "temp_layers2", "temp_layers3", "temp_layers4", "temp_layers5", "temp_layers6", "temp_layers7", "temp_layers8", "temp_layers9", "temp_layers10", "temp_layers11", "temp_layers12")
rm(ls = "precip_layers1", "precip_layers2", "precip_layers3", "precip_layers4", "precip_layers5", "precip_layers6", "precip_layers7", "precip_layers8", "precip_layers9", "precip_layers10", "precip_layers11", "precip_layers12")
rm(ls = "temp_data1", "temp_data2", "temp_data3", "temp_data4", "temp_data5", "temp_data6", "temp_data7", "temp_data8", "temp_data9", "temp_data10", "temp_data11", "temp_data12")
rm(ls = "precip_data1", "precip_data2", "precip_data3", "precip_data4", "precip_data5", "precip_data6", "precip_data7", "precip_data8", "precip_data9", "precip_data10", "precip_data11", "precip_data12")
rm(ls = "start_time1", "start_time2", "start_time3", "start_time4", "start_time5", "start_time6", "start_time7", "start_time8", "start_time9", "start_time10", "start_time11", "start_time12")
rm(ls = "n_timepoints1", "n_timepoints2", "n_timepoints3", "n_timepoints4", "n_timepoints5", "n_timepoints6", "n_timepoints7", "n_timepoints8", "n_timepoints9", "n_timepoints10", "n_timepoints11", "n_timepoints12")
rm(ls = "datetimes1", "datetimes2", "datetimes3", "datetimes4", "datetimes5", "datetimes6", "datetimes7", "datetimes8", "datetimes9", "datetimes10", "datetimes11", "datetimes12")

#now that I have daily temperature and precipitation data I can start calculating extreme events 
install.packages("heatwaveR")
library(heatwaveR)
library(tidyverse)
library(data.table)
library(purrr)

#use a 95% threshold for temperature and 99% threshold for precipitation
#initially, I will use the number of days exceeding the given threshold to define the predictor variable 

#95% threshold for temperature
thrs95_temp <- data.table()

for (i in 1:10) {
  data <- filter(europeData, SiteID == i)
  output <- ts2clm(data = data, x = t, y = Temp, climatologyPeriod = c("1950-01-01", "1970-12-31"), pctile = 95) #20 year baseline period
  output <- select(output, SiteID, Latitude, Longitude, t, Temp, seas, thresh)
  output <- rename(output, temp_thresh = thresh)
  output <- rename(output, temp_seas = seas)

  thrs95_temp <- bind_rows(thrs95_temp, output)
}

#95% threshold for precipitation
thrs95_precip <- data.table()

for (i in 1:10) {
  data <- filter(europeData, SiteID == i)
  output <- ts2clm(data = data, x = t, y = Precip, climatologyPeriod = c("1950-01-01", "1970-12-31"), pctile = 95) #20 year baseline period
  output <- select(output, SiteID, Latitude, Longitude, t, Precip, seas, thresh) 
  output <- rename(output, precip_thresh = thresh)
  output <- rename(output, precip_seas = seas)

  thrs95_precip <- bind_rows(thrs95_precip, output)
}

#join tables together
thresholds <- thrs95_temp |> 
  left_join(thrs95_precip, by = c("SiteID", "Latitude", "Longitude", "t")) 

#now, count the number of days per year per site that exceed the temperature and precipitation thresholds 
thrs_exceeded <- thresholds |> 
  group_by(SiteID) |> 
  separate(t, c("Year", "Month", "Day"), sep = "-") |>
  group_by(SiteID, Year) |>
  summarise(temp_exceeded = sum(Temp > temp_thresh, na.rm = TRUE),
            precip_exceeded = sum(Precip > precip_thresh, na.rm = TRUE),
            avg_temp = mean(Temp, na.rm = TRUE),
            total_precip = sum(Precip, na.rm = TRUE),
            .groups = "drop") |> 
  mutate(SiteID = case_when(
    SiteID == 1 ~ "G8647",
    SiteID == 2 ~ "G8637",
    SiteID == 3 ~ "G7358",
    SiteID == 4 ~ "G7343",
    SiteID == 5 ~ "G2961",
    SiteID == 6 ~ "G10556",
    SiteID == 7 ~ "G11473",
    SiteID == 8 ~ "G404",
    SiteID == 9 ~ "G9542",
    SiteID == 10 ~ "G10191"
  )) |> 
  rename(TimeSeriesID = SiteID)

thrs_exceeded$Year <- as.numeric(thrs_exceeded$Year)

#calculate growth rates for 10 different brown trout populations 
salmotruttaData <- salmotruttaData %>%
  group_by(TimeSeriesID, Year) %>%
  slice_max(order_by = Abundance, n = 1, with_ties = FALSE) %>%
  ungroup()

#tidy up the data by removing non-consecutive records 
salmotruttaData <- salmotruttaData |> 
  filter(TimeSeriesID == "G8647" & Year >= 1992 | 
         TimeSeriesID == "G8637" & Year >= 1992 |
         TimeSeriesID == "G7358" & Year <= 2014 |
         TimeSeriesID == "G2961" & Year >= 2001 |
         TimeSeriesID == "G10556" & Year >= 1988 |
         TimeSeriesID == "G11473" & Year >= 1980 |
         TimeSeriesID == "G404" & Year >= 1984 & Year <= 2012 | 
         TimeSeriesID == "G7343" |
         TimeSeriesID == "G9542" |
         TimeSeriesID == "G10191")

growthRates <- salmotruttaData |> 
  group_by(TimeSeriesID) |>
  arrange(Year) |>
  mutate(log_value = log10(Abundance + 1), 
          growth_rate = log_value - lag(log_value)) |> 
  select(TimeSeriesID, Year, Latitude, Longitude, Country, log_value, growth_rate) 

#join the two tables together and create pdat to use for regression model
pdat <- data.table()

panelData <- growthRates |> 
  left_join(thrs_exceeded, by = c("TimeSeriesID", "Year")) 

for (i in seq_along(longest_ids)) {
  data <- filter(panelData, TimeSeriesID == longest_ids[i])
  output <- data |> 
    mutate(logval_prev = lag(log_value),
           temp_prev = lag(avg_temp),
           precip_prev = lag(total_precip),
           extTemp_prev = lag(temp_exceeded), 
           extPrecip_prev = lag(precip_exceeded), 
           extTemp_abslat = lag(temp_exceeded)*Latitude) |> 
    select(TimeSeriesID, Year, Latitude, Longitude, Country, growth_rate, logval_prev, temp_prev, precip_prev, extTemp_prev, extPrecip_prev, extTemp_abslat) |> 
    filter(!is.na(growth_rate))

pdat <- bind_rows(pdat, output)

}

#regression models
library(fixest)
library(ggplot2)

panel1 <- feols(growth_rate ~ logval_prev + temp_prev + precip_prev | TimeSeriesID, data = pdat, panel.id=~TimeSeriesID+Year)
summary(panel1)

predicts <- all.vars(formula(panel1))[-1]

plots <- map(predicts, ~ {
    ggplot(pdat, aes_string(x = .x, y = "growth_rate")) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = TRUE, color = "blue") +
        labs(x = .x, y = "log(growth rate)", title = paste("Effect of", .x, "on growth rate")) +
        theme_minimal()
})

panel2 <- feols(growth_rate ~ logval_prev + temp_prev + precip_prev + extTemp_prev + extPrecip_prev | TimeSeriesID + Year, data = pdat, panel.id=~TimeSeriesID+Year)
summary(panel2)

predicts <- all.vars(formula(panel2))[-1]

plots <- map(predicts, ~ {
    ggplot(pdat, aes_string(x = .x, y = "growth_rate")) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = TRUE, color = "blue") +
        labs(x = .x, y = "log(growth rate)", title = paste("Effect of", .x, "on growth rate")) +
        theme_minimal()
})

panel3 <- feols(growth_rate ~ logval_prev + temp_prev + precip_prev + extTemp_prev + extPrecip_prev + extTemp_abslat | TimeSeriesID + Year + TimeSeriesID[Year], data = pdat, panel.id=~TimeSeriesID+Year)

predicts <- all.vars(formula(panel3))[-1]

plots <- map(predicts, ~ {
    ggplot(pdat, aes_string(x = .x, y = "growth_rate")) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = TRUE, color = "blue") +
        labs(x = .x, y = "log(growth rate)", title = paste("Effect of", .x, "on growth rate")) +
        theme_minimal()
})

##QUICKLY - check 99%


#99% threshold for temperature
thrs99_temp <- data.table()

for (i in 1:10) {
  data <- filter(europeData, SiteID == i)
  output <- ts2clm(data = data, x = t, y = Temp, climatologyPeriod = c("1950-01-01", "1970-12-31"), pctile = 99) #20 year baseline period
  output <- select(output, SiteID, Latitude, Longitude, t, Temp, seas, thresh)
  output <- rename(output, temp_thresh = thresh)
  output <- rename(output, temp_seas = seas)

  thrs99_temp <- bind_rows(thrs99_temp, output)
}

#95% threshold for precipitation
thrs99_precip <- data.table()

for (i in 1:10) {
  data <- filter(europeData, SiteID == i)
  output <- ts2clm(data = data, x = t, y = Precip, climatologyPeriod = c("1950-01-01", "1970-12-31"), pctile = 99) #20 year baseline period
  output <- select(output, SiteID, Latitude, Longitude, t, Precip, seas, thresh) 
  output <- rename(output, precip_thresh = thresh)
  output <- rename(output, precip_seas = seas)

  thrs99_precip <- bind_rows(thrs99_precip, output)
}

#join tables together
thresholds99 <- thrs99_temp |> 
  left_join(thrs99_precip, by = c("SiteID", "Latitude", "Longitude", "t")) 

#count number of days thresholds were exceeded
thrs99_exceeded <- thresholds99 |> 
  group_by(SiteID) |> 
  separate(t, c("Year", "Month", "Day"), sep = "-") |>
  group_by(SiteID, Year) |>
  summarise(temp_exceeded = sum(Temp > temp_thresh, na.rm = TRUE),
            precip_exceeded = sum(Precip > precip_thresh, na.rm = TRUE),
            avg_temp = mean(Temp, na.rm = TRUE),
            total_precip = sum(Precip, na.rm = TRUE),
            .groups = "drop") |> 
  mutate(SiteID = case_when(
    SiteID == 1 ~ "G8647",
    SiteID == 2 ~ "G8637",
    SiteID == 3 ~ "G7358",
    SiteID == 4 ~ "G7343",
    SiteID == 5 ~ "G2961",
    SiteID == 6 ~ "G10556",
    SiteID == 7 ~ "G11473",
    SiteID == 8 ~ "G404",
    SiteID == 9 ~ "G9542",
    SiteID == 10 ~ "G10191"
  )) |> 
  rename(TimeSeriesID = SiteID)

thrs99_exceeded$Year <- as.numeric(thrs99_exceeded$Year)

#join climate data with abundance data
pdat2 <- data.table()

panelData2 <- growthRates |> 
  left_join(thrs99_exceeded, by = c("TimeSeriesID", "Year")) 

for (i in seq_along(longest_ids)) {
  data <- filter(panelData2, TimeSeriesID == longest_ids[i])
  output <- data |> 
    mutate(logval_prev = lag(log_value),
           temp_prev = lag(avg_temp),
           precip_prev = lag(total_precip),
           extTemp_prev = lag(temp_exceeded), 
           extPrecip_prev = lag(precip_exceeded), 
           extTemp_abslat = lag(temp_exceeded)*Latitude) |> 
    select(TimeSeriesID, Year, Latitude, Longitude, Country, growth_rate, logval_prev, temp_prev, precip_prev, extTemp_prev, extPrecip_prev, extTemp_abslat) |> 
    filter(!is.na(growth_rate))

pdat2 <- bind_rows(pdat2, output)

}

#regression models 
panel1_99 <- feols(growth_rate ~ logval_prev + temp_prev + precip_prev | TimeSeriesID, data = pdat2, panel.id=~TimeSeriesID+Year)
summary(panel1_99)

predicts <- all.vars(formula(panel1_99))[-1]

plots <- map(predicts, ~ {
    ggplot(pdat2, aes_string(x = .x, y = "growth_rate")) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = TRUE, color = "blue") +
        labs(x = .x, y = "log(growth rate)", title = paste("Effect of", .x, "on growth rate")) +
        theme_minimal()
})

panel2_99 <- feols(growth_rate ~ logval_prev + temp_prev + precip_prev + extTemp_prev + extPrecip_prev | TimeSeriesID + Year, data = pdat2, panel.id=~TimeSeriesID+Year)
summary(panel2_99)

predicts <- all.vars(formula(panel2_99))[-1]

plots <- map(predicts, ~ {
    ggplot(pdat2, aes_string(x = .x, y = "growth_rate")) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = TRUE, color = "blue") +
        labs(x = .x, y = "log(growth rate)", title = paste("Effect of", .x, "on growth rate")) +
        theme_minimal()
})

panel3_99 <- feols(growth_rate ~ logval_prev + temp_prev + precip_prev + extTemp_prev + extPrecip_prev + extTemp_abslat | TimeSeriesID + Year + TimeSeriesID[Year], data = pdat2, panel.id=~TimeSeriesID+Year)
summary(panel3_99)

predicts <- all.vars(formula(panel3_99))[-1]

plots <- map(predicts, ~ {
    ggplot(pdat2, aes_string(x = .x, y = "growth_rate")) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = TRUE, color = "blue") +
        labs(x = .x, y = "log(growth rate)", title = paste("Effect of", .x, "on growth rate")) +
        theme_minimal()
})

write.csv(pdat, file = "data/new_climate_data/pdat.csv", row.names = FALSE)
write.csv(pdat2, file = "data/new_climate_data/pdat2.csv", row.names = FALSE)
capture.output(panel1, file = "results/panel1.txt")
capture.output(panel2, file = "results/panel2.txt")
capture.output(panel3, file = "results/panel3.txt")
capture.output(panel1_99, file = "results/panel1_99.txt")
capture.output(panel2_99, file = "results/panel2_99.txt")
capture.output(panel3_99, file = "results/panel3_99.txt")


