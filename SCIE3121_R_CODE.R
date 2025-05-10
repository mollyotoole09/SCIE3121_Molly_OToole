##27/02 - First attempt at analysing BioTIME dataset ---- 
rainbow_bay <- read.csv("~/SCIE3121_Molly_OToole/data/raw_data_328.csv")
rainbow_bay

#Could have sorted by GENUS_SPECIES 
y_vals_ao <- rainbow_bay[1:30, 1] 
x_vals_ao <- rainbow_bay[1:30, 10]
plot(x_vals_ao, y_vals_ao, type = 'l', xlab = 'time', ylab = 'abundance')
title(main = 'Ambystoma opacum')

y_vals_at <- rainbow_bay[31:60, 1]
x_vals_at <- rainbow_bay[31:60, 10]
plot(x_vals_at, y_vals_at, type = 'l', xlab = 'time', ylab = 'abundance')
title(main = 'Ambystoma talpoideum')

y_vals_ati <- rainbow_bay[61:91, 1]
x_vals_ati <- rainbow_bay[61:91, 10]
plot(x_vals_ati, y_vals_ati, type = 'l', xlab = 'time', ylab = 'abundance')
title(main = 'Ambystoma tigrinum')

y_vals_as <- rainbow_bay[92:121, 1]
x_vals_as <- rainbow_bay[92:121, 10]
plot(x_vals_as, y_vals_as, type = 'l', xlab = 'time', ylab = 'abundance')
title(main = 'Anaxyrus sp')

y_vals_eq <- rainbow_bay[122:151, 1]
x_vals_eq <- rainbow_bay[122:151, 10]
plot(x_vals_eq, y_vals_eq, type = 'l', xlab = 'time', ylab = 'abundance')
title(main = 'Eurycea quadridigitatta')

y_vals_gc <- rainbow_bay[152:181, 1]
x_vals_gc <- rainbow_bay[152:181, 10]
plot(x_vals_gc, y_vals_gc, type = 'l', xlab = 'time', ylab = 'abundance')
title(main = 'Gastrophryne carolinensis')

y_vals_ls <- rainbow_bay[182:211, 1]
x_vals_ls <- rainbow_bay[182:211, 10]
plot(x_vals_ls, y_vals_ls, type = 'l', xlab = 'time', ylab = 'abundance')
title(main = 'Lithobates sp')

y_vals_pc <- rainbow_bay[212:241, 1]
x_vals_pc <- rainbow_bay[212:241, 10]
plot(x_vals_pc, y_vals_pc, type = 'l', xlab = 'time', ylab = 'abundance')
title(main = 'Pseudacris crucifer')

y_vals_po <- rainbow_bay[242:271, 1]
x_vals_po <- rainbow_bay[242:271, 10]
plot(x_vals_po, y_vals_po, type = 'l', xlab = 'time', ylab = 'abundance')
title(main = 'Pseudacris ornata')

y_vals_sh <- rainbow_bay[272:301, 1]
x_vals_sh <- rainbow_bay[272:301, 10]
plot(x_vals_sh, y_vals_sh, type = 'l', xlab = 'time', ylab = 'abundance')
title(main = 'Scaphiopus holbrookii')

rm(list = ls())


##4/03 - Second Trial at BioTIME ---- 
library(data.table)
library(tidyverse)
zooplanktonData <- read.csv("data/raw_data_253.csv") #load zooplankton data from BioTIME

speciesData <- data.table(zooplanktonData$GENUS_SPECIES)  #create a new data table with only genus information
summarySpeciesData <- table(zooplanktonData$GENUS_SPECIES) #create a summary table that shows how many records of each species are found in the dataset 

#Because there are 133 different species in this data set we want to only include the best ones
sortedSpeciesData <- speciesData[, .N, by = "V1"][order(-N)]
print(sortedSpeciesData)

#Copepod nauplii (1659), Polyarthra vulgaris (1469), Keratella cochlearis (1350), Polyarthra remata
#(1314), Kellicottia longispina (1218), Bosminidae (1208), Diacyclops thomasi (1197), 
#Gastropus stylifer (988), 	Conochilus sp (971) and Keratella earlinae (938) were the top 10 species
#with the most records. 

######COPEPOD NAUPLII#################################### ----
copepodNauplii <- filter(zooplanktonData, GENUS_SPECIES == "Copepod nauplii")
copepodNauplii <- select(copepodNauplii, !GENUS_SPECIES)
copepodNauplii

datatableCN <- data.table(copepodNauplii)

timeSeriesData <- datatableCN[, .(meanAbundance = mean(ABUNDANCE, na.rm = TRUE)), by = .(YEAR, LATITUDE)]
timeSeriesData

L1 <- table(timeSeriesData$LATITUDE)
L1 

Loc1 <- timeSeriesData[LATITUDE == 46.00275]
Loc2 <- timeSeriesData[LATITUDE == 46.007583]
Loc3 <- timeSeriesData[LATITUDE == 46.007733]                    
Loc4 <- timeSeriesData[LATITUDE == 46.021067]
Loc5 <- timeSeriesData[LATITUDE == 46.029267]
Loc6 <- timeSeriesData[LATITUDE == 46.038317]
Loc7 <- timeSeriesData[LATITUDE == 46.04125]

plot(Loc1$YEAR, Loc1$meanAbundance, type = "l", xlab = "time", ylab = "abundance")
title(main = "Location 1, (46.00275)")
plot(Loc2$YEAR, Loc2$meanAbundance, type = "l", xlab = "time", ylab = "abundance")
title(main = "Location 2, (46.007583)")
plot(Loc3$YEAR, Loc3$meanAbundance, type = "l", xlab = "time", ylab = "abundance")
title(main = "Location 3, (46.007733)")
plot(Loc4$YEAR, Loc4$meanAbundance, type = "l", xlab = "time", ylab = "abundance")
title(main = "Location 4, (46.021067)")
plot(Loc5$YEAR, Loc5$meanAbundance, type = "l", xlab = "time", ylab = "abundance")
title(main = "Location 5, (46.029267)")
plot(Loc6$YEAR, Loc6$meanAbundance, type = "l", xlab = "time", ylab = "abundance")
title(main = "Location 6, (46.038317)")
plot(Loc7$YEAR, Loc7$meanAbundance, type = "l", xlab = "time", ylab = "abundance")
title(main = "Location 7, (46.04125)")

rm(list = ls())


##18/03 - RivFishTIME dataset ---- 
fishData <- read_csv("data/RivFishTIME_DATA.csv")

speciesFishData <- table(fishData$Species)

##Popn 1 - Salmo trutta - 39 consecutive years
G11473A_SalmoDat <- filter(fishData, TimeSeriesID == "G11473" & Year > 1979 & Species == "Salmo trutta")

salmoDataTable <- G11473A_SalmoDat |> 
    select(!TimeSeriesID & !SurveyID & !Species) |>
    group_by(Year) |> 
    rename(year = Year) |>
    summarise(Abundance = max(Abundance)) #select the maximum abundance for the year instead of the mean - more accurate in this study

pdf("plots/Salmo_trutta.pdf", width = 6, height = 5)
plot(salmoDataTable$year, salmoDataTable$Abundance, type = "l", xlab = "time", ylab = "abundance")
title(main = "Salmo trutta - G11473")
dev.off()


##19/03 - Further RivFishTIME analysis -----
##Popn 2 - Salmo trutta - 36 consecutive years
G10988A_SalmoDat <- filter(fishData, TimeSeriesID == "G10988" & Year > 1982 & Species == "Salmo trutta")

G10988A_SalmoDat <- select(G10988A_SalmoDat, !TimeSeriesID & !SurveyID & !Species)

salmoDataTable2 <- G10988A_SalmoDat |> 
    group_by(Year) |> 
    summarise(Abunance = mean(Abundance))

pdf("plots/Salmo_trutta_2.pdf", width = 6, height = 5)
plot(salmoDataTable2$Year, salmoDataTable2$Abunance, type = "l", xlab = "time", ylab = "abundance")
title(main = "Salmo trutta - G10988")
dev.off()

##Popn 3 - Salmo salar - 38 consecutive years
G10538A_SalmoDat <- filter(fishData, TimeSeriesID == "G10538" & Year > 1969 & Year < 2008 & Species == "Salmo salar")

G10538A_SalmoDat <- select(G10538A_SalmoDat, !TimeSeriesID & !SurveyID & !Species)

salmoDataTable3 <- G10538A_SalmoDat |> 
    group_by(Year) |> 
    summarise(Abunance = mean(Abundance))

pdf("plots/Salmo_salar.pdf", width = 6, height = 5)
plot(salmoDataTable3$Year, salmoDataTable3$Abunance, type = "l", xlab = "time", ylab = "abundance")
title(main = "Salmo salar - G10538")
dev.off()

#Popn 4 - Salmo salar - 36 consecutive years 
G10120A_SalmoDat <- filter(fishData, TimeSeriesID == "G10120" & Year > 1982 & Species == "Salmo salar")

G10120A_SalmoDat <- select(G10120A_SalmoDat, !TimeSeriesID & !SurveyID & !Species)

salmoDataTable4 <- G10120A_SalmoDat |> 
    group_by(Year) |> 
    summarise(Abunance = mean(Abundance))

pdf("plots/Salmo_salar_2.pdf", width = 6, height = 5)
plot(salmoDataTable4$Year, salmoDataTable4$Abunance, type = "l", xlab = "time", ylab = "abundance")
title(main = "Salmo salar - G10120")
dev.off()


##2/04 - Climate data analysis ---- 

library(readxl)
library(tidyverse)
library(patchwork)

#ERA data - CCKP (Climate Change Knowledge Portal) - this data is only monthly averages so not going 
#to be used, just helpful with visualisation and practice.

#backgroundWeather <- read_excel("data/sweden_background_temp.xlsx", col_names = TRUE)

#tempDat <- backgroundWeather |>
#  pivot_longer(cols = everything(), names_to = "Date", values_to = "Temperature") |> 
#  separate(Date, c("Year", "Month"), sep = "-") |> 
#  group_by(Month) |> 
#  filter(!is.na(Year) & !is.na(Month)) |> 
#  summarise(mean_monthly_temp = mean(Temperature)) 

#ggplot(tempDat) + 
#    geom_point(aes(Month, mean_monthly_temp))

#CRU data for mean, max and min monthly temp - CCKP

#cruMonthlyData <- read_excel("data/monthly_cru_data.xlsx", col_names = TRUE)

#baselineCruDat <- cruMonthlyData |> 
#    pivot_longer(!name, names_to = "Date", values_to = "Temperature") |> 
#    separate(Date, c("Year", "Month"), sep = "-") |> 
#    group_by(Month, name) |> 
#    summarise(avg_temp = mean(Temperature))

#tempPlot <- ggplot(baselineCruDat) + 
#                geom_line(mapping = aes(Month, avg_temp, colour = name, group = name)) + 
#                theme_linedraw()

#ggsave(filename = "plots/temp_baseline_plot.pdf", plot = tempPlot, width = 5, height = 6)


##7/04 - ERA5 Analysis - Heatwave data ---- 
install.packages("raster")
library(raster)
library(ggplot2)

##### HEATWAVE DATA ########
#Read part 1 (1980 - 1992) of the era5 data and select specific coordinates
heatwavesDat1 <- brick("data/era5part1.grib")
print(heatwavesDat1)

coordinates <- matrix(c(11.44062, 58.90116), ncol = 2) #coordinates of the sampling site

heatwavesDat1 <- raster::extract(heatwavesDat1, coordinates)

#Now that I have reshaped the grib file to only include values at the specific coordinates 
#of the sampling site, I need to select the maximum temperature recorded for each day

#First, create a new matrix where rows are days and columns are hourly records
num_records1 <- length(heatwavesDat1)
complete_days1 <- floor(num_records1 / 24) #number of days in the dataset

hourlyData1 <- matrix(heatwavesDat1[1:(complete_days1 * 24)], ncol = 24, byrow = TRUE)
head(hourlyData1)

#Now, only select the maximum temp for each day 
dailyData1 <- apply(hourlyData1, 1, max)
head(dailyData1)
rm(ls = "hourlyData1", "num_records1", "complete_days1")

##Read part 2 (1993 - 2005) of the era5 data and select specific coordinates 
heatwavesDat2 <- brick("data/era5part2.grib")
print(heatwavesDat2)

heatwavesDat2 <- raster::extract(heatwavesDat2, coordinates)

num_records2 <- length(heatwavesDat2)
complete_days2 <- floor(num_records2 / 24)

hourlyData2 <- matrix(heatwavesDat2[1:(complete_days2 * 24)], ncol = 24, byrow = TRUE)
head(hourlyData2)

dailyData2 <- apply(hourlyData2, 1, max)
head(dailyData2)
rm(ls = "hourlyData2", "num_records2", "complete_days2")

##Read part 3 (2006 - 2011) of the era5 data and select specific coordinates
heatwavesDat3 <- brick("data/era5part3.grib")
print(heatwavesDat3)

heatwavesDat3 <- raster::extract(heatwavesDat3, coordinates)

num_records3 <- length(heatwavesDat3)
complete_days3 <- floor(num_records3 / 24)

hourlyData3 <- matrix(heatwavesDat3[1:(complete_days3 * 24)], ncol = 24, byrow = TRUE)
head(hourlyData3)

dailyData3 <- apply(hourlyData3, 1, max)
head(dailyData3)
rm(ls = "hourlyData3", "num_records3", "complete_days3")

##Read part 4 (2012 - 2018) of the era5 data and select specific coordinates
heatwavesDat4 <- brick("data/era5part4.grib")
print(heatwavesDat4)

heatwavesDat4 <- raster::extract(heatwavesDat4, coordinates)

num_records4 <- length(heatwavesDat4)
complete_days4 <- floor(num_records4 / 24)

hourlyData4 <- matrix(heatwavesDat4[1:(complete_days4 * 24)], ncol = 24, byrow = TRUE)
head(hourlyData4)

dailyData4 <- apply(hourlyData4, 1, max)
head(dailyData4)
rm(ls = "hourlyData4", "num_records4", "complete_days4")

#Now that we have all the daily temperature recording, we can combine it all together
combinedTemp <- c(dailyData1, dailyData2, dailyData3, dailyData4)

#create sequences of dates corresponding to the data in dailyData and dailyData2
start_year1 <- 1980
start_year2 <- 1993
start_year3 <- 2006 
start_year4 <- 2012

dates1 <- seq.Date(from = as.Date(paste(start_year1, "-01-01", sep="")), by = "day", length.out = length(dailyData1))
dates2 <- seq.Date(from = as.Date(paste(start_year2, "-01-01", sep="")), by = "day", length.out = length(dailyData2))
dates3 <- seq.Date(from = as.Date(paste(start_year3, "-01-01", sep="")), by = "day", length.out = length(dailyData3))
dates4 <- seq.Date(from = as.Date(paste(start_year4, "-01-01", sep="")), by = "day", length.out = length(dailyData4))

#combine the sequences of dates together
combinedDates <- c(dates1, dates2, dates3, dates4)
rm(ls = "dates1", "dates2", "dates3", "dates4")
rm(ls = "start_year1", "start_year2", "start_year3", "start_year4")

#create a data table for the maximum temperature recorded at the site for each date between 1980-2018
temperatureData <- data.frame(t = combinedDates, temp = combinedTemp) #columns here are in the correct form for heatwaveR

rm(ls = "dailyData1", "dailyData2", "dailyData3", "dailyData4")
rm(ls = "heatwavesDat1", "heatwavesDat2", "heatwavesDat3", "heatwavesDat4")

#for future use, just load temperature data file 
write.csv(temperatureData, "data/temperatureData.csv") #write the data to a csv file for future use

temperatureData <- read_csv("data/temperatureData.csv") 

#We have now selected the maximum temperature for each day, which we can run through the heatwavesR package 
#to find the number of heatwave events per year. Before we can do this however, we need to define clear, 
#baseline daily values to use as a threshold


##9/04 - ERA5 Analysis - Baseline heatwave data ---- 
#### BASELINE DATA ####
#I had to download the data in chunks from the ERA5 database because the request was too large 
#so before I can join it with the heatwaves data I need to combine it all

#First, import all the data
baseline1 <- brick("data/era5BASELINE1.grib")
print(baseline1)
baseline2 <- brick("data/era5BASELINE2a.grib")
print(baseline2)
baseline3 <- brick("data/era5BASELINE2b.grib")
print(baseline3)
baseline4 <- brick("data/era5BASELINE3.grib")
print(baseline4)

#Select the same coordinates 
baseline1 <- raster::extract(baseline1, coordinates)
baseline2 <- raster::extract(baseline2, coordinates)
baseline3 <- raster::extract(baseline3, coordinates)
baseline4 <- raster::extract(baseline4, coordinates)

#To make the data cohesive, I will select the maximum temperature recorded each day over the 30 year baseline 
#period and use this to set thresholds for maximum daily temperatures
num_baseline1_records <- length(baseline1)
complete_baseline1_days <- floor(num_baseline1_records / 24)
num_baseline2_records <- length(baseline2)
complete_baseline2_days <- floor(num_baseline2_records / 24)
num_baseline3_records <- length(baseline3)
complete_baseline3_days <- floor(num_baseline3_records / 24)
num_baseline4_records <- length(baseline4)
complete_baseline4_days <- floor(num_baseline4_records / 24)

hourlyBaselineData1 <- matrix(baseline1[1:(complete_baseline1_days * 24)], ncol = 24, byrow = TRUE)
head(hourlyBaselineData1)
hourlyBaselineData2 <- matrix(baseline2[1:(complete_baseline2_days * 24)], ncol = 24, byrow = TRUE)
head(hourlyBaselineData2)
hourlyBaselineData3 <- matrix(baseline3[1:(complete_baseline3_days * 24)], ncol = 24, byrow = TRUE)
head(hourlyBaselineData3)
hourlyBaselineData4 <- matrix(baseline4[1:(complete_baseline4_days * 24)], ncol = 24, byrow = TRUE)
head(hourlyBaselineData4)

#select the daily maximum temperature
dailyBaselineData1 <- apply(hourlyBaselineData1, 1, max)
head(dailyBaselineData1)
dailyBaselineData2 <- apply(hourlyBaselineData2, 1, max)
head(dailyBaselineData2)
dailyBaselineData3 <- apply(hourlyBaselineData3, 1, max)
head(dailyBaselineData3)
dailyBaselineData4 <- apply(hourlyBaselineData4, 1, max)
head(dailyBaselineData4)

#Join all of the baseline temperature data together 
combinedBaselineTemp <- c(dailyBaselineData1, dailyBaselineData2, dailyBaselineData3, dailyBaselineData4)

#Now, I need to create another sequence of dates corresponding to the baseline data
start_baselineYr1 <- 1950 
start_baselineYr2 <- 1963
start_baselineYr3 <- 1969
start_baselineYr4 <- 1976

baselineDates1 <- seq.Date(from = as.Date(paste(start_baselineYr1, "-01-01", sep="")), by = "day", length.out = length(dailyBaselineData1))
baselineDates2 <- seq.Date(from = as.Date(paste(start_baselineYr2, "-01-01", sep="")), by = "day", length.out = length(dailyBaselineData2))
baselineDates3 <- seq.Date(from = as.Date(paste(start_baselineYr3, "-01-01", sep="")), by = "day", length.out = length(dailyBaselineData3))
baselineDates4 <- seq.Date(from = as.Date(paste(start_baselineYr4, "-01-01", sep="")), by = "day", length.out = length(dailyBaselineData4))

#combine the dates into one sequence
combinedBaselineDates <- c(baselineDates1, baselineDates2, baselineDates3, baselineDates4)

#create a data table for the baseline data
baselineData <- data.frame(t = combinedBaselineDates, temp = combinedBaselineTemp)

rm(ls = "baseline1", "baseline2", "baseline3", "baseline4")
rm(ls = "num_baseline1_records", "num_baseline2_records", "num_baseline3_records", "num_baseline4_records")
rm(ls = "complete_baseline1_days", "complete_baseline2_days", "complete_baseline3_days", "complete_baseline4_days")
rm(ls = "hourlyBaselineData1", "hourlyBaselineData2", "hourlyBaselineData3", "hourlyBaselineData4")
rm(ls = "dailyBaselineData1", "dailyBaselineData2", "dailyBaselineData3", "dailyBaselineData4")
rm(ls = "start_baselineYr1", "start_baselineYr2", "start_baselineYr3", "start_baselineYr4")
rm(ls = "baselineDates1", "baselineDates2", "baselineDates3", "baselineDates4")

write.csv(baselineData, "data/baselineTemperatureData.csv") #write the data to a csv file for future use

baselineData <- read_csv("data/baselineTemperatureData.csv") 

##This will be used in heatwaveR - this data table is a collection of the maximum daily temperature recorded 
#at the sampling site over the 30 year baseline period (1950-1980) and over the entire sampling period (1980-2018)
heatwaveData <- rbind(baselineData, temperatureData) |> 
    mutate(temp = temp - 273.15) #convert from Kelvins to degrees Celsius

##### heatwaveR ######
install.packages("heatwaveR")
library(heatwaveR)
library(cowplot)
library(ggplot2)

#1a. 95% threshold, one day heatwaves 
thrs95dur1 <- ts2clm(data = heatwaveData, x = t, y = temp, climatologyPeriod = c("1950-01-01", "1979-12-31"), pctile = 95)
res1a <- filter(thrs95dur1, t >= "1980-01-01") #filter for study years
out1a <- detect_event(res1a, minDuration = 1) 
heatwaves1a <- block_average(out1a)

summary(glm(count ~ year, family = "poisson", data = heatwaves1a)) #want to see if there is evidence that the number of 1 day heatwaves
                                                                   #events is increasing during the study period

#plot of the number of heatwaves each year from 1980-2018 given the threshold and minimum duration 
thrs95dur1Num <- ggplot(data = heatwaves1a, aes(x = year, y = count)) + 
    geom_point(colour = "red") + 
    geom_line() + 
    labs(x = "year", y = "number of events")

#plot of temperature in 2018 as a demonstration for how the number of heatwaves per year might 
#change as duration and threshold definitions are changed 
thrs95dur1Yr <- event_line(out1a, start_date = "2018-01-01", end_date = "2018-12-31")

thrs95dur1Plot <- plot_grid(thrs95dur1Num, thrs95dur1Yr, nrow = 2)

ggsave(filename = "plots/thresholds_temp/1day_95.pdf", plot = thrs95dur1Plot)
rm(ls = "thrs95dur1Num", "thrs95dur1Yr")

#1b. 90% threshold, one day heatwaves 
thrs90dur1 <- ts2clm(data = heatwaveData, x = t, y = temp, climatologyPeriod = c("1950-01-01", "1979-12-31"), pctile = 90)
res1b <- filter(thrs90dur1, t >= "1980-01-01")
out1b <- detect_event(res1b, minDuration = 1)
heatwaves1b <- block_average(out1b)

summary(glm(count ~ year, family = "poisson", data = heatwaves1b))

thrs90dur1Num <- ggplot(data = heatwaves1b, aes(x = year, y = count)) + 
    geom_point(colour = "red") + 
    geom_line() + 
    labs(x = "year", y = "number of events")

thrs90dur1Yr <- event_line(out1b, start_date = "2018-01-01", end_date = "2018-12-31")

thrs90dur1Plot <- plot_grid(thrs90dur1Num, thrs90dur1Yr, nrow = 2)

ggsave(filename = "plots/thresholds_temp/1day_90.pdf", plot = thrs90dur1Plot)
rm(ls = "thrs90dur1Num", "thrs90dur1Yr")

#1c. 85% threshold, one day heatwaves 
thrs85dur1 <- ts2clm(data = heatwaveData, x = t, y = temp, climatologyPeriod = c("1950-01-01", "1979-12-31"), pctile = 85)
res1c <- filter(thrs85dur1, t >= "1980-01-01")
out1c <- detect_event(res1c, minDuration = 1)
heatwaves1c <- block_average(out1c)

summary(glm(count ~ year, family = "poisson", data = heatwaves1c))

thrs85dur1Num <- ggplot(data = heatwaves1c, aes(x = year, y = count)) + 
    geom_point(colour = "red") + 
    geom_line() + 
    labs(x = "year", y = "number of events")

thrs85dur1Yr <- event_line(out1c, start_date = "2018-01-01", end_date = "2018-12-31")

thrs85dur1Plot <- plot_grid(thrs85dur1Num, thrs85dur1Yr, nrow = 2)

ggsave(filename = "plots/thresholds_temp/1day_85.pdf", plot = thrs85dur1Plot)
rm(ls = "thrs85dur1Num", "thrs85dur1Yr")

#1d. 99% threshold, one day heatwaves 
thrs99dur1 <- ts2clm(data = heatwaveData, x = t, y = temp, climatologyPeriod = c("1950-01-01", "1979-12-31"), pctile = 99)
res1d <- filter(thrs99dur1, t >= "1980-01-01")
out1d <- detect_event(res1d, minDuration = 1)
heatwaves1d <- block_average(out1d)

summary(glm(count ~ year, family = "poisson", data = heatwaves1d))

thrs99dur1Num <- ggplot(data = heatwaves1d, aes(x = year, y = count)) + 
    geom_point(colour = "red") + 
    geom_line() + 
    labs(x = "year", y = "number of events")

thrs99dur1Yr <- event_line(out1d, start_date = "2018-01-01", end_date = "2018-12-31")

thrs99dur1Plot <- plot_grid(thrs99dur1Num, thrs99dur1Yr, nrow = 2)

ggsave(filename = "plots/thresholds_temp/1day_99.pdf", plot = thrs99dur1Plot)
rm(ls = "thrs99dur1Num", "thrs99dur1Yr")

#2a. 95% threshold, two day heatwaves 
thrs95dur2 <- ts2clm(data = heatwaveData, x = t, y = temp, climatologyPeriod = c("1950-01-01", "1979-12-31"), pctile = 95)
res2a <- filter(thrs95dur2, t >= "1980-01-01")
out2a <- detect_event(res2a, minDuration = 2)
heatwaves2a <- block_average(out2a)

summary(glm(count ~ year, family = "poisson", data = heatwaves2a))

thrs95dur2Num <- ggplot(data = heatwaves2a, aes(x = year, y = count)) + 
    geom_point(colour = "red") + 
    geom_line() + 
    labs(x = "year", y = "number of events")

thrs95dur2Yr <- event_line(out2a, start_date = "2018-01-01", end_date = "2018-12-31")

thrs95dur2Plot <- plot_grid(thrs95dur2Num, thrs95dur2Yr, nrow = 2)

ggsave(filename = "plots/thresholds_temp/2day_95.pdf", plot = thrs95dur2Plot)
rm(ls = "thrs95dur2Num", "thrs95dur2Yr")

#2b. 90% threshold, two day heatwaves 
thrs90dur2 <- ts2clm(data = heatwaveData, x = t, y = temp, climatologyPeriod = c("1950-01-01", "1979-12-31"), pctile = 90)
res2b <- filter(thrs90dur2, t >= "1980-01-01")
out2b <- detect_event(res2b, minDuration = 2)
heatwaves2b <- block_average(out2b)

summary(glm(count ~ year, family = "poisson", data = heatwaves2b))

thrs90dur2Num <- ggplot(data = heatwaves2b, aes(x = year, y = count)) + 
    geom_point(colour = "red") + 
    geom_line() + 
    labs(x = "year", y = "number of events")

thrs90dur2Yr <- event_line(out2b, start_date = "2018-01-01", end_date = "2018-12-31")

thrs90dur2Plot <- plot_grid(thrs90dur2Num, thrs90dur2Yr, nrow = 2)

ggsave(filename = "plots/thresholds_temp/2day_90.pdf", plot = thrs90dur2Plot)
rm(ls = "thrs90dur2Num", "thrs90dur2Yr")

#2c. 85% threshold, two day heatwaves 
thrs85dur2 <- ts2clm(data = heatwaveData, x = t, y = temp, climatologyPeriod = c("1950-01-01", "1979-12-31"), pctile = 85)
res2c <- filter(thrs85dur2, t >= "1980-01-01")
out2c <- detect_event(res2c, minDuration = 2)
heatwaves2c <- block_average(out2c)

summary(glm(count ~ year, family = "poisson", data = heatwaves2c))

thrs85dur2Num <- ggplot(data = heatwaves2c, aes(x = year, y = count)) + 
    geom_point(colour = "red") + 
    geom_line() + 
    labs(x = "year", y = "number of events")

thrs85dur2Yr <- event_line(out2c, start_date = "2018-01-01", end_date = "2018-12-31")

thrs85dur2Plot <- plot_grid(thrs85dur2Num, thrs85dur2Yr, nrow = 2)

ggsave(filename = "plots/thresholds_temp/2day_85.pdf", plot = thrs85dur2Plot)
rm(ls = "thrs85dur2Num", "thrs85dur2Yr")

#2d. 99% threshold, two day heatwaves 
thrs99dur2 <- ts2clm(data = heatwaveData, x = t, y = temp, climatologyPeriod = c("1950-01-01", "1979-12-31"), pctile = 99)
res2d <- filter(thrs99dur2, t >= "1980-01-01")
out2d <- detect_event(res2d, minDuration = 2)
heatwaves2d <- block_average(out2d)

summary(glm(count ~ year, family = "poisson", data = heatwaves2d))

thrs99dur2Num <- ggplot(data = heatwaves2d, aes(x = year, y = count)) + 
    geom_point(colour = "red") + 
    geom_line() + 
    labs(x = "year", y = "number of events")

thrs99dur2Yr <- event_line(out2d, start_date = "2018-01-01", end_date = "2018-12-31")

thrs99dur2Plot <- plot_grid(thrs99dur2Num, thrs99dur2Yr, nrow = 2)

ggsave(filename = "plots/thresholds_temp/2day_99.pdf", plot = thrs99dur2Plot)
rm(ls = "thrs99dur2Num", "thrs99dur2Yr")

#3a. 95% threshold, three day heatwaves 
thrs95dur3 <- ts2clm(data = heatwaveData, x = t, y = temp, climatologyPeriod = c("1950-01-01", "1979-12-31"), pctile = 95)
res3a <- filter(thrs95dur3, t >= "1980-01-01")
out3a <- detect_event(res3a, minDuration = 3)
heatwaves3a <- block_average(out3a)

summary(glm(count ~ year, family = "poisson", data = heatwaves3a))

thrs95dur3Num <- ggplot(data = heatwaves3a, aes(x = year, y = count)) + 
    geom_point(colour = "red") + 
    geom_line() + 
    labs(x = "year", y = "number of events")

thrs95dur3Yr <- event_line(out3a, start_date = "2018-01-01", end_date = "2018-12-31")

thrs95dur3Plot <- plot_grid(thrs95dur3Num, thrs95dur3Yr, nrow = 2)

ggsave(filename = "plots/thresholds_temp/3day_95.pdf", plot = thrs95dur3Plot)
rm(ls = "thrs95dur3Num", "thrs95dur3Yr")

#3b. 90% threshold, three day heatwaves 
thrs90dur3 <- ts2clm(data = heatwaveData, x = t, y = temp, climatologyPeriod = c("1950-01-01", "1979-12-31"), pctile = 90)
res3b <- filter(thrs90dur3, t >= "1980-01-01")
out3b <- detect_event(res3b, minDuration = 3)
heatwaves3b <- block_average(out3b)

summary(glm(count ~ year, family = "poisson", data = heatwaves3b))

thrs90dur3Num <- ggplot(data = heatwaves3b, aes(x = year, y = count)) + 
    geom_point(colour = "red") + 
    geom_line() + 
    labs(x = "year", y = "number of events")

thrs90dur3Yr <- event_line(out3b, start_date = "2018-01-01", end_date = "2018-12-31")

thrs90dur3Plot <- plot_grid(thrs90dur3Num, thrs90dur3Yr, nrow = 2)

ggsave(filename = "plots/thresholds_temp/3day_90.pdf", plot = thrs90dur3Plot)
rm(ls = "thrs90dur3Num", "thrs90dur3Yr")

#3c. 85% threshold, three day heatwaves 
thrs85dur3 <- ts2clm(data = heatwaveData, x = t, y = temp, climatologyPeriod = c("1950-01-01", "1979-12-31"), pctile = 85)
res3c <- filter(thrs85dur3, t >= "1980-01-01")
out3c <- detect_event(res3c, minDuration = 3)
heatwaves3c <- block_average(out3c)

summary(glm(count ~ year, family = "poisson", data = heatwaves3c))

thrs85dur3Num <- ggplot(data = heatwaves3c, aes(x = year, y = count)) + 
    geom_point(colour = "red") + 
    geom_line() + 
    labs(x = "year", y = "number of events")

thrs85dur3Yr <- event_line(out3c, start_date = "2018-01-01", end_date = "2018-12-31")

thrs85dur3Plot <- plot_grid(thrs85dur3Num, thrs85dur3Yr, nrow = 2)

ggsave(filename = "plots/thresholds_temp/3day_85.pdf", plot = thrs85dur3Plot)
rm(ls = "thrs85dur3Num", "thrs85dur3Yr")

#3d. 99% threshold, three day heatwaves 
thrs99dur3 <- ts2clm(data = heatwaveData, x = t, y = temp, climatologyPeriod = c("1950-01-01", "1979-12-31"), pctile = 99)
res3d <- filter(thrs99dur3, t >= "1980-01-01")
out3d <- detect_event(res3d, minDuration = 3)
heatwaves3d <- block_average(out3d)

summary(glm(count ~ year, family = "poisson", data = heatwaves3d))

thrs99dur3Num <- ggplot(data = heatwaves3d, aes(x = year, y = count)) + 
    geom_point(colour = "red") + 
    geom_line() + 
    labs(x = "year", y = "number of events")

thrs99dur3Yr <- event_line(out3d, start_date = "2018-01-01", end_date = "2018-12-31")

thrs99dur3Plot <- plot_grid(thrs99dur3Num, thrs99dur3Yr, nrow = 2)

ggsave(filename = "plots/thresholds_temp/3day_99.pdf", plot = thrs99dur3Plot)
rm(ls = "thrs99dur3Num", "thrs99dur3Yr")


##14/04 - ERA5 Analysis - Precipitation data ----
## PRECIP DATA: 
# era5BASELINE1_precip = 1950-1960
# era5BASELINE2_precip = 1961-1970
# era5BASELINE3_precip = 1971-1979

# era5part1_precip = 1980-1990
# era5part2_precip = 1991-2000
# era5part3_precip = 2001-2009
# era5part4_precip = 2010-2018

#First, load all of the precipitation data from 1980-2018, and all of the baseline 
#data from 1950-1980
precipData1 <- brick("data/era5part1_precip.grib")
print(precipData1)
precipData2 <-brick("data/era5part2_precip.grib")
print(precipData2)
precipData3 <- brick("data/era5part3_precip.grib")
print(precipData3)
precipData4 <- brick("data/era5part4_precip.grib")
print(precipData4)

precipBaseline1 <- brick("data/era5BASELINE1_precip.grib")
print(precipBaseline1)
precipBaseline2 <- brick("data/era5BASELINE2_precip.grib")
print(precipBaseline2)
precipBaseline3 <- brick("data/era5BASELINE3_precip.grib")
print(precipBaseline3)

#Now, select the coordinates of the sampling sites 
precipData1 <- extract(precipData1, coordinates)
precipData2 <- extract(precipData2, coordinates)
precipData3 <- extract(precipData3, coordinates)
precipData4 <- extract(precipData4, coordinates)
precipBaseline1 <- extract(precipBaseline1, coordinates)
precipBaseline2 <- extract(precipBaseline2, coordinates)
precipBaseline3 <- extract(precipBaseline3, coordinates)

#Instead of selecting the maximum precipitation value for each day, I will calculate the total 
#sum of precipitation for the whole day 

num_precipBaseline1_records <- length(precipBaseline1)
complete_precipBaseline1_days <- floor(num_precipBaseline1_records / 24)
num_precipBaseline2_records <- length(precipBaseline2)
complete_precipBaseline2_days <- floor(num_precipBaseline2_records / 24)
num_precipBaseline3_records <- length(precipBaseline3)
complete_precipBaseline3_days <- floor(num_precipBaseline3_records / 24)

num_precip1_records <- length(precipData1)
complete_precip1_days <- floor(num_precip1_records / 24)
num_precip2_records <- length(precipData2)
complete_precip2_days <- floor(num_precip2_records / 24)
num_precip3_records <- length(precipData3)
complete_precip3_days <- floor(num_precip3_records / 24)
num_precip4_records <- length(precipData4)
complete_precip4_days <- floor(num_precip4_records / 24)

#Join all of the data together 
hourlyPrecipBaseline1 <- matrix(precipBaseline1[1:(complete_precipBaseline1_days * 24)], ncol = 24, byrow = TRUE)
head(hourlyPrecipBaseline1)
hourlyPrecipBaseline2 <- matrix(precipBaseline2[1:(complete_precipBaseline2_days * 24)], ncol = 24, byrow = TRUE)
head(hourlyPrecipBaseline2)
hourlyPrecipBaseline3 <- matrix(precipBaseline3[1:(complete_precipBaseline3_days * 24)], ncol = 24, byrow = TRUE)
head(hourlyPrecipBaseline3)

hourlyPrecip1 <- matrix(precipData1[1:(complete_precip1_days * 24)], ncol = 24, byrow = TRUE)
head(hourlyPrecip1)
hourlyPrecip2 <- matrix(precipData2[1:(complete_precip2_days * 24)], ncol = 24, byrow = TRUE)
head(hourlyPrecip2)
hourlyPrecip3 <- matrix(precipData3[1:(complete_precip3_days * 24)], ncol = 24, byrow = TRUE)
head(hourlyPrecip3)
hourlyPrecip4 <- matrix(precipData4[1:(complete_precip4_days * 24)], ncol = 24, byrow = TRUE)
head(hourlyPrecip4)

#Calculate total daily precipitation 
dailyPrecipBaseline1 <- rowSums(hourlyPrecipBaseline1)
dailyPrecipBaseline2 <- rowSums(hourlyPrecipBaseline2)
dailyPrecipBaseline3 <- rowSums(hourlyPrecipBaseline3)

dailyPrecip1 <- rowSums(hourlyPrecip1)
dailyPrecip2 <- rowSums(hourlyPrecip2)
dailyPrecip3 <- rowSums(hourlyPrecip3)
dailyPrecip4 <- rowSums(hourlyPrecip4)

#Finally, combine all of the precipitation data together
combinedPrecip <- c(dailyPrecipBaseline1, dailyPrecipBaseline2, dailyPrecipBaseline3, dailyPrecip1, dailyPrecip2, dailyPrecip3, dailyPrecip4)

#Now, add corresponding dates to the dataset 
dates <- seq.Date(from = as.Date("1950-01-01"), by = "day", length.out = length(combinedPrecip))

#Create a dataset for total precipitation recorded at the site from 1950-1980 (baseline period)
#and 1980-2018
precipitationData <- data.frame(t = dates, precip = combinedPrecip) 

rm(ls = "precipData1", "precipData2", "precipData3", "precipData4")
rm(ls = "precipBaseline1", "precipBaseline2", "precipBaseline3")
rm(ls = "num_precipBaseline1_records", "num_precipBaseline2_records", "num_precipBaseline3_records")
rm(ls = "num_precip1_records", "num_precip2_records", "num_precip3_records", "num_precip4_records")
rm(ls = "complete_precipBaseline1_days", "complete_precipBaseline2_days", "complete_precipBaseline3_days")
rm(ls = "complete_precip1_days", "complete_precip2_days", "complete_precip3_days", "complete_precip4_days")
rm(ls = "hourlyPrecipBaseline1", "hourlyPrecipBaseline2", "hourlyPrecipBaseline3")
rm(ls = "hourlyPrecip1", "hourlyPrecip2", "hourlyPrecip3", "hourlyPrecip4")
rm(ls = "dailyPrecipBaseline1", "dailyPrecipBaseline2", "dailyPrecipBaseline3")
rm(ls = "dailyPrecip1", "dailyPrecip2", "dailyPrecip3", "dailyPrecip4")

write.csv(precipitationData, "data/precipitationData.csv") #write the data to a csv file for future use

precipitationData <- read_csv("data/precipitationData.csv")

#To get an understanding of the data, heres an example where I calculated the total precipitation recorded in 2018
data2018 <- precipitationData |> 
    filter(str_detect(t, "2018")) |> 
    mutate(total_precip = sum(precip))

total2018 <- data2018[1,3]
total2018 * 1000 #total precipitation recorded at the site in 2018 in mm

#Now, I can use the exact same methods I used to analyse the heatwave data to analyse the precipitaion data 
library(heatwaveR)
library(tidyverse)
library(cowplot)
library(ggplot2)

#Initially, for precipitation we only want to count the total number of days per year that exceed a given threshold, therefore 
#we do not need to change minimum duration, we can just set it to 1 and use 

#95% threshold 
thrs95precip <- ts2clm(data = precipitationData, x = t, y = precip, climatologyPeriod = c("1950-01-01", "1979-12-31"), pctile = 95)
res1 <- filter(thrs95precip, t >= "1980-01-01") #filter for study years 
out1 <- detect_event(res1, x = t, y = precip, minDuration = 1)
precip1 <- block_average(out1, x = t, y = precip)

thrs95_exceeded <- res1 |> 
    separate(t, c("year", "month", "date"), sep = "-") |> 
    group_by(year) |> 
    summarise(yearlyCount = sum(precip > thresh))

thrs95_exceeded$year <- as.numeric(thrs95_exceeded$year)
thrs95_exceeded$yearlyCount <- as.numeric(thrs95_exceeded$yearlyCount)

thrs95precipPlot <- ggplot(data = thrs95_exceeded, aes(x = year, y = yearlyCount)) + 
        geom_point(colour = "red") + 
        geom_line() + 
        labs(x = "year", y = "number of events")

ggsave(filename = "plots/thresholds_precip/95.pdf", plot = thrs95precipPlot)

#90% threshold 
thrs90precip <- ts2clm(data = precipitationData, x = t, y = precip, climatologyPeriod = c("1950-01-01", "1979-12-31"), pctile = 90)
res2 <- filter(thrs90precip, t >= "1980-01-01") #filter for study years 
out2 <- detect_event(res2, x = t, y = precip, minDuration = 1)
precip2 <- block_average(out2, x = t, y = precip)

thrs90_exceeded <- res2 |> 
    separate(t, c("year", "month", "date"), sep = "-") |> 
    group_by(year) |> 
    summarise(yearlyCount = sum(precip > thresh))

thrs90_exceeded$year <- as.numeric(thrs90_exceeded$year)
thrs90_exceeded$yearlyCount <- as.numeric(thrs90_exceeded$yearlyCount)

thrs90precipPlot <- ggplot(data = thrs90_exceeded, aes(x = year, y = yearlyCount)) + 
        geom_point(colour = "red") + 
        geom_line() + 
        labs(x = "year", y = "number of events")

ggsave(filename = "plots/thresholds_precip/90.pdf", plot = thrs90precipPlot)

#85% threshold 
thrs85precip <- ts2clm(data = precipitationData, x = t, y = precip, climatologyPeriod = c("1950-01-01", "1979-12-31"), pctile = 85)
res3 <- filter(thrs85precip, t >= "1980-01-01") #filter for study years 
out3 <- detect_event(res3, x = t, y = precip, minDuration = 1)
precip3 <- block_average(out3, x = t, y = precip)

thrs85_exceeded <- res3 |> 
    separate(t, c("year", "month", "date"), sep = "-") |> 
    group_by(year) |> 
    summarise(yearlyCount = sum(precip > thresh))

thrs85_exceeded$year <- as.numeric(thrs85_exceeded$year)
thrs85_exceeded$yearlyCount <- as.numeric(thrs85_exceeded$yearlyCount)

thrs85precipPlot <- ggplot(data = thrs85_exceeded, aes(x = year, y = yearlyCount)) + 
        geom_point(colour = "red") + 
        geom_line() + 
        labs(x = "year", y = "number of events")

ggsave(filename = "plots/thresholds_precip/85.pdf", plot = thrs85precipPlot)

#99% threshold 
thrs99precip <- ts2clm(data = precipitationData, x = t, y = precip, climatologyPeriod = c("1950-01-01", "1979-12-31"), pctile = 99)
res4 <- filter(thrs99precip, t >= "1980-01-01") #filter for study years 
out4 <- detect_event(res4, x = t, y = precip, minDuration = 1)
precip4 <- block_average(out4, x = t, y = precip)

thrs99_exceeded <- res4 |> 
    separate(t, c("year", "month", "date"), sep = "-") |> 
    group_by(year) |> 
    summarise(yearlyCount = sum(precip > thresh))

thrs99_exceeded$year <- as.numeric(thrs99_exceeded$year)
thrs99_exceeded$yearlyCount <- as.numeric(thrs99_exceeded$yearlyCount)

thrs99precipPlot <- ggplot(data = thrs99_exceeded, aes(x = year, y = yearlyCount)) + 
        geom_point(colour = "red") + 
        geom_line() + 
        labs(x = "year", y = "number of events")

ggsave(filename = "plots/thresholds_precip/99.pdf", plot = thrs99precipPlot)


##15/04 - Abundance Analysis and Regression ---- 
#Now that I have all of the temperature and precipitation data, I can go through and compare it 
#to the abundance data I previously collected for the population of brown trout at the sampling site 

#Population abundance 
salmoDataTable 

salmoDataTable$year <- as.numeric(salmoDataTable$year)

library(dplyr)

#Calculate log of growth rate 
growthRates <- salmoDataTable |> 
    mutate(growth_rate = log10(Abundance + 1) - log10(dplyr::lag(Abundance + 1)))  #+1 to minimise error if population = 0

#I can now do a sensitivity test with different thresholds and minimum durations 
#First, consider heatwaves with a minimum duration of 1 day and a 90% threshold
regression1 <- growthRates |> 
    left_join(heatwaves1b[,1:2], by = "year") |> 
    rename(heatwaves = count) |>
    mutate(heatwaves_prev = dplyr::lag(heatwaves)) |> 
    dplyr::select(!heatwaves) |> 
    filter(year > 1980)

lm1 <- lm(growth_rate ~ heatwaves_prev, data = regression1)

summary(lm1)
anova(lm1)

plot1 <- ggplot(data = regression1, aes(x = heatwaves_prev, y = growth_rate)) +  
    geom_point() + 
    geom_smooth(aes(x = heatwaves_prev, y = growth_rate), method = 'lm') + 
    labs(x = "number of heatwaves", y = "log(growth rate)", title = "90% threshold, 1 day")

ggsave(filename = "plots/regression_plots/regression1.pdf", plot = plot1)

#Now, try a minimum duration of 1 day and a 95% threshold 
regression2 <- growthRates |> 
    left_join(heatwaves1a[,1:2], by = "year") |> 
    rename(heatwaves = count) |> 
    mutate(heatwaves_prev = dplyr::lag(heatwaves)) |>
    dplyr::select(!heatwaves) |>
    filter(year > 1980)

lm2 <- lm(growth_rate ~ heatwaves_prev, data = regression2)

summary(lm2)
anova(lm2)

plot2 <- ggplot(data = regression2, aes(x = heatwaves_prev, y = growth_rate)) +  
    geom_point() + 
    geom_smooth(aes(x = heatwaves_prev, y = growth_rate), method = 'lm') + 
    labs(x = "number of heatwaves", y = "log(growth rate)", title = "95% threshold, 1 day")

ggsave(filename = "plots/regression_plots/regression2.pdf", plot = plot2)

#Minimum duration of 2 days and a 90% threshold 
regression3 <- growthRates |> 
    left_join(heatwaves2b[,1:2], by = "year") |> 
    rename(heatwaves = count) |> 
    mutate(heatwaves_prev = dplyr::lag(heatwaves)) |> 
    dplyr::select(!heatwaves) |> 
    filter(year > 1980)

lm3 <- lm(growth_rate ~ heatwaves_prev, data = regression3)

summary(lm3)
anova(lm3) 

plot3 <- ggplot(data = regression3, aes(x = heatwaves_prev, y = growth_rate)) +  
    geom_point() + 
    geom_smooth(aes(x = heatwaves_prev, y = growth_rate), method = 'lm') + 
    labs(x = "number of heatwaves", y = "log(growth rate)", title = "90% threshold, 2 days")

ggsave(filename = "plots/regression_plots/regression3.pdf", plot = plot3)

#Minimum duration of 2 days, 95% threshold
regression4 <- growthRates |> 
    left_join(heatwaves2a[,1:2], by = "year") |> 
    rename(heatwaves = count) |> 
    mutate(heatwaves_prev = dplyr::lag(heatwaves)) |> 
    dplyr::select(!heatwaves) |>
    filter(year > 1980)

lm4 <- lm(growth_rate ~ heatwaves_prev, data = regression4)

summary(lm4)
anova(lm4)

plot4 <- ggplot(data = regression4, aes(x = heatwaves_prev, y = growth_rate)) +  
    geom_point() + 
    geom_smooth(aes(x = heatwaves_prev, y = growth_rate), method = 'lm') + 
    labs(x = "number of heatwaves", y = "log(growth rate)", title = "95% threshold, 2 days")

ggsave(filename = "plots/regression_plots/regression4.pdf", plot = plot4)

#Minimum duration of 3 days, 90% threshold 
regression5 <- growthRates |> 
    left_join(heatwaves3b[,1:2], by = "year") |> 
    rename(heatwaves = count) |> 
    mutate(heatwaves_prev = dplyr::lag(heatwaves)) |> 
    dplyr::select(!heatwaves) |> 
    filter(year > 1980)

lm5 <- lm(growth_rate ~ heatwaves_prev, data = regression5)

summary(lm5)
anova(lm5)

plot5 <- ggplot(data = regression5, aes(x = heatwaves_prev, y = growth_rate)) +  
    geom_point() + 
    geom_smooth(aes(x = heatwaves_prev, y = growth_rate), method = 'lm') + 
    labs(x = "number of heatwaves", y = "log(growth rate)", title = "90% threshold, 3 days")

ggsave(filename = "plots/regression_plots/regression5.pdf", plot = plot5)

#Minimum duration of 3 days, 95% threshold 
regression6 <- growthRates |> 
    left_join(heatwaves3a[,1:2], by = "year") |> 
    rename(heatwaves = count) |> 
    mutate(heatwaves_prev = dplyr::lag(heatwaves)) |> 
    dplyr::select(!heatwaves) |> 
    filter(year > 1980)

lm6 <- lm(growth_rate ~ heatwaves_prev, data = regression6)

summary(lm6)
anova(lm6) 

plot6 <- ggplot(data = regression6, aes(x = heatwaves_prev, y = growth_rate)) +  
    geom_point() + 
    geom_smooth(aes(x = heatwaves_prev, y = growth_rate), method = 'lm') + 
    labs(x = "number of heatwaves", y = "log(growth rate)", title = "95% threshold, 3 days")

ggsave(filename = "plots/regression_plots/regression6.pdf", plot = plot6)

#So far, it appears that there is a stronger effect of heatwaves on the population abundance 
#using a 95% threshold as opposed to a 90% threshold, and it is slightly stronger with a minimum duration 
#of two days followed by three days. Interestingly, 1 day minimum with 90% threshold had the strongest effect. 
#All of these models have a positive effect on growth rate

#I want to try running heatwaveR with a minimum duration of 4 days to make sure that the effect 
#continues to become less significant with longer minimum duration

#Minimum duration of 4 days, 90% threshold
thrs90dur4 <- ts2clm(data = heatwaveData, x = t, y = temp, climatologyPeriod = c("1950-01-01", "1979-12-31"), pctile = 90)
res4b <- filter(thrs90dur4, t >= "1980-01-01")
out4b <- detect_event(res4b, minDuration = 4)
heatwaves4b <- block_average(out4b)

regression7 <- growthRates |> 
    left_join(heatwaves4b[,1:2], by = "year") |> 
    rename(heatwaves = count) |> 
    mutate(heatwaves_prev = dplyr::lag(heatwaves)) |> 
    dplyr::select(!heatwaves) |>
    filter(year > 1980)

lm7 <- lm(growth_rate ~ heatwaves_prev, data = regression7)

summary(lm7)
anova(lm7)

plot7 <- ggplot(data = regression7, aes(x = heatwaves_prev, y = growth_rate)) +  
    geom_point() + 
    geom_smooth(aes(x = heatwaves_prev, y = growth_rate), method = 'lm') + 
    labs(x = "number of heatwaves", y = "log(growth rate)", title = "90% threshold, 4 days")

ggsave(filename = "plots/regression_plots/regression7.pdf", plot = plot7)

#Minimum duration of 4 days, 95% threshold 
thrs95dur4 <- ts2clm(data = heatwaveData, x = t, y = temp, climatologyPeriod = c("1950-01-01", "1979-12-31"), pctile = 95)
res4a <- filter(thrs95dur4, t >= "1980-01-01")
out4a <- detect_event(res4a, minDuration = 4)
heatwaves4a <- block_average(out4a)

regression8 <- growthRates |> 
    left_join(heatwaves4a[,1:2], by = "year") |> 
    rename(heatwaves = count) |> 
    mutate(heatwaves_prev = dplyr::lag(heatwaves)) |> 
    dplyr::select(!heatwaves) |>
    filter(year > 1980)

lm8 <- lm(growth_rate ~ heatwaves_prev, data = regression8)

summary(lm8)
anova(lm8)

plot8 <- ggplot(data = regression8, aes(x = heatwaves_prev, y = growth_rate)) +  
    geom_point() + 
    geom_smooth(aes(x = heatwaves_prev, y = growth_rate), method = 'lm') + 
    labs(x = "number of heatwaves", y = "log(growth rate)", title = "95% threshold, 4 days")

ggsave(filename = "plots/regression_plots/regression8.pdf", plot = plot8)

#With minimum duration of 4 days there are no significant observed effects of heatwaves on population growth 

#A lot of unexplained variation in the models meaning I should reconsider my definitions of heatwaves to potentially include 
#an extra factor which considers the number of total days in each year that exceed the given threshold 

#Repreat the above process with the precipitation data 
#Start with 95% threshold 
regression9 <- growthRates |> 
    left_join(thrs95_exceeded, by = "year") |> 
    rename(precipitation = yearlyCount) |> 
    mutate(precip_prev = dplyr::lag(precipitation)) |> 
    dplyr::select(!precipitation) |>
    filter(year > 1980)

lm9 <- lm(growth_rate ~ precip_prev, data = regression9)

summary(lm9)
anova(lm9)

plot9 <- ggplot(data = regression9, aes(x = precip_prev, y = growth_rate)) +  
    geom_point() + 
    geom_smooth(aes(x = precip_prev, y = growth_rate), method = 'lm') + 
    labs(x = "number of days of extreme precipitation", y = "log(growth rate)", title = "95% threshold")

ggsave(filename = "plots/regression_plots/regression9.pdf", plot = plot9)

#90% threshold 
regression10 <- growthRates |> 
    left_join(thrs90_exceeded, by = "year") |> 
    rename(precipitation = yearlyCount) |> 
    mutate(precip_prev = dplyr::lag(precipitation)) |> 
    dplyr::select(!precipitation) |>
    filter(year > 1980)

lm10 <- lm(growth_rate ~ precip_prev, data = regression10)

summary(lm10)
anova(lm10)

plot10 <- ggplot(data = regression10, aes(x = precip_prev, y = growth_rate)) +  
    geom_point() + 
    geom_smooth(aes(x = precip_prev, y = growth_rate), method = 'lm') + 
    labs(x = "number of days of extreme precipitation", y = "log(growth rate)", title = "90% threshold")

ggsave(filename = "plots/regression_plots/regression10.pdf", plot = plot10)

#85% threshold 
regression11 <- growthRates |> 
    left_join(thrs85_exceeded, by = "year") |> 
    rename(precipitation = yearlyCount) |> 
    mutate(precip_prev = dplyr::lag(precipitation)) |> 
    dplyr::select(!precipitation) |>
    filter(year > 1980)

lm11 <- lm(growth_rate ~ precip_prev, data = regression11)

summary(lm11)
anova(lm11)

plot11 <- ggplot(data = regression11, aes(x = precip_prev, y = growth_rate)) +  
    geom_point() + 
    geom_smooth(aes(x = precip_prev, y = growth_rate), method = 'lm') + 
    labs(x = "number of days of extreme precipitation", y = "log(growth rate)", title = "85% threshold")

ggsave(filename = "plots/regression_plots/regression11.pdf", plot = plot11)

#99% threshold 
regression12 <- growthRates |> 
    left_join(thrs99_exceeded, by = "year") |> 
    rename(precipitation = yearlyCount) |> 
    mutate(precip_prev = dplyr::lag(precipitation)) |> 
    dplyr::select(!precipitation) |> 
    filter(year > 1980)

lm12 <- lm(growth_rate ~ precip_prev, data = regression12)

summary(lm12)
anova(lm12)

plot12 <- ggplot(data = regression12, aes(x = precip_prev, y = growth_rate)) +  
    geom_point() + 
    geom_smooth(aes(x = precip_prev, y = growth_rate), method = 'lm') + 
    labs(x = "number of days of extreme precipitation", y = "log(growth rate)", title = "99% threshold")

ggsave(filename = "plots/regression_plots/regression12.pdf", plot = plot12)

#In the case of precipitation, the effect on population growth rate is strongest with a 95% threshold but is positive. 

#Combine the two variables into a multiple regression model
#Start with a 95% threshold for precipitation and a minimum of 3 day heatwaves with a 95% threshold 
regression13 <- growthRates |> 
    left_join(heatwaves3a[,1:2], by = "year") |> 
    left_join(thrs95_exceeded, by = "year") |> 
    rename(heatwaves = count, precipitation = yearlyCount) |> 
    mutate(heatwaves_prev = dplyr::lag(heatwaves)) |> 
    mutate(precip_prev = dplyr::lag(precipitation)) |> 
    dplyr::select(!heatwaves & !precipitation) |> 
    filter(year > 1980)

lm13 <- lm(growth_rate ~ heatwaves_prev * precip_prev, data = regression13)

summary(lm13)
anova(lm13)

#95% threshold for precipitation, minimum duration of 2 day heatwaves with 95% threshold
regression14 <- growthRates |> 
    left_join(heatwaves2a[,1:2], by = "year") |> 
    left_join(thrs95_exceeded, by = "year") |> 
    rename(heatwaves = count, precipitation = yearlyCount) |> 
    mutate(heatwaves_prev = dplyr::lag(heatwaves)) |> 
    mutate(precip_prev = dplyr::lag(precipitation)) |> 
    dplyr::select(!heatwaves & !precipitation) |> 
    filter(year > 1980)

lm14 <- lm(growth_rate ~ heatwaves_prev * precip_prev, data = regression14)

summary(lm14)
anova(lm14)

#95% threshold for precipitation, minimum duration of 1 day heatwaves with 90% threshold 
regression15 <- growthRates |> 
    left_join(heatwaves1b[,1:2], by = "year") |> 
    left_join(thrs95_exceeded, by = "year") |> 
    rename(heatwaves = count, precipitation = yearlyCount) |> 
    mutate(heatwaves_prev = dplyr::lag(heatwaves)) |> 
    mutate(precip_prev = dplyr::lag(precipitation)) |> 
    dplyr::select(!heatwaves & !precipitation) |> 
    filter(year > 1980)

lm15 <- lm(growth_rate ~ heatwaves_prev * precip_prev, data = regression15)

summary(lm15)
anova(lm15)


##28/04 - Refining analysis ----
#First, define a predictor variable for average annual temperatures during study period with specified yearly period
yearlyTempAvg <- heatwaveData |> 
    mutate(year = if_else(month(t) >= 07, 
                                as.integer(year(t)),
                                as.integer(year(t) - 1))) |> #counts yearly temp avg for year t-1 as temps from month 07 
    group_by(year) |>                                        #in year t-1 to month 06 in year t
    summarise(yearlyMeanTemp = mean(temp)) |> 
    filter(year >= 1980)

#I also need to define a predictor variable for total annual precipitation during the study period with specified yearly period
yearlyPrecipSum <- precipitationData |> 
    mutate(year = if_else(month(t) >= 07, 
                                as.integer(year(t)),
                                as.integer(year(t) - 1))) |> 
    group_by(year) |> 
    summarise(yearlyTotalPrecip = sum(precip) * 1000) |> # x 1000 so that it is in mm
    filter(year >= 1980)

#Perform a regression analysis for average temperature and total precipitation on growth rate 
simpleRegression <- growthRates |> 
    left_join(yearlyTempAvg, by = "year") |> 
    left_join(yearlyPrecipSum, by = "year") |> 
    mutate(meanTemp_prev = dplyr::lag(yearlyMeanTemp)) |> 
    mutate(totalPrecip_prev = dplyr::lag(yearlyTotalPrecip)) |> 
    mutate(abundance_prev = dplyr::lag(Abundance)) |> 
    dplyr::select(!yearlyMeanTemp & !yearlyTotalPrecip & !Abundance) |> 
    filter(year > 1980)

simplelm <- lm(growth_rate ~ meanTemp_prev + totalPrecip_prev + abundance_prev, data = simpleRegression)
summary(simplelm)

predictors <- all.vars(formula(simplelm))[-1]

plots <- map(predictors, ~ {
    ggplot(simpleRegression, aes_string(x = .x, y = "growth_rate")) +
        geom_point() +
        geom_smooth(method = "lm") +
        labs(x = .x, y = "log(growth rate)", title = paste("Effect of", .x, "on growth rate"))
})

#I first want to develop a model with average annual temperature, total number of heatwave events, average duration of 
#heatwaves, and total number of days exceeding the threshold for a 95% threshold and a minimum duration of 3 days

#Number of days exceeding 95% threshold
thrs95exceeded_heatwaves <- res3a |> 
    mutate(year = if_else(month(t) >= 07, 
                                as.integer(year(t)),
                                as.integer(year(t) - 1))) |> 
    group_by(year) |> 
    summarise(daysExceeded = sum(temp > thresh)) |> 
    filter(year >= 1980)

#Number of heatwave events with 95% threshold and a minimum duration of 3 days + average duration 
newHeatwaves1 <- out3a$event |> 
    mutate(year = if_else(month(date_start) >= 07, 
                                     as.integer(year(date_start)), 
                                     as.integer(year(date_start) - 1))) |> 
    dplyr::select(event_no, duration, date_start, date_end, year) |> 
    group_by(year) |>
    summarise(mean_duration = mean(duration), 
              heatwaves = n()) 
    
#regression
newRegression1 <- growthRates |> 
    left_join(newHeatwaves1, by = "year") |> 
    left_join(yearlyTempAvg, by = "year") |>
    left_join(thrs95exceeded_heatwaves, by = "year") |> 
    mutate(heatwaves_prev = dplyr::lag(heatwaves)) |> 
    mutate(avgTemp_prev = dplyr::lag(yearlyMeanTemp)) |>
    mutate(daysExceeded_prev = dplyr::lag(daysExceeded)) |>
    mutate(avgDuration_prev = dplyr::lag(mean_duration)) |>
    mutate(abundance_prev = dplyr::lag(Abundance)) |> 
    dplyr::select(!heatwaves & !Abundance & !yearlyMeanTemp & !daysExceeded & !mean_duration) |> 
    filter(year > 1980)

newlm1 <- lm(growth_rate ~ heatwaves_prev + avgTemp_prev + daysExceeded_prev + avgDuration_prev + abundance_prev, data = newRegression1)

summary(newlm1) 

predictors <- all.vars(formula(newlm1))[-1]

plots <- map(predictors, ~ {
    ggplot(newRegression1, aes_string(x = .x, y = "growth_rate")) +
        geom_point() +
        geom_smooth(method = "lm") +
        labs(x = .x, y = "log(growth rate)", title = paste("Effect of", .x, "on growth rate"))
})

#All of the temperature predictors, though not significant, have a positive effect on growth rate 
#Abundance as expected is significant and has a negative effect on growth rate 

#3 day minimum, 99% threshold 
#Number of days exceeding 99% threshold
thrs99exceeded_heatwaves <- res3d |> 
    mutate(year = if_else(month(t) >= 07, 
                                as.integer(year(t)),
                                as.integer(year(t) - 1))) |> 
    group_by(year) |> 
    summarise(daysExceeded = sum(temp > thresh)) |> 
    filter(year >= 1980)

#Number of heatwave events with 99% threshold and a minimum duration of 3 days + average duration 
newHeatwaves2 <- out3d$event |> 
    mutate(year = if_else(month(date_start) >= 07, 
                                     as.integer(year(date_start)), 
                                     as.integer(year(date_start) - 1))) |> 
    dplyr::select(event_no, duration, date_start, date_end, year) |> 
    group_by(year) |>
    summarise(mean_duration = mean(duration), 
              heatwaves = n()) 

#regression
newRegression2 <- growthRates |> 
    left_join(newHeatwaves2, by = "year") |> 
    left_join(yearlyTempAvg, by = "year") |>
    left_join(thrs99exceeded_heatwaves, by = "year") |> 
    mutate(heatwaves_prev = dplyr::lag(heatwaves), 
           heatwaves_prev = replace_na(heatwaves_prev, 0)) |> 
    mutate(avgTemp_prev = dplyr::lag(yearlyMeanTemp)) |>
    mutate(daysExceeded_prev = dplyr::lag(daysExceeded)) |>
    mutate(avgDuration_prev = dplyr::lag(mean_duration), 
           avgDuration_prev = replace_na(avgDuration_prev, 0)) |>
    mutate(abundance_prev = dplyr::lag(Abundance)) |> 
    dplyr::select(!heatwaves & !Abundance & !yearlyMeanTemp & !daysExceeded & !mean_duration) |> 
    filter(year > 1980)

newlm2 <- lm(growth_rate ~ heatwaves_prev + avgTemp_prev + daysExceeded_prev + avgDuration_prev + abundance_prev, data = newRegression2)

summary(newlm2) 

predictors <- all.vars(formula(newlm2))[-1]

plots <- map(predictors, ~ {
    ggplot(newRegression2, aes_string(x = .x, y = "growth_rate")) +
        geom_point() +
        geom_smooth(method = "lm") +
        labs(x = .x, y = "log(growth rate)", title = paste("Effect of", .x, "on growth rate"))
})

#Analyse precipitation data with the new variables
#Start with 95% threshold, I already have a variable for the number of days exceeding the threshold but I need 
#to modify it so that it accounts for the new time span
thrs95exceeded_precip <- res1 |> 
    mutate(year = if_else(month(t) >= 07, 
                                as.integer(year(t)),
                                as.integer(year(t) - 1))) |> 
    group_by(year) |> 
    summarise(daysExceeded_precip = sum(precip > thresh)) |> 
    filter(year >= 1980)

#Now, using heatwaveR to calculate the number of precip events with a 95% threshold and a minimum duration of 1 day + average duration
thrs95dur1_precip <- ts2clm(data = precipitationData, x = t, y = precip, climatologyPeriod = c("1950-01-01", "1979-12-31"), pctile = 95)
precipRes1 <- filter(thrs95dur2_precip, t >= "1980-01-01") #filter for study years
precipOut1 <- detect_event(precipRes1, x = t, y = precip, minDuration = 1)
precipEvents1 <- precipOut1$event |> 
    mutate(year = if_else(month(date_start) >= 07, 
                                     as.integer(year(date_start)), 
                                     as.integer(year(date_start) - 1))) |> 
    dplyr::select(event_no, duration, date_start, date_end, year) |> 
    group_by(year) |>
    summarise(mean_duration = mean(duration), 
              precipEvents = n())

#regression
newRegression3 <- growthRates |> 
    left_join(precipEvents1, by = "year") |> 
    left_join(yearlyPrecipSum, by = "year") |>
    left_join(thrs95exceeded_precip, by = "year") |> 
    mutate(precip_prev = dplyr::lag(precipEvents)) |> 
    mutate(totalPrecip_prev = dplyr::lag(yearlyTotalPrecip)) |>
    mutate(daysExceeded_prev = dplyr::lag(daysExceeded_precip)) |>
    mutate(avgDuration_prev = dplyr::lag(mean_duration)) |>
    mutate(abundance_prev = dplyr::lag(Abundance)) |> 
    dplyr::select(!precipEvents & !Abundance & !yearlyTotalPrecip & !daysExceeded_precip & !mean_duration) |> 
    filter(year > 1980)

newlm3 <- lm(growth_rate ~ precip_prev + totalPrecip_prev + daysExceeded_prev + avgDuration_prev + abundance_prev, data = newRegression3)

summary(newlm3) 

predictors <- all.vars(formula(newlm3))[-1]

plots <- map(predictors, ~ {
    ggplot(newRegression3, aes_string(x = .x, y = "growth_rate")) +
        geom_point() +
        geom_smooth(method = "lm") +
        labs(x = .x, y = "log(growth rate)", title = paste("Effect of", .x, "on growth rate"))
})

#2 day minimum, 99% treshold
thrs99exceeded_precip <- res4 |> 
    mutate(year = if_else(month(t) >= 07, 
                                as.integer(year(t)),
                                as.integer(year(t) - 1))) |> 
    group_by(year) |> 
    summarise(daysExceeded_precip = sum(precip > thresh)) |> 
    filter(year >= 1980)

#Now, using heatwaveR to calculate the number of precip events with a 99% threshold and a minimum duration of 1 day + average duration
thrs99dur1_precip <- ts2clm(data = precipitationData, x = t, y = precip, climatologyPeriod = c("1950-01-01", "1979-12-31"), pctile = 99)
precipRes2 <- filter(thrs99dur2_precip, t >= "1980-01-01") #filter for study years
precipOut2 <- detect_event(precipRes2, x = t, y = precip, minDuration = 1)
precipEvents2 <- precipOut2$event |> 
    mutate(year = if_else(month(date_start) >= 07, 
                                     as.integer(year(date_start)), 
                                     as.integer(year(date_start) - 1))) |> 
    dplyr::select(event_no, duration, date_start, date_end, year) |> 
    group_by(year) |>
    summarise(mean_duration_precip = mean(duration), 
              precipEvents = n())

#regression
newRegression4 <- growthRates |> 
    left_join(precipEvents2, by = "year") |> 
    left_join(yearlyPrecipSum, by = "year") |>
    left_join(thrs99exceeded_precip, by = "year") |> 
    mutate(precip_prev = dplyr::lag(precipEvents), 
           precip_prev = replace_na(precip_prev, 0)) |> 
    mutate(totalPrecip_prev = dplyr::lag(yearlyTotalPrecip)) |>
    mutate(daysExceeded_prev = dplyr::lag(daysExceeded_precip)) |>
    mutate(avgDuration_prev = dplyr::lag(mean_duration_precip), 
           avgDuration_prev = replace_na(avgDuration_prev, 0)) |>
    mutate(abundance_prev = dplyr::lag(Abundance)) |> 
    dplyr::select(!precipEvents & !Abundance & !yearlyTotalPrecip & !daysExceeded_precip & !mean_duration_precip) |> 
    filter(year > 1980)

newlm4 <- lm(growth_rate ~ precip_prev + totalPrecip_prev + daysExceeded_prev + avgDuration_prev + abundance_prev, data = newRegression4)

summary(newlm4) 

predictors <- all.vars(formula(newlm4))[-1]

plots <- map(predictors, ~ {
    ggplot(newRegression4, aes_string(x = .x, y = "growth_rate")) +
        geom_point() +
        geom_smooth(method = "lm") +
        labs(x = .x, y = "log(growth rate)", title = paste("Effect of", .x, "on growth rate"))
})

#Analyse the interaction between rainfall and temperature to look at correlation between variables - need to find 
#a better way to quantify the variables for extreme events - eg combine number of days exceeding threshold, number of events, 
#and average duration into a single variable.
bigRegression <- growthRates |> 
    left_join(precipEvents2, by = "year") |> 
    left_join(newHeatwaves2, by = "year") |>
    left_join(yearlyPrecipSum, by = "year") |>
    left_join(yearlyTempAvg, by = "year") |>
    left_join(thrs99exceeded_precip, by = "year") |> 
    left_join(thrs99exceeded_heatwaves, by = "year") |>
    mutate(precip_prev = dplyr::lag(precipEvents), 
           precip_prev = replace_na(precip_prev, 0)) |> 
    mutate(heatwaves_prev = dplyr::lag(heatwaves),
           heatwaves_prev = replace_na(heatwaves_prev, 0)) |>
    mutate(totalPrecip_prev = dplyr::lag(yearlyTotalPrecip)) |>
    mutate(avgTemp_prev = dplyr::lag(yearlyMeanTemp)) |>
    mutate(daysExceededPrecip_prev = dplyr::lag(daysExceeded_precip)) |>
    mutate(daysExceededHeatwaves_prev = dplyr::lag(daysExceeded)) |>
    mutate(avgDurationPrecip_prev = dplyr::lag(mean_duration_precip), 
           avgDurationPrecip_prev = replace_na(avgDurationPrecip_prev, 0)) |>
    mutate(avgDurationHeatwave_prev = dplyr::lag(mean_duration), 
           avgDurationHeatwave_prev = replace_na(avgDurationHeatwave_prev, 0)) |>
    mutate(abundance_prev = dplyr::lag(Abundance)) |> 
    dplyr::select(!precipEvents & !heatwaves & !Abundance & !yearlyTotalPrecip & !yearlyMeanTemp & !daysExceeded_precip & !daysExceeded & !mean_duration_precip & !mean_duration) |> 
    filter(year > 1980)

biglm <- lm(growth_rate ~ heatwaves_prev + avgTemp_prev + daysExceededHeatwaves_prev + avgDurationHeatwave_prev + 
                precip_prev + totalPrecip_prev + daysExceededPrecip_prev + avgDurationPrecip_prev + abundance_prev, data = bigRegression)

summary(biglm)

pairs(bigRegression[,c("growth_rate", "heatwaves_prev", "avgTemp_prev", "daysExceededHeatwaves_prev", "avgDurationHeatwave_prev", 
                   "precip_prev", "totalPrecip_prev", "daysExceededPrecip_prev", "avgDurationPrecip_prev")], 
      panel = panel.smooth)

install.packages("car")
library(car)
vif(biglm)

#There is obviously alot of correlation between total number of days exceeding a given threshold and the number of events 
#Need to look at just using one variable for extreme events 

#I am making the executive decision to use a 99% threshold for precipitaiton and a 95% threshold for heatwaves and simply 
#using the number of days that exceed this threshold as the predictor variable for extreme events 

#heatwaves -- 
heatwaveRegression <- growthRates |> 
    left_join(yearlyTempAvg, by = "year") |>
    left_join(thrs95exceeded_heatwaves, by = "year") |>
    mutate(heatwaves_prev = dplyr::lag(daysExceeded)) |>
    mutate(avgTemp_prev = dplyr::lag(yearlyMeanTemp)) |>
    mutate(abundance_prev = dplyr::lag(Abundance)) |>  
    dplyr::select(!daysExceeded & !yearlyMeanTemp & !Abundance) |>
    filter(year > 1980)

heatwavelm <- lm(growth_rate ~ heatwaves_prev + avgTemp_prev + abundance_prev, data = heatwaveRegression)
heatwaveModel <- summary(heatwavelm)
write_csv(heatwaveRegression, "results/heatwaveRegression.csv") 
capture.output(heatwaveModel, file = "results/heatwavelm.txt")

predictors <- all.vars(formula(heatwavelm))[-1]

plots <- map(predictors, ~ {
    ggplot(heatwaveRegression, aes_string(x = .x, y = "growth_rate")) +
        geom_point() +
        geom_smooth(method = "lm") +
        labs(x = .x, y = "log(growth rate)", title = paste("Effect of", .x, "on growth rate"))
})

heatwavePlots <- plot_grid(plots[[1]], plots[[2]], plots[[3]], nrow = 3, labels = c("A", "B", "C"))
ggsave(filename = "plots/results/heatwaves.pdf", plot = heatwavePlots)

#precipitation -- 
precipRegression <- growthRates |> 
    left_join(yearlyPrecipSum, by = "year") |>
    left_join(thrs99exceeded_precip, by = "year") |>
    mutate(precip_prev = dplyr::lag(daysExceeded_precip)) |>
    mutate(totalPrecip_prev = dplyr::lag(yearlyTotalPrecip)) |>
    mutate(abundance_prev = dplyr::lag(Abundance)) |>  
    dplyr::select(!daysExceeded_precip & !yearlyTotalPrecip & !Abundance) |>
    filter(year > 1980)

preciplm <- lm(growth_rate ~ precip_prev + totalPrecip_prev + abundance_prev, data = precipRegression)
precipModel <- summary(preciplm)
write_csv(precipRegression, "results/precipRegression.csv")
capture.output(precipModel, file = "results/preciplm.txt")

predictors <- all.vars(formula(preciplm))[-1]

plots <- map(predictors, ~ {
    ggplot(precipRegression, aes_string(x = .x, y = "growth_rate")) +
        geom_point() +
        geom_smooth(method = "lm") +
        labs(x = .x, y = "log(growth rate)", title = paste("Effect of", .x, "on growth rate"))
})

precipPlots <- plot_grid(plots[[1]], plots[[2]], plots[[3]], nrow = 3, labels = c("A", "B", "C"))
ggsave(filename = "plots/results/precipitation.pdf", plot = precipPlots)

#combination -- 
combinedRegression <- growthRates |> 
    left_join(yearlyTempAvg, by = "year") |>
    left_join(yearlyPrecipSum, by = "year") |>
    left_join(thrs95exceeded_heatwaves, by = "year") |>
    left_join(thrs99exceeded_precip, by = "year") |>
    mutate(heatwaves_prev = dplyr::lag(daysExceeded)) |>
    mutate(avgTemp_prev = dplyr::lag(yearlyMeanTemp)) |>
    mutate(precip_prev = dplyr::lag(daysExceeded_precip)) |>
    mutate(totalPrecip_prev = dplyr::lag(yearlyTotalPrecip)) |>
    mutate(abundance_prev = dplyr::lag(Abundance)) |>  
    dplyr::select(!daysExceeded & !daysExceeded_precip & !yearlyMeanTemp & !yearlyTotalPrecip & !Abundance) |>
    filter(year > 1980)

combinedlm <- lm(growth_rate ~ heatwaves_prev + avgTemp_prev + precip_prev + totalPrecip_prev + abundance_prev, data = combinedRegression)
combinedModel <- summary(combinedlm)
write_csv(combinedRegression, "results/combinedRegression.csv")
capture.output(combinedModel, file = "results/combinedlm.txt")

example <- res1a |> 
    filter(str_detect(t, "2018"))


##2/05 - Final Analysis/Models ---- 
#Add year as an extra fixed effect term for the three models above 
install.packages("fixest")
library(fixest)

#Redefine growth rate to include log_value term
growthRates <- salmoDataTable |> 
    mutate(log_value = log10(Abundance + 1),                    #+1 to minimise error if population = 0
           growth_rate = log_value - dplyr::lag(log_value))  

#Now redefine regression models from above 
##heatwaves --
heatwaveRegression <- growthRates |> 
    left_join(yearlyTempAvg, by = "year") |>
    left_join(thrs95exceeded_heatwaves, by = "year") |>
    mutate(heatwaves_prev = dplyr::lag(daysExceeded)) |>
    mutate(avgTemp_prev = dplyr::lag(yearlyMeanTemp)) |>
    mutate(logval_prev = dplyr::lag(log_value)) |>  
    dplyr::select(!daysExceeded & !yearlyMeanTemp & !Abundance &!log_value) |>
    filter(year > 1980)

heatwavelm <- lm(growth_rate ~ logval_prev + avgTemp_prev + heatwaves_prev + year, data = heatwaveRegression)
heatwaveModel <- summary(heatwavelm)
write_csv(heatwaveRegression, "data/heatwaveRegression.csv") 
capture.output(heatwaveModel, file = "results/heatwavelm.txt")

predictors <- all.vars(formula(heatwavelm))[-1]

plots <- map(predictors, ~ {
    ggplot(heatwaveRegression, aes_string(x = .x, y = "growth_rate")) +
        geom_point() +
        geom_smooth(method = "lm") +
        labs(x = .x, y = "log(growth rate)", title = paste("Effect of", .x, "on growth rate"))
})

heatwavePlots <- plot_grid(plots[[1]], plots[[2]], plots[[3]], plots[[4]], nrow = 2, ncol = 2, labels = c("A", "B", "C", "D"))
ggsave(filename = "plots/results/heatwaves.pdf", plot = heatwavePlots)

#precipitation -- 
precipRegression <- growthRates |> 
    left_join(yearlyPrecipSum, by = "year") |>
    left_join(thrs95exceeded_precip, by = "year") |>
    mutate(precip_prev = dplyr::lag(daysExceeded_precip)) |>
    mutate(totalPrecip_prev = dplyr::lag(yearlyTotalPrecip)) |>
    mutate(logval_prev = dplyr::lag(log_value)) |>  
    dplyr::select(!daysExceeded_precip & !yearlyTotalPrecip & !Abundance &!log_value) |>
    filter(year > 1980)

preciplm <- lm(growth_rate ~ logval_prev + totalPrecip_prev + precip_prev + year, data = precipRegression)
precipModel <- summary(preciplm)
write_csv(precipRegression, "data/precipRegression.csv")
capture.output(precipModel, file = "results/preciplm.txt")

predictors <- all.vars(formula(preciplm))[-1]

plots <- map(predictors, ~ {
    ggplot(precipRegression, aes_string(x = .x, y = "growth_rate")) +
        geom_point() +
        geom_smooth(method = "lm") +
        labs(x = .x, y = "log(growth rate)", title = paste("Effect of", .x, "on growth rate"))
})

precipPlots <- plot_grid(plots[[1]], plots[[2]], plots[[3]], plots[[4]], nrow = 2, ncol = 2, labels = c("A", "B", "C", "D"))
ggsave(filename = "plots/results/precipitation.pdf", plot = precipPlots)

#combination -- 
combinedRegression <- growthRates |> 
    left_join(yearlyTempAvg, by = "year") |>
    left_join(yearlyPrecipSum, by = "year") |>
    left_join(thrs95exceeded_heatwaves, by = "year") |>
    left_join(thrs95exceeded_precip, by = "year") |>
    mutate(heatwaves_prev = dplyr::lag(daysExceeded)) |>
    mutate(avgTemp_prev = dplyr::lag(yearlyMeanTemp)) |>
    mutate(precip_prev = dplyr::lag(daysExceeded_precip)) |>
    mutate(totalPrecip_prev = dplyr::lag(yearlyTotalPrecip)) |>
    mutate(logval_prev = dplyr::lag(log_value)) |>  
    dplyr::select(!daysExceeded & !daysExceeded_precip & !yearlyMeanTemp & !yearlyTotalPrecip & !Abundance & !log_value) |>
    filter(year > 1980)

combinedlm <- lm(growth_rate ~  logval_prev + avgTemp_prev + totalPrecip_prev + heatwaves_prev + precip_prev + year, data = combinedRegression)
combinedModel <- summary(combinedlm)
write_csv(combinedRegression, "data/combinedRegression.csv")
capture.output(combinedModel, file = "results/combinedlm.txt")


##2/05 - Extension: 10 populations of salmo trutta at different latitudes ---- 
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
    
write.csv(salmotruttaData, file = "data/europeTimeSeriesData.csv", row.names = FALSE)


##5/05 - Europe climate analysis ----
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
df_results1 <- read.csv("data/new_climate_data/europeData1_coords.csv")

results1 <- vector("list", length = 10)

for (i in 0:9) {
  # Column suffix: "" for site 1, ".1" for site 2, ..., ".9" for site 10
  suffix <- if (i == 0) "" else paste0(".", i)
  
  results1[[i + 1]] <- df_results1 |>
    dplyr::select(
      !!paste0("Longitude", suffix),
      !!paste0("Latitude", suffix),
      !!paste0("DateTime", suffix),
      !!paste0("Temperature", suffix),
      !!paste0("Precipitation", suffix)
    ) |>
    dplyr::rename(
      Longitude = !!paste0("Longitude", suffix),
      Latitude = !!paste0("Latitude", suffix),
      DateTime = !!paste0("DateTime", suffix),
      Temperature = !!paste0("Temperature", suffix),
      Precipitation = !!paste0("Precipitation", suffix)
    ) |>
    dplyr::mutate(SiteID = i + 1)
}

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
df_results2 <- read.csv("data/new_climate_data/europeData2_coords.csv")

results2 <- vector("list", length = 10)

for (i in 0:9) {
  # Column suffix: "" for site 1, ".1" for site 2, ..., ".9" for site 10
  suffix <- if (i == 0) "" else paste0(".", i)
  
  results2[[i + 1]] <- df_results2 |>
    dplyr::select(
      !!paste0("Longitude", suffix),
      !!paste0("Latitude", suffix),
      !!paste0("DateTime", suffix),
      !!paste0("Temperature", suffix),
      !!paste0("Precipitation", suffix)
    ) |>
    dplyr::rename(
      Longitude = !!paste0("Longitude", suffix),
      Latitude = !!paste0("Latitude", suffix),
      DateTime = !!paste0("DateTime", suffix),
      Temperature = !!paste0("Temperature", suffix),
      Precipitation = !!paste0("Precipitation", suffix)
    ) |>
    dplyr::mutate(SiteID = i + 1)
}

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
df_results3 <- read.csv("data/new_climate_data/europeData3_coords.csv")

results3 <- vector("list", length = 10)

for (i in 0:9) {
  # Column suffix: "" for site 1, ".1" for site 2, ..., ".9" for site 10
  suffix <- if (i == 0) "" else paste0(".", i)
  
  results3[[i + 1]] <- df_results3 |>
    dplyr::select(
      !!paste0("Longitude", suffix),
      !!paste0("Latitude", suffix),
      !!paste0("DateTime", suffix),
      !!paste0("Temperature", suffix),
      !!paste0("Precipitation", suffix)
    ) |>
    dplyr::rename(
      Longitude = !!paste0("Longitude", suffix),
      Latitude = !!paste0("Latitude", suffix),
      DateTime = !!paste0("DateTime", suffix),
      Temperature = !!paste0("Temperature", suffix),
      Precipitation = !!paste0("Precipitation", suffix)
    ) |>
    dplyr::mutate(SiteID = i + 1)
}

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
df_results4 <- read.csv("data/new_climate_data/europeData4_coords.csv")

results4 <- vector("list", length = 10)

for (i in 0:9) {
  # Column suffix: "" for site 1, ".1" for site 2, ..., ".9" for site 10
  suffix <- if (i == 0) "" else paste0(".", i)
  
  results4[[i + 1]] <- df_results4 |>
    dplyr::select(
      !!paste0("Longitude", suffix),
      !!paste0("Latitude", suffix),
      !!paste0("DateTime", suffix),
      !!paste0("Temperature", suffix),
      !!paste0("Precipitation", suffix)
    ) |>
    dplyr::rename(
      Longitude = !!paste0("Longitude", suffix),
      Latitude = !!paste0("Latitude", suffix),
      DateTime = !!paste0("DateTime", suffix),
      Temperature = !!paste0("Temperature", suffix),
      Precipitation = !!paste0("Precipitation", suffix)
    ) |>
    dplyr::mutate(SiteID = i + 1)
}

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
df_results5 <- read.csv("data/new_climate_data/europeData5_coords.csv")

results5 <- vector("list", length = 10)

for (i in 0:9) {
  # Column suffix: "" for site 1, ".1" for site 2, ..., ".9" for site 10
  suffix <- if (i == 0) "" else paste0(".", i)
  
  results5[[i + 1]] <- df_results5 |>
    dplyr::select(
      !!paste0("Longitude", suffix),
      !!paste0("Latitude", suffix),
      !!paste0("DateTime", suffix),
      !!paste0("Temperature", suffix),
      !!paste0("Precipitation", suffix)
    ) |>
    dplyr::rename(
      Longitude = !!paste0("Longitude", suffix),
      Latitude = !!paste0("Latitude", suffix),
      DateTime = !!paste0("DateTime", suffix),
      Temperature = !!paste0("Temperature", suffix),
      Precipitation = !!paste0("Precipitation", suffix)
    ) |>
    dplyr::mutate(SiteID = i + 1)
}

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
df_results6 <- read.csv("data/new_climate_data/europeData6_coords.csv")

results6 <- vector("list", length = 10)

for (i in 0:9) {
  # Column suffix: "" for site 1, ".1" for site 2, ..., ".9" for site 10
  suffix <- if (i == 0) "" else paste0(".", i)
  
  results6[[i + 1]] <- df_results6 |>
    dplyr::select(
      !!paste0("Longitude", suffix),
      !!paste0("Latitude", suffix),
      !!paste0("DateTime", suffix),
      !!paste0("Temperature", suffix),
      !!paste0("Precipitation", suffix)
    ) |>
    dplyr::rename(
      Longitude = !!paste0("Longitude", suffix),
      Latitude = !!paste0("Latitude", suffix),
      DateTime = !!paste0("DateTime", suffix),
      Temperature = !!paste0("Temperature", suffix),
      Precipitation = !!paste0("Precipitation", suffix)
    ) |>
    dplyr::mutate(SiteID = i + 1)
}

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
df_results7 <- read.csv("data/new_climate_data/europeData7_coords.csv")

results7 <- vector("list", length = 10)

for (i in 0:9) {
  # Column suffix: "" for site 1, ".1" for site 2, ..., ".9" for site 10
  suffix <- if (i == 0) "" else paste0(".", i)
  
  results7[[i + 1]] <- df_results7 |>
    dplyr::select(
      !!paste0("Longitude", suffix),
      !!paste0("Latitude", suffix),
      !!paste0("DateTime", suffix),
      !!paste0("Temperature", suffix),
      !!paste0("Precipitation", suffix)
    ) |>
    dplyr::rename(
      Longitude = !!paste0("Longitude", suffix),
      Latitude = !!paste0("Latitude", suffix),
      DateTime = !!paste0("DateTime", suffix),
      Temperature = !!paste0("Temperature", suffix),
      Precipitation = !!paste0("Precipitation", suffix)
    ) |>
    dplyr::mutate(SiteID = i + 1)
}

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
df_results8 <- read.csv("data/new_climate_data/europeData8_coords.csv")

results8 <- vector("list", length = 10)

for (i in 0:9) {
  # Column suffix: "" for site 1, ".1" for site 2, ..., ".9" for site 10
  suffix <- if (i == 0) "" else paste0(".", i)
  
  results8[[i + 1]] <- df_results8 |>
    dplyr::select(
      !!paste0("Longitude", suffix),
      !!paste0("Latitude", suffix),
      !!paste0("DateTime", suffix),
      !!paste0("Temperature", suffix),
      !!paste0("Precipitation", suffix)
    ) |>
    dplyr::rename(
      Longitude = !!paste0("Longitude", suffix),
      Latitude = !!paste0("Latitude", suffix),
      DateTime = !!paste0("DateTime", suffix),
      Temperature = !!paste0("Temperature", suffix),
      Precipitation = !!paste0("Precipitation", suffix)
    ) |>
    dplyr::mutate(SiteID = i + 1)
}

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
df_results9 <- read.csv("data/new_climate_data/europeData9_coords.csv")

results9 <- vector("list", length = 10)

for (i in 0:9) {
  # Column suffix: "" for site 1, ".1" for site 2, ..., ".9" for site 10
  suffix <- if (i == 0) "" else paste0(".", i)
  
  results9[[i + 1]] <- df_results9 |>
    dplyr::select(
      !!paste0("Longitude", suffix),
      !!paste0("Latitude", suffix),
      !!paste0("DateTime", suffix),
      !!paste0("Temperature", suffix),
      !!paste0("Precipitation", suffix)
    ) |>
    dplyr::rename(
      Longitude = !!paste0("Longitude", suffix),
      Latitude = !!paste0("Latitude", suffix),
      DateTime = !!paste0("DateTime", suffix),
      Temperature = !!paste0("Temperature", suffix),
      Precipitation = !!paste0("Precipitation", suffix)
    ) |>
    dplyr::mutate(SiteID = i + 1)
}

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
df_results10 <- read.csv("data/new_climate_data/europeData10_coords.csv")

results10 <- vector("list", length = 10)

for (i in 0:9) {
  # Column suffix: "" for site 1, ".1" for site 2, ..., ".9" for site 10
  suffix <- if (i == 0) "" else paste0(".", i)
  
  results10[[i + 1]] <- df_results10 |>
    dplyr::select(
      !!paste0("Longitude", suffix),
      !!paste0("Latitude", suffix),
      !!paste0("DateTime", suffix),
      !!paste0("Temperature", suffix),
      !!paste0("Precipitation", suffix)
    ) |>
    dplyr::rename(
      Longitude = !!paste0("Longitude", suffix),
      Latitude = !!paste0("Latitude", suffix),
      DateTime = !!paste0("DateTime", suffix),
      Temperature = !!paste0("Temperature", suffix),
      Precipitation = !!paste0("Precipitation", suffix)
    ) |>
    dplyr::mutate(SiteID = i + 1)
}

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
df_results11 <- read.csv("data/new_climate_data/europeData11_coords.csv")

results11 <- vector("list", length = 10)

for (i in 0:9) {
  # Column suffix: "" for site 1, ".1" for site 2, ..., ".9" for site 10
  suffix <- if (i == 0) "" else paste0(".", i)
  
  results11[[i + 1]] <- df_results11 |>
    dplyr::select(
      !!paste0("Longitude", suffix),
      !!paste0("Latitude", suffix),
      !!paste0("DateTime", suffix),
      !!paste0("Temperature", suffix),
      !!paste0("Precipitation", suffix)
    ) |>
    dplyr::rename(
      Longitude = !!paste0("Longitude", suffix),
      Latitude = !!paste0("Latitude", suffix),
      DateTime = !!paste0("DateTime", suffix),
      Temperature = !!paste0("Temperature", suffix),
      Precipitation = !!paste0("Precipitation", suffix)
    ) |>
    dplyr::mutate(SiteID = i + 1)
}

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
df_results12 <- read.csv("data/new_climate_data/europeData12_coords.csv")

results12 <- vector("list", length = 10)

for (i in 0:9) {
  # Column suffix: "" for site 1, ".1" for site 2, ..., ".9" for site 10
  suffix <- if (i == 0) "" else paste0(".", i)
  
  results12[[i + 1]] <- df_results12 |>
    dplyr::select(
      !!paste0("Longitude", suffix),
      !!paste0("Latitude", suffix),
      !!paste0("DateTime", suffix),
      !!paste0("Temperature", suffix),
      !!paste0("Precipitation", suffix)
    ) |>
    dplyr::rename(
      Longitude = !!paste0("Longitude", suffix),
      Latitude = !!paste0("Latitude", suffix),
      DateTime = !!paste0("DateTime", suffix),
      Temperature = !!paste0("Temperature", suffix),
      Precipitation = !!paste0("Precipitation", suffix)
    ) |>
    dplyr::mutate(SiteID = i + 1)
}

#Now that I have all of the data imported, I need to calculate daily average temperatures and total daily precipitation 

combinedData <- bind_rows(results1, results2, results3, results4, results5, results6, results7, results8, results9, results10, results11, results12)

europeData <- combinedData |> 
  separate(DateTime, into = c("Date", "Time"), sep = " ") |> 
  group_by(SiteID, Date, Latitude, Longitude) |> 
  summarise(
    Temp = max(Temperature, na.rm = TRUE),
    AvgTemp = mean(Temperature, na.rm = TRUE),
    Precip = sum(Precipitation, na.rm = TRUE),
    .groups = "drop") |> 
  rename(t = Date)

#save the table as a file for future use 
write.csv(europeData, file = "data/new_climate_data/europeData.csv", row.names = FALSE)

europeData <- read_csv("data/new_climate_data/europeData.csv")

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
rm(ls = "df_results1", "df_results2", "df_results3", "df_results4", "df_results5", "df_results6", "df_results7", "df_results8", "df_results9", "df_results10", "df_results11", "df_results12")

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
salmotruttaData <- salmotruttaData |>
  group_by(TimeSeriesID, Year) |>
  slice_max(order_by = Abundance, n = 1, with_ties = FALSE) |>
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
  mutate(log_value = log(Abundance + 1), 
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
           extPrecip_prev = lag(precip_exceeded))|> 
    select(TimeSeriesID, Year, Latitude, Longitude, Country, growth_rate, logval_prev, temp_prev, precip_prev, extTemp_prev, extPrecip_prev) |> 
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

panel2 <- feols(growth_rate ~ logval_prev + temp_prev + precip_prev + extTemp_prev + extPrecip_prev | TimeSeriesID, data = pdat, panel.id=~TimeSeriesID+Year)
summary(panel2)

predicts <- all.vars(formula(panel2))[-1]

plots <- map(predicts, ~ {
    ggplot(pdat, aes_string(x = .x, y = "growth_rate")) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = TRUE, color = "blue") +
        labs(x = .x, y = "log(growth rate)", title = paste("Effect of", .x, "on growth rate")) +
        theme_minimal()
})

panel3 <- feols(growth_rate ~ logval_prev + temp_prev + precip_prev + extTemp_prev + extPrecip_prev + extTemp_prev:Latitude | TimeSeriesID, data = pdat, panel.id=~TimeSeriesID+Year)
summary(panel3)

predicts <- all.vars(formula(panel3))[-1]

plots <- map(predicts, ~ {
    ggplot(pdat, aes_string(x = .x, y = "growth_rate")) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = TRUE, color = "blue") +
        labs(x = .x, y = "log(growth rate)", title = paste("Effect of", .x, "on growth rate")) +
        theme_minimal()
})

panel4 <- feols(growth_rate ~ logval_prev + temp_prev + precip_prev + extTemp_prev + extPrecip_prev + extTemp_prev:Latitude + extTemp_prev:extPrecip_prev | TimeSeriesID, data = pdat, panel.id=~TimeSeriesID+Year)
summary(panel4)

AIC(panel1, panel2, panel3, panel4)

##check 99% threshold
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
           extPrecip_prev = lag(precip_exceeded)) |> 
    select(TimeSeriesID, Year, Latitude, Longitude, Country, growth_rate, logval_prev, temp_prev, precip_prev, extTemp_prev, extPrecip_prev) |> 
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

panel2_99 <- feols(growth_rate ~ logval_prev + temp_prev + precip_prev + extTemp_prev + extPrecip_prev | TimeSeriesID, data = pdat2, panel.id=~TimeSeriesID+Year)
summary(panel2_99)

predicts <- all.vars(formula(panel2_99))[-1]

plots <- map(predicts, ~ {
    ggplot(pdat2, aes_string(x = .x, y = "growth_rate")) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = TRUE, color = "blue") +
        labs(x = .x, y = "log(growth rate)", title = paste("Effect of", .x, "on growth rate")) +
        theme_minimal()
})

panel3_99 <- feols(growth_rate ~ logval_prev + temp_prev + precip_prev + extTemp_prev + extPrecip_prev + extTemp_prev:Latitude | TimeSeriesID, data = pdat2, panel.id=~TimeSeriesID+Year)
summary(panel3_99)

predicts <- all.vars(formula(panel3_99))[-1]

plots <- map(predicts, ~ {
    ggplot(pdat2, aes_string(x = .x, y = "growth_rate")) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = TRUE, color = "blue") +
        labs(x = .x, y = "log(growth rate)", title = paste("Effect of", .x, "on growth rate")) +
        theme_minimal()
})

panel4_99 <- feols(growth_rate ~ logval_prev + temp_prev + precip_prev + extTemp_prev + extPrecip_prev + extTemp_prev:Latitude + extTemp_prev:extPrecip_prev | TimeSeriesID, data = pdat2, panel.id=~TimeSeriesID+Year)
summary(panel4_99)

AIC(panel1_99, panel2_99, panel3_99, panel4_99)

#in the case of the 99% threshold, panel1_99 is the best which means there is not a big effect of 
#extreme events above the 99% threshold on these 10 populations 

write.csv(pdat, file = "data/new_climate_data/pdat.csv", row.names = FALSE)
write.csv(pdat2, file = "data/new_climate_data/pdat2.csv", row.names = FALSE)
capture.output(panel1, file = "results/panel1.txt")
capture.output(panel2, file = "results/panel2.txt")
capture.output(panel3, file = "results/panel3.txt")
capture.output(panel4, file = "results/panel4.txt")
capture.output(panel1_99, file = "results/panel1_99.txt")
capture.output(panel2_99, file = "results/panel2_99.txt")
capture.output(panel3_99, file = "results/panel3_99.txt")
capture.output(panel4_99, file = "results/panel4_99.txt")


##7/05 - Finalising analysis ---- 

#add extreme event variable in terms of intensity 

#95% threshold, min 3 days, avg intensity - temperature
events_temp <- data.table()

for (i in 1:10) {
  data <- filter(europeData, SiteID == i)
  output <- ts2clm(data = data, x = t, y = Temp, climatologyPeriod = c("1950-01-01", "1970-12-31"), pctile = 95) #20 year baseline period
  out <- detect_event(output, x = t, y = Temp, seasClim = seas, threshClim = thresh, minDuration = 3)
  heatwaves <- block_average(out, x = t, y = Temp)

  heatwaves$heatwave_count <- heatwaves$count
  heatwaves$heatwave_duration <- heatwaves$duration 
  heatwaves$heatwave_intensity_mean <- heatwaves$intensity_mean
  heatwaves$Year <- heatwaves$year

  heatwaves$SiteID <- i 

  heatwaves <- select(heatwaves, SiteID, Year, heatwave_count, heatwave_duration, heatwave_intensity_mean)

  events_temp <- bind_rows(events_temp, heatwaves)
}

#95% threshold, min 1 day, avg intensity - precipitation
events_precip <- data.table()

for (i in 1:10) {
  data <- filter(europeData, SiteID == i)
  output <- ts2clm(data = data, x = t, y = Precip, climatologyPeriod = c("1950-01-01", "1970-12-31"), pctile = 95) #20 year baseline period
  out <- detect_event(output, x = t, y = Precip, seasClim = seas, threshClim = thresh, minDuration = 1)
  precips <- block_average(out, x = t, y = Precip)

  precips$precip_count <- precips$count
  precips$precip_duration <- precips$duration
  precips$precip_intensity_mean <- precips$intensity_mean
  precips$SiteID <- i
  precips$Year <- precips$year

  precips <- select(precips, SiteID, Year, precip_count, precip_duration, precip_intensity_mean)

  events_precip <- bind_rows(events_precip, precips)
}

#average temperature
avgYear_europeData <- europeData |> 
  group_by(SiteID) |> 
  separate(t, c("Year", "Month", "Day"), sep = "-") |>
  group_by(SiteID, Year) |>
  summarise(AvgTemp = mean(Temp, na.rm = TRUE), 
            TotalPrecip = sum(Precip, na.rm = TRUE), .groups = "drop")

avgYear_europeData$Year <- as.numeric(avgYear_europeData$Year)

#join tables together 
events <- events_temp |> 
  left_join(events_precip, by = c("SiteID", "Year")) |> 
  left_join(avgYear_europeData, by = c("SiteID", "Year")) |> 
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

#join with growth rates to create regression model 
pdat3 <- data.table()

panelData3 <- growthRates |> 
  left_join(events, by = c("TimeSeriesID", "Year")) 

for (i in seq_along(longest_ids)) {
  data <- filter(panelData3, TimeSeriesID == longest_ids[i])
  output <- data |> 
    mutate(logval_prev = lag(log_value),
           temp_prev = lag(AvgTemp),
           precip_prev = lag(TotalPrecip),
           temp_count_prev = lag(heatwave_count), 
           precip_count_prev = lag(precip_count),
           extTempInt_prev = lag(heatwave_count * heatwave_intensity_mean),
           extPrecipInt_prev = lag(precip_count * precip_intensity_mean)) |> 
    select(TimeSeriesID, Year, Latitude, Longitude, Country, growth_rate, logval_prev, temp_prev, precip_prev, temp_count_prev, precip_count_prev, extTempInt_prev, extPrecipInt_prev) |> 
    filter(!is.na(growth_rate))

pdat3 <- bind_rows(pdat3, output)

}

#regression models 
panel1_int <- feols(growth_rate ~ logval_prev + temp_prev + precip_prev | TimeSeriesID, data = pdat3, panel.id=~TimeSeriesID+Year)
summary(panel1_99)

panel2_int <- feols(growth_rate ~ logval_prev + temp_prev + precip_prev + temp_count_prev + precip_count_prev | TimeSeriesID, data = pdat3, panel.id=~TimeSeriesID+Year)
summary(panel2_int)

predicts <- all.vars(formula(panel2_int))[-1]

plots <- map(predicts, ~ {
    ggplot(pdat3, aes_string(x = .x, y = "growth_rate")) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = TRUE, color = "blue") +
        labs(x = .x, y = "log(growth rate)", title = paste("Effect of", .x, "on growth rate")) +
        theme_minimal()
})

panel3_int <- feols(growth_rate ~ logval_prev + temp_prev + precip_prev + extTempInt_prev + extPrecipInt_prev | TimeSeriesID, data = pdat3, panel.id=~TimeSeriesID+Year)
summary(panel3_int)

predicts <- all.vars(formula(panel3_int))[-1]

plots <- map(predicts, ~ {
    ggplot(pdat3, aes_string(x = .x, y = "growth_rate")) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = TRUE, color = "blue") +
        labs(x = .x, y = "log(growth rate)", title = paste("Effect of", .x, "on growth rate")) +
        theme_minimal()
})

panel4_int <- feols(growth_rate ~ logval_prev + temp_prev + precip_prev + extTempInt_prev + extPrecipInt_prev + extTempInt_prev:extPrecipInt_prev | TimeSeriesID, data = pdat3, panel.id=~TimeSeriesID+Year)
summary(panel4_int)

predicts <- all.vars(formula(panel4_int))[-1]

plots <- map(predicts, ~ {
    ggplot(pdat3, aes_string(x = .x, y = "growth_rate")) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = TRUE, color = "blue") +
        labs(x = .x, y = "log(growth rate)", title = paste("Effect of", .x, "on growth rate")) +
        theme_minimal()
})

AIC(panel1_int, panel2_int)
AIC(panel3_int, panel4_int)
