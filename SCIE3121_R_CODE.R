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

simplelm <- lm(growth_rate ~ meanTemp_prev * totalPrecip_prev * abundance_prev, data = simpleRegression)
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


## 30/04 - New Population analysis ---- 
#Different species in same location - how do different species respond to the same climates 
roachData <- filter(fishData, TimeSeriesID == "G11473" & Species == "Rutilus rutilus")

roachDataTable <- roachData |> 
    dplyr::select(!TimeSeriesID & !Species & !SurveyID) |>
    group_by(Year) |>
    rename(year = Year) |> 
    summarise(Abundance = max(Abundance)) |> 
    complete(year = full_seq(year, 1), fill = list(Abundance = 0))

plot(roachDataTable$year, roachDataTable$Abundance, type = "b", pch = 19, col = "blue", xlab = "year", ylab = "abundance")

#salmo trutta in a different location - warmer waters? might be more susceptible to the effects 
#of heatwaves here

franceTrout <- filter(fishData, TimeSeriesID == "G7343" & Species == "Salmo trutta")

troutDataTable <- franceTrout |> 
    dplyr::select(!TimeSeriesID & !Species & !SurveyID) |>
    group_by(Year) |>
    rename(year = Year) |> 
    summarise(Abundance = max(Abundance)) 

plot(troutDataTable$year, troutDataTable$Abundance, type = "b", pch = 19, col = "blue", xlab = "year", ylab = "abundance")
