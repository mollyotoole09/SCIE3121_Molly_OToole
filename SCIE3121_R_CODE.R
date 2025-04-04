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


##4/03 - Second Trial ---- 
library(data.table)
library("tidyverse")
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


##18/03 - RivFishTIME dataset ---- 
fishData <- read.csv("data/RivFishTIME_DATA.csv")

speciesFishData <- table(fishData$Species)

##Popn 1 - Salmo trutta - 39 consecutive years
G11473A_SalmoDat <- filter(fishData, TimeSeriesID == "G11473" & Year > 1979 & Species == "Salmo trutta")

G11473A_SalmoDat <- select(G11473A_SalmoDat, !TimeSeriesID & !SurveyID & !Species)

salmoDataTable <- data.table(G11473A_SalmoDat) |> 
    group_by(Year) |> 
    summarise(Abunance = mean(Abundance))

pdf("plots/Salmo_trutta.pdf", width = 6, height = 5)
plot(salmoDataTable$Year, salmoDataTable$Abunance, type = "l", xlab = "time", ylab = "abundance")
title(main = "Salmo trutta - G11473")
dev.off()


##19/03 -----
##Popn 2 - Salmo trutta - 36 consecutive years
G10988A_SalmoDat <- filter(fishData, TimeSeriesID == "G10988" & Year > 1982 & Species == "Salmo trutta")

G10988A_SalmoDat <- select(G10988A_SalmoDat, !TimeSeriesID & !SurveyID & !Species)

salmoDataTable2 <- data.table(G10988A_SalmoDat) |> 
    group_by(Year) |> 
    summarise(Abunance = mean(Abundance))

pdf("plots/Salmo_trutta_2.pdf", width = 6, height = 5)
plot(salmoDataTable2$Year, salmoDataTable2$Abunance, type = "l", xlab = "time", ylab = "abundance")
title(main = "Salmo trutta - G10988")
dev.off()

##Popn 3 - Salmo salar - 38 consecutive years
G10538A_SalmoDat <- filter(fishData, TimeSeriesID == "G10538" & Year > 1969 & Year < 2008 & Species == "Salmo salar")

G10538A_SalmoDat <- select(G10538A_SalmoDat, !TimeSeriesID & !SurveyID & !Species)

salmoDataTable3 <- data.table(G10538A_SalmoDat) |> 
    group_by(Year) |> 
    summarise(Abunance = mean(Abundance))

pdf("plots/Salmo_salar.pdf", width = 6, height = 5)
plot(salmoDataTable3$Year, salmoDataTable3$Abunance, type = "l", xlab = "time", ylab = "abundance")
title(main = "Salmo salar - G10538")
dev.off()

#Popn 4 - Salmo salar - 36 consecutive years
G10120A_SalmoDat <- filter(fishData, TimeSeriesID == "G10120" & Year > 1982 & Species == "Salmo salar")

G10120A_SalmoDat <- select(G10120_SalmoDat, !TimeSeriesID & !SurveyID & !Species)

salmoDataTable4 <- data.table(G10120A_SalmoDat) |> 
    group_by(Year) |> 
    summarise(Abunance = mean(Abundance))

pdf("plots/Salmo_salar_2.pdf", width = 6, height = 5)
plot(salmoDataTable4$Year, salmoDataTable4$Abunance, type = "l", xlab = "time", ylab = "abundance")
title(main = "Salmo salar - G10120")
dev.off()


##2/04 - Climate data ---- 

library(readxl)
library(tidyverse)
library(patchwork)

##ERA data - CCKP

backgroundWeather <- read_excel("data/sweden_background_temp.xlsx", col_names = TRUE)

tempDat <- backgroundWeather |>
  pivot_longer(cols = everything(), names_to = "Date", values_to = "Temperature") |> 
  separate(Date, c("Year", "Month"), sep = "-") |> 
  group_by(Month) |> 
  filter(!is.na(Year) & !is.na(Month)) |> 
  summarise(mean_monthly_temp = mean(Temperature)) 

ggplot(tempDat) + 
    geom_point(aes(Month, mean_monthly_temp))


##CRU data for mean, max and min monthly temp - CCKP

cruMonthlyData <- read_excel("data/monthly_cru_data.xlsx", col_names = TRUE)

baselineCruDat <- cruMonthlyData |> 
    pivot_longer(!name, names_to = "Date", values_to = "Temperature") |> 
    separate(Date, c("Year", "Month"), sep = "-") |> 
    group_by(Month, name) |> 
    summarise(avg_temp = mean(Temperature))


tempPlot <- ggplot(baselineCruDat) + 
                geom_line(mapping = aes(Month, avg_temp, colour = name, group = name)) + 
                theme_linedraw()

ggsave(filename = "plots/temp_baseline_plot.pdf", plot = tempPlot, width = 5, height = 6)




###TESTS ----

##ERA5 data - GRIB

install.packages("terra")   # For handling raster data
install.packages("ncdf4")   # For NetCDF support (sometimes needed)
library(ncdf4)
library(terra)

era5dat <- terra::rast("data/era5Dat.grib")

print(era5dat)
names(era5dat)
terra::ext(era5dat)
terra::crs(era5dat)

point <- data.frame(x = 11.44062, y = 58.90116)
temp_value <- terra::extract(era5dat, point)
print(temp_value)

df <- as.data.frame(era5dat, xy = TRUE)
head(df)

time(era5dat)


##ERA5 data - nc

era5datNC <- terra::rast("data/era5Dat.nc")
print(era5datNC)
