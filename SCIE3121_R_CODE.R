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
G11473A_SalmoDat <- filter(fishData, TimeSeriesID == "G11473" & Year > 1979 & Species == "Salmo trutta")

G11473A_SalmoDat <- select(G11473A_SalmoDat, !TimeSeriesID & !SurveyID & !Species)

salmoDataTable <- data.table(G11473A_SalmoDat) |> 
    group_by(Year) |> 
    summarise(Abunance = mean(Abundance))

pdf("plots/Salmo_trutta.pdf", width = 6, height = 5)
plot(salmoDataTable$Year, salmoDataTable$Abunance, type = "l", xlab = "time", ylab = "abundance")
dev.off()