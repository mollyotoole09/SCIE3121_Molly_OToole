##upload studies with salmo trutta records >5 years 

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

##now, create a table with the abundance data for salmo trutta in each of the given studies
#include latitude so that it can be used as a fixed effect in the model 

salmotruttaData <- fishData |> 
    left_join(RivFishStudies, by = "TimeSeriesID") |>
    dplyr::select(TimeSeriesID, Year, Quarter, Species, Abundance, UnitAbundance, Latitude, Longitude, Country) |> 
    filter(Species == "Salmo trutta", TimeSeriesID %in% c("G8647", "G8637", "G7358", "G7343", "G2961", "G10556", "G11473", "G404", "G9542", "G10191")) |> 
    group_by(TimeSeriesID, Year)
    
