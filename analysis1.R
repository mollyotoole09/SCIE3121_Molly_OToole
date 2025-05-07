library(ggplot2)
library(cowplot)
library(tidyverse)
library(fixest)

#regression 1: heatwaves 
heatwaveRegression <- read_csv("results/heatwaveRegression.csv")
heatwavelm <- feols(growth_rate ~ logval_prev + avgTemp_prev + heatwaves_prev + year, data = heatwaveRegression)
summary(heatwavelm)

predictors <- all.vars(formula(heatwavelm))[-1]

plots <- map(predictors, ~ {
    ggplot(heatwaveRegression, aes_string(x = .x, y = "growth_rate")) +
        geom_point() +
        geom_smooth(method = "lm") +
        labs(x = .x, y = "log(growth rate)", title = paste("Effect of", .x, "on growth rate"))
})

heatwavePlots <- plot_grid(plots[[1]], plots[[2]], plots[[3]], plots[[4]], nrow = 2, ncol = 2, labels = c("A", "B", "C", "D"))
heatwavePlots

#regression 2: precipitation
precipRegression <- read_csv("results/precipRegression.csv")
preciplm <- feols(growth_rate ~ logval_prev + totalPrecip_prev + precip_prev + year, data = precipRegression)
summary(preciplm)

predictors <- all.vars(formula(preciplm))[-1]

plots <- map(predictors, ~ {
    ggplot(precipRegression, aes_string(x = .x, y = "growth_rate")) +
        geom_point() +
        geom_smooth(method = "lm") +
        labs(x = .x, y = "log(growth rate)", title = paste("Effect of", .x, "on growth rate"))
})

precipPlots <- plot_grid(plots[[1]], plots[[2]], plots[[3]], plots[[4]], nrow = 2, ncol = 2, labels = c("A", "B", "C", "D"))
precipPlots

#regression 3: combination 1 - avg yearly temperature and total yearly precipitation
combinedRegression <- read_csv("results/combinedRegression.csv")
combinedlm1 <- feols(growth_rate ~ logval_prev + avgTemp_prev + totalPrecip_prev + year, data = combinedRegression)
summary(combinedlm1)

#regression 4: combination 2 - extreme temperature and precipitation events
combinedlm2 <- feols(growth_rate ~ logval_prev + heatwaves_prev + precip_prev + year, data = combinedRegression)
summary(combinedlm2)

#regression 5: combination 3 - avg yearly temperature, total yearly precipitation, number of days exceeding 
#95% heatwave threshold and number of days exceeding 95% precipitation threshold
combinedlm3 <- feols(growth_rate ~ logval_prev + avgTemp_prev + totalPrecip_prev + heatwaves_prev + precip_prev + year, data = combinedRegression)
summary(combinedlm3)

predictors <- all.vars(formula(combinedlm3))[-1]

plots <- map(predictors, ~ {
    ggplot(combinedRegression, aes_string(x = .x, y = "growth_rate")) +
        geom_point() +
        geom_smooth(method = "lm") +
        labs(x = .x, y = "log(growth rate)", title = paste("Effect of", .x, "on growth rate"))
})

combinedRegressionPlots <- plot_grid(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]], nrow = 3, ncol = 2, labels = c("A", "B", "C", "D", "E", "F"))
combinedRegressionPlots


#Description of variables: 

#growth_rate: growth rate of the species - log(abundance in current year) - log(abundance in previous year)
#logval_prev: log of abundance in the previous year 
#avgTemp_prev: average temperature recorded in the previous year
#totalPrecip_prev: total precipitation recorded in the previous year
#heatwaves_prev: number of days exceeding 95% heatwave threshold in the previous year
#precip_prev: number of days exceeding 95% precipitation threshold in the previous year
#year: year of observation

