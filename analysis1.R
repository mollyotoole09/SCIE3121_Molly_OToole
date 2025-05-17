library(ggplot2)
library(cowplot)
library(tidyverse)
library(fixest)

#regression 1: heatwaves 
heatwaveRegression <- read_csv("data/heatwaveRegression.csv")
heatwavelm <- lm(growth_rate ~ logval_prev + avgTemp_prev + heatwaves_prev + year, data = heatwaveRegression)
summary(heatwavelm)

#plot the variables
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
precipRegression <- read_csv("data/precipRegression.csv")
preciplm <- lm(growth_rate ~ logval_prev + totalPrecip_prev + precip_prev + year, data = precipRegression)
summary(preciplm)

#plot the variables
predictors <- all.vars(formula(preciplm))[-1]

plots <- map(predictors, ~ {
    ggplot(precipRegression, aes_string(x = .x, y = "growth_rate")) +
        geom_point() +
        geom_smooth(method = "lm") +
        labs(x = .x, y = "log(growth rate)", title = paste("Effect of", .x, "on growth rate"))
})

precipPlots <- plot_grid(plots[[1]], plots[[2]], plots[[3]], plots[[4]], nrow = 2, ncol = 2, labels = c("A", "B", "C", "D"))
precipPlots

#regression 3: combination 1 - density dependence
combinedRegression <- read_csv("data/combinedRegression.csv")

combinedlm1 <- lm(growth_rate ~ logval_prev + year, data = combinedRegression)
summary(combinedlm1)

#regression 4: combination 2 - add avg yearly temperature and total yearly precipitation
combinedlm2 <- lm(growth_rate ~ logval_prev + avgTemp_prev + totalPrecip_prev + year, data = combinedRegression) 
summary(combinedlm2) 

#regression 5: combination 3 - add extreme temperature and precipitation events
combinedlm3 <- lm(growth_rate ~ logval_prev + avgTemp_prev + totalPrecip_prev + heatwaves_prev + precip_prev + year, data = combinedRegression)
summary(combinedlm3)

#regression 6: combination 4 - add heatwave and precipitation interaction
combinedlm4 <- lm(growth_rate ~ logval_prev + avgTemp_prev + totalPrecip_prev + heatwaves_prev + precip_prev + heatwaves_prev:precip_prev + year, data = combinedRegression)
summary(combinedlm4)

#plot the variables
plot_labels <- c(
  logval_prev = "log(Abundance)",
  avgTemp_prev = "Average annual temperature (ºC)",
  totalPrecip_prev = "Total annual precipitation (mm)",
  heatwaves_prev = "Number of days exceeding 95% temperature threshold",
  precip_prev = "Number of days exceeding 95% precipitation threshold"
)

predictors <- all.vars(formula(combinedlm4))[-1]

plots <- map(predictors, ~ {
    label <- plot_labels[.x]
    ggplot(combinedRegression, aes_string(x = .x, y = "growth_rate")) +
        geom_point() +
        geom_smooth(method = "lm") +
        labs(x = label, y = "log(growth rate)")
})

combinedRegressionPlots <- plot_grid(plots[[2]], plots[[3]], plots[[4]], plots[[5]], nrow = 2, ncol = 2, labels = c("A", "B", "C", "D"))
combinedRegressionPlots

ggsave(filename = "results/Plots_analysis1.pdf", plot = combinedRegressionPlots)

#density dependence plot
plot_density1 <- ggplot(combinedRegression, aes(x = logval_prev, y = growth_rate)) + 
    geom_point() + 
    geom_smooth(method = "lm") + 
    labs(x = "log(Abundance in year t - 1)", y = "log(Growth Rate in year t)", title = "Density Dependence - Analysis 1")

ggsave(filename = "results/Plot_DensityDependence1.pdf", plot = plot_density1)

AIC(combinedlm1, combinedlm2, combinedlm3, combinedlm4)

#combinedlm1 is the best model 

capture.output(summary(combinedlm1), file = "results/combinedlm1.txt")
capture.output(summary(combinedlm2), file = "results/combinedlm2.txt")
capture.output(summary(combinedlm3), file = "results/combinedlm3.txt")
capture.output(summary(combinedlm4), file = "results/combinedlm4.txt")

#Description of variables: 

#growth_rate: growth rate of the species - log(abundance in current year) - log(abundance in previous year)
#logval_prev: log of abundance in the previous year 
#avgTemp_prev: average temperature recorded in the previous year
#totalPrecip_prev: total precipitation recorded in the previous year
#heatwaves_prev: number of days exceeding 95% heatwave threshold in the previous year
#precip_prev: number of days exceeding 95% precipitation threshold in the previous year
#year: year of observation


