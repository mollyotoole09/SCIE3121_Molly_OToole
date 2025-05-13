library(ggplot2)
library(cowplot)
library(tidyverse)
library(fixest)

#panel 1 - 95% threshold for heatwaves and precipitation
pdat <- read_csv("data/new_climate_data/pdat.csv")

#regression1: density 
panel1 <- feols(growth_rate ~ logval_prev | TimeSeriesID, data = pdat, panel.id=~TimeSeriesID+Year)
summary(panel1)

plot_density2 <- ggplot(pdat, aes(x = logval_prev, y = growth_rate)) + 
    geom_point() + 
    geom_smooth(method = "lm") + 
    labs(x = "log(Abundance in year t - 1)", y = "log(Growth Rate in year t)", title = "Density Dependence - Analysis 2")

plot_density2
ggsave(filename = "results/Plot_DensityDependence2.pdf", plot = plot_density2)

#regression 2: add avg yearly temperature and total yearly precipitation
panel2 <- feols(growth_rate ~ logval_prev + temp_prev + precip_prev | TimeSeriesID, data = pdat, panel.id=~TimeSeriesID+Year)
summary(panel2)

predicts2 <- all.vars(formula(panel2))[-1]

plots2 <- map(predicts2, ~ {
    ggplot(pdat, aes_string(x = .x, y = "growth_rate")) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = TRUE, color = "blue") +
        labs(x = .x, y = "log(growth rate)", title = paste("Effect of", .x, "on growth rate")) +
        theme_minimal()
})

panel2Plots <- plot_grid(plots2[[2]], plots2[[3]], nrow = 2, ncol = 1, labels = c("A", "B", "C"))
panel2Plots

#regression 3: add extreme temperature and precipitation events
panel3 <- feols(growth_rate ~ logval_prev + temp_prev + precip_prev + extTemp_prev + extPrecip_prev | TimeSeriesID, data = pdat, panel.id=~TimeSeriesID+Year)
summary(panel3)

predicts3 <- all.vars(formula(panel3))[-1]

plots3 <- map(predicts3, ~ {
    ggplot(pdat, aes_string(x = .x, y = "growth_rate")) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = TRUE, color = "blue") +
        labs(x = .x, y = "log(growth rate)", title = paste("Effect of", .x, "on growth rate")) +
        theme_minimal()
})

panel3Plots <- plot_grid(plots3[[4]], plots3[[5]], nrow = 2, ncol = 1, labels = c("A", "B"))
panel3Plots

#regression 4: add interaction between extreme temperature and latitude and an interaction between extreme temperature and extreme precipitation
panel4 <- feols(growth_rate ~ logval_prev + temp_prev + precip_prev + extTemp_prev + extPrecip_prev + extTemp_prev:Latitude + extTemp_prev:extPrecip_prev| TimeSeriesID, data = pdat, panel.id=~TimeSeriesID+Year)
summary(panel4)

plot_labels <- c(
  logval_prev = "log(Abundance)",
  temp_prev = "Average annual temperature (ºC)",
  precip_prev = "Total annual precipitation (mm)",
  extTemp_prev = "Number of days exceeding 95% temperature threshold",
  extPrecip_prev = "Number of days exceeding 95% precipitation threshold"
)

predicts4 <- all.vars(formula(panel4))[-1]

plots4 <- map(predicts4, ~ {
    label <- plot_labels[.x]
    ggplot(pdat, aes_string(x = .x, y = "growth_rate")) +
        geom_point() +
        geom_smooth(method = "lm") +
        labs(x = label, y = "log(growth rate)") 
})

panel4Plots <- plot_grid(plots4[[2]], plots4[[3]], plots4[[4]], plots4[[5]], nrow = 2, ncol = 2, labels = c("A", "B", "C", "D"))
panel4Plots

ggsave(filename = "results/Plots_analysis2.pdf", plot = panel4Plots)

#regression 5: filter only for the values that explain a significant amount of variation - logval_prev, temp_prev, extTemp_prev, extTemp_prev:Latitude
panel5 <- feols(growth_rate ~ logval_prev + extTemp_prev + extTemp_prev:Latitude | TimeSeriesID, data = pdat, panel.id=~TimeSeriesID+Year)
summary(panel5)

#visualise interaction between extreme temperature and latitude
ggplot(pdat) + 
  geom_point(aes(extTemp_prev, growth_rate, col = Latitude)) +
  geom_smooth(aes(extTemp_prev, growth_rate), method = "lm") + 
  scale_color_viridis_c(option = "A") + 
  labs(color = "Latitude") +
  theme_minimal()

#separate populations 
ggplot(pdat, aes(extTemp_prev, growth_rate)) +
  geom_point(aes(color = Latitude)) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  facet_wrap(~ TimeSeriesID) +
  scale_color_viridis_c() +
  theme_minimal()

AIC(panel1, panel2, panel3, panel4, panel5)

capture.output(panel1, file = "results/panel1.txt")
capture.output(panel2, file = "results/panel2.txt")
capture.output(panel3, file = "results/panel3.txt")
capture.output(panel4, file = "results/panel4.txt")
capture.output(panel5, file = "results/panel5.txt")


#panel 2 - 99% threshold for heatwaves and precipitation
pdat2 <- read_csv("data/new_climate_data/pdat2.csv")

#regression 1: avg yearly temperature and total yearly precipitation
panel1_99 <- feols(growth_rate ~ logval_prev + temp_prev + precip_prev | TimeSeriesID, data = pdat2, panel.id=~TimeSeriesID+Year)
summary(panel1_99)

#regression 2: extreme temperature and precipitation events
panel2_99 <- feols(growth_rate ~ logval_prev + temp_prev + preip_prev + extTemp_prev + extPrecip_prev | TimeSeriesID, data = pdat2, panel.id=~TimeSeriesID+Year)
summary(panel2_99)

predicts2_99 <- all.vars(formula(panel2_99))[-1]

plots2_99 <- map(predicts2_99, ~ {
    ggplot(pdat2, aes_string(x = .x, y = "growth_rate")) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = TRUE, color = "blue") +
        labs(x = .x, y = "log(growth rate)", title = paste("Effect of", .x, "on growth rate")) +
        theme_minimal()
})

panel2_99Plots <- plot_grid(plots2_99[[1]], plots2_99[[2]], plots2_99[[3]], plots2_99[[4]], plots2_99[[5]], nrow = 3, ncol = 2, labels = c("A", "B", "C", "D", "E"))
panel2_99Plots

#regression 3: avg yearly temperature, total yearly precipitation, number of days exceeding temperature and precipitation thresholds 
panel3_99 <- feols(growth_rate ~ logval_prev + temp_prev + precip_prev + extTemp_prev + extPrecip_prev + extTemp_prev:Latitude| TimeSeriesID, data = pdat2, panel.id=~TimeSeriesID+Year)
summary(panel3_99)

#regression 4: avg yearly temperature, total yearly precipitation, number of days exceeding temperature and precipitation thresholds
# + extreme temperature * latitude and extreme precipitation * latitude
panel4_99 <- feols(growth_rate ~ logval_prev + temp_prev + precip_prev + extTemp_prev + extPrecip_prev + extTemp_prev:Latitude + extTemp_prev:extPrecip_prev | TimeSeriesID, data = pdat2, panel.id=~TimeSeriesID+Year)
summary(panel4_99)

predicts4_99 <- all.vars(formula(panel4_99))[-1]

plots4_99 <- map(predicts4_99, ~ {
    ggplot(pdat2, aes_string(x = .x, y = "growth_rate")) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = TRUE, color = "blue") +
        labs(x = .x, y = "log(growth rate)", title = paste("Effect of", .x, "on growth rate")) +
        theme_minimal()
})

panel4_99Plots <- plot_grid(plots1_99[[2]], plots1_99[[3]], plots2_99[[4]], plots2_99[[5]], nrow = 2, ncol = 2, labels = c("A", "B", "C", "D"))
panel4_99Plots

AIC(panel1_99, panel2_99, panel3_99, panel4_99)

capture.output(panel1_99, file = "results/panel1_99.txt")
capture.output(panel2_99, file = "results/panel2_99.txt")
capture.output(panel3_99, file = "results/panel3_99.txt")
capture.output(panel4_99, file = "results/panel4_99.txt")

#Description of variables: 

#growth_rate: growth rate of the species - log(abundance in current year) - log(abundance in previous year)
#logval_prev: log of abundance in the previous year 
#temp_prev: average temperature recorded in the previous year
#precip_prev: total precipitation recorded in the previous year
#extTemp_prev: number of days exceeding 95%/99% heatwave threshold in the previous year
#extPrecip_prev: number of days exceeding 95/99% precipitation threshold in the previous year
#Year: year of observation