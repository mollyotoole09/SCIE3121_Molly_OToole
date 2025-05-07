library(ggplot2)
library(fixest)
library(cowplot)

#panel 1 - 95% threshold for heatwaves and precipitation
pdat <- read_csv("data/new_climate_data/pdat.csv")

#regression 1: avg yearly temperature and total yearly precipitation
panel1 <- feols(growth_rate ~ logval_prev + temp_prev + precip_prev | TimeSeriesID + Year, data = pdat, panel.id=~TimeSeriesID+Year)
summary(panel1)

predicts1 <- all.vars(formula(panel1))[-1]

plots1 <- map(predicts1, ~ {
    ggplot(pdat, aes_string(x = .x, y = "growth_rate")) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = TRUE, color = "blue") +
        labs(x = .x, y = "log(growth rate)", title = paste("Effect of", .x, "on growth rate")) +
        theme_minimal()
})

panel1Plots <- plot_grid(plots1[[1]], plots1[[2]], plots1[[3]], plots1[[5]], nrow = 2, ncol = 2, labels = c("A", "B", "C", "D"))
panel1Plots

#regression 2: extreme temperature and precipitation events
panel2 <- feols(growth_rate ~ logval_prev + extTemp_prev + extPrecip_prev | TimeSeriesID + Year, data = pdat, panel.id=~TimeSeriesID+Year)
summary(panel2)

predicts2 <- all.vars(formula(panel2))[-1]

plots2 <- map(predicts2, ~ {
    ggplot(pdat, aes_string(x = .x, y = "growth_rate")) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = TRUE, color = "blue") +
        labs(x = .x, y = "log(growth rate)", title = paste("Effect of", .x, "on growth rate")) +
        theme_minimal()
})

panel2Plots <- plot_grid(plots2[[1]], plots2[[2]], plots2[[3]], plots2[[5]], nrow = 2, ncol = 2, labels = c("A", "B", "C", "D"))
panel2Plots

#regression 3: avg yearly temperature, total yearly precipitation, number of days exceeding temperature and precipitation thresholds 
panel3 <- feols(growth_rate ~ logval_prev + temp_prev + precip_prev + extTemp_prev + extPrecip_prev | TimeSeriesID + Year, data = pdat, panel.id=~TimeSeriesID+Year)
summary(panel3)

#regression 4: avg yearly temperature, total yearly precipitation, number of days exceeding temperature and precipitation thresholds
# + extreme temperature * latitude and extreme precipitation * latitude
panel4 <- feols(growth_rate ~ logval_prev + temp_prev + precip_prev + extTemp_prev + extPrecip_prev + extTemp_abslat + extPrecip_abslat | TimeSeriesID + Year, data = pdat, panel.id=~TimeSeriesID+Year)
summary(panel4)

predicts4 <- all.vars(formula(panel4))[-1]

plots4 <- map(predicts4, ~ {
    ggplot(pdat, aes_string(x = .x, y = "growth_rate")) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = TRUE, color = "blue") +
        labs(x = .x, y = "log(growth rate)", title = paste("Effect of", .x, "on growth rate")) +
        theme_minimal()
})

panel4Plots <- plot_grid(plots4[[6]], plots4[[7]], nrow = 2, ncol = 1, labels = c("A", "B"))
panel4Plots


#panel 2 - 99% threshold for heatwaves and precipitation
pdat2 <- read_csv("data/new_climate_data/pdat2.csv")

#regression 1: avg yearly temperature and total yearly precipitation
panel1_99 <- feols(growth_rate ~ logval_prev + temp_prev + precip_prev | TimeSeriesID + Year, data = pdat2, panel.id=~TimeSeriesID+Year)
summary(panel1_99)

predicts1_99 <- all.vars(formula(panel1_99))[-1]

plots1_99 <- map(predicts1_99, ~ {
    ggplot(pdat2, aes_string(x = .x, y = "growth_rate")) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = TRUE, color = "blue") +
        labs(x = .x, y = "log(growth rate)", title = paste("Effect of", .x, "on growth rate")) +
        theme_minimal()
})

panel1_99Plots <- plot_grid(plots1_99[[1]], plots1_99[[2]], plots1_99[[3]], plots1_99[[5]], nrow = 2, ncol = 2, labels = c("A", "B", "C", "D"))
panel1_99Plots

#regression 2: extreme temperature and precipitation events
panel2_99 <- feols(growth_rate ~ logval_prev + extTemp_prev + extPrecip_prev | TimeSeriesID + Year, data = pdat2, panel.id=~TimeSeriesID+Year)
summary(panel2_99)

predicts2_99 <- all.vars(formula(panel2_99))[-1]

plots2_99 <- map(predicts2_99, ~ {
    ggplot(pdat2, aes_string(x = .x, y = "growth_rate")) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = TRUE, color = "blue") +
        labs(x = .x, y = "log(growth rate)", title = paste("Effect of", .x, "on growth rate")) +
        theme_minimal()
})

panel2_99Plots <- plot_grid(plots2_99[[1]], plots2_99[[2]], plots2_99[[3]], plots2_99[[5]], nrow = 2, ncol = 2, labels = c("A", "B", "C", "D"))
panel2_99Plots

#regression 3: avg yearly temperature, total yearly precipitation, number of days exceeding temperature and precipitation thresholds 
panel3_99 <- feols(growth_rate ~ logval_prev + temp_prev + precip_prev + extTemp_prev + extPrecip_prev | TimeSeriesID + Year, data = pdat2, panel.id=~TimeSeriesID+Year)
summary(panel3_99)

#regression 4: avg yearly temperature, total yearly precipitation, number of days exceeding temperature and precipitation thresholds
# + extreme temperature * latitude and extreme precipitation * latitude
panel4_99 <- feols(growth_rate ~ logval_prev + temp_prev + precip_prev + extTemp_prev + extPrecip_prev + extTemp_abslat + extPrecip_abslat | TimeSeriesID + Year, data = pdat2, panel.id=~TimeSeriesID+Year)
summary(panel4_99)

predicts4_99 <- all.vars(formula(panel4_99))[-1]

plots4_99 <- map(predicts4_99, ~ {
    ggplot(pdat2, aes_string(x = .x, y = "growth_rate")) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = TRUE, color = "blue") +
        labs(x = .x, y = "log(growth rate)", title = paste("Effect of", .x, "on growth rate")) +
        theme_minimal()
})

panel4_99Plots <- plot_grid(plots4_99[[6]], plots4_99[[7]], nrow = 2, ncol = 1, labels = c("A", "B"))
panel4_99Plots


#Description of variables: 

#growth_rate: growth rate of the species - log(abundance in current year) - log(abundance in previous year)
#logval_prev: log of abundance in the previous year 
#temp_prev: average temperature recorded in the previous year
#precip_prev: total precipitation recorded in the previous year
#extTemp_prev: number of days exceeding 95%/99% heatwave threshold in the previous year
#extPrecip_prev: number of days exceeding 95/99% precipitation threshold in the previous year
#extTemp_abslat: number of days exceeding 95%/99% heatwave threshold in the previous year * absolute latitude
#extPrecip_abslat: number of days exceeding 95%/99% precipitation threshold in the previous year * absolute latitude
#Year: year of observation