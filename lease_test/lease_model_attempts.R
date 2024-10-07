## GLM to identify drivers
library(MASS)
glm_model <- glm(`Social Sensitivity Score` ~
                      `Women Fish Work Y/N`* `Livelihood Dependence` + 
                      `Women Fish Work Y/N`*`Economic Dependence` +
                      `Women Fish Work Y/N`*`Food Security Dependence` + 
                      `Women Fish Work Y/N`*`Food Insecurity Perception`,
                 data = fishworker_t,
                 ## could also use binomial or Gamma(link= "log")
                 family = gaussian(link= "identity"))


## plot along quantiles
plot(fishworker_t$`Economic Dependence Quantile`, fishworker_t$`Social Sensitivity Score`)

ggplot(fishworker_t, aes(x = `Female Fish Worker Quantile`, 
                         y = `Social Sensitivity Score`, 
                         color = `Women Fish Work Y/N`)) +
     geom_point() +  
     geom_smooth(method = "lm", se = FALSE) +
     scale_color_manual(values = c("blue", "red")) +  
     theme_minimal()

## isolate just the most fisheries-dependent households
## defined as in the top two quantiles of both Economic and Livelihood Dependence
fishworker_t <- fishworker_t %>%
     mutate(Dependence = `Economic Dependence Quantile` + `Livelihood Dependence Quantile`)

# Perform linear regression
lm_model <- lm(`Social Sensitivity Score` ~., data = fishworker_t)

# Summary of the regression model
summary(lm_model)