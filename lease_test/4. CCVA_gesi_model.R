install.packages("MASS")
library(MASS)
library(broom)

## 1. Test independence of variables
test <- glm(`Economic Dependence` ~
                 `demo_col`,
            data = CCVA_gesi_hypothesis,
            family = gaussian(link= "identity"))
summary(test)

## in theory, households with women fish workers will have higher Economic Dependence
## and Livelihood Dependence.


## 2. Run the models for each variable and produce a table of results
# Define the dataset
set <- CCVA_gesi_hypothesis

# Create a function to run glm and extract the coefficient, p-value, and significance
run_glm_and_extract <- function(dep_var, set) {
     # Construct the formula for the GLM
     formula <- as.formula(paste0("`", dep_var, "` ~ demo_col"))
     
     # Run the GLM
     model <- glm(formula, data = set, family = gaussian(link = "identity"), 
                  na.action= na.exclude)
     
     # Get the summary of the model
     model_summary <- summary(model)
     
     # Extract coefficient and significance
     coef_estimate <- model_summary$coefficients["demo_col", "Estimate"]
     p_value <- model_summary$coefficients["demo_col", "Pr(>|t|)"]
     significance <- symnum(p_value, cutpoints = c(0, 0.001, 0.01, 0.05, 1), symbols = c("***", "**", "*", "."))
     
     # Return results as a data frame
     return(data.frame(
          "Dependent Variable" = dep_var,
          "Effect" = coef_estimate,
          "Significance" = paste0(round(p_value, 4), " ", significance)
     ))
}

# List of dependent variables
dependent_vars <- c(
     "ccva_social",
     "social_sensitivity",
     "social_adaptive",
     "econ_dependence",
     "livelihood_dependence",
     "food_dependence",
     "food_perception",
     "livelihood_div",
     "gear_div",
     "emergency_funds",
     "social_trust",
     "cmmty_empowerment",
     "mobile_phones",
     "ed_level"
)

# Run GLMs and compile results into a data frame
gesi_models_results <- bind_rows(lapply(dependent_vars, function(var) run_glm_and_extract(var, set)))

rownames(gesi_models_results) <- 
     c("CCVA Social Score",
     "Social Sensitivity Score",
     "Social Adaptive Score",
     "Economic Dependence",
     "Livelihood Dependence",
     "Food Security Dependence",
     "Food Insecurity Perception",
     "Livelihood Diversity",
     "Gear Diversity",
     "Access to Emergency Funds",
     "Social Trust",
     "Community Empowerment (Opinions Considered)",
     "Mobile Phone Prevalence",
     "% Educated 10+ years"
)
