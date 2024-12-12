install.packages("MASS")
library(MASS)
library(broom)
install.packages("brms")
library(brms)
install.packages("parameters")
library(parameters)

##Run the models for each variable, based on data type
##1. First, need to create a new dataset that puts columns into appropriate data type
epsilon <- 1e-8

## these are the columns with continuous data, for beta family distribution
beta_fam <- c("livelihood_dependence",
              "econ_dependence",
              "social_sensitivity",
              "ccva_social",
              "social_adaptive",
              "social_trust")
## these are the columns with ordinal data
ordinal <- c("food_dependence",
             "food_perception",
             "emergency_funds",
             "mobile_phones",
             "cmmty_empowerment",
             "ed_level")

CCVA_gesi_models <- CCVA_gesi_hypothesis %>% 
     ungroup() %>%
     mutate(demo_col = demo_col %>% factor) %>% 
     # Transform continuous columns to fit the beta family
     mutate(across(all_of(beta_fam), ~ case_when(
          . >= 1 ~ 1 - epsilon,
          . <= 0 ~ epsilon,
          TRUE ~ .
     ))) %>% 
     # Convert the ordinal columns to ascending integer values
     mutate(across(all_of(ordinal), ~ {
          factor(., 
                 levels = sort(unique(.)),
                 labels = seq_along(sort(unique(.))),
                 ordered = TRUE)
     }))

## convert gear_div to an ordered factor with dynamic levels
unique_levels <- sort(unique(CCVA_gesi_models$gear_div))
CCVA_gesi_models$gear_div <- factor(CCVA_gesi_models$gear_div, 
                                          levels = unique_levels, 
                                          ordered = TRUE)
## convert total_educated to an ordered factor with dynamic levels
unique_levels <- sort(unique(CCVA_gesi_models$livelihood_div))
CCVA_gesi_models$livelihood_div <- factor(CCVA_gesi_models$livelihood_div, 
                                          levels = unique_levels, 
                                          ordered = TRUE)

##add Total Number Educated & Total HH Size for education model
CCVA_gesi_models$`total_educated` <- as.factor(hypothesis_table$`Total Number Educated`)
## convert total_educated to an ordered factor with dynamic levels
unique_levels <- sort(unique(CCVA_gesi_models$total_educated))
CCVA_gesi_models$total_educated <- factor(CCVA_gesi_models$total_educated, 
                                          levels = unique_levels, 
                                          ordered = TRUE)
CCVA_gesi_models$`hh_size` <- as.integer(hypothesis_table$`Number of HH Members`)
CCVA_gesi_models <- CCVA_gesi_models %>%
     mutate("percent_ed" = case_when(
          `total_educated`/`hh_size` >= 1 ~ 1 - epsilon,
          `total_educated`/`hh_size` <= 0 ~ epsilon,
          TRUE ~ `total_educated`/`hh_size`))

##2. Univariate models for demo_col and each variable
##2a. Livelihood Dependence
liv_dep_model <- brm(formula = livelihood_dependence ~ demo_col + (1|community),
                  family = "beta",
                  data = CCVA_gesi_models)

## The following checks for fit
liv_dep_model <- add_criterion(liv_dep_model, "loo", save_psis = T, reloo = T)
## Looking for Pareto-K to be < 0.7
loo(liv_dep_model)
## normal distributions, 'hairy caterpillars'
plot(liv_dep_model)
## Rhat near 1, < 1.05; ESS > 1000
summary(liv_dep_model) 
## general alignment
pp_check(liv_dep_model, ndraws = 50)

## The following transforms model results out of logit

# posterior_samples(model1, pars = "b_")[,1:2] %>%
#     mutate_at(vars(b_Intercept, b_demo_col1), exp) %>%
#      posterior_summary() %>%
#      as.data.frame() %>%
#      rownames_to_column("Parameter") 

model_parameters(liv_dep_model, exponentiate = T, ci = c(0.95, 0.80, 0.60))

plot(conditional_effects(liv_dep_model, dpar = "mu"),
     points = T)

##2b. Economic Dependence
econ_dep_model <- brm(formula = econ_dependence ~ demo_col + (1|community),
                     family = "beta",
                     data = CCVA_gesi_models)

## The following checks for fit
econ_dep_model <- add_criterion(econ_dep_model, "loo", save_psis = T, reloo = T)
## Looking for Pareto-K to be < 0.7
loo(econ_dep_model)
## normal distributions, 'hairy caterpillars'
plot(econ_dep_model)
## Rhat near 1, < 1.05; ESS > 1000
summary(econ_dep_model) 
## general alignment
pp_check(econ_dep_model, ndraws = 50)

## The following transforms model results out of logit
model_parameters(econ_dep_model, exponentiate = T, ci = c(0.95, 0.80, 0.60))

plot(conditional_effects(econ_dep_model, dpar = "mu"),
     points = T)

##2c. Food Security Dependence
food_dep_model <- brm(formula = food_dependence ~ demo_col + (1|community),
                      family = cumulative("logit"),
                      cores = 4,
                      data = CCVA_gesi_models,
                      save_pars = save_pars(all = T))

## The following checks for fit
food_dep_model <- add_criterion(food_dep_model, "loo", save_psis = T, reloo = T)
## Looking for Pareto-K to be < 0.7
loo(food_dep_model)
## normal distributions, 'hairy caterpillars'
plot(food_dep_model)
## Rhat near 1, < 1.05; ESS > 1000
summary(food_dep_model) 
## general alignment
pp_check(food_dep_model, ndraws = 50)

## The following transforms model results out of logit
model_parameters(food_dep_model, exponentiate = T, ci = c(0.95, 0.80, 0.60))

plot(conditional_effects(food_dep_model, dpar = "mu"),
     points = T)

##2d. Food Insecurity Perception
food_insecurity_model <- brm(formula = food_perception ~ demo_col + (1|community),
                      family = cumulative("logit"),
                      data = CCVA_gesi_models,
                      save_pars = save_pars(all = T))

## The following checks for fit
food_insecurity_model <- add_criterion(food_insecurity_model, "loo", save_psis = T, reloo = T)
## Looking for Pareto-K to be < 0.7
loo(food_insecurity_model)
## normal distributions, 'hairy caterpillars'
plot(food_insecurity_model)
## Rhat near 1, < 1.05; ESS > 1000
summary(food_insecurity_model) 
## general alignment
pp_check(food_insecurity_model, ndraws = 50)

## The following transforms model results out of logit
model_parameters(food_insecurity_model, exponentiate = T, ci = c(0.95, 0.80, 0.60))

plot(conditional_effects(food_insecurity_model, dpar = "mu"),
     points = T)

##2c. Food Security Dependence
food_dep_model <- brm(formula = food_dependence ~ demo_col + (1|community),
                      family = cumulative("logit"),
                      cores = 4,
                      data = CCVA_gesi_models,
                      save_pars = save_pars(all = T))

## The following checks for fit
food_dep_model <- add_criterion(food_dep_model, "loo", save_psis = T, reloo = T)
## Looking for Pareto-K to be < 0.7
loo(food_dep_model)
## normal distributions, 'hairy caterpillars'
plot(food_dep_model)
## Rhat near 1, < 1.05; ESS > 1000
summary(food_dep_model) 
## general alignment
pp_check(food_dep_model, ndraws = 50)

## The following transforms model results out of logit
model_parameters(food_dep_model, exponentiate = T, ci = c(0.95, 0.80, 0.60))

plot(conditional_effects(food_dep_model, dpar = "mu"),
     points = T)

##2e. Access to Emergency Funds
emergency_funds_model <- brm(formula = emergency_funds ~ demo_col + (1|community),
                             family = bernoulli("logit"),
                             data = CCVA_gesi_models,
                             save_pars = save_pars(all = T))

## The following checks for fit
emergency_funds_model <- add_criterion(emergency_funds_model, "loo", save_psis = T, reloo = T)
## Looking for Pareto-K to be < 0.7
loo(emergency_funds_model)
## normal distributions, 'hairy caterpillars'
plot(emergency_funds_model)
## Rhat near 1, < 1.05; ESS > 1000
summary(emergency_funds_model) 
## general alignment
pp_check(emergency_funds_model, ndraws = 50)
## The following transforms model results out of logit
model_parameters(emergency_funds_model, exponentiate = T, ci = c(0.95, 0.80, 0.60))

plot(conditional_effects(emergency_funds_model, dpar = "mu"),
     points = T)

##2f. Social Trust
trust_model <- brm(formula = social_trust ~ demo_col + (1|community),
                     family = "beta",
                     data = CCVA_gesi_models)

## The following checks for fit
trust_model <- add_criterion(trust_model, "loo", save_psis = T, reloo = T)
## Looking for Pareto-K to be < 0.7
loo(trust_model)
## normal distributions, 'hairy caterpillars'
plot(trust_model)
## Rhat near 1, < 1.05; ESS > 1000
summary(trust_model) 
## general alignment
pp_check(trust_model, ndraws = 50)

## The following transforms model results out of logit
model_parameters(trust_model, exponentiate = T, ci = c(0.95, 0.80, 0.60))

plot(conditional_effects(trust_model, dpar = "mu"),
     points = T)

##2g. Mobile Phones
phones_model <- brm(formula = mobile_phones ~ demo_col + (1|community),
                             family = bernoulli("logit"),
                             data = CCVA_gesi_models,
                             save_pars = save_pars(all = T))

## The following checks for fit
phones_model <- add_criterion(phones_model, "loo", save_psis = T, reloo = T)
## Looking for Pareto-K to be < 0.7
loo(phones_model)
## normal distributions, 'hairy caterpillars'
plot(phones_model)
## Rhat near 1, < 1.05; ESS > 1000
summary(phones_model) 
## general alignment
pp_check(phones_model, ndraws = 50)
## The following transforms model results out of logit
model_parameters(phones_model, exponentiate = T, ci = c(0.95, 0.80, 0.60))

plot(conditional_effects(phones_model, dpar = "mu"),
     points = T)

##2h. Community Empowerment
empowerment_model <- brm(formula = cmmty_empowerment ~ demo_col + (1|community),
                      family = cumulative("logit"),
                      data = CCVA_gesi_models,
                      save_pars = save_pars(all = T))

## The following checks for fit
empowerment_model <- add_criterion(empowerment_model, "loo", save_psis = T, reloo = T)
## Looking for Pareto-K to be < 0.7
loo(empowerment_model)
## normal distributions, 'hairy caterpillars'
plot(empowerment_model)
## Rhat near 1, < 1.05; ESS > 1000
summary(empowerment_model) 
## general alignment
pp_check(empowerment_model, ndraws = 50)
## The following transforms model results out of logit
model_parameters(empowerment_model, exponentiate = T, ci = c(0.95, 0.80, 0.60))

plot(conditional_effects(empowerment_model, dpar = "mu"),
     points = T)

##2i. Education Level
ed_model <- brm(formula = total_educated ~ demo_col + (1|community),
                family = cratio, 
                data = CCVA_gesi_models)

## The following checks for fit
ed_model <- add_criterion(ed_model, "loo", save_psis = T, reloo = T)
## Looking for Pareto-K to be < 0.7
loo(ed_model)
## normal distributions, 'hairy caterpillars'
plot(ed_model)
## Rhat near 1, < 1.05; ESS > 1000
summary(ed_model) 
## general alignment
pp_check(ed_model, ndraws = 50)

## The following transforms model results out of logit
model_parameters(ed_model, exponentiate = T, ci = c(0.95, 0.80, 0.60))

plot(conditional_effects(ed_model, dpar = "mu"),
     points = T)

##2j. Gear Diversity
gear_model <- brm(formula = gear_div ~ demo_col + (1|community),
                family = cratio, 
                data = CCVA_gesi_models)

## The following checks for fit
gear_model <- add_criterion(gear_model, "loo", save_psis = T, reloo = T)
## Looking for Pareto-K to be < 0.7
loo(gear_model)
## normal distributions, 'hairy caterpillars'
plot(gear_model)
## Rhat near 1, < 1.05; ESS > 1000
summary(gear_model) 
## general alignment
pp_check(gear_model, ndraws = 50)

## The following transforms model results out of logit
model_parameters(gear_model, exponentiate = T, ci = c(0.95, 0.80, 0.60))

plot(conditional_effects(gear_model, dpar = "mu"),
     points = T)

##2k. Livelihood Diversity
livdiv_model <- brm(formula = livelihood_div ~ demo_col + (1|community),
                family = cratio, 
                data = CCVA_gesi_models)

## The following checks for fit
livdiv_model <- add_criterion(livdiv_model, "loo", save_psis = T, reloo = T)
## Looking for Pareto-K to be < 0.7
loo(livdiv_model)
## normal distributions, 'hairy caterpillars'
plot(livdiv_model)
## Rhat near 1, < 1.05; ESS > 1000
summary(livdiv_model) 
## general alignment
pp_check(livdiv_model, ndraws = 50)

## The following transforms model results out of logit
model_parameters(livdiv_model, exponentiate = T, ci = c(0.95, 0.80, 0.60))

plot(conditional_effects(livdiv_model, dpar = "mu"),
     points = T)




## 2. This is the simple one-by-one GLM approach that produces a table of results for each variable
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

