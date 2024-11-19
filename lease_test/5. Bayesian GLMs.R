install.packages("brms")
library(brms)
install.packages("parameters")
library(parameters)

##1. Univariate GLMM of CCVA score
## set a value to nudge values away from 0 and 1
## this allows beta distribution without meaningfully changing results
epsilon <- 1e-8 
CCVA_gesi_hypothesis <- CCVA_gesi_hypothesis %>% 
     mutate(demo_col = demo_col %>% factor) %>%
     ## transform 1's and 0's to fit into beta family
     mutate_at(vars(5:18),
               ~ case_when(
                    . >= 1 ~ 1 - epsilon,
                    . <= 0 ~ epsilon,
                    TRUE ~ .
               ))

##1. CCVA score model
ccva_model <- brm(formula = ccva_social ~ demo_col + (1|community),
    family = "beta",
    data = CCVA_gesi_hypothesis)

## The following checks for fit
ccva_model <- add_criterion(ccva_model, "loo", save_psis = T, reloo = T)
## Looking for Pareto-K to be < 0.7
loo(ccva_model)
## normal distributions, 'hairy caterpillars'
plot(ccva_model)
## Rhat near 1, < 1.05; ESS > 1000
summary(ccva_model) 
## general alignment
pp_check(ccva_model, ndraws = 50)

## The following transforms model results out of logit

# posterior_samples(model1, pars = "b_")[,1:2] %>%
#     mutate_at(vars(b_Intercept, b_demo_col1), exp) %>%
#      posterior_summary() %>%
#      as.data.frame() %>%
#      rownames_to_column("Parameter") 

model_parameters(ccva_model, exponentiate = T, ci = c(0.95, 0.80, 0.60))

plot(conditional_effects(ccva_model, dpar = "mu"),
     points = T)



##2. Multivariate GLMMs parsing driver variables

##2a. Social Sensitivity & Adaptive Capacity
sens_ac_model <- brm(bf(mvbind(social_sensitivity, social_adaptive) ~ demo_col 
                        + (1|community)),
   family = "beta",
   data = CCVA_gesi_hypothesis)

## check for fit
sens_ac_model <- add_criterion(sens_ac_model, "loo", save_psis = T, reloo = T)
## Looking for Pareto-K to be < 0.7
loo(sens_ac_model)
## normal distributions, 'hairy caterpillars'
plot(sens_ac_model)
## Rhat near 1, < 1.05; ESS > 1000
summary(sens_ac_model) 
## general alignment
pp_check(sens_ac_model, resp = "socialsensitivity", ndraws = 50)
pp_check(sens_ac_model, resp = "socialadaptive", ndraws = 50)


## transform results for interpretability
## model_parameters() doesn't allow multivariate, so this won't work
model_parameters(sens_ac_model, exponentiate = T, ci = c(0.95, 0.80, 0.60))
## this is my work-around for the multivariate model
library(bayestestR)
# Extract posterior samples
post_ac_samples <- insight::get_parameters(sens_ac_model) %>%
     # Exponentiate all parameters
     mutate(across(everything(), exp))
# Summarize exponentiated parameters
describe_posterior(post_ac_samples, ci = c(0.95, 0.80, 0.60))


plot(conditional_effects(sens_ac_model, dpar = "mu"),
     points = T)



##2b. Social Sensitivity, with component variables
sensitivity_var_model <- brm(bf(mvbind(livelihood_dependence, econ_dependence,
              food_dependence, food_perception) ~ demo_col + (1|community)),
    family = "beta",
    data = CCVA_gesi_hypothesis)

## check for fit
sensitivity_var_model <- add_criterion(sensitivity_var_model, "loo", save_psis = T, reloo = T)
## Looking for Pareto-K to be < 0.7
loo(sensitivity_var_model)
## normal distributions, 'hairy caterpillars'
plot(sensitivity_var_model)
## Rhat near 1, < 1.05; ESS > 1000
summary(sensitivity_var_model) 
## general alignment
pp_check(sensitivity_var_model, resp = "livelihooddependence", ndraws = 50)
pp_check(sensitivity_var_model, resp = "econdependence", ndraws = 50)
pp_check(sensitivity_var_model, resp = "fooddependence", ndraws = 50)
pp_check(sensitivity_var_model, resp = "foodperception", ndraws = 50)


## transform results for interpretability
## Tentative solution
# Extract posterior samples
post_sensvar_samples <- insight::get_parameters(sensitivity_var_model) %>%
     # Exponentiate all parameters
     mutate(across(everything(), exp))
# Summarize exponentiated parameters
describe_posterior(post_sensvar_samples, ci = c(0.95, 0.80, 0.60))


plot(conditional_effects(sensitivity_var_model, dpar = "mu"),
     points = T)




##2c. Adaptive Capacity, with component variables
ac_var_model <- brm(bf(mvbind(emergency_funds, livelihood_div,
              gear_div, social_trust, mobile_phones,
              cmmty_empowerment, ed_level) ~ demo_col),
    family = "beta",
    data = CCVA_gesi_hypothesis)

## check for fit
ac_var_model <- add_criterion(ac_var_model, "loo", save_psis = T, reloo = T)
## Looking for Pareto-K to be < 0.7
loo(ac_var_model)
## normal distributions, 'hairy caterpillars'
plot(ac_var_model)
## Rhat near 1, < 1.05; ESS > 1000
summary(ac_var_model) 
## general alignment
pp_check(ac_var_model, resp= "emergencyfunds", ndraws = 50)
pp_check(ac_var_model, resp= "livelihooddiv", ndraws = 50)
pp_check(ac_var_model, resp= "geardiv", ndraws = 50)
pp_check(ac_var_model, resp= "socialtrust", ndraws = 50)
pp_check(ac_var_model, resp= "mobilephones", ndraws = 50)
pp_check(ac_var_model, resp= "cmmtyempowerment", ndraws = 50)
pp_check(ac_var_model, resp= "edlevel", ndraws = 50)


## transform results for interpretability
##doesn't work for multivariate model
model_parameters(ac_var_model, exponentiate = T, ci = c(0.95, 0.80, 0.60))
## Tentative solution
# Extract posterior samples
post_acvar_samples <- insight::get_parameters(ac_var_model) %>%
     # Exponentiate all parameters
     mutate(across(everything(), exp))
# Summarize exponentiated parameters
describe_posterior(post_acvar_samples, ci = c(0.95, 0.80, 0.60))

plot(conditional_effects(ac_var_model, dpar = "mu"),
     points = T)
