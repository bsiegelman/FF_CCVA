install.packages("brms")
library(brms)

##1. Univariate GLMM of CCVA score

## CCVA score
model1 <- brm(formula = ccva_social ~ demo_col,
    family = "zero_one_inflated_beta",
    data = CCVA_gesi_hypothesis)

##2. Multivariate GLMMs parsing driver variables

##2a. Social Sensitivity & Adaptive Capacity
brm(bf(mvbind(social_sensitivity, social_adaptive) ~ demo_col),
   family = "zero_one_inflated_beta",
   data = CCVA_gesi_hypothesis)

##2b. Social Sensitivity, with component variables
brm(bf(mvbind(livelihood_dependence, econ_dependence,
              food_dependence, food_perception) ~ demo_col),
    family = "zero_one_inflated_beta",
    data = CCVA_gesi_hypothesis)

##2c. Adaptive Capacity, with component variables
brm(bf(mvbind(emergency_funds, livelihood_div,
              gear_div, social_trust, mobile_phones,
              cmmty_empowerment, ed_level) ~ demo_col),
    family = "zero_one_inflated_beta",
    data = CCVA_gesi_hypothesis)