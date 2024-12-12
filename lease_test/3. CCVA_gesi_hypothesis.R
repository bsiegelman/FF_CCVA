## Build the table that summarizes CCVA scores
## per household based on a given hypothesis/demographic grouping

##1. Define the column you will use for demographic grouping/hypothesis
demo_col <- ccva_gender_results$`Women Fish Work Y/N`
##update the text to describe your demographic grouping
demo_text <- "Female Fish Workers"

##2. Subset the relevant columns for CCVA calculation
## this automatically includes the demo_col object defined above
hypothesis_table <- ccva_gender_results %>%
     ##subset ccva social variables
     select(`Livelihood Dependence`,
            `Economic Dependence`,
            `Food Security Dependence`,
            `Food Insecurity Perception`,
            `Access to Emergency Funds`,
            `Livelihood Diversity`,
            `Gear Diversity`,
            `Social Trust`,
            `Mobile Phone Prevalence`,
            `Community Empowerment (Opinions Considered)`,
            `Number of HH Members`,
            `Total Number Educated`) %>%
     rowwise() %>%
     mutate(across(everything(), ~ifelse(
          is.numeric(.), round(., 3), .)))
#bind demo_col and group by demo_col
hypothesis_table <- bind_cols(
     demo_col = demo_col,
     Province = as.factor(ccva_gender_results$`Province`),
     MA = as.factor(ccva_gender_results$`MA Area`),
     Community = as.factor(ccva_gender_results$`Community`), 
     hypothesis_table) %>%
     group_by(`demo_col`)

hypothesis_table$`demo_col` <- ifelse(
     is.na(hypothesis_table$`demo_col`), 0, hypothesis_table$`demo_col`)

##3. Set weighted value for % educated in each household
## this is important in order to account for the different sizes of households
## this is done by multiplying the households % educated by the group's % educated
## this weighting approach scales by group-level education levels while preserving
## hh and in-group proportional relationships, and avoids inflating values of smaller groups

# Create a function to calculate weighted education scores per group
weighted_ed <- function(num_educated_col, num_members_col, data) {
     data %>%
          group_by(demo_col) %>%
          mutate(
               group_total_members = sum({{ num_members_col }}, na.rm = TRUE),
               group_total_educated = sum({{ num_educated_col }}, na.rm = TRUE),
               prop_educated_in_group = group_total_educated / group_total_members,
               `Education Weighted` = ({{ num_educated_col }} / {{ num_members_col }}) * prop_educated_in_group
          ) %>%
          ungroup()
}

# Applying the function to calculate weighted scores
hypothesis_table <- weighted_ed(
     num_educated_col = `Total Number Educated`, 
     num_members_col = `Number of HH Members`, 
     data = hypothesis_table
)

## calculate CCVA scores
CCVA_gesi_hypothesis <- hypothesis_table %>%
     select(-c(`Number of HH Members`,
               `Total Number Educated`,
               `group_total_members`,
               `group_total_educated`,
               `prop_educated_in_group`)) %>%
     rowwise() %>%
     mutate("Social Sensitivity Score" = round(
          mean(c(`Livelihood Dependence`,
                 `Economic Dependence`,
                 `Food Security Dependence`,
                 `Food Insecurity Perception`), na.rm=TRUE),
          2)) %>%
     rowwise()%>%
     mutate("Social Adaptive Score" = round(
          mean(c(`Access to Emergency Funds`,
                 `Livelihood Diversity`,
                 `Gear Diversity`,
                 `Social Trust`,
                 `Mobile Phone Prevalence`,
                 `Community Empowerment (Opinions Considered)`,
                 `Education Weighted`), na.rm = TRUE), 
          2)) %>%
     rowwise()%>%
     mutate("CCVA Social Score" = 
                 `Social Sensitivity Score` - `Social Adaptive Score`) %>%
     ## convert negative ccva social scores to 0 
     rowwise %>%
     mutate("CCVA Social Score" = ifelse(
          `CCVA Social Score` < 0, 0, `CCVA Social Score`)) %>%
     rename("% Educated 10+ years" = `Education Weighted`) %>%
     group_by(`demo_col`)

colnames(CCVA_gesi_hypothesis) <- c("demo_col", 
                                    "province",
                                    "ma",
                                    "community",
                                    "livelihood_dependence",
                                    "econ_dependence",
                                    "food_dependence",
                                    "food_perception",
                                    "emergency_funds",
                                    "livelihood_div",
                                    "gear_div",
                                    "social_trust",
                                    "mobile_phones",
                                    "cmmty_empowerment",
                                    "ed_level",
                                    "social_sensitivity",
                                    "social_adaptive",
                                    "ccva_social")


## You now have a table listing the CCVA Social Variable scores for each group
## in your chosen demographic breakdown.

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

## Use this table for Bayesian modeling
