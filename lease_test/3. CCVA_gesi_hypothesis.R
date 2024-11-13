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
hypothesis_table <- cbind(demo_col, hypothesis_table) %>%
     group_by(`demo_col`)

hypothesis_table$`demo_col` <- ifelse(
     is.na(hypothesis_table$`demo_col`), 0, hypothesis_table$`demo_col`)

##3. Set weighted value for % educated in each household
## this is important in order to account for the different sizes of households
## this is done by dividing Number Educated by total number of individuals in that group, then multiplying by the number of hh in that group

# Create a function to calculate weighted education scores per group
weighted_ed <- function(num_educated_col, num_members_col, data) {
     # Calculate group sums and counts
     group_stats <- data %>%
          group_by(demo_col) %>%
          summarise(
               group_indiv = sum({{ num_members_col }}, na.rm = TRUE),
               group_hh = n(),
               .groups = 'drop'  # Avoid warning about grouping in summarise
          )
     
     # Merge group stats back to the original data
     data <- data %>%
          left_join(group_stats, by = "demo_col")
     
     # Calculate Education Weighted for each row
     data <- data %>%
          mutate(`Education Weighted` = ({{ num_educated_col }} / group_indiv * group_hh)) %>%
          select(-c(group_indiv, group_hh))
     
     return(data)
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
               `Total Number Educated`)) %>%
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

##scale ed_level to a 0-1 range for modeling, using min-max normalization
# Rescale ed_level to a 0-1 range
CCVA_gesi_hypothesis$ed_level <- (CCVA_gesi_hypothesis$ed_level - 
                                       min(CCVA_gesi_hypothesis$ed_level,
                                           na.rm = TRUE)) /
     (max(CCVA_gesi_hypothesis$ed_level,
          na.rm = TRUE) - min(CCVA_gesi_hypothesis$ed_level,
                              na.rm = TRUE))


## You now have a table listing the CCVA Social Variable scores for each group
## in your chosen demographic breakdown.
