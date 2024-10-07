## FEMALE FISH WORKERS T-TEST ANALYSIS
## get sample numbers
fishwork_t <- ccva_gender_results %>%
     ##subset ccva social variables
     select(`Women Fish Work Y/N`,
            `Livelihood Dependence`,
            `Economic Dependence`,
            `Food Security Dependence`,
            `Food Insecurity Perception`,
            `Access to Emergency Funds`,
            `Livelihood Diversity`,
            `Gear Diversity`,
            `Social Trust (Outsiders)`,
            `Mobile Phone Prevalence`,
            `Community Empowerment (Opinions Considered)`,
            `Number of HH Members`,
            `Total Number Educated`) %>%
     rowwise() %>%
     mutate(across(everything(), ~ifelse(
          is.numeric(.), round(., 3), .)))

## set weighted value for % educated in each household
## this is important in order to account for the different sizes of households
## this is done by dividing Number Educated by total number of individuals in each group, then multiplying by the number of hh in that group

## 1. set fixed numbers of total individuals in group and number of hh in group
fw_yes_indiv <- sum(fishwork_t$`Number of HH Members`[
     fishwork_t$`Women Fish Work Y/N` == 1], na.rm=TRUE)
fw_no_indiv <- sum(fishwork_t$`Number of HH Members`[
     fishwork_t$`Women Fish Work Y/N` == 0], na.rm=TRUE)
fw_yes_hh <- sum(fishwork_t$`Women Fish Work Y/N` == 1)
fw_no_hh <- sum(fishwork_t$`Women Fish Work Y/N` == 0)

## 2. add column calculating the weighted score for education
fishwork_t <- fishwork_t %>%
     mutate("Education Weighted" = ifelse(
          `Women Fish Work Y/N` == 1,
          `Total Number Educated`/fw_yes_indiv*fw_yes_hh,
          `Total Number Educated`/fw_no_indiv*fw_no_hh))
## calculate CCVA scores
fishwork_t <- fishwork_t %>%
     rowwise() %>%
     mutate("Social Sensitivity Score" = round(
          mean(c(`Livelihood Dependence`,
                 `Economic Dependence`,
                 `Food Security Dependence`,
                 `Food Insecurity Perception`), na.rm=TRUE),
          3)) %>%
     rowwise()%>%
     mutate("Social Adaptive Score" = round(
          mean(c(`Access to Emergency Funds`,
                 `Livelihood Diversity`,
                 `Gear Diversity`,
                 `Social Trust (Outsiders)`,
                 `Mobile Phone Prevalence`,
                 `Community Empowerment (Opinions Considered)`,
                 `Education Weighted`), na.rm = TRUE), 
          3)) %>%
     rowwise()%>%
     mutate("CCVA Social Score" = 
                 `Social Sensitivity Score` - `Social Adaptive Score`) %>%
     ## convert negative ccva social scores to 0 
     rowwise %>%
     mutate("CCVA Social Score" = ifelse(
          `CCVA Social Score` < 0, 0, `CCVA Social Score`)) %>%
     rename("% Educated 10+ years" = `Education Weighted`) %>%
     select(-c(`Number of HH Members`,
               `Total Number Educated`))
     
ccva_fishwork_scores <- fishwork_t %>%
     group_by(`Women Fish Work Y/N`) %>%
     summarize(across(everything(), 
                      ~mean(., na.rm=TRUE))) %>%
     rowwise()%>%
     mutate(across(everything(), ~ifelse(
          is.numeric(.), round(.,3), .))) 

## Run t-test of Sensitivity Score testing houses with female fishworkers against those without
t_fishwork_sensitivity <- t.test(
     fishwork_t$`Social Sensitivity Score`[fishwork_t$`Women Fish Work Y/N`== 1],
     fishwork_t$`Social Sensitivity Score`[fishwork_t$`Women Fish Work Y/N`== 0],
)

##Run t-test of Adaptive Capacity Score
t_fishwork_adaptive <- t.test(
     fishwork_t$`Social Adaptive Score`[fishwork_t$`Women Fish Work Y/N`== 1],
     fishwork_t$`Social Adaptive Score`[fishwork_t$`Women Fish Work Y/N`== 0])
##Run t-test of overall CCVA Social Score
t_fishwork_ccvasocial <- t.test(
     fishwork_t$`CCVA Social Score`[fishwork_t$`Women Fish Work Y/N`== 1],
     fishwork_t$`CCVA Social Score`[fishwork_t$`Women Fish Work Y/N`== 0])

## just to check variance
ggplot(fishwork_t, aes(x = factor(`Women Fish Work Y/N`), y = `Social Adaptive Score`, fill = factor(`Women Fish Work Y/N`))) +
     geom_boxplot() +
     # Facet the plot by the "Women Fish Work Y/N" variable
     facet_wrap(~ factor(`Women Fish Work Y/N`), nrow = 1) +
     # Customize labels and titles
     labs(x = "Women Fish Work Y/N", y = "Social Adaptive Capacity Score", title = "Boxplot of Social Adaptive Capacity Score by Women Fish Work Y/N") +
     # Adjust theme
     theme_minimal()

# Perform t-tests for each variable and return a table showing statistical significance of each variable
# Function to perform t-tests for each variable
# Function to perform t-tests for each variable
fishwork_t_tests <- function(data) {
     # Initialize an empty data frame to store results
     results <- data.frame(variable = character(),
                           significant = character(),
                           stringsAsFactors = FALSE)
     
     # Loop through each column (excluding the first column)
     for (i in 2:ncol(data)) {
          # Perform t-test comparing variable by 'Women Fish Work Y/N'
          t_test_result <- t.test(data[data$`Women Fish Work Y/N` == 0, i], 
                                  data[data$`Women Fish Work Y/N` == 1, i])
          
          # Check if p-value is less than 0.05
          if (t_test_result$p.value < 0.05) {
               # If p-value is significant, append 'yes' to results
               results <- rbind(results, data.frame(variable = names(data)[i],
                                                    significant = "yes"))
          } else {
               # If p-value is not significant, append 'no' to results
               results <- rbind(results, data.frame(variable = names(data)[i],
                                                    significant = "no"))
          }
     }
     
     # Transpose the results
     results <- t(results)
     
     # Create a new column for statistical significance label
     sig_col <- c("", "Statistically Significant")
     
     # Add the new row to the results data frame
     results <- cbind(sig_col, results)
     
     return(results)
}

# Call the function with your dataset
result_table <- fishwork_t_tests(fishwork_t)

# Add the result_table as a row to ccva_fishwork_scores
ccva_fishwork_scores <- rbind(ccva_fishwork_scores, result_table[2,])

     