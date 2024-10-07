## FEMALE RESPONDENT T-TEST ANALYSIS
## get sample numbers
sex_t <- ccva_gender_results %>%
     ##subset ccva social variables
     select(`Gender`,
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
sex_f_indiv <- sum(sex_t$`Number of HH Members`[
     sex_t$`Gender` == 1], na.rm=TRUE)
sex_m_indiv <- sum(sex_t$`Number of HH Members`[
     sex_t$`Gender` == 0], na.rm=TRUE)
sex_f_hh <- sum(sex_t$`Gender` == 1)
sex_m_hh <- sum(sex_t$`Gender` == 0)

## 2. add column calculating the weighted score for education
sex_t <- sex_t %>%
     mutate("Education Weighted" = ifelse(
          `Gender` == 1,
          `Total Number Educated`/sex_f_indiv*sex_f_hh,
          `Total Number Educated`/sex_m_indiv*sex_m_hh))
## calculate CCVA scores
sex_t <- sex_t %>%
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

ccva_sex_scores <- sex_t %>%
     group_by(`Gender`) %>%
     summarize(across(everything(), 
                      ~mean(., na.rm=TRUE))) %>%
     rowwise()%>%
     mutate(across(everything(), ~ifelse(
          is.numeric(.), round(.,3), .))) 

## Run t-test of Sensitivity Score testing houses with female respondent against those without
t_sex_sensitivity <- t.test(
     sex_t$`Social Sensitivity Score`[sex_t$`Gender`== 1],
     sex_t$`Social Sensitivity Score`[sex_t$`Gender`== 0],
)

##Run t-test of Adaptive Capacity Score
t_sex_adaptive <- t.test(
     sex_t$`Social Adaptive Score`[sex_t$`Gender`== 1],
     sex_t$`Social Adaptive Score`[sex_t$`Gender`== 0])
##Run t-test of overall CCVA Social Score
t_sex_ccvasocial <- t.test(
     sex_t$`CCVA Social Score`[sex_t$`Gender`== 1],
     sex_t$`CCVA Social Score`[sex_t$`Gender`== 0])

## just to check variance
ggplot(sex_t, aes(x = factor(`Gender`), y = `Social Adaptive Score`, fill = factor(`Gender`))) +
     geom_boxplot() +
     # Facet the plot by the "Gender" variable
     facet_wrap(~ factor(`Gender`), nrow = 1) +
     # Customize labels and titles
     labs(x = "Gender", y = "Social Adaptive Capacity Score", title = "Boxplot of Social Adaptive Capacity Score by Gender") +
     # Adjust theme
     theme_minimal()

# Perform t-tests for each variable and return a table showing statistical significance of each variable
# Function to perform t-tests for each variable
# Function to perform t-tests for each variable
sex_t_tests <- function(data) {
     # Initialize an empty data frame to store results
     results <- data.frame(variable = character(),
                           significant = character(),
                           stringsAsFactors = FALSE)
     
     # Loop through each column (excluding the first column)
     for (i in 2:ncol(data)) {
          # Perform t-test comparing variable by 'Gender'
          t_test_result <- t.test(data[data$`Gender` == 0, i], 
                                  data[data$`Gender` == 1, i])
          
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
result_table <- sex_t_tests(sex_t)

# Add the result_table as a row to ccva_sex_scores
ccva_sex_scores <- rbind(ccva_sex_scores, result_table[2,])

