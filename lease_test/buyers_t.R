## FEMALE BUYER T-TEST ANALYSIS
## get sample numbers
buyer_t <- ccva_gender_results %>%
     ##subset ccva social variables
     select(`Woman Buyer/Trader`,
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
buyer_yes_indiv <- sum(buyer_t$`Number of HH Members`[
     buyer_t$`Woman Buyer/Trader` == 1], na.rm=TRUE)
buyer_no_indiv <- sum(buyer_t$`Number of HH Members`[
     buyer_t$`Woman Buyer/Trader` == 0], na.rm=TRUE)
buyer_yes_hh <- sum(buyer_t$`Woman Buyer/Trader` == 1)
buyer_no_hh <- sum(buyer_t$`Woman Buyer/Trader` == 0)

## 2. add column calculating the weighted score for education
buyer_t <- buyer_t %>%
     mutate("Education Weighted" = ifelse(
          `Woman Buyer/Trader` == 1,
          `Total Number Educated`/buyer_yes_indiv*buyer_yes_hh,
          `Total Number Educated`/buyer_no_indiv*buyer_no_hh))
## calculate CCVA scores
buyer_t <- buyer_t %>%
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

ccva_buyer_scores <- buyer_t %>%
     group_by(`Woman Buyer/Trader`) %>%
     summarize(across(everything(), 
                      ~mean(., na.rm=TRUE))) %>%
     rowwise()%>%
     mutate(across(everything(), ~ifelse(
          is.numeric(.), round(.,3), .))) 

## Run t-test of Sensitivity Score testing houses with female buyers against those without
t_buyer_sensitivity <- t.test(
     buyer_t$`Social Sensitivity Score`[buyer_t$`Woman Buyer/Trader`== 1],
     buyer_t$`Social Sensitivity Score`[buyer_t$`Woman Buyer/Trader`== 0],
)

##Run t-test of Adaptive Capacity Score
t_buyer_adaptive <- t.test(
     buyer_t$`Social Adaptive Score`[buyer_t$`Woman Buyer/Trader`== 1],
     buyer_t$`Social Adaptive Score`[buyer_t$`Woman Buyer/Trader`== 0])
##Run t-test of overall CCVA Social Score
t_buyer_ccvasocial <- t.test(
     buyer_t$`CCVA Social Score`[buyer_t$`Woman Buyer/Trader`== 1],
     buyer_t$`CCVA Social Score`[buyer_t$`Woman Buyer/Trader`== 0])

## just to check variance
ggplot(buyer_t, aes(x = factor(`Woman Buyer/Trader`), y = `Social Adaptive Score`, fill = factor(`Woman Buyer/Trader`))) +
     geom_boxplot() +
     # Facet the plot by the "Woman Buyer/Trader" variable
     facet_wrap(~ factor(`Woman Buyer/Trader`), nrow = 1) +
     # Customize labels and titles
     labs(x = "Woman Buyer/Trader", y = "Social Adaptive Capacity Score", title = "Boxplot of Social Adaptive Capacity Score by Woman Buyer/Trader") +
     # Adjust theme
     theme_minimal()

# Perform t-tests for each variable and return a table showing statistical significance of each variable
# Function to perform t-tests for each variable
# Function to perform t-tests for each variable
buyer_t_tests <- function(data) {
     # Initialize an empty data frame to store results
     results <- data.frame(variable = character(),
                           significant = character(),
                           stringsAsFactors = FALSE)
     
     # Loop through each column (excluding the first column)
     for (i in 2:ncol(data)) {
          # Perform t-test comparing variable by 'Woman Buyer/Trader'
          t_test_result <- t.test(data[data$`Woman Buyer/Trader` == 0, i], 
                                  data[data$`Woman Buyer/Trader` == 1, i])
          
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
result_table <- buyer_t_tests(buyer_t)

# Add the result_table as a row to ccva_buyer_scores
ccva_buyer_scores <- rbind(ccva_buyer_scores, result_table[2,])

