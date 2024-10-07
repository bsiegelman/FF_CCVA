## subset CCVA Social categories
gesi_calc <- ccva_social_clean %>%
     select(`Gender`,
            `HH Status`,
            `Woman Buyer/Trader`,
            `_9_hh_religion`,
            `_10_certain_tribe_Ya`,
            `Number of HH Members`,
            `Total Number Educated`,
            `Total % Educated`,
            `Number of Women`,
            `Women Educated`,
            `% Women Educated`,
            `_16a_fishing_men`:`_16c_fishing_children`,
            `_17a_processing_men`: `_17c_processing_children`,
            `Livelihood Dependence`,
            `Fisheries Income`,
            `_64_hh_fish_consumption`,
            `_63_food_procurement`,
            `Access to Emergency Funds`,
            `Total Livelihoods`,
            `Number of Gears`,
            `_30d_trust_community_neighbors`,
            `Have Mobile Phone`,
            `_52_opinions_considered`,
            `_88_post_hours_man`,
            `_88_post_income_man`,
            `_88_post_hours_woman`,
            `_88_post_income_woman`,
            `_87_hh_average_income`)
colnames(gesi_calc) <- c("Gender", 
                                "HH Status",
                                "Woman Buyer/Trader",
                                "Religion",
                                "Tribe",
                                "Number of HH Members",
                                "Total Number Educated",
                                "% Total Educated",
                                "Number of Women in HH",
                                "Number of Women Educated",
                                "% of Women Educated",
                                "Fishing Men",
                                "Fishing Women",
                                "Fishing Children",
                                "Processing Men",
                                "Processing Women",
                                "Processing Children",
                                "Livelihood Dependence",
                                "Economic Dependence",
                                "Food Security Dependence",
                                "Food Insecurity Perception",
                                "Access to Emergency Funds",
                                "Livelihood Diversity",
                                "Gear Diversity",
                                "Social Trust (Outsiders)",
                                "Mobile Phone Prevalence",
                                "Community Empowerment (Opinions Considered)",
                              "Men's Processing Hrs/Day",
                                "Men's Processing Income/Day",
                                "Women's Processing Hrs/Day",
                                "Women's Processing Income/Day",
                                "HH Avg Weekly Income")

## create columns defining % of female fishers, processors, and fish workers
gesi_results <- gesi_calc %>%
     mutate("% Female Adult Fishers" = 
                 `Fishing Women`/(`Fishing Women`+`Fishing Men`), 
            .before = `Fishing Men`) %>%
     mutate("% Female Adult Processors" = 
                 `Processing Women`/(`Processing Women`+`Processing Men`), 
            .before = `Fishing Men`) %>%
     mutate("% Female Adult Fish Workers" = round(
          (`Fishing Women`+ `Processing Women`)/
               (`Fishing Women`+`Fishing Men` + 
                     `Processing Women` + `Processing Men`), 2), 
          .before = `Fishing Men`) %>%
     select(-(`Fishing Men`:`Processing Children`)) %>%
     ## column with 1's and 0's for female-headed households
     mutate("Woman Headed" = ifelse(
          `Gender` == "f" & `HH Status` == "head", 1, 0),
          .after = `HH Status`) %>%
     ## create columns calculating women's processing wages
     mutate("Men's Processing Wages/Hr (Local Currency)" = round(
          `Men's Processing Income/Day`/`Men's Processing Hrs/Day`, 
          0)) %>%
     mutate(across("Men's Processing Wages/Hr (Local Currency)", ~ifelse(
          is.nan(.), NA, ifelse(
               . == "Inf", NA, .)))) %>%
     mutate("Women's Processing Wages/Hr (Local Currency)" = round(
          `Women's Processing Income/Day`/`Women's Processing Hrs/Day`, 
          0)) %>%
     mutate(across("Women's Processing Wages/Hr (Local Currency)", ~ifelse(
          is.nan(.), NA, ifelse(
               . == "Inf", NA, .)))) %>%
     ## create column classifying households as having or not having women in fish work
     mutate("Women Fish Work Y/N" = ifelse(
          `% Female Adult Fish Workers` > 0 | 
               `Woman Buyer/Trader` == 1, 1, 0)) %>%
     replace_na(list(`Women Fish Work Y/N` = 0)) %>%
     ## put Economic Dependence, Gear Diversity, and Livelihood Diversity into their index scores
     mutate("Economic Dependence" = `Economic Dependence`/100) %>%
     mutate(across(where(is.numeric), ~ round(., 3))) %>%
     mutate("Gear Diversity" = `Gear Diversity`*.2) %>%
     mutate("Gear Diversity" = ifelse(`Gear Diversity` > 1, 1, 
                                      `Gear Diversity`)) %>%
     mutate("Livelihood Diversity" = `Livelihood Diversity`*.25) %>%
     mutate("Livelihood Diversity" = ifelse(`Livelihood Diversity` > 1, 1,
                                            `Livelihood Diversity`)) %>%
     ## convert "Gender" column to 1's for "f" and 0's for "m"
     mutate("Gender" = ifelse(`Gender` == "f", 1, 0)) %>%
     mutate("Women's Daily Processing Income as Portion of HH Weekly Income" =
                 `Women's Processing Income/Day`/
                 `HH Avg Weekly Income`) %>%
     mutate(across(`Women's Daily Processing Income as Portion of HH Weekly Income`, 
                   ~ifelse(. > 1, 1, .))) %>%
     replace_na(list(
          `Women's Daily Processing Income as Portion of HH Weekly Income` 
          = 0))

## quantiles of women's processing % of hh income
fprocessing_breaks <- quantile(unique(
     gesi_results$`Women's Daily Processing Income as Portion of HH Weekly Income`, 
     na.rm=TRUE), 
     probs = seq(0, 1, length.out = 5), na.rm=TRUE)
fprocessing_quantiles <- as.factor(cut(
     gesi_results$`Women's Daily Processing Income as Portion of HH Weekly Income`, 
     breaks = fprocessing_breaks, labels=FALSE))
## add to data set
## keep NAs because these are households that don't fish
gesi_results <- gesi_results %>%
     cbind("HH Female Processing Quantile" = fprocessing_quantiles)
gesi_results <- gesi_results %>%
     replace_na(list(`HH Female Processing Quantile` = as.factor(1)))

glm_data <- gesi_results %>%
     ## create categories of women's fishery role
     rowwise %>%
     mutate("Woman Role in Fishery" = ifelse(
          ## if a woman is a buyer/trader but no other fishwork, then "Buyer/Trader"
          `Woman Buyer/Trader` > 0 & `% Female Adult Processors` == 0 & 
               `% Female Adult Fishers` == 0, "Buyer/Trader", ifelse(
          ## if a woman is a processor but no other fishwork, then "Fish Processor"
                    `% Female Adult Processors` > 0 & `Woman Buyer/Trader` == 0 &
                         `% Female Adult Fishers` == 0, "Fish Processor", ifelse(
          ## if women have more than one fishwork role in hh, "Multiple Roles" 
                              `Woman Buyer/Trader`+`% Female Adult Processors` +
                                   `% Female Adult Fishers` > 1, "Multiple Roles", ifelse(
          ## if no women do fish work, "None"
                                        `Woman Buyer/Trader`+`% Female Adult Processors` +
                                             `% Female Adult Fishers` == 0, "None", 
                                        NA))))) %>%
     ## if "Woman Role in Fishery" in NA, make it "None"
     replace_na(list(`Woman Role in Fishery` = "None")) %>%
     mutate(`Woman Role in Fishery` = as.factor(`Woman Role in Fishery`))

## quantiles of hh income
income_breaks <- quantile(unique(glm_data$`HH Avg Weekly Income`,
                              na.rm=TRUE), 
                       probs = seq(0, 1, length.out = 5), na.rm=TRUE)
income_quantiles <- as.factor(cut(glm_data$`HH Avg Weekly Income`,
                     breaks = income_breaks, labels=FALSE))
## add to data set
## keep NAs because these are households that don't fish
glm_data <- glm_data %>%
     cbind("HH Income Quantile" = income_quantiles)

## select columns for glm
glm_data <- glm_data %>%
     select(`Gender`,
            `Woman Headed`,
            `Woman Role in Fishery`,
            'HH Income Quantile',
            `HH Female Processing Quantile`,
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
            `Total Number Educated`)

## set weighted value for % educated in each household
## this is important in order to account for the different sizes of households
## this is done by dividing Number Educated by total number of individuals in each group, 
## then multiplying by the number of hh in that group

## this function calculates the weighted education value for every unique factor in a column
          calculate_weighted <- function(data, column_name) {
               unique_values <- unique(data[[column_name]])
               education_weighted <- rep(NA, nrow(data))
               for (value in unique_values) {
                    indiv <- sum(data$`Number of HH Members`[data[[column_name]] == value], na.rm = TRUE)
                    hh <- sum(data[[column_name]] == value, na.rm = TRUE)
                    weighted <- ifelse(data[[column_name]] == value, data$`Total Number Educated` / indiv * hh, NA)
                    education_weighted <- ifelse(!is.na(weighted), weighted, education_weighted)
               }
               return(education_weighted)
          }
          
## ***UPDATE THIS DEPENDING ON INDEPENDENT VARIABLE***
weighted_ed <- calculate_weighted(glm_data, "Woman Role in Fishery")

glm_data <- cbind(glm_data, weighted_ed) %>%
     rename("Education Weighted" = weighted_ed) %>%
     select(-c(`Number of HH Members`,
               `Total Number Educated`))

## calculate CCVA scores
## calculate CCVA scores
glm_data <- glm_data %>%
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
          `CCVA Social Score` < 0, 0, `CCVA Social Score`))

## set "None" as reference level for woman's role in the fishery for modeling
glm_data$`Woman Role in Fishery` <- relevel(
     glm_data$`Woman Role in Fishery`, ref = "None")


## run model
model3 <- glm(`Social Adaptive Score` ~ 
                   `Woman Role in Fishery`, 
    data = glm_data)
model4 <- glm(`Food Insecurity Perception` ~ 
                   `Woman Role in Fishery`*`HH Income Quantile`, 
              data = glm_data)

