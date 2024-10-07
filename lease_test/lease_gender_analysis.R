## subset CCVA Social categories
ccva_gender_calc <- ccva_social_clean %>%
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
colnames(ccva_gender_calc) <- c("Gender", 
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
ccva_gender_results <- ccva_gender_calc %>%
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
     mutate("Gender" = ifelse(`Gender` == "f", 1, 0))

## create quantiles of dependence on fisheries
## quantiles of economic dependence
edq_def <- quantile(ccva_gender_results$`Economic Dependence`, 
                               probs = seq(0, 1, by = 0.25))
## ensure distinct breaks by adding 1e-10 to each quantile and removing duplicate values
edq_def <- unique(edq_def + 1e-10)
economic_quantiles <- cut(ccva_gender_results$`Economic Dependence`, 
                          breaks = edq_def, labels = FALSE)


## 4 quantile groups based on the UNIQUE values of the distribution
edq_breaks <- quantile(unique(ccva_gender_results$`Economic Dependence`), 
                            probs = seq(0, 1, length.out = 5))
economic_quantiles <- cut(ccva_gender_results$`Economic Dependence`,
                          breaks = edq_breaks, labels=FALSE)
## add to data set
ccva_gender_results <- ccva_gender_results %>%
     mutate("Economic Dependence Quantile" = economic_quantiles,
            .after = `Economic Dependence`) %>%
     ## make sure 0 is included in quantile 1 rather than marked as NA
     mutate(across("Economic Dependence Quantile", ~ifelse(
          is.na(.), 1, .)))
## quantiles of livelihood dependence
ldq_breaks <- quantile(unique(ccva_gender_results$`Livelihood Dependence`), 
                       probs = seq(0, 1, length.out = 5))
livelihood_dep_quantiles <- cut(ccva_gender_results$`Livelihood Dependence`,
                          breaks = ldq_breaks, labels=FALSE)
## add to data set
ccva_gender_results <- ccva_gender_results %>%
     mutate("Livelihood Dependence Quantile" = livelihood_dep_quantiles,
            .after = `Economic Dependence Quantile`) %>%
     ## make sure 0 is included in quantile 1 rather than marked as NA
     mutate(across("Livelihood Dependence Quantile", ~ifelse(
          is.na(.), 1, .)))
## quantiles of % of adult fishworkers who are women
wfw_breaks <- quantile(unique(ccva_gender_results$`% Female Adult Fish Workers`,
                              na.rm=TRUE), 
                       probs = seq(0, 1, length.out = 5), na.rm=TRUE)
wfw_quantiles <- cut(ccva_gender_results$`% Female Adult Fish Workers`,
                                breaks = wfw_breaks, labels=FALSE)
## add to data set
## keep NAs because these are households that don't fish
ccva_gender_results <- ccva_gender_results %>%
     mutate("Female Fish Worker Quantile" = wfw_quantiles,
            .after = `Livelihood Dependence Quantile`)


## calculate some basic demographic/data features and summary statistics
#n
sample_size <- nrow(ccva_gender_results)

## % of respondents female
women_respondents <- round(sum(ccva_gender_results$Gender == 1)/
                                nrow(ccva_gender_results)*100, 2)
## % of households that are female-headed
female_headed <- round(sum(ccva_gender_results$`Woman Headed`)/
                            nrow(ccva_gender_results)*100, 2)
## % of households in which women fish
women_fish <- round(sum(ccva_gender_results$`% Female Adult Fishers` > 0, 
                        na.rm=TRUE)/
                    nrow(ccva_gender_results)*100, 2)
## % of households in which women process fish
women_process <- round(sum(ccva_gender_results$`% Female Adult Processors` > 0, 
                           na.rm=TRUE)/
                            nrow(ccva_gender_results)*100, 2)
## % of households in which women buy/trade fish
women_buyers <- round(sum(ccva_gender_calc$`Woman Buyer/Trader`)/
                           nrow(ccva_gender_results)*100, 2)
## % of households in which women do fish work
women_fishwork <- round(sum(ccva_gender_results$`Women Fish Work Y/N`, 
                            na.rm=TRUE)/
                             nrow(ccva_gender_results)*100, 2)
## proportion of the average household's adult fish workers who are women
hh_fishworkers_female <- mean(ccva_gender_results$`% Female Adult Fish Workers`, 
                              na.rm=TRUE)
## proportion of the avg hh's adult processors who are women
hh_processors_female <- mean(ccva_gender_results$`% Female Adult Processors`, 
                             na.rm=TRUE)

## create function to perform t-test and add asterisks across columns
## this will be used in later analyses
col_t_tests <- function(data) {
     for (col in names(data)[-1]) {  # Skip the first column
          if (is.numeric(data[[col]])) {
               t_test_result <- t.test(data[1, col], data[2, col])
               if (t_test_result$p.value < 0.05) {
                    data[1, col] <- paste0(data[1, col], "*")
               }
          }
     }
     return(data)
}