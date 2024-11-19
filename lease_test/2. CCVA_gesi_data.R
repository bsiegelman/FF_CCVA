## 1. subset CCVA Social categories
ccva_gender_calc <- ccva_social_clean %>%
     select(`Gender`,
            `HH Status`,
            `Provinsi`,
            `Kabupaten`,
            `Desa`,
            `_10_certain_tribe_Ya`,
            `Number of HH Members`,
            `Total Number Educated`,
            `Total % Educated`,
            `Number of Women`,
            `Women Educated`,
            `% Women Educated`,
            `Woman Buyer/Trader`,
            `_16a_fishing_men`:`_16c_fishing_children`,
            `_17a_processing_men`: `_17c_processing_children`,
            `Livelihood Dependence`,
            `Fisheries Income`,
            `_64_hh_fish_consumption`,
            `_63_food_procurement`,
            `Access to Emergency Funds`,
            `Total Livelihoods`,
            `Number of Gears`,
            `_30c_trust_community`,
            `_30d_trust_community_neighbors`,
            `Have Mobile Phone`,
            `_52_opinions_considered`,
            `_87_hh_average_income`)
colnames(ccva_gender_calc) <- c("Gender", 
                                "HH Status",
                                "Province",
                                "MA Area",
                                "Community",
                                "Ethnicity",
                                "Number of HH Members",
                                "Total Number Educated",
                                "% Total Educated",
                                "Number of Women in HH",
                                "Number of Women Educated",
                                "% of Women Educated",
                                "Woman Buyer/Trader",
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
                                "Social Trust (In-Group)",
                                "Social Trust (Out-Group)",
                                "Mobile Phone Prevalence",
                                "Community Empowerment (Opinions Considered)",
                                "HH Avg Weekly Income")

## 2. Transform data to get the appropriate set of measures
ccva_gender_results <- ccva_gender_calc %>%
     ## column with 1's and 0's for female-headed households
     mutate("Woman Headed" = ifelse(
          `Gender` == "f" & `HH Status` == "head", 1, 0),
          .after = `HH Status`) %>%
     ## create column classifying households as having or not having women in fish work
     mutate("Women Fish Work Y/N" = ifelse(
          rowSums(select(., `Fishing Women`, `Processing Women`, `Woman Buyer/Trader`)
                  , na.rm = TRUE) > 0, 1, 0)) %>%
replace_na(list(`Women Fish Work Y/N` = 0)) %>%
     ## convert "Gender" column to 1's for "f" and 0's for "m"
     mutate("Gender" = ifelse(`Gender` == "f", 1, 0)) %>%
     
     ## put Economic Dependence, Gear Diversity, and Livelihood Diversity into their index scores
     mutate("Economic Dependence" = `Economic Dependence`/100) %>%
     mutate(across(where(is.numeric), ~ round(., 3))) %>%
     mutate("Gear Diversity" = `Gear Diversity`*.25) %>%
     mutate("Gear Diversity" = ifelse(`Gear Diversity` > 1, 1, 
                                      `Gear Diversity`)) %>%
     mutate("Livelihood Diversity" = `Livelihood Diversity`*.25) %>%
     mutate("Livelihood Diversity" = ifelse(`Livelihood Diversity` > 1, 1,
                                            `Livelihood Diversity`)) %>%
     ## calculate Trust Score
     mutate("Social Trust" = `Social Trust (In-Group)`/(
          `Social Trust (Out-Group)` + `Social Trust (In-Group)`))
## replace Trust Score NaN with 0     
ccva_gender_results$`Social Trust`[is.nan(
     ccva_gender_results$`Social Trust`)] <- 0


## quantiles of hh income
income_breaks <- quantile(unique(ccva_gender_results$`HH Avg Weekly Income`,
                                 na.rm=TRUE), 
                          probs = seq(0, 1, length.out = 5), na.rm=TRUE)
income_quantiles <- as.factor(cut(ccva_gender_results$`HH Avg Weekly Income`,
                                  breaks = income_breaks, labels=FALSE,
                                  include.lowest = TRUE))
## add to data set
## keep NAs because these are households that don't fish
ccva_gender_results <- ccva_gender_results %>%
     cbind("HH Income Quantile" = income_quantiles)
ccva_gender_results$`Female Processers` <- ifelse(
     ccva_gender_results$`Processing Women` > 0, 1, 0)


## calculate some basic demographic/data features and summary statistics
#n
sample_size <- nrow(ccva_gender_results)

## % of respondents female
women_respondents <- round(sum(ccva_gender_results$Gender == 1)/
                                nrow(ccva_gender_results)*100, 1)
## % of households that are female-headed
female_headed <- round(sum(ccva_gender_results$`Woman Headed`)/
                            nrow(ccva_gender_results)*100, 1)
## % of households in which women do fish work
women_fishwork <- round(sum(ccva_gender_results$`Women Fish Work Y/N`, 
                            na.rm=TRUE)/
                             nrow(ccva_gender_results)*100, 1)
## % of households in which women are buyers/sellers
women_traders <- round(sum(ccva_gender_results$`Woman Buyer/Trader`, 
                           na.rm=TRUE)/
                            nrow(ccva_gender_results)*100, 1)

## 3. Create summary table of samples
## use this to decide what demographic variables to test
gesi_samples <- data.frame("Measure" = c("N", "% Female Respondents",
                                         "% Female-Headed Households",
                                         "% of HH in which Women Do Fish Work",
                                         "% of HH with Female Buyers"),
                           "Value" = c(sample_size,
                                       women_respondents,
                                       female_headed,
                                       women_fishwork,
                                       women_traders))
