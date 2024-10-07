library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(stringr)
library(tidytext)
library(readxl)

##read in data
data <- read.csv2("hhs_kobo_idn.csv")
## standardize the naming conventions
names(data) <- gsub("^X", "", names(data))

ed_data <- read_xlsx("lease_people.xlsx")

activities_data <- read_xlsx("lease_activities.xlsx")

##subset relevant columns (NOTE: This does not include EDUCATION LEVEL)
ccva_social <- data %>%
     select(`_16a_fishing_men`:`_16c_fishing_children`,
            `_17a_processing_men`:`_17c_processing_children`,
            `_15a_farming_yes_no`,
            `_15b_harvesting_yes_no`,
            `_15c_artisanal_yes_no`,
            `_15d_aquaculture_yes_no`,
            `_15e_fishbuying_trading_yes_no`,
            `_15f_processing_yes_no`,
            `_15g_extraction_yes_no`,
            `_15h_tourism_yes_no`,
            `_15i_other_wage_labor_yes_no`,
            `_15j_industrial_yes_no`,
            `_15k_other_yes_no`,
            `_15a_income_farming`,
            `_15b_income_harvesting`,
            `_15c_income_fishing_artisanal`,
            `_15d_income_fishing_aquaculture`,
            `_15e_income_buying_trading`,
            `_15f_income_processing`,
            `_15g_income_extraction`,
            `_15h_income_tourism`,
            `_15i_income_other_wage`,
            `_15j_income_industrial`,
            `_15k_income_other`,
            `_64_hh_fish_consumption`,
            `_63_food_procurement`,
            `_77a_emergency_homeaa.cash`:`_77h_emergency_other.no`,
            `_25a_gear_traditional`:`_25g_gear_other_specify`,
            `_30d_trust_community_neighbors`,
            `_27b_item_hp`,
            `_27c_item_smartphone`,
            `_52_opinions_considered`,
            `_88_post_hours_man`,
            `_88_post_hours_woman`,
            `_88_post_income_man`,
            `_88_post_income_woman`,
            `_87_hh_average_income`)

##set answers to convert to NA
## NOTE: In this context, "no_mgmt" answers are 0's rather than NAs
## because lack of mgmt means the community is not empowered through mgmt
NAs <- c("",
         "na")

## clean data to put everything in numbers
ccva_social_numeric <- ccva_social %>%
     ## remove emergency funds rows that only count "no" answers
     ## and the "commercial_bank" and "non_bank" columns which use characters to summarize the following more detailed, numeric columns
     select(-c(`_77a_emergency_homeaa.no`,
               `_77e_commercial_bank`,
               `_77e_commercial_bank.no`,
               `_77f_emergency_non_bank`,
               `_77f_emergency_non_bank.no`,
               `_77g_emergency_saving_clubs.no`,
               `_77h_emergency_other.no`)) %>%
     ## set NAs where appropriate
     mutate(across(everything(), ~ifelse(
          . %in% NAs, NA, .))) %>%
     ## turn yes/no into 1/0
     mutate(across(`_15a_farming_yes_no`:`_15k_other_yes_no`, 
            ~as.integer(. == "yes"))) %>%
     ## turn NA in income % columns into 0
     mutate(across(`_15a_income_farming`:`_15k_income_other`,
                   ~ifelse(is.na(.), 0, .)))


efunds_cols <- grep("^_77[a-h]_emergency_", 
                    names(ccva_social_numeric), 
                    value = TRUE)
## turn emergency fund "no" responses to 0
for (col in efunds_cols) {
     if (is.character(ccva_social_numeric[[col]])) {
          ccva_social_numeric[[col]] <- ifelse(
               grepl("no", ccva_social_numeric[[col]], 
                     ignore.case = TRUE), 0, 1)
     }
}

ccva_social_numeric <- ccva_social_numeric %>%
     ## for q25, anything containing "no" should be 0, all else 1
     mutate(across(`_25a_gear_traditional`:`_25h_gear_other`,
                   ~ ifelse(. == "no", 0, 1)))

##set number scales for fish consumption, food procurement, opinions considered, & trust
## note, for food security the NEGATIVE answers are 1, because this is a measure of sensitivity (therefore insecurity)
## we set scales here because we want to avoid binary yes/no scoring wherever possible
zero <- c("few_times",
          "certain",
          "no",
          "strongly_disagree",
          "very_low",
          "no_mgmt")
one_quarter <- c("high_chance",
                 "low",
                 "disagree")
one_third <- "few_times_per_month"
one_half <- c("uncertain",
              "neither")
two_thirds <- "few_times_per_week"
three_quarters <- c("confident_not",
                    "strong",
                    "agree")
one <- c("very_confident_not",
         "very_strong",
         "strongly_agree",
         "more_than_few_times",
         "yes")

## convert remaining multiple choice questions into numeric scales based on above criteria
for (col in names(ccva_social_numeric)) {
     if (is.character(ccva_social_numeric[[col]])) {
          ccva_social_numeric[[col]] <- ifelse(
               ccva_social_numeric[[col]] %in% zero, 0, ifelse(
                    ccva_social_numeric[[col]] %in% one_quarter, .25, ifelse(
                         ccva_social_numeric[[col]] %in% one_third, .333, ifelse(
                              ccva_social_numeric[[col]] %in% one_half, .5, ifelse(
                                   ccva_social_numeric[[col]] %in% two_thirds, .667, ifelse(
                                        ccva_social_numeric[[col]] %in% three_quarters, .75, ifelse(
                                             ccva_social_numeric[[col]] %in% one, 1, ifelse(
                                                  is.na(ccva_social_numeric[[col]]), NA, 
                                                  0))))))))
     }
}







     ## count total number of gears used
     mutate("Number of Gears" = 
                 rowSums(across(`_25a_gear_traditional`:
                                     `_25g_gear_other_specify`), na.rm = TRUE),
                         .after = `_25g_gear_other_specify`) %>%
     ## 1 if they have access to a mobile phone or smartphone
     mutate("Have Mobile Phone" = ifelse(
                 rowSums(across(c(`_27b_item_hp`, `_27c_item_smartphone`)),
                                na.rm = TRUE) > 0, 1, 0),
                         .after = `_27c_item_smartphone`) %>%
     mutate(Gender = data$`_8_gender`, .before = `_16a_fishing_men`) %>%
     mutate("HH Status" = data$`_7_hh_status`, .after = Gender)

## Subset Education Data
ed_subset <- ed_data %>%
     select(`_9_hh_gender`,
            `_9_hh_age`,
            `_9_education`,
            `_9_hh_religion`,
            `_10_certain_tribe_Ya`,
            `_parent_index`) %>%
## make the education column a 1/0 binary depending on level of education
     mutate("_9_education" = ifelse(
          `_9_education` == "secondary_school" | 
               `_9_education` == "university", 1, 0)) %>%
## remove minors (under 18) so that it's calculating adults only
     filter(`_9_hh_age` > 17) %>%
     group_by(`_parent_index`) %>%
## calculate % of HH and % of Women in HH who have 10+ years education
     mutate("Number of HH Members" = n(),
            "Total Number Educated" = sum(`_9_education`, na.rm=TRUE),
            "Number of Women" = sum(`_9_hh_gender` == "f", na.rm = TRUE),
            "Women Educated" = sum(`_9_education`[`_9_hh_gender` == "f"], 
                                   na.rm = TRUE),
            "Total % Educated" = round(`Total Number Educated` /
                 `Number of HH Members`, 1),
            "% Women Educated" = round(`Women Educated` / 
                                            `Number of Women`, 1)) %>%
     replace_na(list(`% Women Educated` = NA))

## create 'ed_clean' that has a single row for each household
## religion and tribe values are taken from the first row of each household
ed_clean <- ed_subset %>%
     select(4:12) %>%
     group_by(`_parent_index`) %>%
     summarize(across(.cols = everything(), .fns = first))

##add rows for missing parent indexes
num_hh <- nrow(ccva_social_clean)
missing_indices <- setdiff(1:num_hh, 
                           unique(ed_clean$`_parent_index`))
missing_rows <- data.frame("_parent_index" = missing_indices)
colnames(missing_rows) <- c("_parent_index")
## add missing parent indices into ed_clean
ed_clean <- bind_rows(ed_clean, missing_rows) %>%
     arrange(`_parent_index`)

## bind ed_clean to ccva_social_clean for one data set
ccva_social_clean <- cbind(ccva_social_clean[,1:2],
                           ed_clean,
                           ccva_social_clean[,3:ncol(ccva_social_clean)]) %>%
     select(-`_parent_index`)

## build out activities table
##list of fish trading activities
trading_words <- c("Menjual ikan",
                   "Menjual hasil tangkapan",
                   "Menjual",
                   "Membersikan dn menjual ikan",
                   "Membersihkan dn menjual ikan",
                   "Jual ikan",
                   "Jual Ikan",
                   "Berjualan Ikan",
                   "Menjual Ikan ke Pasar",
                   "Membersihkan ikan dn menjual",
                   "Jual ikan segar",
                   "Jual ikan olahan",
                   "Menjual ikan tangkapan",
                   "Membeli ikan",
                   "Menjual ikan hasil tangkapan suami",
                   "Berdagang",
                   "Berjualan",
                   "Menjual hasil tangkapan suami",
                   "Menjual Ikan",
                   "Asar Ikan",
                   "Berjualan ikan",
                   "Membersikan ikan dan jual ikan",
                   "menjual ikan",
                   "Membersikan ikan dn menjual ikan",
                   "Menjual hasil",
                   "Jual ikan hasil tangkapan",
                   "Menjual dan membersihkan ikan",
                   "Membersihkan dan Menjual Ikan",
                   "Mejual ikan",
                   "Jual ikan tangkapan suami",
                   "jual ikan",
                   "Menjual Ikan Keliling Pulau",
                   "Jual ikan/papalele",
                   "Menjual ikan ke pasar",
                   "Menjual Ikan ke pasar",
                   "Menjual ikan tangkapan suami",
                   "Beli ikan",
                   "Menjual Ikan suami",
                   "Menjual Ikqn",
                   "Membeli ikan di pasar Saparua",
                   "Menjual ikan di pasar ullat",
                   "Membeli ikan di pasar saparua",
                   "Menjual ikan di pasar ullath",
                   "Menjual ikan pada jibu-jibu",
                   "Menjual ikan di dalam desa",
                   "Membeli dan menjual ikan di pasar saparua",
                   "Membeli ikan dan menjual ikan di pasar",
                   "Membeli Ikan dan menjual di pasar",
                   "Menjual ke pasar Nolloth",
                   "Membeli dan menjual ikan di pasar Saparua",
                   "Membeli dan menjual ikan",
                   "Membeli ikan dan menjual ikan di pasar saparua")


activities_sellers <- activities_data %>%
     select(`_18a_women_activity`,
            `_parent_index`) %>%
## add column to indicate if women buy/trade fish
     mutate("Woman Buyer/Trader" = ifelse(
          `_18a_women_activity` %in% trading_words, 1, 0)) %>%
     select(-`_18a_women_activity`) %>%
     group_by(`_parent_index`) %>%
     summarize(across(everything(), ~sum(.))) %>%
     mutate("Woman Buyer/Trader" = ifelse(
          `Woman Buyer/Trader` > 1, 1, `Woman Buyer/Trader`))

## find any missing parent index values
num_hh <- nrow(ccva_social_clean)
missing_indices <- setdiff(1:num_hh, 
                           unique(activities_sellers$`_parent_index`))
missing_rows <- data.frame("_parent_index" = missing_indices, 
                           "Woman_Buyer_Trader" = 0)
colnames(missing_rows) <- c("_parent_index", "Woman Buyer/Trader")
## add missing parent indices into activities_selers, with 0 values for Woman Buyer/Trader
activities_sellers <- bind_rows(activities_sellers, missing_rows) %>%
     arrange(`_parent_index`)
## combine into ccva_social_clean
ccva_social_clean <- cbind(ccva_social_clean[,1:2],
                           activities_sellers,
                           ccva_social_clean[,3:ncol(ccva_social_clean)]) %>%
     select(-`_parent_index`)


     