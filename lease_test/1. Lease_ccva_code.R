library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(stringr)
library(tidytext)
library(readxl)
library(data.world)
install.packages("glmmTMB")
library(glmmTMB)

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
            `_30c_trust_community`,
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

## create sum columns for relevant questions
ccva_social_clean <- ccva_social_numeric %>%
     ## count total number of livelihoods
     mutate("Total Livelihoods" = 
                 rowSums(across(`_15a_farming_yes_no`:`_15k_other_yes_no`), 
                         na.rm = TRUE), 
            .after = "_15k_other_yes_no") %>%
     ## count number of fisheries livelihoods
     mutate("Fisheries Livelihoods" =
                 rowSums(across(c(`_15c_artisanal_yes_no`,
                                  `_15e_fishbuying_trading_yes_no`,
                                  `_15f_processing_yes_no`,
                                  `_15j_industrial_yes_no`)), na.rm = TRUE),
            .after = "Total Livelihoods") %>%
     ## % of livelihoods in fisheries
     mutate("Livelihood Dependence" = 
                 `Fisheries Livelihoods`/`Total Livelihoods`,
            .after = "Fisheries Livelihoods") %>%
     ## sum total proportion of income from fisheries
     mutate("Fisheries Income" = 
                 rowSums(across(c(`_15c_income_fishing_artisanal`,
                                  `_15e_income_buying_trading`,
                                  `_15f_income_processing`,
                                  `_15j_income_industrial`)), na.rm = TRUE),
            .after = "Livelihood Dependence") %>%
     ## 1 if any emergency funds
     mutate("Access to Emergency Funds" = ifelse(
          rowSums(across(c(`_77a_emergency_homeaa.cash`:
                                `_77h_emergency_other.social_fund`)), 
                  na.rm=TRUE) > 0, 1, 0),
          .after = `_77h_emergency_other.social_fund`) %>%
     ## count total number of gears used
     mutate("Number of Gears" = ifelse(
          rowSums(across(`_25a_gear_traditional`:
                              `_25g_gear_other_specify`), na.rm = TRUE) == 0,
          NA,
          rowSums(across(`_25a_gear_traditional`:
                              `_25g_gear_other_specify`), na.rm = TRUE))) %>%
     ## 1 if they have access to a mobile phone AND they're fishers
     ## NEED TO TURN phone NA into 0, remove rows in which income from fishing is 0
     mutate("Have Mobile Phone" = ifelse(
          `_15c_income_fishing_artisanal` == 0, NA, 
          ifelse(
               rowSums(across(c(`_27b_item_hp`, `_27c_item_smartphone`)),
                       na.rm = TRUE) > 0, 1, 0))) %>%
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
## NOTE: We have to use parent index for this data set rather than uuid
## there seems to be an issue with unmatching uuid's
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

## subset CCVA Social categories
ccva_social_calc <- ccva_social_clean %>%
     select(`Livelihood Dependence`,
            `Fisheries Income`,
            `_64_hh_fish_consumption`,
            `_63_food_procurement`,
            `Access to Emergency Funds`,
            `Total Livelihoods`,
            `Number of Gears`,
            `_30c_trust_community`,
            `_30d_trust_community_neighbors`,
            `Have Mobile Phone`,
            `_52_opinions_considered`)
colnames(ccva_social_calc) <- c("Livelihood Dependence",
                                "Economic Dependence",
                                "Food Security Dependence",
                                "Food Insecurity Perception",
                                "Access to Emergency Funds",
                                "Livelihood Diversity",
                                "Gear Diversity",
                                "Social Trust (In-Group)",
                                "Social Trust (Out-Group)",
                                "Mobile Phone Prevalence",
                                "Community Empowerment (Opinions Considered)")

## Calculate hh-level "Trust Score" that weights in-group trust relative to overall trust
## This accounts for people being generally 'trusting' - the score tells you how much more trust people have in their own community than in others
ccva_social_calc$`Social Trust` <- ccva_social_calc$`Social Trust (In-Group)`/
     (ccva_social_calc$`Social Trust (In-Group)` + 
           ccva_social_calc$`Social Trust (Out-Group)`)
## replace NaN values with 0 (because in this case, 0/0 should equal 0 trust)
ccva_social_calc$`Social Trust`[is.nan(
     ccva_social_calc$`Social Trust`)] <- 0

## calculate component scores, max, min, and range and put into a table
ccva_social_results <- ccva_social_calc %>%
     summarize(across(everything(), 
                      ~mean(., na.rm=TRUE), .names = "Score_{.col}"),
               across(everything(), 
                      ~max(., na.rm=TRUE), .names = "Max_{.col}"),
               across(everything(), 
                      ~min(., na.rm=TRUE), .names = "Min_{.col}")) %>%
     pivot_longer(cols = everything(), 
                  names_to = c(".value", "Variable"), 
                  names_sep = "_") %>%
     group_by(Variable) %>%
     summarise(Score = mean(Score),
               Max = max(Max),
               Min = min(Min),
               Range = Max - Min) %>%
     pivot_longer(cols = c(Score, Max, Min, Range), 
                  names_to = "Statistic", 
                  values_to = "Value") %>%
     pivot_wider(names_from = Variable, values_from = Value) %>%
     mutate("Economic Dependence" = `Economic Dependence`/100) %>%
     mutate(across(where(is.numeric), ~ round(., 3))) %>%
     ## convert Gear and Livelihood Diversity counts to scores
     mutate("Gear Diversity" = `Gear Diversity`*.25) %>%
     mutate("Gear Diversity" = ifelse(`Gear Diversity` > 1, 1, 
                                      `Gear Diversity`)) %>%
     mutate("Livelihood Diversity" = `Livelihood Diversity`*.1) %>%
     mutate("Livelihood Diversity" = ifelse(`Livelihood Diversity` > 1, 1,
                                            `Livelihood Diversity`)) %>%
     select(-c(Score, Max)) %>%
     column_to_rownames(var = "Statistic")

## Because education is calculated at individual level across community rather than household level,
## it is calculated separately and then added to the ccva_social_results data set
ed_score <- round(sum(ed_clean$`Total Number Educated`, na.rm=TRUE)/
                       sum(ed_clean$`Number of HH Members`, na.rm=TRUE), 3)
ed_results <- data.frame(
     "Cmmty Education" = c(ed_score, max(ed_clean$`Total % Educated`,
                                         na.rm=TRUE), 
                           min(ed_clean$`Total % Educated`,
                               na.rm=TRUE), 
                           max(ed_clean$`Total % Educated`,
                               na.rm=TRUE) -
                                min(ed_clean$`Total % Educated`,
                                    na.rm=TRUE)))
colnames(ed_results) <- "% of Community Educated +10yrs"

## bind ed_results to ccva_social_results
ccva_social_results <- cbind(ccva_social_results, ed_results) %>%
     ## remove the two social trust columns that feed into Trust Score
     select(-c(`Social Trust (In-Group)`,
               `Social Trust (Out-Group)`))


## calculate Social Sensitivity, Social Adaptive Capacity, and Overall Social scores
## Set up so that Overall Social score cannot be less than 0
social_sensitivity <- round(
     mean(c(ccva_social_results$`Livelihood Dependence`[1],
            ccva_social_results$`Economic Dependence`[1],
            ccva_social_results$`Food Security Dependence`[1],
            ccva_social_results$`Food Insecurity Perception`[1])), 
     3)

social_adaptive <- round(
     mean(c(ccva_social_results$`Access to Emergency Funds`[1],
            ccva_social_results$`Livelihood Diversity`[1],
            ccva_social_results$`Gear Diversity`[1],
            ccva_social_results$`Trust Score`[1],
            ccva_social_results$`Mobile Phone Prevalence`[1],
            ccva_social_results$`Community Empowerment (Opinions Considered)`[1],
            ccva_social_results$`% of Community Educated +10yrs`[1])),
     3)

social_vulnerability <- ifelse(
     (social_sensitivity - social_adaptive) >= 0, 
     (social_sensitivity - social_adaptive), 0)

## combine into social_scores table
social_scores <- tibble(social_sensitivity,
                        social_adaptive,
                        social_vulnerability) %>%
     t()
row.names(social_scores) <- c("Social Sensitivity",
                              "Social Adaptive Capacity",
                              "Overall Social Vulnerability Score")
colnames(social_scores) <- "Score (0-1 Scale)"

## create table with the "at risk" and "severe risk" thresholds for each variable
social_thresholds <- data.frame(
     Variable = colnames(ccva_social_results),
     "At-Risk Threshold" = c(.66, .66, .5, .5, .5, .5, .5, .5, .66, .5, .2),
     "Severe Risk Threshold" = c(.33, .33, .75, .75, .75, .25, .75, .25, .33, .25, .1)
)

##create a table of sample sizes for each variable
ccva_social_samples <- ccva_social_calc %>%
     select(-c('Social Trust (In-Group)',
               'Social Trust (Out-Group)')) %>%
     summarize(across(everything(), ~sum(!is.na(.)))) %>%
     mutate("% of Community Educated +10yrs" = sum(ed_clean$`Number of HH Members`, na.rm=TRUE)) %>%
     t() %>%
     as.data.frame()%>%
     rownames_to_column()
colnames(ccva_social_samples) <- c("Variable", "N")


## plot results
ccva_social_plot <- ccva_social_results %>%
     t() %>%
     cbind("Component" = c("Social Adaptive Capacity",
                           "Social Adaptive Capacity",
                           "Social Sensitivity",
                           "Social Sensitivity",
                           "Social Sensitivity",
                           "Social Adaptive Capacity",
                           "Social Sensitivity",
                           "Social Adaptive Capacity",
                           "Social Adaptive Capacity",
                           "Social Adaptive Capacity",
                           "Social Adaptive Capacity")) %>%
     as.data.frame() %>%
     mutate("Score" = as.numeric(Score)) %>%
     mutate(Variable = rownames(.), .before = Score) %>%
     left_join(social_thresholds, by = "Variable") %>%
     mutate("Risk Status" = ifelse(
          Component == "Social Adaptive Capacity", ifelse(
               Score < Severe.Risk.Threshold, "Severe Risk", ifelse(
                    Score < At.Risk.Threshold, "At Risk", "No Risk")),
          ifelse(Score > Severe.Risk.Threshold, "Severe Risk", ifelse(
               Score > At.Risk.Threshold, "At Risk", "No Risk")))) %>%
     select(Variable, Score, Component, `Risk Status`) %>%
     mutate(Component = factor(Component)) %>%
     group_by(Component) %>%
     mutate(Component = factor(Component, levels = c("Social Sensitivity", "Social Adaptive Capacity"))) %>%
     ## add sample sizes for each variable
     left_join(ccva_social_samples, by = "Variable") %>%
     arrange(Component, Score) %>%
     mutate(Variable = factor(Variable, levels = unique(Variable)))


##set colors
rare_primaries <- c("#005BBB", "#008542", "#5E6A71")
rare_secondaries <- c("#AA1948", "#00AFD8", "#F58233", "#7AB800",
                      "#EEAF00")
chart_colors <- c("#005BBB", "#F58233", "#5E6A71", "#00AFD8", "#EEAF00",
                  "#7AB800", "#AA1948", "#008542", "#00AFA0", "#AB5000")

## lollipop chart of variables
ggplot(ccva_social_plot, aes(x = Score, y = Variable, 
                             color = factor(`Risk Status`, levels = 
                                                 c("No Risk",
                                                   "At Risk",
                                                   "Severe Risk")))) +
     geom_segment(aes(x = 0, xend = Score, 
                      y = Variable, yend = Variable)) +
     geom_point(size = 3) +
     labs(title = "Lease \nCCVA Social Variable Scores",
          x = "Score (0-1 scale)",
          y = "",
          color = "Risk Status") +
     scale_y_discrete(labels = function(x) str_wrap(x, width = 10)) +
     scale_x_continuous(breaks = seq(0, 1, by = 0.5), limits = c(0, 1.5)) +
     scale_colour_manual(values = c(chart_colors[6], chart_colors[5],
                                    chart_colors[7])) +
     annotate("text", x = 1.15, y = 8, 
              label = "Social Adaptive\nCapacity Variables", 
              color = chart_colors[4], size = 4, 
              hjust = 0, vjust = .75,
              check_overlap = TRUE) +
     annotate("text", x = 1.15, y = 3, 
              label = "Social Sensitivity\nVariables", 
              color = chart_colors[2], size = 4, 
              hjust = 0, vjust = -.1,
              check_overlap = TRUE) +
     geom_segment(aes(x = 1.1, xend = 1.1, y = 4.75, yend = 10),
                  arrow = arrow(length = unit(0.2,"cm")), 
                  color = chart_colors[4]) +
     geom_segment(aes(x = 1.1, xend = 1.1, y = 4.18, yend = 1),
                  arrow = arrow(length = unit(0.2,"cm")), 
                  color = chart_colors[2]) +
     coord_cartesian(clip = 'off') +
     theme_grey() +
     theme(axis.title = element_blank(),
           panel.grid.minor = element_blank(),
           text = element_text(family = "Georgia"),
           axis.text.y = element_text(size = 10),
           plot.title = element_text(size = 20, margin = margin(b = 10), hjust = 0.5),
           plot.subtitle = element_text(size = 12, color = "darkslategrey", margin = margin(b = 18, l = -18)),
           plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey70", hjust = 0))
