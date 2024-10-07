library(tidyr)
library(tibble)
library(ggplot2)
library(stringr)
library(tidytext)

## subset CCVA Social categories
ccva_social_calc <- ccva_social_clean %>%
     select(`Livelihood Dependence`,
            `Fisheries Income`,
            `_64_hh_fish_consumption`,
            `_63_food_procurement`,
            `Access to Emergency Funds`,
            `Total Livelihoods`,
            `Number of Gears`,
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
                                "Social Trust (Outsiders)",
                                "Mobile Phone Prevalence",
                                "Community Empowerment (Opinions Considered)")

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
     mutate("Gear Diversity" = `Gear Diversity`*.2) %>%
     mutate("Gear Diversity" = ifelse(`Gear Diversity` > 1, 1, 
                                      `Gear Diversity`)) %>%
     mutate("Livelihood Diversity" = `Livelihood Diversity`*.25) %>%
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
ccva_social_results <- cbind(ccva_social_results, ed_results)

     
## calculate Social Sensitivity, Social Adaptive Capacity, and Overall Social scores
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
            ccva_social_results$`Social Trust (Outsiders)`[1],
            ccva_social_results$`Mobile Phone Prevalence`[1],
            ccva_social_results$`Community Empowerment (Opinions Considered)`[1],
            ccva_social_results$`% of Community Educated +10yrs`[1])),
     3)
     
social_vulnerability <- social_sensitivity - social_adaptive

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
     labs(title = "CCVA Social Variable Scores",
          x = "Score (0-1 scale)",
          y = "",
          color = "Risk Status") +
     scale_y_discrete(labels = function(x) str_wrap(x, width = 15)) +
     scale_x_continuous(breaks = seq(0, 1, by = 0.5), limits = c(0, 1.5)) +
     scale_colour_manual(values = c(chart_colors[6], chart_colors[5],
                                    chart_colors[7])) +
     annotate("text", x = 1.15, y = 8, 
              label = "Social Adaptive\nCapacity Variables", 
              color = chart_colors[4], size = 4, 
              hjust = 0, vjust = .75) +
     annotate("text", x = 1.15, y = 3, 
              label = "Social Sensitivity\nVariables", 
              color = chart_colors[2], size = 4, 
              hjust = 0, vjust = -.1) +
     geom_segment(aes(x = 1.1, xend = 1.1, y = 4.75, yend = 10),
                  arrow = arrow(length = unit(0.2,"cm")), 
                  color = chart_colors[4]) +
     geom_segment(aes(x = 1.1, xend = 1.1, y = 4.25, yend = 1),
                  arrow = arrow(length = unit(0.2,"cm")), 
                  color = chart_colors[2]) +
     theme_grey() +
     theme(axis.title = element_blank(),
           panel.grid.minor = element_blank(),
           text = element_text(family = "Georgia"),
           axis.text.y = element_text(size = 10),
           plot.title = element_text(size = 20, margin = margin(b = 10), hjust = 0),
           plot.subtitle = element_text(size = 12, color = "darkslategrey", margin = margin(b = 25, l = -25)),
           plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey70", hjust = 0))