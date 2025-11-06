
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("gridExtra")
#install.packages("patchwork")
library(ggplot2)
library(dplyr)
library(gridExtra)
library(patchwork)
library(grid)
library(stringr)

# SCSS Results
sa2_results <- read.csv("C:/Users/bmart/OneDrive - Johns Hopkins/JHU Post-Doc/SemaglutideNAION/cm-scss-results2/results-cm-sensitivity-analysis-2.csv")

# Convert relevant columns to numeric (handling '<5' as NA)
sa2_results$hr <- as.numeric(as.character(sa2_results$hr))
sa2_results$hr_lb <- as.numeric(as.character(sa2_results$hr_lb))
sa2_results$hr_ub <- as.numeric(as.character(sa2_results$hr_ub))

# Filter out rows where hr_lb or hr_ub is missing
sa2_results <- sa2_results %>% filter(!is.na(hr_lb) & !is.na(hr_ub))

# Fix copyright symbol unicode
sa2_results$database_name <- stringr::str_replace_all(sa2_results$database_name,"\xae","")
sa2_results$database_name <- stringr::str_replace_all(sa2_results$database_name,"Bayesian Synthesis","Meta-analysis")
sa2_results$comparison_name <- stringr::str_replace_all(sa2_results$comparison_name,"New user of semaglutide as 2nd-line treatment with prior T2DM","Semaglutide")
sa2_results$comparison_name <- stringr::str_replace_all(sa2_results$comparison_name,"New user of empagliflozin as 2nd-line treatment with prior T2DM","Empagliflozin")
sa2_results$comparison_name <- stringr::str_replace_all(sa2_results$comparison_name,"Dec2017-Jan2020","(December 2017 to January 2020)")
sa2_results$comparison_name <- stringr::str_replace_all(sa2_results$comparison_name,"Feb2020-Jun2021","(February 2020 to June 2021)")
sa2_results$comparison_name <- stringr::str_replace_all(sa2_results$comparison_name,"Jul2021-Dec2023","(July 2017 to December 2023)")
sa2_results$comparison_name <- stringr::str_replace_all(sa2_results$comparison_name,"Semaglutide \\(December 2017 to January 2020\\)","Semaglutide")
sa2_results$comparison_name <- stringr::str_replace_all(sa2_results$comparison_name,"Semaglutide \\(February 2020 to June 2021\\)","Semaglutide")
sa2_results$comparison_name <- stringr::str_replace_all(sa2_results$comparison_name,"Semaglutide \\(July 2017 to December 2023\\)","Semaglutide")

# Sum N by exposure
sa2_results <- sa2_results %>% 
  group_by(comparator_name, outcome_name) %>% 
  mutate(exposure_n = max(group_n)) %>% 
  ungroup()

# Concatenate label for exposure with N
sa2_results <- sa2_results %>% 
  mutate(exposure_label = paste0(comparison_name, " (N=", exposure_n, ")")) %>% 
  mutate(exposure_label = str_replace_all(exposure_label,"vs.","v."))


sa2_results$exposure_label <- stringr::str_replace_all(sa2_results$exposure_label,"December","Dec")
sa2_results$exposure_label <- stringr::str_replace_all(sa2_results$exposure_label,"February","Feb")
sa2_results$exposure_label <- stringr::str_replace_all(sa2_results$exposure_label,"January","Jan")

# Order the bars
sa2_results <- sa2_results %>%
  arrange(database_order) %>%  # Sort by your custom column
  mutate(database_name = factor(database_name, levels = rev(unique(database_name))))




# sa2_results_ordered <- sa2_results_ordered %>%
#   arrange(database_order) %>%  # Sort by your custom column
#   mutate(comparator_name = factor(comparator_name, levels = unique(comparator_name))) %>% 
#   mutate(exposure_label = factor(exposure_label, levels = unique(exposure_label)))



# Separate Sensitivity and Specificity HR calculations
sa2_results_sens <- sa2_results %>% filter(outcome_id==17760)
sa2_results_spec <- sa2_results %>% filter(outcome_id==17761)



# Create the forest plot for Sensitive NAION
sa2_sens <- ggplot(sa2_results_sens, aes(x = hr, y = database_name, xmin = hr_lb, xmax = hr_ub,
                                       color = ifelse(database_name == "Meta-analysis", "firebrick3", "dodgerblue3"))) +
  geom_point(size = 3) +
  geom_errorbarh(height = 0.3, linewidth=1.1) +
  geom_vline(xintercept = 1, linetype = "solid", color = "black") +
  facet_wrap(~ exposure_label, scales = "free_y",ncol = 1) +
  scale_x_log10(limits = c(0.01,800), breaks = c(0.01,0.05, 0.2, 0.5, 1, 2, 4,20), labels = c("0.01","0.05","0.2", "0.5", "1", "2", "4", "20")) +
  scale_color_identity() +
  labs(title = "Sensitive NAION",
       x = "Hazard Ratio (95% CI)", y = "") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(size = 24, hjust = 0.5),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 16, margin = margin(t=15)),
        strip.text = element_text(size = 14),
        strip.text.x = element_text(margin = margin(0,3,0,3, "cm")),
        strip.clip = "off",
        plot.margin = unit(c(1, 2, 1, 1), "lines")) +
  geom_text(aes(x = max(hr_ub),
                label = ifelse(p < 0.001, 
                               paste0("HR: ", 
                                      sub("^0+", "", ifelse(hr >= 0.01, format(round(hr, 2)), format(round(hr, 3)))), 
                                      " (", 
                                      sub("^0+", "", ifelse(hr_lb >= 0.01, format(round(hr_lb, 2)), format(round(hr_lb, 3)))), 
                                      "-", 
                                      sub("^0+", "", ifelse(hr_ub >= 0.01, format(round(hr_ub, 2)), format(round(hr_ub, 3)))), 
                                      "), P<0.001"),
                               paste0("HR: ", 
                                      sub("^0+", "", ifelse(hr >= 0.01, format(round(hr, 2)), format(round(hr, 3)))), 
                                      " (", 
                                      sub("^0+", "", ifelse(hr_lb >= 0.01, format(round(hr_lb, 2)), format(round(hr_lb, 3)))), 
                                      "-", 
                                      sub("^0+", "", ifelse(hr_ub >= 0.01, format(round(hr_ub, 2)), format(round(hr_ub, 3)))), 
                                      "), P=", 
                                      sub("^0+", "", ifelse(p >= 0.01, format(round(p, 2)), format(round(p, 3)))))),
                fontface = ifelse(p < 0.05, "bold", "plain")),
            hjust = -0.2, size = 4.5, color = "black", check_overlap = TRUE)


# Create the forest plot for Specific NAION
sa2_spec <- ggplot(sa2_results_spec, aes(x = hr, y = database_name, xmin = hr_lb, xmax = hr_ub,
                                       color = ifelse(database_name == "Meta-analysis", "firebrick3", "dodgerblue3"))) +
  geom_point(size = 3) +
  geom_errorbarh(height = 0.3, linewidth=1.1) +
  geom_vline(xintercept = 1, linetype = "solid", color = "black") +
  facet_wrap(~ exposure_label, scales = "free_y",ncol = 1) +
  scale_x_log10(limits = c(0.004,2000), breaks = c(0.01,0.05, 0.2, 0.5, 1, 2, 4,20), labels = c("0.01","0.05","0.2", "0.5", "1", "2", "4", "20")) +
  scale_color_identity() +
  labs(title = "Specific NAION",
       x = "Hazard Ratio (95% CI)", y = "") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(size = 24, hjust = 0.5),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 16, margin = margin(t=15)),
        strip.text = element_text(size = 14),
        strip.clip = "off",
        strip.text.x = element_text(margin = margin(0,3,0,3, "cm")),
        plot.margin = unit(c(1, 2, 1, 1), "lines")) +
  geom_text(aes(x = max(hr_ub),
                label = ifelse(p < 0.001, 
                               paste0("HR: ", 
                                      sub("^0+", "", ifelse(hr >= 0.01, format(round(hr, 2)), format(round(hr, 3)))), 
                                      " (", 
                                      sub("^0+", "", ifelse(hr_lb >= 0.01, format(round(hr_lb, 2)), format(round(hr_lb, 3)))), 
                                      "-", 
                                      sub("^0+", "", ifelse(hr_ub >= 0.01, format(round(hr_ub, 2)), format(round(hr_ub, 3)))), 
                                      "), P<0.001"),
                               paste0("HR: ", 
                                      sub("^0+", "", ifelse(hr >= 0.01, format(round(hr, 2)), format(round(hr, 3)))), 
                                      " (", 
                                      sub("^0+", "", ifelse(hr_lb >= 0.01, format(round(hr_lb, 2)), format(round(hr_lb, 3)))), 
                                      "-", 
                                      sub("^0+", "", ifelse(hr_ub >= 0.01, format(round(hr_ub, 2)), format(round(hr_ub, 3)))), 
                                      "), P=", 
                                      sub("^0+", "", ifelse(p >= 0.01, format(round(p, 2)), format(round(p, 3)))))),
                fontface = ifelse(p < 0.05, "bold", "plain")),
            hjust = -0.3, size = 4.5, color = "black", check_overlap = TRUE)


# Convert ggplot objects to grobs using ggplotGrob()
sa2_sens_grob <- ggplotGrob(sa2_sens)
sa2_spec_grob <- ggplotGrob(sa2_spec)



sa2_combined_grobs <- grid.arrange(
  grobTree(rectGrob(gp = gpar(lwd = 2, col = "black")), sa2_sens_grob),  # Add border around plot1
  grobTree(rectGrob(gp = gpar(lwd = 2, col = "black")), sa2_spec_grob),  # Add border around plot2
  ncol = 2)


ggsave(sa2_combined_grobs,file = "C:/Users/bmart/OneDrive - Johns Hopkins/JHU Post-Doc/SemaglutideNAION/Sensitivity2_plot_100224.eps", device = "eps", width = 70, height = 20, units = "cm")

