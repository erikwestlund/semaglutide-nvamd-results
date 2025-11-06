
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
sa1_results <- read.csv("C:/Users/bmart/OneDrive - Johns Hopkins/JHU Post-Doc/SemaglutideNAION/cm-scss-results2/results-cm-sensitivity-analysis-1.csv")

# Convert relevant columns to numeric (handling '<5' as NA)
sa1_results$hr <- as.numeric(as.character(sa1_results$hr))
sa1_results$hr_lb <- as.numeric(as.character(sa1_results$hr_lb))
sa1_results$hr_ub <- as.numeric(as.character(sa1_results$hr_ub))

# Filter out rows where hr_lb or hr_ub is missing
sa1_results <- sa1_results %>% filter(!is.na(hr_lb) & !is.na(hr_ub))

# Fix copyright symbol unicode
sa1_results$database_name <- stringr::str_replace_all(sa1_results$database_name,"\xae","")
sa1_results$database_name <- stringr::str_replace_all(sa1_results$database_name,"Bayesian Synthesis","Meta-analysis")
sa1_results$comparison_name <- stringr::str_replace_all(sa1_results$comparison_name,"New user of semaglutide with prior T2DM and prior metformin and no insulin","Semaglutide")
sa1_results$comparison_name <- stringr::str_replace_all(sa1_results$comparison_name,"New user of dulaglutide with prior T2DM and prior metformin and no insulin","Dulaglutide")
sa1_results$comparison_name <- stringr::str_replace_all(sa1_results$comparison_name,"New user of empagliflozin with prior T2DM and prior metformin and no insulin","Empagliflozin")
sa1_results$comparison_name <- stringr::str_replace_all(sa1_results$comparison_name,"New user of sitagliptin with prior T2DM and prior metformin and no insulin","Sitagliptin")
sa1_results$comparison_name <- stringr::str_replace_all(sa1_results$comparison_name,"New user of glipizide with prior T2DM and prior metformin and no insulin","Glipizide")

# Sum N by exposure
sa1_results <- sa1_results %>% 
  group_by(comparator_name, outcome_name) %>% 
  mutate(exposure_n = max(group_n)) %>% 
  ungroup()

# Concatenate label for exposure with N
sa1_results <- sa1_results %>% 
  mutate(exposure_label = paste0(comparison_name, " (N=", exposure_n, ")")) %>% 
  mutate(exposure_label = str_replace_all(exposure_label,"vs.","v."))

# Order the bars
sa1_results_ordered <- sa1_results %>%
  arrange(database_order) %>%  # Sort by your custom column
  mutate(database_name = factor(database_name, levels = rev(unique(database_name)))) 

# sa1_results_ordered <- sa1_results_ordered %>%
#   arrange(database_order) %>%  # Sort by your custom column
#   mutate(comparator_name = factor(comparator_name, levels = unique(comparator_name))) %>% 
#   mutate(exposure_label = factor(exposure_label, levels = unique(exposure_label)))



# Separate Sensitivity and Specificity HR calculations
sa1_results_sens <- sa1_results_ordered %>% filter(outcome_id==17760)
sa1_results_spec <- sa1_results_ordered %>% filter(outcome_id==17761)



# Create the forest plot for Sensitive NAION
sa1_sens <- ggplot(sa1_results_sens, aes(x = hr, y = database_name, xmin = hr_lb, xmax = hr_ub,
                                       color = ifelse(database_name == "Meta-analysis", "firebrick3", "dodgerblue3"))) +
  geom_point(size = 2.8) +
  geom_errorbarh(height = 0.5, linewidth=1.1) +
  geom_vline(xintercept = 1, linetype = "solid", color = "black") +
  facet_wrap(~ exposure_label, scales = "free_y",ncol = 1) +
  scale_x_log10(limits = c(0.01,60), breaks = c(0.01,0.05, 0.2, 0.5, 1, 2, 4,20), labels = c("0.01","0.05","0.2", "0.5", "1", "2", "4", "20")) +
  scale_color_identity() +
  labs(title = "Sensitive NAION",
       x = "Hazard Ratio (95% CI)", y = "") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(size = 24, hjust = 0.5),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 16, margin = margin(t=15)),
        strip.text = element_text(size = 16),
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
            hjust = -0.1, size = 4.5, color = "black", check_overlap = TRUE)


# Create the forest plot for Specific NAION
sa1_spec <- ggplot(sa1_results_spec, aes(x = hr, y = database_name, xmin = hr_lb, xmax = hr_ub,
                                       color = ifelse(database_name == "Meta-analysis", "firebrick3", "dodgerblue3"))) +
  geom_point(size = 2.8) +
  geom_errorbarh(height = 0.5, linewidth=1.1) +
  geom_vline(xintercept = 1, linetype = "solid", color = "black") +
  facet_wrap(~ exposure_label, scales = "free_y",ncol = 1) +
  scale_x_log10(limits = c(0.004,800), breaks = c(0.01,0.05, 0.2, 0.5, 1, 2, 4,20), labels = c("0.01","0.05","0.2", "0.5", "1", "2", "4", "20")) +
  scale_color_identity() +
  labs(title = "Specific NAION",
       x = "Hazard Ratio (95% CI)", y = "") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(size = 24, hjust = 0.5),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 16, margin = margin(t=15)),
        strip.text = element_text(size = 16),
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
            hjust = -0.1, size = 4.5, color = "black", check_overlap = TRUE)



# Convert ggplot objects to grobs using ggplotGrob()
sa1_sens_grob <- ggplotGrob(sa1_sens)
sa1_spec_grob <- ggplotGrob(sa1_spec)



sa1_combined_grobs <- grid.arrange(
  grobTree(rectGrob(gp = gpar(lwd = 2, col = "black")), sa1_sens_grob),  # Add border around plot1
  grobTree(rectGrob(gp = gpar(lwd = 2, col = "black")), sa1_spec_grob),  # Add border around plot2
  ncol = 2)



ggsave(sa1_combined_grobs,file = "C:/Users/bmart/OneDrive - Johns Hopkins/JHU Post-Doc/SemaglutideNAION/Sensitivity1_plot_100224.eps", device = "eps", width = 70, height = 30, units = "cm")




