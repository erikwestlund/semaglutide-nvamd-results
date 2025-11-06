
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

# CM Results
cm_results <- read.csv("C:/Users/bmart/OneDrive - Johns Hopkins/JHU Post-Doc/SemaglutideNAION/cm-scss-results2/results-cm.csv") 

# Convert relevant columns to numeric (handling '<5' as NA)
cm_results$hr <- as.numeric(as.character(cm_results$hr))
cm_results$hr_lb <- as.numeric(as.character(cm_results$hr_lb))
cm_results$hr_ub <- as.numeric(as.character(cm_results$hr_ub))

# Filter out rows where hr_lb or hr_ub is missing
cm_results <- cm_results %>% filter(!is.na(hr_lb) & !is.na(hr_ub))

# Fix copyright symbol unicode
cm_results$database_name <- stringr::str_replace_all(cm_results$database_name,"\xae","")
cm_results$database_name <- stringr::str_replace_all(cm_results$database_name,"Bayesian Synthesis","Meta-analysis")

# Filter only Semaglutide comparisons
cm_results <- cm_results %>% 
  filter(target_cohort_id==201)

# Sum N by comparison
cm_results <- cm_results %>% 
  group_by(comparison_name, outcome_name) %>% 
  mutate(comparison_n = 2*max(group_n)) %>% 
  ungroup()

# Concatenate label for comparison with N
cm_results <- cm_results %>% 
  mutate(comparison_label = paste0(comparison_name, " (N=", comparison_n, ")")) %>% 
  mutate(comparison_label = str_replace_all(comparison_label,"vs.","v."))

# Order the bars
cm_results_ordered <- cm_results %>%
  arrange(database_order) %>%  # Sort by your custom column
  mutate(database_name = factor(database_name, levels = rev(unique(database_name)))) 

cm_results_ordered <- cm_results_ordered %>%
  arrange(database_order) %>%  # Sort by your custom column
  mutate(comparison_name = factor(comparison_name, levels = unique(comparison_name))) %>% 
  mutate(comparison_label = factor(comparison_label, levels = unique(comparison_label)))



# Separate Sensitivity and Specificity HR calculations
cm_results_sens <- cm_results_ordered %>% filter(outcome_id==17760)
cm_results_spec <- cm_results_ordered %>% filter(outcome_id==17761)



# Create the forest plot for Sensitive NAION
CM_sens <- ggplot(cm_results_sens, aes(x = hr, y = database_name, xmin = hr_lb, xmax = hr_ub,
                            color = ifelse(database_name == "Meta-analysis", "firebrick3", "dodgerblue3"))) +
                  geom_point(size = 2.8) +
                  geom_errorbarh(height = 0.3, linewidth=1.1) +
                  geom_vline(xintercept = 1, linetype = "solid", color = "black") +
                  facet_wrap(~ comparison_label, scales = "free_y",ncol = 1) +
                  scale_x_log10(limits = c(0.05,300), breaks = c(0.05, 0.2, 0.5, 1, 2, 4,50), labels = c("0.05","0.2", "0.5", "1", "2", "4", "50")) +
                  scale_color_identity() +
                  labs(title = "Sensitive NAION",
                       x = "Hazard Ratio (95% CI)", y = "") +
                  theme_minimal() +
                  theme(panel.grid.minor = element_blank(),
                        plot.title = element_text(size = 24, hjust = 0.5),
                        axis.text.y = element_text(size = 14),
                        axis.text.x = element_text(size = 14),
                        axis.title.x = element_text(size = 16, margin = margin(t=15)),
                        strip.text = element_text(size = 15),
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
CM_spec <- ggplot(cm_results_spec, aes(x = hr, y = database_name, xmin = hr_lb, xmax = hr_ub,
                            color = ifelse(database_name == "Meta-analysis", "firebrick3", "dodgerblue3"))) +
                  geom_point(size = 2.8) +
                  geom_errorbarh(height = 0.3, linewidth=1.1) +
                  geom_vline(xintercept = 1, linetype = "solid", color = "black") +
                  facet_wrap(~ comparison_label, scales = "free_y",ncol = 1) +
                  scale_x_log10(limits = c(0.01,480), breaks = c(0.05, 0.2, 0.5, 1, 2, 4,50), labels = c("0.05","0.2", "0.5", "1", "2", "4", "50")) +
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
CM_sens_grob <- ggplotGrob(CM_sens)
CM_spec_grob <- ggplotGrob(CM_spec)



CM_combined_grobs <- grid.arrange(
                        grobTree(rectGrob(gp = gpar(lwd = 2, col = "black")), CM_sens_grob),  # Add border around plot1
                        grobTree(rectGrob(gp = gpar(lwd = 2, col = "black")), CM_spec_grob),  # Add border around plot2
                             ncol = 2)



ggsave(CM_combined_grobs,file = "C:/Users/bmart/OneDrive - Johns Hopkins/JHU Post-Doc/SemaglutideNAION/CM_plot_100224.eps", device = "eps", width = 70, height = 30, units = "cm")


