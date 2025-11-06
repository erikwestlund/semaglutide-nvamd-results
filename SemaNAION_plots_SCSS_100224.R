
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("gridExtra")
#install.packages("patchwork")
library(ggplot2)
library(dplyr)
library(gridExtra)
library(patchwork)
library(grid)


# SCSS Results
scss_results <- read.csv("C:/Users/bmart/OneDrive - Johns Hopkins/JHU Post-Doc/SemaglutideNAION/cm-scss-results2/results-scss.csv")

# Convert relevant columns to numeric (handling '<5' as NA)
scss_results$hr <- as.numeric(as.character(scss_results$hr))
scss_results$hr_lb <- as.numeric(as.character(scss_results$hr_lb))
scss_results$hr_ub <- as.numeric(as.character(scss_results$hr_ub))

# Filter out rows where hr_lb or hr_ub is missing
scss_results <- scss_results %>% filter(!is.na(hr_lb) & !is.na(hr_ub))

# Fix copyright symbol unicode
scss_results$database_name <- stringr::str_replace_all(scss_results$database_name,"\xae","")
scss_results$database_name <- stringr::str_replace_all(scss_results$database_name,"Bayesian Synthesis","Meta-analysis")
scss_results$database_name <- stringr::str_replace_all(scss_results$database_name,"WU","WashU")

# Sum N by exposure
scss_results <- scss_results %>% 
  group_by(exposure_name, outcome_name) %>% 
  mutate(exposure_n = max(outcome_subjects)) %>% 
  ungroup()

# Concatenate label for exposure with N
scss_results <- scss_results %>% 
  mutate(exposure_label = paste0(exposure_name, " (N=", exposure_n, ")"))

# Order the bars
scss_results_ordered <- scss_results %>%
  arrange(database_order) %>%  # Sort by your custom column
  mutate(database_name = factor(database_name, levels = rev(unique(database_name)))) 

scss_results_ordered <- scss_results_ordered %>%
  arrange(database_order) %>%  # Sort by your custom column
  mutate(exposure_name = factor(exposure_name, levels = unique(exposure_name))) %>% 
  mutate(exposure_label = factor(exposure_label, levels = unique(exposure_label)))



# Separate Sensitivity and Specificity HR calculations
scss_results_sens <- scss_results_ordered %>% filter(outcome_id==17760)
scss_results_spec <- scss_results_ordered %>% filter(outcome_id==17761)



# Create the forest plot for Sensitive NAION
SCSS_sens <- ggplot(scss_results_sens, aes(x = hr, y = database_name, xmin = hr_lb, xmax = hr_ub,
                                       color = ifelse(database_name == "Meta-analysis", "firebrick3", "dodgerblue3"))) +
  geom_point(size = 2.8) +
  geom_errorbarh(height = 0.5, linewidth=1.1) +
  geom_vline(xintercept = 1, linetype = "solid", color = "black") +
  facet_wrap(~ exposure_label, scales = "free_y",ncol = 1) +
  scale_x_log10(limits = c(0.01,120), breaks = c(0.01,0.05, 0.2, 0.5, 1, 2, 4,20), labels = c("0.01","0.05","0.2", "0.5", "1", "2", "4", "20")) +
  scale_color_identity() +
  labs(title = "Sensitive NAION",
       x = "Incidence Rate Ratio (95% CI)", y = "") +
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
                               paste0("IRR: ", 
                                      sub("^0+", "", ifelse(hr >= 0.01, format(round(hr, 2)), format(round(hr, 3)))), 
                                      " (", 
                                      sub("^0+", "", ifelse(hr_lb >= 0.01, format(round(hr_lb, 2)), format(round(hr_lb, 3)))), 
                                      "-", 
                                      sub("^0+", "", ifelse(hr_ub >= 0.01, format(round(hr_ub, 2)), format(round(hr_ub, 3)))), 
                                      "), P<0.001"),
                               paste0("IRR: ", 
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
SCSS_spec <- ggplot(scss_results_spec, aes(x = hr, y = database_name, xmin = hr_lb, xmax = hr_ub,
                                       color = ifelse(database_name == "Meta-analysis", "firebrick3", "dodgerblue3"))) +
  geom_point(size = 2.8) +
  geom_errorbarh(height = 0.5, linewidth=1.1) +
  geom_vline(xintercept = 1, linetype = "solid", color = "black") +
  facet_wrap(~ exposure_label, scales = "free_y",ncol = 1) +
  scale_x_log10(limits = c(0.004,2000), breaks = c(0.01,0.05, 0.2, 0.5, 1, 2, 4,20), labels = c("0.01","0.05","0.2", "0.5", "1", "2", "4", "20")) +
  scale_color_identity() +
  labs(title = "Specific NAION",
       x = "Incidence Rate Ratio (95% CI)", y = "") +
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
                               paste0("IRR: ", 
                                      sub("^0+", "", ifelse(hr >= 0.01, format(round(hr, 2)), format(round(hr, 3)))), 
                                      " (", 
                                      sub("^0+", "", ifelse(hr_lb >= 0.01, format(round(hr_lb, 2)), format(round(hr_lb, 3)))), 
                                      "-", 
                                      sub("^0+", "", ifelse(hr_ub >= 0.01, format(round(hr_ub, 2)), format(round(hr_ub, 3)))), 
                                      "), P<0.001"),
                               paste0("IRR: ", 
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
SCSS_sens_grob <- ggplotGrob(SCSS_sens)
SCSS_spec_grob <- ggplotGrob(SCSS_spec)



SCSS_combined_grobs <- grid.arrange(
  grobTree(rectGrob(gp = gpar(lwd = 2, col = "black")), SCSS_sens_grob),  # Add border around plot1
  grobTree(rectGrob(gp = gpar(lwd = 2, col = "black")), SCSS_spec_grob),  # Add border around plot2
  ncol = 2)


ggsave(SCSS_combined_grobs,file = "C:/Users/bmart/OneDrive - Johns Hopkins/JHU Post-Doc/SemaglutideNAION/SCCS_plot_100224.eps", device = "eps", width = 70, height = 40, units = "cm")

