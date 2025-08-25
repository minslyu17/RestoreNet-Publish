# Current question:
# 1. Do certain surface treatments or seed mix types (cool vs warm) improve establishment of seeded species and natural native plant regeneration? Does this differ by site? 
# "Q1-1.3" means Question 1, first season, response variable 3: "Whole_plot_cover"

rm(list = ls())
library(readxl)
library(dplyr)
library(glmmTMB)
library(sjPlot)
library(emmeans)
library(multcomp)
library(car)
library(ggplot2)


# data preparation(use the same dataset as in Q1-1.1)
Model1_a2_filtered

# univariate analysis
library(glmmTMB)
univar_models1_a2 <- list()
predictor_vars <- c("MAP_mm", "MAT", "elevation", "PPT_since_lastsampling_mm", "total_PPT", "cumulative_PPT", "SeedTreat", "SoilSurfaceTreat", "MAP95", "Site","MAT95")

for (var in predictor_vars) {
  formula <- as.formula(paste("Whole_plot_cover ~", var))
  univar_models1_a2[[var]] <- glmmTMB(formula, data = Model1_a2_filtered, 
                                      ziformula = ~1,
                                      family = nbinom1)}
for (i in seq_along(predictor_vars)) {
  cat("Summary for model with predictor variable:", predictor_vars[i], "\n")
  print(summary(univar_models1_a2[[i]]))
  cat("\n\n")}
AIC_values1_a2 <- sapply(univar_models1_a2, function(model) AIC(model))
print(AIC_values1_a2)
best_model_index1_a2 <- which.min(AIC_values1_a2)
print(best_model_index1_a2)


# model
library(glmmTMB)
Model1_a3 <- glmmTMB(Whole_plot_cover ~   Site+SeedTreat+SoilSurfaceTreat
                        +(1|Site) +(1|SitePlot),
                        data = Model1_a2_filtered,
                        family = nbinom1)
summary(Model1_a3)
Anova(Model1_a3, type = "III") 


# pairwise comparisons
library(sjPlot)
tab_model(Model1_a3)

library(emmeans)
library(multcomp)
marginal = emmeans(Model1_a3, ~ SeedTreat)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs)


# generalize plot about Average Whole Plot Coverage in the Pre-Drought Season by Seed Treatments
library(ggplot2)
Model1_a3_filtered_summary <- Model1_a2_filtered %>%
  group_by(Seed_Mix) %>%
  summarise(mean_cover = mean(Whole_plot_cover, na.rm = TRUE),
            SE_cover = sd(Whole_plot_cover, na.rm = TRUE) / sqrt(n()))

plot1_a3 = ggplot(Model1_a3_filtered_summary, aes(x = Seed_Mix, y = mean_cover)) +
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.6) +
  geom_errorbar(aes(ymin = mean_cover - SE_cover, ymax = mean_cover + SE_cover), 
                position = position_dodge(0.7), width = 0.2) +
  labs(y = "Whole plot cover (%)",
       x = "Seed Treatments") +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "lightgrey", color = NA),
    strip.text.y = element_text(angle = 0))
