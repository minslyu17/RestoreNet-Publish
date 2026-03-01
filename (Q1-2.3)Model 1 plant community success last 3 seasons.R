# Current question:
# 1. Do certain surface treatments or seed mix types (cool vs warm) improve establishment of seeded species and natural native plant regeneration? Does this differ by site? 
# "Q1-2.3" means Question 1, last 3 seasons, response variable 3: "Whole_plot_cover"

rm(list = ls())
library(readxl)
library(dplyr)
library(glmmTMB)
library(sjPlot)
library(emmeans)
library(multcomp)
library(car)
library(ggplot2)

# data preparation(use the same dataset as in Q1-2.1)
Model1_b2_filtered_spring 
Model1_b2_filtered_fall

# univariate analysis
##### data = Model1_b2_filtered_spring
library(glmmTMB)
univar_models <- list()
predictor_vars <- c("MAP_mm", "MAT", "elevation", "PPT_since_lastsampling_mm", "total_PPT", "cumulative_PPT", "SeedTreat", "SoilSurfaceTreat", "MAP95", "Site","MAT95")

for (var in predictor_vars) {
  formula <- as.formula(paste("Whole_plot_cover ~", var))
  univar_models[[var]] <- glmmTMB(formula, data = Model1_b2_filtered_spring,
                                  ziformula = ~1,
                                  family = nbinom1)}
for (i in seq_along(predictor_vars)) {
  cat("Summary for model with predictor variable:", predictor_vars[i], "\n")
  print(summary(univar_models[[i]]))
  cat("\n\n")}

AIC_values <- sapply(univar_models, function(model) AIC(model))
print(AIC_values)
best_model_index <- which.min(AIC_values)
print(best_model_index)


##### data = Model1_b2_filtered_fall
univar_models <- list()
predictor_vars <- c("MAP_mm", "MAT", "elevation", "PPT_since_lastsampling_mm", "total_PPT", "cumulative_PPT", "SeedTreat", "SoilSurfaceTreat", "MAP95", "Site","MAT95")

for (var in predictor_vars) {
  formula <- as.formula(paste("Whole_plot_cover ~", var))
  univar_models[[var]] <- glmmTMB(formula, data = Model1_b2_filtered_fall,
                                  ziformula = ~1,
                                  family = nbinom1)}
for (i in seq_along(predictor_vars)) {
  cat("Summary for model with predictor variable:", predictor_vars[i], "\n")
  print(summary(univar_models[[i]]))
  cat("\n\n")}

AIC_values <- sapply(univar_models, function(model) AIC(model))
print(AIC_values)
best_model_index <- which.min(AIC_values)
print(best_model_index)


# model
library(glmmTMB)
Model1_b3_spring <- glmmTMB(Whole_plot_cover ~   Site+SeedTreat+SoilSurfaceTreat+Season
                                       +(1|Site) +(1|SitePlot),
                                       data = Model1_b2_filtered_spring,
                                       family = nbinom1)
summary(Model1_b3_spring)
Anova(Model1_b3_spring, type = "III") 

Model1_b3_fall <- glmmTMB(Whole_plot_cover ~   Site+SeedTreat+SoilSurfaceTreat
                              +(1|Site) +(1|SitePlot),
                              data = Model1_b2_filtered_fall,
                              family = nbinom1)
summary(Model1_b3_fall)
Anova(Model1_b3_fall, type = "III")


# pairwise comparisons
library(sjPlot)
tab_model(Model1_b3_spring)
tab_model(Model1_b3_fall)

library(emmeans)
library(multcomp)
marginal_spring = emmeans(Model1_b3_spring, ~ SeedTreat)
contrast(marginal_spring, method = "tukey")
pairwise_IRRs_spring <- contrast(marginal_spring, method = "pairwise", type = "response")
print(pairwise_IRRs_spring) 

marginal_spring = emmeans(Model1_b3_spring, ~ SoilSurfaceTreat)
contrast(marginal_spring, method = "tukey")
pairwise_IRRs_spring <- contrast(marginal_spring, method = "pairwise", type = "response")
print(pairwise_IRRs_spring) 

marginal_spring = emmeans(Model1_b3_spring, ~ Season)
contrast(marginal_spring, method = "tukey")
pairwise_IRRs_spring <- contrast(marginal_spring, method = "pairwise", type = "response")
print(pairwise_IRRs_spring)  


marginal_fall = emmeans(Model1_b3_fall, ~ Site)
contrast(marginal_fall, method = "tukey")
pairwise_IRRs_fall <- contrast(marginal_fall, method = "pairwise", type = "response")
print(pairwise_IRRs_fall)  

marginal_fall = emmeans(Model1_b3_fall, ~ SeedTreat)
contrast(marginal_fall, method = "tukey")
pairwise_IRRs_fall <- contrast(marginal_fall, method = "pairwise", type = "response")
print(pairwise_IRRs_fall)  


# generalize plot about Average Whole Plot Coverage in the Post-Drought Spring seasons by Seed and Surface Treatments
# spring
library(ggplot2)
Model1_b3_filtered_spring_summary <- Model1_b2_filtered_spring %>%
  group_by(Seed_Mix, Treatment, seasonyear) %>%
  summarise(mean_cover = mean(Whole_plot_cover, na.rm = TRUE),
            SE_cover = sd(Whole_plot_cover, na.rm = TRUE) / sqrt(n()))

library(ggpattern)
treatment_levels <- c("ConMod", "Mulch", "Pits", "Seed only")
pattern_values <- c("none", "stripe", "crosshatch", "circle")  
fill_values <- c("gray90", "gray70", "gray50", "gray30") 

plot1_b3_spring <- ggplot(Model1_b3_filtered_spring_summary, aes(x = Seed_Mix, y = mean_cover, fill = Treatment, pattern = Treatment)) +
  geom_bar_pattern(stat = "identity", position = position_dodge(0.7), width = 0.6,color = "black",
                   pattern_fill = "black", pattern_angle = 45, pattern_density = 0.1,
                   pattern_spacing = 0.05) +  
  scale_pattern_manual(values = setNames(pattern_values, treatment_levels)) +
  scale_fill_manual(values = setNames(fill_values, treatment_levels)) +
  geom_errorbar(aes(ymin = mean_cover - SE_cover, ymax = mean_cover + SE_cover), 
                position = position_dodge(0.7), width = 0.2) +
  facet_grid(seasonyear ~ .) +
  labs(y = "Seeded species cover (%)",
       x = "Seed Mixes",  
       fill = "Surface Treatments",
       pattern = "Surface Treatments") +
  theme_test()
