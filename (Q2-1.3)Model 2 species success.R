# Current question:
# 2. Did certain species do better than others? Did this differ by site? Was there a treatment by species effect? Could this be predicted by their MAT95 or their MAP95?
# "Q2-1.3" means Question 2, first season, response variable 3: "Whole_plot_cover", include species(MAT95) as a fixed effect

rm(list = ls())
library(readxl)
library(dplyr)
library(forcats)
library(glmmTMB)
library(sjPlot)
library(emmeans)
library(multcomp)
library(car)
library(ggplot2)

# data preparation(use the same dataset as in Q2-1.1)
Model2_a1_filtered

library(dplyr)
Model2_a2_filtered <- Model2_a1_filtered %>% mutate(growth = Seedling_Count * Avg_Height_mm)
summary(Model2_a2_filtered)

levels(Model2_a2_filtered$Species)
Model2_a2_filtered$Species <- fct_drop(Model2_a2_filtered$Species)
levels(Model2_a2_filtered$Species)

Model2_a2_filtered_summary <- Model2_a2_filtered %>%
  group_by(Species) %>%
  summarise(sum = sum(Seedling_Count))
Model2_a2_filtered <- filter(Model2_a2_filtered,
                             Species != "BOAR", Species != "BORO2",
                             Species != "SECO10", Species != "SPAMA")
summary(Model2_a2_filtered)


# univariate analysis
library(glmmTMB)
univar_models <- list()
predictor_vars <- c("MAP_mm", "Monsoonoftotal", "MAT", "elevation", "PPT_since_lastsampling_mm", "total_PPT", "cumulative_PPT", "SeedTreat", "SoilSurfaceTreat", "MAP95", "Site","MAT95","Species")

for (var in predictor_vars) {
  formula <- as.formula(paste("Whole_plot_cover ~", var))
  univar_models[[var]] <- glmmTMB(formula, data = Model2_a2_filtered, 
                                  ziformula = ~1,
                                  family = nbinom1)}
for (i in seq_along(predictor_vars)) {
  cat("Summary for model with predictor variable:", predictor_vars[i], "\n")
  print(summary(univar_models[[i]]))
  cat("\n\n")
}
AIC_values <- sapply(univar_models, function(model) AIC(model))
print(AIC_values)
best_model_index <- which.min(AIC_values)
print(best_model_index)


# model
library(glmmTMB)
Model2_a3 <- glmmTMB(Whole_plot_cover ~   Site+SoilSurfaceTreat+MAT95
                            +(1|SitePlot),
                            data = Model2_a2_filtered,
                            family = nbinom1)
summary(Model2_a3)
Anova(Model2_a3, type = "III") 


# pairwise comparisons
library(sjPlot)
tab_model(Model2_a3)

library(emmeans)
library(multcomp)
marginal = emmeans(Model2_a3, ~ Site)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs)


# generalize plot about Average Whole Plot Coverage in the Pre-Drought Season by Species and Site
library(ggplot2)
Model2_a3_filtered_summary <- Model2_a2_filtered %>%
  group_by(Species, MAT95, Site) %>%
  summarise(mean_cover = mean(Whole_plot_cover, na.rm = TRUE),
            SE_cover = sd(Whole_plot_cover, na.rm = TRUE) / sqrt(n()))

plot2_a3 = ggplot(Model2_a3_filtered_summary, aes(x = reorder(Species, MAT95), y = mean_cover, fill = MAT95)) +
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.6) +
  geom_errorbar(aes(ymin = mean_cover - SE_cover, ymax = mean_cover + SE_cover), 
                position = position_dodge(0.7), width = 0.2) +
  facet_grid(Site ~ .) +
  scale_fill_gradient(low="blue", high="red") +
  labs(y = "Whole plot cover (%)",
       x = "Species code",
       fill = "MAT95 (°C)") +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "lightgrey", color = NA),
        strip.text.y = element_text(angle = 0))
