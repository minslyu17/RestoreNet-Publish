# Current question:
# 2. Did certain species do better than others? Did this differ by site? Was there a treatment by species effect? Could this be predicted by their MAT95 or their MAP95?
# "Q2-2.3" means Question 2, last 3 seasons, response variable 3: "Whole_plot_cover", include species(MAT95) as a fixed effect

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

# data preparation(use the same dataset as in Q2-2.1)
Model2_b1_filtered

library(dplyr)
Model2_b2_filtered <- Model2_b1_filtered %>% mutate(growth = Seedling_Count * Avg_Height_mm)
summary(Model2_b2_filtered)

levels(Model2_b2_filtered$Species)
Model2_b2_filtered$Species <- fct_drop(Model2_b2_filtered$Species)
levels(Model2_b2_filtered$Species)

Model2_b2_filtered_spring <- filter(Model2_b2_filtered,(Season == "spring 2021"|Season == "spring 2022"))
Model2_b2_filtered_spring_summary <- Model2_b2_filtered_spring %>%
  group_by(Species) %>%
  summarise(sum = sum(Seedling_Count))
Model2_b2_filtered_spring <- filter(Model2_b2_filtered_spring,
                                    Species != "BOAR", Species != "BORO2",
                                    Species != "ARPUP6", Species != "BAMU")

Model2_b2_filtered_fall <- filter(Model2_b2_filtered,(Season == "fall 2021"))
Model2_b2_filtered_fall_summary <- Model2_b2_filtered_fall %>%
  group_by(Species) %>%
  summarise(sum = sum(Seedling_Count))
Model2_b2_filtered_fall <- filter(Model2_b2_filtered_fall,
                                  Species != "ARPUP6", Species != "BAMU", Species != "DICA8", Species != "LUSP2",
                                  Species != "PLOV", Species != "SACO6", Species != "SPAMA")

Model2_b2_filtered_spring
Model2_b2_filtered_fall


# univariate analysis
library(glmmTMB)
##### data = Model2_b2_filtered_spring
univar_models <- list()
predictor_vars <- c("MAP_mm", "Monsoonoftotal", "MAT", "elevation", "PPT_since_lastsampling_mm", "total_PPT", "cumulative_PPT", "SeedTreat", "SoilSurfaceTreat", "MAP95", "Site","MAT95","Species")

for (var in predictor_vars) {
  formula <- as.formula(paste("Whole_plot_cover ~", var))
  univar_models[[var]] <- glmmTMB(formula, data = Model2_b2_filtered_spring, 
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

##### data = Model2_b2_filtered_fall
univar_models <- list()
predictor_vars <- c("MAP_mm", "Monsoonoftotal", "MAT", "elevation", "PPT_since_lastsampling_mm", "total_PPT", "cumulative_PPT", "SeedTreat", "SoilSurfaceTreat", "MAP95", "Site","MAT95","Species")

for (var in predictor_vars) {
  formula <- as.formula(paste("Whole_plot_cover ~", var))
  univar_models[[var]] <- glmmTMB(formula, data = Model2_b2_filtered_fall, 
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
Model2_b3_spring <- glmmTMB(Whole_plot_cover ~   Site+SoilSurfaceTreat+MAT95
                               +Site*MAT95
                               +(1|Site) +(1|SitePlot),
                               data = Model2_b2_filtered_spring,
                               family = nbinom1) 
summary(Model2_b3_spring)
Anova(Model2_b3_spring, type = "III") 

Model2_b3_fall <- glmmTMB(Whole_plot_cover ~   Site+SoilSurfaceTreat+MAT95
                             +(1|Site) +(1|SitePlot),
                             data = Model2_b2_filtered_fall,
                             family = nbinom1)
summary(Model2_b3_fall)
Anova(Model2_b3_fall, type = "III") 


# pairwise comparisons
library(sjPlot)
tab_model(Model2_b3_spring)
tab_model(Model2_b3_fall)

library(emmeans)
library(multcomp)
marginal_spring = emmeans(Model2_b3_spring, ~ Site)
contrast(marginal_spring, method = "tukey")
pairwise_IRRs <- contrast(marginal_spring, method = "pairwise", type = "response")
print(pairwise_IRRs)

marginal_spring = emmeans(Model2_b3_spring, ~ SoilSurfaceTreat)
contrast(marginal_spring, method = "tukey")
pairwise_IRRs <- contrast(marginal_spring, method = "pairwise", type = "response")
print(pairwise_IRRs)

marginal_fall = emmeans(Model2_b3_fall, ~ Site)
contrast(marginal_fall, method = "tukey")
pairwise_IRRs <- contrast(marginal_fall, method = "pairwise", type = "response")
print(pairwise_IRRs)


# generalize plot
library(ggplot2)
#### spring
# Average Whole Plot Coverage in the Post-Drought Springs by Species, Site and Surface Treatments
Model2_b3_filtered_spring_summary <- Model2_b2_filtered_spring %>%
  group_by(Site, Species, MAT95, SoilSurfaceTreat) %>%
  summarise(mean_cover = mean(Whole_plot_cover, na.rm = TRUE),
            SE_cover = sd(Whole_plot_cover, na.rm = TRUE) / sqrt(n()))

plot2_b3_spring <- ggplot(Model2_b3_filtered_spring_summary, aes(x = reorder(Species, MAT95), y = mean_cover, fill = MAT95)) +
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.6) +
  geom_errorbar(aes(ymin = mean_cover - SE_cover, ymax = mean_cover + SE_cover), 
                position = position_dodge(0.7), width = 0.2) +
  facet_grid(SoilSurfaceTreat ~ Site) +
  scale_fill_gradient(low="blue", high="red") +
  labs(y = "Whole plot cover (%)",
       x = "Species code",
       fill = "MAT95 (°C)") +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "lightgrey", color = NA),
        strip.text.y = element_text(angle = 0))

#### fall
# Average Whole Plot Coverage in the Post-Drought Fall by Species and Site
Model2_b3_filtered_fall_summary <- Model2_b2_filtered_fall %>%
  group_by(Species, MAT95, Site) %>%
  summarise(mean_cover = mean(Whole_plot_cover, na.rm = TRUE),
            SE_cover = sd(Whole_plot_cover, na.rm = TRUE) / sqrt(n()))

plot2_b3_fall <- ggplot(Model2_b3_filtered_fall_summary, aes(x = reorder(Species, MAT95), y = mean_cover, fill = MAT95)) +
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
