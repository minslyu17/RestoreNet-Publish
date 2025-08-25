# Current question:
# 1. Do certain surface treatments or seed mix types (cool vs warm) improve establishment of seeded species and natural native plant regeneration? Does this differ by site? 
# "Q1-2.1" means Question 1, last 3 seasons, response variable 1: "seedling_count"

rm(list = ls())
library(readxl)
library(dplyr)
library(glmmTMB)
library(sjPlot)
library(emmeans)
library(multcomp)
library(car)
library(ggplot2)


# data preparation
library(readxl)
FullDatawithZeros10_7_2024 <- read_excel("2024/Data for models/FullDatawithZeros10_7_2024.xlsx")
View(FullDatawithZeros10_7_2024)
Model1All <- FullDatawithZeros10_7_2024
summary(Model1All)

Model1All <- within(Model1All, 
                    {
                      SoilSurfaceTreat<- factor(Treatment)
                      Species         <- factor(Species_Code)
                      Seededornot     <- factor(Seeded)
                      Site            <- factor(Site_name)
                      Season          <- factor(seasonyear)
                      SeedTreat       <- factor(Seed_Mix)
                      Sandcat         <- factor(Sand_Category)
                      Claycat         <- factor(Clay_Category)
                      Disturbance     <- factor(Disturbance)
                      Native          <- factor(Nativity)
                    })
Model1All$concatenated <- paste(Model1All$Site, Model1All$Plot)
Model1All <- within( Model1All, 
                     {
                       SitePlot <- factor(concatenated)
                     })
summary(Model1All)

Model1_b1_filtered <- filter(Model1All, 
                           (Seededornot == "N_control"|Seededornot == "Y_cool"|Seededornot == "Y_warm") &
                             (Season == "spring 2021"|Season == "fall 2021"|Season == "spring 2022") &
                             (SeedTreat == "Cool"|SeedTreat == "Warm"))

Model1_b1_filtered_spring <- filter(Model1_b1_filtered,(Season == "spring 2021"|Season == "spring 2022"))
summary(Model1_b1_filtered_spring)
Model1_b1_filtered_fall <- filter(Model1_b1_filtered,(Season == "fall 2021"))
summary(Model1_b1_filtered_fall)


# univariate analysis
##### data = Model1_b1_filtered_spring
library(glmmTMB)
univar_models <- list()
predictor_vars <- c("MAP_mm", "Monsoonoftotal", "MAT", "elevation", "PPT_since_lastsampling_mm", "total_PPT", "cumulative_PPT", "SeedTreat", "SoilSurfaceTreat", "MAP95", "Site","MAT95")

for (var in predictor_vars) {
  formula <- as.formula(paste("Seedling_Count ~", var))
  univar_models[[var]] <- glmmTMB(formula, data = Model1_b1_filtered_spring, 
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

##### data = Model1_b1_filtered_fall
library(glmmTMB)
univar_models <- list()
predictor_vars <- c("MAP_mm", "Monsoonoftotal", "MAT", "elevation", "PPT_since_lastsampling_mm", "total_PPT", "cumulative_PPT", "SeedTreat", "SoilSurfaceTreat", "MAP95", "Site","MAT95")

for (var in predictor_vars) {
  formula <- as.formula(paste("Seedling_Count ~", var))
  univar_models[[var]] <- glmmTMB(formula, data = Model1_b1_filtered_fall, 
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
Model1_b1_spring <- glmmTMB(Seedling_Count ~   Site+SeedTreat+SoilSurfaceTreat+Season
                                      +(1|Site) +(1|SitePlot),
                                      data = Model1_b1_filtered_spring,
                                      family = nbinom1)
summary(Model1_b1_spring) 
Anova(Model1_b1_spring, type = "III") 

Model1_b1_fall <- glmmTMB(Seedling_Count ~   Site+SeedTreat+SoilSurfaceTreat
                               +Site:SeedTreat
                               +(1|Site) +(1|SitePlot),
                               data = Model1_b1_filtered_fall,
                               ziformula = ~1,
                               family = nbinom1)
summary(Model1_b1_fall) 
Anova(Model1_b1_fall, type = "III")


# pairwise comparisons
library(sjPlot)
tab_model(Model1_b1_spring)
tab_model(Model1_b1_fall)

library(emmeans)
library(multcomp)
marginal_spring = emmeans(Model1_b1_spring, ~ SoilSurfaceTreat)
contrast(marginal_spring, method = "tukey")
pairwise_IRRs_spring <- contrast(marginal_spring, method = "pairwise", type = "response")
print(pairwise_IRRs_spring)

marginal_spring = emmeans(Model1_b1_spring, ~ SeedTreat)
contrast(marginal_spring, method = "tukey")
pairwise_IRRs_spring <- contrast(marginal_spring, method = "pairwise", type = "response")
print(pairwise_IRRs_spring)

marginal_spring = emmeans(Model1_b1_spring, ~ Season)
contrast(marginal_spring, method = "tukey")
pairwise_IRRs_spring <- contrast(marginal_spring, method = "pairwise", type = "response")
print(pairwise_IRRs_spring)


marginal_fall = emmeans(Model1_b1_fall, ~ Site)
contrast(marginal_fall, method = "tukey")
pairwise_IRRs_fall <- contrast(marginal_fall, method = "pairwise", type = "response")
print(pairwise_IRRs_fall)

marginal_fall = emmeans(Model1_b1_fall, ~ Site|SeedTreat)
contrast(marginal_fall, method = "tukey")
pairwise_IRRs_fall <- contrast(marginal_fall, method = "pairwise", type = "response")
print(pairwise_IRRs_fall)


# generalize plot about Average Number of Seedlings in the Post-Drought Spring Season by Seed and Surface Treatments
# Spring
library(ggplot2)
Model1_b1_filtered_spring_summary <- Model1_b1_filtered_spring %>%
  group_by(Seed_Mix, Treatment, seasonyear) %>%
  summarise(mean_count = mean(Seedling_Count, na.rm = TRUE),
            SE_count = sd(Seedling_Count, na.rm = TRUE) / sqrt(n()))

plot1_b1_Spring <- ggplot(Model1_b1_filtered_spring_summary, aes(x = Seed_Mix, y = mean_count, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.6) +
  geom_errorbar(aes(ymin = mean_count - SE_count, ymax = mean_count + SE_count), 
                position = position_dodge(0.7), width = 0.2) +
  facet_grid(seasonyear ~ .) +
  labs(y = "Seeded speceis density (#/subplot)",
       x = "Seed Treatments",
       fill = "Surface Treatments") +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "lightgrey", color = NA),
        strip.text.y = element_text(angle = 0))
