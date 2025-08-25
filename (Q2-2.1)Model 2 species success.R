# Current question:
# 2. Did certain species do better than others? Did this differ by site? Was there a treatment by species effect? Could this be predicted by their MAT95 or their MAP95?
# "Q2-2.1" means Question 2, last 3 seasons, response variable 1: "seedling_count", include species(MAT95) as a fixed effect

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

# data preparation
library(readxl)
FullDatawithZeros10_7_2024 <- read_excel("2024/Data for models/FullDatawithZeros10_7_2024.xlsx")
Model1All <- FullDatawithZeros10_7_2024
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
Model1All <- within( Model1All, {SitePlot <- factor(concatenated)})

## The Model1All used for Q2 should be the one filtered out those species that emerge at wrong plot. Since we are focusing on the species success, those wrong species should be discluded.
Model1All_filtered_coolWrong <- filter(Model1All,(SeedTreat == "Cool") & (Seededornot == "Y_warm"))
Model1All_filtered_warmWrong <- filter(Model1All,(SeedTreat == "Warm") & (Seededornot == "Y_cool"))
Model1All_filtered_wrong <- rbind(Model1All_filtered_coolWrong, Model1All_filtered_warmWrong)

WrongSpeciesOut <- setdiff(Model1All, Model1All_filtered_wrong)
Model1All <- WrongSpeciesOut
summary(Model1All)

library(dplyr)
Model2_b1_filtered <- filter(Model1All, 
                            (Seededornot == "Y_cool"|Seededornot == "Y_warm") &
                              (Season == "spring 2021"|Season == "fall 2021"|Season == "spring 2022") &
                              (SeedTreat == "Cool"|SeedTreat == "Warm") &
                              (SoilSurfaceTreat == "ConMod"|SoilSurfaceTreat == "Mulch"|SoilSurfaceTreat == "Pits"|SoilSurfaceTreat == "Seed only"))

levels(Model2_b1_filtered$SoilSurfaceTreat)
levels(Model2_b1_filtered$Seededornot) 

library(forcats)
Model2_b1_filtered$SoilSurfaceTreat <- fct_drop(Model2_b1_filtered$SoilSurfaceTreat, only = "Control")
levels(Model2_b1_filtered$SoilSurfaceTreat)
Model2_b1_filtered$Seededornot <- fct_drop(Model2_b1_filtered$Seededornot, only = c("N", "N_control"))
levels(Model2_b1_filtered$Seededornot)
summary(Model2_b1_filtered)

Model2_b1_filtered_spring <- filter(Model2_b1_filtered,(Season == "spring 2021"|Season == "spring 2022"))
summary(Model2_b1_filtered_spring)
Model2_b1_filtered_fall <- filter(Model2_b1_filtered,(Season == "fall 2021"))
summary(Model2_b1_filtered_fall)


# univariate analysis
##### data = Model2_b1_filtered_spring
library(glmmTMB)
univar_models <- list()
predictor_vars <- c("MAP_mm", "Monsoonoftotal", "MAT", "elevation", "PPT_since_lastsampling_mm", "total_PPT", "cumulative_PPT", "SeedTreat", "SoilSurfaceTreat", "MAP95", "Site","MAT95","Species")

for (var in predictor_vars) {
  formula <- as.formula(paste("Seedling_Count ~", var))
  univar_models[[var]] <- glmmTMB(formula, data = Model2_b1_filtered_spring, 
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

##### data = Model2_b1_filtered_fall
univar_models <- list()
predictor_vars <- c("MAP_mm", "Monsoonoftotal", "MAT", "elevation", "PPT_since_lastsampling_mm", "total_PPT", "cumulative_PPT", "SeedTreat", "SoilSurfaceTreat", "MAP95", "Site","MAT95","Species")

for (var in predictor_vars) {
  formula <- as.formula(paste("Seedling_Count ~", var))
  univar_models[[var]] <- glmmTMB(formula, data = Model2_b1_filtered_fall, 
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
library(car)
Model2_b1_spring <- glmmTMB(Seedling_Count ~   Site+SoilSurfaceTreat+MAT95
                                      +Site*MAT95
                                      +(1|Site) +(1|SitePlot),
                                      data = Model2_b1_filtered_spring,
                                      family = nbinom1)
summary(Model2_b1_spring)
Anova(Model2_b1_spring, type = "III") 

Model2_b1_fall <- glmmTMB(Seedling_Count ~   Site+SoilSurfaceTreat+MAT95 
                                    + Site*MAT95
                                    +(1|Site) +(1|SitePlot), 
                                     data = Model2_b1_filtered_fall,
                                     family = nbinom1)
summary(Model2_b1_fall)
Anova(Model2_b1_fall, type = "III") 


# pairwise comparisons
library(sjPlot)
tab_model(Model2_b1_spring)
tab_model(Model2_b1_fall)

library(emmeans)
library(multcomp)
marginal_spring = emmeans(Model2_b1_spring, ~ SoilSurfaceTreat)
contrast(marginal_spring, method = "tukey")
pairwise_IRRs <- contrast(marginal_spring, method = "pairwise", type = "response")
print(pairwise_IRRs)

marginal_fall = emmeans(Model2_b1_fall, ~ Site)
contrast(marginal_fall, method = "tukey")
pairwise_IRRs <- contrast(marginal_fall, method = "pairwise", type = "response")
print(pairwise_IRRs)


# generalize plot
library(ggplot2)
library(dpylr)
#### spring
# Average Number of Seedlings in the Post-Drought Fall by Species and Surface Treatments
Model2_b1_filtered_spring_summary <- Model2_b1_filtered_spring %>%
  group_by(Species, SeedTreat, MAT95, SoilSurfaceTreat) %>%
  summarise(mean_count = mean(Seedling_Count, na.rm = TRUE),
            SE_count = sd(Seedling_Count, na.rm = TRUE) / sqrt(n()))

plot2_b1_spring <- ggplot(Model2_b1_filtered_spring_summary, aes(x = reorder(Species, MAT95), y = mean_count, fill = MAT95)) +
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.6) +
  geom_errorbar(aes(ymin = mean_count - SE_count, ymax = mean_count + SE_count), 
                position = position_dodge(0.7), width = 0.2) +
  scale_fill_gradient(low="blue", high="red") +
  facet_grid(SoilSurfaceTreat ~ .) +
  labs(y = "Seeded speceis density (#/subplot)",
       x = "Species code",
       fill = "MAT95 (°C)") +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "lightgrey", color = NA),
        strip.text.y = element_text(angle = 0))

#### fall
# Average Number of Seedlings in the Post-Drought Springs by Species and Site
Model2_b1_filtered_fall_summary <- Model2_b1_filtered_fall %>%
  group_by(Site, Species, SeedTreat, MAT95) %>%
  summarise(mean_count = mean(Seedling_Count, na.rm = TRUE),
            SE_count = sd(Seedling_Count, na.rm = TRUE) / sqrt(n()))

plot2_b1_fall <- ggplot(Model2_b1_filtered_fall_summary, aes(x = reorder(Species, MAT95), y = mean_count, fill = MAT95)) +
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.6) +
  geom_errorbar(aes(ymin = mean_count - SE_count, ymax = mean_count + SE_count), 
                position = position_dodge(0.7), width = 0.2) +
  scale_fill_gradient(low="blue", high="red") +
  facet_grid(Site ~ .) +
  labs(y = "Seeded species density (#/subplot)",
       x = "Species code",
       fill = "MAT95 (°C)") +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "lightgrey", color = NA),
        strip.text.y = element_text(angle = 0))
