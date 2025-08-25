# Current question:
# 2. Did certain species do better than others? Did this differ by site? Was there a treatment by species effect? Could this be predicted by their MAT95 or their MAP95?
# "Q2-1.1" means Question 2, first season, response variable 1: "seedling_count", include species(MAT95) as a fixed effect

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
Model1All <- within( Model1All, {SitePlot <- factor(concatenated)})

## The Model1All used for Q2 should be the one filtered out those species that emerge at wrong plot. Since we are focusing on the species success, those wrong species should be discluded.
Model1All_filtered_coolWrong <- filter(Model1All,(SeedTreat == "Cool") & (Seededornot == "Y_warm"))
Model1All_filtered_warmWrong <- filter(Model1All,(SeedTreat == "Warm") & (Seededornot == "Y_cool"))
Model1All_filtered_wrong <- rbind(Model1All_filtered_coolWrong, Model1All_filtered_warmWrong)

WrongSpeciesOut <- setdiff(Model1All, Model1All_filtered_wrong)
Model1All <- WrongSpeciesOut
summary(Model1All)

library(dplyr)
Model2_a1_filtered <- filter(Model1All, 
                            (Seededornot == "Y_cool"|Seededornot == "Y_warm") &
                              (Season == "spring 2020") &
                              (SeedTreat == "Cool"|SeedTreat == "Warm") &
                              (SoilSurfaceTreat == "ConMod"|SoilSurfaceTreat == "Mulch"|SoilSurfaceTreat == "Pits"|SoilSurfaceTreat == "Seed only"))

library(forcats)
Model2_a1_filtered$SoilSurfaceTreat <- fct_drop(Model2_a1_filtered$SoilSurfaceTreat, only = "Control")
levels(Model2_a1_filtered$SoilSurfaceTreat)
Model2_a1_filtered$Seededornot <- fct_drop(Model2_a1_filtered$Seededornot, only = c("N", "N_control"))
levels(Model2_a1_filtered$Seededornot)
summary(Model2_a1_filtered)


# univariate analysis
##### data = Model2_a1_filtered
library(glmmTMB)
univar_models <- list()
predictor_vars <- c("MAP_mm", "Monsoonoftotal", "MAT", "elevation", "PPT_since_lastsampling_mm", "total_PPT", "cumulative_PPT", "SeedTreat", "SoilSurfaceTreat", "MAP95", "Site","MAT95","Species")

for (var in predictor_vars) {
  formula <- as.formula(paste("Seedling_Count ~", var))
  univar_models[[var]] <- glmmTMB(formula, data = Model2_a1_filtered, 
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
Model2_a1 <- glmmTMB(Seedling_Count ~   Site+SoilSurfaceTreat+MAT95 
                                      + Site*MAT95
                                      +(1|Site) +(1|SitePlot),
                                      data = Model2_a1_filtered,
                                      family = nbinom1)
summary(Model2_a1)
Anova(Model2_a1, type = "III") 


# pairwise comparisons
library(sjPlot)
tab_model(Model2_a1)

library(emmeans)
library(multcomp)
marginal = emmeans(Model2_a1, ~ Site)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs)


# generalize plot about Average Number of Seeded Species in the Pre-Drought Season by Species and Site
library(ggplot2)
Model2_a1_filtered_summary <- Model2_a1_filtered %>%
  group_by(Site, Species, MAT95) %>%
  summarise(mean_count = mean(Seedling_Count, na.rm = TRUE),
            SE_count = sd(Seedling_Count, na.rm = TRUE) / sqrt(n()))

plot2_a1 <- ggplot(Model2_a1_filtered_summary, aes(x = reorder(Species, MAT95), y = mean_count, fill = MAT95)) +
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.6) +
  geom_errorbar(aes(ymin = mean_count - SE_count, ymax = mean_count + SE_count), 
                position = position_dodge(0.7), width = 0.2) +
  scale_fill_gradient(low="blue", high="red") +
  facet_grid(Site ~ .) +
  labs(y = "Seeded species density (#/subplot)",
       x = "Species code",
       fill = "MAT95 (°C)" ) +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "lightgrey", color = NA),
        strip.text.y = element_text(angle = 0))
