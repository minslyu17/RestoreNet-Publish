# Current question:
# 1. Do certain surface treatments or seed mix types (cool vs warm) improve establishment of seeded species and natural native plant regeneration? Does this differ by site? 
# "Q1-1.1" means Question 1, first season, response variable 1: "seedling_count"

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
FullDatawithZeros10_7_2024 <- read_excel("FullDatawithZeros10_7_2024.xlsx")
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
Model1All <- within( Model1All, {SitePlot <- factor(concatenated)})
summary(Model1All)

Model1_a1_filtered <- filter(Model1All, 
                             (Seededornot == "N_control"|Seededornot == "Y_cool"|Seededornot == "Y_warm") &
                             (Season == "spring 2020") &
                             (SeedTreat == "Cool"|SeedTreat == "Warm"))
summary(Model1_a1_filtered) 


# univariate analysis
library(glmmTMB)
univar_models1_a1 <- list()
predictor_vars <- c("MAP_mm", "Monsoonoftotal", "MAT", "elevation", "PPT_since_lastsampling_mm", "total_PPT", "cumulative_PPT", "SeedTreat", "SoilSurfaceTreat", "MAP95", "Site","MAT95")

for (var in predictor_vars) {
  formula <- as.formula(paste("Seedling_Count ~", var))
  univar_models1_a1[[var]] <- glmmTMB(formula, data = Model1_a1_filtered, 
                                     ziformula = ~1,
                                     family = nbinom1)}
for (i in seq_along(predictor_vars)) {
  cat("Summary for model with predictor variable:", predictor_vars[i], "\n")
  print(summary(univar_models1_a1[[i]]))
  cat("\n\n")}
AIC_values1_a1 <- sapply(univar_models1_a1, function(model) AIC(model))
print(AIC_values1_a1)
best_model_index1_a1 <- which.min(AIC_values1_a1)
print(best_model_index1_a1)


# model
library(glmmTMB)
library(car)
Model1_a1 <- glmmTMB(Seedling_Count ~   Site+SeedTreat+SoilSurfaceTreat
                     +(1|Site) +(1|SitePlot),
                     data = Model1_a1_filtered,
                     family = nbinom1)
summary(Model1_a1) 
Anova(Model1_a1, type = "III") 


# pairwise comparisons
library(sjPlot)
tab_model(Model1_a1)

library(emmeans)
library(multcomp)
marginal = emmeans(Model1_a1, ~ SeedTreat)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs)


# generalize plot about Average Number of Seedlings in the Pre-Drought Season by Seed Treatments
library(ggplot2)
Model1_a1_filtered_summary <- Model1_a1_filtered %>%
  group_by(Seed_Mix, Treatment) %>%
  summarise(mean_count = mean(Seedling_Count, na.rm = TRUE),
            SE_count = sd(Seedling_Count, na.rm = TRUE) / sqrt(n()))

library(ggpattern)
treatment_levels <- c("ConMod", "Mulch", "Pits", "Seed only")
pattern_values <- c("none", "stripe", "crosshatch", "circle")  
fill_values <- c("gray90", "gray70", "gray50", "gray30")

plot1_a1 = ggplot(Model1_a1_filtered_summary, aes(x = Seed_Mix, y = mean_count, fill = Treatment, pattern = Treatment)) +
  geom_bar_pattern(stat = "identity", position = position_dodge(0.7), width = 0.6,color = "black",                     pattern_fill = "black", pattern_angle = 45, pattern_density = 0.1,                                  pattern_spacing = 0.05) +  
  scale_pattern_manual(values = setNames(pattern_values, treatment_levels)) +
  scale_fill_manual(values = setNames(fill_values, treatment_levels)) +
  geom_errorbar(aes(ymin = mean_count - SE_count, ymax = mean_count + SE_count), 
                position = position_dodge(0.7), width = 0.2) +
  labs(y = "Seeded speceis density (#/subplot)",
       x = "Seed Mixes",  
       fill = "Surface Treatments",
       pattern = "Surface Treatments") +
  annotate("text", x = -Inf, y = 2.52,
           label = "atop('   Seed Mixes: ' ~ italic(p) < 0.001)",
           parse = TRUE, hjust = 0, vjust = 1, size = 2) +
  annotate("text", x = -Inf, y = 2.4,
           label = "atop('   Surface Treatments: ' ~ italic(p) == 0.683)",
           parse = TRUE, hjust = 0, vjust = 1, size = 2) +
  theme_test()
