# Current question:
# 3. Did the soil surface or seeding treatments or other abiotic treatments improve the (unseeded) native or non-native plant community establishment over time?
# "Q3-1.1" means Question 3, first season, response variable 1: "seedling_count"

rm(list = ls())
library(readxl)
library(tidyverse)
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
FullDatawithZeros10_7_2024 <- read_excel("FullDatawithZeros10_7_2024.xlsx")
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


Model1All_filtered <- filter(Model1All, (Seededornot == "N"))
Model1All_filtered_summary <- Model1All_filtered %>%
  group_by(Site, Plot, seasonyear, SoilSurfaceTreat, Nativity) %>%
  summarise(sum_count = sum(Seedling_Count, na.rm = TRUE),
            mean_height = mean(Avg_Height_mm, na.rm = TRUE),
            Whole_plot_cover = sum(Whole_plot_cover, na.rm = TRUE))

library(tidyverse)
full_combinations <- expand_grid(
  Site = unique(Model1All$Site),
  Plot = unique(Model1All$Plot),
  seasonyear = unique(Model1All$seasonyear),
  SoilSurfaceTreat = unique(Model1All$SoilSurfaceTreat),
  Nativity = unique(Model1All$Nativity)
  )

filled <- full_combinations %>% 
  left_join(Model1All_filtered_summary, by = c("Site", "Plot", "seasonyear", "SoilSurfaceTreat", "Nativity"))

Model1All_filtered_summary_filled <- filled %>% 
  mutate(across(c(sum_count, mean_height, Whole_plot_cover), ~ replace_na(.x, 0)))

Model3_a1_filtered <- filter(Model1All_filtered_summary_filled, (seasonyear == "spring 2020"))
Model3_a1_filtered_native <- filter(Model3_a1_filtered, (Nativity == "native"))
Model3_a1_filtered_nonnative <- filter(Model3_a1_filtered, (Nativity == "nonnative"))


# univariate analysis
##### data = Model3_a1_filtered
library(glmmTMB)
univar_models <- list()
predictor_vars <- c("Site", "SoilSurfaceTreat", "Nativity")

for (var in predictor_vars) {
  formula <- as.formula(paste("sum_count ~", var))
  univar_models[[var]] <- glmmTMB(formula, data = Model3_a1_filtered, 
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
Model3_a1_native <- glmmTMB(sum_count ~   Site+SoilSurfaceTreat
                            +(1|Site) +(1|Plot),
                            data = Model3_a1_filtered_native,
                            ziformula = ~1,
                            family = nbinom1)
summary(Model3_a1_native)
Anova(Model3_a1_native, type = "III") 

Model3_a1_nonnative <- glmmTMB(sum_count ~   Site+SoilSurfaceTreat
                               +Site:SoilSurfaceTreat
                               +(1|Site) +(1|Plot),
                            data = Model3_a1_filtered_nonnative,
                            ziformula = ~1,
                            family = nbinom1)
summary(Model3_a1_nonnative)
Anova(Model3_a1_nonnative, type = "III") 


# pairwise comparisons
library(sjPlot)
tab_model(Model3_a1_native)
tab_model(Model3_a1_nonnative)

library(emmeans)
library(multcomp)
marginal = emmeans(Model3_a1_native, ~ Site)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs)

marginal = emmeans(Model3_a1_native, ~ SoilSurfaceTreat)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs)  

marginal = emmeans(Model3_a1_nonnative, ~ Site)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs)

marginal = emmeans(Model3_a1_nonnative, ~ Site|SoilSurfaceTreat)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs)


# generalize plot about Averaged Total Seedling Numbers of Unseeded Species in the Pre-drought Season by Site and Surface Treatments
library(ggplot2)
Model3_a1_filtered_summary <- Model3_a1_filtered %>%
  group_by(Site, SoilSurfaceTreat, Nativity) %>%
  summarise(mean_sum_count = mean(sum_count, na.rm = TRUE),
            SE_sum_count = sd(sum_count, na.rm = TRUE) / sqrt(n()))

plot3_a1 <- ggplot(Model3_a1_filtered_summary, aes(x = Site, y = mean_sum_count, fill = SoilSurfaceTreat)) +
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.6) +
  geom_errorbar(aes(ymin = mean_sum_count - SE_sum_count, ymax = mean_sum_count + SE_sum_count), 
                position = position_dodge(0.7), width = 0.2) +
  facet_grid(Nativity ~ .) +
  labs(y = "Unseeded speceis density (#/subplot)",
       x = "Site",
       fill = "Surface treatments") +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "lightgrey", color = NA),
        strip.text.y = element_text(angle = -90))
