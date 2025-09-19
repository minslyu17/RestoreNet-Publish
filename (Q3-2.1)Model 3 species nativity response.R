# Current question:
# 3. Did the soil surface or seeding treatments or other abiotic treatments improve the (unseeded) native or non-native plant community establishment over time?
# "Q3-2.1" means Question 3, last 3 seasons, response variable 1: "seedling_count"

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

Model3_b1_filtered <- filter(Model1All_filtered_summary_filled, 
                             (seasonyear == "spring 2021"| seasonyear == "fall 2021"| seasonyear == "spring 2022"))

Model3_b1_filtered_native <- filter(Model3_b1_filtered, (Nativity == "native"))
Model3_b1_filtered_native_spring <- filter(Model3_b1_filtered_native, 
                                    (seasonyear == "spring 2021"| seasonyear == "spring 2022"))
Model3_b1_filtered_native_fall <- filter(Model3_b1_filtered_native, 
                                    (seasonyear == "fall 2021"))

Model3_b1_filtered_nonnative <- filter(Model3_b1_filtered, (Nativity == "nonnative"))
Model3_b1_filtered_nonnative_spring <- filter(Model3_b1_filtered_nonnative, 
                                       (seasonyear == "spring 2021"| seasonyear == "spring 2022"))
Model3_b1_filtered_nonnative_fall <- filter(Model3_b1_filtered_nonnative, 
                                       (seasonyear == "fall 2021"))


# model
library(glmmTMB)
library(car)
Model3_b1_native_spring <- glmmTMB(sum_count ~   Site+SoilSurfaceTreat+seasonyear
                                   +Site:SoilSurfaceTreat
                                   +(1|Site) +(1|Plot),
                            data = Model3_b1_filtered_native_spring,
                            ziformula = ~1,
                            family = nbinom1)
summary(Model3_b1_native_spring)
Anova(Model3_b1_native_spring, type = "III") 

Model3_b1_native_fall <- glmmTMB(sum_count ~   Site+SoilSurfaceTreat
                                 +(1|Site) +(1|Plot),
                            data = Model3_b1_filtered_native_fall,
                            family = nbinom1)
summary(Model3_b1_native_fall)
Anova(Model3_b1_native_fall, type = "III") 

Model3_b1_nonnative_spring <- glmmTMB(sum_count ~   Site+SoilSurfaceTreat+seasonyear
                                      +(1|Site) +(1|Plot),
                               data = Model3_b1_filtered_nonnative_spring,
                               ziformula = ~1,
                               family = nbinom1)
summary(Model3_b1_nonnative_spring)
Anova(Model3_b1_nonnative_spring, type = "III") 

Model3_b1_nonnative_fall <- glmmTMB(sum_count ~   Site+SoilSurfaceTreat
                                    +(1|Site) +(1|Plot),
                            data = Model3_b1_filtered_nonnative_fall,
                            ziformula = ~1,
                            family = nbinom1)
summary(Model3_b1_nonnative_fall)
Anova(Model3_b1_nonnative_fall, type = "III") 


# pairwise comparisons
library(sjPlot)
tab_model(Model3_b1_native_spring)
tab_model(Model3_b1_native_fall)
tab_model(Model3_b1_nonnative_spring)
tab_model(Model3_b1_nonnative_fall)

library(emmeans)
library(multcomp)
marginal = emmeans(Model3_b1_native_spring, ~ SoilSurfaceTreat)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs) 

marginal = emmeans(Model3_b1_native_spring, ~ Site)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs) 

marginal = emmeans(Model3_b1_native_spring, ~ seasonyear)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs) 

marginal = emmeans(Model3_b1_native_spring, ~ Site|SoilSurfaceTreat)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs)


marginal = emmeans(Model3_b1_native_fall, ~ Site)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs) 


marginal = emmeans(Model3_b1_nonnative_spring, ~ SoilSurfaceTreat)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs) 

marginal = emmeans(Model3_b1_nonnative_spring, ~ Site)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs) # !Roo the best

marginal = emmeans(Model3_b1_nonnative_spring, ~ seasonyear)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs)


marginal = emmeans(Model3_b1_nonnative_fall, ~ SoilSurfaceTreat)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs) 


# plot
library(ggplot2)
Model3_b1_filtered_spring <- filter(Model3_b1_filtered, (seasonyear == "spring 2021"| seasonyear == "spring 2022"))
Model3_b1_filtered_fall <- filter(Model3_b1_filtered, (seasonyear == "fall 2021"))

#### spring
# Averaged Total Seedling Numbers of Unseeded Species in the Post-drought Spring Seasons by Site and Surface Treatments
Model3_b1_filtered_spring_summary <- Model3_b1_filtered_spring %>%
  group_by(Site, SoilSurfaceTreat, Nativity, seasonyear) %>%
  summarise(mean_sum_count = mean(sum_count, na.rm = TRUE),
            SE_sum_count = sd(sum_count, na.rm = TRUE) / sqrt(n()))

plot3_b1_spring = ggplot(Model3_b1_filtered_spring_summary, aes(x = Site, y = mean_sum_count, fill = SoilSurfaceTreat)) +
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.6) +
  geom_errorbar(aes(ymin = mean_sum_count - SE_sum_count, ymax = mean_sum_count + SE_sum_count), 
                position = position_dodge(0.7), width = 0.2) +
  facet_grid(Nativity ~ seasonyear) +
  labs(y = "Unseeded speceis density (#/subplot)",
       x = "Site",
       fill = "Surface treatments") +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "lightgrey", color = NA),
        strip.text.y = element_text(angle = -90))

#### fall
# Averaged Total Seedling Numbers of Unseeded Species in the Post-drought Fall Season by Site and Surface Treatments
Model3_b1_filtered_fall_summary <- Model3_b1_filtered_fall %>%
  group_by(Site, Nativity) %>%
  summarise(mean_sum_count = mean(sum_count, na.rm = TRUE),
            SE_sum_count = sd(sum_count, na.rm = TRUE) / sqrt(n()))

plot3_b1_fall = ggplot(Model3_b1_filtered_fall_summary, aes(x = Site, y = mean_sum_count)) +
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.6) +
  geom_errorbar(aes(ymin = mean_sum_count - SE_sum_count, ymax = mean_sum_count + SE_sum_count), 
                position = position_dodge(0.7), width = 0.2) +
  facet_grid(. ~ Nativity) +
  labs(y = "Unseeded speceis density (#/subplot)",
       x = "Site") +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "lightgrey", color = NA),
        strip.text.y = element_text(angle = -90))
