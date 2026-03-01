# Current question:
# 3. Did the soil surface or seeding treatments or other abiotic treatments improve the native or non-native plant community establishment over time?
# "Q3-2.3" means Question 3, last 3 season3, response variable 3: "Total_Plant_Cover(TPC)"

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
WholePlotCoverSeededUnseeded <- read_excel("WholePlotCoverSeededUnseeded.xlsx")
Model3All <- WholePlotCoverSeededUnseeded
Model3All <- within(Model3All, 
                    {
                      Site            <- factor(Site)
                      seasonyear      <- factor(SeasonYear)
                      Plot            <- factor(Plot)
                      Siteplot        <- factor(SitePlot)
                      SoilSurfaceTreat<- factor(Treatment)
                      SeedTreat       <- factor(Seed_Mix)
                    })
summary(Model3All)

Model3_b3_filtered <- filter(Model3All, 
                             (Site == "Pleasant"|Site =="Preserve"|Site == "Roosevelt"),
                             (seasonyear == "SPRING2021"|seasonyear == "SPRING2022"|seasonyear == "AUTUMN2021"))

Model3_b3_filtered_spring <- filter(Model3_b3_filtered, 
                                    (seasonyear == "SPRING2021"|seasonyear == "SPRING2022"))
Model3_b3_filtered_fall <- filter(Model3_b3_filtered, 
                                  (seasonyear == "AUTUMN2021"))


# model
library(glmmTMB)
library(car)
Model3_b3_TPC_spring <- glmmTMB(Total_Plant_Cover ~   Site+SoilSurfaceTreat+seasonyear
                            + Site:seasonyear
                            +(1|Site) +(1|Plot),
                            data = Model3_b3_filtered_spring,
                            family = nbinom1)
summary(Model3_b3_TPC_spring)
Anova(Model3_b3_TPC_spring, type = "III") 

Model3_b3_TPC_fall <- glmmTMB(Total_Plant_Cover ~   Site+SoilSurfaceTreat
                          +(1|Site) +(1|Plot),
                          data = Model3_b3_filtered_fall,
                          family = nbinom1)
summary(Model3_b3_TPC_fall)
Anova(Model3_b3_TPC_fall, type = "III") 


# pairwise comparisons
library(sjPlot)
tab_model(Model3_b3_TPC_spring)
tab_model(Model3_b3_TPC_fall)

library(emmeans)
library(multcomp)
marginal = emmeans(Model3_b3_TPC_spring, ~ SoilSurfaceTreat)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs) 

marginal = emmeans(Model3_b3_TPC_spring, ~ Site)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs) 

marginal = emmeans(Model3_b3_TPC_spring, ~ seasonyear)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs) 

marginal = emmeans(Model3_b3_TPC_spring, ~ Site|seasonyear)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs) 

marginal = emmeans(Model3_b3_TPC_fall, ~ Site)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs) 


# plot
library(ggplot2)
#### spring
# Averaged Total Plant Coverage of the Post-Drought Spring Seasons by Site and Surface Treatments
Model3_b3_filtered_spring_summary_TPC <- Model3_b3_filtered_spring %>%
  group_by(Site, seasonyear, SoilSurfaceTreat) %>%
  summarise(mean_TPC = mean(Total_Plant_Cover, na.rm = TRUE),
            SE_TPC = sd(Total_Plant_Cover, na.rm = TRUE) / sqrt(n()))

library(ggpattern)
treatment_levels <- c("ConMod", "Control","Mulch", "Pits", "Seed only")
pattern_values <- c("none", "stripe", "crosshatch", "circle", "wave")  
fill_values <- c("gray90", "gray75", "gray60", "gray45", "gray25")

plot3_b3_spring_TPC = ggplot(Model3_b3_filtered_spring_summary_TPC, 
  aes(x = Site, y = mean_TPC, fill = SoilSurfaceTreat, pattern = SoilSurfaceTreat)) + 
  geom_bar_pattern(stat = "identity", position = position_dodge(0.7), width = 0.6,color = "black",  
                   pattern_fill = "black", pattern_angle = 45, pattern_density = 0.1,  pattern_spacing = 0.05) +  
  scale_pattern_manual(values = setNames(pattern_values, treatment_levels)) +
  scale_fill_manual(values = setNames(fill_values, treatment_levels)) +
  geom_errorbar(aes(ymin = mean_TPC - SE_TPC, ymax = mean_TPC + SE_TPC), 
                position = position_dodge(0.7), width = 0.2) +
  facet_grid(. ~ seasonyear, labeller = labeller(
             seasonyear = c("SPRING2021" = "spring 2021","SPRING2022" = "spring 2022"))) +
  labs(y = "Unseeded total plant cover (%)",
       x = "Site",
       fill = "Surface Treatments",
       pattern = "Surface Treatments") +
  theme_test()


#### fall
# Averaged Total Plant Coverage of the Post-Drought Fall Season by Site and Surface Treatments
Model3_b3_filtered_fall_summary_TPC <- Model3_b3_filtered_fall %>%
  group_by(Site, SoilSurfaceTreat) %>%
  summarise(mean_TPC = mean(Total_Plant_Cover, na.rm = TRUE),
            SE_TPC = sd(Total_Plant_Cover, na.rm = TRUE) / sqrt(n()))

library(ggpattern)
treatment_levels <- c("ConMod", "Control","Mulch", "Pits", "Seed only")
pattern_values <- c("none", "stripe", "crosshatch", "circle", "wave")  
fill_values <- c("gray90", "gray75", "gray60", "gray45", "gray25")

plot3_b3_fall_TPC = ggplot(Model3_b3_filtered_fall_summary_TPC, 
  aes(x = Site, y = mean_TPC, fill = SoilSurfaceTreat, pattern = SoilSurfaceTreat)) + 
  geom_bar_pattern(stat = "identity", position = position_dodge(0.7), width = 0.6,color = "black",  
                   pattern_fill = "black", pattern_angle = 45, pattern_density = 0.1,  pattern_spacing = 0.05) +  
  scale_pattern_manual(values = setNames(pattern_values, treatment_levels)) +
  scale_fill_manual(values = setNames(fill_values, treatment_levels)) +
  geom_errorbar(aes(ymin = mean_TPC - SE_TPC, ymax = mean_TPC + SE_TPC), 
                position = position_dodge(0.7), width = 0.2) +
  labs(y = "Unseeded total plant cover (%)",
       x = "Site",
       fill = "Surface Treatments",
       pattern = "Surface Treatments") +
  annotate("text", x = -Inf, y = 105,
           label = "atop('  Site: ' ~ italic(p) < 0.001)",
           parse = TRUE, hjust = 0, vjust = 1, size = 2) +
  annotate("text", x = -Inf, y = 100,
           label = "atop('  Surface: ' ~ italic(p) == 0.650)",
           parse = TRUE, hjust = 0, vjust = 1, size = 2) +
  theme_test()
