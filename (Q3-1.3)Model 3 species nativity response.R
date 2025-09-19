# Current question:
# 3. Did the soil surface or seeding treatments or other abiotic treatments improve the (unseeded) native or non-native plant community establishment over time?
# "Q3-1.3" means Question 3, first season, response variable 3: "Total_Plant_Cover(TPC)"

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

Model3_a3_filtered <- filter(Model3All, 
                             (seasonyear == "SPRING2020"),
                             (Site == "Pleasant" | Site == "Preserve" | Site == "Roosevelt"))


# model
library(glmmTMB)
library(car)
Model3_a3_TPC <- glmmTMB(Total_Plant_Cover ~   Site+SoilSurfaceTreat
                        +(1|Site) +(1|Plot),
                        data = Model3_a3_filtered,
                        family = nbinom1)
summary(Model3_a3_TPC)
Anova(Model3_a3_TPC, type = "III")


# pairwise comparisons
library(sjPlot)
tab_model(Model3_a3_TPC)

library(emmeans)
library(multcomp)
marginal = emmeans(Model3_a3_TPC, ~ Site)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs) 


# generalize plot about Averaged Total Plant Coverage of the Pre-Drought Season by Site and Surface Treatments
library(ggplot2)
Model3_a3_filtered_summary_TPC <- Model3_a3_filtered %>%
  group_by(Site) %>%
  summarise(mean_cover = mean(Total_Plant_Cover, na.rm = TRUE),
            SE_cover = sd(Total_Plant_Cover, na.rm = TRUE) / sqrt(n()))

plot3_a3_TPC = ggplot(Model3_a3_filtered_summary_TPC, aes(x = Site, y = mean_cover)) +
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.6) +
  geom_errorbar(aes(ymin = mean_cover - SE_cover, ymax = mean_cover + SE_cover), 
                position = position_dodge(0.7), width = 0.2) +
  labs(y = "Total plant cover (%)",
       x = "Site") +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "lightgrey", color = NA),
        strip.text.y = element_text(angle = -90))
