# Current question:
# 3. Did the soil surface or seeding treatments or other abiotic treatments improve the native or non-native plant community establishment over time?
# "Q3-2.3" means Question 3, last 3 season3, response variable 3
# 3-2.3 after drought? response var: "Unseeded_Cover(UC)" and "Total_Plant_Cover(TPC)"

## clear work space ##
rm(list = ls())

## libraries we're using
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

#------------------------------------------------------------------------------#
#----------#0) data preparation
#------------------------------------------------------------------------------#
# import the dataset about plant coverage
library(readxl)
WholePlotCoverSeededUnseeded <- read_excel("2024/Data for models/WholePlotCoverSeededUnseeded.xlsx")
Model3All <- WholePlotCoverSeededUnseeded

# Set categorical factors for main effects:
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
# there's no nativity, only seeded and unseeded species

Model3_b3_filtered <- filter(Model3All, 
                             (Site == "Pleasant"|Site =="Preserve"|Site == "Roosevelt"),
                             (seasonyear == "SPRING2021"|seasonyear == "SPRING2022"|seasonyear == "AUTUMN2021"))


# separate by seasons
Model3_b3_filtered_spring <- filter(Model3_b3_filtered, 
                                    (seasonyear == "SPRING2021"|seasonyear == "SPRING2022"))
# spring has 1 NA, row 56: preserve 20 spring 2021 ConMod cool, missing Total_Plant_Cover


Model3_b3_filtered_fall <- filter(Model3_b3_filtered, 
                                  (seasonyear == "AUTUMN2021"))
# fall also has 1 NA, row 25: pleasant 25 autumn 2021 Control none, missing Seeded_Cover and Total_Plant_Cover, now these 2 cells are filled with 0



#------------------------------------------------------------------------------#
#----------#1) univariate relationships between individual explanatory and response variable(Unseeded_Cover)
#------------------------------------------------------------------------------#

################################################################################
# Since we are focusing on only 2 factors(Site, SoilSurfaceTreat and Nativity maybe), and they are all going to be included into the model, so no need for univariate analysis
################################################################################

#------------------------------------------------------------------------------#
#----------#2) significant factors to build model
#------------------------------------------------------------------------------#
library(glmmTMB)
library(car)
# sig factors as fix effects
# still add in the random effect site and siteplot 

#UC model
# spring
Model3_b3_UC_spring <- glmmTMB(Unseeded_Cover ~   Site+SoilSurfaceTreat+seasonyear
                         + Site*seasonyear
                         +(1|Site) +(1|Plot),
                         data = Model3_b3_filtered_spring,
                         #ziformula = ~1,
                         family = nbinom1)
summary(Model3_b3_UC_spring)
Anova(Model3_b3_UC_spring, type = "III") # Site and seasonyear are sig

# fall
Model3_b3_UC_fall <- glmmTMB(Unseeded_Cover ~   Site+SoilSurfaceTreat
                         +(1|Site) +(1|Plot),
                         data = Model3_b3_filtered_fall,
                         #ziformula = ~1,
                         family = nbinom1)
summary(Model3_b3_UC_fall)
# zero inflation model: p-value 0.996, thereby remove the term
Anova(Model3_b3_UC_fall, type = "III") # site is sig
# SoilSurfaceTreat(P-value: 0.09)


#TPC model
# spring
Model3_b3_TPC_spring <- glmmTMB(Total_Plant_Cover ~   Site+SoilSurfaceTreat+seasonyear
                            + Site:seasonyear
                            +(1|Site) +(1|Plot),
                            data = Model3_b3_filtered_spring,
                            #ziformula = ~1,
                            family = nbinom1)
summary(Model3_b3_TPC_spring)
# zero inflation model: p-value 0.996, thereby remove the term
Anova(Model3_b3_TPC_spring, type = "III") # site， SoilSurfaceTreat and seasonyear are sig

# fall
Model3_b3_TPC_fall <- glmmTMB(Total_Plant_Cover ~   Site+SoilSurfaceTreat
                          +(1|Site) +(1|Plot),
                          data = Model3_b3_filtered_fall,
                          #ziformula = ~1,
                          family = nbinom1)
summary(Model3_b3_TPC_fall)
Anova(Model3_b3_TPC_fall, type = "III") # only site is sig
#####


#------------------------------------------------------------------------------#
#----------#3) model checks and statistics
#------------------------------------------------------------------------------#
# creates HTML tables from regression models
library(sjPlot)
tab_model(Model3_b3_UC_spring)
tab_model(Model3_b3_UC_fall)
tab_model(Model3_b3_TPC_spring)
tab_model(Model3_b3_TPC_fall)

#### loading post hoc comparisons required libraries
library(emmeans)
library(multcomp)

# Model3_b3_UC_spring - SoilSurfaceTreat(not sig)
# Model3_b3_UC_spring - Site
marginal = emmeans(Model3_b3_UC_spring,
                   ~ Site)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs) #!preserve the best

# Model3_b3_UC_spring - seasonyear
marginal = emmeans(Model3_b3_UC_spring,
                   ~ seasonyear)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs) #! spring 22>spring 21


# Model3_b3_UC_fall - SoilSurfaceTreat(not sig)
# Model3_b3_UC_fall - Site
marginal = emmeans(Model3_b3_UC_fall,
                   ~ Site)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs) # !preserve the best, p-value < 0.0001


# Model3_b3_TPC_spring - SoilSurfaceTreat
marginal = emmeans(Model3_b3_TPC_spring,
                   ~ SoilSurfaceTreat)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs) # no

# Model3_b3_TPC_spring - Site
marginal = emmeans(Model3_b3_TPC_spring,
                   ~ Site)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs) # !preserve the best

# Model3_b3_TPC_spring - seasonyear
marginal = emmeans(Model3_b3_TPC_spring,
                   ~ seasonyear)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs) # !spring 2022 the best

# Model3_b3_TPC_spring - Site*seasonyear
marginal = emmeans(Model3_b3_TPC_spring,
                   ~ Site|seasonyear)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs) 


# Model3_b3_TPC_fall - SoilSurfaceTreat(not sig)
# Model3_b3_TPC_fall - Site
marginal = emmeans(Model3_b3_TPC_fall,
                   ~ Site)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs) # !preserve the best


#------------------------------------------------------------------------------#
# Answer the  question(first season, UC/TPC):

# UC
# spring:
# 1. surface treatments - no sig
# 2. differ by site? - !site, preserve the best
# 3. season - !spring 22>spring 21
# fall:
# 1. surface treatments - no sig 
# 2. differ by site? - !site, preserve the best

# TPC
# spring:
# 1. surface treatments - !Mulch/Pits the best
# 2. differ by site? - !site, preserve the best
# 3. season - !spring 2022 the best
# fall:
# 1. surface treatments - no sig 
# 2. differ by site? - !site, preserve the best



#------------------------ try to plot the results -----------------------------#
library(ggplot2)

#### spring
# UC
Model3_b3_filtered_spring_summary_UC <- Model3_b3_filtered_spring %>%
  group_by(Site, seasonyear) %>%
  summarise(
    mean_cover = mean(Unseeded_Cover, na.rm = TRUE),
    SE_cover = sd(Unseeded_Cover, na.rm = TRUE) / sqrt(n())
  )
# Averaged Unseeded Species Coverage of the Post-Drought Spring Seasons by Site and Surface Treatments
plot3_b3_spring_UC = ggplot(Model3_b3_filtered_spring_summary_UC, 
                            aes(x = Site, y = mean_cover)) + 
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.6) +
  geom_errorbar(aes(ymin = mean_cover - SE_cover, ymax = mean_cover + SE_cover), 
                position = position_dodge(0.7), width = 0.2) +
  facet_grid(. ~ seasonyear) +
  labs(y = "Unseeded species cover (%)",
       x = "Site") +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "lightgrey", color = NA))

# TPC
Model3_b3_filtered_spring_summary_TPC <- Model3_b3_filtered_spring %>%
  group_by(Site, seasonyear) %>%
  summarise(
    mean_TPC = mean(Total_Plant_Cover, na.rm = TRUE),
    SE_TPC = sd(Total_Plant_Cover, na.rm = TRUE) / sqrt(n())
  )
# Averaged Total Plant Coverage of the Post-Drought Spring Seasons by Site and Surface Treatments
plot3_b3_spring_TPC = ggplot(Model3_b3_filtered_spring_summary_TPC, 
                             aes(x = Site, y = mean_TPC)) + 
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.6) +
  geom_errorbar(aes(ymin = mean_TPC - SE_TPC, ymax = mean_TPC + SE_TPC), 
                position = position_dodge(0.7), width = 0.2) +
  facet_grid(. ~ seasonyear) +
  labs(y = "Total plant cover (%)",
       x = "Site") +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "lightgrey", color = NA))


#### fall
# UC
Model3_b3_filtered_fall_summary_UC <- Model3_b3_filtered_fall %>%
  group_by(Site) %>%
  summarise(
    mean_cover = mean(Unseeded_Cover, na.rm = TRUE),
    SE_cover = sd(Unseeded_Cover, na.rm = TRUE) / sqrt(n())
  )
# Averaged Unseeded Species Coverage of the Post-Drought Fall Season by Site and Surface Treatments
plot3_b3_fall_UC = ggplot(Model3_b3_filtered_fall_summary_UC, 
                            aes(x = Site, y = mean_cover)) + 
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.6) +
  geom_errorbar(aes(ymin = mean_cover - SE_cover, ymax = mean_cover + SE_cover), 
                position = position_dodge(0.7), width = 0.2) +
  labs(y = "Unseeded species cover (%)",
       x = "Site") +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "lightgrey", color = NA),
        strip.text.y = element_text(angle = -90))

# TPC
Model3_b3_filtered_fall_summary_TPC <- Model3_b3_filtered_fall %>%
  group_by(Site) %>%
  summarise(
    mean_TPC = mean(Total_Plant_Cover, na.rm = TRUE),
    SE_TPC = sd(Total_Plant_Cover, na.rm = TRUE) / sqrt(n())
  )
# Averaged Total Plant Coverage of the Post-Drought Fall Season by Site and Surface Treatments
plot3_b3_fall_TPC = ggplot(Model3_b3_filtered_fall_summary_TPC, 
                             aes(x = Site, y = mean_TPC)) + 
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.6) +
  geom_errorbar(aes(ymin = mean_TPC - SE_TPC, ymax = mean_TPC + SE_TPC), 
                position = position_dodge(0.7), width = 0.2) +
  labs(y = "Total plant cover (%)",
       x = "Site") +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "lightgrey", color = NA),
        strip.text.y = element_text(angle = -90))