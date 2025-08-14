# Current question:
# 3. Did the soil surface or seeding treatments or other abiotic treatments improve the (unseeded) native or non-native plant community establishment over time?
# "Q3-1.3" means Question 3, first season, response variable 3
# 3-1.3 before drought? response var: "Unseeded_Cover(UC)" and "Total_Plant_Cover(TPC)"

## clear work space ##
rm(list = ls())

## libraries we're using
library(readxl)
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
Model3_a3_filtered <- filter(Model3All, 
                             (seasonyear == "SPRING2020"),
                             (Site == "Pleasant" | Site == "Preserve" | Site == "Roosevelt"))


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

# UC model
Model3_a3_UC <- glmmTMB(Unseeded_Cover ~   Site+SoilSurfaceTreat
                         +(1|Site) +(1|Plot),
                         data = Model3_a3_filtered,
                         #ziformula = ~1,
                         family = nbinom1)
summary(Model3_a3_UC)
# zero inflation model: p-value 0.996, thereby remove the term
Anova(Model3_a3_UC, type = "III") # only site is sig


# TPC model
Model3_a3_TPC <- glmmTMB(Total_Plant_Cover ~   Site+SoilSurfaceTreat
                        +(1|Site) +(1|Plot),
                        data = Model3_a3_filtered,
                        #ziformula = ~1,
                        family = nbinom1)
summary(Model3_a3_TPC)
# no need for zero inflation model cause there is no zero inflation
Anova(Model3_a3_TPC, type = "III") # only site is sig


#------------------------------------------------------------------------------#
#----------#3) model checks and statistics
#------------------------------------------------------------------------------#
# creates HTML tables from regression models
library(sjPlot)
tab_model(Model3_a3_UC)
tab_model(Model3_a3_TPC)

#### loading post hoc comparisons required libraries
library(emmeans)
library(multcomp)
# do post hoc if the factor is significant in the anova, and for the post hoc comparison we use "pairwise" function 

# Model3_a3_UC - SoilSurfaceTreat(not sig)
# Model3_a3_UC - Site
marginal = emmeans(Model3_a3_UC,
                   ~ Site)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs) # preserve and Roo, Roo is slightly better


# Model3_a3_TPC - SoilSurfaceTreat(not sig)
# Model3_a3_TPC - Site
marginal = emmeans(Model3_a3_TPC,
                   ~ Site)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs) # still Roo is slightly better than preserve


#------------------------------------------------------------------------------#
# Answer the  question(first season, TPC/UC):

# UC
# 1. surface treatments - no sig diff
# 2. differ by site? - !sig, preserve and Roo, Roo is slightly better
# TPC
# 1. surface treatments - no sig diff
# 2. differ by site? - !sig, Roo is slightly better than preserve



#------------------------ try to plot the results -----------------------------#
library(ggplot2)

# UC
Model3_a3_filtered_summary_UC <- Model3_a3_filtered %>%
  group_by(Site) %>%
  summarise(
    mean_cover = mean(Unseeded_Cover, na.rm = TRUE),
    SE_cover = sd(Unseeded_Cover, na.rm = TRUE) / sqrt(n())
  )
# Averaged Unseeded Species Coverage of the Pre-Drought Season by Site and Surface Treatments
plot3_a3_UC = ggplot(Model3_a3_filtered_summary_UC, 
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
Model3_a3_filtered_summary_TPC <- Model3_a3_filtered %>%
  group_by(Site) %>%
  summarise(
    mean_cover = mean(Total_Plant_Cover, na.rm = TRUE),
    SE_cover = sd(Total_Plant_Cover, na.rm = TRUE) / sqrt(n())
  )
# Averaged Total Plant Coverage of the Pre-Drought Season by Site and Surface Treatments
plot3_a3_TPC = ggplot(Model3_a3_filtered_summary_TPC, 
                      aes(x = Site, y = mean_cover)) +
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.6) +
  geom_errorbar(aes(ymin = mean_cover - SE_cover, ymax = mean_cover + SE_cover), 
                position = position_dodge(0.7), width = 0.2) +
  labs(y = "Total plant cover (%)",
       x = "Site") +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "lightgrey", color = NA),
        strip.text.y = element_text(angle = -90))
