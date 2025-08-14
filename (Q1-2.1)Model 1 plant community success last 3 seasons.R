# Current question:
# 1. Do certain surface treatments or seed mix types (cool vs warm) improve establishment of seeded species and natural native plant regeneration? Does this differ by site? 
# "Q1-2.1" means Question 1, last 3 seasons, response variable 1
# 1-2.1 after drought, response var: "seedling_count"


## clear work space ##
rm(list = ls())

## libraries we're using
library(readxl)
library(dplyr)
library(glmmTMB)
library(sjPlot)
library(emmeans)
library(multcomp)
library(car)
library(ggplot2)

#------------------------------------------------------------------------------#
#----------#0) data preparation
#------------------------------------------------------------------------------#
## import .csv file to Global Environment
library(readxl)
FullDatawithZeros10_7_2024 <- read_excel("2024/Data for models/FullDatawithZeros10_7_2024.xlsx")
View(FullDatawithZeros10_7_2024)

Model1All <- FullDatawithZeros10_7_2024
summary(Model1All)

# Set categorical factors for main effects:
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
summary(Model1All)

# concatenating site:plot so that we can summarize based on that
Model1All$concatenated <- paste(Model1All$Site, Model1All$Plot)
print(Model1All)

# Set categorical factors for main effects:
Model1All <- within( Model1All, 
                     {
                       SitePlot <- factor(concatenated)
                     })
summary(Model1All)


## filter out the seeded species, so we have only seeded species and only the 3 seasons after the drought
## the "none" SeedTreat is the exact same as Soilsurfacetreat "control"

# the filtered dataset with seedtreat "none" changed the significance for the soil surface treatments, but not for the seed treatment. Since the seed treatment was what we want to make a difference, then we would use the dataset that filter out SeadTreat "none". The soil surface treatment idealy should include all seeded species regardless whether we seeded them into that plot. 

## so this filter take out seedtreat "none"
Model1_b1_filtered <- filter(Model1All, 
                           (Seededornot == "N_control"|Seededornot == "Y_cool"|Seededornot == "Y_warm") &
                             (Season == "spring 2021"|Season == "fall 2021"|Season == "spring 2022") &
                             (SeedTreat == "Cool"|SeedTreat == "Warm"))
summary(Model1_b1_filtered) 


## separate the seasons into springs and fall
Model1_b1_filtered_spring <- filter(Model1_b1_filtered,(Season == "spring 2021"|Season == "spring 2022"))
summary(Model1_b1_filtered_spring)

Model1_b1_filtered_fall <- filter(Model1_b1_filtered,(Season == "fall 2021"))
summary(Model1_b1_filtered_fall)


#------------------------------------------------------------------------------#
#----------#1) univariate relationships between individual explanatory and response variable(seedling_Count)
#------------------------------------------------------------------------------#

##### data = Model1_b1_filtered_spring
library(glmmTMB)
# Initialize an empty list to store models
univar_models <- list()

# List of predictor variables
predictor_vars <- c("MAP_mm", "Monsoonoftotal", "MAT", "elevation", "PPT_since_lastsampling_mm", "total_PPT", "cumulative_PPT", "SeedTreat", "SoilSurfaceTreat", "MAP95", "Site","MAT95")

# Loop through each predictor variable
for (var in predictor_vars) {
  # Build the formula for glmmTMB
  formula <- as.formula(paste("Seedling_Count ~", var))
  
  # Fit glmmTMB model
  univar_models[[var]] <- glmmTMB(formula, data = Model1_b1_filtered_spring, 
                                     ziformula = ~1,
                                     family = nbinom1)
}

# Print summary for each model
for (i in seq_along(predictor_vars)) {
  cat("Summary for model with predictor variable:", predictor_vars[i], "\n")
  print(summary(univar_models[[i]]))
  cat("\n\n")
}
# Extract AIC values from each model
AIC_values <- sapply(univar_models, function(model) AIC(model))

# Print AIC values
print(AIC_values)
best_model_index <- which.min(AIC_values)
print(best_model_index)


##### data = Model1_b1_filtered_fall
library(glmmTMB)
# Initialize an empty list to store models
univar_models <- list()

# List of predictor variables
predictor_vars <- c("MAP_mm", "Monsoonoftotal", "MAT", "elevation", "PPT_since_lastsampling_mm", "total_PPT", "cumulative_PPT", "SeedTreat", "SoilSurfaceTreat", "MAP95", "Site","MAT95")

# Loop through each predictor variable
for (var in predictor_vars) {
  # Build the formula for glmmTMB
  formula <- as.formula(paste("Seedling_Count ~", var))
  
  # Fit glmmTMB model
  univar_models[[var]] <- glmmTMB(formula, data = Model1_b1_filtered_fall, 
                                  ziformula = ~1,
                                  family = nbinom1)
}

# Print summary for each model
for (i in seq_along(predictor_vars)) {
  cat("Summary for model with predictor variable:", predictor_vars[i], "\n")
  print(summary(univar_models[[i]]))
  cat("\n\n")
}
# Extract AIC values from each model
AIC_values <- sapply(univar_models, function(model) AIC(model))

# Print AIC values
print(AIC_values)
best_model_index <- which.min(AIC_values)
print(best_model_index)


################################################################################
## significant factors from the univariate were: 
#### spring: MAT95, SeedTreat, SoilSurfaceTreat(AIC value ascending order)

# MAT95: species specific
# SeedTreat
# SoilSurfaceTreat

#### fall: MAT95, Site, Monsoonoftotal(AIC value ascending order)
# MAT95: species specific
# Site
# Monsoonoftotal: siteseason specific

# Fixed factors to include: (Dr Rowe's table)
# 1) Site or other site or climate variables (depending on which is more significant in the univariate analyses), 2)surface treatment, 3)seeding treatment, 4) site
# the point of the univariate relationship analysis is to find sig climate variables, and compare them with "Site". If none of their AIC is smaller than Site's, we go by "Site" as fixed effect.

# Based on this and the results we got above, still we stick to these 3 - "Site", "SoilSrufaceTreat", "SeedTreat" for both spring model and fall model
################################################################################


#------------------------------------------------------------------------------#
#----------#2) significant factors to build model
#------------------------------------------------------------------------------#
library(glmmTMB)
# instead of Site, MAT95 has the min AIC in univar relationship analysis, but the current question is focusing on seed treatment and soil treatment so we don't add in MAT95(species specific factor)

# sig factors as fix effects
# site as random effect and tell the model for each site there are repeated measure for diff season

#### springs only
# within springs there are 2 seasons, try to explore the effect of season
Model1_b1_spring <- glmmTMB(Seedling_Count ~   Site+SeedTreat+SoilSurfaceTreat+Season
                                      #+SeedTreat*SoilSurfaceTreat          
                                      +(1|Site) +(1|SitePlot),
                                      data = Model1_b1_filtered_spring,
                                      family = nbinom1)
summary(Model1_b1_spring) 
# Zero-inflation model: p-value = 0.661, thereby remove this term
# interaction(SeedTreat*SoilSurfaceTreat) not sig, so removed
Anova(Model1_b1_spring, type = "III") 
# seedtreat, soilsurfacetreat and season are sig.


#### fall only
Model1_b1_fall <- glmmTMB(Seedling_Count ~   Site+SeedTreat+SoilSurfaceTreat
                               +Site:SeedTreat
                               +(1|Site) +(1|SitePlot),
                               data = Model1_b1_filtered_fall,
                               ziformula = ~1,
                               family = nbinom1)
summary(Model1_b1_fall) 
# Zero-inflation model: p-value = 0.645, but removed would cause warning
# interaction(Site*SeedTreat) is sig
Anova(Model1_b1_fall, type = "III")
# show Site and SeedTreat sig


#------------------------------------------------------------------------------#
#----------#3) model checks and statistics
#------------------------------------------------------------------------------#

# creates HTML tables from regression models
library(sjPlot)
tab_model(Model1_b1_spring)
tab_model(Model1_b1_fall)

#### loading post hoc comparisons required libraries
# do post hoc if the factor is significant in the anova, and for the post hoc comparison we use "pairwise" function 
library(emmeans)
library(multcomp)

# spring - Site(not sig)
# spring-SoilSurfaceTreat
marginal_spring = emmeans(Model1_b1_spring,
                   ~ SoilSurfaceTreat)
contrast(marginal_spring, method = "tukey")
pairwise_IRRs_spring <- contrast(marginal_spring, method = "pairwise", type = "response")
print(pairwise_IRRs_spring)
# !Pits > Seed only > ConMod

# spring-SeedTreat
marginal_spring = emmeans(Model1_b1_spring,
                          ~ SeedTreat)
contrast(marginal_spring, method = "tukey")
pairwise_IRRs_spring <- contrast(marginal_spring, method = "pairwise", type = "response")
print(pairwise_IRRs_spring)
# !cool > warm

#spring - season
marginal_spring = emmeans(Model1_b1_spring,
                          ~ Season)
contrast(marginal_spring, method = "tukey")
pairwise_IRRs_spring <- contrast(marginal_spring, method = "pairwise", type = "response")
print(pairwise_IRRs_spring)
# !spring 2021 > spring2022


# fall-SoilSurfaceTreat(not sig)
# fall-SeedTreat(not sig)
# fall-Site
marginal_fall = emmeans(Model1_b1_fall,
                        ~ Site)
contrast(marginal_fall, method = "tukey")
pairwise_IRRs_fall <- contrast(marginal_fall, method = "pairwise", type = "response")
print(pairwise_IRRs_fall)
# !Preserve > Roo > Pleasant

# fall-Site*SeedTreat
marginal_fall = emmeans(Model1_b1_fall,
                        ~ Site|SeedTreat)
contrast(marginal_fall, method = "tukey")
pairwise_IRRs_fall <- contrast(marginal_fall, method = "pairwise", type = "response")
print(pairwise_IRRs_fall)




#------------------------------------------------------------------------------#
# Answer the  question(last 3 seasons, seedling_count):
# spring:
# 1. surface treatments - yes! pits the best
# 2. seed mix types (cool vs warm) - yes! Cool > warm
# 3. differ by site? - no sig diff
# 4. season - yes! spring 2021 > spring2022
# fall:
# 1. surface treatments - no sig effect
# 2. seed mix types (cool vs warm) - yes! warm > Cool
# 3. differ by site? - yes! preserve the best



#------------------------ try to plot the results -----------------------------#
library(ggplot2)
Model1_b1_filtered_summary <- Model1_b1_filtered %>%
  group_by(Seed_Mix, Treatment, seasonyear, Site) %>%
  summarise(
    mean_count = mean(Seedling_Count, na.rm = TRUE),
    SE_count = sd(Seedling_Count, na.rm = TRUE) / sqrt(n())
  )
# generalize plot about Average Number of Seedlings in the Post-Drought Seasons by Seed and Surface Treatments
plot1_b1_SpringFall = ggplot(Model1_b1_filtered_summary, aes(x = Seed_Mix, y = mean_count, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.6) +
  geom_errorbar(aes(ymin = mean_count - SE_count, ymax = mean_count + SE_count), 
                position = position_dodge(0.7), width = 0.2) +
  facet_grid(seasonyear ~ Site) +
  labs(
    y = "Seedling Numbers",
    x = "Seed Treatments",
    fill = "Surface Treatments"
  ) +
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "lightgrey", color = NA),
    strip.text.y = element_text(angle = 0)
  )


# Fall
Model1_b1_filtered_fall_summary <- Model1_b1_filtered_fall %>%
  group_by(Site) %>%
  summarise(
    mean_count = mean(Seedling_Count, na.rm = TRUE),
    SE_count = sd(Seedling_Count, na.rm = TRUE) / sqrt(n())
  )
# generalize plot about Average Number of Seedlings in the Post-Drought Fall Season by Seed and Surface Treatments
plot1_b1_Fall = ggplot(Model1_b1_filtered_fall_summary, aes(x = Site, y = mean_count)) +
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.6) +
  geom_errorbar(aes(ymin = mean_count - SE_count, ymax = mean_count + SE_count), 
                position = position_dodge(0.7), width = 0.2) +
  labs(y = "Seedling (#)", x = "Site") +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "lightgrey", color = NA),
    strip.text.y = element_text(angle = 0))


# Spring
Model1_b1_filtered_spring_summary <- Model1_b1_filtered_spring %>%
  group_by(Seed_Mix, Treatment, seasonyear) %>%
  summarise(
    mean_count = mean(Seedling_Count, na.rm = TRUE),
    SE_count = sd(Seedling_Count, na.rm = TRUE) / sqrt(n())
  )
# generalize plot about Average Number of Seedlings in the Post-Drought Spring Season by Seed and Surface Treatments
plot1_b1_Spring = ggplot(Model1_b1_filtered_spring_summary, aes(x = Seed_Mix, y = mean_count, fill = Treatment)) +
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
