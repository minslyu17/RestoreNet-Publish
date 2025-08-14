# Current question:
# 1. Do certain surface treatments or seed mix types (cool vs warm) improve establishment of seeded species and natural native plant regeneration? Does this differ by site? 
# "Q1-1.1" means Question 1, first season, response variable 1
# 1-1.1 before drought, response var: "seedling_count"


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

table(Model1All$seasonyear) # the 1st season is spring 2020

## filter out the seeded species, so we have only seeded species and only the 1st season before the drought
Model1_a1_filtered <- filter(Model1All, 
                           (Seededornot == "N_control"|Seededornot == "Y_cool"|Seededornot == "Y_warm") &
                             (Season == "spring 2020") &
                             (SeedTreat == "Cool"|SeedTreat == "Warm"))
summary(Model1_a1_filtered) 


################################################################################
##Response variables: seedling_Count, seedling_Count*Avg_Height_mm
##Explanatory variables: 
# seed and soil: "SeedTreat", "SoilSurfaceTreat"
# spatial: "Site", "MAP_mm", "MAP95", "MAT", "MAT95", "elevation", "Monsoonoftotal", "total_PPT" (except "MAP95" and "MAT95" are species specific, all others are site specific)
# temporal: "cumulative_PPT", "PPT_since_lastsampling_mm" (both siteseason specific)
################################################################################

################################################################################
# [Min - I'll leave these here for the future review]
# the thing I mentioned that there are some missing vars in the predictor_vars are these 4:
# (Do we need them? Or why we don't need them? I made some thoughts behind these vars, are those somehow the reason?)

# 1"Season": time variable
#HIR - we are interested in season if there is an interaction in which the treatment differences change by season - if that is not significant, it can probably be taken out?
# so we want to check if it's sig, so yes we add it to the predict_var

# 2"Seededornot": on the filtered dataset we only have the seeded, do we need to intruduce this as a factor?
#HIR - correct, we do not need it.

# 3"Species": we are focusing on the treatment influence(seed treat and soil treat), so maybe we don't need it in this question?
#HIR - correct - the species level questions come about in question 2. Did certain species do better than others? Did this differ by site? Was there a treatment by species effect? Could this be predicted by their MAT95 or their MAP95?

# 4"SitePlot": we never want it, it's kind of like a mistakenly combined column?
#correct - not sure what I used that for...
################################################################################

#------------------------------------------------------------------------------#
#----------#1) univariate relationships between individual explanatory and response variable(seedling_Count)
#------------------------------------------------------------------------------#

##### data = Model1_a1_filtered
library(glmmTMB)
# Initialize an empty list to store models
univar_models1_a1 <- list()

# List of predictor variables
predictor_vars <- c("MAP_mm", "Monsoonoftotal", "MAT", "elevation", "PPT_since_lastsampling_mm", "total_PPT", "cumulative_PPT", "SeedTreat", "SoilSurfaceTreat", "MAP95", "Site","MAT95")

# Loop through each predictor variable
for (var in predictor_vars) {
  # Build the formula for glmmTMB
  formula <- as.formula(paste("Seedling_Count ~", var))
  
  # Fit glmmTMB model
  univar_models1_a1[[var]] <- glmmTMB(formula, data = Model1_a1_filtered, 
                                     ziformula = ~1,
                                     family = nbinom1)
}

# Print summary for each model
for (i in seq_along(predictor_vars)) {
  cat("Summary for model with predictor variable:", predictor_vars[i], "\n")
  print(summary(univar_models1_a1[[i]]))
  cat("\n\n")
}
# Extract AIC values from each model
AIC_values1_a1 <- sapply(univar_models1_a1, function(model) AIC(model))

# Print AIC values
print(AIC_values1_a1)
best_model_index1_a1 <- which.min(AIC_values1_a1)
print(best_model_index1_a1)

################################################################################
## significant factors from the univariate were: 
#  MAT95, MAP95, SeedTreat, Monsoonoftotal(AIC value ascending order)

# MAT95: species specific
# MAP95: species specific
# SeedTreat
# Monsoonoftotal: site specific

# Fixed factors to include: (Dr Rowe's table)
# 1) Site or other site or climate variables (depending on which is more significant in the univariate analyses), 2)surface treatment, 3)seeding treatment, 4) site
# the point of the univariate relationship analysis is to find sig climate variables, and compare them with "Site". If none of their AIC is smaller than Site's, we go by "Site" as fixed effect.

# Based on this and the results we got above, we stick to these 3 - "Site", "SoilSrufaceTreat", "SeedTreat"
################################################################################

################################################################################
## Compare different family codes
# Fit model with nbinom1 family
Model1Full_zinf_nbinom1 <- glmmTMB(Seedling_Count ~ Site+SeedTreat+SoilSurfaceTreat,
                                   data = Model1_a1_filtered,
                                   ziformula = ~1,
                                   family = nbinom1)

# Fit model with nbinom2 family
Model1Full_zinf_nbinom2 <- glmmTMB(Seedling_Count ~ Site+SeedTreat+SoilSurfaceTreat,
                                   data = Model1_a1_filtered,
                                   ziformula = ~1,
                                   family = nbinom2)

# Fit model with Poisson family
Model1Full_zinf_poisson <- glmmTMB(Seedling_Count ~ Site+SeedTreat+SoilSurfaceTreat,
                                   data = Model1_a1_filtered,
                                   ziformula = ~1,
                                   family = poisson())

# Extract AIC values
AIC_values <- c(AIC(Model1Full_zinf_nbinom1), 
                AIC(Model1Full_zinf_nbinom2), 
                AIC(Model1Full_zinf_poisson))

# Select the model with the lowest AIC
best_model_index <- which.min(AIC_values)
best_model_name <- c("nbinom1", "nbinom2", "poisson")[best_model_index]

# Print AIC values
print(AIC_values)

# Print the best model
print(paste("Best model:", best_model_name))
# Yes, we stick to the nbinom1 model
summary(Model1Full_zinf_nbinom1)
aov(Model1Full_zinf_nbinom1)
################################################################################


#------------------------------------------------------------------------------#
#----------#2) significant factors to build model
#------------------------------------------------------------------------------#
library(glmmTMB)
library(car)
# sig factors as fix effects
# site as random effect and tell the model for each site there are repeated measure

# interactions(SeedTreat*SoilSurfaceTreat) not significant, so took them out again
Model1_a1 <- glmmTMB(Seedling_Count ~   Site+SeedTreat+SoilSurfaceTreat
                     +(1|Site) +(1|SitePlot),
                     data = Model1_a1_filtered,
                     family = nbinom1)
summary(Model1_a1) # Zero-inflation model: p-value = 0.997, thereby remove this term
Anova(Model1_a1, type = "III") #And yes, only SeedTreat is sig.


#------------------------------------------------------------------------------#
#----------#3) model checks and statistics
#------------------------------------------------------------------------------#

# creates HTML tables from regression models
library(sjPlot)
tab_model(Model1_a1)

#### loading post hoc comparisons required libraries
# do post hoc if the factor is significant in the anova, and for the post hoc comparison we use "pairwise" function 
library(emmeans)
library(multcomp)

# Model1_a1 - Site(not sig)
# Model1_a1 - SoilSurfaceTreat(not sig)
# Model1_a1 - SeedTreat
marginal = emmeans(Model1_a1,
                          ~ SeedTreat)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs)
# !cool > warm


#------------------------------------------------------------------------------#
# Answer the  question(first season, seedling_count):
# 1. surface treatments - no sig effect
# 2. seed mix types (cool vs warm) - yes! Cool species better than warm
# 3. differ by site? - no sig diff


#------------------------ try to plot the results -----------------------------#

library(ggplot2)
Model1_a1_filtered_summary <- Model1_a1_filtered %>%
  group_by(Seed_Mix) %>%
  summarise(
    mean_count = mean(Seedling_Count, na.rm = TRUE),
    SE_count = sd(Seedling_Count, na.rm = TRUE) / sqrt(n())
  )

# generalize plot about Average Number of Seedlings in the Pre-Drought Season by Seed Treatments
plot1_a1 = ggplot(Model1_a1_filtered_summary, aes(x = Seed_Mix, y = mean_count)) +
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.6) +
  geom_errorbar(aes(ymin = mean_count - SE_count, ymax = mean_count + SE_count), 
                position = position_dodge(0.7), width = 0.2) +
  labs(y = "Seeded speceis density (#/subplot)",
       x = "Seed Treatments") +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "lightgrey", color = NA),
        strip.text.y = element_text(angle = 0))
