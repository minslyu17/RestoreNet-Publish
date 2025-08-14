# Current question:
# 2. Did certain species do better than others? Did this differ by site? Was there a treatment by species effect? Could this be predicted by their MAT95 or their MAP95?
# "Q2-1.1" means Question 2, first season, response variable 1
# 2-1.1 before drought, response var: "seedling_count", include species(MAT95) as a fixed effect

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

## The Model1All used for Q2 should be the one filtered out those species that emerge at wrong plot. Since we are focusing on the species success, those wrong species should be discluded.
Model1All_filtered_coolWrong <- filter(Model1All,(SeedTreat == "Cool") & (Seededornot == "Y_warm"))
Model1All_filtered_warmWrong <- filter(Model1All,(SeedTreat == "Warm") & (Seededornot == "Y_cool"))

# all wrong species
Model1All_filtered_wrong <- rbind(Model1All_filtered_coolWrong, Model1All_filtered_warmWrong)

# get these out from Model1All
WrongSpeciesOut <- setdiff(Model1All, Model1All_filtered_wrong)
Model1All <- WrongSpeciesOut
summary(Model1All)


## filter out the seeded species, so we have only seeded species and only the 3 seasons after the drought
## the "none" SeedTreat is the exact same as Soilsurfacetreat "control", filter out the controls
library(dplyr)
Model2_a1_filtered <- filter(Model1All, 
                            (Seededornot == "Y_cool"|Seededornot == "Y_warm") &
                              (Season == "spring 2020") &
                              (SeedTreat == "Cool"|SeedTreat == "Warm") &
                              (SoilSurfaceTreat == "ConMod"|SoilSurfaceTreat == "Mulch"|SoilSurfaceTreat == "Pits"|SoilSurfaceTreat == "Seed only"))

library(forcats)
# remove the "Control" level in "SoilSurfaceTreat" factor
Model2_a1_filtered$SoilSurfaceTreat <- fct_drop(Model2_a1_filtered$SoilSurfaceTreat, only = "Control")
levels(Model2_a1_filtered$SoilSurfaceTreat)
# remove the "N" and "N_control" in "Seededornot" factor
Model2_a1_filtered$Seededornot <- fct_drop(Model2_a1_filtered$Seededornot, only = c("N", "N_control"))
levels(Model2_a1_filtered$Seededornot)

summary(Model2_a1_filtered)


#------------------------------------------------------------------------------#
#----------#1) univariate relationships between individual explanatory and response variable(seedling_Count)
#------------------------------------------------------------------------------#

##### data = Model2_a1_filtered
library(glmmTMB)
# Initialize an empty list to store models
univar_models <- list()

# List of predictor variables
predictor_vars <- c("MAP_mm", "Monsoonoftotal", "MAT", "elevation", "PPT_since_lastsampling_mm", "total_PPT", "cumulative_PPT", "SeedTreat", "SoilSurfaceTreat", "MAP95", "Site","MAT95","Species")

# Loop through each predictor variable
for (var in predictor_vars) {
  # Build the formula for glmmTMB
  formula <- as.formula(paste("Seedling_Count ~", var))
  
  # Fit glmmTMB model
  univar_models[[var]] <- glmmTMB(formula, data = Model2_a1_filtered, 
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
# Species, MAT95, MAP95, SeedTreat
# (AIC value ascending order)

# Fixed factors to include: Site, SoilSurfaceTreat, Species
# include MAT95 as a fixed effect to replace Species, since numeric variable can be sorted
#(not including SeedTreat because we focus on species effect)
################################################################################


#------------------------------------------------------------------------------#
#----------#2) significant factors to build model
#------------------------------------------------------------------------------#
library(glmmTMB)
library(car)
# sig factors as fix effects and siteplot as random effect
# site as random effect and tell the model for each site there are repeated measure for diff season

Model2_a1 <- glmmTMB(Seedling_Count ~   Site+SoilSurfaceTreat+MAT95 
                                      + Site*MAT95
                                      +(1|Site) +(1|SitePlot),
                                      data = Model2_a1_filtered,
                                      #ziformula = ~1,
                                      family = nbinom1)
summary(Model2_a1)
# Zero-inflation model: p-value = 0.998, thereby remove this term
# MAT95:-0.348(higher temperature, lower estimate seedling counts)
Anova(Model2_a1, type = "III") # Site, MAT95 and interaction are sig



#------------------------------------------------------------------------------#
#----------#3) model checks and statistics
#------------------------------------------------------------------------------#

# creates HTML tables from regression models
library(sjPlot)
tab_model(Model2_a1)

#### loading post hoc comparisons required libraries
# do post hoc if the factor is significant in the anova, and for the post hoc comparison we use "pairwise" function 
library(emmeans)
library(multcomp)

# Model2_a1 - SoilSurfaceTreat(not sig)
# Model2_a1 - Site
marginal = emmeans(Model2_a1,
                          ~ Site)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs)
# Preserve > Roo > Pleasant

# Model2_a1 - MAT95
marginal = emmeans(Model2_a1,
                          ~ MAT95)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs)
# NAs, MAT95 is numeric variable


#------------------------------------------------------------------------------#
# Answer the question(first season, seedling_count):
# 1. soil surface treatments - no sig diff
# 2. species - yes! see plot
# 3. site - yes! Preserve the best


#------------------------------------------------------------------------------#
#----------#4) visualization of the difference among species
#------------------------------------------------------------------------------#
# Use mean/SE of seedling counts ordered from cool to warm based on MAT95 to visualize more clearly
library(ggplot2)
Model2_a1_filtered_summary <- Model2_a1_filtered %>%
  group_by(Site, Species, MAT95) %>%
  summarise(
    mean_count = mean(Seedling_Count, na.rm = TRUE),
    SE_count = sd(Seedling_Count, na.rm = TRUE) / sqrt(n())
  )

# generalize plot about Average Number of Seeded Species in the Pre-Drought Season by Species and Site
plot2_a1 <- ggplot(Model2_a1_filtered_summary, 
                 aes(x = reorder(Species, MAT95), y = mean_count, fill = MAT95)) +
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
# PLOV is high for all 3 sites, instead cool species in Pleasant did do so well