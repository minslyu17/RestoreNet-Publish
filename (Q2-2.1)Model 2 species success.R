# Current question:
# 2. Did certain species do better than others? Did this differ by site? Was there a treatment by species effect? Could this be predicted by their MAT95 or their MAP95?
# "Q2-2.1" means Question 2, last 3 seasons, response variable 1
# 2-2.1 after drought? response var: "seedling_count", include species(MAT95) as a fixed effect

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
Model1All <- FullDatawithZeros10_7_2024

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
Model2_b1_filtered <- filter(Model1All, 
                            (Seededornot == "Y_cool"|Seededornot == "Y_warm") &
                              (Season == "spring 2021"|Season == "fall 2021"|Season == "spring 2022") &
                              (SeedTreat == "Cool"|SeedTreat == "Warm") &
                              (SoilSurfaceTreat == "ConMod"|SoilSurfaceTreat == "Mulch"|SoilSurfaceTreat == "Pits"|SoilSurfaceTreat == "Seed only"))

levels(Model2_b1_filtered$SoilSurfaceTreat)
levels(Model2_b1_filtered$Seededornot) 
# we want to filter out controls but they are set as factors, needed to delete the whole empty level to remove

# remove useless levels that might cause troubles
#install.packages("forcats")
library(forcats)
# remove the "Control" level in "SoilSurfaceTreat" factor
Model2_b1_filtered$SoilSurfaceTreat <- fct_drop(Model2_b1_filtered$SoilSurfaceTreat, only = "Control")
levels(Model2_b1_filtered$SoilSurfaceTreat)
# remove the "N" and "N_control" in "Seededornot" factor
Model2_b1_filtered$Seededornot <- fct_drop(Model2_b1_filtered$Seededornot, only = c("N", "N_control"))
levels(Model2_b1_filtered$Seededornot)

summary(Model2_b1_filtered)


## separate the seasons into springs and falls
Model2_b1_filtered_spring <- filter(Model2_b1_filtered,(Season == "spring 2021"|Season == "spring 2022"))
summary(Model2_b1_filtered_spring)
Model2_b1_filtered_fall <- filter(Model2_b1_filtered,(Season == "fall 2021"))
summary(Model2_b1_filtered_fall)


#------------------------------------------------------------------------------#
#----------#1) univariate relationships between individual explanatory and response variable(seedling_Count)
#------------------------------------------------------------------------------#
library(glmmTMB)

##### data = Model2_b1_filtered_spring
# Initialize an empty list to store models
univar_models <- list()

# List of predictor variables
predictor_vars <- c("MAP_mm", "Monsoonoftotal", "MAT", "elevation", "PPT_since_lastsampling_mm", "total_PPT", "cumulative_PPT", "SeedTreat", "SoilSurfaceTreat", "MAP95", "Site","MAT95","Species")

# Loop through each predictor variable
for (var in predictor_vars) {
  # Build the formula for glmmTMB
  formula <- as.formula(paste("Seedling_Count ~", var))
  
  # Fit glmmTMB model
  univar_models[[var]] <- glmmTMB(formula, data = Model2_b1_filtered_spring, 
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


##### data = Model2_b1_filtered_fall
# Initialize an empty list to store models
univar_models <- list()

# List of predictor variables
predictor_vars <- c("MAP_mm", "Monsoonoftotal", "MAT", "elevation", "PPT_since_lastsampling_mm", "total_PPT", "cumulative_PPT", "SeedTreat", "SoilSurfaceTreat", "MAP95", "Site","MAT95","Species")

# Loop through each predictor variable
for (var in predictor_vars) {
  # Build the formula for glmmTMB
  formula <- as.formula(paste("Seedling_Count ~", var))
  
  # Fit glmmTMB model
  univar_models[[var]] <- glmmTMB(formula, data = Model2_b1_filtered_fall, 
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
#### Spring: Species, MAT95, SeedTreat, SoilSurfaceTreat (AIC value ascending order)

#### Fall: MAT95, SeedTreat, Site (AIC value ascending order)

# Fixed factors to include: Site, SoilSurfaceTreat, Species
# include MAT95 as a fixed effect to replace Species, since numeric variable can be sorted
#(not including SeedTreat because we focus on species effect)
################################################################################

#------------------------------------------------------------------------------#
#----------#2) significant factors to build model
#------------------------------------------------------------------------------#
library(glmmTMB)
library(car)
# sig factors as fix effects
# site as random effect and tell the model for each site there are repeated measure for diff season

#### springs only
Model2_b1_spring <- glmmTMB(Seedling_Count ~   Site+SoilSurfaceTreat+MAT95
                                      +Site*MAT95
                                      +(1|Site) +(1|SitePlot),
                                      data = Model2_b1_filtered_spring,
                                      #ziformula = ~1,
                                      family = nbinom1)
summary(Model2_b1_spring)
# Zero-inflation model: p-value = 0.997, thereby remove this term
# interactions not sig
Anova(Model2_b1_spring, type = "III") # soilsurfacetreat and MAT95 are sig


#### fall only
Model2_b1_fall <- glmmTMB(Seedling_Count ~   Site+SoilSurfaceTreat+MAT95 
                                    + Site*MAT95
                                    +(1|Site) +(1|SitePlot), 
                                     data = Model2_b1_filtered_fall,
                                     #ziformula = ~1,
                                     family = nbinom1)
summary(Model2_b1_fall)
# Zero-inflation model: p-value = 0.4, thereby remove this term
Anova(Model2_b1_fall, type = "III") # site and interactions are sig


#------------------------------------------------------------------------------#
#----------#3) model checks and statistics
#------------------------------------------------------------------------------#

# creates HTML tables from regression models
library(sjPlot)
tab_model(Model2_b1_spring)
tab_model(Model2_b1_fall)

#### loading post hoc comparisons required libraries
# do post hoc if the factor is significant in the anova, and for the post hoc comparison we use "pairwise" function 
library(emmeans)
library(multcomp)

# spring - Site(not sig)
# spring - MAT95
# spring - SoilSurfaceTreat
marginal_spring = emmeans(Model2_b1_spring,
                        ~ SoilSurfaceTreat)
contrast(marginal_spring, method = "tukey")
pairwise_IRRs <- contrast(marginal_spring, method = "pairwise", type = "response")
print(pairwise_IRRs)
# Pits > Mulch > Seed only > ConMod


# fall - SoilSurfaceTreat(not sig)
# fall - MAT95(not sig)
# fall - Site
marginal_fall = emmeans(Model2_b1_fall,
                   ~ Site)
contrast(marginal_fall, method = "tukey")
pairwise_IRRs <- contrast(marginal_fall, method = "pairwise", type = "response")
print(pairwise_IRRs)
# !Preserve > Roo > Pleasant



#------------------------------------------------------------------------------#
# Answer the question(last 3 seasons, seedling_count):
# spring:
# 1. soil surface treatments - yes! pits the best
# 2. species - yes! see plot
# 3. site - no sig diff
# fall:
# 1. soil surface treatments - no sig effect
# 2. species - no sig effect
# 3. site - yes! preserve the best


#------------------------------------------------------------------------------#
#----------#4) visualization of the difference among species
#------------------------------------------------------------------------------#
# Create a plot that shows a box plot or mean/SE of seedling counts ordered from cool to warm based on MAT95
library(ggplot2)
library(dpylr)

#### spring
Model2_b1_filtered_spring_summary <- Model2_b1_filtered_spring %>%
  group_by(Species, SeedTreat, MAT95, SoilSurfaceTreat) %>%
  summarise(
    mean_count = mean(Seedling_Count, na.rm = TRUE),
    SE_count = sd(Seedling_Count, na.rm = TRUE) / sqrt(n())
  )
# Average Number of Seedlings in the Post-Drought Fall by Species and Surface Treatments
plot2_b1_spring <- ggplot(Model2_b1_filtered_spring_summary, 
                 aes(x = reorder(Species, MAT95), y = mean_count, fill = MAT95)) +
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.6) +
  geom_errorbar(aes(ymin = mean_count - SE_count, ymax = mean_count + SE_count), 
                position = position_dodge(0.7), width = 0.2) +
  scale_fill_gradient(low="blue", high="red") +
  facet_grid(SoilSurfaceTreat ~ .) +
  labs(y = "Seeded speceis density (#/subplot)",
       x = "Species code",
       fill = "MAT95 (°C)") +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "lightgrey", color = NA),
        strip.text.y = element_text(angle = 0))



#### fall
Model2_b1_filtered_fall_summary <- Model2_b1_filtered_fall %>%
  group_by(Site, Species, SeedTreat, MAT95) %>%
  summarise(
    mean_count = mean(Seedling_Count, na.rm = TRUE),
    SE_count = sd(Seedling_Count, na.rm = TRUE) / sqrt(n())
  )
# Average Number of Seedlings in the Post-Drought Springs by Species and Site
plot2_b1_fall <- ggplot(Model2_b1_filtered_fall_summary, 
                        aes(x = reorder(Species, MAT95), y = mean_count, fill = MAT95)) +
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.6) +
  geom_errorbar(aes(ymin = mean_count - SE_count, ymax = mean_count + SE_count), 
                position = position_dodge(0.7), width = 0.2) +
  scale_fill_gradient(low="blue", high="red") +
  facet_grid(Site ~ .) +
  labs(y = "Seeded species density (#/subplot)",
       x = "Species code",
       fill = "MAT95 (°C)") +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "lightgrey", color = NA),
        strip.text.y = element_text(angle = 0))
