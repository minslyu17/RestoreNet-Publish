# Current question:
# 2. Did certain species do better than others? Did this differ by site? Was there a treatment by species effect? Could this be predicted by their MAT95 or their MAP95?
# "Q2-2.3" means Question 2, last 3 seasons, response variable 3
# 2-2.3 after drought? response var: "Whole_plot_cover", include species(MAT95) as a fixed effect

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
# using the same 2 season-separated dataset since we just change into another response var
Model2_b2_filtered_spring
Model2_b2_filtered_fall


#------------------------------------------------------------------------------#
#----------#1) univariate relationships between individual explanatory and response variable (Whole_plot_cover)
#------------------------------------------------------------------------------#
library(glmmTMB)

##### data = Model2_b2_filtered_spring
# Initialize an empty list to store models
univar_models <- list()

# List of predictor variables
predictor_vars <- c("MAP_mm", "Monsoonoftotal", "MAT", "elevation", "PPT_since_lastsampling_mm", "total_PPT", "cumulative_PPT", "SeedTreat", "SoilSurfaceTreat", "MAP95", "Site","MAT95","Species")

# Loop through each predictor variable
for (var in predictor_vars) {
  # Build the formula for glmmTMB
  formula <- as.formula(paste("Whole_plot_cover ~", var))
  
  # Fit glmmTMB model
  univar_models[[var]] <- glmmTMB(formula, data = Model2_b2_filtered_spring, 
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


##### data = Model2_b2_filtered_fall
# Initialize an empty list to store models
univar_models <- list()

# List of predictor variables
predictor_vars <- c("MAP_mm", "Monsoonoftotal", "MAT", "elevation", "PPT_since_lastsampling_mm", "total_PPT", "cumulative_PPT", "SeedTreat", "SoilSurfaceTreat", "MAP95", "Site","MAT95","Species")

# Loop through each predictor variable
for (var in predictor_vars) {
  # Build the formula for glmmTMB
  formula <- as.formula(paste("Whole_plot_cover ~", var))
  
  # Fit glmmTMB model
  univar_models[[var]] <- glmmTMB(formula, data = Model2_b2_filtered_fall, 
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
#### Spring: Species, cumulative_PPT, PPT_since_lastsampling_mm (AIC value ascending order)

#### Fall: Species, SeedTreat, Site (AIC value ascending order)

# Fixed factors to include: Site, SoilSurfaceTreat, Species
# include MAT95 as a fixed effect to replace Species, since numeric variable can be sorted
#(not including SeedTreat because we focus on species effect)
################################################################################

#------------------------------------------------------------------------------#
#----------#2) significant factors to build model
#------------------------------------------------------------------------------#
library(glmmTMB)
# sig factors as fix effects and siteplot as random effect
# site as random effect and tell the model for each site there are repeated measure for diff season

#### springs only
Model2_b3_spring <- glmmTMB(Whole_plot_cover ~   Site+SoilSurfaceTreat+MAT95
                               +Site*MAT95
                               +(1|Site) +(1|SitePlot),
                               data = Model2_b2_filtered_spring,
                               #ziformula = ~1,
                               family = nbinom1) # have warning
summary(Model2_b3_spring)
# Zero-inflation model doesn't fit, cause NaNs, thereby remove this term
# MAT95 not sig
Anova(Model2_b3_spring, type = "III") # Site, SoilSurfaceTreat and interactions are sig


#### fall only
Model2_b3_fall <- glmmTMB(Whole_plot_cover ~   Site+SoilSurfaceTreat+MAT95
                             #+Site*MAT95
                             +(1|Site) +(1|SitePlot),
                             data = Model2_b2_filtered_fall,
                             #ziformula = ~1,
                             family = nbinom1) # have warning
# Zero-inflation model doesn't fit, cause NaNs, thereby remove this term
summary(Model2_b3_fall)
Anova(Model2_b3_fall, type = "III") # site and interaction are sig


#------------------------------------------------------------------------------#
#----------#3) model checks and statistics
#------------------------------------------------------------------------------#

# creates HTML tables from regression models
library(sjPlot)
tab_model(Model2_b3_spring)
tab_model(Model2_b3_fall)


#### loading post hoc comparisons required libraries
# do post hoc if the factor is significant in the anova, and for the post hoc comparison we use "pairwise" function 
library(emmeans)
library(multcomp)

# spring - MAT95(not sig)
# spring - Site
marginal_spring = emmeans(Model2_b3_spring,
                          ~ Site)
contrast(marginal_spring, method = "tukey")
pairwise_IRRs <- contrast(marginal_spring, method = "pairwise", type = "response")
print(pairwise_IRRs)
# Roo > Pleasant > Preserve

# spring - SoilSurfaceTreat
marginal_spring = emmeans(Model2_b3_spring,
                          ~ SoilSurfaceTreat)
contrast(marginal_spring, method = "tukey")
pairwise_IRRs <- contrast(marginal_spring, method = "pairwise", type = "response")
print(pairwise_IRRs)
# Pits > Mulch > ConMod > Seed only


# fall - SoilSurfaceTreat(not sig)
# fall - MAT95
# fall - Site
marginal_fall = emmeans(Model2_b3_fall,
                        ~ Site)
contrast(marginal_fall, method = "tukey")
pairwise_IRRs <- contrast(marginal_fall, method = "pairwise", type = "response")
print(pairwise_IRRs)
# Preserve > Roo > Pleasant


#------------------------------------------------------------------------------#
# Answer the question(last 3 seasons, growth):
# spring:
# 1. soil surface treatments - yes! pits the best
# 2. species - no sig effect
# 3. site - yes! Roosevelt
# fall:
# 1. soil surface treatments - no sig effect
# 2. species - yes! see plots
# 3. site - yes! preserve the best


#------------------------------------------------------------------------------#
#----------#4) visualization of the difference among species
#------------------------------------------------------------------------------#
# Create a plot that shows a box plot or mean/SE of Whole_plot_cover ordered from cool to warm based on MAT95
library(ggplot2)

#### spring
Model2_b3_filtered_spring_summary <- Model2_b2_filtered_spring %>%
  group_by(Site, Species, MAT95, SoilSurfaceTreat) %>%
  summarise(
    mean_cover = mean(Whole_plot_cover, na.rm = TRUE),
    SE_cover = sd(Whole_plot_cover, na.rm = TRUE) / sqrt(n())
  )
# Average Whole Plot Coverage in the Post-Drought Springs by Species, Site and Surface Treatments
plot2_b3_spring <- ggplot(Model2_b3_filtered_spring_summary, 
                  aes(x = reorder(Species, MAT95), y = mean_cover, fill = MAT95)) +
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.6) +
  geom_errorbar(aes(ymin = mean_cover - SE_cover, ymax = mean_cover + SE_cover), 
                position = position_dodge(0.7), width = 0.2) +
  facet_grid(SoilSurfaceTreat ~ Site) +
  scale_fill_gradient(low="blue", high="red") +
  labs(y = "Whole plot cover (%)",
       x = "Species code",
       fill = "MAT95 (°C)") +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "lightgrey", color = NA),
        strip.text.y = element_text(angle = 0))


#### fall
Model2_b3_filtered_fall_summary <- Model2_b2_filtered_fall %>%
  group_by(Species, MAT95, Site) %>%
  summarise(
    mean_cover = mean(Whole_plot_cover, na.rm = TRUE),
    SE_cover = sd(Whole_plot_cover, na.rm = TRUE) / sqrt(n())
  )
# Average Whole Plot Coverage in the Post-Drought Fall by Species and Site
plot2_b3_fall <- ggplot(Model2_b3_filtered_fall_summary, 
               aes(x = reorder(Species, MAT95), y = mean_cover, fill = MAT95)) +
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.6) +
  geom_errorbar(aes(ymin = mean_cover - SE_cover, ymax = mean_cover + SE_cover), 
                position = position_dodge(0.7), width = 0.2) +
  facet_grid(Site ~ .) +
  scale_fill_gradient(low="blue", high="red") +
  labs(y = "Whole plot cover (%)",
       x = "Species code",
       fill = "MAT95 (°C)") +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "lightgrey", color = NA),
        strip.text.y = element_text(angle = 0))
