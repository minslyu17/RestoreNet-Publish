# Current question:
# 2. Did certain species do better than others? Did this differ by site? Was there a treatment by species effect? Could this be predicted by their MAT95 or their MAP95?
# "Q2-1.3" means Question 2, first season, response variable 3
# 2-1.3 before drought, response var: "Whole_plot_cover", include species(MAT95) as a fixed effect

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
Model2_a2_filtered


#------------------------------------------------------------------------------#
#----------#1) univariate relationships between individual explanatory and response variable (Whole_plot_cover)
#------------------------------------------------------------------------------#
library(glmmTMB)

##### data = Model2_a2_filtered
# Initialize an empty list to store models
univar_models <- list()

# List of predictor variables
predictor_vars <- c("MAP_mm", "Monsoonoftotal", "MAT", "elevation", "PPT_since_lastsampling_mm", "total_PPT", "cumulative_PPT", "SeedTreat", "SoilSurfaceTreat", "MAP95", "Site","MAT95","Species")

# Loop through each predictor variable
for (var in predictor_vars) {
  # Build the formula for glmmTMB
  formula <- as.formula(paste("Whole_plot_cover ~", var))
  
  # Fit glmmTMB model
  univar_models[[var]] <- glmmTMB(formula, data = Model2_a2_filtered, 
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
#### Spring: Species, MAT95, MAP95 (AIC value ascending order)

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
Model2_a3 <- glmmTMB(Whole_plot_cover ~   Site+SoilSurfaceTreat+MAT95
                            #+Site*MAT95
                            #+(1|Site) 
                            +(1|SitePlot),
                            data = Model2_a2_filtered,
                            #ziformula = ~1,
                            family = nbinom1)
summary(Model2_a3)
# taking random effect site out, the NAs are gone
Anova(Model2_a3, type = "III") # MAT95 is sig

#------------------------------------------------------------------------------#
#----------#3) model checks and statistics
#------------------------------------------------------------------------------#

# creates HTML tables from regression models
library(sjPlot)
tab_model(Model2_a3)


#### loading post hoc comparisons required libraries
# do post hoc if the factor is significant in the anova, and for the post hoc comparison we use "pairwise" function 
library(emmeans)
library(multcomp)

# Model2_a3 - SoilSurfaceTreat(not sig)
# Model2_a3 - MAT95
# Model2_a3 - Site
marginal = emmeans(Model2_a3,
                   ~ Site)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs) # !pleasant the best


#------------------------------------------------------------------------------#
# Answer the question(first season, Whole_plot_cover):
# 1. soil surface treatments - no sig effect
# 2. species - yes!
# 3. site - yes!pleasant the best



library(ggplot2)
Model2_a3_filtered_summary <- Model2_a2_filtered %>%
  group_by(Species, MAT95, Site) %>%
  summarise(
    mean_cover = mean(Whole_plot_cover, na.rm = TRUE),
    SE_cover = sd(Whole_plot_cover, na.rm = TRUE) / sqrt(n())
  )

# generalize plot about Average Whole Plot Coverage in the Pre-Drought Season by Species and Site
plot2_a3 = ggplot(Model2_a3_filtered_summary, 
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
