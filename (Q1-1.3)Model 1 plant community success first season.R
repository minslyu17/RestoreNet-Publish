# Current question:
# 1. Do certain surface treatments or seed mix types (cool vs warm) improve establishment of seeded species and natural native plant regeneration? Does this differ by site? 
# "Q1-1.3" means Question 1, first season, response variable 3
# 1-1.3 before drought, response var: "Whole_plot_cover"


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
# using the same "Model1_a2_filtered" dataset as (Q1-1.2) since we just change to another response var
Model1_a2_filtered


#------------------------------------------------------------------------------#
#----------#1) univariate relationships between individual explanatory and response variable(Whole_plot_cover)
#------------------------------------------------------------------------------#

##### data = Model1_a2_filtered
library(glmmTMB)
# Initialize an empty list to store models
univar_models1_a2 <- list()

# List of predictor variables
predictor_vars <- c("MAP_mm", "MAT", "elevation", "PPT_since_lastsampling_mm", "total_PPT", "cumulative_PPT", "SeedTreat", "SoilSurfaceTreat", "MAP95", "Site","MAT95")

# Loop through each predictor variable
for (var in predictor_vars) {
  # Build the formula for glmmTMB
  formula <- as.formula(paste("Whole_plot_cover ~", var))
  
  # Fit glmmTMB model
  univar_models1_a2[[var]] <- glmmTMB(formula, data = Model1_a2_filtered, 
                                      ziformula = ~1,
                                      family = nbinom1)
}

# Print summary for each model
for (i in seq_along(predictor_vars)) {
  cat("Summary for model with predictor variable:", predictor_vars[i], "\n")
  print(summary(univar_models1_a2[[i]]))
  cat("\n\n")
}
# Extract AIC values from each model
AIC_values1_a2 <- sapply(univar_models1_a2, function(model) AIC(model))

# Print AIC values
print(AIC_values1_a2)
best_model_index1_a2 <- which.min(AIC_values1_a2)
print(best_model_index1_a2)

################################################################################
## significant factors from the univariate were: 
#  MAT95, MAP95, SeedTreat(AIC value ascending order)

# MAT95: species specific
# MAP95: species specific
# SeedTreat

# Fixed factors to include: (Dr Rowe's table)
# 1) Site or other site or climate variables (depending on which is more significant in the univariate analyses), 2)surface treatment, 3)seeding treatment, 4) site
# the point of the univariate relationship analysis is to find sig climate variables, and compare them with "Site". If none of their AIC is smaller than Site's, we go by "Site" as fixed effect.

# Based on this and the results we got above, SeedTreat is the most sig factor, so we stick to these 3 - "Site", "SoilSrufaceTreat", "SeedTreat"
################################################################################


#------------------------------------------------------------------------------#
#----------#2) significant factors to build model
#------------------------------------------------------------------------------#
library(glmmTMB)
# sig factors as fix effects and siteplot as random effect
# site as random effect and tell the model for each site there are repeated measure

# interactions not significant, so took them out
Model1_a3 <- glmmTMB(Whole_plot_cover ~   Site+SeedTreat+SoilSurfaceTreat
                        +(1|Site) +(1|SitePlot),
                        data = Model1_a2_filtered,
                        #ziformula = ~1,
                        family = nbinom1)
summary(Model1_a3) # Zero-inflation model: p-value = 0.997, thereby remove this term
Anova(Model1_a3, type = "III") #And yes, only SeedTreat is sig.


#------------------------------------------------------------------------------#
#----------#3) model checks and statistics
#------------------------------------------------------------------------------#

# creates HTML tables from regression models
library(sjPlot)
tab_model(Model1_a3)

#### loading post hoc comparisons required libraries
# do post hoc if the factor is significant in the anova, and for the post hoc comparison we use "pairwise" function 
library(emmeans)
library(multcomp)

# Model1_a3 - Site(not sig)
# Model1_a3 - SoilSurfaceTreat(not sig)
# Model1_a3 - SeedTreat
marginal = emmeans(Model1_a3,
                   ~ SeedTreat)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs)
# !cool > warm


#------------------------------------------------------------------------------#
# Answer the  question(first season, Whole_plot_cover):
# 1. surface treatments - no sig effect
# 2. seed mix types (cool vs warm) - yes! Cool species better than warm
# 3. differ by site? - no sig diff


#------------------------ try to plot the results -----------------------------#

library(ggplot2)
Model1_a3_filtered_summary <- Model1_a2_filtered %>%
  group_by(Seed_Mix) %>%
  summarise(
    mean_cover = mean(Whole_plot_cover, na.rm = TRUE),
    SE_cover = sd(Whole_plot_cover, na.rm = TRUE) / sqrt(n())
  )

# generalize plot about Average Whole Plot Coverage in the Pre-Drought Season by Seed Treatments
plot1_a3 = ggplot(Model1_a3_filtered_summary, aes(x = Seed_Mix, y = mean_cover)) +
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.6) +
  geom_errorbar(aes(ymin = mean_cover - SE_cover, ymax = mean_cover + SE_cover), 
                position = position_dodge(0.7), width = 0.2) +
  labs(y = "Whole plot cover (%)",
       x = "Seed Treatments") +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "lightgrey", color = NA),
    strip.text.y = element_text(angle = 0))