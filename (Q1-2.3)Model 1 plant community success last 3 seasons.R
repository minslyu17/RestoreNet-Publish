# Current question:
# 1. Do certain surface treatments or seed mix types (cool vs warm) improve establishment of seeded species and natural native plant regeneration? Does this differ by site? 
# "Q1-2.3" means Question 1, last 3 seasons, response variable 3
# 1-2.3 after drought, response var: "Whole_plot_cover"


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
# using the same 2 separated-season dataset since we are just change to another response var
Model1_b2_filtered_spring 
Model1_b2_filtered_fall


#------------------------------------------------------------------------------#
#----------#1) univariate relationships between individual explanatory and response variable(Whole_plot_cover)
#------------------------------------------------------------------------------#
library(glmmTMB)

##### data = Model1_b2_filtered_spring
# Initialize an empty list to store models
univar_models <- list()

# List of predictor variables(removing Monsoonoftotal)
predictor_vars <- c("MAP_mm", "MAT", "elevation", "PPT_since_lastsampling_mm", "total_PPT", "cumulative_PPT", "SeedTreat", "SoilSurfaceTreat", "MAP95", "Site","MAT95")

# Loop through each predictor variable
for (var in predictor_vars) {
  # Build the formula for glmmTMB
  formula <- as.formula(paste("Whole_plot_cover ~", var))
  
  # Fit glmmTMB model
  univar_models[[var]] <- glmmTMB(formula, data = Model1_b2_filtered_spring,
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


##### data = Model1_b2_filtered_fall
# Initialize an empty list to store models
univar_models <- list()

# List of predictor variables(removing Monsoonoftotal)
predictor_vars <- c("MAP_mm", "MAT", "elevation", "PPT_since_lastsampling_mm", "total_PPT", "cumulative_PPT", "SeedTreat", "SoilSurfaceTreat", "MAP95", "Site","MAT95")

# Loop through each predictor variable
for (var in predictor_vars) {
  # Build the formula for glmmTMB
  formula <- as.formula(paste("Whole_plot_cover ~", var))
  
  # Fit glmmTMB model
  univar_models[[var]] <- glmmTMB(formula, data = Model1_b2_filtered_fall,
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
#### spring: MAT95, MAP95, SeedTreat(AIC value ascending order)

#### fall: MAT95, Site, elevation(AIC value ascending order)

# Fixed factors to include: (Dr Rowe's table)
# 1) Site or other site or climate variables (depending on which is more significant in the univariate analyses), 2)surface treatment, 3)seeding treatment, 4) site

# Based on this and the results we got above, still we stick to these 3 - "Site", "SoilSrufaceTreat", "SeedTreat" for both spring model and fall model
################################################################################

#------------------------------------------------------------------------------#
#----------#2) significant factors to build model
#------------------------------------------------------------------------------#

###other than just for modelling purpose - separate seasons for a ecological reason- specific species may not grow at the same time####

# springs only
Model1_b3_spring <- glmmTMB(Whole_plot_cover ~   Site+SeedTreat+SoilSurfaceTreat+Season
                                       #+SeedTreat*SoilSurfaceTreat
                                       +(1|Site) +(1|SitePlot),
                                       data = Model1_b2_filtered_spring,
                                       family = nbinom1)
# interactions(SeedTreat*SoilSurfaceTreat) not significant, so took them out
summary(Model1_b3_spring)
# Zero-inflation model: p-value = 0.996, thereby remove this term
Anova(Model1_b3_spring, type = "III") 
# model give some warning, but at the same time ANOVA shows SeedTreat/SoilSurfaceTreat/Season sig


# fall only
Model1_b3_fall <- glmmTMB(Whole_plot_cover ~   Site+SeedTreat+SoilSurfaceTreat
                              #+Site*SeedTreat
                              +(1|Site) +(1|SitePlot),
                              data = Model1_b2_filtered_fall,
                              family = nbinom1)
# interactions(Site*SeedTreat) not significant, so took them out
summary(Model1_b3_fall)
# Zero-inflation model: p-value = 0.998, thereby remove this term
Anova(Model1_b3_fall, type = "III")
# model give some warning, but Site and seedtreat are sig


#------------------------------------------------------------------------------#
#----------#3) model checks and statistics
#------------------------------------------------------------------------------#

## creates HTML tables from regression models
library(sjPlot)
tab_model(Model1_b3_spring)
tab_model(Model1_b3_fall)


#### loading post hoc comparisons required libraries
# do post hoc if the factor is significant in the anova, and for the post hoc comparison we use "pairwise" function 
library(emmeans)
library(multcomp)

# spring-Site(not sig)
# spring-SeedTreat
marginal_spring = emmeans(Model1_b3_spring,
                          ~ SeedTreat)
contrast(marginal_spring, method = "tukey")
pairwise_IRRs_spring <- contrast(marginal_spring, method = "pairwise", type = "response")
print(pairwise_IRRs_spring) 
# p-value < 0.0001, cool > warm

# spring-SoilSurfaceTreat
marginal_spring = emmeans(Model1_b3_spring,
                          ~ SoilSurfaceTreat)
contrast(marginal_spring, method = "tukey")
pairwise_IRRs_spring <- contrast(marginal_spring, method = "pairwise", type = "response")
print(pairwise_IRRs_spring) 
# Pits > ConMod > Seed only
# ConMod and Seed only are very close(ConMod = 1.076166*Seed only)

# spring-Season
marginal_spring = emmeans(Model1_b3_spring,
                          ~ Season)
contrast(marginal_spring, method = "tukey")
pairwise_IRRs_spring <- contrast(marginal_spring, method = "pairwise", type = "response")
print(pairwise_IRRs_spring)  
# p-value=0.0008, spring 2022 > spring 2021


# fall-SoilSurfaceTreat(not sig)
# fall-Site
marginal_fall = emmeans(Model1_b3_fall,
                          ~ Site)
contrast(marginal_fall, method = "tukey")
pairwise_IRRs_fall <- contrast(marginal_fall, method = "pairwise", type = "response")
print(pairwise_IRRs_fall)  
# Preserve > Roo > Pleasant

# fall-SeedTreat
marginal_fall = emmeans(Model1_b3_fall,
                        ~ SeedTreat)
contrast(marginal_fall, method = "tukey")
pairwise_IRRs_fall <- contrast(marginal_fall, method = "pairwise", type = "response")
print(pairwise_IRRs_fall)  
# warm > cool



#------------------------------------------------------------------------------#
# Answer the  question(last 3 seasons, whole_plot_cover):
# spring:
# 1. surface treatments - yes! pits the best
# 2. seed mix types (cool vs warm) - yes! cool > warm
# 3. differ by site? - no sig diff
# 4. season - yes! spring 2022 > spring2021
# fall:
# 1. surface treatments - no sig effect
# 2. seed mix types (cool vs warm) - yes! warm > cool
# 3. differ by site? - yes! Preserve the best


# fall 
Model1_b3_filtered_fall_summary <- Model1_b2_filtered_fall %>%
  group_by(Site, Seed_Mix, Treatment) %>%
  summarise(
    mean_cover = mean(Whole_plot_cover, na.rm = TRUE),
    SE_cover = sd(Whole_plot_cover, na.rm = TRUE) / sqrt(n())
  )

# generalize plot about Average Whole Plot Coverage in the Post-Drought Fall by Site
plot1_b3_fall <- ggplot(Model1_b3_filtered_fall_summary, aes(x = Seed_Mix, y = mean_cover, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.6) +
  geom_errorbar(aes(ymin = mean_cover - SE_cover, ymax = mean_cover + SE_cover), 
                position = position_dodge(0.7), width = 0.2) +
  facet_grid(. ~ Site) +
  labs(y = "Whole Plot Coverage(%)",
       x = "Seed Treatments",
       fill = "Surface Treatments"
  ) +
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "lightgrey", color = NA),
    strip.text.y = element_text(angle = 0)
  )



# spring
Model1_b3_filtered_spring_summary <- Model1_b2_filtered_spring %>%
  group_by(Seed_Mix, Treatment, seasonyear) %>%
  summarise(
    mean_cover = mean(Whole_plot_cover, na.rm = TRUE),
    SE_cover = sd(Whole_plot_cover, na.rm = TRUE) / sqrt(n())
  )
# generalize plot about Average Whole Plot Coverage in the Post-Drought Spring seasons by Seed and Surface Treatments
plot1_b3_spring <- ggplot(Model1_b3_filtered_spring_summary, aes(x = Seed_Mix, y = mean_cover, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.6) +
  geom_errorbar(aes(ymin = mean_cover - SE_cover, ymax = mean_cover + SE_cover), 
                position = position_dodge(0.7), width = 0.2) +
  facet_grid(seasonyear ~ .) +
  labs(y = "Whole plot cover (%)",
       x = "Seed Treatments",
       fill = "Surface Treatments") +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "lightgrey", color = NA),
    strip.text.y = element_text(angle = 0))


# generalize plot about Average Whole Plot Coverage in the Post-Drought Fall season by Site
summary123 <- Model1_b2_filtered_fall %>%
  group_by(Site, Seed_Mix) %>%
  summarise(
    mean_cover = mean(Whole_plot_cover, na.rm = TRUE),
    SE_cover = sd(Whole_plot_cover, na.rm = TRUE) / sqrt(n())
  )
ggplot(summary123, aes(x = Seed_Mix, y = mean_cover, fill = Site)) +
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.6) +
  geom_errorbar(aes(ymin = mean_cover - SE_cover, ymax = mean_cover + SE_cover), 
                position = position_dodge(0.7), width = 0.2) +
  labs(y = "Whole plot cover (%)",
       x = "Seed Treatments",
       fill = "Site") +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "lightgrey", color = NA),
        strip.text.y = element_text(angle = 0))
