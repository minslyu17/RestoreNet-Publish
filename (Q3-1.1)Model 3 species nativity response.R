# Current question:
# 3. Did the soil surface or seeding treatments or other abiotic treatments improve the (unseeded) native or non-native plant community establishment over time?
# "Q3-1.1" means Question 3, first season, response variable 1
# 3-1.1 before drought? response var: "seedling_count"

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

citation()
citation("dplyr")
citation("tidyverse")
citation("glmmTMB")
citation("ggplot2")
citation("sjPlot")

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
Model1All <- within( Model1All, {SitePlot <- factor(concatenated)})

# take out all the seeded species(Y-warm,Y-cool,N-controls, just left with N), and separate by nativity so we would have 2 dataset(N native, N non-native).
# run it with model 1 with the same 3 response variables(seedling_count, growth and whole_plot_cover), but don't need "SeedTreat" as fixed effect, so only Site and SoilSurfaceTreat?
Model1All_filtered <- filter(Model1All, (Seededornot == "N"))

# since species and seedmix are not considered, we take all unseeded native species as a group, and unseeded non-native species as a group
# sum seedling_count by site, plot, seasonyear, SoilSurfaceTreat and Nativity
Model1All_filtered_summary <- Model1All_filtered %>%
  group_by(Site, Plot, seasonyear, SoilSurfaceTreat, Nativity) %>%
  summarise(
    sum_count = sum(Seedling_Count, na.rm = TRUE),
    mean_height = mean(Avg_Height_mm, na.rm = TRUE),
    Whole_plot_cover = sum(Whole_plot_cover, na.rm = TRUE)
  )

# for 3 sites, 36 plots, 4 seasons, 5 SoilSurfaceTreats and 2 Nativities, it should include 4320 observations
# need to add in the zeros to ensure each SoilSurfaceTreats in each plot/site/seasonyear have 2 observations including native and nonnative
library(tidyverse)
full_combinations <- expand_grid(
  Site = unique(Model1All$Site),
  Plot = unique(Model1All$Plot),
  seasonyear = unique(Model1All$seasonyear),
  SoilSurfaceTreat = unique(Model1All$SoilSurfaceTreat),
  Nativity = unique(Model1All$Nativity)
)

# left join to fill in the observations that we have
filled <- full_combinations %>% 
  left_join(Model1All_filtered_summary, by = c("Site", "Plot", "seasonyear", "SoilSurfaceTreat", "Nativity"))
# fill the NA with 0
Model1All_filtered_summary_filled <- filled %>% 
  mutate(across(c(sum_count, mean_height, Whole_plot_cover), ~ replace_na(.x, 0)))

# filtered the first season to be the dataset for this sub question
Model3_a1_filtered <- filter(Model1All_filtered_summary_filled, 
                               (seasonyear == "spring 2020"))

# separate by nativity
Model3_a1_filtered_native <- filter(Model3_a1_filtered, (Nativity == "native"))
Model3_a1_filtered_nonnative <- filter(Model3_a1_filtered, (Nativity == "nonnative"))


#------------------------------------------------------------------------------#
#----------#1) univariate relationships between individual explanatory and response variable(sum_count)
#------------------------------------------------------------------------------#
library(glmmTMB)

##### data = Model3_a1_filtered
# Initialize an empty list to store models
univar_models <- list()

# List of predictor variables
predictor_vars <- c("Site", "SoilSurfaceTreat", "Nativity")

# Loop through each predictor variable
for (var in predictor_vars) {
  # Build the formula for glmmTMB
  formula <- as.formula(paste("sum_count ~", var))
  
  # Fit glmmTMB model
  univar_models[[var]] <- glmmTMB(formula, data = Model3_a1_filtered, 
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
## significant factors from the univariate were:  (AIC value ascending order)
#### all: Site, Nativity, SoilSurfaceTreat
#### native: Site, SoilSurfaceTreat 
#### nonnative: Site, SoilSurfaceTreat 

# Since we are focusing on only 3 factors(Site, SoilSurfaceTreat, seasonyear and maybe Nativity), and they are all going to be included into the model, so no need for univariate analysis anymore
################################################################################

#------------------------------------------------------------------------------#
#----------#2) significant factors to build model
#------------------------------------------------------------------------------#
library(glmmTMB)
library(car)

#### native
Model3_a1_native <- glmmTMB(sum_count ~   Site+SoilSurfaceTreat
                            #+Site*SoilSurfaceTreat
                            +(1|Site) +(1|Plot),
                            data = Model3_a1_filtered_native,
                            ziformula = ~1,
                            family = nbinom1)
summary(Model3_a1_native)
# Zero-inflation model fits! p-value small enough!
Anova(Model3_a1_native, type = "III") # site and soilsurfacetreat are sig

#### nonnative
Model3_a1_nonnative <- glmmTMB(sum_count ~   Site+SoilSurfaceTreat
                               +Site:SoilSurfaceTreat
                               +(1|Site) +(1|Plot),
                            data = Model3_a1_filtered_nonnative,
                            ziformula = ~1,
                            family = nbinom1)
summary(Model3_a1_nonnative)
# Zero-inflation model fits! p-value small enough!
Anova(Model3_a1_nonnative, type = "III") # site and interaction are sig


#------------------------------------------------------------------------------#
#----------#3) model checks and statistics
#------------------------------------------------------------------------------#

# creates HTML tables from regression models
library(sjPlot)
tab_model(Model3_a1_native)
tab_model(Model3_a1_nonnative)

#### loading post hoc comparisons required libraries
# do post hoc if the factor is significant in the anova, and for the post hoc comparison we use "pairwise" function 
library(emmeans)
library(multcomp)

# Model3_a1_native - Site
marginal = emmeans(Model3_a1_native,
                   ~ Site)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs)
# !preserve the best

# Model3_a1_native - SoilSurfaceTreat
marginal = emmeans(Model3_a1_native,
                   ~ SoilSurfaceTreat)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs)  # ConMod the best


# Model3_a1_nonnative - SoilSurfaceTreat(not sig)
# Model3_a1_nonnative - Site
marginal = emmeans(Model3_a1_nonnative,
                   ~ Site)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs)
# !same preserve the best, but p-value 0.08

# Model3_a1_nonnative - Site*SoilSurfaceTreat
marginal = emmeans(Model3_a1_nonnative,
                   ~ Site|SoilSurfaceTreat)
contrast(marginal, method = "tukey")

pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs)




#------------------------------------------------------------------------------#
# Answer the  question(first season, sum_count):
# 1. soil surface treatments - ConMod the best for native, nonnative not sig
# 2. site? - preserve site the best for native/nonnative
# 3. Nativity - no sig


#------------------------ try to plot the results -----------------------------#
library(ggplot2)

Model3_a1_filtered_summary <- Model3_a1_filtered %>%
  group_by(Site, SoilSurfaceTreat, Nativity) %>%
  summarise(
    mean_sum_count = mean(sum_count, na.rm = TRUE),
    SE_sum_count = sd(sum_count, na.rm = TRUE) / sqrt(n())
  )
# Averaged Total Seedling Numbers of Unseeded Species in the Pre-drought Season by Site and Surface Treatments
plot3_a1 <- ggplot(Model3_a1_filtered_summary, 
                   aes(x = Site, y = mean_sum_count, fill = SoilSurfaceTreat)) +
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.6) +
  geom_errorbar(aes(ymin = mean_sum_count - SE_sum_count, ymax = mean_sum_count + SE_sum_count), 
                position = position_dodge(0.7), width = 0.2) +
  facet_grid(Nativity ~ .) +
  labs(y = "Unseeded speceis density (#/subplot)",
       x = "Site",
       fill = "Surface treatments") +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "lightgrey", color = NA),
        strip.text.y = element_text(angle = -90))
