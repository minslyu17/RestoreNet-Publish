# Current question:
# 3. Did the soil surface or seeding treatments or other abiotic treatments improve the (unseeded) native or non-native plant community establishment over time?
# "Q3-2.1" means Question 3, last 3 seasons, response variable 1
# 3-2.1 after drought? response var: "seedling_count"

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

# take out all the seeded species(Y-warm,Y-cool,N-controls, just left with N), and separate by nativity so we would have 2 datasets(N native, N non-native).
Model1All_filtered <- filter(Model1All, (Seededornot == "N"))

# since species and seedmix are not considered, we take all unseeded native species as a group, and unseeded non-native species as a group
# sum seedling_count by site, plot, seasonyear, SoilSurfaceTreat and nativity
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


# filtered the last 3 seasons to be the dataset for this sub question
Model3_b1_filtered <- filter(Model1All_filtered_summary_filled, 
                             (seasonyear == "spring 2021"| seasonyear == "fall 2021"| seasonyear == "spring 2022"))

# separate by nativity and then by spring and fall
Model3_b1_filtered_native <- filter(Model3_b1_filtered, (Nativity == "native"))
Model3_b1_filtered_native_spring <- filter(Model3_b1_filtered_native, 
                                    (seasonyear == "spring 2021"| seasonyear == "spring 2022"))
Model3_b1_filtered_native_fall <- filter(Model3_b1_filtered_native, 
                                    (seasonyear == "fall 2021"))

Model3_b1_filtered_nonnative <- filter(Model3_b1_filtered, (Nativity == "nonnative"))
Model3_b1_filtered_nonnative_spring <- filter(Model3_b1_filtered_nonnative, 
                                       (seasonyear == "spring 2021"| seasonyear == "spring 2022"))
Model3_b1_filtered_nonnative_fall <- filter(Model3_b1_filtered_nonnative, 
                                       (seasonyear == "fall 2021"))


#------------------------------------------------------------------------------#
#----------#1) univariate relationships between individual explanatory and response variable(seedling_Count)
#------------------------------------------------------------------------------#

################################################################################
# Since we are focusing on only 3 factors(Site, SoilSurfaceTreat, seasonyear and maybe Nativity), and they are all going to be included into the model, so no need for univariate analysis
################################################################################

#------------------------------------------------------------------------------#
#----------#2) significant factors to build model
#------------------------------------------------------------------------------#
library(glmmTMB)
library(car)

#### native
# spring
Model3_b1_native_spring <- glmmTMB(sum_count ~   Site+SoilSurfaceTreat+seasonyear
                                   +Site:SoilSurfaceTreat
                                   +(1|Site) +(1|Plot),
                            data = Model3_b1_filtered_native_spring,
                            ziformula = ~1,
                            family = nbinom1)
summary(Model3_b1_native_spring)
# Zero-inflation model fits! p-value small enough!
Anova(Model3_b1_native_spring, type = "III") # Site, SoilSurfaceTreat and seasonyear are all sig

# fall
Model3_b1_native_fall <- glmmTMB(sum_count ~   Site+SoilSurfaceTreat
                                 +(1|Site) +(1|Plot),
                            data = Model3_b1_filtered_native_fall,
                            #ziformula = ~1,
                            family = nbinom1)
summary(Model3_b1_native_fall)
# Zero-inflation cause problem, thereby removing the term
Anova(Model3_b1_native_fall, type = "III") # Site is sig


#### nonnative
# spring
Model3_b1_nonnative_spring <- glmmTMB(sum_count ~   Site+SoilSurfaceTreat+seasonyear
                                      #+Site*SoilSurfaceTreat
                                      +(1|Site) +(1|Plot),
                               data = Model3_b1_filtered_nonnative_spring,
                               ziformula = ~1,
                               family = nbinom1)
summary(Model3_b1_nonnative_spring)
# Zero-inflation model fits! p-value small enough!
Anova(Model3_b1_nonnative_spring, type = "III") # Site, SoilSurfaceTreat and seasonyear are all sig

# fall
Model3_b1_nonnative_fall <- glmmTMB(sum_count ~   Site+SoilSurfaceTreat
                                    +(1|Site) +(1|Plot),
                            data = Model3_b1_filtered_nonnative_fall,
                            ziformula = ~1,
                            family = nbinom1)
summary(Model3_b1_nonnative_fall)
# Zero-inflation model fits! p-value small enough!
Anova(Model3_b1_nonnative_fall, type = "III") # SoilSurfaceTreat is sig


#------------------------------------------------------------------------------#
#----------#3) model checks and statistics
#------------------------------------------------------------------------------#

# creates HTML tables from regression models
library(sjPlot)
tab_model(Model3_b1_native_spring)
tab_model(Model3_b1_native_fall)
tab_model(Model3_b1_nonnative_spring)
tab_model(Model3_b1_nonnative_fall)

#### loading post hoc comparisons required libraries
# do post hoc if the factor is significant in the anova, and for the post hoc comparison we use "pairwise" function 
library(emmeans)
library(multcomp)


# Model3_b1_native_spring - SoilSurfaceTreat
marginal = emmeans(Model3_b1_native_spring,
                   ~ SoilSurfaceTreat)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs) # !pits the best

# Model3_b1_native_spring - Site
marginal = emmeans(Model3_b1_native_spring,
                   ~ Site)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs) # !preserve the best

# Model3_b1_native_spring - seasonyear
marginal = emmeans(Model3_b1_native_spring,
                   ~ seasonyear)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs) # !spring 2022 > 2021

# Model3_b1_native_spring - Site*SoilSurfaceTreat
marginal = emmeans(Model3_b1_native_spring,
                   ~ Site|SoilSurfaceTreat)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs)



# Model3_b1_native_fall - SoilSurfaceTreat(not sig)
# Model3_b1_native_fall - Site
marginal = emmeans(Model3_b1_native_fall,
                   ~ Site)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs) # ! preserve the best


# Model3_b1_nonnative_spring - SoilSurfaceTreat
marginal = emmeans(Model3_b1_nonnative_spring,
                   ~ SoilSurfaceTreat)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs) # !pits the best

# Model3_b1_nonnative_spring - Site
marginal = emmeans(Model3_b1_nonnative_spring,
                   ~ Site)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs) # !Roo the best

# Model3_b1_nonnative_spring - seasonyear
marginal = emmeans(Model3_b1_nonnative_spring,
                   ~ seasonyear)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs) # !spring 2021 > 2022


# Model3_b1_nonnative_fall - Site(not sig)
# Model3_b1_nonnative_fall - SoilSurfaceTreat
marginal = emmeans(Model3_b1_nonnative_fall,
                   ~ SoilSurfaceTreat)
contrast(marginal, method = "tukey")
pairwise_IRRs <- contrast(marginal, method = "pairwise", type = "response")
print(pairwise_IRRs) # !pits the best


#------------------------------------------------------------------------------#
# Answer the question(last 3 seasons, Seedling_Count):
# native-spring
# 1. soil surface treatments - !sig, pits the best
# 2. site? - !sig, preserve the best
# 3. seasonyear - !sig, spring 2022 > 2021

# native-fall
# 1. soil surface treatments - no sig
# 2. site? - !sig, preserve the best

# nonnative-spring
# 1. soil surface treatments - !sig, pits the best
# 2. site? - !sig, Roo the best
# 3. seasonyear - !sig, spring 2021 > 2022

# nonnative-fall
# 1. soil surface treatments - !sig, pits the best
# 2. site? - no sig 


#------------------------ try to plot the results -----------------------------#
# the mean seedling count 
library(ggplot2)
# separate by seasons only
Model3_b1_filtered_spring <- filter(Model3_b1_filtered, (seasonyear == "spring 2021"| seasonyear == "spring 2022"))
Model3_b1_filtered_fall <- filter(Model3_b1_filtered, (seasonyear == "fall 2021"))


#### spring (native + non-native)
Model3_b1_filtered_spring_summary <- Model3_b1_filtered_spring %>%
  group_by(Site, SoilSurfaceTreat, Nativity, seasonyear) %>%
  summarise(
    mean_sum_count = mean(sum_count, na.rm = TRUE),
    SE_sum_count = sd(sum_count, na.rm = TRUE) / sqrt(n())
  )
# Averaged Total Seedling Numbers of Unseeded Species in the Post-drought Spring Seasons by Site and Surface Treatments
plot3_b1_spring = ggplot(Model3_b1_filtered_spring_summary, 
                         aes(x = Site, y = mean_sum_count, fill = SoilSurfaceTreat)) +
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.6) +
  geom_errorbar(aes(ymin = mean_sum_count - SE_sum_count, ymax = mean_sum_count + SE_sum_count), 
                position = position_dodge(0.7), width = 0.2) +
  facet_grid(Nativity ~ seasonyear) +
  labs(y = "Unseeded speceis density (#/subplot)",
       x = "Site",
       fill = "Surface treatments") +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "lightgrey", color = NA),
        strip.text.y = element_text(angle = -90))

#### fall (native + non-native)
Model3_b1_filtered_fall_summary <- Model3_b1_filtered_fall %>%
  group_by(Site, Nativity) %>%
  summarise(
    mean_sum_count = mean(sum_count, na.rm = TRUE),
    SE_sum_count = sd(sum_count, na.rm = TRUE) / sqrt(n())
  )
# Averaged Total Seedling Numbers of Unseeded Species in the Post-drought Fall Season by Site and Surface Treatments
plot3_b1_fall = ggplot(Model3_b1_filtered_fall_summary, 
                       aes(x = Site, y = mean_sum_count)) +
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.6) +
  geom_errorbar(aes(ymin = mean_sum_count - SE_sum_count, ymax = mean_sum_count + SE_sum_count), 
                position = position_dodge(0.7), width = 0.2) +
  facet_grid(. ~ Nativity) +
  labs(y = "Unseeded speceis density (#/subplot)",
       x = "Site") +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "lightgrey", color = NA),
        strip.text.y = element_text(angle = -90))
