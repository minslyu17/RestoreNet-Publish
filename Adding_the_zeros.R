# Model2_filtered_SumChecks


library(readr)
Model2_filtered <- read_csv("2024/Data for models/Model2_filtered.csv")
View(Model2_filtered)

# Verify that Helen added the zeros accurately.
# That is, only observed species were recorded. 
# Then after 0s needed to be added to species that were not observed (that should be).  
# 0’s needed to be added to non-observed species.  
# HR to provide master list that will dictate the rows that need to be assigned a 0 if missing – TABLE 3.


table(Model2_filtered$seasonyear) # Model2_filtered only containing the last 3 seasons
table(Model2_filtered$Species_Code) # containing just these 10 species

####### check the sum of seedling counts for this 3 seasons with Table.3

library(dplyr)
Model2_filtered_seeded = Model2_filtered %>% filter(Seeded %in% c("Y_cool","Y_warm"))

Model2_filtered_SumSeedlings <- Model2_filtered_seeded %>%
  group_by(seasonyear, Species_Code) %>%
  summarise(Total_SeedlingCount = sum(Seedling_Count, na.rm = TRUE))

View(Model2_filtered_SumSeedlings)
# I checked each sum at each season, there are 2 didn't match both in fall 2021, BORO2(61/62) and SECO10(13/15).

# this plot doesn't have that much use
library(ggplot2)
Model2_filtered_SumSeedlings %>%
  ggplot( aes(x = seasonyear, y = Total_SeedlingCount, fill = Species_Code)) +
  geom_bar(stat = "identity", position="dodge") +
  geom_text(aes(label = Total_SeedlingCount), position = position_dodge(width = 0.9), 
            vjust = -0.5)
  labs(title = "Total Seedling Counts by Seasons and Species",
       x = "Seasons",
       y = "Total Seedling Count")


  
  
###### find out why in fall 2021, BORO2(61/62) and SECO10(13/15) didn't match.
# listed all the non-zero seedling count rows of BORO2 in fall 2021
Model2_filtered_seeded_2021f_BORO2 = Model2_filtered_seeded %>% filter(seasonyear == "fall 2021" & Species_Code == "BORO2")

View(Model2_filtered_seeded_2021f_BORO2)
# I think I need the dataset that Table.3 used to compare with, so that I can find out whether there are some rows that are accidentally altered.

# listed all the non-zero seedling count rows of SECO10 in fall 2021
Model2_filtered_seeded_2021f_SECO10 = Model2_filtered_seeded %>% filter(seasonyear == "fall 2021" & Species_Code == "SECO10")

View(Model2_filtered_seeded_2021f_SECO10)
# same with the previous one



###### check if the sum=0 are all in those non-observed species(0 seedling count in Table.3)
# find out what species have 0 total seedling count
###### try the FallDatawithZreos file, get the sum of seeded species at that file
library(readxl)
FullDatawithZeros3_27_2024 <- read_excel("2024/Data for models/FullDatawithZeros3_27_2024.xlsx")
View(FullDatawithZeros3_27_2024)

# Y_cool
datacheck <- FullDatawithZeros3_27_2024 %>%
  group_by(siteseason, Plot, Seeded, Seed_Mix) %>%
  summarise(count = n()) %>%
  filter(Seeded == "Y_cool" & count != 5) %>%
  filter(Seed_Mix == "Cool")
View(datacheck) # non!Nice!

# Y_warm
datacheck2 <- FullDatawithZeros3_27_2024 %>%
  group_by(siteseason, Plot, Seeded, Seed_Mix) %>%
  summarise(count = n()) %>%
  filter(Seeded == "Y_warm" & count != 5) %>%
  filter(Seed_Mix == "Warm")
View(datacheck2) # non!Nice!


# N_control
datacheck3 <- FullDatawithZeros3_27_2024 %>%
  group_by(siteseason, Plot, Seeded, Seed_Mix) %>%
  summarise(count = n()) %>%
  filter(Seeded == "N_control") %>%
  filter(count != 10)
View(datacheck3)
###### we are here on Sept 25



###### try left join - for each row in datacheck3, find what's the missing species
## get all the N_control group observed species
datacheck_N_control <- FullDatawithZeros3_27_2024 %>% 
  group_by(siteseason, Plot, Seeded, Seed_Mix, Species_Code) %>% 
  filter(Seeded == "N_control") %>% 
  summarise(count = n())
View(datacheck_N_control)

## get all combinations
# copy all 10 species codes here into a dataframe for the following left join
species10 <- data.frame(Species_Code = c("ARPUP6","BAMU","BOAR","BORO2","DICA8","LUSP2","PLOV","SACO6","SECO10","SPAMA"))
View(species10)

# get all unique siteseason and plot comb from datacheck3
unique_siteseason_plot <- datacheck3 %>% distinct(siteseason, Plot)
View(unique_siteseason_plot)

# product species 10 and unique comb 
library(tidyr)
all_combinations <- expand_grid(unique_siteseason_plot, species10)
View(all_combinations)

## left_join and filter the NA's(missing species)
result <- left_join(all_combinations, datacheck_N_control, by = c("Seeded", "siteseason", "Plot", "Species_Code"))
View(result)

missingSpecies <- result %>% filter(is.na(count))
View(missingSpecies)

