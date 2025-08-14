# Seeded species at wrong plot

# this time with it broken down as to which seeded treatment plots they were found in.
# find out how many seedlings are there emerged at the "wrong" plot by species
# get a new "Model1All" dataset that filtered out these wrong plot species


## clear work space ##
rm(list = ls())

## use the modified Model1All(same code from Q1 file) for the filtering
library(readxl)
library(dplyr)
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
# concatenating site:plot so that we can summarize based on that
Model1All$concatenated <- paste(Model1All$Site, Model1All$Plot)
# Set categorical factors for main effects:
Model1All <- within(Model1All, {SitePlot <- factor(concatenated)})
View(Model1All)


#### cool plot but warm species showed up
Model1All_filtered_cool <- filter(Model1All,(SeedTreat == "Cool") & (Seededornot == "Y_warm"))
#37 rows

# group by species, calculates the sum seedling counts of each species
Model1All_filtered_cool_SumSeedlings <- Model1All_filtered_cool %>%
  group_by(Species_Code) %>%
  summarise(Total_SeedlingCount = sum(Seedling_Count, na.rm = TRUE))


#### warm plot but cool species showed up
Model1All_filtered_warm <- filter(Model1All,(SeedTreat == "Warm") & (Seededornot == "Y_cool"))
#47 rows

# group by species, calculates the sum seedling counts of each species
Model1All_filtered_warm_SumSeedlings <- Model1All_filtered_warm %>%
  group_by(Species_Code) %>%
  summarise(Total_SeedlingCount = sum(Seedling_Count, na.rm = TRUE))


#------------------------------------------------------------------------------#
#### try to filter these wrong species out from Model2b using setdiff 
# getting cool(37) out
View(Model2b_filtered)  # 1523
WrongSpeciesOut <- setdiff(Model2b_filtered, Model1All_filtered_cool)
View(WrongSpeciesOut)  # 1486 - checked

# getting warm(47) out
WrongSpeciesOut2 <- setdiff(WrongSpeciesOut, Model1All_filtered_warm)
View(WrongSpeciesOut2)  # 1440


# try to find out which row is not count in this 1440
check <- setdiff(WrongSpeciesOut, WrongSpeciesOut2)
View(check)  
View(Model1All_filtered_warm)
# find diff between the 2 dataset above
# the mismatch is at line#38 - SPAMA at spring 2020, plot 31, treatment Mulch
# no wonder model2b(only have the last 3 seasons) dosen't have this SPAMA

# so the "WrongSpeciesOut2" is the desired one, try it on Q1-2.1
# replace the model2b_filtered with it but not changing the name, makes the work easier
Model2b_filtered <- WrongSpeciesOut2


#------------------------------------------------------------------------------#
#### try to filter these wrong species out from Model1All using setdiff, so that it would be easier to try this updated Model1All in all sub question scripts
# getting cool(37) out
View(Model1All)  # 4085

WrongSpeciesOut <- setdiff(Model1All, Model1All_filtered_cool)
# WrongSpeciesOut have 4048 rows - checked

# getting warm(47) out
WrongSpeciesOut2 <- setdiff(WrongSpeciesOut, Model1All_filtered_warm)
# WrongSpeciesOut2 have 4001 rows - checked

Model1All <- WrongSpeciesOut2


#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
# All in all, the code to filter out the wrong species
library(dplyr)
Model1All_filtered_cool <- filter(Model1All,(SeedTreat == "Cool") & (Seededornot == "Y_warm"))
Model1All_filtered_warm <- filter(Model1All,(SeedTreat == "Warm") & (Seededornot == "Y_cool"))

# all wrong species
Model1All_filtered_wrong <- rbind(Model1All_filtered_cool, Model1All_filtered_warm)

# get these out from Model1All
WrongSpeciesOut <- setdiff(Model1All, Model1All_filtered_wrong)
Model1All <- WrongSpeciesOut
