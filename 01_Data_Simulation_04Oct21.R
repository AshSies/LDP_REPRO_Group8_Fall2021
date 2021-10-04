#=================================================================================================#
#============================ LDP: Productivity and Reproducibility ==============================#
#============================== Group Assignment: Pre-Registration ===============================#
#=============== By : Ashton Sies, Emma Nikkel, Jenna Scherger, and Sabrina St-Pierre ============#
#================================== Date : 1st of October 2021 ===================================#
#=================================================================================================#


## Script 01. Data Simulation


# Read in packages ===============================================================
library(tidyverse) # for wrangling data - using primarily dplyr, tidyr, purrr

# Set working directory:===========================================================
#setwd("C:/Users/sabri/OneDrive/Documents/BackUp/Living Data Project/Productivity/Assignements/Reproductibility/doi_10.5061_dryad.5hqbzkh37__v2")
setwd("/Users/jennascherger/Desktop/MSc/Courses/NSERC_CREATE/01_Productivity_Reproducibility/Assignments/02_Reproducible_Research/LDP_REPRO_Group8_Fall2021")

# Read in data:===================================================================
moose_raw <- read.csv("Raw_Data/Moose_presence_abundance.csv")

## OR:

# The data are available on a Dryad repository
# https://datadryad.org/stash/share/svRCeH_hvPZVH2UjmzWQOvErl-B6deccaKMXgGEeHtM


#==================================================================================


# 01. Initial Exploration of Raw Data:=============================================
## By looking at the distribution of the raw data we will know what type of simulation to use
par(mfrow = c(2,4))

hist(moose_raw$Taxar,
     main = "",
     xlab = "Taxar - Year (Raw Data)") # random distribution of years (this will be the random effect in the models)
hist(moose_raw$Forest_class,
     main = "",
     xlab = "Forest Class (Raw Data)") # 4 categorical turned continous (1, 2, 3, 4)
hist(moose_raw$Wolf.territory,
     main = "",
     xlab = "Wolf Territory (Raw Data)") # binary (0 = no; 1 = yes)
hist(moose_raw$Moose.presence,
     main = "",
     xlab = "Moose Presence") # binary (0 = no; 1 = yes) -> lots of 0's
hist(moose_raw$Small.roads,
     main = "",
     xlab = "Small Roads (Raw Data)")  # continuous variable
hist(moose_raw$Big.roads,
     main = "",
     xlab = "Big Roads (Raw Data)") # continuous variable
hist(moose_raw$Pine_proportion,
     main = "",
     xlab = "Pine Proportion (Raw Data)") # continuous variable (0-1 proportion)
hist(moose_raw$Time.since.establishment,
     main = "",
     xlab = "Time Since Wolf Pack Establishment (Raw Data)") # continuous variable in years



# 02. Simulating Raw Data:========================================================
# Set seed
set.seed(1234)

# Creating empty data frame
moose_simulated <- data.frame(matrix(0, ncol = 0, nrow = nrow(moose_raw)))
moose_size <- nrow(moose_simulated)


# A. Forest Class (categorical data)
size_forest <- as.integer(length(moose_raw$Forest_class))

# Determine the probability of each category
prob_1 <- sum(moose_raw$Forest_class == 1)/moose_size
prob_2 <- sum(moose_raw$Forest_class == 2)/moose_size
prob_3 <- sum(moose_raw$Forest_class == 3)/moose_size
prob_4 <- sum(moose_raw$Forest_class == 4)/moose_size

# Run the simulation
moose_simulated$Forest_class <- sample(c(1:4), size = size_forest, replace = TRUE, prob = c(prob_1, prob_2, prob_3, prob_4))

# Compare simulated data to raw data
par(mfrow = c(1,2))
hist(moose_raw$Forest_class,
     main = "",
     xlab = "Forest Class - Raw")
hist(moose_simulated$Forest_class,
     main = "",
     xlab = "Forest Class - Simulated")
table(moose_raw$Forest_class) # 1 = 499; 2 = 2386; 3 = 4301; 4 = 3309
table(moose_simulated$Forest_class) # 1 = 477; 2 = 2387; 3 = 4255; 4 = 3376

##==

# B. Wolf Territory (binomial distribution)
# Determine the probaility of each category
prob_wolf <- sum(moose_raw$Wolf.territory == 1) / moose_size

# Run the simulation
moose_simulated$Wolf.territory <- rbinom(moose_size, 1, prob_wolf)

# Compare simulated data to raw data
hist(moose_raw$Wolf.territory,
     main = "",
     xlab = "Wolf Territory - Raw")
hist(moose_simulated$Wolf.territory,
     main = "",
     xlab = "Wolf Territory - Simulated")
table(moose_raw$Wolf.territory) # 0 = 6518; 1 = 3977
table(moose_simulated$Wolf.territory) # 0 = 6452; 1 = 4043

##==

# C. Moose Presence (binomial distribution)
# Determine the probability of each category 
prob_moose <- sum(moose_raw$Moose.presence == 1) / moose_size

# Run the simulation
moose_simulated$Moose.presence <- rbinom(moose_size, 1, prob_moose)

# Compare simulated data to raw data
hist(moose_raw$Moose.presence,
     main = "",
     xlab = "Moose Presence - Raw")
hist(moose_simulated$Moose.presence,
     main = "",
     xlab = "Moose Presence - Simulated")
table(moose_raw$Moose.presence) # 0 = 9252; 1 = 1242
table(moose_simulated$Moose.presence) # 0 = 9239; 1 = 1256

##==

# D. RASE Presence (binomial distribution)
# Check the probaility of each category
prob_rase <- sum(moose_raw$RASE.presence == 1) / moose_size

# Run the simulation
moose_simulated$Rase.presence <- rbinom(moose_raw$RASE.presence, 1, prob_rase)

# Compare the simulated data to the raw data
hist(moose_raw$RASE.presence,
     main = "",
     xlab = "RASE Presence - Raw")
hist(moose_simulated$Rase.presence,
     main = "",
     xlab = "RASE Presence - Simulated")
table(moose_raw$RASE.presence) # 0 = 5018; 1 = 5477
table(moose_simulated$Rase.presence) # 0 - 4968; 1 = 5527

##==

## E. Small Roads (negative binomial distribution)
hist(moose_raw$Small.roads)
mean(moose_raw$Small.roads)
sd(moose_raw$Small.roads)
moose_simulated$Small.roads <- rnbinom(n = nrow(moose_simulated), mu=254.1479, size=1.25) 

# Compare simulated data to raw data
hist(moose_raw$Small.roads,
     main = "",
     xlab = "Small Roads - Raw")
hist(moose_simulated$Small.roads,
     main = "", 
     xlab = "Small Roads - Simulated")

summary(moose_raw$Small.roads)
summary(moose_simulated$Small.roads)

##==

## F. Big Roads (negative binomial distribution)
mean(moose_raw$Big.roads)
sd(moose_raw$Big.roads)
moose_simulated$Big.roads <- rnbinom(n = nrow(moose_simulated), mu=2288.179, size=1.25) 

# Compare simulated data to raw data
hist(moose_raw$Big.roads,
     main = "",
     xlab = "Big Roads - Raw")
hist(moose_simulated$Big.roads,
     main = "",
     xlab = "Big Roads - Simulated")

summary(moose_raw$Big.roads)
summary(moose_simulated$Big.roads)

##==

## G. Pine Proportion (negative binomial distribution)
moose_simulated$Pine.proportion.100 <- moose_raw$Pine_proportion*100
range(moose_simulated$Pine.proportion.100)
mean(moose_simulated$Pine.proportion.100)

moose_simulated$Pine.proportion <- rnbinom(n = nrow(moose_simulated), mu=3.982581, size=0.52)/100

# Compare simulated data to raw data
summary(moose_raw$Pine_proportion)
summary(moose_simulated$Pine.proportion)

hist(moose_raw$Pine_proportion,
     main = "",
     xlab = "Pine Proportion - Raw")
hist(moose_simulated$Pine.proportion,
     main = "", 
     xlab = "Pine Proportion - Simulated")

##==

## H. Time Since Wolf Establishment 
table(moose_raw$Time.since.establishment)
# Defining the raw data ratio of the different categories
prob_0 <- sum(moose_raw$Time.since.establishment == 0)/nrow(moose_raw)
prob_1 <- sum(moose_raw$Time.since.establishment == 1)/nrow(moose_raw)
prob_2 <- sum(moose_raw$Time.since.establishment == 2)/nrow(moose_raw)
prob_3 <- sum(moose_raw$Time.since.establishment == 3)/nrow(moose_raw)
prob_4 <- sum(moose_raw$Time.since.establishment == 4)/nrow(moose_raw)
prob_5 <- sum(moose_raw$Time.since.establishment == 5)/nrow(moose_raw)
prob_6 <- sum(moose_raw$Time.since.establishment == 6)/nrow(moose_raw)
prob_7 <- sum(moose_raw$Time.since.establishment == 7)/nrow(moose_raw)
prob_8 <- sum(moose_raw$Time.since.establishment == 8)/nrow(moose_raw)
prob_9 <- sum(moose_raw$Time.since.establishment == 9)/nrow(moose_raw)
prob_10 <- sum(moose_raw$Time.since.establishment == 10)/nrow(moose_raw)
prob_11 <- sum(moose_raw$Time.since.establishment == 11)/nrow(moose_raw)
prob_12 <- sum(moose_raw$Time.since.establishment == 12)/nrow(moose_raw)
prob_13 <- sum(moose_raw$Time.since.establishment == 13)/nrow(moose_raw)
prob_14 <- sum(moose_raw$Time.since.establishment == 14)/nrow(moose_raw)
prob_15 <- sum(moose_raw$Time.since.establishment == 15)/nrow(moose_raw)
prob_16 <- sum(moose_raw$Time.since.establishment == 16)/nrow(moose_raw)
prob_17 <- sum(moose_raw$Time.since.establishment == 17)/nrow(moose_raw)
prob_18 <- sum(moose_raw$Time.since.establishment == 18)/nrow(moose_raw)
prob_19 <- sum(moose_raw$Time.since.establishment == 19)/nrow(moose_raw)
prob_20 <- sum(moose_raw$Time.since.establishment == 20)/nrow(moose_raw)
prob_21 <- sum(moose_raw$Time.since.establishment == 21)/nrow(moose_raw)
prob_22 <- sum(moose_raw$Time.since.establishment == 22)/nrow(moose_raw)
prob_23 <- sum(moose_raw$Time.since.establishment == 23)/nrow(moose_raw)
prob_24 <- sum(moose_raw$Time.since.establishment == 24)/nrow(moose_raw)
prob_26 <- sum(moose_raw$Time.since.establishment == 26)/nrow(moose_raw)
prob_27 <- sum(moose_raw$Time.since.establishment == 27)/nrow(moose_raw) 

# Incorporating the variables
moose_simulated$Time.since.establishment <- sample(c(0:24,26,27), size = nrow(moose_raw), replace = TRUE, 
                                                   prob = c(prob_0, prob_1, prob_2, prob_3,
                                                            prob_4, prob_5, prob_6, prob_7,
                                                            prob_8, prob_9, prob_10, prob_11,
                                                            prob_12, prob_13, prob_14, prob_15,
                                                            prob_16, prob_17, prob_18, prob_19,
                                                            prob_20, prob_21, prob_22, prob_23, 
                                                            prob_24, prob_26, prob_27))

# Compare simulated data to the raw data
summary(moose_raw$Time.since.establishment)
summary(moose_simulated$Time.since.establishment)

hist(moose_raw$Time.since.establishment,
     xlab = "Time Since Establishment - Raw",
     main = "")
hist(moose_simulated$Time.since.establishment,
     xlab = "Time Since Establishment - Simulated",
     main = "")

##==

# I. Taxar (random effect variable - uniform distribution of dates) 
# Run the simulation
moose_simulated$Taxar <- purrr::rdunif(moose_size, min(moose_raw$Taxar), max(moose_raw$Taxar))

# Compare simulated data to raw data
hist(moose_raw$Taxar,
     main = "",
     xlab = "Taxar - Raw")
hist(moose_simulated$Taxar,
     main = "",
     xlab = "Taxar - Simulated")
table(moose_raw$Taxar)
table(moose_simulated$Taxar)

##==

## J. Pellet counts (Response variable - negative binomial distribution)
summary(moose_raw$Pellet.counts)
moose_simulated$Pellet.counts <- rnbinom(n = nrow(moose_simulated), mu=0.1675, size=0.1) 

# Compare simulated data to raw data
summary(moose_raw$Pellet.counts)
summary(moose_simulated$Pellet.counts)

hist(moose_raw$Pellet.counts,
     main = "",
     xlab = "Pellet Counts - Raw")
hist(moose_simulated$Pellet.counts,
     main = "",
     xlab = "Pellet Counts, Simulated")

##==

## Done Simulations

## Save new simulation data as csv:=================================================
write.csv(moose_simulated,"Simulated_Data/moose_abundance_simulated_04Oct21.csv")


## End Script
