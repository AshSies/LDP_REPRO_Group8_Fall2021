#=================================================================================================#
#============================ LDP: Productivity and Reproducibility ==============================#
#============================== Group Assignment: Pre-Registration ===============================#
#=============== By : Ashton Sies, Emma Nikkel, Jenna Scherger, and Sabrina St-Pierre ============#
#================================== Date : 1st of October 2021 ===================================#
#=================================================================================================#

# Read in packages ===============================================================================#
library(readxl) # To import excel documents.
library(tidyverse) # for wrangling data - using primarily dplyr, tidyr, purrr, maybe ggplot
library(emdbook) # To simulate zero-inflated negative binomial distribution
library(parallel) # for parallel processing when running model selection 
library(glmmTMB) # for running model
library(MuMIn) # for dredging
library(sjPlot) # for marginal effects plots
library(ggpubr) # for arranging multiple plots into 1 figure

# Importing the raw data =========================================================================#
# The data are available on a Dryad repository
# https://datadryad.org/stash/share/svRCeH_hvPZVH2UjmzWQOvErl-B6deccaKMXgGEeHtM

# Set working directory:===========================================================
#setwd("C:/Users/sabri/OneDrive/Documents/BackUp/Living Data Project/Productivity/Assignements/Reproductibility/doi_10.5061_dryad.5hqbzkh37__v2")
setwd("/Users/jennascherger/Desktop/MSc/Courses/NSERC_CREATE/01_Productivity_Reproducibility/Assignments/02_Reproducible_Research/LDP_REPRO_Group8_Fall2021")

# Read in data:===================================================================
moose_raw <- read.csv("Raw_Data/Moose_presence_abundance.csv")
names(moose_raw)

# A. Initial data exploration:===========================================================
## 1. Variable distribution
par(mfrow = c(2,4))
hist(moose_raw$Taxar,
    main = "Taxar")
hist(moose_raw$Forest_class,
     main = "Forest Class") # 4 categorical turned continuous (1, 2, 3, 4)
hist(moose_raw$Wolf.territory,
     main = "Wolf Territory") # binary (0 = no; 1 = yes)
hist(moose_raw$Moose.presence,
     main = "Moose Presence") # binary (0 = no; 1 = yes) -> lots of 0's
hist(moose_raw$Small.roads,
     main = "Small Roads") 
hist(moose_raw$Big.roads,
     main = "Big Roads")
hist(moose_raw$Pine_proportion,
     main = "Pine Proportion")
hist(moose_raw$Time.since.establishment,
     main = "Time Since Establishment")

## 2. Effects of predictors on response:
boxplot(Pellet.counts ~ Forest_class,
     data = moose_raw,
     xlab = "Forest Class",
     ylab = "Pellet Counts")

boxplot(Pellet.counts ~ Wolf.territory,
        data = moose_raw,
        xlab = "Wolf Territory",
        ylab = "Pellet Counts")

boxplot(Pellet.counts ~ Moose.presence,
        data = moose_raw,
        xlab = "Moose Presence",
        ylab = "Pellet Counts")

plot(Pellet.counts ~ Small.roads,
     data = moose_raw,
     xlab = "Distance to Small Roads [m]",
     ylab = "Pellet Counts")

plot(Pellet.counts ~ Big.roads,
     data = moose_raw,
     xlab = "Distance to Large Roads [m]",
     ylab = "Pellet Counts")

plot(Pellet.counts ~ Pine_proportion,
     data = moose_raw,
     xlab = "Pine Proportion",
     ylab = "Pellet Counts")

plot(Pellet.counts ~ Time.since.establishment,
     data = moose_raw,
     xlab = "Time Since Wolf Pack Establishment",
     ylab = "Pellet Counts")

##===================================================================================
##===================================================================================

# B. Simulating Data:==================================================================

# Create empty data frame
moose_simulated <- data.frame(matrix(0, ncol = 0, nrow = nrow(moose_raw)))
moose_size <- nrow(moose_simulated)

# 1. TAXAR 
par(mfrow = c(1,2))
## Visualize raw distribution:
hist(moose_raw$Taxar, main = "Raw data: Taxar") # shows a uniform distribution of dates

## Set the seed
set.seed(5)

# Simulate the data:
# The package purrr (available in tidyverse) is needed to simulate an uniform distribution of dates.
moose_simulated$Taxar <- purrr::rdunif(n = nrow(moose_raw), min(moose_raw$Taxar), max(moose_raw$Taxar))

# Compare the simulated data distribution
hist(moose_simulated$Taxar, main = "Simulated data: Taxar")

summary(moose_raw$Taxar) # Median = 2010; Mean = 2010 
summary(moose_simulated$Taxar) # Median = 2009; Mean = 2009

##==

# 2. RASE PRESENCE 
## Visualize raw distribution
hist(moose_raw$RASE.presence, main = "Raw data: RASE presence")
# Binary (0 = absence; 1 = presence)

## Set the seed
set.seed(5)

## Simulate the data
# Defining the raw data ratio of the presence
prob_rase <- sum(moose_raw$RASE.presence == 1) / nrow(moose_raw)
# Incorporating the variables
moose_simulated$RASE.presence <- rbinom(n = nrow(moose_raw), 1, prob_rase)

## Compare the simulated data distribution
hist(moose_simulated$RASE.presence, main = "Simulated data: RASE presence")

summary(moose_raw$RASE.presence) # Median = 1.0000; Mean = 0.5219
summary(moose_simulated$RASE.presence) # Meadian = 1.0000; Mean = 0.5213

##==

# 3. FOREST CLASS 
## Visualize raw distribution
hist(moose_raw$Forest_class, main = "Raw data: Forest Class") 
# Categorical data

## Set the seed
set.seed(5)

## Simulate the data
# Defining the raw data ratio of the different categories
prob_forest1 <- sum(moose_raw$Forest_class == 1)/nrow(moose_raw)
prob_forest2 <- sum(moose_raw$Forest_class == 2)/nrow(moose_raw)
prob_forest3 <- sum(moose_raw$Forest_class == 3)/nrow(moose_raw)
prob_forest4 <- sum(moose_raw$Forest_class == 4)/nrow(moose_raw)

# Incorporating the variables
moose_simulated$Forest.class <- sample(c(1:4), size = nrow(moose_raw), replace = TRUE, 
                                       prob = c(prob_forest1, prob_forest2, prob_forest3, prob_forest4))

## Compare the simulated data distribution
hist(moose_simulated$Forest.class, main = "Simulated data: Forest class")

summary(moose_raw$Forest_class) # Median = 3.000; Mean = 2.993
summary(moose_simulated$Forest.class) # Median = 3.000; Mean = 2.981

##==

# 4. MOOSE PRESENCE 
## Visualize raw data distribution
hist(moose_raw$Moose.presence, main = "Raw data: Moose Presence") 
# Binary (0 = absence; 1 = presence)

## Set the seed
set.seed(5)

## Simulate the data
# Defining the raw data ration of the presence
prob_moose <- sum(moose_raw$Moose.presence == 1) / nrow(moose_simulated)
# Incorporating the variables
moose_simulated$Moose.presence <- rbinom(n = nrow(moose_raw), 1, prob_moose)

## Compare the simulated data distribution
hist(moose_simulated$Moose.presence, main = "Simulated data: Moose presence")

summary(moose_raw$Moose.presence) # Mean = 0.1184
summary(moose_simulated$Moose.presence) # Mean = 0.1197

##==

# 5. WOLF TERRITORY 
## Visualize raw data distribution
hist(moose_raw$Wolf.territory, main = "Raw data: Wolf Territory") 
# Binary (0 = absence; 1 = presence)

## Set the seed
set.seed(5)

## Simulate the data
# Defining the raw data ratio of the presence
prob_wolf <- sum(moose_raw$Wolf.territory == 1) / nrow(moose_raw)
# Incorporating the variables
moose_simulated$Wolf.territory <- rbinom(n = nrow(moose_raw), 1, prob_wolf)

## Compare the simulated data distribution
hist(moose_simulated$Wolf.territory, main = "Simulated data: Wolf territory")

summary(moose_raw$Wolf.territory) # Mean = 0.3789
summary(moose_simulated$Wolf.territory) # Mean = 0.3836

##==

# 6. PINE PROPORTION ===============================================================#
## Visualize the raw data distribution
hist(moose_raw$Pine_proportion, main = "Raw data: Pine Proportion")
# Zero-inflated negative binomial model

## Set the seed
set.seed(5)

## Simulate the data
# Determination the pre-required variables for the function
# Adjusting the data
# ADD DETAIL WHY (SABRINA)
moose_simulated$Pine_raw_100 <- (moose_raw$Pine_proportion)*100

# zprob : probability of structural zeros
pine_zprob <- sum(moose_simulated$Pine_raw_100 == 0)/nrow(moose_raw)

# mu : mean of the pine proportion
pine_mu <- mean(moose_simulated$Pine_raw_100)

# Variance : measuring the dispersion
pine_var <- var(moose_simulated$Pine_raw_100)

# Size : the number of scores used to compute a mean (SABRINA)
# We can find the size knowing that variance = (mu + mu^2)/size
# Then size = (mu + mu^2)/variance
pine_size <- (pine_mu + (pine_mu^2))/pine_var

# Simulate the data with the variables established
#moose_simulated$Pine.proportion <- (emdbook::rzinbinom(n = nrow(moose_raw), mu = pine_mu, size = pine_size, zprob = pine_zprob))/100
moose_simulated$Pine.proportion <- rnbinom(n = nrow(moose_raw), mu = pine_mu, size = pine_size)/100
moose_simulated$Pine.proportion <- ifelse(moose_simulated$Pine.proportion >= 1, 0.96, moose_simulated$Pine.proportion)

## Compare the simulated data distribution
hist(moose_simulated$Pine.proportion, main = "Simulated data: Pine proportion")

summary(moose_raw$Pine_proportion) # Median = 0.006496; Mean = 0.039826
summary(moose_simulated$Pine.proportion) # Median = 0.00000; Mean = 0.02409

## test:
#moose_simulated$Pine.proportion <- moose_raw$Pine_proportion

##==

# 7. PELLET COUNTS ===============================================================#
## Visualize the raw data distribution
hist(moose_raw$Pellet.counts, main = "Raw data: Pellet counts")
# Zero-inflated negative binomial model

## Set the seed
set.seed(5)

## Simulate the data
# ADD DETAIL WHY (SABRINA)
moose_simulated$Pellet_raw_10 <- (moose_raw$Pellet.counts)*10

# zprob : probability of structural zeros
pellet_zprob <- sum(moose_simulated$Pellet_raw_10 == 0)/nrow(moose_raw)

# mu : mean of the pine proportion
pellet_mu <- mean(moose_simulated$Pellet_raw_10) #2.675

# Variance : measuring the dispersion 
pellet_var <- var(moose_simulated$Pellet_raw_10) # 31.708

# Size : the number of scores used to compute a mean (SABRINA)
# We can find the size knowing that variance = (mu + mu^2)/size
# Then size = (mu + mu^2)/variance
pellet_size <- (pellet_mu + (pellet_mu^2))/pellet_var #0.1413

# Simulating the data with the variables established
moose_simulated$Pellet.counts <- (emdbook::rzinbinom(n = nrow(moose_raw), mu = pellet_mu, size = pellet_size, zprob = pellet_zprob))/10

# Make whole numbers
moose_simulated$Pellet.counts <- round(moose_simulated$Pellet.counts)

# 4. Comparing the simulated data distribution
hist(moose_simulated$Pellet.counts, main = "Simulated data: Pellet counts")

summary(moose_raw$Pellet.counts) # Median = 0.0000; Mean = 0.1675
summary(moose_simulated$Pellet.counts) # Median = 0.00000; Mean = 0.02233


moose_simulated$Pellet.counts <- moose_raw$Pellet.counts

##==

# 8. TIME SINCE ESTABLISHMENT ==============================================================#
## Visualize the raw data distribution
hist(moose_raw$Time.since.establishment, main = "Raw data: Time since establishment")
# Negative binomial model

## Set the seed
set.seed(5)

## Simulate the data

# mu : mean of the pine proportion
time_mu <- mean(moose_raw$Time.since.establishment)

# Variance : measuring the dispersion
time_var <- var(moose_raw$Time.since.establishment)

# Size : the number of scores used to compute a mean (SABRINA)
# We can find the size knowing that variance = (mu + mu^2)/size
# Then size = (mu + mu^2)/variance
time_size <- (time_mu + (time_mu^2))/time_var

# Simulating the data with the variables established
moose_simulated$Time.since.establishment <- rnbinom(n = nrow(moose_raw), mu = time_mu, size = time_size)
moose_simulated$Time.since.establishment <- ifelse(moose_simulated$Time.since.establishment > 27, 27, moose_simulated$Time.since.establishment)

# 4. Comparing the simulated data distribution
hist(moose_simulated$Time.since.establishment, main = "Simulated data: Time since establishment")

summary(moose_raw$Time.since.establishment) # Median = 0.000; Mean = 2.324
summary(moose_simulated$Time.since.establishment) # Median = 1.0000; Mean = 2.345

#moose_simulated$Time.since.establishment <- moose_raw$Time.since.establishment

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









##==

# 9. SMALL ROADS 
## Visualize the raw data distribution
hist(moose_raw$Small.roads, main = "Raw data: Small Roads") 
# Negative binomial model

## Set the seed
set.seed(5)

## Simulate the data

# mu : mean of the pine proportion
sroad_mu <- mean(moose_raw$Small.roads) # 254.1479

# Variance : measuring the dispersion
sroad_var <- var(moose_raw$Small.roads) # 52296.74

# Size : the number of scores used to compute a mean (SABRINA)
# We can find the size knowing that variance = (mu + mu^2)/size
# Then size = (mu + mu^2)/variance
sroad_size <- (sroad_mu + (sroad_mu^2))/sroad_var # 1.23995

# Simulating the data with the variables established
moose_simulated$Small.roads <- rnbinom(n = nrow(moose_raw), mu = sroad_mu, size = sroad_size)

# 4. Comparing the simulated data distribution
hist(moose_simulated$Small.roads, main = "Simulated data: Small roads")

summary(moose_raw$Small.roads) # Median = 198.9641; Mean = 254.1479
summary(moose_simulated$Small.roads) # Median = 187.0; Mean = 251.4
##test:
#moose_simulated$Small.roads <- moose_raw$Small.roads

##==

# 10. BIG ROADS ===============================================================#
## Visualize the raw data distribution
hist(moose_raw$Big.roads, main = "Raw data: Big Roads")
# Negative binomial model

## Set the seed
set.seed(5)

## Simulate the data

# mu : mean of the pine proportion
broad_mu <- mean(moose_raw$Big.roads) # 2288.179

# Variance : measuring the dispersion
broad_var <- var(moose_raw$Big.roads) # 6075845

# Size : the number of scores used to compute a mean (SABRINA)
# We can find the size knowing that variance = (mu + mu^2)/size
# Then size = (mu + mu^2)/variance
broad_size <- (broad_mu + (broad_mu^2))/broad_var # 0.8621109

# Simulating the data with the variables established
moose_simulated$Big.roads <- rnbinom(n = nrow(moose_raw), mu = broad_mu, size = broad_size)

# 4. Comparing the simulated data distribution
hist(moose_simulated$Big.roads, main = "Simulated data: Big roads")

summary(moose_raw$Big.roads) # Median = 1514.442; Mean = 2288.179
summary(moose_simulated$Big.roads) # Median = 1456; Mean = 2297

#moose_simulated$Big.roads <- moose_raw$Big.roads

## Done simulations

##==================================================================================

# C. Analyses:======================================================================

## First adjust column classes for the models
moose_simulated$Forest.class <- factor(moose_simulated$Forest.class, levels = c(1:4), labels = c("Clearcut", "Young", "Thinned", "Mature"))
moose_simulated$Wolf.territory <- factor(moose_simulated$Wolf.territory, levels = c(0:1), labels = c("Outside","Inside"))
moose_simulated$Moose.presence<- factor(moose_simulated$Moose.presence, levels = c(0:1), labels = c("Absent", "Present"))
moose_simulated$Small <- moose_simulated$Small.roads/1000
moose_simulated$Big <- moose_simulated$Big.roads/1000
moose_simulated$RASE.presence<- factor(moose_simulated$RASE.presence, levels = c(0:1), labels = c("Absent", "Present"))
head(moose_simulated)

# Set taxar (grouping variable) to a factor:
moose_simulated <-
  moose_simulated %>%
  mutate(Taxar = as.factor(Taxar))

head(moose_simulated)


## Running the model
# Formula for zero inflation
zifo <- ~ Pine.proportion + Big + Small

# start model
m_moose_ab <- glmmTMB(Pellet.counts ~ Wolf.territory + Forest.class +
                        Wolf.territory:Forest.class + Pine.proportion + Big +
                        Small + RASE.presence +  (1|Taxar),
                      data = moose_simulated,
                      ziformula = zifo,
                      family = nbinom2)

summary(m_moose_ab)

# --- dredging ---
# how many models would that make with dredge?
moose_ab_m_lst <- dredge(m_moose_ab, evaluate = FALSE)
length(moose_ab_m_lst)


#moose_ab_m_all <- dredge(m_moose_ab)
#head(moose_ab_m_all)

# fit all models outside of "model.selection" to avoid having to refit
# note: function mclapply() is not available on windows
# note: adjust mc.cores to run on another (unix) computer
moose_ab_m_all <- parallel::mclapply(moose_ab_m_lst, eval, mc.cores = 16) #started at 3:19
head(moose_ab_m_all)

# --- model selection ---

# drop models which did not converge
# and models with virtually not weight
moose_ab_modsel_tbl <- bbmle::AICctab(moose_ab_m_all,
                                      weights = TRUE,
                                      base = TRUE,
                                      logLik = TRUE,
                                      nobs = TRUE,
                                      mnames = names(moose_ab_m_all)) %>%
  as.data.frame() %>%
  rownames_to_column(var = "ModelName") %>%
  dplyr::filter(!is.na(logLik) & weight > 0.01)
head(moose_ab_modsel_tbl)
# List of models to keep for model averaging
moose_ab_m_tokeep <-
  moose_ab_m_all[names(moose_ab_m_all) %in% moose_ab_modsel_tbl$ModelName]
head(moose_ab_m_tokeep)

# --- model-averaging ---

moose_ab_mavg <- MuMIn::model.avg(moose_ab_m_tokeep)
moose_ab_mavg2 <- MuMIn::model.avg(get.models(moose_ab_m_tokeep, subset = df <5))

moose_ab_mavg_summary <- summary(moose_ab_mavg)

moose_ab_mavg_ci <- confint(moose_ab_mavg)
moose_ab_mavg_summary
summary(moose_ab_mavg)

# --- best model ---

name_bm <- first(moose_ab_modsel_tbl$ModelName)

moose_ab_best_model <- moose_ab_m_tokeep[[eval(name_bm)]]
summary(moose_ab_best_model)

## Best Model:
  ## Conditional = 
      # Pellet.counts ~ Big + Forest_class + Pine.proportion + Small + Wolf.territory + (1 | Taxar)
  ## Zero-inflation = Big + Pine.proportion + Small
# --- plots for best model ---

#topmodel <- glmmTMB(Pellet.counts ~ Wolf.territory + Forest_class + Pine.proportion + Big +
 #                       Small + (1|Taxar),
  #                    data = moose_simulated,
   #                   ziformula = zifo,
    #                  family = nbinom2)
#summary(topmodel)

moose_ab_p1 <- sjPlot::plot_model(moose_ab_best_model, type = "est")

moose_ab_p2 <- sjPlot::plot_model(moose_ab_best_model, type = "pred", grid = TRUE)
moose_ab_p2


## Predictions from model: =======================================================
## Marginal Effects Plots

## Set theme for sjPlots:
sjPlot::set_theme(
  geom.outline.color = "antiquewhite4",
  geom.outline.size = 1,
  geom.label.size = 2,
  geom.label.color = "grey50",
  title.color = "black",
  title.size = 1.25,
  axis.textcolor = "black",
  #axis.title.size = 2.5,
  axis.title.color = "black",
  #axis.textsize = 2,
  #legend.title.size = 2,
  base = theme_bw(
    base_family = "Arial Narrow",
    base_size = 10.5)
) 



# Main Roads:
Plot_abu_mainroads<- sjPlot::plot_model(moose_ab_best_model, type= "pred", terms = ("Big"),
                            axis.title = c("Distance main roads [km]", "Moose pellet counts"), 
                            title = "") 

Plot_abu_mainroads + sjPlot::font_size(axis_title.x = 20, axis_title.y = 20, labels.x = 20, labels.y = 20, title = 15)

# forest roads:
Plot_abu_forestroads <- sjPlot::plot_model(moose_ab_best_model, type= "pred", terms = ("Small"),
                                    axis.title = c("Distance forest roads [km]", "Moose pellet counts"), 
                                    title = "")
Plot_abu_forestroads + sjPlot::font_size(axis_title.x = 20, axis_title.y = 20, labels.x = 20, labels.y = 20, title = 15)

# Wolf territory:
Plot_abu_wolfterritory <- sjPlot::plot_model(moose_ab_best_model, type= "pred", terms = ("Wolf.territory"),
                                           axis.title = c("Wolf Territory", "Moose pellet counts"), 
                                           title = "")
Plot_abu_wolfterritory + sjPlot::font_size(axis_title.x = 20, axis_title.y = 20, labels.x = 20, labels.y = 20, title = 15)


# Wolf territory:
Plot_abu_foreststage <- sjPlot::plot_model(moose_ab_best_model, type= "pred", terms = ("Forest_class"),
                                             axis.title = c("Forest Stage", "Moose pellet counts"), 
                                             title = "")
Plot_abu_foreststage + sjPlot::font_size(axis_title.x = 20, axis_title.y = 20, labels.x = 20, labels.y = 20, title = 15)

summary(moose_ab_best_model)


# Pine proportion:
Plot_abu_pineproportion <- sjPlot::plot_model(moose_ab_best_model, type= "pred", terms = ("Pine.proportion"),
                                           axis.title = c("Pine proportion", "Moose pellet counts"), 
                                           title = "",
                                           base_family = "Arial Narrow")
Plot_abu_pineproportion + sjPlot::font_size(axis_title.x = 20, axis_title.y = 20, labels.x = 20, labels.y = 20, title = 15)


# Time since establishment
Plot_abu_tse <- sjPlot::plot_model(moose_ab_best_model, type= "pred", terms = ("Time.since.establishment"),
                                              axis.title = c("Time Since Establishment", "Moose pellet counts"), 
                                              title = "",
                                              base_family = "Arial Narrow")
Plot_abu_tse + sjPlot::font_size(axis_title.x = 20, axis_title.y = 20, labels.x = 20, labels.y = 20, title = 15)



## Plot Grid:
sjPlot::plot_grid(list(Plot_abu_mainroads, Plot_abu_forestroads, Plot_abu_wolfterritory, Plot_abu_tse,
               Plot_abu_foreststage, Plot_abu_pineproportion), tags = TRUE) + sjPlot::font_size(axis_title.x = 20, axis_title.y = 20, labels.x = 20, labels.y = 20, title = 15)

ggsave("mooseabundance_forestplot_29Sept21.png",
       height = 4, width = 10)

## =================================================================================


## Making marginal effects plots with ggpredict and ggplot:


## 1. Distance to coast:
## Make dataframe from predictions
pred_pineproportion <- data.frame(ggpredict(moose_ab_best_model, terms = c("Pine.proportion[all]"))) %>%
  rename(Pine.proportion = x)
head(pred_pineproportion)

## Plot Marginal Effects Plots in ggplot 
## This is nicer because I can set the axis limits
pineprop <- ggplot(data = pred_pineproportion) +
  geom_line(aes(x = Pine.proportion, y = predicted), size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, x = Pine.proportion), alpha = 0.1) +
  scale_fill_grey() +
  theme(panel.grid.major.y = element_line(colour="lightgrey", linetype = "dotted"),
        panel.grid.minor.y = element_line(colour="lightgrey", linetype = "dotted"),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.6)) +
  theme(axis.text.x = element_text(color = "black", size = 24, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 24, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "black", size = 28, angle = 0, hjust = .5, vjust = -0.5, face = "bold"),
        axis.title.y = element_text(color = "black", size = 28, angle = 90, hjust = .5, vjust = 1, face = "bold"),
        legend.text = element_text(color = "black", size = 24, face = "plain"),
        legend.title = element_text(color = "black", size = 28, face = "bold"))+ 
  theme(text = element_text(family = "Arial Narrow")) +
  labs(
    x = "Pine Proportion",
    y = "Moose Abundance"#,
    #title = paste(
    #"Example group_by() with summarise()"
  )


#=================================================================================================#
#==================================== END OF SCRIPT ==============================================#
#=================================================================================================#




moose_raw <- read.csv("Raw_Data/Moose_presence_abundance.csv")

#To look at the distribution of raw data
dev.off()
par(mfrow = c(2,4))
hist(moose_raw$Taxar,
     main = "",
     xlab = "Taxar - Year (Raw Data)")
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
     xlab = "Small Roads (Raw Data)") 
hist(moose_raw$Big.roads,
     main = "",
     xlab = "Big Roads (Raw Data)")
hist(moose_raw$Pine_proportion,
     main = "",
     xlab = "Pine Proportion (Raw Data)")
hist(moose_raw$Time.since.establishment,
     main = "",
     xlab = "Time Since Wolf Pack Establishment (Raw Data)")

# I looked through the script for abundance and I think we only need to simulate those. Correct me if I'm wrong!

# Creating empty data frame
moose_simulated <- data.frame(matrix(0, ncol = 0, nrow = nrow(moose_raw)))
moose_size <- nrow(moose_simulated)

#Set seed
set.seed(1234)

# Simulating Forest_class (categorical data)
size_forest <- as.integer(length(moose_raw$Forest_class))

prob_1 <- sum(moose_raw$Forest_class == 1)/moose_size
prob_2 <- sum(moose_raw$Forest_class == 2)/moose_size
prob_3 <- sum(moose_raw$Forest_class == 3)/moose_size
prob_4 <- sum(moose_raw$Forest_class == 4)/moose_size

moose_simulated$Forest_class <- sample(c(1:4), size = size_forest, replace = TRUE, prob = c(prob_1, prob_2, prob_3, prob_4))

# Checking simulated data
par(mfrow = c(1,2))
hist(moose_raw$Forest_class,
     main = "",
     xlab = "Forest Class - Raw")
hist(moose_simulated$Forest_class,
     main = "",
     xlab = "Forest Class - Simulated")
table(moose_raw$Forest_class) # 1 = 499; 2 = 2386; 3 = 4301; 4 = 3309
table(moose_simulated$Forest_class) # 1 = 477; 2 = 2387; 3 = 4255; 4 = 3376

# Simulating wolf territory (binomial distribution)
prob_wolf <- sum(moose_raw$Wolf.territory == 1) / moose_size

moose_simulated$Wolf.territory <- rbinom(moose_size, 1, prob_wolf)

# Checking simulated data
hist(moose_raw$Wolf.territory,
     main = "",
     xlab = "Wolf Territory - Raw")
hist(moose_simulated$Wolf.territory,
     main = "",
     xlab = "Wolf Territory - Simulated")
table(moose_raw$Wolf.territory) # 0 = 6518; 1 = 3977
table(moose_simulated$Wolf.territory) # 0 = 6452; 1 = 4043

# Simulating moose presence (binomial distribution)
prob_moose <- sum(moose_raw$Moose.presence == 1) / moose_size

moose_simulated$Moose.presence <- rbinom(moose_size, 1, prob_moose)

# Checking simulated data
hist(moose_raw$Moose.presence,
     main = "",
     xlab = "Moose Presence - Raw")
hist(moose_simulated$Moose.presence,
     main = "",
     xlab = "Moose Presence - Simulated")
table(moose_raw$Moose.presence) # 0 = 9252; 1 = 1242
table(moose_simulated$Moose.presence) # 0 = 9239; 1 = 1256

# Simulating RASE presence (binomial distribution)
prob_rase <- sum(moose_raw$RASE.presence == 1) / moose_size

moose_simulated$Rase.presence <- rbinom(moose_raw$RASE.presence, 1, prob_rase)

# Checking simulated data
hist(moose_raw$RASE.presence,
     main = "",
     xlab = "RASE Presence - Raw")
hist(moose_simulated$Rase.presence,
     main = "",
     xlab = "RASE Presence - Simulated")
table(moose_raw$RASE.presence) # 0 = 5018; 1 = 5477
table(moose_simulated$Rase.presence) # 0 - 4968; 1 = 5527

# Simulating Taxar (uniform distribution of dates)
# We need the package purrr (in tidyverse)
library(purrr)
moose_simulated$Taxar <- rdunif(moose_size, min(moose_raw$Taxar), max(moose_raw$Taxar))

# Checking simulated data:
hist(moose_raw$Taxar,
     main = "",
     xlab = "Taxar - Raw")
hist(moose_simulated$Taxar,
     main = "",
     xlab = "Taxar - Simulated")
table(moose_raw$Taxar)
table(moose_simulated$Taxar)


## Small roads:
hist(moose_raw$Small.roads)
mean(moose_raw$Small.roads)
sd(moose_raw$Small.roads)
moose_simulated$Small.roads <- rnbinom(n = nrow(moose_simulated), mu=254.1479, size=1.25) 

# compare simulated to untouched
hist(moose_raw$Small.roads,
     main = "",
     xlab = "Small Roads - Raw")
hist(moose_simulated$Small.roads,
     main = "", 
     xlab = "Small Roads - Simulated")

summary(moose_raw$Small.roads)
summary(moose_simulated$Small.roads)


## Big roads:
mean(moose_raw$Big.roads)
sd(moose_raw$Big.roads)
moose_simulated$Big.roads <- rnbinom(n = nrow(moose_simulated), mu=2288.179, size=1.25) 

# compare simulated to untouched
hist(moose_raw$Big.roads,
     main = "",
     xlab = "Big Roads - Raw")
hist(moose_simulated$Big.roads,
     main = "",
     xlab = "Big Roads - Simulated")

summary(moose_raw$Big.roads)
summary(moose_simulated$Big.roads)


## Pellet counts
summary(moose_raw$Pellet.counts)
moose_simulated$Pellet.counts <- rnbinom(n = nrow(moose_simulated), mu=0.1675, size=0.1) 

# compare simulated to untouched
summary(moose_raw$Pellet.counts)
summary(moose_simulated$Pellet.counts)

hist(moose_raw$Pellet.counts,
     main = "",
     xlab = "Pellet Counts - Raw")
hist(moose_simulated$Pellet.counts,
     main = "",
     xlab = "Pellet Counts, Simulated")


## Pine proportion
moose_simulated$Pine.proportion.100 <- moose_raw$Pine_proportion*100
range(moose_simulated$Pine.proportion.100)
mean(moose_simulated$Pine.proportion.100)

moose_simulated$Pine.proportion <- rnbinom(n = nrow(moose_simulated), mu=3.982581, size=0.52)/100


summary(moose_raw$Pine_proportion)
summary(moose_simulated$Pine.proportion)

hist(moose_raw$Pine_proportion,
     main = "",
     xlab = "Pine Proportion - Raw")
hist(moose_simulated$Pine.proportion,
     main = "", 
     xlab = "Pine Proportion - Simulated")


## Time since establishment:
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

summary(moose_raw$Time.since.establishment)
summary(moose_simulated$Time.since.establishment)

hist(moose_raw$Time.since.establishment,
     xlab = "Time Since Establishment - Raw",
     main = "")
hist(moose_simulated$Time.since.establishment,
     xlab = "Time Since Establishment - Simulated",
     main = "")

## Done simulations


##==================================================================================

## Exploratory Analyses of Simulated Data
# First, change class of the different predictors:
moose_simulated$Forest_class <- factor(moose_simulated$Forest_class, levels = c(1:4), labels = c("Clearcut", "Young", "Thinned", "Mature"))
moose_simulated$Wolf.territory <- factor(moose_simulated$Wolf.territory, levels = c(0:1), labels = c("Outside","Inside"))
moose_simulated$Moose.presence<- factor(moose_simulated$Moose.presence, levels = c(0:1), labels = c("Absent", "Present"))
moose_simulated$Small <- moose_simulated$Small.roads/1000
moose_simulated$Big <- moose_simulated$Big.roads/1000
moose_simulated$Rase.presence<- factor(moose_simulated$Rase.presence, levels = c(0:1), labels = c("Absent", "Present"))
head(moose_simulated)

# Set taxar (grouping variable) to a factor:
moose_simulated <-
  moose_simulated %>%
  mutate(Taxar = as.factor(Taxar))

head(moose_simulated)

## Visualize effects of predictors on response variable:
dev.off()
par(mfrow = c(2,2))

plot(Pellet.counts ~ Time.since.establishment,
     data = moose_simulated,
     xlab = "Time Since Wolf Pack Establishment",
     ylab = "Pellet Counts")

boxplot(Pellet.counts ~ Wolf.territory,
        data = moose_simulated,
        xlab = "Wolf Territory",
        ylab = "Pellet Counts")

boxplot(Pellet.counts ~ Forest_class,
        data = moose_simulated,
        xlab = "Forest Class",
        ylab = "Pellet Counts")

boxplot(Pellet.counts ~ Moose.presence,
        data = moose_simulated,
        xlab = "Moose Presence",
        ylab = "Pellet Counts")

plot(Pellet.counts ~ Small,
     data = moose_simulated,
     xlab = "Distance to Small Roads [km]",
     ylab = "Pellet Counts")

plot(Pellet.counts ~ Big,
     data = moose_simulated,
     xlab = "Distance to Large Roads [km]",
     ylab = "Pellet Counts")

plot(Pellet.counts ~ Pine.proportion,
     data = moose_simulated,
     xlab = "Pine Proportion",
     ylab = "Pellet Counts")


##==================================================================================


## Running Model:
# Formula for zero inflation:
zifo <- ~ Pine.proportion + Big + Small

# 1. Run model:
m_moose_ab <- glmmTMB(Pellet.counts ~ Wolf.territory + Forest_class +
                        Wolf.territory:Forest_class + Pine.proportion + Big +
                        Small + Rase.presence + Time.since.establishment + (1|Taxar),
                      data = moose_simulated,
                      ziformula = zifo,
                      family = nbinom2)
summary(m_moose_ab)


# 2. AICc Based Model Selection 
# First checking how many models would that make with dredge?
moose_ab_m_lst <- MuMIn::dredge(m_moose_ab, evaluate = FALSE)
length(moose_ab_m_lst) #1280 models


## Please note: I have hashed out the next couple chunks of code because 
## running model selection takes ~ 2 hours using parallel processing,
## I have written in topmodel() at line ____ which gives the best model found
## after completing the model selection and model averaging.

# Fit all models outside of "model.selection" to avoid having to refit
# note: function mclapply() is not available on windows
# note: adjust mc.cores to run on another (unix) computer
    #moose_ab_m_all <- parallel::mclapply(moose_ab_m_lst, eval, mc.cores = 16) #started at 2:05
    #head(moose_ab_m_all)

# drop models which did not converge
# and models with virtually not weight
    #moose_ab_modsel_tbl <- bbmle::AICctab(moose_ab_m_all,
     #                                 weights = TRUE,
      #                                base = TRUE,
       #                               logLik = TRUE,
        #                              nobs = TRUE,
         #                             mnames = names(moose_ab_m_all)) %>%
  #  as.data.frame() %>%
  #rownames_to_column(var = "ModelName") %>%
  #dplyr::filter(!is.na(logLik) & weight > 0.01)
  #head(moose_ab_modsel_tbl)

# List of models to keep for model averaging
    #moose_ab_m_tokeep <-
    #moose_ab_m_all[names(moose_ab_m_all) %in% moose_ab_modsel_tbl$ModelName]
    #head(moose_ab_m_tokeep)

# Perform model averaging:
    #moose_ab_mavg <- MuMIn::model.avg(moose_ab_m_tokeep)
    #moose_ab_mavg_summary <- summary(moose_ab_mavg)

# Find the confidence intervals 
    #moose_ab_mavg_ci <- confint(moose_ab_mavg)
    #moose_ab_mavg_summary
    #summary(moose_ab_mavg)

# Best model:
    #name_bm <- first(moose_ab_modsel_tbl$ModelName)
    #moose_ab_best_model <- moose_ab_m_tokeep[[eval(name_bm)]]
    #summary(moose_ab_best_model)


## Top Model:
topmodel <- glmmTMB(Pellet.counts ~ Wolf.territory + Forest_class + 
                      Pine.proportion + Big + Small + Rase.presence + 
                      Time.since.establishment + Wolf.territory:Forest_class +
                      (1|Taxar),
                    data = moose_simulated,
                   ziformula = zifo,
                  family = nbinom2)
summary(topmodel)

    ##moose_ab_p1 <- sjPlot::plot_model(moose_ab_best_model, type = "est")

    ##moose_ab_p2 <- sjPlot::plot_model(moose_ab_best_model, type = "pred", grid = TRUE)
    ##moose_ab_p2


## Predictions from model: =======================================================
## Marginal Effects Plots

## Set theme for sjPlots:
sjPlot::set_theme(
  geom.outline.color = "antiquewhite4",
  geom.outline.size = 1,
  geom.label.size = 2,
  geom.label.color = "grey50",
  title.color = "black",
  title.size = 1.25,
  axis.textcolor = "black",
  #axis.title.size = 2.5,
  axis.title.color = "black",
  #axis.textsize = 2,
  #legend.title.size = 2,
  base = theme_bw(
    base_family = "Arial Narrow",
    base_size = 12)
) 



# Main Roads:
Plot_abu_mainroads <- sjPlot::plot_model(topmodel, type= "pred", terms = ("Big"),
                                        axis.title = c("Distance main roads [km]", "Moose pellet counts"), 
                                        title = "") 

Plot_abu_mainroads + sjPlot::font_size(axis_title.x = 20, axis_title.y = 20, labels.x = 20, labels.y = 20, title = 15)

# forest roads:
Plot_abu_forestroads <- sjPlot::plot_model(topmodel, type= "pred", terms = ("Small"),
                                           axis.title = c("Distance forest roads [km]", "Moose pellet counts"), 
                                           title = "")
Plot_abu_forestroads + sjPlot::font_size(axis_title.x = 20, axis_title.y = 20, labels.x = 20, labels.y = 20, title = 15)

# Wolf territory:
Plot_abu_wolfterritory <- sjPlot::plot_model(topmodel, type= "pred", terms = ("Wolf.territory"),
                                             axis.title = c("Wolf Territory", "Moose pellet counts"), 
                                             title = "")
Plot_abu_wolfterritory + sjPlot::font_size(axis_title.x = 20, axis_title.y = 20, labels.x = 20, labels.y = 20, title = 15)


# Wolf territory:
Plot_abu_foreststage <- sjPlot::plot_model(topmodel, type= "pred", terms = ("Forest_class"),
                                           axis.title = c("Forest Stage", "Moose pellet counts"), 
                                           title = "")
Plot_abu_foreststage + sjPlot::font_size(axis_title.x = 20, axis_title.y = 20, labels.x = 20, labels.y = 20, title = 15)

summary(topmodel)


# Pine proportion:
Plot_abu_pineproportion <- sjPlot::plot_model(topmodel, type= "pred", terms = ("Pine.proportion"),
                                              axis.title = c("Pine proportion", "Moose pellet counts"), 
                                              title = "",
                                              base_family = "Arial Narrow")
Plot_abu_pineproportion + sjPlot::font_size(axis_title.x = 20, axis_title.y = 20, labels.x = 20, labels.y = 20, title = 15)


# Time since establishment
Plot_abu_tse <- sjPlot::plot_model(topmodel, type= "pred", terms = ("Time.since.establishment"),
                                   axis.title = c("Time Since Establishment", "Moose pellet counts"), 
                                   title = "",
                                   base_family = "Arial Narrow")
Plot_abu_tse + sjPlot::font_size(axis_title.x = 20, axis_title.y = 20, labels.x = 20, labels.y = 20, title = 15)

# Wolf territory:Forest stage
Plot_abu_int <- sjPlot::plot_model(topmodel, type= "int",
                                   axis.title = c("Wolf Territory", "Moose pellet counts"),
                                   legend.title = "Forest Class",
                                   title = "",
                                   base_family = "Arial Narrow")
Plot_abu_int + sjPlot::font_size(axis_title.x = 20, axis_title.y = 20, labels.x = 20, labels.y = 20, title = 15)



## Plot Grid:
ggpubr::ggarrange(Plot_abu_mainroads, Plot_abu_forestroads, Plot_abu_wolfterritory, 
                  Plot_abu_tse,Plot_abu_foreststage, Plot_abu_pineproportion, 
                  Plot_abu_int) + sjPlot::font_size(axis_title.x = 20, axis_title.y = 20, labels.x = 20, labels.y = 20, title = 15)

ggsave("Plots/mooseabundance_marginaleffects_03Oct21.png", height = 6, width = 10)


sjPlot::plot_grid(list(Plot_abu_mainroads, Plot_abu_forestroads, Plot_abu_wolfterritory, 
                  Plot_abu_tse,Plot_abu_foreststage, Plot_abu_pineproportion, 
                  Plot_abu_int), tags = TRUE, margin = c(0.25,0.25,0.25,0.25)) + sjPlot::font_size(axis_title.x = 25, axis_title.y = 25, labels.x = 25, labels.y = 25, title = 15)

??plot_grid
