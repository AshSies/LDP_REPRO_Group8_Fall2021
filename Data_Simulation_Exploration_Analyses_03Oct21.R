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


names(moose_simulated)
# Check correlation between predictors:
preds <- moose_simulated %>%
  dplyr::select(-c(Pellet.counts, Pine.proportion.100))
head(preds)

par(mfrow=c(1,1))
preds_cor <- cor(preds)
head(round(preds_cor, 2))
corrplot::corrplot(preds_cor, method = "color", addCoef.col="black", order = "AOE", tl.cex = 0.75, number.cex = 0.4)
# no pairings >= 0.6


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

# Check how many zeros in the response variable
zifo_test <- nrow(moose_simulated %>% dplyr::filter(Pellet.counts == 0))/nrow(moose_simulated)*100
zifo_test # 90.68% of data is = 0

hist(moose_simulated$Pellet.counts,
     main = "Zero Inflation Visual",
     xlab = "Pellet Counts")


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



## End of Script
