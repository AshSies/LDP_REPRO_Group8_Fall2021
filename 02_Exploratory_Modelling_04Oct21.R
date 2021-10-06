#=================================================================================================#
#============================ LDP: Productivity and Reproducibility ==============================#
#============================== Group Assignment: Pre-Registration ===============================#
#=============== By : Ashton Sies, Emma Nikkel, Jenna Scherger, and Sabrina St-Pierre ============#
#================================== Date : 1st of October 2021 ===================================#
#=================================================================================================#


## Script 02. Simulated Data Exploration & Analyses
# Includes:
  # 1. Correlation matrix of predictor variables
  # 2. Initial exploration of predictors on response variable
  # 3. Modelling (model selection, model averaging)
  # 4. Predictions of model (marginal effects plots)
  

# Read in packages ===============================================================================#
library(tidyverse) # for wrangling data - using primarily dplyr, tidyr
library(corrplot) # for checking correlation matrix 
library(glmmTMB) # for running model
library(parallel) # for parallel processing when running model selection 
library(MuMIn) # for dredging (model selection)
library(sjPlot) # for marginal effects plots
library(ggpubr) # for arranging multiple plots into 1 figure

# Set working directory:===========================================================
#setwd("C:/Users/sabri/OneDrive/Documents/BackUp/Living Data Project/Productivity/Assignements/Reproductibility/doi_10.5061_dryad.5hqbzkh37__v2")
setwd("/Users/jennascherger/Desktop/MSc/Courses/NSERC_CREATE/01_Productivity_Reproducibility/Assignments/02_Reproducible_Research/LDP_REPRO_Group8_Fall2021")

# Read in data:===================================================================
moose_simulated <- read.csv("Simulated_Data/moose_abundance_simulated_04Oct21.csv") %>%
  dplyr::select(-X)


# 01. Check For Correlation Between Predictor Variables:
preds <- moose_simulated %>%
  dplyr::select(-c(Pellet.counts, Pine.proportion.100, Taxar))
head(preds)

par(mfrow=c(1,1))
preds_cor <- cor(preds)
head(round(preds_cor, 2))
corrplot::corrplot(preds_cor, method = "color", addCoef.col="black", order = "AOE", tl.cex = 0.75, number.cex = 0.4)
# no pairings >= 0.6


##==================================================================================


## 02. Exploratory Analyses of Simulated Data
# First, change class of the different predictors:
moose_simulated$Forest_class <- factor(moose_simulated$Forest_class, levels = c(1:4), labels = c("Clearcut", "Young", "Thinned", "Mature"))
moose_simulated$Wolf.territory <- factor(moose_simulated$Wolf.territory, levels = c(0:1), labels = c("Outside","Inside"))
moose_simulated$Moose.presence<- factor(moose_simulated$Moose.presence, levels = c(0:1), labels = c("Absent", "Present"))
moose_simulated$Small <- moose_simulated$Small.roads/1000
moose_simulated$Big <- moose_simulated$Big.roads/1000
moose_simulated$Rase.presence<- factor(moose_simulated$Rase.presence, levels = c(0:1), labels = c("Absent", "Present"))

# Set taxar (grouping variable) to a factor:
moose_simulated <-
  moose_simulated %>%
  mutate(Taxar = as.factor(Taxar))

head(moose_simulated)

## Visualize effects of predictors on response variable:
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

# 03. Check For Zero-Inflated Data
## Determining how many zeros are in the repsonse variable
zifo_test <- nrow(moose_simulated %>% dplyr::filter(Pellet.counts == 0))/nrow(moose_simulated)*100
zifo_test # 90.68% of data is = 0, therefore should use negative binomial distribution in models

hist(moose_simulated$Pellet.counts,
     main = "Zero Inflation Visual",
     xlab = "Pellet Counts")


##==================================================================================


# 04. Running Model
# Formula for zero inflation:
zifo <- ~ Pine.proportion + Big + Small

# A. Run model:
m_moose_ab <- glmmTMB(Pellet.counts ~ Wolf.territory + Forest_class +
                        Wolf.territory:Forest_class + Pine.proportion + Big +
                        Small + Rase.presence + Time.since.establishment + (1|Taxar),
                      data = moose_simulated,
                      ziformula = zifo,
                      family = nbinom2)
summary(m_moose_ab)


# B. AICc Based Model Selection 
# First checking how many models would that make with dredge?
moose_ab_m_lst <- MuMIn::dredge(m_moose_ab, evaluate = FALSE)
length(moose_ab_m_lst) #1280 models


## Please note: I have hashed out the next couple chunks of code because 
## running model selection takes ~ 2 hours using parallel processing,
## I have written in topmodel() at line 185 which gives the best model found
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


##=================================================================================


## 05. Visualizing Predictions From Model - Marginal Effects Plots
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


# A. Main Roads:
Plot_abu_mainroads <- sjPlot::plot_model(topmodel, type= "pred", terms = ("Big"),
                                         axis.title = c("Distance main roads [km]", "Moose Abundance"), 
                                         title = "") 

Plot_abu_mainroads + sjPlot::font_size(axis_title.x = 20, axis_title.y = 20, labels.x = 20, labels.y = 20, title = 15)

# B. Forest Roads:
Plot_abu_forestroads <- sjPlot::plot_model(topmodel, type= "pred", terms = ("Small"),
                                           axis.title = c("Distance forest roads [km]", "Moose Abundance"), 
                                           title = "")
Plot_abu_forestroads + sjPlot::font_size(axis_title.x = 20, axis_title.y = 20, labels.x = 20, labels.y = 20, title = 15)

# C. Wolf Territory:
Plot_abu_wolfterritory <- sjPlot::plot_model(topmodel, type= "pred", terms = ("Wolf.territory"),
                                             axis.title = c("Wolf Territory", "Moose Abundance"), 
                                             title = "")
Plot_abu_wolfterritory + sjPlot::font_size(axis_title.x = 20, axis_title.y = 20, labels.x = 20, labels.y = 20, title = 15)

# D. Forest Stage:
Plot_abu_foreststage <- sjPlot::plot_model(topmodel, type= "pred", terms = ("Forest_class"),
                                           axis.title = c("Forest Stage", "Moose Abundance"), 
                                           title = "")
Plot_abu_foreststage + sjPlot::font_size(axis_title.x = 20, axis_title.y = 20, labels.x = 20, labels.y = 20, title = 15)

# E. Pine Proportion:
Plot_abu_pineproportion <- sjPlot::plot_model(topmodel, type= "pred", terms = ("Pine.proportion"),
                                              axis.title = c("Pine proportion", "Moose Abundance"), 
                                              title = "",
                                              base_family = "Arial Narrow")
Plot_abu_pineproportion + sjPlot::font_size(axis_title.x = 20, axis_title.y = 20, labels.x = 20, labels.y = 20, title = 15)


# F. Time Since Wolf Establishment:
Plot_abu_tse <- sjPlot::plot_model(topmodel, type= "pred", terms = ("Time.since.establishment"),
                                   axis.title = c("Time Since Establishment", "Moose Abundance"), 
                                   title = "",
                                   base_family = "Arial Narrow")
Plot_abu_tse + sjPlot::font_size(axis_title.x = 20, axis_title.y = 20, labels.x = 20, labels.y = 20, title = 15)

# G. Interaction Term - Wolf Territory:Forest Stage
Plot_abu_int <- sjPlot::plot_model(topmodel, type= "int",
                                   axis.title = c("Wolf Territory", "Moose Abundance"),
                                   legend.title = "Forest Class",
                                   title = "",
                                   base_family = "Arial Narrow")
Plot_abu_int + sjPlot::font_size(axis_title.x = 20, axis_title.y = 20, labels.x = 20, labels.y = 20, title = 15)


## Plot Grid (all marginal effects together):
sjPlot::plot_grid(list(Plot_abu_mainroads, Plot_abu_forestroads, Plot_abu_wolfterritory, 
                       Plot_abu_tse,Plot_abu_foreststage, Plot_abu_pineproportion, 
                       Plot_abu_int), tags = TRUE, margin = c(0.25,0.25,0.25,0.25)) + sjPlot::font_size(axis_title.x = 25, axis_title.y = 25, labels.x = 25, labels.y = 25, title = 15)

## or use ggppubr:
ggpubr::ggarrange(Plot_abu_mainroads, Plot_abu_forestroads, Plot_abu_wolfterritory, 
                  Plot_abu_tse,Plot_abu_foreststage, Plot_abu_pineproportion, 
                  Plot_abu_int) + sjPlot::font_size(axis_title.x = 20, axis_title.y = 20, labels.x = 20, labels.y = 20, title = 15)
ggsave("Plots/mooseabundance_marginaleffects_03Oct21.png", height = 6, width = 10)


## End Script
