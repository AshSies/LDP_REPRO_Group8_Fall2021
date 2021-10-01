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

# Importing the raw data =========================================================================#
# The data are available on a Dryad repository
# https://datadryad.org/stash/share/svRCeH_hvPZVH2UjmzWQOvErl-B6deccaKMXgGEeHtM

moose_raw <- readxl::read_excel("Raw_Data/Moose_presence_abundance.xlsx")

# Data frame for the simulation ===============================================================#

# Creating empty data frame that will host the simulated data
moose_simulated <- data.frame(matrix(0, ncol = 0, nrow = nrow(moose_raw)))

# Simulating the data : TAXAR ===============================================================#

# 1.  Visualizing the raw data distribution
hist(moose_raw$Taxar, main = "Raw data: Taxar") 
# Uniform distribution of dates

# 2. Setting the seed
set.seed(5)

# 3. Simulating the data
# The package purrr (available in tidyverse) is needed to simulate an uniform distribution of dates.
moose_simulated$Taxar <- purrr::rdunif(n = nrow(moose_raw), min(moose_raw$Taxar), max(moose_raw$Taxar))

# 4. Comparing the simulated data distribution
hist(moose_simulated$Taxar, main = "Simulated data: Taxar")

summary(moose_raw$Taxar) # Median = 2010; Mean = 2010 
summary(moose_simulated$Taxar) # Median = 2009; Mean = 2009

# Simulating the data : RASE PRESENCE ===============================================================#

# 1.  Visualizing the raw data distribution
hist(moose_raw$`RASE presence`, main = "Raw data: RASE presence")
# Binary (0 = absence; 1 = presence)

# 2. Setting the seed
set.seed(5)

# 3. Simulating the data

# Defining the raw data ratio of the presence
prob_rase <- sum(moose_raw$`RASE presence` == 1) / nrow(moose_raw)

# Incorporating the variables
moose_simulated$RASE_presence <- rbinom(n = nrow(moose_raw), 1, prob_rase)

# 4. Comparing the simulated data distribution
hist(moose_simulated$RASE_presence, main = "Simulated data: RASE presence")

summary(moose_raw$`RASE presence`) # Median = 1.0000; Mean = 0.5219
summary(moose_simulated$RASE_presence) # Meadian = 1.0000; Mean = 0.5213

# Simulating the data : FOREST CLASS ===============================================================#

# Defining the distribution of the raw data.
hist(moose_raw$Forest_class, main = "Raw data: Forest Class") 
# Categorical data

# 2. setting the seed
set.seed(5)

# 3. Simulating the data

# Defining the raw data ratio of the different categories
prob_forest1 <- sum(moose_raw$Forest_class == 1)/nrow(moose_raw)
prob_forest2 <- sum(moose_raw$Forest_class == 2)/nrow(moose_raw)
prob_forest3 <- sum(moose_raw$Forest_class == 3)/nrow(moose_raw)
prob_forest4 <- sum(moose_raw$Forest_class == 4)/nrow(moose_raw)

# Incorporating the variables
moose_simulated$Forest_class <- sample(c(1:4), size = nrow(moose_raw), replace = TRUE, 
                                       prob = c(prob_forest1, prob_forest2, prob_forest3, prob_forest4))

# 4. Comparing the simulated data distribution
hist(moose_simulated$Forest_class, main = "Simulated data: Forest class")

summary(moose_raw$Forest_class) # Median = 3.000; Mean = 2.993
summary(moose_simulated$Forest_class) # Median = 3.000; Mean = 2.981

# Simulating the data : MOOSE PRESENCE ===============================================================#

# 1.  Visualizing the raw data distribution
hist(moose_raw$`Moose presence`, main = "Raw data: Moose Presence") 
# Binary (0 = absence; 1 = presence)

# 2. Setting the seed
set.seed(5)

# 3. Simulating the data

# Defining the raw data ration of the presence
prob_moose <- sum(moose_raw$`Moose presence` == 1) / nrow(moose_simulated)

# Incorporating the variables
moose_simulated$Moose_presence <- rbinom(n = nrow(moose_raw), 1, prob_moose)

# 4. Comparing the simulated data distribution
hist(moose_simulated$Moose_presence, main = "Simulated data: Moose presence")

summary(moose_raw$`Moose presence`) # Mean = 0.1184
summary(moose_simulated$Moose_presence) # Mean = 0.1197

# Simulating the data : WOLF TERRITORY ===============================================================#

# 1.  Visualizing the raw data distribution
hist(moose_raw$`Wolf territory`, main = "Raw data: Wolf Territory") 
# Binary (0 = absence; 1 = presence)

# 2. Setting the seed
set.seed(5)

# 3. Simulating the data

# Defining the raw data ratio of the presence
prob_wolf <- sum(moose_raw$`Wolf territory` == 1) / nrow(moose_raw)

# Incorporating the variables
moose_simulated$Wolf_territory <- rbinom(n = nrow(moose_raw), 1, prob_wolf)

# 4. Comparing the simulated data distribution
hist(moose_simulated$Wolf_territory, main = "Simulated data: Wolf territory")

summary(moose_raw$`Wolf territory`) # Mean = 0.3789
summary(moose_simulated$Wolf_territory) # Mean = 0.3836


# Simulating the data : PINE PROPORTION ===============================================================#

# 1.  Visualizing the raw data distribution
hist(moose_raw$Pine_proportion, main = "Raw data: Pine Proportion")
# Zero-inflated negative binomial model

# 2. Setting the seed
set.seed(5)

# 3. Simulating the data

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

# Simulating the data with the variables established
moose_simulated$Pine_proportion <- (emdbook::rzinbinom(n = nrow(moose_raw), mu = pine_mu, size = pine_size, zprob = pine_zprob))/100

# 4. Comparing the simulated data distribution
hist(moose_simulated$Pine_proportion, main = "Simulated data: Pine proportion")

summary(moose_raw$Pine_proportion) # Median = 0.006496; Mean = 0.039826
summary(moose_simulated$Pine_proportion) # Median = 0.00000; Mean = 0.02409

# Simulating the data : PELLET COUNTS ===============================================================#

# 1.  Visualizing the raw data distribution
hist(moose_raw$`Pellet counts`, main = "Raw data: Pellet counts")
# Zero-inflated negative binomial model

# 2. Setting the seed
set.seed(5)

# 3. Simulating the data

# ADD DETAIL WHY (SABRINA)
moose_simulated$Pellet_raw_10 <- (moose_raw$`Pellet counts`)*10

# zprob : probability of structural zeros
pellet_zprob <- sum(moose_simulated$Pellet_raw_10 == 0)/nrow(moose_raw)

# mu : mean of the pine proportion
pellet_mu <- mean(moose_simulated$Pellet_raw_10)

# Variance : measuring the dispersion
pellet_var <- var(moose_simulated$Pellet_raw_10)

# Size : the number of scores used to compute a mean (SABRINA)
# We can find the size knowing that variance = (mu + mu^2)/size
# Then size = (mu + mu^2)/variance
pellet_size <- (pellet_mu + (pellet_mu^2))/pellet_var

# Simulating the data with the variables established
moose_simulated$Pellet_counts <- (emdbook::rzinbinom(n = nrow(moose_raw), mu = pellet_mu, size = pellet_size, zprob = pellet_zprob))/10

# 4. Comparing the simulated data distribution
hist(moose_simulated$Pellet_counts, main = "Simulated data: Pellet counts")

summary(moose_raw$`Pellet counts`) # Median = 0.0000; Mean = 0.1675
summary(moose_simulated$Pellet_counts) # Median = 0.00000; Mean = 0.02233

# Simulating the data : TIME SINCE ESTABLISHMENT ==============================================================#

# 1.  Visualizing the raw data distribution
hist(moose_raw$`Time since establishment`, main = "Raw data: Time since establishment")
# Zero-inflated negative binomial model

# 2. Setting the seed
set.seed(50)

# 3. Simulating the data

# mu : mean of the pine proportion
time_mu <- mean(moose_raw$`Time since establishment`)

# Variance : measuring the dispersion
time_var <- var(moose_raw$`Time since establishment`)

# Size : the number of scores used to compute a mean (SABRINA)
# We can find the size knowing that variance = (mu + mu^2)/size
# Then size = (mu + mu^2)/variance
time_size <- (time_mu + (time_mu^2))/time_var

# Simulating the data with the variables established
moose_simulated$Time_since_establishment <- rnbinom(n = nrow(moose_raw), mu = time_mu, size = time_size)

# 4. Comparing the simulated data distribution
hist(moose_simulated$Time_since_establishment, main = "Simulated data: Time since establishment")

summary(moose_raw$`Time since establishment`) # Median = 0.000; Mean = 2.324
summary(moose_simulated$Time_since_establishment) # Median = 0.0000; Mean = 0.8843

# Simulating the data : SMALL ROADS ===============================================================#

# 1.  Visualizing the raw data distribution
hist(moose_raw$`Small roads`, main = "Raw data: Small Roads") 
# Zero-inflated negative binomial model

# 2. Setting the seed
set.seed(5)

# 3. Simulating the data

# mu : mean of the pine proportion
sroad_mu <- mean(moose_raw$`Small roads`)

# Variance : measuring the dispersion
sroad_var <- var(moose_raw$`Small roads`)

# Size : the number of scores used to compute a mean (SABRINA)
# We can find the size knowing that variance = (mu + mu^2)/size
# Then size = (mu + mu^2)/variance
sroad_size <- (sroad_mu + (sroad_mu^2))/sroad_var

# Simulating the data with the variables established
moose_simulated$Small_roads <- rnbinom(n = nrow(moose_raw), mu = sroad_mu, size = sroad_size)

# 4. Comparing the simulated data distribution
hist(moose_simulated$Small_roads, main = "Simulated data: Small roads")

summary(moose_raw$`Small roads`) # Median = 198.9641; Mean = 254.1479
summary(moose_simulated$Small_roads) # Median = 191.0; Mean = 254.4

# Simulating the data : BIG ROADS ===============================================================#

# 1.  Visualizing the raw data distribution
hist(moose_raw$`Big roads`, main = "Raw data: Big Roads")
# Zero-inflated negative binomial model

# 2. Setting the seed
set.seed(5)

# 3. Simulating the data

# mu : mean of the pine proportion
broad_mu <- mean(moose_raw$`Big roads`)

# Variance : measuring the dispersion
broad_var <- var(moose_raw$`Big roads`)

# Size : the number of scores used to compute a mean (SABRINA)
# We can find the size knowing that variance = (mu + mu^2)/size
# Then size = (mu + mu^2)/variance
broad_size <- (broad_mu + (broad_mu^2))/broad_var

# Simulating the data with the variables established
moose_simulated$Big_roads <- rnbinom(n = nrow(moose_raw), mu = broad_mu, size = broad_size)

# 4. Comparing the simulated data distribution
hist(moose_simulated$Big_roads, main = "Simulated data: Big roads")

summary(moose_raw$`Big roads`) # Median = 1514.442; Mean = 2288.179
summary(moose_simulated$Big_roads) # Median = 1456; Mean = 2297

##==================================================================================
## Next runing through the analyses


# Still need pellet counts, big roads, and small roads - not familar with how to simulate, so for now I (jenna) will just
# use the raw values
#head(moose_untouched)

## Adding pellet counts, pine proprtion, big, and small roads:
#moose_simulated <- moose_simulated %>%
#  mutate(Small.roads = moose_untouched$Small.roads) %>%
#  mutate(Big.roads = moose_untouched$Big.roads) %>%
#  mutate(Pellet.counts = moose_untouched$Pellet.counts) %>%
#  mutate(Pine.proportion = moose_untouched$Pine_proportion)
  

## Now following the authors script:
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

# --- starting model ---


# formula for zero inflation
zifo <- ~ Pine.proportion + Big + Small

# start model
m_moose_ab <- glmmTMB(Pellet.counts ~ Wolf.territory + Forest_class +
                        Wolf.territory:Forest_class + Pine.proportion + Big +
                        Small + Rase.presence + (1|Taxar),
                      data = moose_simulated,
                      ziformula = zifo,
                      family = nbinom2)
summary(m_moose_ab)

# --- dredging ---

# how many models would that make with dredge?
moose_ab_m_lst <- dredge(m_moose_ab, evaluate = FALSE)
length(moose_ab_m_lst)


moose_ab_m_all <- dredge(m_moose_ab)
head(moose_ab_m_all)

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


## Plot Grid:
sjPlot::plot_grid(list(Plot_abu_mainroads, Plot_abu_forestroads, Plot_abu_wolfterritory,
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