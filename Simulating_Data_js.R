#My working directory
#setwd("C:/Users/sabri/OneDrive/Documents/BackUp/Living Data Project/Productivity/Assignements/Reproductibility/doi_10.5061_dryad.5hqbzkh37__v2")
setwd("/Users/jennascherger/Desktop/MSc/Courses/NSERC_CREATE/01_Productivity_Reproducibility/Assignments/02_Reproducible_Research/LDP_REPRO_Group8_Fall2021")

## Read in packages:
library(tidyverse)
library(parallel)
library(glmmTMB) # for running model
library(MuMIn) # for dredging

#Needed to import the data
#library(readxl) - if reading in the xlsx file need to use this
moose_original <- read.csv("Raw_Data/Moose_presence_abundance.csv")
moose_untouched <- read.csv("Raw_Data/Moose_presence_abundance.csv")

names(moose_original)
names(moose_untouched)

# Ignore this, i wanted to know where each column was.
#grep("Forest_class", names(moose_untouched)) #factor
#grep("Wolf.territory", names(moose_untouched)) #factor
#grep("Moose.presence", names(moose_untouched)) #factor
#grep("Small", names(moose_untouched)) #factor
#grep("Big", names(moose_untouched)) #factor
#grep("RASE.presence", names(moose_untouched)) # factor
#grep("Pine_proportion", names(moose_untouched))
#grep("Taxar", names(moose_untouched)) #factor
#grep("Pellet counts", names(moose_untouched))

#To look at the distribution
dev.off()
par(mfrow = c(2,4))
hist(moose_untouched$Taxar,
    main = "Taxar")
hist(moose_untouched$Forest_class,
     main = "Forest Class") # 4 categorical turned continous (1, 2, 3, 4)
hist(moose_untouched$Wolf.territory,
     main = "Wolf Territory") # binary (0 = no; 1 = yes)
hist(moose_untouched$Moose.presence,
     main = "Moose Presence") # binary (0 = no; 1 = yes) -> lots of 0's
hist(moose_untouched$Small.roads,
     main = "Small Roads") 
hist(moose_untouched$Big.roads,
     main = "Big Roads")
hist(moose_untouched$Pine_proportion,
     main = "Pine Proportion")

# I looked through the script for abundance and I think we only need to simulate those. Correct me if I'm wrong!

# Creating empty data frame
moose_simulated <- data.frame(matrix(0, ncol = 0, nrow = 10495))
moose_size <- nrow(moose_simulated)

#Set seed
set.seed(1234)

# Simulating Forest_class (categorical data)
size_forest <- as.integer(length(moose_untouched$Forest_class))

prob_1 <- sum(moose_untouched$Forest_class == 1)/moose_size
prob_2 <- sum(moose_untouched$Forest_class == 2)/moose_size
prob_3 <- sum(moose_untouched$Forest_class == 3)/moose_size
prob_4 <- sum(moose_untouched$Forest_class == 4)/moose_size

moose_simulated$Forest_class <- sample(c(1:4), size = size_forest, replace = TRUE, prob = c(prob_1, prob_2, prob_3, prob_4))

# Checking simulated data
par(mfrow = c(1,2))
hist(moose_untouched$Forest_class,
     main = "Forest Class - Raw")
hist(moose_simulated$Forest_class,
     main = "Forest Class - Simulated")
table(moose_untouched$Forest_class) # 1 = 499; 2 = 2386; 3 = 4301; 4 = 3309
table(moose_simulated$Forest_class) # 1 = 477; 2 = 2387; 3 = 4255; 4 = 3376

# Simulating wolf territory (binomial distribution)
prob_wolf <- sum(moose_untouched$Wolf.territory == 1) / moose_size

moose_simulated$Wolf.territory <- rbinom(moose_size, 1, prob_wolf)

# Checking simulated data
hist(moose_untouched$Wolf.territory,
     main = "Wolf Territory - Raw")
hist(moose_simulated$Wolf.territory,
     main = "Wolf Territory - Simulated")
table(moose_untouched$Wolf.territory) # 0 = 6518; 1 = 3977
table(moose_simulated$Wolf.territory) # 0 = 6452; 1 = 4043

# Simulating moose presence (binomial distribution)
prob_moose <- sum(moose_untouched$Moose.presence == 1) / moose_size

moose_simulated$Moose.presence <- rbinom(moose_size, 1, prob_moose)

# Checking simulated data
hist(moose_untouched$Moose.presence,
     main = "Moose Presence - Raw")
hist(moose_simulated$Moose.presence,
     main = "Moose Presence - Simulated")
table(moose_untouched$Moose.presence) # 0 = 9252; 1 = 1242
table(moose_simulated$Moose.presence) # 0 = 9239; 1 = 1256

# Simulating RASE presence (binomial distribution)
prob_rase <- sum(moose_untouched$RASE.presence == 1) / moose_size

moose_simulated$Rase.presence <- rbinom(moose_untouched$RASE.presence, 1, prob_rase)

# Checking simulated data
hist(moose_untouched$RASE.presence,
     main = "RASE Presence - Raw")
hist(moose_simulated$Rase.presence,
     main = "RASE Presence - Simulated")
table(moose_untouched$RASE.presence) # 0 = 5018; 1 = 5477
table(moose_simulated$Rase.presence) # 0 - 4968; 1 = 5527

# Simulating Taxar (uniform distribution of dates)
# We need the package purrr (in tidyverse)
library(purrr)
moose_simulated$Taxar <- rdunif(moose_size, min(moose_untouched$Taxar), max(moose_untouched$Taxar))
hist(moose_simulated$Taxar)

#Out put of what Sabrina had
#write.csv(moose_simulated, "Moose_simulate")


head(moose_simulated)
# Still need pellet counts, big roads, and small roads - not familar with how to simulate, so for now I (jenna) will just
# use the raw values
head(moose_untouched)

## Adding pellet counts, pine proprtion, big, and small roads:
moose_simulated <- moose_simulated %>%
  mutate(Small.roads = moose_untouched$Small.roads) %>%
  mutate(Big.roads = moose_untouched$Big.roads) %>%
  mutate(Pellet.counts = moose_untouched$Pellet.counts) %>%
  mutate(Pine.proportion = moose_untouched$Pine_proportion)
  

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
