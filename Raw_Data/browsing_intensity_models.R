

library(glmmTMB)
library(MuMIn)
library(parallel)
library(tidyverse)
library(sjPlot)
library(bbmle)



# -------------------------
### BROWSING MODELS #####
# ------------------------

### Preparing the data
Browsing_pr <- read.delim("New_browsing_analysis.txt", header = T)
Browsing_pr$Wolf.territory <- factor(Browsing_pr$Wolf.territory,
                                     levels = c(0:1),
                                     labels = c("Outside", "Inside"))
Browsing_pr$Distance_forest_roads <- Browsing_pr$Small.roads / 1000
Browsing_pr$Distance_main_roads <- Browsing_pr$Big.roads / 1000
Browsing_pr$Browsing_presence <- factor(Browsing_pr$Browsing_presence,
                                        levels = c(0:1),
                                        labels = c("Absent", "Present"))
Browsing_pr$RASE.presence <- factor(Browsing_pr$RASE.presence,
                                    levels = c(0:1),
                                    labels = c("Absent", "Present"))

names(Browsing_pr)[names(Browsing_pr) == "Wolf.territory"] <- "Wolf_presence"
names(Browsing_pr)[names(Browsing_pr) == "Old"] <- "Previous_browsing"
names(Browsing_pr)[names(Browsing_pr) == "Pellet.counts"] <- "Moose_pellet_counts"
names(Browsing_pr)[names(Browsing_pr) == "RASE.presence"] <- "RAWO_presence"
sum(is.na(Browsing_pr)) # 0 Nas



# --------------------------------------------
### Browsing intensity and wolf presence ###
# --------------------------------------------

Browsing_int_m <- glmmTMB(Fresh ~ Moose_pellet_counts + Proportion_pine +
                          Wolf_presence + RAWO_presence + Distance_forest_roads +
                          Distance_main_roads + Previous_browsing +
                          (1 | Taxar),
                        data = Browsing_pr,
                        ziformula = ~ Moose_pellet_counts + Proportion_pine +
                          Previous_browsing,
                        family = nbinom2)

# Browsing_int_m_d <- dredge(Browsing_int_m, rank = "AIC") # dredge
# Browsing_int_m_av <- model.avg(Browsing_int_m_d) # model averaging
# summary(Browsing_int_m_av)

# how many models?
Browsing_int_m_lst <- dredge(Browsing_int_m, rank = "AIC", evaluate = FALSE)
length(Browsing_int_m_lst)

# fit all models outside of "model.selection" to avoid having to refit
# note: function mclapply() is not available on windows
# note: adjust mc.cores to run on another (unix) computer
Browsing_int_m_d <- mclapply(Browsing_int_m_lst, eval, mc.cores = 16)


# --- model selection ---

# if model-averaging directly on list of models, we don't get the
# model-averaged coefficients estimates, because some models have
# singularities (ie they did not converge)
# so first we need to remove the models with singularities
# we'll consider that the models with singularities are the ones without an
# AIC; so we'll drop model without an AIC


# model selection table (note the use of bbmle::AICctab())
# keep only models which converged (ie likelihood not NA)
# keep only models with weight higher than 0.01 for model selection
# (comment last line for full model selection table if needed)
Browsing_int_modsel_tbl <- bbmle::AICctab(Browsing_int_m_d,
                                          weights = TRUE,
                                          base = TRUE,
                                          logLik = TRUE,
                                          nobs = TRUE,
                                          mnames = names(Browsing_int_m_d)) %>%
  as.data.frame() %>%
  rownames_to_column(var = "ModelName") %>%
  dplyr::filter(!is.na(logLik) & weight > 0.01)

# List of models to keep for model averaging
Browsing_int_m_tokeep <-
  Browsing_int_m_d[names(Browsing_int_m_d) %in% Browsing_int_modsel_tbl$ModelName]


# --- model-averaging ---

Browsing_mavg <- MuMIn::model.avg(Browsing_int_m_tokeep)

Browsing_mavg_summary <- summary(Browsing_mavg)
Browsing_mavg_summary

Browsing_mavg_ci <- confint(Browsing_mavg)

summary(Browsing_mavg)
# --- best model ---

name_bm <- first(Browsing_int_modsel_tbl$ModelName)

Browsing_int_best_model <- Browsing_int_m_tokeep[[eval(name_bm)]]


# --- plots for best model ---

Browsing_int_p1 <- plot_model(Browsing_int_best_model, type = "est")
Browsing_int_p1

Browsing_int_p2 <- plot_model(Browsing_int_best_model, type = "pred", grid = TRUE)
Browsing_int_p2









# ----------------------------------------------------------------------
### Browsing intensity and time since wolf territory establishment ###
# ----------------------------------------------------------------------

# Repeat same analysis but with continuous variable for wolf presence

Browsing_int_m_t <- glmmTMB(Fresh ~ Moose_pellet_counts + Proportion_pine +
                            Time.since.establishment + RAWO_presence +
                            Distance_forest_roads + Distance_main_roads +
                            Previous_browsing +
                            (1 | Taxar),
                          data = Browsing_pr,
                          ziformula = ~ Moose_pellet_counts + Proportion_pine +
                            Previous_browsing,
                          family = nbinom2)

# Browsing_int_m_t_d <- dredge(Browsing_int_m_t, rank = "AIC") # dredge
# Browsing_int_m_t_d_av <- model.avg(Browsing_int_m_t_d) # model averaging
# summary(Browsing_int_m_av)


Browsing_int_m_t_lst <- dredge(Browsing_int_m_t, rank = "AIC", evaluate = FALSE)
length(Browsing_int_m_t_lst)

Browsing_int_m_t_d <- mclapply(Browsing_int_m_t_lst, eval, mc.cores = 16)


# ***
# --- model selection ---

# if model-averaging directly on list of models, we don't get the
# model-averaged coefficients estimates, because some models have
# singularities (ie they did not converge)
# so first we need to remove the models with singularities
# we'll consider that the models with singularities are the ones without an
# AIC; so we'll drop model without an AIC


# model selection table (note the use of bbmle::AICctab())
# keep only models which converged (ie likelihood not NA)
# keep only models with weight higher than 0.01 for model selection
# (comment last line for full model selection table if needed)
Browsing_int_t_modsel_tbl <- bbmle::AICctab(Browsing_int_m_t_d,
                                          weights = TRUE,
                                          base = TRUE,
                                          logLik = TRUE,
                                          nobs = TRUE,
                                          mnames = names(Browsing_int_m_t_d)) %>%
  as.data.frame() %>%
  rownames_to_column(var = "ModelName") %>%
  dplyr::filter(!is.na(logLik) & weight > 0.01)

# List of models to keep for model averaging
Browsing_int_m_t_tokeep <-
  Browsing_int_m_t_d[names(Browsing_int_m_t_d) %in% Browsing_int_t_modsel_tbl$ModelName]


# --- model-averaging ---

Browsing_t_mavg <- MuMIn::model.avg(Browsing_int_m_t_tokeep)

Browsing_t_mavg_summary <- summary(Browsing_t_mavg)

Browsing_t_mavg_ci <- confint(Browsing_t_mavg)
Browsing_t_mavg_summary

# --- best model ---

name_bm <- first(Browsing_int_t_modsel_tbl$ModelName)

Browsing_int_t_best_model <- Browsing_int_m_t_tokeep[[eval(name_bm)]]

# --- plots for best model ---

Browsing_int_t_p1 <- plot_model(Browsing_int_t_best_model, type = "est")

Browsing_int_t_p2 <- plot_model(Browsing_int_t_best_model, type = "pred", grid = TRUE)


