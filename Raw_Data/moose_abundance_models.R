
library(glmmTMB)
library(MuMIn)
library(parallel)
library(tidyverse)
library(sjPlot)
library(bbmle)



# --- data ---

Moose_pr <- read.delim("", header = T)

Moose_pr$Forest_class <- factor(Moose_pr$Forest_class, levels = c(1:4), labels = c("Clearcut", "Young", "Thinned", "Mature"))
Moose_pr$Wolf.territory <- factor(Moose_pr$Wolf.territory, levels = c(0:1), labels = c("Outside","Inside"))
Moose_pr$Moose.presence<- factor(Moose_pr$Moose.presence, levels = c(0:1), labels = c("Absent", "Present"))
Moose_pr$Small <- Moose_pr$Small.roads/1000
Moose_pr$Big <- Moose_pr$Big.roads/1000
Moose_pr$RASE.presence<- factor(Moose_pr$RASE.presence, levels = c(0:1), labels = c("Absent", "Present"))

# make sure the grouping variable is a factor
Moose_pr <-
  Moose_pr %>%
  mutate(Taxar = as.factor(Taxar))



# --- starting model ---

# formula for zero inflation
zifo <- ~ Pine_proportion + Big + Small

# start model
m_moose_ab <- glmmTMB(Pellet.counts ~ Wolf.territory + Forest_class +
                     Wolf.territory:Forest_class + Pine_proportion + Big +
                     Small + RASE.presence + (1|Taxar),
                   data = Moose_pr,
                   ziformula = zifo,
                   family = nbinom2
)


# --- dredging ---

# how many models would that make with dredge?
moose_ab_m_lst <- dredge(m_moose_ab, evaluate = FALSE)
length(moose_ab_m_lst)

# fit all models outside of "model.selection" to avoid having to refit
# note: function mclapply() is not available on windows
# note: adjust mc.cores to run on another (unix) computer
moose_ab_m_all <- mclapply(moose_ab_m_lst, eval, mc.cores = 16)


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

# List of models to keep for model averaging
moose_ab_m_tokeep <-
  moose_ab_m_all[names(moose_ab_m_all) %in% moose_ab_modsel_tbl$ModelName]


# --- model-averaging ---

moose_ab_mavg <- MuMIn::model.avg(moose_ab_m_tokeep)
moose_ab_mavg2 <- model.avg(get.models(moose_ab_m_tokeep, subset = df <5))

moose_ab_mavg_summary <- summary(moose_ab_mavg)

moose_ab_mavg_ci <- confint(moose_ab_mavg)
moose_ab_mavg_summary
summary(moose_ab_mavg)

# --- best model ---

name_bm <- first(moose_ab_modsel_tbl$ModelName)

moose_ab_best_model <- moose_ab_m_tokeep[[eval(name_bm)]]
summary(moose_ab_best_model)


# --- plots for best model ---

moose_ab_p1 <- plot_model(moose_ab_best_model, type = "est")

moose_ab_p2 <- plot_model(moose_ab_best_model, type = "pred", grid = TRUE)
moose_ab_p2

Plot_abu_pine2<- plot_model(moose_ab_best_model, type= "pred", terms = ("Big"),
                            axis.title = c("Distance from main roads", "Moose pellet counts"), 
                            title = "")
Plot_abu_pine2 +font_size(axis_title.x = 20, axis_title.y = 20, labels.x = 20, labels.y = 20, title = 15)

