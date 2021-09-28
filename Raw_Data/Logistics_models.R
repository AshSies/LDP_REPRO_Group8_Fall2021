## Ecological effects of wolves in anthropogenic landscapes
## Giorgia Ausilio, 2020
## Script for moose presence/abundance and browsing damage/intensity

rm(list = ls())

## Packages

library(car)
library(lme4)
library(ggplot2)
library(sjPlot)
library(MuMIn)
library(glmmTMB)
library(sjlabelled)
library(finalfit)
library(dplyr)
library(gridExtra)


## Preparing the data 
## Moose presence
Moose_pr <- read.delim("New_abundance.txt", header = T)
Moose_pr$Forest_stage <- factor(Moose_pr$Forest_class, levels = c(1:4), labels = c("Clearcut", "Young", "Thinned", "Mature"))
Moose_pr$Wolf_presence <- factor(Moose_pr$Wolf.territory, levels = c(0:1), labels = c("Absent","Present"))
Moose_pr$Moose_presence<- factor(Moose_pr$Moose.presence, levels = c(0:1), labels = c("Absent", "Present"))
Moose_pr$Distance_forest_roads <- Moose_pr$Small.roads/1000
Moose_pr$Distance_main_roads <- Moose_pr$Big.roads/1000
Moose_pr$RAWO_presence<- factor(Moose_pr$RASE.presence, levels = c(0:1), labels = c("Absent", "Present"))

## Browsing damage presence
Browsing_pr <- read.delim("New_browsing.txt", header = T)
Browsing_pr$Wolf.territory <- factor(Browsing_pr$Wolf.territory, levels = c(0:1), labels = c("Absent","Present"))
Browsing_pr$Distance_forest_roads <- Browsing_pr$Small.roads/1000
Browsing_pr$Distance_main_roads <- Browsing_pr$Big.roads/1000
Browsing_pr$Browsing_damage_presence <-factor(Browsing_pr$Browsing_presence, levels = c(0:1), labels = c("Absent", "Present"))
Browsing_pr$RASE.presence<- factor(Browsing_pr$RASE.presence, levels = c(0:1), labels = c("Absent", "Present"))

names(Browsing_pr)[names(Browsing_pr) == "Wolf.territory"] <-"Wolf_presence"
names(Browsing_pr)[names(Browsing_pr) == "Old"] <- "Previous_browsing_damage"
names(Browsing_pr)[names(Browsing_pr) == "Pellet.counts"] <- "Moose_pellet_counts"
names(Browsing_pr)[names(Browsing_pr) == "RASE.presence"] <- "RAWO_presence"

-------------------------
###Moose presence models
-------------------------
  
## Wolf presence
Moose_presence <- glmer(Moose_presence~Wolf_presence*Forest_stage + Distance_main_roads + Distance_forest_roads +
              Both.Pine.cover + RAWO_presence + (1|Taxar), data = Moose_pr, family = binomial(link = "logit"), 
            na.action = "na.fail")

## Time since wolf territory establishment
Moose_presence_time <- glmer(Moose_presence~Time_since_terr_establishment*Forest_stage + Distance_main_roads + Distance_forest_roads +
              Pine_proportion + RAWO_presence + (1|Taxar), data = Moose_pr, family = binomial(link = "logit"), 
            na.action = "na.fail")

summary(m1)

## Dredge of models 
Dr_moose_presence_ <- dredge(Moose_presence, rank= "AIC")
Dr_moose_presence_time <- dredge(Moose_presence_time, rank = "AIC")

## Model averaging using MuMIn
## Best models
Av_moose_presence <- model.avg(get.models(Dr_moose_presence, subset = delta <2))
Av_moose_presence_time <- model.avg(get.models(Dr_moose_presence_time, subset = delta<2))

##Intercept and univariate models 
Av_moose_presence_intercept <- model.avg(get.models(Dr_moose_presence, subset = df < 5))
Av_moose_presence_time_intercept <- model.avg(get.models(Dr_moose_presence_time, subset = df < 5))

summary(Av_moose_presence_intercept)
summary(Av_moose_presence_time_intercept)

## R squared for GLMM
r.squaredGLMM(Moose_presence)



----------------------------------
###Browsing damage presence models
----------------------------------
#Wolf presence
Browsing_presence <-glmer(Browsing_damage_presence~Distance_main_roads + RAWO_presence +
                            Distance_forest_roads+ Wolf_presence + Both.Pine.cover +
                            Moose_pellet_counts +Previous_browsing_damage +
                            (1|Taxar), data = Browsing_pr, family = binomial(link = "logit"), na.action = "na.fail")

## Time since territory establishment
Browsing_presence_time <-glmer(Browsing_damage_presence~Distance_main_roads + RAWO_presence +
                            Distance_forest_roads+ Time_since_terr_establishment + Both.Pine.cover +
                            Moose_pellet_counts +Previous_browsing_damage +
                            (1|Taxar), data = Browsing_pr, family = binomial(link = "logit"), na.action = "na.fail")
summary(Browsing_presence)
summary(Browsing_presence_time)

##Dredge
Dr_browsing_presence <- dredge(Browsing_presence, rank="AIC")
Dr_browsing_presence_time<- dredge(Browsing_presence_time, rank = "AIC")
summary(Browsing_presence)
summary(Browsing_presence_time)

##Model averaging using MuMIn
Browsing_presence_av <- model.avg(get.models(Dr_browsing_presence, subset = delta< 2))
Browsing_presence_av_time <- model.avg(get.models(Dr_browsing_presence_time, subset = delta<2))
summary(Browsing_presence_av)
summary(Browsing_presence_av_time)

## R squared for GLMM
r.squaredGLMM(Browsing_presence)

## FIGURES 

set_theme(
  geom.outline.color = "antiquewhite4", 
  theme.font = "serif",
  geom.outline.size = 1, 
  geom.label.size = 2,
  geom.label.color = "grey50",
  title.color = "black", 
  title.size = 1.5, 
  axis.textcolor = "black", 
  base = theme_bw()
)


## Odds ratios for both moose and browsing damage presence 

Odd_moose<- plot_model(Moose_presence, title = "a) Moose presence", sort.est = T, dot.size = 4.5, line.size = 1.5) +  
  theme_sjplot() + font_size(axis_title.x = 14,labels.y = 14, labels.x = 14, title = 14)

Odd_browsing<- plot_model(Browsing_presence, title = "b) Browsing damage presence",  sort.est = T, exponentiate=F, dot.size = 4.5, line.size = 1.5) + 
  theme_sjplot() + font_size(axis_title.x = 14,labels.y = 14, labels.x = 14, title = 14) 

grid.arrange(Odd_moose,Odd_browsing)

###MOOSE PRESENCE PLOTS
## Plots of predicted values 
#Distance forest roads
Plot_forest_roads <- plot_model(Moose_presence, type= "pred", transform, terms = ("Distance_forest_roads [all]"),  auto.label = F,
                         axis.title = c("Distance forest roads [km]", ""),axis.lim = c(0, .63),
                         title = "d)") +theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.title.x =element_text(family = "serif", size = 20))+
  theme(axis.text.x = element_text(family = "serif", size = 16, face = "bold"))+
  theme(axis.text.y = element_text(family = "serif", size = 16, face = "bold"))+
  theme(plot.title = element_text(family = "serif", size = 20, face = "bold"))


#Pine proportion

Plot_pine <- plot_model(Moose_presence, type= "pred", digits=2, terms = ("Pine_proportion [all]"),  auto.label = F,
                        axis.title = c("Pine proportion", ""),axis.lim = c(0, 0.5),
                        title = "c)", wrap.labels = 10)+ theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.title.x =element_text(family = "serif", size = 20))+
  theme(axis.text.x = element_text(family = "serif", size = 16, face = "bold"))+
  theme(axis.text.y = element_text(family = "serif", size = 16, face = "bold"))+
  theme(plot.title = element_text(family = "serif", size = 20, face = "bold"))

Plot_pine


#Forest stage
Plot_forest_stage <- plot_model(Moose_presence, type= "pred", transform, dot.size = 5, line.size = 1.2, terms = ("Forest_stage"), axis.lim = c(0,0.23), 
                                axis.title = c("Forest stage", ""), 
                                title = "e)", digits = 2, wrap.labels = 10)+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.title.x =element_text(family = "serif", size = 20))+
  theme(axis.text.x = element_text(family = "serif", size = 16, face = "bold"))+
  theme(axis.text.y = element_text(family = "serif", size = 16, face = "bold"))+
  theme(plot.title = element_text(family = "serif", size = 20, face = "bold"))


#Wolf presence
Plot_wolf<- plot_model(Moose_presence, type= "pred", transform, terms = ("Wolf_presence"), dot.size = 4, line.size = 1.3, 
                                axis.title = c("Wolf presence", ""))+
  theme_bw() + theme(panel.border = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.title.x =element_text(family = "serif", size = 20))+
  theme(axis.text.x = element_text(family = "serif", size = 16, face = "bold"))+
  theme(axis.text.y = element_text(family = "serif", size = 16, face = "bold"))+
  theme(plot.title = element_text(family = "serif", size = 20, face = "bold"))


#Time since territory establishment
Plot_wolftime<- plot_model(Moose_presence, type= "pred", transform, terms = ("Time_since_terr_establishment"), axis.lim = c(0,0.25), 
                            axis.title = c("Years", ""), 
                            title = "b)") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.title.x =element_text(family = "serif", size = 20))+
  theme(axis.text.x = element_text(family = "serif", size = 16, face = "bold"))+
  theme(axis.text.y = element_text(family = "serif", size = 16, face = "bold"))+
  theme(plot.title = element_text(family = "serif", size = 20, face = "bold"))


grid.arrange(Plot_wolf,Plot_wolftime,Plot_pine,Plot_forest_roads, Plot_forest_stage,
             left = textGrob("Moose Presence", rot = 90, vjust = 1, 
                             gp = gpar(fontfamily = "serif", fontsize = 25, col = 'black')))


###BROWSING DAMAGE PRESENCE PLOTS
##Predicted values 
#Distance from main roads 
Plot_mainroads<- plot_model(Browsing_presence, type= "pred", transform, terms = ("Distance_main_roads[all]"),axis.lim = c(0,0.6),
                            axis.title = c("Distance main roads [km]", ""), 
                            title = "b)")+ theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.title.x =element_text(family = "serif", size = 20))+
  theme(axis.text.x = element_text(family = "serif", size = 16, face = "bold"))+
  theme(axis.text.y = element_text(family = "serif", size = 16, face = "bold"))+
  theme(plot.title = element_text(family = "serif", size = 20, face = "bold"))

#Moose pellet counts
#With grids
Plot_mooseabu <- plot_model(Browsing_presence, type= "pred", transform, terms = ("Moose_pellet_counts[all]"), axis.lim = c(0,1),
                           axis.title = c("Moose abundance", ""), 
                           title = "c)")+ theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.title.x =element_text(family = "serif", size = 20))+
  theme(axis.text.x = element_text(family = "serif", size = 16, face = "bold"))+
  theme(axis.text.y = element_text(family = "serif", size = 16, face = "bold"))+
  theme(plot.title = element_text(family = "serif", size = 20, face = "bold"))

#Previous damage
Plot_previous<- plot_model(Browsing_presence, type= "pred", transform, terms = ("Previous_browsing_damage[all]"),axis.lim = c(0,1),
                           axis.title = c("Previous browsing damage", ""), 
                           title = "d)") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.title.x =element_text(family = "serif", size = 20))+
  theme(axis.text.x = element_text(family = "serif", size = 16, face = "bold"))+
  theme(axis.text.y = element_text(family = "serif", size = 16, face = "bold"))+
  theme(plot.title = element_text(family = "serif", size = 20, face = "bold"))


# Wolf presence
Plot_wolf_browsing<- plot_model(Browsing_presence, type= "pred", transform, terms = ("Wolf_presence"),
                                axis.title = c("Wolf territory", ""), dot.size = 5, line.size = 1.3, axis.lim = c(0,0.15), 
                                title = "a)") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.title.x =element_text(family = "serif", size = 20))+
  theme(axis.text.x = element_text(family = "serif", size = 16, face = "bold"))+
  theme(axis.text.y = element_text(family = "serif", size = 16, face = "bold"))+
  theme(plot.title = element_text(family = "serif", size = 20, face = "bold"))


grid.arrange(Plot_wolf_browsing,Plot_mainroads,Plot_mooseabu, Plot_previous, ncol=2, 
             left = textGrob("Browsing damage presence", rot = 90, vjust = 1, 
                             gp = gpar(fontfamily = "serif", fontsize = 25, col = 'black')))

