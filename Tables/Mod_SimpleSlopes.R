###################################################################################################
##########################        GRMPY - Simple Slope Analyses          ##########################
##########################              Robert Jirsaraie                 ##########################
##########################             rjirsaraie@upenn.edu              ##########################
##########################                  01/14/2019                   ##########################
###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################

#################################################################
##### Reads in Data from Follow-up & Load Relevant Packages #####
#################################################################

csv <- read.csv("/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/processedData/follow-up/n141_Nmf24BasesCT_COMBAT_TP2.csv")
rds <- readRDS("/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/processedData/follow-up/n141_Demo+Psych+DX+QA_20180724.rds")

data<-merge(csv,rds, by=c('bblid','scanid'))
attach(data)

library(moments)
library(lm.beta)
library(jtools)

################################################
##### Compute Simple Slope Analyses on Age #####
################################################

m1<-lm(Ct_Nmf24C6~ScanAgeYears+sex+rating+ari_log*ScanAgeYears)
summary(m1)
sim_slopes(model = m1, pred = ari_log, modx = ScanAgeYears)
interact_plot(m1, pred = "ari_log", modx = "ScanAgeYears", data = data)

#############################################################
##### Compute Simple Slope Analyses on Diagnosis Status #####
#############################################################

m2<-lm(Ct_Nmf24C22~ScanAgeYears+sex+rating+ari_log*dx_NCvsDX)
summary(m2)
sim_slopes(model = m2, pred = ari_log, modx = dx_NCvsDX)
interact_plot(m2, pred = "ari_log", modx = "dx_NCvsDX", data = data)

###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################