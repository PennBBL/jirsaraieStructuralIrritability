###################################################################################################
##########################             GRMPY - Create Tables             ##########################
##########################              Robert Jirsaraie                 ##########################
##########################             rjirsaraie@upenn.edu              ##########################
##########################                  09/24/2018                   ##########################
###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################

###############################################
### Read in Data and Load Relevent Packages ###
###############################################

rds<-readRDS("/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/processedData/longitudinal/n137_Demo+Psych+DX+QA_20180531.rds")

library(effsize)
library(dplyr)
library(lsr)

#####################################################
### Create Datasets Seperated by Diagnosis Groups ###
#####################################################

NC <- rds[which(rds$TP1_IrritabilitySum==0),]
dim(NC)
DX <- rds[which(rds$TP1_IrritabilitySum>=1),]
dim(DX)

rds$TP1_IrritabilityGrp<-9999
rds[,c('TP1_IrritabilityGrp')]<-ifelse(rds[,c('TP1_IrritabilitySum')] == 0, 0, ifelse(rds[,c('TP1_IrritabilitySum')] >= 1, 1, 999))
rds$TP1_IrritabilityGrp<-as.factor(rds$TP1_IrritabilityGrp)

###########################################
### Aanlyze Differences in Baseline Age ###
###########################################

summary(NC$TP1_ageAtScan1)
sd(NC$TP1_ageAtScan1)
summary(DX$TP1_ageAtScan1)
sd(DX$TP1_ageAtScan1)

t.test(rds$TP1_ageAtScan1~rds$TP1_IrritabilityGrp)

############################################
### Aanlyze Differences in Follow Up Age ###
############################################

summary(NC$TP2_ScanAgeYears)
sd(NC$TP2_ScanAgeYears)
summary(DX$TP2_ScanAgeYears)
sd(DX$TP2_ScanAgeYears)

t.test(rds$TP2_ScanAgeYears~rds$TP1_IrritabilityGrp)

############################################
### Aanlyze Differences in Sex per Group ###
############################################

summary(NC$TP2_sex)
summary(DX$TP2_sex)

chisq.test(table(rds$TP1_IrritabilityGrp,rds$TP2_sex))

###############################################
### Aanlyze Differences in Father Education ###
###############################################

summary(NC$TP2_dad_educ)
sd(NC$TP2_dad_educ, na.rm=TRUE)
summary(DX$TP2_dad_educ)
sd(DX$TP2_dad_educ, na.rm=TRUE)

t.test(rds$TP2_dad_educ~rds$TP1_IrritabilityGrp)

###############################################
### Aanlyze Differences in Mother Education ###
###############################################

summary(NC$TP2_mom_educ)
sd(NC$TP2_mom_educ, na.rm=TRUE)
summary(DX$TP2_mom_educ)
sd(DX$TP2_mom_educ, na.rm=TRUE)

t.test(rds$TP2_mom_educ~rds$TP1_IrritabilityGrp)

#####################################################
### Aanlyze Differences in Follow-up Irritability ###
#####################################################

summary(NC$TP2_ari_total)
sd(NC$TP2_ari_total, na.rm=TRUE)
summary(DX$TP2_ari_total)
sd(DX$TP2_ari_total, na.rm=TRUE)

t.test(rds$TP2_ari_total~rds$TP1_IrritabilityGrp)

###################################################
### Aanlyze Differences in Follow-up Depression ###
###################################################

summary(NC$TP2_bdi_total)
sd(NC$TP2_bdi_total, na.rm=TRUE)
summary(DX$TP2_bdi_total)
sd(DX$TP2_bdi_total, na.rm=TRUE)

t.test(rds$TP2_bdi_total~rds$TP1_IrritabilityGrp)

#####################################################
### Aanlyze Differences in Followup ADHD Symptoms ###
#####################################################

summary(NC$TP2_adhd_total)
sd(NC$TP2_adhd_total, na.rm=TRUE)
summary(DX$TP2_adhd_total)
sd(DX$TP2_adhd_total, na.rm=TRUE)

t.test(rds$TP2_adhd_total~rds$TP1_IrritabilityGrp)

########################################################
### Aanlyze Differences in Followup Anxiety Symptoms ###
########################################################

summary(NC$TP2_scared_total)
sd(NC$TP2_scared_total, na.rm=TRUE)
summary(DX$TP2_scared_total)
sd(DX$TP2_scared_total, na.rm=TRUE)

t.test(rds$TP2_scared_total~rds$TP1_IrritabilityGrp)

########################################################
### Aanlyze Differences in Baseline Diagnosis Status ###
########################################################

Diagnosed<-length(which(NC$TP1_goassessSmryNCvsDX==1))
Diagnosed/dim(NC)[1]

Diagnosed<-length(which(DX$TP1_goassessSmryNCvsDX==1))
Diagnosed/dim(DX)[1]

chisq.test(table(rds$TP1_IrritabilityGrp,rds$TP1_goassessSmryNCvsDX))

#########################################################
### Aanlyze Differences in Follow-up Diagnosis Status ###
#########################################################

Diagnosed<-length(which(NC$TP2_dx_NCvsDX==1))
Diagnosed/dim(NC)[1]

Diagnosed<-length(which(DX$TP2_dx_NCvsDX==1))
Diagnosed/dim(DX)[1]

chisq.test(table(rds$TP1_IrritabilityGrp,rds$TP2_dx_NCvsDX))

###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################
