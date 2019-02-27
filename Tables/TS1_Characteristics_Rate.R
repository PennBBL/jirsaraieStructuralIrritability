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

NC <- rds[which(rds$TP1_goassessSmryNCvsDX ==0 & rds$TP2_dx_NCvsDX ==0),]
dim(NC)
DX <- rds[which(rds$TP1_goassessSmryNCvsDX ==1 & rds$TP2_dx_NCvsDX ==1),]
dim(DX)
Resilient <- rds[which(rds$TP1_goassessSmryNCvsDX ==1 & rds$TP2_dx_NCvsDX ==0),]
dim(Resilient)
Emerge <- rds[which(rds$TP1_goassessSmryNCvsDX ==0 & rds$TP2_dx_NCvsDX == 1),]
dim(Emerge)

###########################################
### Aanlyze Differences in Baseline Age ###
###########################################

summary(NC$TP1_ageAtScan1)[4]
sd(NC$TP1_ageAtScan1)
summary(DX$TP1_ageAtScan1)[4]
sd(DX$TP1_ageAtScan1)
summary(Resilient$TP1_ageAtScan1)[4]
sd(Resilient$TP1_ageAtScan1)
summary(Emerge$TP1_ageAtScan1)[4]
sd(Emerge$TP1_ageAtScan1)

summary(aov(rds$TP1_ageAtScan1~rds$DeltaDiagnosisGroup))

############################################
### Aanlyze Differences in Follow Up Age ###
############################################

summary(NC$TP2_ScanAgeYears)[4]
sd(NC$TP2_ScanAgeYears)
summary(DX$TP2_ScanAgeYears)[4]
sd(DX$TP2_ScanAgeYears)
summary(Resilient$TP2_ScanAgeYears)[4]
sd(Resilient$TP2_ScanAgeYears)
summary(Emerge$TP2_ScanAgeYears)[4]
sd(Emerge$TP2_ScanAgeYears)

summary(aov(rds$TP2_ScanAgeYears~rds$DeltaDiagnosisGroup))

############################################
### Aanlyze Differences in Sex per Group ###
############################################

summary(NC$TP2_sex)
summary(DX$TP2_sex)
summary(Resilient$TP2_sex)
summary(Emerge$TP2_sex)

chisq.test(table(rds$DeltaDiagnosisGroup,rds$TP2_sex))

###############################################
### Aanlyze Differences in Father Education ###
###############################################

summary(NC$TP2_dad_educ)[4]
sd(NC$TP2_dad_educ, na.rm=TRUE)
summary(DX$TP2_dad_educ)[4]
sd(DX$TP2_dad_educ, na.rm=TRUE)
summary(Resilient$TP2_dad_educ)[4]
sd(Resilient$TP2_dad_educ, na.rm=TRUE)
summary(Emerge$TP2_dad_educ)[4]
sd(Emerge$TP2_dad_educ, na.rm=TRUE)

summary(aov(rds$TP2_dad_educ~rds$DeltaDiagnosisGroup))

###############################################
### Aanlyze Differences in Mother Education ###
###############################################

summary(NC$TP2_mom_educ)[4]
sd(NC$TP2_mom_educ, na.rm=TRUE)
summary(DX$TP2_mom_educ)[4]
sd(DX$TP2_mom_educ, na.rm=TRUE)
summary(Resilient$TP2_mom_educ)[4]
sd(Resilient$TP2_mom_educ, na.rm=TRUE)
summary(Emerge$TP2_mom_educ)[4]
sd(Emerge$TP2_mom_educ, na.rm=TRUE)

summary(aov(rds$TP2_mom_educ~rds$DeltaDiagnosisGroup))

#####################################################
### Aanlyze Differences in Follow-up Irritability ###
#####################################################

summary(NC$TP2_ari_total)[4]
sd(NC$TP2_ari_total, na.rm=TRUE)
summary(DX$TP2_ari_total)[4]
sd(DX$TP2_ari_total, na.rm=TRUE)
summary(Resilient$TP2_ari_total)[4]
sd(Resilient$TP2_ari_total, na.rm=TRUE)
summary(Emerge$TP2_ari_total)[4]
sd(Emerge$TP2_ari_total, na.rm=TRUE)

summary(aov(rds$TP2_ari_total~rds$DeltaDiagnosisGroup))

###################################################
### Aanlyze Differences in Follow-up Depression ###
###################################################

summary(NC$TP2_bdi_total)[4]
sd(NC$TP2_bdi_total, na.rm=TRUE)
summary(DX$TP2_bdi_total)[4]
sd(DX$TP2_bdi_total, na.rm=TRUE)
summary(Resilient$TP2_bdi_total)[4]
sd(Resilient$TP2_bdi_total, na.rm=TRUE)
summary(Emerge$TP2_bdi_total)[4]
sd(Emerge$TP2_bdi_total, na.rm=TRUE)

summary(aov(rds$TP2_bdi_total~rds$DeltaDiagnosisGroup))

#####################################################
### Aanlyze Differences in Followup ADHD Symptoms ###
#####################################################

summary(NC$TP2_adhd_total)[4]
sd(NC$TP2_adhd_total, na.rm=TRUE)
summary(DX$TP2_adhd_total)[4]
sd(DX$TP2_adhd_total, na.rm=TRUE)
summary(Resilient$TP2_adhd_total)[4]
sd(Resilient$TP2_adhd_total, na.rm=TRUE)
summary(Emerge$TP2_adhd_total)[4]
sd(Emerge$TP2_adhd_total, na.rm=TRUE)

summary(aov(rds$TP2_adhd_total~rds$DeltaDiagnosisGroup))

########################################################
### Aanlyze Differences in Followup Anxiety Symptoms ###
########################################################

summary(NC$TP2_scared_total)[4]
sd(NC$TP2_scared_total, na.rm=TRUE)
summary(DX$TP2_scared_total)[4]
sd(DX$TP2_scared_total, na.rm=TRUE)
summary(Resilient$TP2_scared_total)[4]
sd(Resilient$TP2_scared_total, na.rm=TRUE)
summary(Emerge$TP2_scared_total)[4]
sd(Emerge$TP2_scared_total, na.rm=TRUE)

summary(aov(rds$TP2_scared_total~rds$DeltaDiagnosisGroup))

###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################
