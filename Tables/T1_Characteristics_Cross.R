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

NC <- rds[which(rds$TP1_goassessSmryNCvsDX==0),]
dim(NC)
DX <- rds[which(rds$TP1_goassessSmryNCvsDX==1),]
dim(DX)

###########################################
### Aanlyze Differences in Baseline Age ###
###########################################

summary(NC$TP1_ageAtScan1)
sd(NC$TP1_ageAtScan1)
summary(DX$TP1_ageAtScan1)
sd(DX$TP1_ageAtScan1)

summary(aov(rds$TP1_ageAtScan1~rds$TP1_goassessSmryNCvsDX)) #0.009
cohen.d(rds$TP1_ageAtScan1, rds$TP1_goassessSmryNCvsDX, paired=FALSE) #0.45

############################################
### Aanlyze Differences in Follow Up Age ###
############################################

summary(NC$TP2_ScanAgeYears)
sd(NC$TP2_ScanAgeYears)
summary(DX$TP2_ScanAgeYears)
sd(DX$TP2_ScanAgeYears)

summary(aov(rds$TP2_ScanAgeYears~rds$TP1_goassessSmryNCvsDX)) #0.006
cohen.d(rds$TP2_ScanAgeYears, rds$TP1_goassessSmryNCvsDX, paired=FALSE) #0.47

############################################
### Aanlyze Differences in Sex per Group ###
############################################

summary(NC$TP2_sex)
summary(DX$TP2_sex)

chisq.test(table(rds$TP1_goassessSmryNCvsDX,rds$TP2_sex)) #0.83

###############################################
### Aanlyze Differences in Father Education ###
###############################################

summary(NC$TP2_dad_educ)
sd(NC$TP2_dad_educ, na.rm=TRUE)
summary(DX$TP2_dad_educ)
sd(DX$TP2_dad_educ, na.rm=TRUE)

summary(aov(rds$TP2_dad_educ~rds$TP1_goassessSmryNCvsDX)) #0.13
cohen.d(rds$TP2_dad_educ, rds$TP1_goassessSmryNCvsDX, paired=FALSE, na.rm=TRUE) #0.28

###############################################
### Aanlyze Differences in Mother Education ###
###############################################

summary(NC$TP2_mom_educ)
sd(NC$TP2_mom_educ, na.rm=TRUE)
summary(DX$TP2_mom_educ)
sd(DX$TP2_mom_educ, na.rm=TRUE)

summary(aov(rds$TP2_mom_educ~rds$TP1_goassessSmryNCvsDX)) #0.01
cohen.d(rds$TP2_mom_edu, rds$TP1_goassessSmryNCvsDX, paired=FALSE, na.rm=TRUE)  #0.043

#####################################################
### Aanlyze Differences in Follow-up Irritability ###
#####################################################

summary(NC$TP2_ari_total)
sd(NC$TP2_ari_total, na.rm=TRUE)
summary(DX$TP2_ari_total)
sd(DX$TP2_ari_total, na.rm=TRUE)

summary(aov(rds$TP2_ari_total~rds$TP1_goassessSmryNCvsDX)) #0.18
cohen.d(rds$TP2_ari_total, rds$TP1_goassessSmryNCvsDX, paired=FALSE, na.rm=TRUE) #0.23

###################################################
### Aanlyze Differences in Follow-up Depression ###
###################################################

summary(NC$TP2_bdi_total)
sd(NC$TP2_bdi_total, na.rm=TRUE)
summary(DX$TP2_bdi_total)
sd(DX$TP2_bdi_total, na.rm=TRUE)

t.test(rds$TP2_bdi_total~rds$TP1_goassessSmryNCvsDX) #0.11
cohen.d(rds$TP2_bdi_total, rds$TP1_goassessSmryNCvsDX, paired=FALSE, na.rm=TRUE)  #0.29

#####################################################
### Aanlyze Differences in Followup ADHD Symptoms ###
#####################################################

summary(NC$TP2_adhd_total)
sd(NC$TP2_adhd_total, na.rm=TRUE)
summary(DX$TP2_adhd_total)
sd(DX$TP2_adhd_total, na.rm=TRUE)

summary(aov(rds$TP2_adhd_total~rds$TP1_goassessSmryNCvsDX)) #0.004
cohen.d(rds$TP2_adhd_total, rds$TP1_goassessSmryNCvsDX, paired=FALSE, na.rm=TRUE) #0.62

########################################################
### Aanlyze Differences in Followup Anxiety Symptoms ###
########################################################

summary(NC$TP2_scared_total)
sd(NC$TP2_scared_total, na.rm=TRUE)
summary(DX$TP2_scared_total)
sd(DX$TP2_scared_total, na.rm=TRUE)

summary(aov(rds$TP2_scared_total~rds$TP1_goassessSmryNCvsDX)) #0.003
cohen.d(rds$TP2_scared_total, rds$TP1_goassessSmryNCvsDX, paired=FALSE, na.rm=TRUE) #0.64

############################################
### Aanlyze Differences in Sex per Group ###
############################################

summary(NC$TP2_dx_NCvsDX)
34/48
summary(DX$TP2_dx_NCvsDX)
44/89

chisq.test(table(rds$TP1_goassessSmryNCvsDX,rds$TP2_dx_NCvsDX)) #0.83

###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################
