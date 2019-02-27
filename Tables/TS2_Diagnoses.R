###################################################################################################
##########################             GRMPY - Create Tables             ##########################
##########################              Robert Jirsaraie                 ##########################
##########################             rjirsaraie@upenn.edu              ##########################
##########################                  09/24/2018                   ##########################
###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################

#######################################
### Load Data and Analysis Packages ###
#######################################

rds<-readRDS("/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/processedData/longitudinal/n137_Demo+Psych+DX+QA_20180531.rds")

library(effsize)
library(dplyr)

#######################################################################
### Calculate Number of Specific and Multiple Diganoses at Baseline ###
#######################################################################

DXtp1<-rds[,c(grep('TP1_goassessSmry', colnames(rds)))]
summary(DXtp1) #1 = # of People with a given disorder
DXtp1$TP1_ComorbidDisorders<-as.numeric(DXtp1$TP1_goassessSmrySum)
length(which(DXtp1$TP1_ComorbidDisorders >= 3)) #See # participants with multiple disorders

########################################
### Deliniate the Comorbid Disorders ###
########################################

COMORBID <- DXtp1[which(DXtp1$TP1_ComorbidDisorders>=3),]
summary(COMORBID) #1 = # of People with comorbid disorders within specific disorders

########################################################################
### Calculate Number of Specific and Multiple Diganoses at Follow-up ###
########################################################################

DXtp2<-rds[,c(grep('TP2_dx_', colnames(rds)))]
summary(DXtp2) #1 = # of People with a given disorder
DXtp2$TP2_ComorbidDisorders<-as.numeric(DXtp2$TP2_dx_Sum)
length(which(DXtp2$TP2_ComorbidDisorders >= 3)) #See # participants with multiple disorders

########################################
### Deliniate the Comorbid Disorders ###
########################################

COMORBID <- DXtp2[which(DXtp2$TP2_ComorbidDisorders>=3),]
summary(COMORBID) #1 = # of People with comorbid disorders within specific disorders

###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################