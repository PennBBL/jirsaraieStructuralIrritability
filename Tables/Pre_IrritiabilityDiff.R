###################################################################################################
##########################          GRMPY - Preliminary Results          ##########################
##########################              Robert Jirsaraie                 ##########################
##########################             rjirsaraie@upenn.edu              ##########################
##########################                  01/01/2019                   ##########################
###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################

###############################################
### Read in Data and Load Relevent Packages ###
###############################################

data<-readRDS("/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/processedData/longitudinal/n137_Demo+Psych+DX+QA_20180531.rds")

attach(data)
library(lm.beta)
library(effsize)

#######################################################
### Follow-up Irritability Scores with Demographics ###
#######################################################

summary(lm(TP2_ari_log~TP2_ScanAgeYears)) #p=0.66
t.test(TP2_ari_log~TP2_sex) #p=0.43
summary(lm(TP2_ari_log~TP2_educ)) #p=0.49
summary(lm(TP2_ari_log~TP2_dad_educ)) #p=0.17
summary(lm(TP2_ari_log~TP2_mom_educ)) #p=0.87
summary(lm(TP2_ari_log~TP2_rating)) #p=0.34

#######################################################
### Follow-up Irritability Scores with Demographics ###
#######################################################

# Analyze within Psychiatric Groups Only

data$TP2_DX_SPECIFIC<-0
data$TP2_DX_SPECIFIC[data$TP2_dx_anx==1] <- 1 #Level 1: Anxiety Disorders
data$TP2_DX_SPECIFIC[data$TP2_dx_adhd==1] <- 2 # Level 2: ADHD Disorders
data$TP2_DX_SPECIFIC[data$TP2_dx_Bipolar==1] <- 3 # Level 3: Bipolar Disorders
data$TP2_DX_SPECIFIC[data$TP2_dx_psychotic==1] <- 3 # Level 3: Psychotic Disorders
data$TP2_DX_SPECIFIC[data$TP2_dx_mdd==1] <- 4 #Level 5: Major Depression
data$TP2_DX_SPECIFIC[data$TP2_dx_Sum==2] <- 5 #Level 6: Comorbid Disorders
data$TP2_DX_SPECIFIC[data$TP2_dx_Sum==3] <- 5 
data$TP2_DX_SPECIFIC[data$TP2_DX_SPECIFIC == 0] <- NA #Remove Healthy Controls
data$TP2_DX_SPECIFIC[data$TP2_DX_SPECIFIC == 5] <- NA #Remove Comorbid Disorders
data$TP2_DX_SPECIFIC<-as.factor(data$TP2_DX_SPECIFIC)

summary(data$TP2_DX_SPECIFIC) #Check Numbers Per Diagnosis

summary(aov(data$TP2_ari_log~data$TP2_DX_SPECIFIC))
pairwise.t.test(data$TP2_ari_log, data$TP2_DX_SPECIFIC, p.adj = "none")

# Analyze within Psychiatric Groups and Typically Developing

data$TP2_DX_SPECIFIC<-0
data$TP2_DX_SPECIFIC[data$TP2_dx_anx==1] <- 1 #Level 1: Anxiety Disorders
data$TP2_DX_SPECIFIC[data$TP2_dx_adhd==1] <- 2 # Level 2: ADHD Disorders
data$TP2_DX_SPECIFIC[data$TP2_dx_Bipolar==1] <- 3 # Level 3: Bipolar Disorders
data$TP2_DX_SPECIFIC[data$TP2_dx_psychotic==1] <- 3 # Level 4: Psychotic Disorders
data$TP2_DX_SPECIFIC[data$TP2_dx_mdd==1] <- 5 #Level 5: Major Depression
data$TP2_DX_SPECIFIC[data$TP2_dx_Sum==2] <- 6 #Level 6: Comorbid Disorders
data$TP2_DX_SPECIFIC[data$TP2_dx_Sum==3] <- 6 
#data$TP2_DX_SPECIFIC[data$TP2_DX_SPECIFIC == 0] <- NA #Remove Healthy Controls
data$TP2_DX_SPECIFIC[data$TP2_DX_SPECIFIC == 6] <- NA #Remove Comorbid Disorders
data$TP2_DX_SPECIFIC<-as.factor(data$TP2_DX_SPECIFIC)

summary(data$TP2_DX_SPECIFIC) #Check Numbers Per Diagnosis

summary(aov(data$TP2_ari_log~data$TP2_DX_SPECIFIC)) #p=0.04
pairwise.t.test(data$TP2_ari_log, data$TP2_DX_SPECIFIC, p.adj = "none")

#######################################################
### Follow-up Irritability Scores with Demographics ###
#######################################################

# Analyze within Psychiatric Groups Only

data$TP1_DX_SPECIFIC<-0
data$TP1_DX_SPECIFIC[data$TP1_goassessSmryAnx==1] <- 1 #Level 1: Anxiety Disorders
data$TP1_DX_SPECIFIC[data$TP1_goassessSmryAdd==1] <- 2 # Level 2: ADHD Disorders
data$TP1_DX_SPECIFIC[data$TP1_goassessSmryMan==1] <- 3 # Level 3: Bipolar Disorders
data$TP1_DX_SPECIFIC[data$TP1_goassessSmryPsy==1] <- 4 # Level 4: Psychotic Disorders
data$TP1_DX_SPECIFIC[data$TP1_goassessSmryDep==1] <- 5 #Level 5: Major Depression
data$TP1_goassessSmrySum<-as.numeric(data$TP1_goassessSmrySum) 
data$TP1_DX_SPECIFIC[data$TP1_goassessSmrySum>=3] <- 6 #Level 6: Comorbid Disorders
data$TP1_goassessSmrySum<-as.factor(data$TP1_goassessSmrySum)

data$TP1_DX_SPECIFIC[data$TP1_DX_SPECIFIC == 0] <- NA #Remove Healthy Controls
data$TP1_DX_SPECIFIC[data$TP1_DX_SPECIFIC == 6] <- NA #Remove Comorbid Disorders
data$TP1_DX_SPECIFIC<-as.factor(data$TP1_DX_SPECIFIC)

summary(data$TP1_DX_SPECIFIC) #Check Numbers Per Diagnosis

summary(aov(data$TP1_IrritabilitySum~data$TP1_DX_SPECIFIC)) #p=0.37
pairwise.t.test(data$TP1_IrritabilitySum, data$TP1_DX_SPECIFIC, p.adj = "none")

# Analyze within Psychiatric Groups and Typically Developing

data$TP1_DX_SPECIFIC<-0
data$TP1_DX_SPECIFIC[data$TP1_goassessSmryAnx==1] <- 1 #Level 1: Anxiety Disorders
data$TP1_DX_SPECIFIC[data$TP1_goassessSmryAdd==1] <- 2 # Level 2: ADHD Disorders
data$TP1_DX_SPECIFIC[data$TP1_goassessSmryMan==1] <- 3 # Level 3: Bipolar Disorders
data$TP1_DX_SPECIFIC[data$TP1_goassessSmryPsy==1] <- 4 # Level 4: Psychotic Disorders
data$TP1_DX_SPECIFIC[data$TP1_goassessSmryDep==1] <- 5 #Level 5: Major Depression
data$TP1_goassessSmrySum<-as.numeric(data$TP1_goassessSmrySum) 
data$TP1_DX_SPECIFIC[data$TP1_goassessSmrySum>=3] <- 6 #Level 6: Comorbid Disorders
data$TP1_goassessSmrySum<-as.factor(data$TP1_goassessSmrySum)

#data$TP1_DX_SPECIFIC[data$TP1_DX_SPECIFIC == 0] <- NA #Remove Healthy Controls
data$TP1_DX_SPECIFIC[data$TP1_DX_SPECIFIC == 6] <- NA #Remove Comorbid Disorders
data$TP1_DX_SPECIFIC<-as.factor(data$TP1_DX_SPECIFIC)

summary(data$TP1_DX_SPECIFIC) #Check Numbers Per Diagnosis

summary(aov(data$TP1_IrritabilitySum~data$TP1_DX_SPECIFIC)) #p<-0.001
pairwise.t.test(data$TP1_IrritabilitySum, data$TP1_DX_SPECIFIC, p.adj = "none")

###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################