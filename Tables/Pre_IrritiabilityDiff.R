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

###############################################################
### Correlation of Irritability Measures Between Timepoints ###
###############################################################

cor(TP1_IrritabilitySum,TP2_ari_log, use='complete.obs') #0.18
summary(lm(TP1_IrritabilitySum~TP2_ari_log)) #p=0.03

#########################################################
### Baseline Irritability Screening with Demographics ###
#########################################################

summary(lm(TP1_IrritabilitySum~TP1_ageAtScan1)) #p=0.09
t.test(TP1_IrritabilitySum~TP1_sex) #p=0.04
summary(lm(TP1_IrritabilitySum~TP1_edu1)) #p=0.29
summary(lm(TP1_IrritabilitySum~TP1_fedu1)) #p=0.09
summary(lm(TP1_IrritabilitySum~TP1_medu1)) #p=0.03
summary(lm(TP1_IrritabilitySum~TP1_averageManualRating)) #p=0.64

### Further Analyses on Sex-Differences ###

cohen.d(TP1_IrritabilitySum,TP1_sex, na.rm=TRUE) #d=0.37
males<-data[which(data$TP1_sex == 1),] 
summary(males$TP1_IrritabilitySum) #M=1.3
sd(males$TP1_IrritabilitySum, na.rm=TRUE) #SD=1.2
females<-data[which(data$TP1_sex == 2),]
summary(females$TP1_IrritabilitySum) #M=1.8
sd(females$TP1_IrritabilitySum, na.rm=TRUE) #SD=1.4

### Further Analyses on Maternal Education Differences ###

cor(TP1_IrritabilitySum,TP1_medu1, use='complete.obs')

######################################################
### Baseline Irritability Screening with Diagnoses ###
######################################################

# Create a Multi-Level Factor Variable Where Each Level Represents A Unique Disorder

data$TP1_DX_SPECIFIC<-0
data$TP1_DX_SPECIFIC[data$TP1_goassessSmryAnx==1] <- 1 #Level 1: Anxiety Disorders
data$TP1_DX_SPECIFIC[data$TP1_goassessSmryAdd==1] <- 2 # Level 2: ADHD Disorders
data$TP1_DX_SPECIFIC[data$TP1_goassessSmryMan==1] <- 3 # Level 3: Bipolar Disorders
data$TP1_DX_SPECIFIC[data$TP1_goassessSmryPsy==1] <- 4 # Level 4: Psychotic Disorders
data$TP1_DX_SPECIFIC[data$TP1_goassessSmryDep==1] <- 5 #Level 5: Major Depression
data$TP1_DX_SPECIFIC[data$TP1_goassessSmryOdd==1] <- 6 #Level 6: Oppositional Defiant
data$TP1_DX_SPECIFIC[data$TP1_goassessSmryCon==1] <- 7 #Level 7: Conduct Disorder
data$TP1_goassessSmrySum<-as.numeric(data$TP1_goassessSmrySum) 
data$TP1_DX_SPECIFIC[data$TP1_goassessSmrySum>=3] <- 8 #Level 8: Comorbid Disorders
data$TP1_goassessSmrySum<-as.factor(data$TP1_goassessSmrySum)

data$TP1_DX_SPECIFIC[data$TP1_DX_SPECIFIC == 0] <- NA #Remove Healthy Controls
data$TP1_DX_SPECIFIC[data$TP1_DX_SPECIFIC == 8] <- NA #Remove Comorbid Disorders
data$TP1_DX_SPECIFIC<-as.factor(data$TP1_DX_SPECIFIC)

summary(data$TP1_DX_SPECIFIC) #Check Numbers Per Diagnosis

summary(aov(data$TP1_IrritabilitySum~data$TP1_DX_SPECIFIC)) #p=0.16
pairwise.t.test(data$TP1_IrritabilitySum, data$TP1_DX_SPECIFIC, p.adj = "none")

# Null Results Even When Removing Groups with Small Sample Sizes (ADHD+Bipolar+Psychosis)

#######################################################
### Follow-up Irritability Scores with Demographics ###
#######################################################

summary(lm(TP2_ari_log~TP2_ScanAgeYears)) #p=0.66
t.test(TP2_ari_log~TP2_sex) #p=0.43
summary(lm(TP2_ari_log~TP2_educ)) #p=0.49
summary(lm(TP2_ari_log~TP2_dad_educ)) #p=0.16
summary(lm(TP2_ari_log~TP2_mom_educ)) #p=0.87
summary(lm(TP2_ari_log~TP2_rating)) #p=0.34

#######################################################
### Follow-up Irritability Scores with Demographics ###
#######################################################

# Create a Multi-Level Factor Variable Where Each Level Represents A Unique Disorder

data$TP2_DX_SPECIFIC<-0
data$TP2_DX_SPECIFIC[data$TP2_dx_anx==1] <- 1 #Level 1: Anxiety Disorders
data$TP2_DX_SPECIFIC[data$TP2_dx_adhd==1] <- 2 # Level 2: ADHD Disorders
data$TP2_DX_SPECIFIC[data$TP2_dx_Bipolar==1] <- 3 # Level 3: Bipolar Disorders
data$TP2_DX_SPECIFIC[data$TP2_dx_psychotic==1] <- 4 # Level 4: Psychotic Disorders
data$TP2_DX_SPECIFIC[data$TP2_dx_mdd==1] <- 5 #Level 5: Major Depression
data$TP2_DX_SPECIFIC[data$TP2_dx_SubstanceUse==1] <- 6 #Level 6: Substance Abuse
data$TP2_DX_SPECIFIC[data$TP2_dx_Sum==2] <- 7 #Level 7: Comorbid Disorders
data$TP2_DX_SPECIFIC[data$TP2_dx_Sum==3] <- 7 
data$TP2_DX_SPECIFIC[data$TP2_DX_SPECIFIC == 0] <- NA #Remove Healthy Controls
data$TP2_DX_SPECIFIC[data$TP2_DX_SPECIFIC == 7] <- NA #Remove Comorbid Disorders
data$TP2_DX_SPECIFIC<-as.factor(data$TP2_DX_SPECIFIC)

summary(data$TP2_DX_SPECIFIC) #Check Numbers Per Diagnosis

summary(aov(data$TP2_ari_log~data$TP2_DX_SPECIFIC)) #p=0.43
pairwise.t.test(data$TP2_ari_log, data$TP2_DX_SPECIFIC, p.adj = "none")

###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################