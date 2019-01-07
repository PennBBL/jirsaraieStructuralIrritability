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

rds<-readRDS("/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/processedData/longitudinal/n137_Demo+Psych+DX+QA_20180531.rds")
csv<-read.csv("/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/processedData/longitudinal/n137_Nmf24BasesCT_COMBAT_Rate.csv")

suppressMessages(require(lm.beta))
suppressMessages(require(voxel))
suppressMessages(require(mgcv))

######################################
### Center All Responce Variables  ###
######################################

Networks<-csv[,c(1,grep("Ct",names(csv)))]

for (col in names(Networks)) {

if(class(Networks[,col]) == 'integer' | class(Networks[,col]) == 'numeric') {

Networks[,col] <- scale(Networks[,col], center=TRUE, scale=FALSE)
    }
}
Networks$X<-NULL

data<-cbind(rds,Networks)
attach(data)

################################################
### Standardized Betas For TP1 Main Effects  ###
################################################

lm.beta(lm(Ct_Nmf24C7~TP1_ageAtScan1+TP2_sex+DeltaQA+TP1_goassessSmryNCvsDX))

lm.beta(lm(Ct_Nmf24C15~TP1_ageAtScan1+TP2_sex+DeltaQA+TP1_goassessSmryNCvsDX))

lm.beta(lm(Ct_Nmf24C17~TP1_ageAtScan1+TP2_sex+DeltaQA+TP1_goassessSmryNCvsDX))

lm.beta(lm(Ct_Nmf24C22~TP1_ageAtScan1+TP2_sex+DeltaQA+TP1_goassessSmryNCvsDX))

################################################
### Standardized Betas For TP2 Main Effects  ###
################################################

lm.beta(lm(Ct_Nmf24C1~TP1_ageAtScan1+TP2_sex+DeltaQA+TP2_ari_log))

lm.beta(lm(Ct_Nmf24C7~TP1_ageAtScan1+TP2_sex+DeltaQA+TP2_ari_log))

lm.beta(lm(Ct_Nmf24C9~TP1_ageAtScan1+TP2_sex+DeltaQA+TP2_ari_log))

lm.beta(lm(Ct_Nmf24C11~TP1_ageAtScan1+TP2_sex+DeltaQA+TP2_ari_log))

lm.beta(lm(Ct_Nmf24C15~TP1_ageAtScan1+TP2_sex+DeltaQA+TP2_ari_log))

lm.beta(lm(Ct_Nmf24C17~TP1_ageAtScan1+TP2_sex+DeltaQA+TP2_ari_log))

lm.beta(lm(Ct_Nmf24C18~TP1_ageAtScan1+TP2_sex+DeltaQA+TP2_ari_log))

lm.beta(lm(Ct_Nmf24C20~TP1_ageAtScan1+TP2_sex+DeltaQA+TP2_ari_log))

lm.beta(lm(Ct_Nmf24C22~TP1_ageAtScan1+TP2_sex+DeltaQA+TP2_ari_log))

##########################################################################
### Standardized Betas For TP2 Controling for General Psychopathology  ###
##########################################################################

lm.beta(lm(Ct_Nmf24C1~TP1_ageAtScan1+TP2_sex+DeltaQA+TP2_GenPsycho+TP2_ari_log))

lm.beta(lm(Ct_Nmf24C7~TP1_ageAtScan1+TP2_sex+DeltaQA+TP2_GenPsycho+TP2_ari_log))

lm.beta(lm(Ct_Nmf24C9~TP1_ageAtScan1+TP2_sex+DeltaQA+TP2_GenPsycho+TP2_ari_log))

lm.beta(lm(Ct_Nmf24C11~TP1_ageAtScan1+TP2_sex+DeltaQA+TP2_GenPsycho+TP2_ari_log))

lm.beta(lm(Ct_Nmf24C15~TP1_ageAtScan1+TP2_sex+DeltaQA+TP2_GenPsycho+TP2_ari_log))

lm.beta(lm(Ct_Nmf24C17~TP1_ageAtScan1+TP2_sex+DeltaQA+TP2_GenPsycho+TP2_ari_log))

lm.beta(lm(Ct_Nmf24C18~TP1_ageAtScan1+TP2_sex+DeltaQA+TP2_GenPsycho+TP2_ari_log))

lm.beta(lm(Ct_Nmf24C20~TP1_ageAtScan1+TP2_sex+DeltaQA+TP2_GenPsycho+TP2_ari_log))

lm.beta(lm(Ct_Nmf24C22~TP1_ageAtScan1+TP2_sex+DeltaQA+TP2_GenPsycho+TP2_ari_log))

###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################