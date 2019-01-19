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

rdsTP1<-readRDS("/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/processedData/baseline/n140_Demo+Psych+DX+QA_20180531.rds")
rdsTP1<-rdsTP1[order(rdsTP1$bblid),]
csvTP1<-read.csv("/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/processedData/baseline/n140_Nmf4SIGBasesCT_COMBAT_TP1.csv")
csvTP1<-csvTP1[order(csvTP1$bblid),]

rdsTP2<-readRDS("/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/processedData/follow-up/n141_Demo+Psych+DX+QA_20180724.rds")
rdsTP2<-rdsTP2[order(rdsTP2$bblid),]
csvTP2<-read.csv("/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/processedData/follow-up/n141_Nmf11SIGBasesCT_COMBAT_TP2.csv")
csvTP2<-csvTP2[order(csvTP2$bblid),]

suppressMessages(require(lm.beta))
suppressMessages(require(voxel))
suppressMessages(require(mgcv))

######################################
### Center All Responce Variables  ###
######################################

### Timepoint 1 ###

NetworksTP1<-csvTP1[,c(1,grep("Ct",names(csvTP1)))]

for (col in names(NetworksTP1)) {

if(class(NetworksTP1[,col]) == 'integer' | class(NetworksTP1[,col]) == 'numeric') {

NetworksTP1[,col] <- scale(NetworksTP1[,col], center=TRUE, scale=FALSE)
    }
}
NetworksTP1$X<-NULL

data1<-cbind(rdsTP1,NetworksTP1)

### Timepoint 2 ###

NetworksTP2<-csvTP2[,c(1,grep("Ct",names(csvTP2)))]

for (col in names(NetworksTP2)) {

if(class(NetworksTP2[,col]) == 'integer' | class(NetworksTP2[,col]) == 'numeric') {

NetworksTP2[,col] <- scale(NetworksTP2[,col], center=TRUE, scale=FALSE)
    }
}
NetworksTP2$X<-NULL

data2<-cbind(rdsTP2,NetworksTP2)

################################################
### Standardized Betas For TP1 Main Effects  ###
################################################

attach(data1)

lm.beta(lm(Ct_Nmf24C2~ageAtScan1+sex+averageManualRating+ari_log_FOLLOWUP))

lm.beta(lm(Ct_Nmf24C13~ageAtScan1+sex+averageManualRating+ari_log_FOLLOWUP))

lm.beta(lm(Ct_Nmf24C15~ageAtScan1+sex+averageManualRating+ari_log_FOLLOWUP))

lm.beta(lm(Ct_Nmf24C18~ageAtScan1+sex+averageManualRating+ari_log_FOLLOWUP))

################################################
### Standardized Betas For TP2 Main Effects  ###
################################################

attach(data2)

lm.beta(lm(Ct_Nmf24C1~ScanAgeYears+sex+rating+ari_log))

lm.beta(lm(Ct_Nmf24C8~ScanAgeYears+sex+rating+ari_log))

lm.beta(lm(Ct_Nmf24C9~ScanAgeYears+sex+rating+ari_log))

lm.beta(lm(Ct_Nmf24C10~ScanAgeYears+sex+rating+ari_log))

lm.beta(lm(Ct_Nmf24C11~ScanAgeYears+sex+rating+ari_log))

lm.beta(lm(Ct_Nmf24C13~ScanAgeYears+sex+rating+ari_log))

lm.beta(lm(Ct_Nmf24C15~ScanAgeYears+sex+rating+ari_log))

lm.beta(lm(Ct_Nmf24C18~ScanAgeYears+sex+rating+ari_log))

lm.beta(lm(Ct_Nmf24C20~ScanAgeYears+sex+rating+ari_log))

lm.beta(lm(Ct_Nmf24C21~ScanAgeYears+sex+rating+ari_log))

lm.beta(lm(Ct_Nmf24C22~ScanAgeYears+sex+rating+ari_log))

###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################