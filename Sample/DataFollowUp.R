###################################################################################################
##########################            GRMPY - Transform ARI              ##########################
##########################               Robert Jirsaraie                ##########################
##########################        rjirsara@pennmedicine.upenn.edu        ##########################
##########################                 03/22/2018                    ##########################
###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################
# Use #

# This script was created to pull the demographic and clinical data from Psycha1 to prepare for analyses.

###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################

###########################################
##### Define the Subjects of Interest #####
###########################################

subs <- read.csv("/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/rawCopies/baseline/BBLids.csv")

library(car)

####################################
##### Prepare the Demographics #####
####################################

DEMO<-read.csv("/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/rawCopies/follow-up/demographics_20180824.csv")
DEMO<-DEMO[which(DEMO$bblid %in% subs$bblid),]
DEMO<-DEMO[,c('bblid','enrollagemonths','sex','race','hand','height','weight','educ','mom_educ','dad_educ')]
DEMO$enrollagemonths<-DEMO$enrollagemonths/12
names(DEMO)[2]<-'ScanAgeYears'

###########################################################
##### Prepare the Data about Perscription Medications #####
###########################################################

MEDS <- read.csv("/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/rawCopies/follow-up/Medications.csv")
MEDS<-MEDS[which(MEDS$bblid %in% subs$bblid),]
MEDS<-MEDS[,c(1,4:5)] # Remove Visit 2 because 85 missing Values
names(MEDS)<-c("bblid","Medications","Psychotropics")
MEDS$MedsExclusion<-1
CurrentUse<-which(MEDS$Psychotropics == 1)
for (subject in CurrentUse)
{
MEDS[subject,4]<-MEDS[subject,4]<-0
}

###################################################################
##### Prepare the Data on Drug Use from the Urine Drug Screen #####
###################################################################

DRUGS<-read.csv("/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/rawCopies/follow-up/imglook_20180622.csv")
DRUGS<-DRUGS[which(DRUGS$bblid %in% subs$bblid),]
DRUGS<-DRUGS[,c(1,7,9)]
DRUGS<-unique(DRUGS[,1:3])
colnames(DRUGS) <- c("bblid","drugscreen1","poscreenexpl1")
DRUGS$drugscreen1<-as.numeric(DRUGS$drugscreen1)
DRUGS$drugscreen1<-recode(DRUGS$drugscreen1,"c('1')=NA")
DRUGS$drugscreen1<-recode(DRUGS$drugscreen1,"c('2')=0")
DRUGS$drugscreen1<-recode(DRUGS$drugscreen1,"c('3')=1")
DRUGS$DrugsExclusion<-1
CurrentUse<-which(DRUGS$drugscreen1 == 1)
for (subject in CurrentUse)
{
DRUGS[subject,4]<-DRUGS[subject,4]<-0
}

########################################################################
##### Prepare the Data on the Quality Assurance of Follow-up Scans #####
########################################################################

QA<-read.csv("/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/rawCopies/follow-up/n144_manualQA_20180318.csv")
QA<-QA[which(QA$bblid %in% subs$bblid),]
QA<-QA[,c('bblid','scanid','rating')]
QA$t1Exclude<-1
Unusable<-which(QA$rating == 0)
for (subject in Unusable)
{
QA[subject,4]<-QA[subject,4]<-0
}
QA <- QA[which(QA$t1Exclude == 1),] #Remove Subjects with unusable scans

##########################################################
##### Prepare the Dimensions of Psychopathology Data #####
##########################################################

PSYCH<-read.csv("/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/rawCopies/follow-up/BBLSelfreportAndStat_DATA_2018-06-02_2153.csv")
PSYCH<-PSYCH[which(PSYCH$bblid %in% subs$bblid),]
PSYCH<-PSYCH[ which(PSYCH$bbl_protocol %in% "GRMPY"),]
PSYCH<-PSYCH[complete.cases(PSYCH[,c(14:19)]),]
PSYCH[PSYCH ==-9999] <- NA

### ARI - Irritability ###
ari<-PSYCH[,c(grep('ari_[0-9]', colnames(PSYCH)))]
ari<-cbind(PSYCH$bblid,ari)
ari<-ari[1:7]
ari$ari_total<-rowSums(ari[2:7]) #ARI Summary Score
ari<-ari[,-c(2:7)]
ari$ari_reduced <- ari$ari_total 
ari$ari_reduced[ari$ari_reduced ==0] <- NA
ari$ari_log <- log(ari$ari_total+1) #Apply Log Transform of ARI to correct skewness
names(ari)[1]<-'bblid'

### SWAN - ADHD ###
swan<-PSYCH[,c(grep('swan_[0-9]', colnames(PSYCH)))]
swan<-cbind(PSYCH$bblid,swan)
swan<-swan[,c(1:19)]
swan$adhd_total<-rowSums(swan[,2:19])
swan<-swan[,-c(2:19)]
swan$adhd_log <- log(swan$adhd_total+1) 
names(swan)[1]<-'bblid'

### ACE - ELS ###
ace<-PSYCH[,c(grep('aces_[0-9]', colnames(PSYCH)))]
ace<-cbind(PSYCH$bblid,ace)
ace$els_total<-rowSums(ace[,2:11])
ace<-ace[,-c(2:11)]
ace$els_log <- log(ace$els_total+1) 
names(ace)[1]<-'bblid'

### SCARED - Anxiety ###
scared<-PSYCH[,c(grep('scared_[0-9]', colnames(PSYCH)))]
scared<-cbind(PSYCH$bblid,scared)
scared<-scared[,c(1:42)]
scared$scared_total<-rowSums(scared[,2:42])
scared<-scared[,-c(2:42)]
scared$scared_log <- log(scared$scared_total+1) 
names(scared)[1]<-'bblid'

### BDI - Depression ###
bdi<-PSYCH[,c(grep('bdi_[0-9]', colnames(PSYCH)))]
bdi<-cbind(PSYCH$bblid,bdi)
bdi<-bdi[,c(1:23)]
bdi$bdi_total<-rowSums(bdi[,2:23])
bdi<-bdi[,-c(2:23)]
bdi$bdi_log<-log(bdi$bdi_total+1) 
names(bdi)[1]<-'bblid'

###################################################
##### Prepare the Diagnosis Data at Follow-up #####
###################################################

DX<-read.csv("/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/rawCopies/follow-up/diagnosis_wsmryvars_20180731.csv")
DX<-DX[which(DX$BBLID %in% subs$bblid),]
DX<-DX[,c(1,24:27,98:120)]
names(DX)[1]<-'bblid'
DX$dx_prodromal<-NULL #Remove Extra Diagnoses
DX$dx_prodromal_remit<-NULL
DX$dx_sub_dep_can<-NULL
DX$dx_sub_dep_alc<-NULL
DX$dx_sub_dep_oth<-NULL
DX$dx_sub_abuse_can<-NULL
DX$dx_sub_abuse_alc<-NULL
DX$dx_sub_abuse_oth<-NULL
DX$dx_pscat<-NULL
DX$dxsum<-NULL
DX$dx_psychotic<-rowSums(DX[,c('dx_psychosis','dx_scz')]) #Merge Psychotic Disorders
DX$dx_psychotic[DX$dx_psychotic>=1] <- 1
DX$dx_psychosis<-NULL
DX$dx_scz<-NULL
DX$dx_Bipolar<-rowSums(DX[,c('dx_bp1','dx_bpoth')]) #Merge Bipolar Disorders
DX$dx_Bipolar[DX$dx_Bipolar>=1] <- 1
DX$dx_bp1<-NULL
DX$dx_bpoth<-NULL
DX$dx_OTHER<-rowSums(DX[,c('dx_sub_dep','dx_sub_abuse')]) #Merge Bipolar Disorders
DX$dx_OTHER[DX$dx_OTHER>=1] <- 1
DX$dx_sub_abuse<-NULL
DX$dx_sub_dep<-NULL

DX$dx_Sum<-rowSums(DX[,c(7:10,13:15)]) #Calculate Summary Variable
DX$dx_NCvsDX<-ifelse(DX$dx_Sum == 0, 0, ifelse(DX$dx_Sum >= 1, 1, 9))

####################################################
##### Merge the Prepared Spreadsheets Together #####
####################################################

rds <- merge(DEMO,MEDS,by=c("bblid"))
rds <- merge(rds,DRUGS,by=c("bblid"))
rds <- merge(rds,QA,by=c("bblid"))
rds <- merge(rds,ari,by=c("bblid"))
rds <- merge(rds,swan,by=c("bblid"))
rds <- merge(rds,ace,by=c("bblid"))
rds <- merge(rds,scared,by=c("bblid"))
rds <- merge(rds,bdi,by=c("bblid"))
rds <- merge(rds,DX,by=c("bblid"))
rds<-rds[,c(1,17,2:16,18:46)]

#############################################################################
##### Conduct Factor Analysis to Compute General Psychopathology Scores #####
#############################################################################

dimensions<-rds[,c('bblid','ari_total','adhd_total','scared_total','bdi_total')]

### Run PCA to Determine the Proportion of Shared Variance for a Single Factor
dimensions.pca<-princomp(dimensions[,2:5])
summary(dimensions.pca) # shared variance =0.79887

### Execute Factor Analysis
factor<-factanal(x=dimensions[,2:5], factors=1, rotation='varimax', scores="regression")
dimensions$Factor1<-factor$scores
dimensions<-dimensions[,c(1,6)]
rds<-merge(rds,dimensions, by=c('bblid'))
names(rds)[47]<-'GenPsycho'

#################################
##### Write Out New Dataset #####
#################################

rds[,c(4:6,13:18,20,32:46)] <- lapply(rds[,c(4:6,13:18,20,32:46)], as.factor)
rds[,c(3,7:11,18,20:30,47)] <- lapply(rds[,c(3,7:11,18,20:30,47)], as.numeric)

saveRDS(rds, "/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/processedData/follow-up/n141_Demo+Psych+DX+QA_20180724.rds")

###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################
