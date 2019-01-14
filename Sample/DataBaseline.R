###################################################################################################
##########################      GRMPY - TP1 Cohort Collection            ##########################
##########################               Robert Jirsaraie                ##########################
##########################        rjirsara@pennmedicine.upenn.edu        ##########################
##########################                 01/31/2017                    ##########################
###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################
#### Use ####

# This script was created to pull the subjects of interest from the PNC datafreeze and create 
# seperate spreadsheets of the baseline data for the current study

###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################

###########################################
##### Define the Subjects of Interest #####
###########################################

subs <- read.csv("/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/rawCopies/baseline/BBLids.csv")

########################################################################
##### Calculate the Dimesional Measure of Irritability at Baseline #####
########################################################################

Irrit <- read.csv("/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/rawCopies/baseline/n1601_goassess_112_itemwise_vars_20161214.csv")
Irrit <- Irrit[(Irrit$bblid %in% subs$bblid),]
Irrit <- Irrit[c("bblid","scanid","dep004","man007","odd001","odd006")]
Irrit$IrritabilitySum<-apply(Irrit[,3:6],1,sum) 
Irrit$IrritabilityZ<-scale(Irrit$IrritabilitySum, center=TRUE, scale=TRUE)

###############################################
##### Prepare the Demographic Spreadsheet #####
###############################################

DEMO<-read.csv("/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/rawCopies/baseline/n1601_demographics_go1_20161212.csv")
DEMO <- DEMO[(DEMO$bblid %in% subs$bblid),]
DEMO$ageAtClinicalAssess1<-NULL
DEMO$ageAtCnb1<-NULL
DEMO$ageAtScan1<-DEMO$ageAtScan1/12

#####################################################
##### Prepare the Quality Assurance Spreadsheet #####
#####################################################

QA<-read.csv("/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/rawCopies/baseline/n1601_t1QaData_20170306.csv")
QA <- QA[(QA$bblid %in% subs$bblid),]
QA <- QA[c("bblid","scanid","t1Exclude","averageManualRating")]
QA$t1Exclude[QA$t1Exclude == "1"] <- "9999" #Recode Exclusion scans for consistency
QA$t1Exclude[QA$t1Exclude == "0"] <- "1"
QA$t1Exclude[QA$t1Exclude == "9999"] <- "0"
QA <- QA[which(QA$t1Exclude == 1),] #Remove Subjects with unusable scans

##################################################
##### Prepare the Diagnosis Data at Baseline #####
##################################################

DX<-read.csv("/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/rawCopies/baseline/n1601_goassess_psych_summary_vars_20131014.csv")
DX <- DX[(DX$bblid %in% subs$bblid),]
for (vars in 3:33)
{
DX[vars]<-ifelse(DX[vars] <= 3, 0, ifelse(DX[vars] == 4, 1, 3))
}
DX$goassessSmryPsychOverallRtg<-NULL
DX$goassessSmryPrimePos2<-NULL
DX$goassessSmry<-NULL
DX$goassessSmryPrimePos1<-NULL
DX$goassessSmryPrimeTot<-NULL
DX$goassessSmryHalTh<-NULL
DX$goassessSmryHalOh<-NULL
DX$goassessSmryHalVh<-NULL
DX$goassessSmryHalAs<-NULL
DX$goassessSmryHalAv<-NULL
DX$goassessSmryHal<-NULL
DX$goassessSmryDel<-NULL
DX$goassessSmryEat<-NULL
DX$goassessSmryBul<-NULL
DX$goassessSmryAno<-NULL
DX$goassessSmryOdd<-NULL
DX$goassessSmryCon<-NULL
DX$goassessSmrySum<-rowSums(DX[,c(4:6,14,15,17)]) #Calculate Summary Variable
DX$goassessSmryNCvsDX<-ifelse(DX$goassessSmrySum == 0, 0, ifelse(DX$goassessSmrySum >= 1, 1, 9))
DX[3:19] <- lapply(DX[3:19], factor)

###################################################################
##### Prepare the Dimensions of Psychopathology Data from TP2 #####
###################################################################

PSYCH<-read.csv("/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/rawCopies/follow-up/BBLSelfreportAndStat_DATA_2018-06-02_2153.csv")
PSYCH<-PSYCH[which(PSYCH$bblid %in% subs$bblid),]
PSYCH<-PSYCH[ which(PSYCH$bbl_protocol %in% "GRMPY"),]
PSYCH<-PSYCH[complete.cases(PSYCH[,c(14:19)]),]
PSYCH[PSYCH ==-9999] <- NA

### ARI - Irritability ###
ari<-PSYCH[,c(grep('ari_[0-9]', colnames(PSYCH)))]
ari<-cbind(PSYCH$bblid,ari)
ari<-ari[1:7]
ari$ari_total_FOLLOWUP<-rowSums(ari[2:7]) #ARI Summary Score
ari<-ari[,-c(2:7)]
ari$ari_reduced_FOLLOWUP <- ari$ari_total 
ari$ari_reduced_FOLLOWUP[ari$ari_reduced_FOLLOWUP ==0] <- NA
ari$ari_log_FOLLOWUP <- log(ari$ari_total+1) #Apply Log Transform of ARI to correct skewness
names(ari)[1]<-'bblid'

### SWAN - ADHD ###
swan<-PSYCH[,c(grep('swan_[0-9]', colnames(PSYCH)))]
swan<-cbind(PSYCH$bblid,swan)
swan<-swan[,c(1:19)]
swan$adhd_FOLLOWUP<-rowSums(swan[,2:19])
swan<-swan[,-c(2:19)]
names(swan)[1]<-'bblid'

### ACE - ELS ###
ace<-PSYCH[,c(grep('aces_[0-9]', colnames(PSYCH)))]
ace<-cbind(PSYCH$bblid,ace)
ace$ace_FOLLOWUP<-rowSums(ace[,2:11])
ace<-ace[,-c(2:11)]
names(ace)[1]<-'bblid'

### SCARED - Anxiety ###
scared<-PSYCH[,c(grep('scared_[0-9]', colnames(PSYCH)))]
scared<-cbind(PSYCH$bblid,scared)
scared<-scared[,c(1:42)]
scared$scared_FOLLOWUP<-rowSums(scared[,2:42])
scared<-scared[,-c(2:42)]
names(scared)[1]<-'bblid'

### BDI - Depression ###
bdi<-PSYCH[,c(grep('bdi_[0-9]', colnames(PSYCH)))]
bdi<-cbind(PSYCH$bblid,bdi)
bdi<-bdi[,c(1:23)]
bdi$bdi_FOLLOWUP<-rowSums(bdi[,2:23])
bdi<-bdi[,-c(2:23)]
names(bdi)[1]<-'bblid'

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

DRUGS<-read.csv("/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/rawCopies/follow-up/GRMPYDataEntryInterv_DATA_2018-06-01_0118.csv")
DRUGS<-DRUGS[which(DRUGS$bblid %in% subs$bblid),]
DRUGS<-DRUGS[,1:3] 
DRUGS$drugscreen1<-recode(DRUGS$drugscreen1,"c('unk')=NA")
DRUGS$DrugsExclusion<-1
CurrentUse<-which(DRUGS$drugscreen1 == 1)
for (subject in CurrentUse)
{
DRUGS[subject,4]<-DRUGS[subject,4]<-0
}
####################################################
##### Merge the Prepared Spreadsheets Together #####
####################################################

rds <- merge(DEMO,Irrit,by=c("bblid","scanid"))
rds <- merge(rds,DX,by=c("bblid","scanid"))
rds <- merge(rds,QA,by=c("bblid","scanid"))
rds <- merge(rds,ari,by=c("bblid"))
rds <- merge(rds,swan,by=c("bblid"))
rds <- merge(rds,ace,by=c("bblid"))
rds <- merge(rds,scared,by=c("bblid"))
rds <- merge(rds,bdi,by=c("bblid"))
rds <- merge(rds,MEDS,by=c("bblid"))
rds <- merge(rds,DRUGS,by=c("bblid"))

################################################################
##### Reclassify Variable Types and Save Final Spreadsheet #####
################################################################

rds$sex<-as.factor(rds$sex)
rds$race<-as.factor(rds$race)
rds$race2<-as.factor(rds$race2)
rds$ageAtScan1<-as.numeric(rds$ageAtScan1)
rds$handednessv2<-as.factor(rds$handednessv2)
rds$IrritabilitySum<-as.numeric(rds$IrritabilitySum)
rds$IrritabilityZ<-as.numeric(rds$IrritabilityZ)
rds$t1Exclude<-as.numeric(rds$t1Exclude)

saveRDS(rds, "/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/processedData/baseline/n140_Demo+Psych+DX+QA_20180531.rds")

###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################