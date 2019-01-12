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
DX$goassessSmrySum<-rowSums(DX[,c(4:6,14,15,17:19)]) #Calculate Summary Variable
DX$goassessSmryNCvsDX<-ifelse(DX$goassessSmrySum == 0, 0, ifelse(DX$goassessSmrySum >= 1, 1, 9))
DX[3:21] <- lapply(DX[3:21], factor) 

####################################################
##### Merge the Prepared Spreadsheets Together #####
####################################################

rds <- merge(DEMO,Irrit,by=c("bblid","scanid"))
rds <- merge(rds,DX,by=c("bblid","scanid"))
rds <- merge(rds,QA,by=c("bblid","scanid"))

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