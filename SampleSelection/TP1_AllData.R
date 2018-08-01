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

####################################################################
##### Read in Data from the PNC DataFreeze and TP2 CT datafile #####
####################################################################

DEMO<-read.csv("/data/jux/BBL/studies/pnc/pncDataFreeze20170905/n1601_dataFreeze/demographics/n1601_demographics_go1_20161212.csv")
Irrit <- read.csv("/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/n144_TP1_JLF/n1601_goassess_112_itemwise_vars_20161214.csv")
CT<-read.csv("/data/jux/BBL/studies/pnc/pncDataFreeze20170905/n1601_dataFreeze/neuroimaging/t1struct/n1601_jlfAntsCTIntersectionCT_20170331.csv")
QA<-read.csv("/data/jux/BBL/studies/pnc/pncDataFreeze20170905/n1601_dataFreeze/neuroimaging/t1struct/n1601_t1QaData_20170306.csv")

TP2 <- read.csv("/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/n144_TP2_JLF/n144_jlfAntsCTIntersectionCT_20180320.csv")

########################################################
##### Save a Seperate Spreadsheet of Baseline CT #####
########################################################

missingSub <- read.csv("/data/joy/BBL/studies/pnc/n2416_dataFreeze/neuroimaging/t1struct/n2416_jlfAntsCTIntersectionCt_20170331.csv")
CT <- CT[(CT$bblid %in% TP2$bblid),]
missingSub<-missingSub[ which(missingSub$bblid=='99964'), ]
CT <- rbind(CT,missingSub)

write.csv(CT, "/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/n144_TP1_JLF/n144_jlfAntsCTIntersectionCT_20180216.csv")

#################################################
##### Prepare the Irritability Spreadsheet  #####
#################################################

Irrit <- Irrit[(Irrit$bblid %in% TP2$bblid),]
Irrit <- Irrit[c("bblid","scanid","dep004","man007","odd001","odd006")]
Irrit$IrritabilitySum<-apply(Irrit[,3:6],1,sum) 
Irrit$IrritabilityZ<-scale(Irrit$IrritabilitySum, center=TRUE, scale=TRUE)

###############################################
##### Prepare the Demographic Spreadsheet #####
###############################################

DEMO <- DEMO[(DEMO$bblid %in% TP2$bblid),]
DEMO$ageAtClinicalAssess1<-DEMO$ageAtClinicalAssess1/12
DEMO$ageAtCnb1<-DEMO$ageAtCnb1/12
DEMO$ageAtScan1<-DEMO$ageAtScan1/12

#####################################################
##### Prepare the Quality Assurance Spreadsheet #####
#####################################################

QA <- QA[(QA$bblid %in% TP2$bblid),]
QA <- QA[c("bblid","scanid","t1Exclude","averageManualRating")]
QA$t1Exclude[QA$t1Exclude == "1"] <- "9999" #Recode T1Exclude for GAM Wrapper
QA$t1Exclude[QA$t1Exclude == "0"] <- "1"
QA$t1Exclude[QA$t1Exclude == "9999"] <- "0"

######################################################################################################
##### Merge the Prepared Spreadsheets Together and Save a Spreadsheet of the Baseline Covariates #####
######################################################################################################

rds <- merge(DEMO,Irrit,by=c("bblid","scanid"))
rds <- merge(rds,QA,by=c("bblid","scanid"))
rds$sex<-as.factor(rds$sex)
rds$race<-as.factor(rds$race)
rds$race2<-as.factor(rds$race2)
rds$ageAtScan1<-as.numeric(rds$ageAtScan1)
rds$handednessv2<-as.factor(rds$handednessv2)
rds$IrritabilitySum<-as.numeric(rds$IrritabilitySum)
rds$IrritabilityZ<-as.numeric(rds$IrritabilityZ)
missing<-c(1:19)
missing[1:19] <- NA
rds <- rbind(rds, c(99964, 9964,missing))

rds<-rds[which(rds$t1Exclude==1),]

saveRDS(rds, "/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/n140_TP1_NMF/n140_Psychometrics_20180724.rds")

###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################