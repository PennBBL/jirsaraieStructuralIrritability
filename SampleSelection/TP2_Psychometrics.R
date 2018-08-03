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

###########################################################
##### Read in Data From Psycha1 & the TP2 cohort file #####
###########################################################

Demo<-read.csv("/data/jux/BBL/studies/grmpy/rawPsycha1/demographics_20180409.csv")
Clinical<-read.csv("/data/jux/BBL/studies/grmpy/rawPsycha1/selfreport_scales_redcap_20180402.csv")
QA<-read.csv("/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/n144_TP2_JLF/n144_manualQA_20180318.csv")
Subjects<-read.csv("/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/n144_TP2_JLF/n144_jlfAntsCTIntersectionCT_20180320.csv")

################################################
##### Prepare the Demographics Spreadsheet #####
################################################

Demo<-Demo[,-c(4)]
Demo<-Demo[-c(1,2),]
Demo<-Demo[which(Demo$bblid %in% Subjects$bblid),]
Demo$enrollagemonths<-Demo$enrollagemonths/12
Demo$sex<-as.factor(Demo$sex)
Demo$race<-as.factor(Demo$race)
Demo$enrollagemonths<-as.numeric(Demo$enrollagemonths)
colnames(Demo)[3]<-"ScanAgeYears"

############################################################
##### Prepare the Clinical Spreadsheet of the ARI Data #####
############################################################

Clinical<-Clinical[which(Clinical$bblid %in% Subjects$bblid),]
Clinical<-Clinical[ which(Clinical$bbl_protocol %in% "GRMPY") , ]
Clinical[Clinical ==-9999] <- NA
Clinical<-Clinical[complete.cases(Clinical[,c(10:16)]),] #Only Select ARI

aribblid<-Clinical[,c('bblid'), drop=FALSE]
ari<-Clinical[,c(grep('ari_[0-9]', colnames(Clinical)))]
ari<-cbind(aribblid,ari)
ari<-ari[,c('bblid','ari_1','ari_2','ari_3','ari_4','ari_5','ari_6')]
ari$ari_avg<-rowMeans(ari[2:6])
ari$ari_total<-rowSums(ari[2:6])#ARI Summary Score
ari$ari_reduced <- ari$ari_total 
ari$ari_reduced[ari$ari_reduced ==0] <- NA
ari$ari_log <- log(ari$ari_total+1) #Apply Log Transform of ARI to correct skewness

#####################################################
##### Prepare the Quality Assurance Spreadsheet #####
#####################################################

QA<-QA[which(QA$bblid %in% Subjects$bblid),]
QA<-QA[,c('bblid','scanid','rating')]
QA$rating<-as.numeric(QA$rating)

##############################################################
##### Merge Spreadsheets together and Apply QA Exclusion #####
##############################################################

Subjects<-Subjects[,c('bblid','scanid')]

final<-merge(Subjects,Demo, by=("bblid"))
final<-merge(final,ari, by=("bblid"))
final<-merge(final,QA, by=c("bblid","scanid"))

final<-final[which(final$rating>=1),] #Remove Subjects that Did not Pass QA 

#################################
##### Write Out New Dataset #####
#################################

saveRDS(final, "/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/n141_TP2_NMF/n141_Psychometrics_20180724.rds")

###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################