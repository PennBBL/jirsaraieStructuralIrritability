###################################################################################################
##########################         GRMPY - Generate Structual RDS        ##########################
##########################               Robert Jirsaraie                ##########################
##########################        rjirsara@pennmedicine.upenn.edu        ##########################
##########################                 02/16/2018                    ##########################
###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################
# Use #

# This script version was create in order to create the covariates (RDS) file need to complete longitudinal 
# analyses of the Annualized Percent Change in Cortical Thickness between the two timepoints.

###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################

##############################################
##### Reads in Data from Both Timepoints #####
##############################################

baseline <- readRDS("/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/processedData/baseline/n140_Demo+Psych+DX+QA_20180531.rds")
followup <- readRDS("/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/processedData/follow-up/n141_Demo+Psych+DX+QA_20180724.rds")

##################################################################
##### Add Prefixes to each Timepoint Before Merging Together #####
##################################################################

colnames(baseline) <- paste("TP1", colnames(baseline), sep = "_")
names(baseline)[1]<-'bblid'
colnames(followup) <- paste("TP2", colnames(followup), sep = "_")
names(followup)[1]<-'bblid'

Combined <- merge(baseline, followup, by=c("bblid")) #Merge TP1 and TP2 Variables into single Spreadsheet.

###########################################################
##### Computes the Delta Values of Specific Variables #####
###########################################################

Combined$TP2_rating<-as.numeric(Combined$TP2_rating)
Combined$TP1_averageManualRating<-as.numeric(Combined$TP1_averageManualRating)

Combined$DeltaAge<-Combined$TP2_ScanAgeYears-Combined$TP1_ageAtScan1
Combined$DeltaQA<-rowMeans(Combined[c('TP2_rating','TP1_averageManualRating')])
Combined$DeltaT1Exclude<-Combined$TP2_t1Exclude

Combined$DeltaDiagnosisGroup<-0
Combined[which(Combined$TP1_goassessSmryNCvsDX ==0 & Combined$TP2_dx_NCvsDX ==0),88]<-1 #NC
Combined[which(Combined$TP1_goassessSmryNCvsDX ==1 & Combined$TP2_dx_NCvsDX ==1),88]<-2 #DX
Combined[which(Combined$TP1_goassessSmryNCvsDX ==1 & Combined$TP2_dx_NCvsDX ==0),88]<-3 #Resilient
Combined[which(Combined$TP1_goassessSmryNCvsDX ==0 & Combined$TP2_dx_NCvsDX ==1),88]<-4 #Emergent
Combined$DeltaDiagnosisGroup<-as.factor(Combined$DeltaDiagnosisGroup)

Combined$TP1_IrritabilitySumZ<-scale(Combined$TP1_IrritabilitySum, center=TRUE, scale=TRUE)
Combined$TP2_ari_logZ<-scale(Combined$TP2_ari_log, center=TRUE, scale=TRUE) 
Combined$DeltaIrritability<-Combined$TP2_ari_logZ-Combined$TP1_IrritabilitySumZ

#####################################
##### Write the Output RDS File #####
#####################################

saveRDS(Combined, "/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/processedData/longitudinal/n137_Demo+Psych+DX+QA_20180531.rds")

###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################
