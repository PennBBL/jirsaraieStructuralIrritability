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

#########################################################################
##### Reads in all the Demographic, Irritability, and structQA Data #####
#########################################################################

TP2 <- readRDS("/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/n141_TP2_NMF/n141_Psychometric_20180724.rds")
TP1 <- readRDS("/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/n140_TP1_NMF/n140_Psychometrics_20180724.rds")

###################################################################################
##### Extract only the variables needed for analyses from TP1 and add prefixs #####
###################################################################################

TP1 <- TP1[c("bblid","scanid", "ageAtScan1", "IrritabilitySum", "IrritabilityZ", "averageManualRating","t1Exclude")]
colnames(TP1) <- c("bblid","TP1scanid", "TP1ageAtScan1", "TP1IrritabilitySum", "TP1IrritabilityZ", "TP1averageManualRating","TP1t1Exclude")

Combined <- merge(TP1, TP2, by=c("bblid")) #Merge TP1 and TP2 Variables into single Spreadsheet.

########################################################
##### Computes the Change Of Age from PNC to GRMPY #####
########################################################

Combined$DeltaAge<-Combined$ScanAgeYears-Combined$TP1ageAtScan1

################################################################
##### Computes the Average QA Ratings For Both DataFrames  #####
################################################################

Combined$TP1t1Exclude<-as.numeric(Combined$TP1t1Exclude)
Combined$TP1averageManualRating<-as.numeric(Combined$TP1averageManualRating)
Combined$rating<-as.numeric(Combined$rating)

###########################################################################
##### Make Z-Scores of Timepoint 2 and Delta Variable of Irritability #####
###########################################################################

Combined$ARIzTP2<-scale(Combined$ari_total, center= TRUE, scale = TRUE)

Combined$DeltaIrritabilityZ <- Combined$ARIzTP2 - Combined$TP1IrritabilityZ

#####################################
##### Write the Output RDS File #####
#####################################

saveRDS(Combined, "/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/n137_Rate_NMF/n137_Psychometrics_20180724.rds")

###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################