###################################################################################################
##########################         GRMPY - Generate NMF CSV's            ##########################
##########################               Robert Jirsaraie                ##########################
##########################        rjirsara@pennmedicine.upenn.edu        ##########################
##########################                 04/21/2018                    ##########################
###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################
#### Use ####

# This script reads in the Raw NMF Data from both time points so that the Annualized Percent Change in 
# CT can be calculated for the longitudinal analyses.


##################################################################
##### Read in the Dataset From the Two Timepoints Seperately #####
##################################################################

TP1<-read.csv("/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/n140_TP1_NMF/n140_Nmf24BasesCT_COMBAT_TP1.csv")
TP2<-read.csv("/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/n141_TP2_NMF/n141_Nmf24BasesCT_COMBAT_TP2.csv")

############################################################################
##### Refine the Datasets to only Select Subjects with Both Timepoints #####
############################################################################

library(gdata)

Delta<-merge(TP1,TP2, by=c("bblid")) #Merge Timepoints to get n=137
varsTP1<-names(Delta[grep(".x",names(Delta))])
DeltaTP1<-Delta[,c("bblid",varsTP1)]
rename<- sub(".x", "", names(DeltaTP1))
DeltaTP1<-rename.vars(DeltaTP1,names(DeltaTP1),rename) #Create Refined TP1 Dataset n137

Delta<-merge(TP1,TP2, by=c("bblid")) #Merge Timepoints to get n=137
varsTP2<-names(Delta[grep(".y",names(Delta))])
DeltaTP2<-Delta[,c("bblid",varsTP2)]
rename<- sub(".y", "", names(DeltaTP2))
DeltaTP2<-rename.vars(DeltaTP2,names(DeltaTP2),rename) #Create Refined TP2 Dataset n137

##############################################################################
##### Subtract TP2 by TP1 then Divide by TP1 to Get Percent Change in CT #####
##############################################################################

DeltaTP1 <- DeltaTP1[order(DeltaTP1$bblid),]
DeltaTP2 <- DeltaTP2[order(DeltaTP2$bblid),]

vdiff <- function(DeltaTP2,DeltaTP1) {
   colnames    <- names(DeltaTP2)[grep('Nmf',names(DeltaTP2))]
   vdiff       <- data.frame(bblid=DeltaTP2$bblid,scanid=DeltaTP2$scanid)
   for (c in colnames){
       vdiff[c] <- (DeltaTP2[c] - DeltaTP1[c])/DeltaTP1[c]
   }
   return(vdiff)
}

percentCT <- vdiff(DeltaTP2,DeltaTP1)

#######################################################################
##### Read in Psychometric Data to Get Delta Age for Each Subject #####
#######################################################################

RDS<-readRDS("/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/n137_Rate_NMF/n137_Psychometrics_20180724.rds")
RDS<-RDS[order(RDS$bblid),]

######################################################################
##### Divide by Delta Age to get Annualized Percent Change in CT #####
######################################################################

rdiff <- function(percentTP2,RDS) {
   colnames    <- names(percentTP2)[grep('Nmf',names(percentTP2))]
   vdiff       <- data.frame(bblid=percentTP2$bblid,scanid=percentTP2$scanid)
   for (c in colnames){
       vdiff[c] <- percentTP2[c]/RDS['DeltaAge']
   }
   return(vdiff)
}

rateCT <- rdiff(percentCT,RDS)

#################################
##### Write Out Spreadsheet #####
#################################

write.csv(rateCT,"/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/n137_Rate_NMF/n137_APR_COMBAT_Rate.csv")

###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################