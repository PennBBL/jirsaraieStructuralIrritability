###################################################################################################
##########################       GRMPY - PRIME Psychosis Scoring          ##########################
##########################               Robert Jirsaraie                ##########################
##########################        rjirsara@pennmedicine.upenn.edu        ##########################
##########################                 07/13/2019                    ##########################
###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################

PRIME<-read.csv('/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/rawCopies/follow-up/primeScreenForRobert.csv')
FollowUp<-readRDS('/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/processedData/follow-up/n141_Demo+Psych+DX+QA_20180724.rds')
Longitudinal<-readRDS('/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/processedData/longitudinal/n137_Demo+Psych+DX+QA+FD_20190704.rds')

################################################################
### Reduce PRIME Spreadsheet and Score Variables of Interest ###
################################################################

data<-PRIME[,c(2,5:16)]
maxcol<-dim(data)[2]

### Create threshold of 2 of higher for all items (Kline et al. 2012) ###

for (x in 2:maxcol){

	none<-which(data[,x] <= 1)
		for (subject in none){
		data[subject,x]<-data[subject,x]<-0
		}

	some<-which(data[,x] >= 2)
		for (subject in some){
		data[subject,x]<-data[subject,x]<-1
		}

}

### Create Summary Score ###

data$TP2_PRIME_total<-rowSums(data[2:maxcol])
summary(data$TP2_PRIME_total) #mean=1.134, range=0-12, missing=18
sd(data$TP2_PRIME_total, na.rm=TRUE) #sd=2.209

data[c(1,maxcol+1),]
data<-data[,c(1,maxcol+1)]

####################################################################
### Merge Summary Scores with Main Spreadsheets and Output Files ###
####################################################################

FOLLOWUP<-merge(FollowUp,data,by='bblid')
saveRDS(FOLLOWUP, "/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/processedData/follow-up/n137_Demo+Psych+DX+QA+PRIME_20190713.rds")

LONGITUDINAL<-merge(Longitudinal,data,by='bblid')
saveRDS(LONGITUDINAL, "/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/processedData/longitudinal/n137_Demo+Psych+DX+QA+PRIME_20190713.rds")

###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################