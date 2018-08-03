#!/bin/sh

###################################################################################################
##########################         GRMPY - Transfer CT Images            ##########################
##########################              Robert Jirsaraie                 ##########################
##########################        rjirsara@pennmedicine.upenn.edu        ##########################
##########################                  03/05/2018                   ##########################
###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################
<<Use

These commands were used to transfer the CT masks to CBICA where they will be inputed into the NMF analysis.
In order to log into CBICA you must connect through a VPN; the PennMedicine Network will not work.

Use
###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################

########################################################################
### Create A Zip File that Contains All Smooth the Files to be Moved ###
########################################################################

CfNpath=/data/jux/BBL/studies/grmpy/processedData/NMF/ctSmooth

cd ${CfNpath}/

tar -cvf ${CfNpath}/n281_Smoothed4mm_2018.tar.gz *

#########################################################################
### Log into CBICA then Get Writing Permission using the Sudo Command ###
#########################################################################


ssh jirsarar@cbica-cluster.uphs.upenn.edu

sudo -u pncnmf sudosh 

#####################################
### Transfer the Smooth CT Images ###
#####################################

CfNpath=/data/jux/BBL/studies/grmpy/processedData/NMF/ctSmooth
CBICApath=/cbica/projects/pncNmf/grmpyNMF/n281_Final_201804/ctSmooth

mkdir -p ${CBICApath}

scp -r rjirsaraie@chead:${CfNpath}/n281_Smoothed4mm_2018.tar.gz ${CBICApath}/

#########################################################################
### Unpack and Remove Zip File while Extending Permissions to the Lab ###
#########################################################################

tar -xvf ${CBICApath}/n281_Smoothed4mm_2018.tar.gz

chmod 777 ${CBICApath}/*

rm -rf ${CBICApath}/n281_Smoothed4mm_2018.tar.gz

#################################
### Transfer the Subject List ###
#################################

CfNsubs=/data/jux/BBL/studies/grmpy/processedData/NMF/ctRaw/n281_Cohort_2018.csv
CfNraw=/data/jux/BBL/studies/grmpy/processedData/NMF/ctRaw/n281_ctRaw_2018.csv

CBICAsubs=/cbica/projects/pncNmf/grmpyNMF/n281_Final_201804/subjectdata

mkdir -p ${CBICAsubs}

scp -r rjirsaraie@chead:${CfNsubs} ${CBICAsubs}/
scp -r rjirsaraie@chead:${CfNraw} ${CBICAsubs}/

chmod 777 ${CBICAsubs}/*

################################################
### Create a Text File of Smooth Image Paths ###
################################################

subjects=$(cat ${CBICAsubs}/n281_Cohort_2018.csv | cut -d ',' -f2)

for s in $subjects; do 

ls -d ${CBICApath}/*_*${s}_*4mm.nii.gz* >> ${CBICAsubs}/n281_ctSmooth_2018.csv

done

chmod 777 ${CBICAsubs}/*

#######################################################
### Transfer the CT Mask of all Subjects onto CBICA ###
#######################################################

CfNmask=/data/jux/BBL/studies/grmpy/processedData/NMF/ctMasks/n281_ctMask_thr9_2mm.nii.gz
CBICAmask=/cbica/projects/pncNmf/grmpyNMF/n281_Final_201804/ctMask

mkdir -p ${CBICAmask}

scp -r rjirsaraie@chead:${CfNmask} ${CBICAmask}/


#######################################################################
### Transfer the Split-Half Reproducibility Subjec Files onto CBICA ###
#######################################################################

 scp rjirsaraie@chead:/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/creationNMF/n141_T1_train_bblids_scanids.csv $CBICAsubs/
 scp rjirsaraie@chead:/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/creationNMF/n140_T1_test_bblids_scanids.csv $CBICAsubs/

###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################