#!/bin/sh

###################################################################################################
##########################             GRMPY - NMF CT Masks              ##########################
##########################              Robert Jirsaraie                 ##########################
##########################        rjirsara@pennmedicine.upenn.edu        ##########################
##########################                  04/05/2018                   ##########################
###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################
<<Use

This script takes copies of the raw CT Maps produced by the XCP pipeline, to make both subject-level and
group-level masks by binarizing and thesholding the images.

To run this script it is required to have a qlogin session with 50G of memory. Use the Command Below:

qlogin -l h_vmem=50.5G,s_vmem=50.0G -q qlogin.himem.q

Use
###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################

##############################################################
### Defines the Subjects that Passed QA & Will be Analyzed ###
##############################################################

subjects=/data/joy/BBL/studies/grmpy/processedData/NMF/ctRaw/*.nii.gz

OutRaw=/data/jux/BBL/studies/grmpy/processedData/NMF/ctRaw
OutMask=/data/jux/BBL/studies/grmpy/processedData/NMF/ctMask

mkdir -p ${OutMask} #Creates Directory to Store Masks

######################################
### Create the Subject-level Masks ###
######################################

for s in ${subjects}; do 
                                                                                                                                                              
   fileName=$(echo $s | cut -d'/' -f10  | cut -d'.' -f1)
   echo "file name is ${fileName}"

	ThresholdImage 3 $s ${OutMask}/${fileName}_mask.nii.gz 0.1 Inf

done

###################################
### Create the Group-level Mask ###
###################################

AverageImages 3 ${OutMask}/coverageMask.nii.gz 0 ${OutMask}/*mask.nii.gz

fslmaths ${OutMask}/coverageMask.nii.gz -thr .9 -bin ${OutMask}/n281_ctMask_thr9_2mm.nii.gz

chmod -R a+x ${OutMask}/*

###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################