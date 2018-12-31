#!/bin/sh

###################################################################################################
##########################             GRMPY - NMF CT Smooth             ##########################
##########################              Robert Jirsaraie                 ##########################
##########################        rjirsara@pennmedicine.upenn.edu        ##########################
##########################                  04/05/2018                   ##########################
###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################
<<Use

This script makes copies of the CT maps that are outputed by the XCP pipeline. These images are then smoothed to 
increase the voxel dimensions for better resolution of the NMF networks.

To run this script it is required to have a qlogin session with 50G of memory. Use the Command Below:

qlogin -l h_vmem=50.5G,s_vmem=50.0G -q qlogin.himem.q

Use
###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################

##############################################################
### Defines the Subjects that Passed QA & Will be Analyzed ###
##############################################################

TP2=$(cat /data/jux/BBL/projects/jirsaraieStructuralIrrit/data/n144_TP2_JLF/n144_jlfAntsCTIntersectionCT_20180320.csv| cut -d ',' -f1 | grep -v '85083' |  grep -v '106331' | grep -v '121476'| grep -v 'bblid')
TP1=$(cat /data/jux/BBL/projects/jirsaraieStructuralIrrit/data/n144_TP1_JLF/n144_jlfAntsCTIntersectionCT_20180216.csv| cut -d ',' -f3 | grep -v '6026' | grep -v '9964' | grep -v '6901' | grep -v '3710'| grep -v 'scanid')

############################################
### Create Directories to Transfer Files ###
############################################

OutRaw=/data/jux/BBL/studies/grmpy/processedData/NMF/ctRaw
OutMask=/data/jux/BBL/studies/grmpy/processedData/NMF/ctMasks
OutSmooth=/data/jux/BBL/studies/grmpy/processedData/NMF/ctSmooth

mkdir -p ${OutRaw}
mkdir -p ${OutMask}
mkdir -p ${OutSmooth}

######################################################################
### Make Copies of the Raw CT Images To Be Processed From TP2 ONLY ###
######################################################################

i=0
for s in $TP2; do

   imgList[i]=$(ls /data/joy/BBL/studies/grmpy/processedData/structural/struct_pipeline_20170716/${s}/*/antsCT/*_CorticalThicknessNormalizedToTemplate.nii.gz)

   echo ${imgList[i]}

   cp ${imgList[i]} ${OutRaw}

   (( i++ )) 

done

#############################################################################
### Applys Transform to Increase Dimensions of voxels by 2mm for TP2 ONLY ###
#############################################################################

copies=/data/jux/BBL/studies/grmpy/processedData/NMF/ctRaw/*_CorticalThicknessNormalizedToTemplate.nii.gz

i=0
for c in $copies; do 

   bblid=$(echo ${c}|cut -d'_' -f1); 
   datexscanid=$(echo ${c}|cut -d'_' -f2); 

   output[i]=${bblid}_${datexscanid}_TP2_CorticalThicknessNormalizedToTemplate_2mm.nii.gz
   
   echo ${output[i]}
   
   antsApplyTransforms -e 3 -d 3 -i ${c} -r /data/joy/BBL/studies/pnc/template/pnc_template_brain_2mm.nii.gz -o ${output[i]}

   (( i++ ))

done
for o in ${output[@]}; do

   if [[ -f ${o} ]] ; then 

      file=$(echo ${o}| sed s@'_2mm'@''@g | sed s@'_TP2'@''@g)

      rm $file

      else 

      echo "Output For This Subject Was Not Successful"

   fi
done

###############################################################################
### Make Copies of TP1 -- TP1 already Has 2mm Voxels So No Transform Needed ###
###############################################################################

i=0
for scanid in $TP1; do

   imgList[i]=$(ls /data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/t1struct/voxelwiseMaps_antsCt/${scanid}_CorticalThicknessNormalizedToTemplate2mm.nii.gz)
   bblid[i]=$(echo /data/joy/BBL/studies/pnc/rawData/*/*x${scanid}/ |cut -d '/' -f8)
   datexscanid[i]=$(echo /data/joy/BBL/studies/pnc/rawData/*/*x${scanid}/ |cut -d '/' -f9)
   
   echo ${imgList[i]}
   echo ${datexscanid[i]}
   echo ${bblid[i]}
   
   cp ${imgList[i]} ${OutRaw}/${bblid[i]}_${datexscanid[i]}_TP1_CorticalThicknessNormalizedToTemplate_2mm.nii.gz

   (( i++ )) 

done

chmod -R a+x ${OutRaw}/*

#############################################
### Create csv's of the Files Information ###
#############################################

ls ${OutRaw}/* >> ${OutRaw}/n281_ctRaw_2018.csv

cohort=$(ls ${OutRaw}/* | grep -v '/data/jux/BBL/studies/grmpy/processedData/NMF/ctRaw/n281_ctRaw_2018.csv')

for c in $cohort ; do

bblid=$(echo ${c} | cut -d '/' -f10| cut -d '_' -f1)
scanid=$(echo ${c} | cut -d '/' -f10| cut -d '_' -f2 | cut -d 'x' -f2)

echo ${bblid},${scanid} >> ${OutRaw}/n281_Cohort_2018.csv

done


#####################################################
### Smooth the RawCT Images for Better Resolution ###
#####################################################

subjects=/data/jux/BBL/studies/grmpy/processedData/NMF/ctRaw/*_CorticalThicknessNormalizedToTemplate_2mm.nii.gz

i=0
for s in ${subjects}; do

basename=$(echo ${s} | cut -d '/' -f10| cut -d '.' -f1)

echo $basename

sig=1.70

fslmaths ${s} -s ${sig} ${OutSmooth}/${basename}_smoothed4mm.nii.gz

(( i++ ))
done

chmod -R a+x ${OutSmooth}/*


###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################