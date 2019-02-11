#Create nifti images: 1) with all NMF components on one brain and 2) with only FDR-significant NMF components

###############################
### LOAD DATA AND LIBRARIES ###
###############################

data.NMF <- readRDS("/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/processedData/follow-up/n141_Demo+Psych+DX+QA_20180724.rds")

#Load libraries
library(mgcv)
library(ANTsR)

#####################################################
### CREATE IMAGE WITH ALL COMPONENTS ON ONE BRAIN ###
#####################################################
#Load the 4d set of merged component images (NOTE: each component needs to be merged in the correct order 1-26 for this script to work).

img<-antsImageRead('/data/jux/BBL/projects/jirsaraieStructuralIrrit/output/NMF_Loadings/MaskedByNeighborThresh300_WarpedToMNI/n281_24NmfComponentsMerged.nii.gz',4)

#Load the prior grey matter mask (warped to MNI space and binarized)
mask<-antsImageRead('/data/jux/BBL/projects/jirsaraieStructuralIrrit/output/NMF_Loadings/mask/prior_grey_thr01_2mm_MNI_bin.nii.gz',3)

#Create matrix: rows are components, columns are voxels
seed.mat<-timeseries2matrix(img, mask)

#Find, for each voxel, which component has highest loading
whichCompStr <-apply(seed.mat,2,which.max) # however, some of those are all zeros, need to remove
foo <-apply(seed.mat,2,sum) 		   # this is sum of loadings across column; if 0, entire column is 0
whichCompStr[which(foo==0)]<-0 		   # assign 0-columns to 0

#Write image with all components on one brain (every voxel is assigned to one component)
newImg<-antsImageClone(mask)               # prep for writing out image
newImg[mask==1]<-as.matrix(whichCompStr)   # put assigned values back in the image
antsImageWrite(newImg,"/data/jux/BBL/projects/jirsaraieStructuralIrrit/output/NMF_Loadings/mask/CtNmf24Components.nii.gz")

##################################################################
### CREATE IMAGE WITH ONLY SIGNIFICANT COMPONENTS ON ONE BRAIN ###
##################################################################
#Assign values for components where gestational age is FDR-significantly associated with volume (see GamAnalyses.R for which components survive fdr correction) 
#Create a vector that assigns each voxel a component number
tvalMap<-antsImageClone(mask)
tvalVoxVector<-whichCompStr

#Assign all components that did not survive fdr correction to 0
tvalVoxVector[which(tvalVoxVector %in% c(1,2,3,4,5,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24))]<-0

#OPTIONAL: For the remaining components, assign the respective t-value for that component
#However, this makes it difficult to make custom color palettes in Caret later.

tvalVoxVector[which(tvalVoxVector==6)] <- -3.3

#Essentially this is a replaced image with 0s and surviving component numbers or t-values
tvalMap[mask==1]<-as.matrix(tvalVoxVector)
antsImageWrite(tvalMap,"/data/jux/BBL/projects/jirsaraieStructuralIrrit/output/NMF_Figures/F5_Mod/n141_CtNmfSigComponents_COMBAT_t300_TP2.nii.gz")
