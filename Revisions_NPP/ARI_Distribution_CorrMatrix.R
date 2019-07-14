###################################################################################################
##########################   GRMPY - Histograms and Correlation Plots    #########################
##########################               Robert Jirsaraie                ##########################
##########################        rjirsara@pennmedicine.upenn.edu        ##########################
##########################                 07/13/2019                    ##########################
###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################

##################################
##### Load Packages and Data #####
##################################

rds<-readRDS("/data/jux/BBL/projects/jirsaraieStructuralIrrit/data/processedData/longitudinal/n137_Demo+Psych+DX+QA+PRIME_20190713.rds")
library(ggplot2)

############################################################
##### Plot Histograms Of ARI while Differentiating Sex #####
############################################################

dist_normal<-ggplot(rds, aes(TP2_ari_total, fill = TP2_sex)) + geom_histogram(binwidth = 1) + scale_color_grey() + scale_fill_grey(start=.7, end=.3) + theme_classic() + theme(legend.position="top")

dist_log<-ggplot(rds, aes(TP2_ari_log, fill = TP2_sex)) + geom_histogram(binwidth = .5) + scale_color_grey() + scale_fill_grey(start=.7, end=.3) + theme_classic() + theme(legend.position="top")

ggplot(rds, aes(TP2_ari_log, fill = veg)) + 
   geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')

###########################################
##### Plot Histograms Of ARI Together #####
###########################################

p1<-hist(rds$TP2_ari_total)
p2<-hist(rds$TP2_ari_log)
plot( p1, col=rgb(0.0,0.5,1.0,0.3), xlim=c(0,12))
plot(p2, col=rgb(0.0,0.3,0.7,0.3), xlim=c(0,12), add=T)

#######################################################
##### Load Software and Create Correlations Plots #####
#######################################################

install.packages("corrplot")
library(corrplot)

PSYCHO<-rds[,c(58,65,61,67,93)]
names(PSYCHO)<-c("ARI","SCARED","ADHD","BDI","PSY")

Matrix<-cor(PSYCHO, use="pairwise.complete.obs")
corrplot.mixed(Matrix, lower.col = "black", number.cex = 1.75)

###################################################################################################
#####  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  ⚡  #####
###################################################################################################
