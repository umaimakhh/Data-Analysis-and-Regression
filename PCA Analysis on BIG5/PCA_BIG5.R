#author: "Umaima Khurshid Ahmad"
#date: "8/1/2020
#reference: DSC424 Lab Week 2"
#Libraries
library(Hmisc) #Describe Function
library(psych) #Multiple Functions for Statistics and Multivariate Analysis
library(GGally) #ggpairs Function
library(ggplot2) #ggplot2 Functions
library(vioplot) #Violin Plot Function
library(corrplot) #Plot Correlations
library(REdaS) #Bartlett's Test of Sphericity
library(psych) #PCA/FA functions
library(factoextra) #PCA Visualizations
library("FactoMineR") #PCA functions
library(ade4) #PCA Visualizations
##############################################################################################

#Set Working Directory
setwd('E:/DPU/Summer I/Advance Data Analysis/mid')
#Read in Datasets

responses <- read.csv(file="16PF.csv", header=TRUE, sep=",",na = c("", "0"))

#Check Sample Size and Number of Variables
dim(responses)
head(responses)

names(responses)

################################################################################################

#Check for Missing Values (i.e. NAs)

#For All Variables - no missing value 
sum(is.na(responses))

#Show Structure of Dataset
str(responses, list.len=ncol(responses))

#Show column Numbers
names(responses)

#formation of blocks for 
E <- responses2[,1:10]
N <- responses2[,11:20]
A <- responses2[,21:30]
C <- responses2[,31:40]
O <- responses2[,41:50]


#Show descriptive statistics

#Normality Rule of Thumb with Skewnewss and Kurtosis (think normal bell curve):
#Short Way:
#If skewnewss is close to 0, the distribution is normal.
#If Kurtosis is -3 or 3, the distribution is normal.

#If skewness is less than -1 or greater than 1, the distribution is highly skewed.
#If skewness is between -1 and -0.5 or between 0.5 and 1, the distribution is moderately skewed.
#If skewness is between -0.5 and 0.5, the distribution is approximately symmetric.
library(psych)
describe(E)
describe(N)
describe(A)
describe(C)
describe(O)

#Check for Multicollinearity with Correlations


M<-cor(responses, method="spearman")
M
#personality
corrplot(cor(M,method="spearman"), method = "number", type = "lower")

#GGplot Correlation
ggcorr(responses[,1:20], method = c("pairwise","spearman"), label=TRUE)

ggcorr(responses, method = c("pairwise","spearman"))

# Run a correlation test to see how correlated the variables are.  Which correlations are significant
options("scipen"=100, "digits"=5)
round(cor(responses), 2)
MCorrTest = corr.test(responses, adjust="none")
MCorrTest

M = MCorrTest$p
M

# Now, for each element, see if it is < .01 (or whatever significance) and set the entry to 
# true = significant or false
MTest = ifelse(M < .01, T, F)
MTest

# Now lets see how many significant correlations there are for each variable.  We can do
# this by summing the columns of the matrix
colSums(MTest) - 1  # We have to subtract 1 for the diagonal elements (self-correlation)

#################################################################################################################
#########################################################################
# PCA_Plot functions
#########################################################################

PCA_Plot = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = data.frame(pcaData$rotation, .names = row.names(pcaData$rotation))
  p + geom_text(data=loadings, mapping=aes(x = PC1, y = PC2, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC1", y = "PC2")
}

PCA_Plot_Secondary = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = data.frame(pcaData$rotation, .names = row.names(pcaData$rotation))
  p + geom_text(data=loadings, mapping=aes(x = PC3, y = PC4, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC3", y = "PC4")
}

PCA_Plot_Psyc = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = as.data.frame(unclass(pcaData$loadings))
  s = rep(0, ncol(loadings))
  for (i in 1:ncol(loadings))
  {
    s[i] = 0
    for (j in 1:nrow(loadings))
      s[i] = s[i] + loadings[j, i]^2
    s[i] = sqrt(s[i])
  }
  
  for (i in 1:ncol(loadings))
    loadings[, i] = loadings[, i] / s[i]
  
  loadings$.names = row.names(loadings)
  
  p + geom_text(data=loadings, mapping=aes(x = PC1, y = PC2, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC1", y = "PC2")
}

PCA_Plot_Psyc_Secondary = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = as.data.frame(unclass(pcaData$loadings))
  s = rep(0, ncol(loadings))
  for (i in 1:ncol(loadings))
  {
    s[i] = 0
    for (j in 1:nrow(loadings))
      s[i] = s[i] + loadings[j, i]^2
    s[i] = sqrt(s[i])
  }
  
  for (i in 1:ncol(loadings))
    loadings[, i] = loadings[, i] / s[i]
  
  loadings$.names = row.names(loadings)
  
  print(loadings)
  p + geom_text(data=loadings, mapping=aes(x = PC3, y = PC4, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC3", y = "PC4")
}

##################################################
#PCA/FA
##################################################

#Test KMO Sampling Adequacy

library(psych)
KMO(responses)
#Overall MSA =  0.91

#Test Bartlett's Test of Sphericity
library(REdaS)
bart_spher(responses)
#p-value < 2.22e-16 (Very Small Number)


#Test for Reliability Analysis using Cronbach's Alpha
library(psych)
alpha(responses,check.keys=TRUE)
#raw_alpha = 0.88
#Parallel Analysis (Horn's parallel analysis)

comp <- fa.parallel(responses)
comp

#######################################################

#Create PCA
p = prcomp(responses, center=T, scale=T)

#Check Scree Plot
plot(p)
abline(1, 0)

#Check PCA Summary Information
summary(p)
print(p)

########################################################

#Check PCA visualizations
plot(p) #Scree Plot
PCA_Plot(p) #PCA_plot1
PCA_Plot_Secondary(p) #PCA_Plot2
biplot(p) #Biplot





p2 = psych::principal(responses, rotate="varimax", nfactors=5, scores=TRUE)
p2
print(p2$loadings, cutoff=.5, sort=T) #no crossloading with .5

#PCAs Other Available Information

ls(p2)

p2$values
p2$communality
p2$rot.mat

########################################################################################

#Calculating scores

scores <- p2$scores
scores_1 <- scores[,1]
summary(scores_1)

scores_2 <- scores[,2]
summary(scores_2)

scores_3 <- scores[,3]
summary(scores_3)

scores_4 <- scores[,4]
summary(scores_4)

scores_5 <- scores[,5]
summary(scores_5)



#Conducting Factor Analysis

fit = factanal(responses, 5)
print(fit$loadings, cutoff=.5, sort=T)
summary(fit)
