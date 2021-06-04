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
library(mice)
##############################################################################################

#Set Working Directory
setwd('E:/DPU/Summer I/Advance Data Analysis/project')
#Read in responsess

responses <- read.csv(file="CPS_.csv", header=TRUE, sep=",",na.strings = "NDA") 


#Check Sample Size and Number of Variables
dim(responses)
head(responses)

names(responses)
summary(responses)


responses[,24:28] <- sapply(responses[,24:28],as.numeric)
responses[,29:36] <- sapply(responses[,29:36],as.numeric)
responses[,37:48] <- sapply(responses[,37:48],as.numeric)

responses$Elementary..Middle..or.High.School<-as.factor(responses$Elementary..Middle..or.High.School)
#For All Variables - 9865 missing values 
sum(is.na(responses))

#============================== DATA PREPARATION ==============================
# Import the data set with NDA as NA
responses <- responses[,-c(1,2,54,55)]
summary(responses)
# Changing all the categorical values into factor
responses$Elementary..Middle..or.High.School<-as.factor(responses$Elementary..Middle..or.High.School)
responses$CPS.Performance.Policy.Status<-as.factor(responses$CPS.Performance.Policy.Status)
responses$Healthy.Schools.Certified. <- as.factor(responses$Healthy.Schools.Certified.)
responses$`Safety.Icon` <- as.factor(responses$`Safety.Icon`)
responses$`Environment.Icon` <- as.factor(responses$`Environment.Icon`)
responses$`Instruction.Icon` <- as.factor(responses$`Instruction.Icon`)
responses$`Leaders.Icon` <- as.factor(responses$`Leaders.Icon`)
responses$`Teachers.Icon` <- as.factor(responses$`Teachers.Icon`)
responses$`Parent.Engagement.Icon` <- as.factor(responses$`Parent.Engagement.Icon`)
responses$`Parent.Environment.Icon` <- as.factor(responses$`Parent.Environment.Icon`)
summary(responses)

# Changing all the NA's with Mean
responses$`Safety.Score`[which(is.na(responses$`Safety.Score`))] = mean(responses$`Safety.Score`,na.rm = TRUE)
responses$`Family.Involvement.Score`[which(is.na(responses$`Family.Involvement.Score`))] = mean(responses$`Family.Involvement.Score`,na.rm = TRUE)
responses$`Environment.Score`[which(is.na(responses$`Environment.Score`))] = mean(responses$`Environment.Score`,na.rm = TRUE)
responses$`Instruction.Score`[which(is.na(responses$`Instruction.Score`))] = mean(responses$`Instruction.Score`,na.rm = TRUE)
responses$`Leaders.Score`[which(is.na(responses$`Leaders.Score`))] = mean(responses$`Leaders.Score`,na.rm = TRUE)



responses$`Teachers.Score`[which(is.na(responses$`Teachers.Score`))] = mean(responses$`Teachers.Score`,na.rm = TRUE)
responses$`Parent.Engagement.Score`[which(is.na(responses$`Parent.Engagement.Score`))] = mean(responses$`Parent.Engagement.Score`,na.rm = TRUE)
responses$`Parent.Environment.Score`[which(is.na(responses$`Parent.Environment.Score`))] = mean(responses$`Parent.Environment.Score`,na.rm = TRUE)
responses$`Average.Student.Attendance`[which(is.na(responses$`Average.Student.Attendance`))] = mean(responses$`Average.Student.Attendance`,na.rm = TRUE)
responses$`Average.Teacher.Attendance`[which(is.na(responses$`Average.Teacher.Attendance`))] = mean(responses$`Average.Teacher.Attendance`,na.rm = TRUE)



responses$`Individualized.Education.Program.Compliance.Rate`[which(is.na(responses$`Individualized.Education.Program.Compliance.Rate`))] = mean(responses$`Individualized.Education.Program.Compliance.Rate`,na.rm = TRUE)
responses$Pk.2.Literacy..[which(is.na(responses$Pk.2.Literacy..))] = mean(responses$Pk.2.Literacy..,na.rm = TRUE)
responses$Pk.2.Math..[which(is.na(responses$Pk.2.Math..))] = mean(responses$Pk.2.Math..,na.rm = TRUE)

responses$Gr3.5.Grade.Level.Math..[which(is.na(responses$Gr3.5.Grade.Level.Math..))] = mean(responses$Gr3.5.Grade.Level.Math..,na.rm = TRUE)

responses$Gr3.5.Grade.Level.Read..[which(is.na(responses$Gr3.5.Grade.Level.Read..))] = mean(responses$Gr3.5.Grade.Level.Read..,na.rm = TRUE)
responses$Gr3.5.Keep.Pace.Read..[which(is.na(responses$Gr3.5.Keep.Pace.Read..))] = mean(responses$Gr3.5.Keep.Pace.Read..,na.rm = TRUE)


responses$Gr3.5.Keep.Pace.Math..[which(is.na(responses$Gr3.5.Keep.Pace.Math..))] = mean(responses$Gr3.5.Keep.Pace.Math..,na.rm = TRUE)
responses$Gr6.8.Grade.Level.Math..[which(is.na(responses$Gr6.8.Grade.Level.Math..))] = mean(responses$Gr6.8.Grade.Level.Math..,na.rm = TRUE)

responses$Gr6.8.Grade.Level.Read..[which(is.na(responses$Gr6.8.Grade.Level.Read..))] = mean(responses$Gr6.8.Grade.Level.Read..,na.rm = TRUE)
responses$Gr6.8.Keep.Pace.Math.[which(is.na(responses$Gr6.8.Keep.Pace.Math.))] = mean(responses$Gr6.8.Keep.Pace.Math.,na.rm = TRUE)

responses$Gr6.8.Keep.Pace.Read..[which(is.na(responses$Gr6.8.Keep.Pace.Read..))] = mean(responses$Gr6.8.Keep.Pace.Read..,na.rm = TRUE)

responses$Gr.8.Explore.Math..[which(is.na(responses$Gr.8.Explore.Math..))] = mean(responses$Gr.8.Explore.Math..,na.rm = TRUE)
responses$Gr.8.Explore.Read..[which(is.na(responses$Gr.8.Explore.Read..))] = mean(responses$Gr.8.Explore.Read..,na.rm = TRUE)


responses$ISAT.Exceeding.Math..[which(is.na(responses$ISAT.Exceeding.Math..))] = mean(responses$ISAT.Exceeding.Math..,na.rm = TRUE)
responses$ISAT.Exceeding.Reading..[which(is.na(responses$ISAT.Exceeding.Reading..))] = mean(responses$ISAT.Exceeding.Reading..,na.rm = TRUE,)

responses$ISAT.Value.Add.Math[which(is.na(responses$ISAT.Value.Add.Math))] = mean(responses$ISAT.Value.Add.Math,na.rm = TRUE)
responses$ISAT.Value.Add.Read[which(is.na(responses$ISAT.Value.Add.Read))] = mean(responses$ISAT.Value.Add.Read,na.rm = TRUE)
responses$Students.Taking..Algebra..[which(is.na(responses$Students.Taking..Algebra..))] = mean(responses$Students.Taking..Algebra..,na.rm = TRUE)


responses$Students.Passing..Algebra..[which(is.na(responses$Students.Passing..Algebra..))] = mean(responses$Students.Passing..Algebra..,na.rm = TRUE,)


responses$X9th.Grade.EXPLORE..2009.[which(is.na(responses$X9th.Grade.EXPLORE..2009.))] = mean(responses$X9th.Grade.EXPLORE..2009.,na.rm = TRUE)
responses$X9th.Grade.EXPLORE..2010.[which(is.na(responses$X9th.Grade.EXPLORE..2010.))] = mean(responses$X9th.Grade.EXPLORE..2010.,na.rm = TRUE)
responses$X10th.Grade.PLAN..2009.[which(is.na(responses$X10th.Grade.PLAN..2009.))] = mean(responses$X10th.Grade.PLAN..2009.,na.rm = TRUE)
responses$X10th.Grade.PLAN..2010.[which(is.na(responses$X10th.Grade.PLAN..2010.))] = mean(responses$X10th.Grade.PLAN..2010.,na.rm = TRUE)



responses$Net.Change.EXPLORE.and.PLAN[which(is.na(responses$Net.Change.EXPLORE.and.PLAN))] = mean(responses$Net.Change.EXPLORE.and.PLAN,na.rm = TRUE)
responses$X11th.Grade.Average.ACT..2011.[which(is.na(responses$X11th.Grade.Average.ACT..2011.))] = mean(responses$X11th.Grade.Average.ACT..2011.,na.rm = TRUE)

responses$Net.Change.PLAN.and.ACT[which(is.na(responses$Net.Change.PLAN.and.ACT))] = mean(responses$Net.Change.PLAN.and.ACT,na.rm = TRUE)

responses$College.Eligibility..[which(is.na(responses$College.Eligibility..))] = mean(responses$College.Eligibility..,na.rm = TRUE)

responses$Graduation.Rate..[which(is.na(responses$Graduation.Rate..))] = mean(responses$Graduation.Rate..,na.rm = TRUE)

responses$College.Enrollment.Rate..[which(is.na(responses$College.Enrollment.Rate..))] = mean(responses$College.Enrollment.Rate..,na.rm = TRUE)
responses$College.Enrollment..number.of.students.[which(is.na(responses$College.Enrollment..number.of.students.))] = mean(responses$College.Enrollment..number.of.students.,na.rm = TRUE)


summary(responses)


impute <- mice(responses[,c(2,3,4,5,7,9,11,13,15,17)],m=3,seed=123)
print(impute)
imputedValues<-complete(impute,1)
responses$CPS.Performance.Policy.Status<-imputedValues[1]
responses$Healthy.Schools.Certified.<-imputedValues[2]
responses$Safety.Icon<-imputedValues[3]
responses$Safety.Score<-imputedValues[4]
responses$Environment.Icon<-imputedValues[5]
responses$Instruction.Icon<-imputedValues[6]
responses$Leaders.Icon<-imputedValues[7]
responses$Teachers.Icon<-imputedValues[8]
responses$Parent.Engagement.Icon<-imputedValues[9]
responses$Parent.Environment.Icon<-imputedValues[10]
summary(responses)



#Check for Missing Values (i.e. NAs)

#For All Variables - 0 missing values 
sum(is.na(responses))

#Show column Numbers
names(responses)

#Show Structure of Dataset
str(responses, list.len=ncol(responses))

#removing categorial varibales
cpsContinous <- responses[,-c(1:4,7,9,11,13,15,17)]
grading_marks <- cpsContinous[,-c(1:12,38:41)] #has only student related levels

grading_marks2 <- cpsContinous[,-c(9:12,38:41)] 

#Show descriptive statistics

#Normality Rule of Thumb with Skewnewss and Kurtosis (think normal bell curve):
#Short Way:
#If skewnewss is close to 0, the distribution is normal.
#If Kurtosis is -3 or 3, the distribution is normal.

#If skewness is less than -1 or greater than 1, the distribution is highly skewed.
#If skewness is between -1 and -0.5 or between 0.5 and 1, the distribution is moderately skewed.
#If skewness is between -0.5 and 0.5, the distribution is approximately symmetric.


library(psych)
describe(grading_marks)



#Exploratory Analysis Graphing

#In order to use the graphing functions, you need to chunk the data into smaller subsets of data.

#GGpairs
p1 <- ggpairs(grading_marks)
p1

#Check for Multicollinearity with Correlations

M<-cor(grading_marks, method="spearman")
M

#personality
corrplot(cor(M,method="spearman"), method = "number", type = "lower")

#GGplot Correlation
ggcorr(grading_marks, method = c("pairwise","spearman"), label=TRUE)



# Run a correlation test to see how correlated the variables are.  Which correlations are significant
options("scipen"=100, "digits"=5)
round(cor(grading_marks), 2)
MCorrTest = corr.test(grading_marks, adjust="none")
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
KMO(grading_marks)
#Overall KSA =  0.85

#Test Bartlett's Test of Sphericity
library(REdaS)
bart_spher(grading_marks)
#p-value < 2.22e-16 (Very Small Number)


#Test for Reliability Analysis using Cronbach's Alpha
library(psych)
alpha(grading_marks,check.keys=TRUE)
#raw_alpha = 0.91
#Parallel Analysis (Horn's parallel analysis)

comp <- fa.parallel(grading_marks)
comp

#######################################################

#Create PCA
p = prcomp(grading_marks, center=T, scale=T)


#Check Scree Plot
plot(p)
abline(1, 0)


#Check PCA Summary Information
summary(p)
print(p)

########################################################

#Check PCA visualizations
PCA_Plot(p) #PCA_plot1
PCA_Plot_Secondary(p) #PCA_Plot2
biplot(p) #Biplot

library(GPArotation)
p2 = psych::principal(grading_marks, rotate="oblimin", nfactors=3, scores=TRUE)
colnames(p2$loadings)<-c("Current-academic-lvl","Career-scores-lvl ","Growth-target-lvl")
print(p2$loadings, cutoff=.5, sort=T) #no crossloading with .5


library("FactoMineR")
p4 <- PCA(grading_marks, graph = FALSE)
#IF graph is set to true, it will provide the individual and variable maps

#Shows all the objects or functions available in PCA
print(p4)

#Options for providing screeplot
fviz_eig(p4, addlabels = TRUE, ylim = c(0, 35))
fviz_screeplot(p4, addlabels = TRUE, ylim = c(0, 35))

variables <- get_pca_var(p4)

#Which variables contibute the most to the PCs?
#there are ll variables
head(variables$contrib, 10)

library("corrplot")
corrplot(variables$contrib, is.corr=FALSE)    

# Contributions of variables to PC1
fviz_contrib(p4, choice = "var", axes = 1, top = 10)



library(ade4)
p5 <- dudi.pca(responses2,
               scannf = FALSE,   # Hide scree plot
               nf = 15         # Number of components kept in the results
)
fviz_screeplot(p5, addlabels = TRUE, ylim = c(0, 35))

variables2 <- get_pca_var(p5)

#Which variables contibute the most to the PCs?
#there are ll variables
head(variables2$contrib, 15)

library("corrplot")
corrplot(variables2$contrib, is.corr=FALSE)    

fviz_contrib(p, choice = "var", axes = 1, top = 9)


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
colnames(scores)<-c("Current-academic-lvl","Career-scores-lvl ","Growth-target-lvl")

summary(scores_3)


#pca in further analysis
model1 <- lm(cpsContinous$College.Enrollment.Rate..~scores_1+scores_2+scores_3)
summary(model1)
