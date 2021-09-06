#---------------Logistic Regression Modelling----#

#-------->Basic Functional Form:
#P(Y=1)=e^Z/(1+e^Z), e refers to exponential
#where Z=B0+B1X1+B2X2+..........+BNXN

#Problem Statement: 
#to Predict who is likely going to click on the Advertisement 
#so it can contribute to the more revenue generation to the organization (Clicked=1)

#-------Preparing the environment for Logistic Regression-------#

list.of.packages <- c("caret", "ggplot2","MASS","car","mlogit","caTools","sqldf","Hmisc",
                      "aod","BaylorEdPsych","ResourceSelection","pROC","ROCR","Metrics")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")

library(caret)# LOGISTIC MODEL
library(ggplot2)# VISUALIZATION
library(MASS)# VIF CALCULATION
library(car)# VIF CALCULATION
library(mlogit)# LOGISTIC MODEL
library(sqldf)#WOE & IV
library(Hmisc)#WOE & IV
library(aod)#WALD TEST
library(BaylorEdPsych)#R-SQUARE
library(ResourceSelection)#HOSMER LEMESHOW TEST
library(pROC)#ROC CURVE
library(ROCR)#ROC CURVE
library(caTools)#TRAIN AND TEST SPLIT
library(mgcv)# MODEL TEST
library(MLmetrics) #f1

#----Setting the Working Directory-----#
Path<-"E:/IVY/Stat+R/Project/logistic reg"
setwd(Path)
getwd()

#---------read data
data<-read.csv("Web_data.csv",header = TRUE)
data1=data#To create a backup of original data
head(data1)

#-----Basic Exploration of the data-----------# 
str(data1)
summary(data1)
dim(data1)

#---change some variables as factor variable------#
names<-c("City_code","Male","Time_Period","Weekday","Month","Year","Clicked")
data1[,names]<-lapply(data1[,names],factor)
str(data1)

#----Missing Value Treatment (if any)---#
data.frame(colSums(is.na(data1)))
# no missing values

#----->Outlier checking for continuous variable
# boxplot (Time_Spent, Avg_Income, Internet_Usage)
boxplot(data1$Time_Spent, horizontal = T)
boxplot(data1$Avg_Income, horizontal = T)
boxplot(data1$Internet_Usage, horizontal = T)

#for Avg_Income
quantile(data1$Avg_Income,c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,
                                 0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99,1))
# not as such outliers are present

# -------------- Univariate analysis
# Time_Spent col
hist(data1$Time_Spent, xlab= "Time_Spent Distribution", col = "green", border = "black")
# Age col
hist(data1$Age, xlab= "Age Distribution", col = "green", border = "black")
# Avg_Income col
hist(data1$Avg_Income, xlab= "Avg_Income Distribution", col = "green", border = "black")
# Internet_Usage col
hist(data1$Internet_Usage, xlab= "Internet_Usage Distribution", col = "green", border = "black")
# Year col
hist(data$Year, xlab= "Year Distribution", col = "green", border = "black") 
# picked data due to the factor variable in data1

# clicked col : target var
hist(data$Clicked, xlab= "Clicked Distribution", col = "green", border = "black") 
# target variable distribution, picked data due to the factor variable in data1  

# -----------------Bivariate analysis
# Time_Spent col with target var clicked
plot(data1$Clicked,data1$Time_Spent, main = "Time_Spent vs Click",
     xlab = "Clicked", ylab = "Time_Spent")
#scatter
ggplot(data1) + geom_point(aes(x = Clicked, y = Time_Spent))

# Age col with target var clicked
plot(data1$Clicked,data1$Age, main = "Age vs Click",
     xlab = "Clicked", ylab = "Age")

#scatter
ggplot(data1) + geom_point(aes(x = Clicked, y = Age))

# Avg_Income col with target var clicked
plot(data1$Clicked,data1$Avg_Income, main = "Avg_Income vs Click",
     xlab = "Clicked", ylab = "Avg_Income")

#scatter
ggplot(data1) + geom_point(aes(x = Clicked, y = Avg_Income))

# Internet_Usage col with target var clicked
plot(data1$Clicked,data1$Internet_Usage, main = "Internet_Usage vs Click",
     xlab = "Clicked", ylab = "Internet_Usage")

#scatter
ggplot(data1) + geom_point(aes(x = Clicked, y = Internet_Usage))

#-------Information Value Calculation (A variable reduction technique)---------#
#--------> Creating two data sets for numeric and categorical values

## Data set with numeric variable
#(ignored the visitor id col that is not relevant)
num <- data1[,c(2:5,14)]#Numerical Data Frame
cat <- data1[,c(6:14)]#Categorical Data Frame
head(cat)
head(num)
str(num)
str(cat)

# -------- Feature selection
#--------IV for numeric data-------#
IV_num <- function(variable, target,data,groups)
{
  data[,"rank"] <- cut2(data[,variable],g=groups)
  tableOutput <-sqldf(sprintf("select rank, count(%s) n, sum(%s) good from data 
                              group by rank",target,target))
  tableOutput <- sqldf("select *, (n - good) bad from tableOutput")
  tableOutput$bad_rate<- tableOutput$bad/sum(tableOutput$bad)*100
  tableOutput$good_rate<- tableOutput$good/sum(tableOutput$good)*100
  tableOutput$WOE<- (log(tableOutput$good_rate/tableOutput$bad_rate))*100
  tableOutput$IV <- (log(tableOutput$good_rate/tableOutput$bad_rate))*
    (tableOutput$good_rate-tableOutput$bad_rate)/100
  IV <- sum(tableOutput$IV[is.finite(tableOutput$IV)])
  IV1 <- data.frame(cbind(variable,IV))
  return(IV1)
}

n1<- IV_num("Time_Spent","Clicked",num,groups=10)
n2<- IV_num("Age","Clicked",num,groups=10)
n3<- IV_num("Avg_Income","Clicked",num,groups=10)
n4<- IV_num("Internet_Usage","Clicked",num,groups=10)

IV_numerical<- data.frame(rbind(n1,n2,n3,n4))

#------Information Value for categorical data------------#

IV_Cat <- function(target, variable, data) {
  A1<- fn$sqldf("select $variable,count($target)n, sum($target)good from data group by $variable")
  A1<- fn$sqldf("select *, (n-good) bad from A1")
  A1$bad_rate <- A1$bad/sum(A1$bad)*100
  A1$good_rate<- A1$good/sum(A1$good)*100
  A1$WOE<- (log(A1$good_rate/A1$bad_rate))*100
  A1$IV <- (log(A1$good_rate/A1$bad_rate))*(A1$good_rate-A1$bad_rate)/100
  IV <- sum(A1$IV[is.finite(A1$IV)])
  IV1 <- data.frame(cbind(variable,IV))
  return(IV1)
}
c1<- IV_Cat("Clicked","Ad_Topic",cat)
c2<- IV_Cat("Clicked","Country_Name",cat)
c3<- IV_Cat("Clicked","City_code",cat)
c4<- IV_Cat("Clicked","Male",cat)
c5<- IV_Cat("Clicked","Time_Period",cat)
c6<- IV_Cat("Clicked","Weekday",cat)
c7<- IV_Cat("Clicked","Month",cat)
c8<- IV_Cat("Clicked","Year",cat)

IV_categorical<- data.frame(rbind(c1,c2,c3,c4,c5,c6,c7,c8))

# final IV dataframe
Final_IV <- data.frame(rbind(IV_numerical,IV_categorical))

#extracting the same
write.csv(Final_IV,"Final_IV.csv",row.names =FALSE)
#threshold value is >.01

#ANOVA Test
# using data not data1 due to factor variables
summary(aov(Clicked ~ data$VistID, data=data)) # not significant
summary(aov(Clicked ~ data$Time_Spent, data=data)) # significant
summary(aov(Clicked ~ data$Age, data=data)) # significant
summary(aov(Clicked ~ data$Avg_Income, data=data)) # significant
summary(aov(Clicked ~ data$Internet_Usage, data=data)) # significant
summary(aov(Clicked ~ data$Ad_Topic, data=data)) # significant
summary(aov(Clicked ~ data$Country_Name, data=data)) # significant
summary(aov(Clicked ~ data$City_code, data=data)) # significant
summary(aov(Clicked ~ data$Male, data=data)) # significant
summary(aov(Clicked ~ data$Time_Period, data=data)) # significant
summary(aov(Clicked ~ data$Weekday, data=data)) # not significant
summary(aov(Clicked ~ data$Month, data=data)) # not significant
summary(aov(Clicked ~ data$Year, data=data)) # not significant
# All cols not have good p values

# Based on anova test and information value, final dataframe is given below
# it will be used for modeling
# removing VistID, weekday, month and year.
final_data <-sqldf("select Time_Spent,Age,Avg_Income,Internet_Usage,Ad_Topic,Country_Name,
                   City_code,Male,Time_Period,Clicked from data1")
#extracting the same
write.csv(final_data,"logReg_finalDataModel.csv",row.names =FALSE)

#----------Splitting the data into training and test data set------#

set.seed(144)#This is used to produce reproducible results, everytime we run the model
spl = sample.split(final_data$Clicked, 0.7)
data.train = subset(final_data, spl == TRUE)
str(data.train)
dim(data.train)

data.test = subset(final_data, spl == FALSE)
str(data.test)
dim(data.test)

#-----Logistic Regression Model Building---------#
model <- glm(Clicked~., data=data.train, family=binomial())
summary(model)
  
# Remove the insignificant variable one by one(starting from country name)
model1 <- glm(Clicked~ Time_Spent+ Age+ Avg_Income+ Internet_Usage+ I(Ad_Topic=="product_3")+
                I(Ad_Topic=="product_8")+ I(Ad_Topic=="product_21")+ I(Ad_Topic=="product_28")+
                I(City_code=="City_2")+ I(City_code=="City_3")+ I(City_code=="City_4")+ 
                I(City_code=="City_5")+ I(City_code=="City_6")+ I(City_code=="City_7")+
                I(Time_Period=="Mid-Night")+ I(Time_Period=="Morning"), 
                data=data.train, family=binomial())
summary(model1)
# model1 is the best model till now based on p values

#Multicollinearity check
vif(model1)
# it is giving good result, all variables are under threshold


#--------->using Wald Test
wald.test(b=coef(model1), Sigma= vcov(model1), Terms=1:9)
#Here Terms, no. of independent variables in your final train model
#Since, p-value is less then 0.001, hence we reject Ho that the all Bi=0

#----->Lagrange Multiplier or Score Test (Assess weather the current variable 
#significantly improves the model fit or not)

# Difference between null deviance and deviance
modelChi <- model1$null.deviance - model1$deviance
#4410.85

#Finding the degree of freedom for Null model and model with variables
chidf <- model1$df.null - model1$df.residual
#16

# With more decimal places
# If p value is less than .05 then we reject the null hypothesis that the model is no better than chance
chisq.prob <- 1 - pchisq(modelChi, chidf)
format(round(chisq.prob, 2), nsmall = 4)
#0.0000, hence very good

#----Predictions
# Make predictions
probabilities <- model1 %>% predict(data.test, type = "response")
predicted.classes <- ifelse(probabilities> 0.5, 1, 0)

# Model Accuracy
mean(predicted.classes == data.test$Clicked) #93%

# R2 value
R2(probabilities, as.numeric(data.test$Clicked)) #78%

#file extraction
data.test$Pred <- ifelse(probabilities> 0.5, 1, 0)
write.csv(data.test,"prediction.csv",row.names =FALSE)

#Accuracy measures
# auc roc
rocCurve   <- roc(response = data.test$Clicked, predictor = probabilities, 
                  levels = rev(levels(data.test$Clicked)))
#Metrics - Fit Statistics
predclass <-ifelse(probabilities>coords(rocCurve,"best",transpose = TRUE)[1],1,0)
Confusion <- table(Predicted = predclass,Actual = data.test$Clicked)
AccuracyRate <- sum(diag(Confusion))/sum(Confusion) #93
Gini <-2*auc(rocCurve)-1
#all
AUCmetric <- data.frame(c(coords(rocCurve,"best",transpose = TRUE),AUC=auc(rocCurve),
                          AccuracyRate=AccuracyRate,Gini=Gini))
AUCmetric <- data.frame(rownames(AUCmetric),AUCmetric)
rownames(AUCmetric) <-NULL
names(AUCmetric) <- c("Metric","Values")
AUCmetric # contains Sensitivity, Specificity, etc.
#confusion matrix
Confusion 

# all via function
#error metrics -- Confusion Matrix
err_metric=function(CM)
{
  TN =CM[1,1]
  TP =CM[2,2]
  FP =CM[1,2]
  FN =CM[2,1]
  precision =(TP)/(TP+FP)
  recall_score =(FP)/(FP+TN)
  f1_score=2*((precision*recall_score)/(precision+recall_score))
  accuracy_model  =(TP+TN)/(TP+TN+FP+FN)
  print(paste("Precision value of the model: ",round(precision,2)))
  print(paste("Accuracy of the model: ",round(accuracy_model,2)))
  print(paste("Recall value of the model: ",round(recall_score,2)))
  print(paste("f1 score of the model: ",round(f1_score,2)))
}
# call function
CM= table(data.test[,10], predicted.classes)
err_metric(CM)

#graph
plot(rocCurve)

# Variable Importance of the model
varImp(model1)

# coefficient
summary(model1)$coef
# Coefficients (Odds) same as above
model1$coefficients
# Coefficients (Odds Ratio)
exp(model1$coefficients)#Interpret (main)

###################################