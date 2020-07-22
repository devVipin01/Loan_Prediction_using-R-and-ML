library(dplyr)   #Library for select() for selecting columns from a dataframe
library(dummies) #Library for one-hot encoding
library(caTools) #Library for splitting dataframe into train and test
library(e1071)   #Library for svm algorithm
#replacing empty values with na
data<-read.csv('D:\\R_Machine_Learning\\train_loan.csv', na.strings = "")

----------------------------------------------------------------------------------------------


#dropping Loan_ID column
data<-select(data,-('Loan_ID'))


#Checking datatype of each column of dataframe
sapply(data, class)

#obtaining missing values in each columns
colSums(is.na(data))

-----------------------------------------------------------------------------------------------

#Replacing numerical column with mean values
data$LoanAmount[is.na(data$LoanAmount)]<- mean(data$LoanAmount,na.rm = TRUE)
data$Loan_Amount_Term[is.na(data$Loan_Amount_Term)]<-mean(data$Loan_Amount_Term, na.rm=TRUE)

-----------------------------------------------------------------------------------------------

#calculating frequency of Credit_history feature
table(data$Credit_History)

#since highest is 1, so replacing the missing values with 1
data$Credit_History[is.na(data$Credit_History)]<-1

----------------------------------------------------------------------------------------------

#creating getmode function to obtain mode
getmode <- function(v, na.rm) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#replacing na values in factor/categorical columns
data$Gender[is.na(data$Gender)]<-getmode(data$Gender, na.rm=TRUE)

data$Dependents[is.na(data$Dependents)]<-getmode(data$Dependents, na.rm = TRUE)
data$Self_Employed[is.na(data$Self_Employed)]<-getmode(data$Self_Employed, na.rm=TRUE)
data$Married[is.na(data$Married)]<-getmode(data$Married, na.rm=TRUE)

colSums(is.na(data))

----------------------------------------------------------------------------------------------

#using dplyr library to drop columnn from a dataframe


# Drop the columns of the dataframe using select function where - specifies the columns to be removed from the dataframe
x=select (data,-(Loan_Status))

#initializing the Loan_Status column to y
y=select(data,(Loan_Status))

----------------------------------------------------------------------------------------------

#one-hot encoding of categorical variable of x
x_dummy=dummy.data.frame(x, sep = "_")

----------------------------------------------------------------------------------------------

#caTools package contains sample.split()

# splitting x and y 
# 75% data assigned true value and 25% data assigned false value

split_xy<-sample.split(c(x_dummy,y), SplitRatio=0.75)

#splitting data into train_data and test_data

----------------------------------------------------------------------------------------------

#All the observations which have True value are assigned to x_train and y_train
x_train<-subset(x_dummy,split_xy==T)
y_train<-subset(y,split_xy==T)

#All the observations which have False value are assigned to x_test and y_train
x_test<-subset(x_dummy,split_xy==F)
y_test<-subset(y,split_xy==F)

----------------------------------------------------------------------------------------------
#installing e1071 package for svm() algorith
#install.packages('e1071')
#use library('e1071')


x_combine <- cbind(x_train,y_train)

# Fitting model in svm (support vector machine) 
fit <-svm(y_train$Loan_Status ~ ., data = x_combine)

#summary(fit)

#Predict Output 
p_svm= predict(fit,x_test)
p_svm[1:5]

#comparing the predicted output with actual output

final_svm<- cbind(Actual=y_test,Predicted=p_svm)
final_svm<-as.data.frame(final_svm)

#calculating score of the model
mean(y_test$Loan_Status==p_svm)
---------------------------------------------------------------------------------------------
  
#applying logistic regression algorithm
logistic <- glm(y_train$Loan_Status ~ ., data = x_combine,family='binomial')

#summary(logistic)

#Predict Output
p_logistic= predict(logistic,x_test,type = "response")
p_logistic
#p_pred_num is 1 if p_logistic is >0.6 other wise zero
p_pred_num <- ifelse(p_logistic > 0.6, 1, 0)

status<-ifelse(p_pred_num==1,'Y','N')

final_logistic<- cbind(Actual=y_test,Predicted=status)
final_logistic<-as.data.frame(final_logistic)

#y_act

#calculating score of the model
mean(status == y_test$Loan_Status) 

----------------------------------------------------------------------------------------------
