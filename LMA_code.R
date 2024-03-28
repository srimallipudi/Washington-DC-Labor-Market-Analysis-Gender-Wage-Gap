#Remove objects (data1a) from your workspace
rm(list=ls(all=TRUE))
options(digits=12)
library(readxl)
library(car)

#read file
data2<-read.csv('LMA_Sample Data 02.10.csv', header = TRUE)

#Print variable names on the screen
colnames(data2)

#Interaction terms
female.private_emp <- ifelse(data2$Female == '1' & data2$COW == '1'|data2$COW == '2', 1, 0)
female.govt_emp <- ifelse((data2$Female == '1') & (data2$COW == '3'|data2$COW == '4'|data2$COW == '5'), 1, 0)
female.bachelors <- ifelse((data2$Female == '1') & (data2$SCHL == '20'|data2$SCHL == '21'), 1, 0)
female.masters <- ifelse(data2$Female == '1' & data2$SCHL == '22', 1, 0)
bachelors.govt_emp <- ifelse((data2$SCHL == '20'|data2$SCHL == '21') & (data2$COW == '3'|data2$COW == '4'|data2$COW == '5'), 1, 0)
bachelors.private_emp <- ifelse(data2$SCHL == '20'|data2$SCHL == '21' & data2$COW == '1'|data2$COW == '2', 1, 0)
married.gov_emp <- ifelse(data2$MAR == '1' & data2$COW == '3'|data2$COW == '4'|data2$COW == '5', 1, 0)
female.married.govt_emp <- ifelse(data2$Female == '1' & data2$MAR == '1' & data2$COW == '3'|data2$COW == '4'|data2$COW == '5', 1, 0)
female.married <- ifelse(data2$Female == '1' & data2$MAR == '1', 1,0)
married.bachelors <- ifelse(data2$MAR == '1' & data2$SCHL == '20'|data2$SCHL == '21', 1,0)

# Regression Model
LogEarnings.Equation = lm(log(Wages.past.12.months, base = exp(1)) ~ Age + I(Age*Age) + Female + female.bachelors + female.govt_emp + Married + High.School.or.Some.College + Associates.or.Bachelor.s.Degree + Master.s.Degree + Doctorate.degree + Weekly.hours.worked + Private.Employee + Government.employee,data=data2)
summary(LogEarnings.Equation)

LogEarnings.Equation2 = lm(log(Wages.past.12.months, base = exp(1)) ~ Age + I(Age*Age) + Married + High.School.or.Some.College + Associates.or.Bachelor.s.Degree + Master.s.Degree + Doctorate.degree + Weekly.hours.worked + Private.Employee + Female*Government.employee,data=data2)
summary(LogEarnings.Equation2)

# Checking VIF
vif(LogEarnings.Equation2, type = "predictor")
