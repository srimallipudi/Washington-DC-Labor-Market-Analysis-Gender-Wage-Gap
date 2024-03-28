#Sample R Program for Estimating a MR Model

#Remove objects (data1a) from your workspace
rm(list=ls(all=TRUE))
options(digits=12)
library(readxl)
#read file
data1<-read.csv(file.choose())
#Print variable names on the screen
colnames(data1)

summary(data1)
round(cor(data1),3)

#Descriptive statistics for quantitative variables of interest
#DS for Wages.or.Salary.Past.12.months
min(data1$Wages.or.Salary.Past.12.months,na.rm=TRUE)
max(data1$Wages.or.Salary.Past.12.months,na.rm=TRUE)
round(mean(data1$Wages.or.Salary.Past.12.months,na.rm=TRUE),1)
round(median(data1$Wages.or.Salary.Past.12.months,na.rm=TRUE),1)
round(sd(data1[,"Wages.or.Salary.Past.12.months"]),1)

#DS for Hours.worked.per.Week
min(data1$Hours.worked.per.Week,na.rm=TRUE)
max(data1$Hours.worked.per.Week,na.rm=TRUE)
round(mean(data1$Hours.worked.per.Week,na.rm=TRUE),1)
round(median(data1$Hours.worked.per.Week,na.rm=TRUE),1)
round(sd(data1[,"Hours.worked.per.Week"]),1)

#DS for Age
min(data1$Age,na.rm=TRUE)
max(data1$Age,na.rm=TRUE)
round(mean(data1$Age,na.rm=TRUE),1)
round(median(data1$Age,na.rm=TRUE),1)
round(sd(data1[,"Age"]),1)

#Histogram for Quantitative variables of interest
hist(data1$Wages.or.Salary.Past.12.months)
hist(data1$Wages.or.Salary.Past.12.months,
     main="Histogram for Wages/Salary for Past 12months",
     ylim=c(0,3500),
     xlim=c(31200,787000),
     xlab = "Wages/Salary")

hist(data1$Hours.worked.per.Week,
     main="Histogram for Hour worked per week",
     ylim=c(0,3000),
     xlim=c(0,100),
     xlab = "Weekly hours worked")

hist(data1$Age,
     main="Histogram for Person's age from 18-70 years",
     ylim=c(0,900),
     xlim=c(0,80),
     xlab = "Age")



#Bar charts/graph for qualitative variables of interest
library(ggplot2)
#ggplot(data1, aes(x= factor(SCHL1)))+geom_bar()

#BarChart for SCHL
ggplot(data1, aes(x= factor(Educational.Attainment)))+geom_bar()
#SCHL - dummy SCHL1
counts <- table(data1$Educational.Attainment)
counts <- cbind(counts, c(0,1,20,21,22,23,24))
barplot(counts[order(counts[,2])],ylim=c(0,2000),main="Educational Attainment", names.arg=c("NoHSD","HSD","Assoc","Bchl","MA","Prof","Doc"))
barplot()

#BarChart for MAR
counts <- table(data1$Marital.Status)
counts <- cbind(counts, c(1,2,3,4,5))
barplot(counts[order(counts[,2])],main="Marital status",ylim= c(0,4000) ,names.arg=c("Married","Widowed","Divorce","Sep","Unmar"))
barplot()
#MAR - DUMMY MARRIED
counts <- table(data1$Married)
counts <- cbind(counts, c(0,1))
barplot(counts[order(counts[,2])],main="Marital status",ylim= c(0,4000),names.arg=c("Otherwise","Married"))
barplot()

#BarChart for FER
counts <- table(data1$FER)
counts <- cbind(counts, c(0,1,2))
barplot(counts[order(counts[,2])],ylim= c(0,2500),main="Gave birth to child in past 12 months", names.arg=c("NA","Yes","No"))
barplot()
#FER - DUMMY CHILD BIRTH
counts <- table(data1$Gave.Birth.to.child.within.the.past.12.months)
counts <- cbind(counts, c(0,1))
barplot(counts[order(counts[,2])],ylim= c(0,7000),main="Gave birth to child in past 12 months", names.arg=c("NA/NO","Yes"))
barplot()

#BarChart for SEX
counts <- table(data1$Sex)
counts <- cbind(counts, c(1,2))
barplot(counts[order(counts[,2])],ylim= c(0,4000),main="SEX Ratio", names.arg=c("Male", "Female"))
barplot()

#SEX - DUMMY FEMALE
counts <- table(data1$Female)
counts <- cbind(counts, c(0,1))
barplot(counts[order(counts[,2])],main="SEX Ratio", names.arg=c("Male", "Female"))
barplot()

counts <- table(data1$Female.married)
counts <- cbind(counts, c(0,1))
barplot(counts[order(counts[,2])],main="Female married", names.arg=c("Otherwise", "Married female"))
barplot()

female.govt_emp <- ifelse(data1$Female == '1' & data1$COW == '3'|data1$COW == '4'|data1$COW == '5', 1, 0)
head(female.govt_emp,50)


#female.Weeklyhours.married <- ifelse(data1$Female == '1' & data1$Weekly.hours.worked.40  == '1'|data1$COW == '3'|data1$COW == '4'|data1$COW == '5', 1, 0)
#head(female.Weeklyhours.married,50)


LogEarnings.Equation = lm(log(Wages.past.12.months, base = exp(1)) ~ Age + I(Age*Age) + Female + female.govt_emp + Married + High.School.or.Some.College + Associate.s.Degree + Bachelor.s.Degree + Master.s.Degree + Doctorate.degree + Weekly.hours.worked + Employee_Private.for.profit.organization+ Employee_.Private.not.for.profit.organization + Government.employee+ Self.employed_.own.not.incorporated.business+ Self.employed_own.incorporated.business ,data=data1)
summary(LogEarnings.Equation)




#Cross Tabulation and Scatterplots
xtabs(~ data1$Wages.or.Salary.Past.12.months + data1$Sex) #count of male and female for each value of wAges 

plot(data1$Hours.worked.per.Week,data1$Wages.or.Salary.Past.12.months,ylim=c(0,800000),main="Earning by Hours.worked.per.Week",xlab="Hours worked per Week",ylab="Wages/Salary past 12months")# Earning by Hours.worked.per.Week
plot(data1$Age,data1$Wages.or.Salary.Past.12.months,ylim=c(0,800000),main="Earning by Age",xlab="Age",ylab="Wages/Salary past 12months")# Earning by Hours.worked.per.Week

plot(data1$Sex,data1$Wages.or.Salary.Past.12.months,ylim=c(0,800000),main="Earning by Age",xlab="Age",ylab="Wages/Salary past 12months")# Earning by Hours.worked.per.Week



DS<- lm (Wages.or.Salary.Past.12.months ~ Female + Married + No.High.School.Degree + High.School.Degree.or.GED.or.1.year + Associates.Degree+Bachelors.Degree + Masters.Degree+Professional.Degree+ Doctorate + Hours.worked.per.Week + Age + Employed.or.in.labour.force, data1a = data1)
#DS<- lm(Wages.or.Salary.Past.12.months ~ Female + Married +Hours.worked.per.Week + Age + Employed.or.in.labour.force + SCHL1+ Child.birth.in.last.12.months, data1a = data1)
summary(DS)
#Impose your sample selection criteria; for example the code below removes employed individuals with zero earnings and those who are not employed
#cor(cbind(data1$Wages.or.Salary.Past.12.months,data1$Hours.worked.per.Week,data1$Age,data1$Female,data1$Child.birth.in.last.12.months,data1$Married ,data1$SCHL1 , data1$Employed.or.in.labour.force))
#employedwithpay <- subset(data1, Earnings.Past.12.Months > 0 & Employed == 1) #note the use of == rather than =
#Generate select correlation coefficients 
round(cor(cbind(data1$Wages.or.Salary.Past.12.months,data1$Hours.worked.per.Week, data1$Age)),3)

#Examine the first 10 observations to make sure data1a set looks ok
head(employedwithpay,10)

#Estimate MR Model
Earnings.Equation = lm(Earnings.Past.12.Months ~ Age + Female + High.School.Degree.or.GED + Some.College + Associates.Degree + Bachelors.Degree + Masters.Degree + Professional.Degree, data1a=employedwithpay)
summary(Earnings.Equation)
library(ggplot2)
qqplot(data1$Educational.Attainment,data1$Wages.or.Salary.Past.12.months)
#=========#
#== END ==#
#=========#