# Analysis of MBA SALARIES
# NAME: Nishtha Nayar
# EMAIL: nishthanayar1998@gmail.com
# COLLEGE / COMPANY: Shiv Nadar University

salaries.df<- read.csv(paste("MBA Starting Salaries Data.csv")) #Create dataframe
summary(salaries.df) #Analysing summary of data

#Analysis of individual variables
boxplot(salaries.df$age, main="Boxplot of Age of Graduates", ylab="Age", col="orange") #Analysing age
library(psych)
table(salaries.df$sex) #Analysing sex
boxplot(salaries.df$gmat_tot, main="Boxplot of Total GMAT Score", ylab="Score", col = "blue") #Analysing total GMAT score
boxplot(salaries.df$gmat_qpc, main="Boxplot of Quantitative GMAT Score", ylab="Score", col = "red") #Analysing Quant GMAT score
boxplot(salaries.df$gmat_vpc, main="Boxplot of Verbal GMAT Score", ylab="Score", col = "yellow") #Analysing Verbal GMAT score
boxplot(salaries.df$gmat_tpc, main="Boxplot of GMAT Percentile", ylab="Percentile", col = "lavender") #Analysing Verbal GMAT score
boxplot(salaries.df$s_avg, main="Spring MBA Average", ylab="Average", col="coral") #Analysing Average Spring MBA Score
boxplot(salaries.df$f_avg, main="Fall MBA Average", ylab="Average", col="gold") #Analysing Average Fall MBA Score
table(salaries.df$frstlang) #Analysing if English is 1st language
boxplot(salaries.df$work_yrs, main="Boxplot of Years of Experience",ylab="Years" )
boxplot(salaries.df$salary, main="Boxplot of Salary",ylab="Salary" )
boxplot(salaries.df$satis, main="Satisfaction with Program")

#Analysis using multiple variable
newdata <- salaries.df[which(salaries.df$salary !=999 & salaries.df$salary !=998),] #Excluding rows with no salary input 
 plot(newdata$salary, newdata$gmat_tot,
      main = "Scatterplot between Salary and GMAT Total",
      xlab="Salary", ylab="GMAT Tot") #Relation between salary and Gmat Total
 
 plot(newdata$salary, newdata$s_avg,
      main = "Scatterplot between Salary and Average Spring GPA",
      xlab="Salary", ylab="Avg Spring GPA") #Relation between salary and Avg Spring GPA
 
 plot(newdata$salary, newdata$f_avg,
      main = "Scatterplot between Salary and Average Fall GPA",
      xlab="Salary", ylab="Avg Fall GPA") #Relation between salary and Avg Fall GPA
 
 #Creating Covariance Matrix
 cov(newdata)
 
 #Creating Correlation Matrix
 cor(newdata)
 
 #Creating Corrgram
 library(corrgram)
 corrgram(newdata, lower.panel = panel.shade, upper.panel = panel.pie, 
          text.panel = panel.txt, main= "Corrgram of MBASalary variables")
 
 #Creating sub-set for graduates who got a job
 gotjob <- newdata[which(newdata$salary != 0),]
 
 #Creating Contingency tables
 table1 <- xtabs(~salary+sex+gmat_tot, data=gotjob)
 ftable(table1)
 
 table2 <- xtabs(~salary+sex, data=gotjob)
 ftable(table2)
 
 table3 <-xtabs(~sex, data=gotjob)
 table3
 
 table4 <- xtabs(~s_avg, data=gotjob)
 table4
 
 table5 <-xtabs(~f_avg, data=gotjob)
 table5
 
 #Chi-square tests
 chisq.test(table2) #Null hypothesis: Sex and Salary independent
 
 table6 <-xtabs(~salary+gmat_tot, data=gotjob)
 chisq.test(table6) #Null hypothesis: GMAT Total and salary independent
 
 table7 <-xtabs(~salary+age, data=gotjob)
 chisq.test(table7)#Null hypothesis: Age and Salary Independent
 
 table8 <-xtabs(~salary+satis, data=gotjob)
 chisq.test(table8)#Null hypothesis: MBA program satisfaction and salary independent
 
 table9 <- xtabs(~salary+work_yrs, data = gotjob)
 chisq.test(table9) #Null hypothesis: Work yrs and Salary independent
 
 #t-tests
 t.test(gotjob$s_avg,gotjob$salary) #Null hypothesis: Spring avg and Salary independent
 t.test(gotjob$f_avg,gotjob$salary) #Null hypothesis: Fall avg and Salary independent
 
 #Forming regression models
 model1 <- lm(salary ~ gmat_tot + gmat_qpc + gmat_vpc + s_avg + f_avg, data=gotjob)
 summary(model1)
 coef(model1)
 
 model2 <- lm(salary ~ gmat_tot + work_yrs + s_avg + f_avg, data=gotjob)
 summary(model2)
 coef(model2)
 
 model3 <- lm(salary ~ gmat_qpc + gmat_vpc + work_yrs + s_avg + f_avg + quarter, data=gotjob)
 summary(model3)
 coef(model3)
 
 #Comparing those who got a job and those who did not.

 newdata$salary[newdata$salary != 0] <- 1 #Making salary data type to be binary with 0: No job, 1: Got job
 
 #Contingency Tests

 table01 <- xtabs(~salary + gmat_tot, data=newdata) #between getting job and GMAT total
 ftable(table01)

 table02 <- xtabs(~salary + work_yrs, data=newdata) #between getting job and work experience(yrs)
 ftable(table02)
 
 table03 <- xtabs(~salary + sex, data=newdata) #between getting job and gender
 ftable(table03)
 
 table04 <- xtabs(~salary + s_avg, data=newdata) #between getting job and spring average
 ftable(table04)

 table05 <- xtabs(~salary + f_avg, data=newdata) #between getting job and fall average
 ftable(table05)
 
 table06 <- xtabs(~salary + frstlang, data=newdata) #between getting job and first language 
 ftable(table06)
 
 table07 <- xtabs(~salary + satis, data=newdata) #between getting job and satisfaction with MBA program
 ftable(table07)
 
 table08 <- xtabs(~salary + quarter, data=newdata) #between getting job and quarter ranking
 ftable(table08)
 
 #Chi-square test
 
 chisq.test(table01) 
 chisq.test(table02)
 chisq.test(table03)
 chisq.test(table04)
 chisq.test(table05)
 chisq.test(table06)
 chisq.test(table07)
 chisq.test(table08)
 
 #t-tests
 
 t.test(newdata$gmat_tot ~newdata$salary ) #between getting job and GMAT total
 t.test(newdata$satis ~newdata$salary ) #between getting job and satisfaction with MBA program
 t.test(newdata$work_yrs ~newdata$salary ) #between getting job and work experience(yrs)
 t.test(newdata$sex ~newdata$salary )#between getting job and gender
 t.test(newdata$s_avg ~newdata$salary )#between getting job and spring average
 t.test(newdata$f_avg ~newdata$salary )#between getting job and fall average
 t.test(newdata$frstlang ~newdata$salary )#between getting job and first language 
 t.test(newdata$quarter ~newdata$salary )#between getting job and quarter ranking
 
 #Logistic Regression
 #As the insufficient data with salary 998 and 999 is already removed and 'salary' is converted to binary, I use 'newdata'.
 train <- newdata[1:200,]
 test <- newdata[201:274,]
 model <- glm(salary ~., family = binomial(link ='logit'), data=train)
 summary(model)