loan = read.csv("loan.csv")
summary(loan)

head(loan)
dim(loan)
names(loan)

## Remove columns with more than 50% NA
loan = loan[, -which(colMeans(is.na(loan)) > 0.6)]
dim(loan)

library(dplyr)
loan_1 = dplyr::select(loan, 3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,21,22,23,24,25,26,27,28,29,30,31,32,33,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,51,52,53,54,55)
head(loan_1)


#EDA - 
#The objective is to identify predictors of default so that at the time of loan application, we can use those variables for approval/rejection of the loan. 
#Now, there are broadly three types of variables - 
#1. those which are related to the applicant (demographic variables such as age, occupation, employment details etc.), 
#2. loan characteristics (amount of loan, interest rate, purpose of loan etc.) and 
#3. Customer behaviour variables (those which are generated after the loan is approved such as delinquent 2 years, revolving balance, next payment date etc.).

dim(loan)
dim(loan_1)

loan = loan[loan$loan_status!="Current",]
loan_1 = loan_1[loan_1$loan_status!="Current",]

summary(loan_1)
loan_1$loan_status = ifelse(loan_1$loan_status=="Fully Paid",0,1)
loan_1$loan_status = factor(loan_1$loan_status)

loan$loan_status = ifelse(loan$loan_status=="Fully Paid",0,1)
loan$loan_status = factor(loan$loan_status)


loan_1$grade = factor(loan_1$grade)
loan_1$term = factor(loan_1$term)
loan_1$sub_grade = factor(loan_1$sub_grade)
loan_1$home_ownership = factor(loan_1$home_ownership)
loan$home_ownership = factor(loan$home_ownership)
loan_1$purpose = factor(loan_1$purpose)
loan_1$int_rate = factor(loan_1$int_rate)

#Univariant Analysis
#1. Grade vs Loan Status
plot(loan_1$grade,loan_1$loan_status, col = c('blue','red') , xlab = "Grade(Decided based on riskiness of loan)", ylab = "Loan Status", main = "Grade vs Loan Status")#,, col = c("red","blue"))

#2. Term vs Loan Status - Higher the term more is the defaulting
plot(loan_1$term,loan_1$loan_status, col = c('blue','red') , xlab = "Loan Term", ylab = "Loan Status", main = "Loan Term vs Loan Status")#,, col = c("red","blue"))

#3. Sub grade vs Loan Status
plot(loan_1$sub_grade,loan_1$loan_status, col = c('blue','red') , xlab = "Sub-Grade(Decided based on riskiness of loan)", ylab = "Loan Status", main = "Sub-Grade vs Loan Status")#,, col = c("red","blue"))

#4. Home ownership
plot(loan$home_ownership,loan_1$loan_status, col = c('blue','red') , xlab = "Grade(Decided based on riskiness of loan)", ylab = "Loan Status", main = "Grade vs Loan Status")#,, col = c("red","blue"))

#5. Verification status
plot(loan_1$verification_status,loan_1$loan_status, col = c('blue','red') , xlab = "Verification Status", ylab = "Loan Status", main = "Verification Status vs Loan Status")#,, col = c("red","blue"))

#6. Purpose
loan_1$purpose = factor(as.numeric(loan_1$purpose))
table(loan_1$purpose)
table(loan$purpose)
#1-car, 2-cc, 3-debt, 4-edu, 5-home impro, 6-house, 7-major purchase, 8-medical, 9-moving, 10-other,11-renewable, 12-small business
#13-vacation, 14-wedding
plot(loan_1$purpose, loan_1$loan_status, col = c('blue','red') , xlab = "Purpose of Loan", ylab = "Loan Status", main = "Purpose vs Loan Status")
#small business, renewable energy, education


#7. Loan amount

loan_1$loan_amnt = ifelse((loan_1$loan_amnt<5000),"Low",
                          ifelse((loan_1$loan_amnt>=5000 & loan_1$loan_amnt<15000),"Medium",
                                 ifelse((loan_1$loan_amnt>=15000 & loan_1$loan_amnt<25000),"High","Very High")
                                 ))
summary(loan_1)
loan_1$loan_amnt = factor(loan_1$loan_amnt)

plot(loan_1$loan_amnt, loan_1$loan_status, col = c('blue','red') , xlab = "Loan Amount", ylab = "Loan Status", main = "Loan Amount vs Loan Status")
#loan very high - default is more

#9.Funded Amount
loan_1$funded_amnt = ifelse((loan_1$funded_amnt<5000),"Low",
                          ifelse((loan_1$funded_amnt>=5000 & loan_1$funded_amnt<15000),"Medium",
                                 ifelse((loan_1$funded_amnt>=15000 & loan_1$funded_amnt<25000),"High","Very High")))
summary(loan_1)
loan_1$funded_amnt = factor(loan_1$funded_amnt)

plot(loan_1$funded_amnt, loan_1$loan_status, col = c('blue','red') , xlab = "Funded Amount", ylab = "Loan Status", main = "Funded Amount vs Loan Status")


#10. Interest Rate
#gsub("%","", as.character(factor("14.9%")))
gsub("%","", as.character(factor(loan_1$int_rate)))
class(loan_1$int_rate)
loan_1$int_rate = as.numeric(loan_1$int_rate)
loan_1$int_rate = ifelse((loan_1$int_rate<10),"Low",
                          ifelse((loan_1$int_rate>=10 & loan_1$int_rate<15),"Medium", "High"))

loan_1$int_rate = factor(loan_1$int_rate)
summary(loan_1)

plot(loan_1$int_rate, loan_1$loan_status, col = c('blue','red') , xlab = "Interest Rate", ylab = "Loan Status", main = "Interest Rate vs Loan Status")
#default more for more interest rate

#11. Installments
class(loan_1$installment)
loan_1$installment = ifelse((loan_1$installment<200),"Low",
                            ifelse((loan_1$installment>=200 & loan_1$installment<400),"Medium",
                                   ifelse((loan_1$installment>=400 & loan_1$installment<600),"High","Very High")))

loan_1$installment = factor(loan_1$installment)

plot(loan_1$installment, loan_1$loan_status, col = c('blue','red') , xlab = "Installment Amount", ylab = "Loan Status", main = "Installments Amount vs Loan Status")

#12.Annual Income
class(loan_1$annual_inc)
loan_1$annual_inc = ifelse((loan_1$annual_inc<50000),"Low",
                            ifelse((loan_1$annual_inc>=50000 & loan_1$annual_inc<100000),"Medium",
                                   ifelse((loan_1$annual_inc>=100000 & loan_1$annual_inc<150000),"High","Very High")))

loan_1$annual_inc = factor(loan_1$annual_inc)

plot(loan_1$annual_inc, loan_1$loan_status, col = c('blue','red') , xlab = "Annual Income", ylab = "Loan Status", main = "Annual Income vs Loan Status")


