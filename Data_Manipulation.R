#Data Manipulation Assignment

setwd("D:/Intellipaat/R_programs/Datasets")
customer_churn=read.csv("customer_churn.csv", stringsAsFactors = T)
library(dplyr)

customer_5 <- select(customer_churn,5)

customer_15 <- select(customer_churn,15)

customer_3_multiple <- select(customer_churn, 3,6,9,12,15,18)

c_10_20 <- select(customer_churn, 10:20)
  
customer_P <- select(customer_churn, starts_with("P"))

customer_s <- select(customer_churn, ends_with("s"))

table(customer_churn$Contract)

table(customer_churn$PaymentMethod)

table(customer_churn$tenure)

table(customer_churn$TotalCharges)

filter(customer_churn,(InternetService == "DSL")) -> customer_DSL

filter(customer_churn,(Contract == "Month-to-month")) -> customer_month

filter(customer_churn,(PaymentMethod == "Electronic check")) -> senior_male_electronic

customer_total_tenure <- filter(customer_churn,(tenure > 70 | TotalCharges > 8000))

two_mail_yes <- filter(customer_churn,(PaymentMethod == "Mailed check" & Churn == "Yes"))

count(customer_churn, PaymentMethod)

sample_n(customer_churn,333)->customer_333

sample_n(customer_churn,1000)->customer_1000

sample_frac(customer_churn,0.23)->customer_23_percent

count(customer_churn, PaymentMethod)

count(customer_churn, Churn)

summarise(customer_churn,median_tenure=median(tenure), var_tenure=var(tenure), Sta_devi=sd(tenure))

summarise(customer_churn,median_MonthlyCharges=median(MonthlyCharges), 
          var_MonthlyCharges=var(MonthlyCharges), Sta_devi_MonthlyCharges=sd(MonthlyCharges))

summarise(group_by(customer_churn,PaymentMethod),sta_dev_tenure=sd(tenure))

summarise(group_by(customer_churn,Contract),sta_dev_contract=median(MonthlyCharges))

summarise(group_by(customer_churn,InternetService),sta_dev_contract=var(TotalCharges, na.rm = T))
