library(ggplot2)
library(dplyr)

ggplot(data = customer_churn,aes(x=PhoneService)) + geom_bar()

ggplot(data = customer_churn,aes(x=PhoneService)) + geom_bar()

ggplot(data = customer_churn,aes(x=PhoneService)) + geom_bar(fill = "pink", col="peru")

ggplot(data=customer_churn, aes(InternetService, fill=InternetService)) + geom_bar()

table(customer_churn$InternetService)

table(customer_churn$InternetService, customer_churn$Contract)

ggplot(data=customer_churn, aes(InternetService, fill=Contract)) + geom_bar()

ggplot(data=customer_churn, aes(InternetService, fill=Contract)) + geom_bar(position = "dodge", col="black")

ggplot(data=customer_churn, aes(InternetService, fill=Contract)) + geom_bar(position  = "identity")

ggplot(data=customer_churn, aes(TechSupport, fill=Churn)) + geom_bar()

ggplot(data=customer_churn,aes(x=tenure))+geom_histogram(bins=100, fill="mediumspringgreen", col="azure") 

ggplot(data = customer_churn,aes(x=MonthlyCharges, fill=OnlineBackup)) + geom_histogram()

dc  <- na.omit(customer_churn)

colSums(is.na(customer_churn))
colSums(is.na(dc))

ggplot(data = customer_churn,aes(x=TotalCharges, fill=gender)) + geom_histogram()

ggplot(data = customer_churn,aes(x=TotalCharges, fill=InternetService)) + geom_histogram()

ggplot(data = customer_churn,aes(y=TotalCharges,x=tenure)) + geom_point(col="wheat3")

ggplot(data = customer_churn,aes(y=TotalCharges,x=tenure, col=PaymentMethod)) + geom_point()

ggplot(data = customer_churn,aes(y=TotalCharges,x=tenure, col=gender)) + geom_point()

ggplot(data = customer_churn,aes(y=TotalCharges,x=tenure, col=gender)) + geom_point() + scale_color_manual(values = c("red","blue"))

################MATTER##############################################

ggplot(data = customer_churn, aes(x=tenure)) +
  geom_histogram(bins=20,fill="steelblue3",col="black",alpha=0.5) +
  stat_bin(bins=20, geom="text", color="black", aes(label=..count..), vjust = -1.1) +
  labs(title = "Tenure Distribution", x="Tenure",y="Frequency") +
  theme(plot.title = element_text(hjust=0.5)) + ylim(0,1000)

ggplot(data = customer_churn, aes(x=MonthlyCharges)) +
  geom_histogram(bins=20,fill="steelblue4",col="black",alpha=0.5) +
  stat_bin(bins=20, geom="text", color="black", aes(label=..count..), vjust = -1.1) +
  labs(title = "Monthly Charges Distribution", x="Monthly Charges",y="Frequency") +
  theme(plot.title = element_text(hjust=0.5)) 

#########################################################################

ggplot(data = customer_churn,aes(y=TotalCharges,x=tenure, col=Dependents, shape=Dependents)) + geom_point()

ggplot(data = customer_churn,aes(x=MonthlyCharges,y=tenure)) + geom_point(col="yellowgreen")

ggplot(data = customer_churn,aes(x=MonthlyCharges,y=tenure, col=InternetService)) + geom_point()

ggplot(data = customer_churn,aes(x=MonthlyCharges,y=tenure, col=Contract)) + geom_point()

ggplot(data = customer_churn,aes(y=tenure,x=Partner))+geom_boxplot(fill="violet", col="snow3")

ggplot(data = customer_churn,aes(y=tenure,x=MultipleLines, fill=Partner))+geom_boxplot()

ggplot(data = customer_churn,aes(y=tenure,x=MultipleLines, fill=PhoneService))+geom_boxplot()

ggplot(data = customer_churn,aes(y=tenure,x=Contract, fill=PaymentMethod))+geom_boxplot()

ggplot(data = customer_churn,aes(y=tenure,x=Contract, fill=PaymentMethod))+geom_boxplot()

ggplot(data = customer_churn,aes(y=tenure,x=MultipleLines, fill=InternetService))+geom_boxplot()+facet_grid(~InternetService)

ggplot(data = customer_churn,aes(y=TotalCharges,x=tenure, col=Contract)) + geom_point() + facet_grid(~Contract)

ggplot(data = customer_churn,aes(x=MonthlyCharges, fill=PaymentMethod)) + geom_histogram()+facet_grid(~PaymentMethod)

ggplot(data = customer_churn,aes(x=gender))+geom_bar(fill="blue4")

ggplot(data = customer_churn,aes(x=gender))+geom_bar(fill="blue4")+theme(panel.background = element_rect(fill = "chartreuse4"))

ggplot(data=customer_churn,aes(x=gender))+geom_bar(fill="blue4")+theme(panel.background = element_rect(fill = "chartreuse4"))+theme(plot.background = element_rect(fill="cornsilk4"))

ggplot(data = customer_churn,aes(y=tenure,x=MonthlyCharges))+geom_point(col="yellowgreen")

ggplot(data = customer_churn,aes(y=tenure,x=MonthlyCharges))+geom_point(col="yellowgreen")+
  labs(title="Tenure vs Monthly Charges")

ggplot(data = customer_churn,aes(y=tenure,x=MonthlyCharges))+geom_point(col="yellowgreen")+
  labs(title="Tenure vs Monthly Charges") + theme(panel.background = element_rect(fill="coral2")) 

ggplot(data = customer_churn,aes(y=tenure,x=MonthlyCharges))+geom_point(col="yellowgreen")+ 
  labs(title="Tenure vs Monthly Charges") + theme(panel.background = element_rect(fill="coral2")) + theme(plot.background = element_rect(fill = "beige"))

ggplot(data = customer_churn,aes(y=tenure,x=MonthlyCharges))+geom_point(col="yellowgreen")+ 
  labs(title="Tenure vs Monthly Charges") + theme(panel.background = element_rect(fill="coral2")) + theme(plot.background = element_rect(fill = "beige"))+theme(plot.title = element_text(hjust = 0.5,colour = "red"))
