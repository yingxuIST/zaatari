mydata <- read.csv("Documents/zaatari_data/R/zaatariDatasetOriginalLabel.csv", header=TRUE, nrows = 234)
install.packages("ggplot2")
install.packages("GGally")
install.packages("ICC")
library(ggplot2)
library(MASS)
library(GGally)
library(ICC)

#Presenting Dataset
summary(mydata)
names(mydata)
youth <- mydata[which(mydata$X1_AGE>14&mydata$X1_AGE<26),]
adult <- mydata[which(mydata$X1_AGE>25),]
dim(youth)
dim(adult)
ggpairs(youth[,c("X1_AGE","X2_EDU","X1_SEX")])

#Bivariate Analysis
internetSyriaYouth<-youth[,"X5_INTERNET_SYRIA"]
internetSyriaAdult<-adult[,"X5_INTERNET_SYRIA"]

ggplot(mydata,aes(X5_INTERNET_SYRIA))+ 
  geom_histogram(data=youth,fill="red",alpha=0.2,position="dodge")+
  geom_histogram(data=adult,fill="green",alpha=0.2,position="dodge")

ggplot(mydata,aes(X5_INTERNET_JORDAN))+ 
  geom_histogram(data=youth,fill="red",alpha=0.2,position="dodge")+
  geom_histogram(data=adult,fill="green",alpha=0.2,position="dodge")

#Multivariate Analysis
lda.fit=lda(X5_INTERNET_JORDAN~X5_INTERNET_SYRIA+X1_AGE+X1_SEX+X2_EDU+X3_ENGLISH,data=mydata)
lda.fit
coef(lda.fit)
ggpairs(youth[,c("X1_AGE","X1_SEX","X2_EDU","X3_ENGLISH","X5_INTERNET_SYRIA","X5_INTERNET_JORDAN")])
chisq.test(mydata$X1_AGE,mydata$X5_INTERNET_SYRIA)
chisq.test(mydata$X1_AGE,mydata$X5_INTERNET_JORDAN)
chisq.test(mydata$X1_SEX,mydata$X5_INTERNET_SYRIA)
chisq.test(mydata$X1_SEX,mydata$X5_INTERNET_JORDAN)
chisq.test(mydata$X2_EDU,mydata$X5_INTERNET_SYRIA)
chisq.test(mydata$X2_EDU,mydata$X5_INTERNET_JORDAN)
chisq.test(mydata$X3_ENGLISH,mydata$X5_INTERNET_SYRIA)
chisq.test(mydata$X3_ENGLISH,mydata$X5_INTERNET_JORDAN)