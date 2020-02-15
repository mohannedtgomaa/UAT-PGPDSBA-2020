setwd ('C:/Users/mgomaa032/Desktop/PwC/Projects/DXB/PnO/WorkFroce analytics/UAT/Week 3')
getwd()
library(readr)
library(readxl)
Master.data=read.csv(file = 'CardioGoodFitness.csv',header = TRUE)
summary(Master.data)
str(Master.data)
head(Master.data)
tail(Master.data)
View (Master.data)
library(dplyr)
library(corrplot)
library(xlsx)


corrplot(cor(Master.data[,c(2,4,6:9)]),method = 'square',title = 'Correlation matrix')

Sales.profileA= Master.data %>% group_by(Product)%>%
  summarise(Product.sold=n(), 
            Avg.Age= mean(Age),
            Avg.Use= mean(Usage), 
            Avg.Fitness= mean(Fitness), Avg.miles = mean(Miles),
            Avg.income = mean(Income))
write.xlsx(Sales.profileA,'Salessummary.xlsx')
Sales.profileB= Master.data %>% group_by(Product, Gender, MaritalStatus)%>%
  summarise(Product.sold=n(), 
            Avg.Age= mean(Age),
            Avg.Use= mean(Usage), 
            Avg.Fitness= mean(Fitness), Avg.miles = mean(Miles),
            Avg.income = mean(Income))
View(Sales.profileB)

write.csv(Sales.profileB,'Summary2.csv')

Sales.profileC= Master.data %>% group_by(MaritalStatus)%>%
  summarise(Product.sold=n(), 
            Avg.Age= mean(Age),
            Avg.Use= mean(Usage), 
            Avg.Fitness= mean(Fitness), Avg.miles = mean(Miles),
            Avg.income = mean(Income))


View(Sales.profileC)


hist(Master.data$Age, main = "Age Profile",col='Navy', xlab = 'Age')
abline(v = mean(Master.data$Age) , col='Blue')
abline(v = median(Master.data$Age) , col='turquoise')

hist(Master.data$Miles, main = "Miles Ran",col='Navy', xlab = 'Miles')
abline(v = mean(Master.data$Miles) , col='Blue')
abline(v = median(Master.data$Miles) , col='turquoise')

hist(Master.data$Income/1000, main = "Income Profile",col='Navy', xlab = 'Income in 000s')
abline(v = mean(Master.data$Income/1000) , col='Blue')
abline(v = median(Master.data$Income/1000) , col='turquoise')

#####TM195#####

TM195 = filter(Master.data, Product=='TM195')
summary (TM195)

Age.Sales.profile.TM195= TM195%>% group_by(Age)%>% 
  summarise(Productsold=n())
View(Age.Sales.profile.TM195)

corrplot(cor(TM195[,c(2,4,6:9)]),method = 'square',tl.col ="Black")

boxplot(TM195[,c(6,7)],col = 'Navy')

hist(TM195$Age, col="Navy", xlab = "Age")
abline(v = mean(TM195$Age) , col='Blue')
abline(v = median(TM195$Age) , col='turquoise')

hist(TM195$Fitness, col = 'Navy',xlab = 'Fitness')
abline(v = mean(TM195$Fitness), col='Blue')
abline(v = median(TM195$Fitness) , col='turquoise')

hist(TM195$Miles, col = 'Navy',xlab = 'Miles')
abline(v = mean(TM195$Miles), col='Blue')
abline(v = median(TM195$Miles) , col='turquoise')

hist(TM195$Education, col = 'Navy',xlab = 'Miles')
abline(v = mean(TM195$Education), col='Blue')
abline(v = median(TM195$Education) , col='turquoise')

hist(TM195$Usage, col = 'Navy',xlab = 'Miles')
abline(v = mean(TM195$Usage), col='Blue')
abline(v = median(TM195$Usage) , col='turquoise')


plot(TM195$Fitness,TM195$Usage)
abline(lm(TM195$Fitness~TM195$Miles),col='Navy')

?plot

hist(TM195$Income, col="Navy", xlab = "Income")
abline(v = mean(TM195$Income), col='Blue')
abline(v = median(TM195$Income) , col='turquoise')


plot(TM195$Gender,TM195$Usage,xlab ='Gender',col='Navy', ylab='Usage')

plot(TM195$MaritalStatus,TM195$Usage,xlab ='Marital Status',col='Navy', ylab='Usage')

plot(TM195$Age,TM195$Income,xlab ='Age',col='Navy', ylab='Income')
abline(lm(TM195$Income~TM195$Age), col='Red')

plot(TM195$Age,TM195$Education,xlab ='Age',col='Navy', ylab='Educ.')
abline(lm(TM195$Education~TM195$Age), col='Red')

plot(TM195$Age,TM195$Miles,xlab ='Age',col='Navy', ylab='Miles')
abline(lm(TM195$Miles~TM195$Age), col='Red')

plot(TM195$Age,TM195$Fitness,xlab ='Age',col='Navy', ylab='Fitness')
abline(lm(TM195$Fitness~TM195$Age), col='Red')


plot(TM195$Age,TM195$Usage,xlab ='Age',col='Navy', ylab='Usage')
abline(lm(TM195$Usage~TM195$Age), col='Red')

plot(TM195$Miles,TM195$Fitness,xlab ='Fitness',col='Navy', ylab='Miles')
abline(lm(TM195$Fitness~TM195$Miles), col='Red')


plot(TM195$Miles,TM195$Usage,xlab ='Miles',col='Navy', ylab='Income')
abline(lm(TM195$Usage~TM195$Miles), col='Red')

plot(Age.Sales.profile.TM195$Productsold,Age.Sales.profile.TM195$Age,xlab ='Age',col='Navy', ylab='units')
abline(lm(Age.Sales.profile.TM195$Productsold~Age.Sales.profile.TM195$Age), col='Red')


#####TM498#####

TM498 = filter(Master.data, Product=='TM498')
summary (TM498)

Sales.profile.TM498= TM498%>% group_by(Gender)%>% 
  summarise(Productsold=n())
View(Sales.profile.TM498)

corrplot(cor(TM498[,c(2,4,6:9)]),method = 'square',tl.col ="Black")

boxplot(TM498[,c(6,7)],col = 'Navy')

hist(TM498$Age, col="Navy", xlab = "Age")
abline(v = mean(TM498$Age) , col='Blue')
abline(v = median(TM498$Age) , col='turquoise')

hist(TM498$Income, col="Navy", xlab = "Income")
abline(v = mean(TM498$Income) , col='Blue')
abline(v = median(TM498$Income) , col='turquoise')


hist(TM498$Fitness, col = 'Navy',xlab = 'Fitness')
abline(v = mean(TM498$Fitness), col='Blue')
abline(v = median(TM498$Fitness) , col='turquoise')

hist(TM498$Miles, col = 'Navy',xlab = 'Miles')
abline(v = mean(TM498$Miles), col='Blue')
abline(v = median(TM498$Miles) , col='turquoise')

hist(TM498$Education, col = 'Navy',xlab = 'Miles')
abline(v = mean(TM498$Education), col='Blue')
abline(v = median(TM498$Education) , col='turquoise')

hist(TM498$Usage, col = 'Navy',xlab = 'Miles')
abline(v = mean(TM498$Usage), col='Blue')
abline(v = median(TM498$Usage) , col='turquoise')


plot(TM498$Miles,TM498$Fitness)
abline(lm(TM498$Fitness~TM498$Miles),col='Navy')

?plot

hist(TM498$Income, col="Navy", xlab = "Income")
abline(v = mean(TM498$Income), col='Blue')
abline(v = median(TM498$Income) , col='turquoise')


plot(TM498$Gender,TM498$Usage,xlab ='Gender',col='Navy', ylab='Usage')

plot(TM498$MaritalStatus,TM498$Usage,xlab ='Marital Status',col='Navy', ylab='Usage')

plot(TM498$Age,TM498$Income,xlab ='Age',col='Navy', ylab='Income')
abline(lm(TM498$Income~TM498$Age), col='Red')

plot(TM498$Age,TM498$Education,xlab ='Age',col='Navy', ylab='Educ.')
abline(lm(TM498$Education~TM498$Age), col='Red')

plot(TM498$Age,TM498$Miles,xlab ='Age',col='Navy', ylab='Miles')
abline(lm(TM498$Miles~TM498$Age), col='Red')

plot(TM498$Age,TM498$Fitness,xlab ='Age',col='Navy', ylab='Fitness')
abline(lm(TM498$Fitness~TM498$Age), col='Red')


plot(TM498$Age,TM498$Usage,xlab ='Age',col='Navy', ylab='Usage')
abline(lm(TM498$Usage~TM498$Age), col='Red')

plot(TM498$Miles,TM498$Fitness,xlab ='Fitness',col='Navy', ylab='Miles')
abline(lm(TM498$Fitness~TM498$Miles), col='Red')


plot(TM498$Miles,TM498$Usage,xlab ='Miles',col='Navy', ylab='Income')
abline(lm(TM498$Usage~TM498$Miles), col='Red')


Age.Sales.profile.TM498= TM498%>% group_by(Age)%>% 
  summarise(Productsold=n())
View(Age.Sales.profile.TM498)


plot(Age.Sales.profile.TM498$Productsold,Age.Sales.profile.TM498$Age,xlab ='Unit',col='Navy', ylab='Age')
abline(lm(Age.Sales.profile.TM498$Age~Age.Sales.profile.TM498$Productsold), col='Red')


#####TM798#####


TM798 = filter(Master.data, Product=='TM798')
summary (TM798)

Sales.profile.TM798= TM798%>% group_by(Gender)%>% 
  summarise(Productsold=n())
View(Sales.profile.TM798)

corrplot(cor(TM798[,c(2,4,6:9)]),method = 'square',tl.col ="Black")

boxplot(TM798[,c(6,7)],col = 'Navy')

hist(TM798$Age, col="Navy", xlab = "Age")
abline(v = mean(TM798$Age) , col='Blue')
abline(v = median(TM798$Age) , col='turquoise')

hist(TM798$Income, col="Navy", xlab = "Income")
abline(v = mean(TM798$Income) , col='Blue')
abline(v = median(TM798$Income) , col='turquoise')


hist(TM798$Fitness, col = 'Navy',xlab = 'Fitness')
abline(v = mean(TM798$Fitness), col='Blue')
abline(v = median(TM798$Fitness) , col='turquoise')

hist(TM798$Miles, col = 'Navy',xlab = 'Miles')
abline(v = mean(TM798$Miles), col='Blue')
abline(v = median(TM798$Miles) , col='turquoise')

hist(TM798$Education, col = 'Navy',xlab = 'Miles')
abline(v = mean(TM798$Education), col='Blue')
abline(v = median(TM798$Education) , col='turquoise')

hist(TM798$Usage, col = 'Navy',xlab = 'Miles')
abline(v = mean(TM798$Usage), col='Blue')
abline(v = median(TM798$Usage) , col='turquoise')


plot(TM798$Miles,TM798$Fitness)
abline(lm(TM798$Fitness~TM798$Miles),col='Navy')

?plot



plot(TM798$Gender,TM798$Usage,xlab ='Gender',col='Navy', ylab='Usage')

plot(TM798$MaritalStatus,TM798$Usage,xlab ='Marital Status',col='Navy', ylab='Usage')

plot(TM798$Age,TM798$Income,xlab ='Age',col='Navy', ylab='Income')
abline(lm(TM798$Income~TM798$Age), col='Red')

plot(TM798$Age,TM798$Education,xlab ='Age',col='Navy', ylab='Educ.')
abline(lm(TM798$Education~TM798$Age), col='Red')

plot(TM798$Age,TM798$Miles,xlab ='Age',col='Navy', ylab='Miles')
abline(lm(TM798$Miles~TM798$Age), col='Red')

plot(TM798$Age,TM798$Fitness,xlab ='Age',col='Navy', ylab='Fitness')
abline(lm(TM798$Fitness~TM798$Age), col='Red')


plot(TM798$Age,TM798$Usage,xlab ='Age',col='Navy', ylab='Usage')
abline(lm(TM798$Usage~TM798$Age), col='Red')

plot(TM798$Miles,TM798$Fitness,xlab ='Fitness',col='Navy', ylab='Miles')
abline(lm(TM798$Fitness~TM798$Miles), col='Red')


plot(TM798$Miles,TM798$Usage,xlab ='Miles',col='Navy', ylab='Income')
abline(lm(TM798$Usage~TM798$Miles), col='Red')


Age.Sales.profile.TM798= TM798%>% group_by(Age)%>% 
  summarise(Productsold=n())
View(Age.Sales.profile.TM798)


plot(Age.Sales.profile.TM798$Productsold,Age.Sales.profile.TM798$Age,xlab ='Unit',col='Navy', ylab='Age')
abline(lm(Age.Sales.profile.TM798$Age~Age.Sales.profile.TM798$Productsold), col='Red')






#######################End#########################






?colors
?corrplot
?cor
quantile(TM195$Age,0.25) 
quantile(TM195$Age,0.75) 
IQR_TM195=quantile(TM195$Age,0.75)-quantile(TM195$Age,0.25) 
outH = quantile(TM195$Age,0.75)+(1.5*IQR_TM195)
outl = quantile(TM195$Age,0.25)-(1.5*IQR_TM195)
boxplot(TM195$Age)
boxplot(TM195$Income)

View(Sales.profileA)
boxplot(Master.data[c(6,7)], main="Usage and Fintess level", ylab='Freq', col = 'Orange')
boxplot(Master.data$Miles)
boxplot(Master.data$Usage)
boxplot(Master.data$Age)
boxplot(Master.data$Education)

hist(Master.data$Age, col='Green')
abline(v=mean(Master.data$Age),col='blue')
hist(Master.data$Usage, col='Green')
abline(v=mean(Master.data$Usage ),col='blue')
hist(Master.data$Miles , col='Green')
abline(v=mean(Master.data$Miles ),col='blue')
?colours

?plot
?lm
install.packages('xlsx')
