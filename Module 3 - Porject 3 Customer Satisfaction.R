#                   Module 3 - Porject 3 Customer satisfaction

#set a working directory 'Wd'for easy access 
setwd('C:/Users/mgomaa032/Desktop/PwC/Projects/DXB/PnO/WorkFroce analytics/UT Austin/Module 3/Project')
getwd() #confirm wd is set
#import data set
Data=read.csv('Factor-Hair-Revised.csv',header = T)
#attach data for easy access 
attach(Data)
#View data
names(Data)
View(Data)
head(Data)
tail(Data)
#Summary data analysis and data types
summary(Data)
str(Data)
#Perform exploratory data analysis on the dataset with the help 
#of appropriate visualizations and identify observations/insights. 
#In the process, also check for outliers and missing values 

#Identify if there is evidence of multicollinearity 
#(multiple variables being correlated to each other). 
#Present your observations on the relationship between 
#various variables.

library(corrplot) #load corrplot to polt a correlation matrix
corrplot(cor(Data[,2:13]),method = 'square',title =
           "correlation matrix",order = c("original"),tl.col="black",tl.cex = 0.6,tl.pos=	'lt')
Cormatrix=cor(Data[,2:13])#plot correlation matrix to present it visually
write.csv(Cormatrix,'Cormatrix.CSV') #extract correlation matrix table
#high multicollinearity appears from our analysis
#Simple linear regression for the dependent variable 
#with every independent variable
SLM1=lm(Satisfaction~ProdQual) #SLM1
summary(SLM1) #view results 
plot(Satisfaction,ProdQual,col='Blue',) #plot SLM1
abline(lm(Satisfaction~ProdQual),col="orange")

SLM2=lm(Satisfaction~Ecom) #SLM2
summary(SLM2) #view results 
plot(Satisfaction,Ecom,col='Blue') #plot SLM2
abline(lm(Ecom~Satisfaction),col="orange")

SLM3=lm(Satisfaction~TechSup) #SLM1
summary(SLM3) #view results 
plot(Satisfaction,TechSup,col='Blue',) #plot SLM3
abline(lm(Satisfaction~TechSup),col="orange")

SLM4=lm(Satisfaction~CompRes) #SLM4
summary(SLM4) #view results 
plot(Satisfaction,CompRes,col='Blue',) #plot SLM4
abline(lm(CompRes~Satisfaction),col="orange")

SLM5=lm(Satisfaction~Advertising) #SLM5
summary(SLM5) #view results 
plot(Satisfaction,Advertising,col='Blue',) #plot SLM5
abline(lm(Advertising~Satisfaction),col="orange")

SLM6=lm(Satisfaction~ProdLine) #SLM6
summary(SLM6) #view results 
plot(Satisfaction,ProdLine,col='Blue',) #plot SLM6
abline(lm(ProdLine~Satisfaction),col="orange")


SLM7=lm(Satisfaction~SalesFImage) #SLM7
summary(SLM7) #view results 
plot(Satisfaction,SalesFImage,col='Blue',) #plot SLM7
abline(lm(SalesFImage~Satisfaction),col="orange")


SLM8=lm(Satisfaction~ComPricing) #SLM8
summary(SLM8) #view results 
plot(Satisfaction,ComPricing,col='Blue',) #plot SLM8
abline(lm(ComPricing~Satisfaction),col="orange")

SLM9=lm(Satisfaction~WartyClaim) #SLM9
summary(SLM9) #view results 
plot(Satisfaction,WartyClaim,col='Blue',) #plot SLM9
abline(lm(WartyClaim~Satisfaction),col="orange")


SLM11=lm(Satisfaction~DelSpeed) #SLM11
summary(SLM11) #view results 
plot(Satisfaction,DelSpeed,col='Blue',) #plot SLM11
abline(lm(DelSpeed~Satisfaction),col="orange")

#PCA/Factor analysis by extracting 4 factors. 
#Interpret the output and name the factors 

View(Cormatrix)#high multicollinearity appears from our analysis

#How many factors should I have? 
install.packages('nFactors')# install package to access eigen value code
library(nFactors)#call for package
ev=eigen(Cormatrix)#eigen values is basis for selecting factors
ev
print(ev,digits=5)
eigenvalues=ev$values #extract eigen values 
eigenvalues
factor=c(1,2,3,4,5,6,7,8,9,10,11,12)#preparing to plot eigen values in a scree plot
scree=data.frame(factor,eigenvalues)#preparing to plot eigen values in a scree plot
plot(scree, main='Scree plot', col='blue', ylim=c(0,4))
lines(scree,col='red') # Scree plot helps in choosing the number 
#of factors, based on a drop in the curve or the Eblow rule, 
#were you see a drop in curve shaped like an elbow followed by 
#plateau.
# number of factors should be 4, both based on Kazier rule and elbow rule.

library(psych) # call psych for PCA analysis 
unrotate=principal(Data[,2:13],nfactors = 4,rotate = 'none')# PCA 
print(unrotate,digits = 4)
unrotate_profile=plot(unrotate,row.names(unrotate$loadings))# plot PCA profile
# By using only 4 factors we are to explain 79% of the variance
#in the data set, the remianing variables explian 29%. 

#check PCA results, to understand to which variable is PC belong.
print(unrotate,digits = 4)
# but as the correlation of PC to considerably high to most 
# of variables, which would make our job identify the PC difficult.
# when we cant interpret any factor in a meaningful manner, this 
# can resolved by rotation of axes 'Varimax or Orthogonal Rotation'
# this help is push all lower correlation to zero, 
# while pushing up all higher correlation toward 1.
rotate=principal(Data[,2:13],nfactors = 4,rotate = 'varimax')# Rotated PCA 
print(rotate,digits = 3)
rotate_profile=plot(rotate,row.names(rotate$loadings),cex=1.0)
# Now we can collapse RC1 based on 1-CompRes 2-DelSpeed & 3- OrdBilling Under Odering
# Now we can collapse RC2 based on 1-Ecom 2-Advertising & 3- SalesFImage Under Marketing and Brand
# Now we can collapse RC3 based on 1-ProdQual 2-ProdLine Under Product
# Now we can collapse RC4 based on 1-techsup 2-wart&calim Under After sale 
rotate
rotate$scores
#segment customers based on the 4 factors identifies. 

DataforLM= cbind(Data[,12],rotate$scores)# consoldate results with depent varibale 
DataforLM
colnames(DataforLM)=c('customers_satisfaction', 'Ordering_Logistics',
                       "Marketing_Brand","After_Sale", "Product_Price") # labeling cols

head(DataforLM)
DataforLM= as.data.frame(DataforLM)
#MLM
LM=lm(customers_satisfaction~.,data=DataforMLM)
summary(LM)

