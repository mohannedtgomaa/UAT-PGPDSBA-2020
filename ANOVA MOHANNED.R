

setwd('C:/Users/mgomaa032/Desktop/PwC/Projects/DXB/PnO/WorkFroce analytics/UAT/Module 2/Week 3')
getwd()

# one way anova
# there are 4 types of golf balls
# there are 10 of each type of ball
# the 40 balls are shuffled
# a golf pro hits the 40 balls successively
# the golf pro doesn't know which type of ball being hit
# the distance each ball travels is recorded
# do the 4 designs all have the same mean distance?

#H0: DistD1=DistD2=DistD3=DistD4
#Ha: H0 is not true 
Golfball=read.csv(file = 'Golfball.csv',header = TRUE)
View(Golfball)
attach(Golfball)
by(Distance,INDICES = Design,FUN = mean )
by(Distance,INDICES = Design,FUN = var )
boxplot(Distance~Design,horizontal = TRUE, main= 'Distance by design', col=c('Red', 'Blue','Yellow', 'Orange'))
Anovamodel=aov(Distance~Design,data = Golfball)
summary(Anovamodel)
pf(53.03,3,36,lower.tail = FALSE)
?TukeyHSD
TukeyHSD(Anovamodel)


# multi-way anova
# several stores are selling Paul Newman food
# each store is exposed to high/low advertising
# each store has High/Medium/Low prices
# each store is a different size
# does each population of stores have the same mean sales?

Storedata=read.csv('paul-newfood.csv',header = TRUE)
View(Storedata)
attach(Storedata)
by(Sales,INDICES = PriceLevel+AdLevel,FUN = mean)
by(Sales,INDICES = PriceLevel+AdLevel,FUN =var)
Storemodel=aov(Sales~PriceLevel+AdLevel,data = Storedata)
summary(Storemodel)
interaction.plot(PriceLevel,AdLevel,Sales, col=c("Red","Blue"),main="Interaction between Price and Advertisement")
#ANCOVA
Storemodelupdate=aov(Sales~StoreSize+PriceLevel+AdLevel,data = Storedata)
summary(Storemodelupdate)
TukeyHSD(Storemodel)

#PRACTICE Questions:-
#1.Suppose that a random sample of n = 5 was selected from 
# the vineyard properties for sale in Sonoma County, California, 
# in each of three years.  The following data are consistent with 
# summary information on price per  acre for disease-resistant 
# grape vineyards in Sonoma County. Carry out an ANOVA to determine
# whether there is evidence to support the claim that the 
# mean price per acre for vineyard land in Sonoma County 
# was not the same for each of the three years considered.  
# Test at the 0.05 level and at the 0.01 level.

#H0: AcrePrice96=AcrePrice97=AcrePrice98
#Ha: H0 is false 
Acreprices= read.csv('Dataset for Q1-Practice Questions.csv', header = T)
View(Acreprices)
attach(Acreprices)
by(price,INDICES = year, FUN = mean)
by(price,INDICES = year, FUN = var)
boxplot(price~year, col=c('Red', 'Orange','Dark Green'), horizontal = TRUE, main = 'Avg Acre Price by Year')
Acrepricesmodel=aov(price~year,data = Acreprices)
summary(Acrepricesmodel)
TukeyHSD(Acrepricesmodel)
0.01044*100
0.05*100

#2.	The following data on calcium content of wheat are consistent 
# with summary quantities that appeared in the article 
# "Mineral Contents of Cereal Grains as Affected by Storage and 
# Insect Infestation" (Journal of Stored Products Research [1992]). 
# Four different storage times were considered. Is there sufficient evidence 
# to conclude that the mean calcium content is not the same for the four different storage times?  
# Test  the appropriate hypotheses at the 0.05 level.

#H0: Cal1=Cal2=Cal3=Cal4
#Ha: H0 is not true

Calconetnt= read.csv('Dataset for Q2-Practice Questions.csv',header = T)
View(Calconetnt)
attach(Calconetnt)
by(calcium,INDICES = storage_time,FUN = mean)
by(calcium,INDICES = storage_time,FUN = var)
boxplot(calcium~storage_time,col=c('Red','Orange','Yellow','Green'),horizontal = T,main='cal by storage time')
Calconetntmodel=aov(calcium~storage_time,data = Calconetnt)
summary(Calconetntmodel)
TukeyHSD(Calconetntmodel)
0.00298*100

#3. Use the data below, showing a summary of highway gas mileage 
# for several observations, to decide if the average highway gas 
# mileage is the same for midsize cars, SUV's, and pickup trucks. Test the appropriate
# hypotheses at the ?? = 0.01 level.

# H0: MSC = SUV = Pickup
# Ha: H0 is not true 

         #n Mean StdDev.
#Midsize 31 25.8  2.56
#SUV's   31 22.68 3.67
#Pickups 14 21.29 2.76
Cartype=c('Midsize','SUV','Pickups')
CarsMeanMiles= c(25.8,22.68,21.29)
CarN=c(31,31,14)
CarSD= c(2.56,3.67,2.76)
Carsdata= data.frame(CarN,CarsMeanMiles,CarSD,row.names = Cartype)
View(Carsdata)
install.packages('car')
install.packages('rpsychi')
require(rpsychi)
CarsFstat= with(Carsdata,ind.oneway.second(CarsMeanMiles,CarSD,CarN))
CarsFstat
Fstatcars=13.056
pf(Fstatcars,2,73,lower.tail = F)
?pf
# We reject the Null hypothesis, Car miles do differ by car type.


#4. To examine the effects of pets and friends in stressful 
# situations, researchers recruited 45 people to participate in
# an experiment. Fifteen of the subjects were randomly assigned to 
# each of three groups to perform a stressful
# task alone (control group), with a good friend present, or with 
# their dog present. Each subject's mean heart rate
# during the task was recorded. Test the appropriate hypotheses 
# at the ?? = 0.05 level to decide if the mean heart
# rate differs between the groups.

      #   n  Mean StdDev.
# Control 15 82.52 9.24
# Pets    15 73.48 9.97
# Friends 15 91.325 8.34

# H0: HR1=HR2=HR3
# H1: H0 is not true 

Testgroups= c('Control','Pets','Friends')
TestGroupsN= c(15,15,15)
TestGroupsMean= c(82.52,73.48,91.325)
TestGroupsSD= c(9.24,9.97,8.34)
TestData= data.frame(TestGroupsN,TestGroupsMean,TestGroupsSD,row.names = Testgroups)
TestData
require(rpsychi)
TestFstat= with(TestData,ind.oneway.second(TestGroupsMean,TestGroupsSD,TestGroupsN))
TestFstat
testp= pf(14.087,2,42,lower.tail = F)
testp
#We reject null hypothesis. Heart rate is different between the 3 groups 


# 5. An investigation carried out to study the toxic effects of 
# mercury was described in the article "Comparative Responses of 
# the Action of Different Mercury Compounds on Barley" 
# (International Journal of Environmental Studies [1983]). 
# Ten different concentrations of mercury were compared with 
# respect to their effects on average dry weight 
# (per 100 seven-day-old seedlings). The basic experiment was 
# replicated 4 times for a total of 40 observations. The article
# reported an ANOVA F statistic of 1.895. Using a significance 
# level of 0.05, test the hypothesis that the true mean dry 
# weight is the same for all 10 concentration levels.
# H0: M1=M2=M3...=M10
# Ha: H0 is not true 
pf(1.895,9,30,lower.tail = F)

# We fail to reject the null hypothesis. No, enough evidance that mercery levels changed.


# 6. High productivity and carbohydrate storage ability of the 
# Jerusalem artichoke make it a promising agricultural
# crop. The article "Leaf Gas Exchange and Tuber Yield in 
# Jerusalem Artichoke Cultivars" (Field Crops Research [1991]) 
# reported on various plant characteristics. Consider the following
# data on chlorophyll concentration (in grams per square meter) 
# for four varieties of Jerusalem artichoke:
         # n Mean StdDev.
# Variety1 5 0.3  0.12
# Variety2 5 0.24 0.089
# Variety3 4 0.41 0.1
# Variety4 6 0.33 0.054
#Do the data suggest that true average chlorophyll concentration 
#depends on the variety? State and test the appropriate hypotheses 
#at a level of 0.05.

varietytype= c('Variety1','Variety2','Variety3','Variety4')
varietyn= c(5,5,4,6)
varietymean= c(0.3,0.24,0.41,0.33)
varietysd= c(0.12,0.089,0.1,0.054)
artichokedata = data.frame(varietyn,varietymean,varietysd,row.names = varietytype)
artichokedata
require(rpsychi)
artichokef= with(artichokedata, ind.oneway.second(varietymean,varietysd,varietyn))
artichokef
artichokep= pf(2.657,3,16,lower.tail = F)
artichokep
# we fail to reject. No enough evidnace that chlorophyll 
# concentration depends on the variety



install.packages('Rcmdr')

 0.95*0.95*0.95*0.95
1- 0.8145062
