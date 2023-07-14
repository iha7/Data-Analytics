#We will need to select certain columns that apply to the research of cognitive decline.  Our research consists of narrowing the location down to 4 major City States.  

keeps <- c("YearStart", "LocationDesc", "Class", "Data_Value", "Stratification1")
Data1 <- Alzheimer_s_Disease_and_Healthy_Aging_Data[keeps]

# We are subsetting our dataset so that we can proceed to linear regression model. There is too much data that we are not going to use for the purpose of our research.  

onlyCD <- Data1[Data1$Class == "Cognitive Decline",]

# We are subsetting our dataset to include only Cognitive Decline.  This is the topic that we are most interested in.

location <- onlyCD[(onlyCD$LocationDesc == "New York") | (onlyCD$LocationDesc == "Illinois") | (onlyCD$LocationDesc == "California") | (onlyCD$LocationDesc == "Texas"),]

#  We are also interested in subsetting 4 major citied states such as New York, Illinois, Texas and California.  They have the most population and demographic of people which is a good representation of United States as a whole.  This is also personal because I have three close elderly friends that are from these states.

Data3 <- location[location$Stratification1 == "Overall",]

#  We wanted all ages that showed decline in the elderly age group

na.omit(Data3)

# We omitted data we can't numerically use


finalData <- na.omit(Data3)



library(ggplot2)
d <- ggplot(finalData, aes(x = YearStart, y = Data_Value))
d + geom_point()

#Here we are attempting to make a scatter plot.  It does not seem to be too useful for our purposes.

cor.test(finalData$YearStart, finalData$Data_Value, method="pearson", use = "complete.obs")




library("car")
library("caret")
library("gvlma")
library("predictmeans")
library("e1071")
library("lmtest")

# Question Setup:  Does cognitive decline increase or decrease throughout the years of 2015-2020

scatter.smooth(x=finalData$YearStart, y=finalData$Data_Value, main="Cognitive Decline by Years")
lmMod <- lm(Data_Value~YearStart, data=finalData)
par(mfrow=c(2,2))
plot(lmMod)

lmtest::bptest(lmMod)

#P value of 0.12 not significant Breush-Pagan test.

car::ncvTest(lmMod)

#P value of 0.29 so not significant Non-constant Variance test

summary(influence.measures(lmMod))

#Here we check for outliers. Looking at our DFBETAS and DFFITS, we can see no values are above 1, which means there are not outliers present

lin_reg <- lm(Data_Value ~ YearStart, finalData)
print(lin_reg)

summary(lin_reg)


#P value seems to be insignificant because it is higher than 0.05.  We accept the null hypothesis.  Time does not seem to be a factor in the percentage of Cognitive Decline.  

