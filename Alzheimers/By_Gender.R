# We will need to select certain columns that apply to the research of cognitive decline.  Our research consists of narrowing the location down to 4 major City States. We will also separate two age groups which are 50-64 years old vs 65 years and older, in order to compare them.  


keeps <- c("YearStart", "LocationDesc", "Class", "Data_Value", "Stratification1", "Stratification2")
Data1 <- Alzheimer_s_Disease_and_Healthy_Aging_Data[keeps]

# We are subsetting our dataset so that we can proceed to linear regression model. There is too much data that we are not going to use for the purpose of our research.  

justCD <- Data1[Data1$Class == "Cognitive Decline",]

# We are subsetting our dataset to include only Cognitive Decline.  This is the topic that we are most interested in.
#we are including data from the whole United States rather than the previous where we included only 4 states


males <- justCD[(justCD$Stratification2 == "Male") & (justCD$Stratification1 == "Overall"),]
females <- justCD[(justCD$Stratification2 == "Female") & (justCD$Stratification1 == "Overall"),]

#making tables to separate the age groups


firstGroup <-males
secondGroup <- females
#We will need to separate the age groups and their data values in order to run our Ind-T Tests.  


firstGroupData <- firstGroup["Data_Value"]


names(firstGroupData)[names(firstGroupData) == "Data_Value"] <- "males"

#Here we are renaming our column to something more logical. "males" will be only data for males

# First group consists of ages 50-64

firstGroupData['Date'] <-firstGroup["YearStart"]
firstGroupData['State'] <-firstGroup["LocationDesc"]



#Here we are making a new table that has just the values for Females, as well as the date variables.

secondGroupData <- secondGroup["Data_Value"]

names(secondGroupData)[names(secondGroupData) == "Data_Value"] <- "females"
#Here we are renaming our column to something more logical. "females" will be Data for just females.


secondGroupData['Date'] <-secondGroup["YearStart"]

#here we are making a new table with only male data, as well as date variables.


#This variable will hold just Male Data
combine <- secondGroupData

#Here we insert our male data into our table, finally making a table with separated values based on sex
combine['males'] <-firstGroupData['males']

final <- na.omit(combine)



keep <- c("Date", "males", "females")

final <- final[,keep]

#here we combine both tables to get one table that has both age group data, but are separated into different columns.
#We also do some rearranging.





#Now We Test for Independent T-Test Assumptions

library(reshape2)
ff <- melt(final, id="Date")


#Testing for Homogeneity of Variance. Seems like we are not Homogeneous because our test came out significant. We will make sure to use var.equal=False when doing our t-test

library("dplyr")
library("rcompanion")
library("car")
result = leveneTest(value ~ variable, ff)
result


#Testing For Normality of Data
library(rcompanion)
plotNormalHistogram(ff$value)


#Our Data is slightly positively skewed it seems. So I will adjust by Square rooting it.

ff$scored_bySQRT <- sqrt(ff$value)

plotNormalHistogram(ff$scored_bySQRT)
#Technically speaking it is better based off of our blue line, but visually it is still not super normal. We will still go with this model 



#We do an Independent T-test
t_ind <- t.test(final$males, final$females, alternative="two.sided", var.equal=FALSE)
print(t_ind)


final$males_bySQRT <- sqrt(final$males)
final$females_bySQRT <- sqrt(final$females)

t_ind <- t.test(final$males_bySQRT, final$females_SQRT, alternative="two.sided", var.equal=FALSE)
print(t_ind)
#Seems like there is a difference between gender


t_ind <- t.test(final$males, final$females, alternative="less", var.equal=FALSE)
print(t_ind)

t_ind <- t.test(final$males_bySQRT, final$females_SQRT, alternative="less", var.equal=FALSE)
print(t_ind)

#Here, we can see that our test is significant: we reject the null and say that we are confident that males seem to have less cognitive decline compared to females


library(ggplot2)
library(tidyverse)


ggplot(ff, aes(x=variable, y=value)) + geom_boxplot() +stat_summary(fun.y="mean")

#Looking at the Boxplots, we can see that indeed both our medians and averages are different, supporting our findings.

