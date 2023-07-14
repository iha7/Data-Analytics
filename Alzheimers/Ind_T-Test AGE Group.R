# We will need to select certain columns that apply to the research of cognitive decline.  Our research consists of narrowing the location down to 4 major City States. We will also separate two age groups which are 50-64 years old vs 65 years and older, in order to compare them.  


keeps <- c("YearStart", "LocationDesc", "Class", "Data_Value", "Stratification1")
Data1 <- Alzheimer_s_Disease_and_Healthy_Aging_Data[keeps]

# We are subsetting our dataset so that we can proceed to linear regression model. There is too much data that we are not going to use for the purpose of our research.  

justCD <- Data1[Data1$Class == "Cognitive Decline",]

# We are subsetting our dataset to include only Cognitive Decline.  This is the topic that we are most interested in.
#we are including data from the whole United States rather than the previous where we included only 4 states



younger <- justCD[(justCD$Stratification1 == "50-64 years"),]
older <- justCD[(justCD$Stratification1 == "65 years or older"),]

#making tables to separate the age groups

first <-younger
second <- older


#We will need to separate the age groups and their data values in order to run our Ind-T Tests.  


firstGroupData <- first["Data_Value"]



names(firstGroupData)[names(firstGroupData) == "Data_Value"] <- "Younger"

#Here we are renaming our column to something more logical. "Younger" will be the elderly aged from 50-64
 

firstGroupData['Date'] <-first["YearStart"]
firstGroupData['State'] <-first["LocationDesc"]


#Here we are making a new table that has just the values of 50-64 year olds, as well as the date variables.

secondGroupData <- second["Data_Value"]

names(secondGroupData)[names(secondGroupData) == "Data_Value"] <- "Older"
#Here we are renaming our column to something more logical. "Older" will be the elderly aged from 65 and over


secondGroupData['Date'] <-second["YearStart"]

#here we are making a new table with only 65 years or older data, as well as date variables.



#here we start to combine the two table
combine <- secondGroupData


combine['Younger'] <-firstGroupData['Younger']

final <- na.omit(combine)

#here we combine both tables to get one table that has both age group data, but are separated into different columns.

keep <- c("Date", "Younger", "Older")

final <- final[,keep]



#Now We Test for Independent T-Test Assumptions
library(reshape2)
ff <- melt(final, id="Date")


library("dplyr")
library("rcompanion")
library("car")


#Testing for homogeneity of Variance - it looks like we are not homogeneous, but we will continue for now.  We will make sure to use var.equal=False when doing our t-test
result = leveneTest(value ~ variable, ff)
result

#Testing For Normality of Data
library(rcompanion)
plotNormalHistogram(ff$value)



#Our Data is slightly positively skewed it seems. So I will adjust by Square rooting it.
ff$scored_bySQRT <- sqrt(ff$value)

plotNormalHistogram(ff$scored_bySQRT)
#Our data looks much more normal like the bell curve.

final$Younger_bySQRT <- sqrt(final$Younger)
final$Older_bySQRT <- sqrt(final$Older)

#Doing a T-test 
t_ind <- t.test(final$Younger, final$Older, alternative="two.sided", var.equal=FALSE)
print(t_ind)



t_ind <- t.test(final$Younger_bySQRT, final$Older_bySQRT, alternative="two.sided", var.equal=FALSE)
print(t_ind)

#Since our P value is less than .05 we can come to the conclusion that there seems to be a difference between age groups


t_ind <- t.test(final$Younger, final$Older, alternative="greater", var.equal=FALSE)
print(t_ind)


t_ind <- t.test(final$Younger_bySQRT, final$Older_bySQRT, alternative="greater", var.equal=FALSE)
print(t_ind)

#Here we ran a test to see if the older age group has less cognitive decline.  Now we can see that our test is significant: we reject the null and say that we are confident that our data shows that the 65 and older elderly experiences less cognitive decline in our dataset.  
#Sidenote-  Common sense wise, one would think the opposite.  So there can be many factors that led to this.  But this is what the data shows. There can be various possibilities: 1.  It can be that most cases that are actively reported are in the younger age group.  It can also mean that most of the older age group by this time is in nursing homes.



library(ggplot2)
library(tidyverse)


ggplot(ff, aes(x=variable, y=value)) + geom_boxplot() +stat_summary(fun.y="mean")

#Looking at the Boxplots, we can see that indeed both our medians and averages are different, supporting our findings.


