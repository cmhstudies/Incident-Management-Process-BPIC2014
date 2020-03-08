library(readr)
library(dplyr)
library(caret)
library(FactoMineR)
library(factoextra)
library(ClustOfVar)

# set local working directory
setwd("~/Documents/GitHub/Incident-Management-Process-BPIC2014/")

### for ChurnDataAsIs
# import data from source file
SLAData <- read.csv("data/05.00 Incident Data Encoded HiCard Vars.csv")

# Switch SLAFail from 0 and 1 to No and Yes
SLAData$SLAFail <- as.factor(recode(SLAData$SLAFail, '0' = "No", '1' = "Yes"))

# Set Open_Time_HourOfDay to factor
SLAData$Open_Time_HourOfDay <- as.factor(as.character(SLAData$Open_Time_HourOfDay))

# summarize the data
str(SLAData)
# check for null values
apply(is.na(SLAData), 2, which)

quantPredVars <- c('Service_Component_WBS_aff',
                   'KM_number',
                   'Count_Related_Interactions',
                   'Count_Related_Incidents',
                   'Count_Related_Changes',
                   'CI_TypeSubType_aff'
                   )
# SLAData$Open_Time_HourOfDay < as.factor(SLAData$Open_Time_HourOfDay)
qualPredVars <- c('Urgency',
                  'Open_Time_DayOfWeek',
                  'Open_Time_HourOfDay')


### variable clustering
variable_tree <- hclustvar(X.quanti = subset(SLAData, select = quantPredVars),
                          X.quali = subset(SLAData, select = qualPredVars),
                          )
png(filename = "reports/05.03.c hclust dendogram.png", height = 450, width = 800)
plot(variable_tree)
dev.off()


clus2 <- cutreevar(variable_tree, 2)
clus2$var


clus3 <- cutreevar(variable_tree, 3)
clus3$var



clus4 <- cutreevar(variable_tree, 4)
clus4$var