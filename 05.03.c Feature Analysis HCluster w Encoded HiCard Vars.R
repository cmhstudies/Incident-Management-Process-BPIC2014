library(readr)
library(dplyr)
library(caret)
library(FactoMineR)
library(factoextra)
library(ClustOfVar)
library(woe)

# set local working directory
setwd("~/Documents/GitHub/Incident-Management-Process-BPIC2014/")

### for ChurnDataAsIs
# import data from source file
SLAData <- read.csv("data/05.00 Incident Data Encoded HiCard Vars.csv")





# Switch SLAFail from 0 and 1 to No and Yes
SLAData$SLAFail <- as.factor(recode(SLAData$SLAFail, '0' = "No", '1' = "Yes"))



# summarize the data
str(SLAData)
# check for null values
apply(is.na(SLAData), 2, which)

quantPredVars <- c('Service_Component_WBS_aff',
                   'KM_number',
                   'Count_Related_Interactions',
                   'Count_Related_Incidents',
                   'Count_Related_Changes',
                   'Open_Time_HourOfDay',
                   'CI_TypeSubType_aff'
                   )
# SLAData$Open_Time_HourOfDay < as.factor(SLAData$Open_Time_HourOfDay)
qualPredVars <- c('Urgency',
                  'Open_Time_DayOfWeek')

res.famd <- FAMD(SLAData, graph = FALSE)

# Visualize eigenvalues (scree plot). Show the percentage of variances explained by each principal component.
fviz_screeplot(res.famd)

# Correlation between variables (qualitative and quantitative) with principal dimensions:
plot(res.famd, choix = "var")
# Correlation circle of quantitative variables:
plot(res.famd, choix = "quanti")

# Plot of variables
fviz_famd_var(res.famd, repel = TRUE)
# Contribution to the first dimension
fviz_contrib(res.famd, "var", axes = 1)
# Contribution to the second dimension
fviz_contrib(res.famd, "var", axes = 2)

fviz_famd_var(res.famd, "quanti.var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)


fviz_famd_var(res.famd, "quali.var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)

### variable clustering
variable_tree <- hclustvar(X.quanti = subset(SLAData, select = quantPredVars),
                          X.quali = subset(SLAData, select = qualPredVars),
                          )
plot(variable_tree)

stability(variable_tree, B=25)

clus3 <- cutreevar(variable_tree, 3)
clus3$var
