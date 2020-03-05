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
SLAData <- read.csv("data/05.00 Incident Data.csv")





# Switch SLAFail from 0 and 1 to No and Yes
SLAData$SLAFail <- as.factor(recode(SLAData$SLAFail, '0' = "No", '1' = "Yes"))

SLAData$Open_Time_HourOfDay <- as.factor(as.character(SLAData$Open_Time_HourOfDay))

# summarize the data
str(SLAData)
# check for null values
apply(is.na(SLAData), 2, which)

quantPredVars <- c('Count_Related_Interactions',
                   'Count_Related_Incidents',
                   'Count_Related_Changes'
                   )
# SLAData$Open_Time_HourOfDay < as.factor(SLAData$Open_Time_HourOfDay)
qualPredVars <- c('Urgency',
                  'Open_Time_DayOfWeek',
                  'Open_Time_HourOfDay',
                  'Service_Component_WBS_aff',
                  'KM_number',
                  'CI_TypeSubType_aff')

res.famd <- FAMD(SLAData, graph = FALSE)

# Visualize eigenvalues (scree plot). Show the percentage of variances explained by each principal component.
#png(filename = "01. screeplot.png", width=1000, height=1000)
fviz_screeplot(res.famd)
##dev.off()

# Correlation between variables (qualitative and quantitative) with principal dimensions:
#png(filename = "02. famd var.png", width=1000, height=1000)
plot(res.famd, choix = "var")
#dev.off()

# Correlation circle of quantitative variables:
#png(filename = "03. famd quanti.png", width=1000, height=1000)
plot(res.famd, choix = "quanti")
#dev.off()

# Plot of variables
#png(filename = "04. famd_var.png", width=1000, height=1000)
fviz_famd_var(res.famd, repel = TRUE)
#dev.off()

# Contribution to the first dimension
#png(filename = "05. contrib var axes 1.png", width=1000, height=1000)
fviz_contrib(res.famd, "var", axes = 1)
#dev.off()

# Contribution to the second dimension
#png(filename = "06. contrib var axes 1.png", width=1000, height=1000)
fviz_contrib(res.famd, "var", axes = 2)
#dev.off()

#png(filename = "07. quant contrib.png", width=1000, height=1000)
fviz_famd_var(res.famd, "quanti.var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)
#dev.off()

png(filename = "08. quali contrib.png", width=1000, height=1000)
fviz_famd_var(res.famd, "quali.var", col.var = "contrib", 
              gradient.cols = c("#f2f2f2", "#E7B800", "#FC4E07"),
              repel = TRUE)
dev.off()

### variable clustering
##
##



#####3
variable_tree <- hclustvar(X.quanti = subset(SLAData, select = quantPredVars),
                          X.quali = subset(SLAData, select = qualPredVars),
                          )
#png(filename = "09. dendo.png", width=1000, height=1000)
plot(variable_tree)
#dev.off()

#png(filename = "10. stability.png", width=1000, height=1000)
stability(variable_tree, B=25)
#dev.off()
clus3 <- cutreevar(variable_tree, 3)
clus3$var
