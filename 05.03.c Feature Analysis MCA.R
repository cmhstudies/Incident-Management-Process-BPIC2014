library(readr)
library(dplyr)
library(caret)
library(FactoMineR)
library(factoextra)
library(ClustOfVar)
library(woe)

# set local working directory
setwd("~/Documents/GitHub/Incident-Management-Process-BPIC2014/")

### for SLAData
# import data from source file
SLAData <- read.csv("data/05.00 Incident Data CatVarsOnly.csv")





# Switch SLAFail from 0 and 1 to No and Yes
SLAData$SLAFail <- as.factor(recode(SLAData$SLAFail, '0' = "No", '1' = "Yes"))

# Set Open_Time_HourOfDay to factor
SLAData$Open_Time_HourOfDay <- as.factor(as.character(SLAData$Open_Time_HourOfDay))



# summarize the data
str(SLAData)
# check for null values
apply(is.na(SLAData), 2, which)

## drop the two high cardinality variables
SLADataSubSet = subset(SLAData, select = -c(Service_Component_WBS_aff,KM_number))
str(SLADataSubSet)

resSubSet.mca <- MCA(SLADataSubSet,  quali.sup = c(2), graph = FALSE)
summary(resSubSet.mca)
#png(filename = "p3.3.1.a.MCAVariableCategies.png", width=1000, height=1000)
# fviz_mca_var(res.mca, repel = TRUE)
#dev.off()

fviz_screeplot(resSubSet.mca, addlabels = TRUE, ylim = c(0, 2.5))

fviz_mca_var(resSubSet.mca, 
             repel = TRUE, 
             alpha.var = "cos2",
             col.var = "cos2",
             gradient.cols = c("white", "blue", "red"))

# plot(resSubSet.mca, invisible = c("ind"), selectMod = "contrib 20", )
