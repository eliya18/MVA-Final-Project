# MVA-Final-Project

library(readxl)
library(dplyr)
library(FactoMineR)
library(tidyverse)

#load dataset
dataset <- read_excel("selection.xlsx", sheet=2)
names(dataset)

#drop additional categories
houses <- subset(dataset, select=-c(MSSubClass,MSZoning,LandContour,KitchenQual,TotRmsAbvGrd,EnclosedPorch,ScreenPorch,YrSold,24))

View(houses)
summary(houses)
names(houses)
str(houses)
dim(houses)

#state type to variables
#character to factor
houses$CentralAir <- as.factor(houses$CentralAir)
houses$FullBath <- as.factor(houses$FullBath)
houses$HalfBath <- as.factor(houses$HalfBath)
houses$Heating <- as.factor(houses$Heating)

#drop utilities
houses$Utilities <- NULL

#transform YearRemodAdd to factor Y/N
houses$YearRemodAdd <- if_else(houses$YearRemodAdd==houses$YearBuilt,"N","Y")
houses$YearRemodAdd <- as.factor(houses$YearRemodAdd)

#transform YearBuilt to segments
houses$YearBuilt <- cut(houses$YearBuilt, breaks = c(1872,1920,1970,Inf),labels = c("Old", "MedOld", "Recent"), include.lowest = TRUE )
str(houses$YearBuilt)

#transform BedroomAbvGr to segments
houses$BedroomAbvGr <- cut(houses$BedroomAbvGr, breaks = c(0,3,Inf), labels = c("Up to 3", "More than 3"), include.lowest=TRUE)
str(houses$BedroomAbvGr)

#recategorize Heating
houses$Heating <-levels(houses$Heating) <- list("Gas"=c("GasA","GasW"), "Floor/Wall"=c("Floor", "Wall"), "Other"=c("Grav", "OthW"))

library(DataExplorer)
create_report(houses, y= "SalePrice")

houses %>%
  kbl(caption = "Houses Dataset") %>%
  kable_classic(full_width = F, html_font = "Cambria")

#outliers 

#normalize
#normalization of numeric vbles

#scaled <- x %>% mutate_if(is.numeric, scale)

#Id to rowname
houses %>%
  remove_rownames() %>%
  column_to_rownames(var = 'Id')

rownames(houses)<- houses$Id
houses <- as.data.frame(houses)

#MCA
#select categorical and salesPrice as supplementary

houses_cat <- subset(houses, select= c(3,4,6,7,11,12,13,18))

res.mca <- MCA(houses_cat,quanti.sup=8,ncp= 10, graph=TRUE, level.ventil = 0.01)
help(MCA)
res.mca

#rearreange variables to use all cuantitive as supplementary
