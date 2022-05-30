library(readxl)
library(dplyr)
library(FactoMineR)
library(tidyverse)
library(kableExtra)

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
  kbl(caption = "Houses Final Dataset") %>%
  kable_classic(full_width = F, html_font = "Cambria")

library(summarytools)
dfSummary(houses) %>%
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

houses <- as.data.frame(houses)
rownames(houses)<- houses$Id

#divide in continous and categorical
houses_cont <- subset(houses, select= c(2,5,8,9,10,14,15,16,17) )

#select categorical 
houses_cat <- subset(houses, select= c(3,4,6,7,11,12,13))

#and categorical with salesPrice as supplementary
houses_cat2 <- subset(houses, select= c(3,4,6,7,11,12,13,18))

library(GGally)
ggpairs(houses_cont)

#MCA
#only prices as sup
res.mca1 <- MCA(houses_cat,quanti.sup=8,ncp= 10, graph=TRUE, level.ventil = 0.01)
help(MCA)
res.mca1

#reorder variables to use all quantitative as supplementary
houses_mca <- houses[, c(3,4,6,7,11,12,13,2,5,8,9,10,14,15,16,17,18)]
res.mca <- MCA(houses_mca,quanti.sup =8:17,ncp= 10, graph=TRUE, level.ventil = 0.01)

#get eigan values
res.mca$eig
# Graph of the eigenvalues
res.mca$eig
barplot(res.mca$eig[,2],main="Eigenvalues", names.arg=1:nrow(res.mca$eig))
##criteria to select how many dimensions are kept
sum(res.mca$eig[,1]>(1/7)) ## keep 10 as requested in statement

# Result summaries of MCA
res.mca$var
res.mca$ind
res.mca$quanti.sup
res.mca$quali.sup

summary(res.mca)

# Graphs of the individuals
plot(res.mca,invisible=c("var","quali.sup"),cex=.5,label="none",title="Graph of the individuals") 
plot(res.mca,invisible=c("var","quali.sup"),cex=.5,label="none",title="Graph of the individuals", habillage="CentralAir") 
plot(res.mca,invisible=c("var","quali.sup"),cex=.5,label="none",title="Graph of the individuals", habillage="YearRemodAdd") 
plot(res.mca,invisible=c("var","quali.sup"),cex=.5,label="none",title="Graph of the individuals", habillage="BedroomAbvGr") 
plot(res.mca,invisible=c("var","quali.sup"),cex=.5,label="none",title="Graph of the individuals", habillage="YearBuilt") 

fviz_mca_biplot(res.mca, select.ind = list(contrib = 50), 
                ggtheme = theme_minimal())

# Graphs of the categories
plot(res.mca,invis=c("ind","quali.sup"),col.var=c(rep(c("black","red"),17),"black",rep("red",4)),title="Graph of the active categories")

# Graphs of the variables
plot(res.mca,choix="var",title="Graph of the variables")
plot(res.mca,choix="quanti.sup",title="Graph of the continuous variables")
library(FactoMineR)
library(factoextra)
fviz_mca_biplot(res.mca,select.ind = list(contrib = 20, repel=TRUE))
fviz_mca_var(res.mca, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())
help("fviz_mca_var")
res.mca$ind$coord
res.mca$var$contrib

