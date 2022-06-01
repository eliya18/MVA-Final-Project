library(readxl)
library(dplyr)
library(FactoMineR)
library(tidyverse)
library(kableExtra)
library(ggplot2)

#load dataset
dataset <- read_excel("selection_2.xlsx", sheet=2)

names(dataset)
#drop additional categories
houses <- subset(dataset, select=-c(MSSubClass,MSZoning,LandContour,KitchenQual,TotRmsAbvGrd,YrSold))

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

#Id to rowname
houses %>%
  remove_rownames() %>%
  column_to_rownames(var = 'Id')

houses <- as.data.frame(houses)
rownames(houses)<- houses$Id

#continuous 
houses_cont <- subset(houses, select= c(2,5,6,9,10,14,15))

#categorical 
houses_cat <- subset(houses, select= c(3,4,7,8,11,12,13))

#outliers 
library(ggstatsplot)
boxplot(houses_cont)$out

#log for large magnitudes
houses_cont1 <- houses_cont 
names(houses_cont1)
houses_cont1$LotArea<- log(0.5+houses_cont1$LotArea)
houses_cont1$TotalBsmtSF<- log(0.5+houses_cont1$TotalBsmtSF)
houses_cont1$GrLivArea<- log(0.5+houses_cont1$GrLivArea)
houses_cont1$GarageArea<- log(0.5+houses_cont1$GarageArea)
houses_cont1$`1stFlrSF`<- log(0.5+houses_cont1$`1stFlrSF`)
houses_cont1$BsmtUnfSF<- log(0.5+houses_cont1$BsmtUnfSF)
houses_cont1$SalePrice<- log(0.5+houses_cont1$SalePrice)

boxplot(houses_cont1)$out

#scale 
scaled <- houses_cont %>% mutate_if(is.numeric, scale)
boxplot(scaled)$out

#detect
outliers_LotArea <- boxplot(scaled$LotArea, plot=FALSE)$out
outliers_TotalBSmtSF <- boxplot(scaled$TotalBsmtSF, plot=FALSE)$out
outliers_1stFlrSF <- boxplot(scaled$`1stFlrSF`, plot=FALSE)$out
outliers_GrLivArea <- boxplot(scaled$GrLivArea, plot=FALSE)$out
outliers_GarageArea <- boxplot(scaled$GarageArea, plot=FALSE)$out
outliers_BsmtUnfSF <- boxplot(scaled$BsmtUnfSF, plot=FALSE)$out
outliers_SalePrice <- boxplot(scaled$SalePrice, plot=FALSE)$out
#aprox 320

#plots for continous data logTransformed
library(GGally)
ggpairs(houses_cont1)

#barplot for categorical 
grid.arrange(
  ggplot(data = houses_cat, aes())+geom_bar(),
  ncol=2)

#MCA
#only cat
res.mca1 <- MCA(houses_cat,ncp= 10, graph=TRUE, level.ventil = 0.01)
help(MCA)
res.mca1

#cat with dependant as supplementary
houses_cat2 <- subset(houses, select= c(3,4,7,8,11,12,13,15))
res.mca2 <- MCA(houses_cat2,quanti.sup=8,ncp= 10, graph=TRUE, level.ventil = 0.01)
res.mca2

#reorder variables to use all quantitative as supplementary
houses_mca3 <- houses[, c(3,4,7,8,11,12,13,2,5,6,9,10,14,15)]
res.mca3 <- MCA(houses_mca3,quanti.sup =8:14,ncp= 10, graph=TRUE, level.ventil = 0.01)

#heating and centralair as supp
houses_mca <- houses[, c(3,4,11,12,13,2,5,6,9,10,14,15,7,8)]
res.mca <- MCA(houses_mca,quanti.sup =6:12,quali.sup=13:14,ncp= 10, graph=TRUE, level.ventil = 0.01)

#get eigan values
res.mca$eig
# Graph of the eigenvalues
res.mca$eig
barplot(res.mca$eig[,2],main="Eigenvalues", names.arg=1:nrow(res.mca$eig))
##criteria to select how many dimensions are kept
sum(res.mca$eig[,1]>(1/5)) ## keep 10 as requested in statement

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
plot(res.mca,invisible=c("var","quali.sup"),cex=.5,label="none",title="Graph of the individuals", habillage="FullBath")

fviz_mca_biplot(res.mca, select.ind = list(contrib = 10), 
                ggtheme = theme_minimal())

# Graphs of the categories
plot(res.mca,invis=c("ind","quali.sup"),col.var=c(rep(c("black","red"),17),"black",rep("red",4)),title="Graph of the active categories")

# Graphs of the variables
plot(res.mca,choix="var",title="Graph of the variables")
plot(res.mca,choix="quanti.sup",title="Graph of the continuous variables")
plot(res.mca,choix="quali.sup",title="Graph of the categorical variables")
library(FactoMineR)
library(factoextra)
fviz_mca_biplot(res.mca,select.ind = list(contrib = 100, repel=TRUE))
fviz_mca_var(res.mca, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())
help("fviz_mca_var")
res.mca$ind$coord
res.mca$var$contrib

#LDA
library(MASS)
summary(houses_cont$SalePrice)

#we need to transform the response variable into categories
#we will segment prices into low, mid and high

houses_lda <- houses_cont
houses_lda$SalePrice <- cut(houses_lda$SalePrice, breaks = c(34900,274900,514000,Inf),labels = c("lowP", "MidP", "HighP"), include.lowest = TRUE )
str(houses_lda$SalePrice)

library(GGally)
ggpairs(houses_lda, columns = 1:6, 
        ggplot2::aes(colour=SalePrice),
        title="Correlation matrix. House data")

#separate the data into train and test
library(caret)
library(klaR)
library(mda)

set.seed(123)
training.samples <- houses_lda$SalePrice %>%
  createDataPartition(p = 0.8, list = FALSE)

train.houses <- houses_lda[training.samples, ]
test.houses <- houses_lda[-training.samples, ]
paste0("Proportion of training is ", round((nrow(train.houses)/nrow(houses_cont1))*100,2),"%")
paste0("Proportion of test is ", round((nrow(test.houses)/nrow(houses_cont1))*100,2),"%")

model_lda <- lda(SalePrice ~ . , data = train.houses)
model_lda
plot(model_lda)
