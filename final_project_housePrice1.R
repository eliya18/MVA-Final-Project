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

#Id to rowname
houses %>%
  remove_rownames() %>%
  column_to_rownames(var = 'Id')

houses <- as.data.frame(houses)
rownames(houses)<- houses$Id
#houses$Id <- NULL

# continuous 
houses_cont <- subset(houses, select= c(2,5,8,9,10,14,15,16,17))

#categorical 
houses_cat <- subset(houses, select= c(3,4,6,7,11,12,13))

#outliers
library(ggstatsplot)
boxplot(houses_cont$LotArea)$out

#log for large magnitudes
# houses_cont1$LotArea<- log(houses_cont1$LotArea)
# houses_cont1$TotalBsmtSF<- log(houses_cont1$TotalBsmtSF)
# houses_cont1$GrLivArea<- log(houses_cont1$GrLivArea)
# houses_cont1$GarageArea<- log(houses_cont1$GarageArea)
# houses_cont1$`1stFlrSF`<- log(houses_cont1$`1stFlrSF`)
# houses_cont1$`2ndFlrSF`<- log(houses_cont1$`2ndFlrSF`)
# houses_cont1$WoodDeckSF<- log(houses_cont1$WoodDeckSF)
# houses_cont1$OpenPorchSF<- log(houses_cont1$OpenPorchSF)
# houses_cont1$PoolArea<- log(houses_cont1$PoolArea)

#detect
outliers_LotArea <- boxplot(houses$LotArea, plot=FALSE)$out
outliers_TotalBSmtSF <- boxplot(houses$TotalBsmtSF, plot=FALSE)$out
outliers_2ndFlrSF <- boxplot(houses$`2ndFlrSF`, plot=FALSE)$out
outliers_1stFlrSF <- boxplot(houses$`1stFlrSF`, plot=FALSE)$out
outliers_GrLivArea <- boxplot(houses$GrLivArea, plot=FALSE)$out
outliers_GarageArea <- boxplot(houses$GarageArea, plot=FALSE)$out
outliers_WoodDeckSF <- boxplot(houses$WoodDeckSF, plot=FALSE)$out
outliers_OpenPorchSF <- boxplot(houses$OpenPorchSF, plot=FALSE)$out
outliers_PoolArea <- boxplot(houses$PoolArea, plot=FALSE)$out
outliers_SalePrice <- boxplot(houses$SalePrice, plot=FALSE)$out
#aprox 320

#remove
houses<- houses[-which(houses$`2ndFlrSF` %in% outliers_2ndFlrSF),] #only 2
houses<- houses[-which(houses$LotArea %in% outliers_LotArea),]
houses<- houses[-which(houses$TotalBsmtSF %in% outliers_TotalBSmtSF),]
houses<- houses[-which(houses$`1stFlrSF` %in% outliers_1stFlrSF),]
houses<- houses[-which(houses$GrLivArea %in% outliers_GrLivArea),]
houses<- houses[-which(houses$GarageArea %in% outliers_GarageArea),]
houses<- houses[-which(houses$WoodDeckSF %in% outliers_WoodDeckSF),]
houses<- houses[-which(houses$OpenPorchSF %in% outliers_OpenPorchSF),]
houses<- houses[-which(houses$PoolArea %in% outliers_PoolArea),]
houses<- houses[-which(houses$SalePrice %in% outliers_SalePrice),]

houses_clean <- houses
summary(houses_clean)

#take SalePrice 
#salePrice_og <- subset(houses, select= c(17))
#colnames(salePrice_og) <- c('SalesPrice_og')

#scale 
scaled <- houses_cont %>% mutate_if(is.numeric, scale)
boxplot(scaled)$out

#plots for continous data 
library(GGally)
ggpairs(houses_cont)

#barplot for categorical 
grid.arrange(
  ggplot(data = houses_cat, aes())+geom_bar(),
  ncol=2)

#MCA
#only categorical
res.mca1 <- MCA(houses_cat,ncp= 10, graph=TRUE, level.ventil = 0.01)
help(MCA)
res.mca1

#categorical with SalePrice as supplementary
houses_cat2 <- subset(houses, select= c(3,4,6,7,11,12,13,18))
res.mca2 <- MCA(houses_cat2,quanti.sup=8,ncp= 10, graph=TRUE, level.ventil = 0.01)
res.mca2

#MCA without unbalanced categorical

#reorder variables to use all quantitative as supplementary
houses_mca <- houses_clean[, c(2,3,5,6,10,11,12,1,4,7,8,9,13,14,15,16,17)]
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
plot(res.mca,invisible=c("var","quali.sup"),cex=.5,label="none",title="Graph of the individuals", habillage="FullBath")

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

#separate the data into train and test
library(caret)
set.seed(123)
training.samples <- houses_clean$SalePrice %>%
  createDataPartition(p = 0.8, list = FALSE)

train.houses <- houses_clean[training.samples, ]
test.houses <- houses_clean[-training.samples, ]
paste0("Proportion of training is ", round((nrow(train.houses)/nrow(houses_clean))*100,2),"%")
paste0("Proportion of test is ", round((nrow(test.houses)/nrow(houses_clean))*100,2),"%")

#LDA
library(MASS)
library(klaR)
library(mda)
summary(houses_clean$SalePrice)
summary(houses_clean$PoolArea)

#we need to transform the response variable into categories
#we will segment prices into low, mid and high
houses$SalePrice <- cut(houses$SalePrice, breaks = c(34900,274900,51400,Inf),labels = c("lowP", "MidP", "HighP"), include.lowest = TRUE )
str(houses_cont1$SalePrice)

library(GGally)
ggpairs(houses_cont1, columns = 1:5, 
        ggplot2::aes(colour=SalePrice),
        title="Correlation matrix. House data")

ggpairs(houses_cont1, columns = 6:9, 
        ggplot2::aes(colour=SalePrice),
        title="Correlation matrix. House data")

model_lda <- lda(SalePrice ~ . , data = train.houses)
model_lda
plot(model_lda)