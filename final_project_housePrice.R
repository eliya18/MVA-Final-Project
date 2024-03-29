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
houses$Id <- NULL

#outliers 
library(ggstatsplot)
boxplot(houses_cont)$out
boxplot(houses_cont1)$out

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

#take SalePrice 
salePrice_og <- subset(houses, select= c(17))
colnames(salePrice_og) <- c('SalesPrice_og')

#scale 
scaled <- houses %>% mutate_if(is.numeric, scale)

#detect
outliers_LotArea <- boxplot(scaled$LotArea, plot=FALSE)$out
outliers_TotalBSmtSF <- boxplot(scaled$TotalBsmtSF, plot=FALSE)$out
outliers_2ndFlrSF <- boxplot(scaled$`2ndFlrSF`, plot=FALSE)$out
outliers_1stFlrSF <- boxplot(scaled$`1stFlrSF`, plot=FALSE)$out
outliers_GrLivArea <- boxplot(scaled$GrLivArea, plot=FALSE)$out
outliers_GarageArea <- boxplot(scaled$GarageArea, plot=FALSE)$out
outliers_WoodDeckSF <- boxplot(scaled$WoodDeckSF, plot=FALSE)$out
outliers_OpenPorchSF <- boxplot(scaled$OpenPorchSF, plot=FALSE)$out
outliers_PoolArea <- boxplot(scaled$PoolArea, plot=FALSE)$out
outliers_SalePrice <- boxplot(scaled$SalePrice, plot=FALSE)$out
#aprox 320

#Join scaled with SalePrice_og
scaled1 <- left_join(scaled, salePrice_og, by='Id')

#remove
scaled<- scaled[-which(scaled$`2ndFlrSF` %in% outliers_2ndFlrSF),] #only 2
scaled<- scaled[-which(scaled$LotArea %in% outliers_LotArea),]
scaled<- scaled[-which(scaled$TotalBsmtSF %in% outliers_TotalBSmtSF),]
scaled<- scaled[-which(scaled$`1stFlrSF` %in% outliers_1stFlrSF),]
scaled<- scaled[-which(scaled$GrLivArea %in% outliers_GrLivArea),]
scaled<- scaled[-which(scaled$GarageArea %in% outliers_GarageArea),]
scaled<- scaled[-which(scaled$WoodDeckSF %in% outliers_WoodDeckSF),]
scaled<- scaled[-which(scaled$OpenPorchSF %in% outliers_OpenPorchSF),]
scaled<- scaled[-which(scaled$PoolArea %in% outliers_PoolArea),]
scaled<- scaled[-which(scaled$SalePrice %in% outliers_SalePrice),]

houses_clean <- scaled

#continuous 
houses_cont <- subset(houses_clean, select= c(1,4,7,8,9,13,14,15,16))

#categorical 
houses_cat <- subset(houses_clean, select= c(2,3,5,6,10,11,12))

#plots for continous data 
library(GGally)
ggpairs(houses_cont)

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
houses_cat2 <- subset(houses_clean, select= c(2,3,5,6,10,11,12,17))
res.mca2 <- MCA(houses_cat2,quanti.sup=8,ncp= 10, graph=TRUE, level.ventil = 0.01)
res.mca2

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

#LDA
library(MASS)
summary(houses_cont1$SalePrice)
summary(houses_cont1$PoolArea)

#we need to transform the response variable into categories
#we will segment prices into low, mid and high

#unscale SalePrice
houses_cont1$SalePrice<-unscale(houses_cont1$SalePrice)
houses_cont1$SalePrice <- cut(houses_cont1$SalePrice, breaks = c(34900,274900,514000,Inf),labels = c("lowP", "MidP", "HighP"), include.lowest = TRUE )
str(houses_cont1$SalePrice)

library(GGally)
ggpairs(houses_cont1, columns = 1:5, 
        ggplot2::aes(colour=SalePrice),
        title="Correlation matrix. House data")

ggpairs(houses_cont1, columns = 6:9, 
        ggplot2::aes(colour=SalePrice),
        title="Correlation matrix. House data")

#separate the data into train and test
library(caret)
library(klaR)
library(mda)

set.seed(123)
training.samples <- houses_cont1$SalePrice %>%
  createDataPartition(p = 0.8, list = FALSE)

train.houses <- houses_cont1[training.samples, ]
test.houses <- houses_cont1[-training.samples, ]
paste0("Proportion of training is ", round((nrow(train.houses)/nrow(houses_cont1))*100,2),"%")
paste0("Proportion of test is ", round((nrow(test.houses)/nrow(houses_cont1))*100,2),"%")

model_lda <- lda(SalePrice ~ . , data = train.houses)
model_lda
plot(model_lda)