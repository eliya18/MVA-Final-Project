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
houses_lda$SalePrice <- cut(houses_lda$SalePrice, breaks = c(34900,114900,194900,274900,514000,Inf),labels = c("lowP","lowP1", "lowP2", "MidP", "HighP"), include.lowest = TRUE )
#houses_lda$SalePrice <- cut(houses_lda$SalePrice, breaks = c(34900,274900,514000,Inf),labels = c("lowP", "MidP", "HighP"), include.lowest = TRUE )

str(houses_lda$SalePrice)

library(GGally)
ggpairs(houses_lda, columns = 1:6, 
        ggplot2::aes(colour=SalePrice),
        title="Correlation matrix. House data")

#log transform fot better distribution inspect/fit
houses_lda$LotArea<- log(0.5+houses_lda$LotArea)
houses_lda$TotalBsmtSF<- log(0.5+houses_lda$TotalBsmtSF)
houses_lda$GrLivArea<- log(0.5+houses_lda$GrLivArea)
houses_lda$GarageArea<- log(0.5+houses_lda$GarageArea)
houses_lda$`1stFlrSF`<- log(0.5+houses_lda$`1stFlrSF`)
houses_lda$BsmtUnfSF<- log(0.5+houses_lda$BsmtUnfSF)

#scale 
scaled <- houses_lda %>% mutate_if(is.numeric, scale)
houses_lda <- scaled

#separate the data into train and test
library(caret)
library(klaR)
library(mda)

set.seed(123)
training.samples <- houses_lda$SalePrice %>%
  createDataPartition(p = 0.8, list = FALSE)

train.houses <- houses_lda[training.samples, ]
test.houses <- houses_lda[-training.samples, ]
paste0("Proportion of training is ", round((nrow(train.houses)/nrow(houses_lda))*100,2),"%")
paste0("Proportion of test is ", round((nrow(test.houses)/nrow(houses_lda))*100,2),"%")

model_fda <- fda(SalePrice ~ . , data = train.houses)
model_fda
plot(model_fda)

predictionsfda <- model_fda %>% predict(test.houses)

# Model accuracy
model_fda$confusion

confusion(model_fda,test.houses) %>% #Confusion in the test data 
  kbl(caption = "Confusion matrix in the test houses data") %>%
  kable_classic(full_width = F, html_font = "Cambria")

sum(diag(confusion(model_fda,test.houses)))/sum(confusion(model_fda,test.houses))

#lda to compare
model <- lda(SalePrice~., data = train.houses)
model
data.lda.values <- predict(model)
plot.data <- data.frame(X=data.lda.values$x[,1], 
                        Y=data.lda.values$x[,2],
                        SalePrice=train.houses$SalePrice)
head(plot.data)
ggplot(data=plot.data, aes(x=X, y=Y)) +
  geom_point(aes(color=SalePrice)) +
  xlab("LD1") + ylab("LD2") +
  theme_bw()

model_predictions <- model %>% predict(test.houses)
names(model_predictions)
head(model_predictions$class, 6)

model_predictions$posterior[sample(1:nrow(model_predictions$posterior), 10, replace=FALSE),] %>% 
  kbl(caption = "Predicted probabilities of class membership (sample of 10)") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# LDs Linear Discriminants
head(model_predictions$x, 3)

#model accuracy
mean(model_predictions$class==test.houses$SalePrice)

#Clustering 
library(kernlab)
library(gridExtra)
library(cluster)
library(factoextra)
library(stats)

#scaled  data
houses_clust<- houses %>% mutate_if(is.numeric, scale)
houses_clust$Id<- NULL
houses_clust

d <- dist(houses_clust, method = "euclidean")

hc.single   <- hclust(d, method="single")
hc.complete <- hclust(d, method="complete")
hc.average  <- hclust(d, method="average")
hc.ward     <- hclust(d, method="ward.D")

plot(hc.single, cex = 0.6, hang = -1)
plot(hc.complete, cex = 0.6, hang = -1)
plot(hc.average, cex = 0.6, hang = -1)
plot(hc.ward, cex = 0.6, hang = -1)

agnes.complete <- agnes(d, method="complete")

#The agglomerative coefficient is:
agnes.complete$ac

# vector of methods to compare
m <- c("average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(d, method = x)$ac
}  

library(purrr)

map_dbl(m, ac)

agnes.ward <- agnes(d, method = "ward")

pltree(agnes.ward, cex = 0.6, hang = -1, main = "Dendrogram of agnes")

#cut the tree
(clust <- cutree(agnes.ward, k = 3))
fviz_cluster(list(data= d, cluster = clust))

(clust4 <- cutree(agnes.ward, k = 4))
fviz_cluster(list(data= d, cluster = clust4))

(clust5 <- cutree(agnes.ward, k = 5))
fviz_cluster(list(data= d, cluster = clust5))

(clust6 <- cutree(agnes.ward, k = 6))
fviz_cluster(list(data= d, cluster = clust6))

#color groups in dendogram
pltree(agnes.ward, hang=-1, cex = 0.6)
rect.hclust(agnes.ward, k = 3, border = 2:5)

#kmeans
houses_clust<- houses_cont %>% mutate_if(is.numeric, scale)
set.seed(123)
k2 <- kmeans(houses_clust, centers = 2, nstart = 25)
k2 
fviz_cluster(k2, data=d)

k3 <- kmeans(houses_clust, centers = 3, nstart = 25)
k3 
fviz_cluster(k3, data=d)

k3 <- kmeans(houses_clust, centers = 3, nstart = 35)
k3 
fviz_cluster(k3, data=d)

k3 <- kmeans(houses_clust, centers = 3, nstart = 10)
k3 
fviz_cluster(k3, data=d)

k4 <- kmeans(houses_clust, centers = 4, nstart = 25)
k4 
fviz_cluster(k4, data=d)

k4 <- kmeans(houses_clust, centers = 4, nstart = 10)
k4 
fviz_cluster(k4, data=d)

k4 <- kmeans(houses_clust, centers = 4, nstart = 35)
k4 
fviz_cluster(k4, data=d)

k5 <- kmeans(houses_clust, centers = 5, nstart = 25)
k5 
fviz_cluster(k5, data=d)

#elbow method
fviz_nbclust(houses_clust, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)

#average siloutte
fviz_nbclust(houses_clust, kmeans, method = "silhouette")

gap_stat <- clusGap(houses_clust, FUN = kmeans, 
                    nstart = 25, K.max = 10, B = 10)

fviz_gap_stat(gap_stat)

library(NbClust)
nc <- NbClust(houses_clust, min.nc=2, max.nc=10, method="kmeans")

set.seed(123)
k2 <- kmeans(houses_clust, centers = 2, nstart = 10)
k2 
fviz_cluster(k2, data=d)

set.seed(123)
k2 <- kmeans(houses_clust, centers = 2, nstart = 50)
k2 
fviz_cluster(k2, data=d)

#Hierarchical K-Means Clustering
library(dplyr)
res.hk <-hkmeans(houses_clust, 2)
res.hk

fviz_dend(res.hk, cex = 0.6, palette = "jco", 
          rect = TRUE, rect_border = "jco", rect_fill = TRUE)

fviz_cluster(res.hk, palette = "jco", repel = TRUE,
             ggtheme = theme_classic())

#model based
library(mclust)

mc <- Mclust(houses_clust)
summary(mc) # Print a summary

mc$modelName  
mc$G 

library(kableExtra)
mc$z  %>%
  as.data.frame(.) %>% 
  sample_n(., 10, replace=FALSE) %>% 
  kbl(caption = "Probability to belong to a given cluster (sample of 10)") %>%
  kable_classic(full_width = F, html_font = "Cambria")

head(mc$classification,10)
plot.Mclust(mc, what="BIC")
plot.Mclust(mc, what="classification", addEllipses = TRUE)

fviz_mclust(mc, "classification", geom = "point", 
            pointsize = 1.5, palette = "jco")

fviz_mclust(mc, "uncertainty", palette = "jco")

table(mc$classification)
mc.G4 <- Mclust(houses_clust, G=4)
summary(mc.G4) # Print a summary

fviz_mclust(mc.G4, "classification", geom = "point", 
            pointsize = 1.5, palette = "jco")

table(houses_clust$hx)

table(houses_clust$hx, mc.G4$classification)
sum(diag(table(houses_clust$hx, mc.G4$classification)))/sum(table(houses_clust$hx, mc.G4$classification))
