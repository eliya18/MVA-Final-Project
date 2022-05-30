library(tidyverse)
library(xlsx)
library(openxlsx)
houses_1 <- select(train,c(1,2,3,5,9,10,20,21,39,40,42,44,45,47,50,51,52,54,55,63,67:72, 78,81))
houses <- loadWorkbook(("selection.xlsx"))
writeData(houses, houses_1, sheet="selection1", colNames=TRUE, rowNames=TRUE)
saveWorkbook(houses, "selection.xlsx", overwrite = TRUE)

#normalize
#normalization of numeric vbles

library(dplyr)
scaled_Prostate <- x %>%
  mutate_if(is.numeric, scale)

scaled_Prostate <- as.data.frame(scaled_Prostate)

cont <- scaled_Prostate[, c(1,2,5,6,8,9,10,11)]