
library(ggplot2)
library(caret)
library(dplyr)
library(corrplot)
library(zoo)
library(xgboost)
library(gdata)


getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}

load("./files/train1.Rda")
load("./files/test1.Rda")

y <- train$SalePrice

data <- rbind(train[, !(names(train) %in% 'SalePrice')], test)

data$remodeled <- ifelse(data$YearRemodAdd-data$YearBuilt > 0, 1, 0)
data$age <- ifelse(data$YearRemodAdd-data$YearBuilt > 0, 
                    data$YrSold-data$YearRemodAdd, data$YrSold-data$YearBuilt)

data$epoch <- ifelse(data$YrSold-data$YearBuilt >100, 2,ifelse(data$YrSold-data$YearBuilt >50, 1,0))

data$ExterQual <- ifelse(train$ExterQual+(train$ExterCond-5)<0, 0,train$ExterQual+(train$ExterCond-5))
data$OverallCond <- ifelse(data$OverallCond> 5, 1, ifelse(data$OverallCond< 5, -1,0))
data$OverallQual <- ifelse((data$OverallQual+data$OverallCond) < 0, 0,(data$OverallQual+data$OverallCond))



# Ordinal Categories
#############################################################################################################
data$GarageQual <- factor(data$GarageQual, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"))
data$GarageQual <- as.numeric(data$GarageQual)

data$GarageCond <- factor(data$GarageCond, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"))
data$GarageCond <- as.numeric(data$GarageCond)


data$GarageFinish <- factor(data$GarageFinish, levels = c("None", "Unf", "RFn", "Fin"))
data$GarageFinish <- as.numeric(data$GarageFinish)

data$PavedDrive <- factor(data$PavedDrive, levels = c("N", "P", "Y"))
data$PavedDrive <- as.numeric(data$PavedDrive)

data$BsmtQual <- factor(data$BsmtQual, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"))
data$BsmtQual <- as.numeric(data$BsmtQual)

data$BsmtExposure <- factor(data$BsmtExposure, levels = c("None", "No", "Mn", "Av", "Gd"))
data$BsmtExposure <- as.numeric(data$BsmtExposure)

data$BsmtFinType1 <- factor(data$BsmtFinType1, levels = c("None", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"))
data$BsmtFinType1 <- as.numeric(data$BsmtFinType1)

data$ExterQual <- factor(data$ExterQual, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
data$ExterQual <- as.numeric(data$ExterQual)

data$ExterCond <- factor(data$ExterCond, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
data$ExterCond <- as.numeric(data$ExterCond)

data$FireplaceQu <- factor(data$FireplaceQu, levels = c("None","Po", "Fa", "TA", "Gd", "Ex"))
data$FireplaceQu <- as.numeric(data$FireplaceQu)

data$HeatingQC <- factor(data$HeatingQC, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
data$HeatingQC <- as.numeric(data$HeatingQC)

data$CentralAir <- factor(data$CentralAir, levels = c("N", "Y"))
data$CentralAir <- as.numeric(data$CentralAir)

data$Electrical <- factor(data$Electrical, levels = c("Mix", "FuseP", "FuseF", "FuseA", "SBrkr"))
data$Electrical <- as.numeric(data$Electrical)

data$KitchenQual <- factor(data$KitchenQual, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
data$KitchenQual <- as.numeric(data$KitchenQual)


# Nominal Categories
#############################################################################################################
exc <- c("GarageQual", "GarageCond", "GarageFinish", "PavedDrive", "BsmtQual", "BsmtExposure", "BsmtFinType1", "ExterQual",
"ExterCond", "FireplaceQu", "HeatingQC", "CentralAir", "Electrical", "KitchenQual")


cat <- data[, sapply(data, is.character)]
cat <- cat[, !(names(cat) %in% exc)]


         