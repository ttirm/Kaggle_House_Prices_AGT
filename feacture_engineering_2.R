
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

train$remodeled <- ifelse(train$YearRemodAdd-train$YearBuilt > 0, 1, 0)
test$remodeled <- ifelse(test$YearRemodAdd-test$YearBuilt > 0, 1, 0)

train$age <- ifelse(train$YearRemodAdd-train$YearBuilt > 0, 
                    train$YrSold-train$YearRemodAdd, train$YrSold-train$YearBuilt)

test$age <- ifelse(test$YearRemodAdd-test$YearBuilt > 0, 
                    test$YrSold-test$YearRemodAdd, test$YrSold-test$YearBuilt)

train_garage <- train[,grep("Garage",names(train))]
test_garage <- test[,grep("Garage",names(test))]

train_garage$SalePrice <- train$SalePrice
test_garage$SalePrice <- test$SalePrice

train_garage$PavedDrive <- train$PavedDrive
test_garage$PavedDrive <- test$PavedDrive

train_garage$GarageQual <- factor(train_garage$GarageQual, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"))
train_garage$GarageQual <- as.numeric(train_garage$GarageQual)

test_garage$GarageQual <- factor(test_garage$GarageQual, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"))
test_garage$GarageQual <- as.numeric(test_garage$GarageQual)

train_garage$GarageCond <- factor(train_garage$GarageCond, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"))
train_garage$GarageCond <- as.numeric(train_garage$GarageCond)

test_garage$GarageCond <- factor(test_garage$GarageCond, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"))
test_garage$GarageCond <- as.numeric(test_garage$GarageCond)

train_garage$GarageFinish <- factor(train_garage$GarageFinish, levels = c("None", "Unf", "RFn", "Fin"))
train_garage$GarageFinish <- as.numeric(train_garage$GarageFinish)

test_garage$GarageFinish <- factor(test_garage$GarageFinish, levels = c("None", "Unf", "RFn", "Fin"))
test_garage$GarageFinish <- as.numeric(test_garage$GarageFinish)

train_garage$PavedDrive <- factor(train_garage$PavedDrive, levels = c("N", "P", "Y"))
train_garage$PavedDrive <- as.numeric(train_garage$PavedDrive)

test_garage$PavedDrive <- factor(test_garage$PavedDrive, levels = c("N", "P", "Y"))
test_garage$PavedDrive <- as.numeric(test_garage$PavedDrive)


dummies = model.matrix(~factor(train_garage$GarageType))
dummies <- as.data.frame(dummies[,2:ncol(dummies)])
nam <- sapply(names(dummies), function(x)paste0("g",unlist(strsplit(x, ")"))[2]))
names(dummies) <- nam
train_garage <- cbind(train_garage[,2:ncol(train_garage)], dummies)
train_garage <- sapply(train_garage, as.numeric)

dummies = model.matrix(~factor(test_garage$GarageType))
dummies <- as.data.frame(dummies[,2:ncol(dummies)])
nam <- sapply(names(dummies), function(x)paste0("g",unlist(strsplit(x, ")"))[2]))
names(dummies) <- nam
test_garage <- cbind(test_garage[,2:ncol(test_garage)], dummies)
test_garage <- sapply(test_garage, as.numeric)

train_tot <- train_garage
test_tot <- test_garage


# Basement
#############################################################
train_basement <- train[,grep("Bsmt",names(train))]
test_basement <- test[,grep("Bsmt",names(test))]

train_basement$BsmtQual <- factor(train_basement$BsmtQual, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"))
train_basement$BsmtQual <- as.numeric(train_basement$BsmtQual)

test_basement$BsmtQual <- factor(test_basement$BsmtQual, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"))
test_basement$BsmtQual <- as.numeric(test_basement$BsmtQual)

train_basement$BsmtExposure <- factor(train_basement$BsmtExposure, levels = c("None", "No", "Mn", "Av", "Gd"))
train_basement$BsmtExposure <- as.numeric(train_basement$BsmtExposure)

test_basement$BsmtExposure <- factor(test_basement$BsmtExposure, levels = c("None", "No", "Mn", "Av", "Gd"))
test_basement$BsmtExposure <- as.numeric(test_basement$BsmtExposure)

train_basement$BsmtFinType1 <- factor(train_basement$BsmtFinType1, levels = c("None", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"))
train_basement$BsmtFinType1 <- as.numeric(train_basement$BsmtFinType1)

test_basement$BsmtFinType1 <- factor(test_basement$BsmtFinType1, levels = c("None", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"))
test_basement$BsmtFinType1 <- as.numeric(test_basement$BsmtFinType1)


train_basement <- sapply(train_basement, as.numeric)
test_basement <- sapply(test_basement, as.numeric)

train_tot <- cbind(train_tot, train_basement)
test_tot <- cbind(test_tot, test_basement)


train_tot <- data.frame(train_tot)
test_tot <- data.frame(test_tot)

train_tot$Overallqual1 <- train$OverallCond*train$OverallQual
test_tot$Overallqual1 <- test$OverallCond*test$OverallQual

train_tot$age <- train$age
test_tot$age <- test$age

train_tot$remodeled <- train$remodeled
test_tot$remodeled <- test$remodeled

train_r <- train[, -(grep("Bsmt|Garage|Id|age|remodeled|SalePrice|PavedDrive|OverallCond|OverallQual",names(train)))]
zones <- c("MSSubClass", "MSZoning", "Neighborhood", "Condition1", "BldgType", "HouseStyle")
train_zones <- train[, zones]
test_zones <- test[, zones]

train_z1 <- train_zones
for(i in 1:ncol(train_zones)){
    dummies = model.matrix(~factor(train_zones[,i]))
    dummies <- as.data.frame(dummies[,2:ncol(dummies)])
    nam <- sapply(names(dummies), function(x)unlist(strsplit(x, ")"))[2])
    names(dummies) <- nam
    train_z1 <- cbind(train_z1, dummies)
}
train_z1 <- train_z1[, 7:ncol(train_z1)]


train_tot <- cbind(train_tot, train_z1)

test_z1 <- test_zones

for(i in 1:ncol(test_zones)){
    dummies = model.matrix(~factor(test_zones[,i]))
    dummies <- as.data.frame(dummies[,2:ncol(dummies)])
    nam <- sapply(names(dummies), function(x)unlist(strsplit(x, ")"))[2])
    names(dummies) <- nam
    test_z1 <- cbind(test_z1, dummies)
}
test_z1 <- test_z1[, 7:ncol(test_z1)]


test_tot <- cbind(test_tot, test_z1)


train_r <- train_r[, !(names(train_r) %in% zones)]

exterior <- c("RoofStyle", "Exterior1st", "Exterior2nd", "MasVnrType", 
              "MasVnrArea","ExterQual", "ExterCond", "Foundation")

train_exterior <- train_r[, exterior]
test_exterior <- test[, exterior]

train_Ext <- train_exterior[, c(1,2,3,4,8)]
test_Ext <- test_exterior[, colnames(train_exterior[, c(1,2,3,4,8)])]
for(i in 1:ncol(train_Ext)){
    dummies = model.matrix(~factor(train_Ext[,i]))
    dummies <- as.data.frame(dummies[,2:ncol(dummies)])
    nam <- sapply(names(dummies), function(x)unlist(strsplit(x, ")"))[2])
    names(dummies) <- nam
    train_exterior <- cbind(train_exterior, dummies)
}

for(i in 1:ncol(test_Ext)){
    dummies = model.matrix(~factor(test_Ext[,i]))
    dummies <- as.data.frame(dummies[,2:ncol(dummies)])
    nam <- sapply(names(dummies), function(x)unlist(strsplit(x, ")"))[2])
    names(dummies) <- nam
    test_exterior <- cbind(test_exterior, dummies)
}

train_exterior <- train_exterior[, !(names(train_exterior) %in% colnames(train_Ext))]
test_exterior <- test_exterior[, !(names(test_exterior) %in% colnames(test_Ext))]


train_exterior$ExterQual <- factor(train_exterior$ExterQual, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
train_exterior$ExterQual <- as.numeric(train_exterior$ExterQual)

test_exterior$ExterQual <- factor(test_exterior$ExterQual, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
test_exterior$ExterQual <- as.numeric(test_exterior$ExterQual)

train_exterior$ExterCond <- factor(train_exterior$ExterCond, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
train_exterior$ExterCond <- as.numeric(train_exterior$ExterCond)

test_exterior$ExterCond <- factor(test_exterior$ExterCond, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
test_exterior$ExterCond <- as.numeric(test_exterior$ExterCond)

train_exterior$ExterQual1 <- train_exterior$ExterQual-train_exterior$ExterCond
test_exterior$ExterQual1 <- test_exterior$ExterQual-test_exterior$ExterCond

train_exterior <- train_exterior[,!(names(train_exterior) %in% c("ExterQual", "ExterCond"))]
test_exterior <- test_exterior[,!(names(test_exterior) %in% c("ExterQual", "ExterCond"))]

train_tot <- cbind(train_tot, train_exterior)
test_tot <- cbind(test_tot, test_exterior)

train_r <- train_r[, !(names(train_r) %in% exterior)]

train_lot <- train_r[,c("LotArea","LotShape","LotConfig")]
test_lot <- test[,c("LotArea","LotShape","LotConfig")]
train_lot1 <- train_lot[, c(2,3)]
test_lot1 <- test_lot[, c(2,3)]
for(i in 1:ncol(train_lot1)){
    dummies = model.matrix(~factor(train_lot1[,i]))
    dummies <- as.data.frame(dummies[,2:ncol(dummies)])
    nam <- sapply(names(dummies), function(x)unlist(strsplit(x, ")"))[2])
    names(dummies) <- nam
    train_lot <- cbind(train_lot, dummies)
}

train_lot <- train_lot[, -c(2,3)]

for(i in 1:ncol(test_lot1)){
    dummies = model.matrix(~factor(test_lot1[,i]))
    dummies <- as.data.frame(dummies[,2:ncol(dummies)])
    nam <- sapply(names(dummies), function(x)unlist(strsplit(x, ")"))[2])
    names(dummies) <- nam
    test_lot <- cbind(test_lot, dummies)
}

test_lot <- test_lot[, -c(2,3)]



train_tot <- cbind(train_tot, train_lot)
test_tot <- cbind(test_tot, test_lot)


train_r <- train_r[,!(names(train_r) %in% c("LotArea","LotShape","LotConfig"))]

train$FireplaceQu <- factor(train$FireplaceQu, levels = c("None","Po", "Fa", "TA", "Gd", "Ex"))
train$FireplaceQu <- as.numeric(train$FireplaceQu)

test$FireplaceQu <- factor(test$FireplaceQu, levels = c("None","Po", "Fa", "TA", "Gd", "Ex"))
test$FireplaceQu <- as.numeric(test$FireplaceQu)

train$Fireplace <- as.numeric(train$FireplaceQu)*as.numeric(train$Fireplaces)
test$Fireplace <- as.numeric(test$FireplaceQu)*as.numeric(test$Fireplaces)



facilities <- c("HeatingQC", "CentralAir", "Electrical")

train_fac <- train_r[, facilities]
test_fac <- test[, facilities]

train_fac$HeatingQC <- factor(train_fac$HeatingQC, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
train_fac$HeatingQC <- as.numeric(train_fac$HeatingQC)

test_fac$HeatingQC <- factor(test_fac$HeatingQC, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
test_fac$HeatingQC <- as.numeric(test_fac$HeatingQC)

train_fac$CentralAir <- factor(train_fac$CentralAir, levels = c("N", "Y"))
train_fac$CentralAir <- as.numeric(train_fac$CentralAir)

test_fac$CentralAir <- factor(test_fac$CentralAir, levels = c("N", "Y"))
test_fac$CentralAir <- as.numeric(test_fac$CentralAir)

train_fac$Electrical <- factor(train_fac$Electrical, levels = c("Mix", "FuseP", "FuseF", "FuseA", "SBrkr"))
train_fac$Electrical <- as.numeric(train_fac$Electrical)

test_fac$Electrical <- factor(test_fac$Electrical, levels = c("Mix", "FuseP", "FuseF", "FuseA", "SBrkr"))
test_fac$Electrical <- as.numeric(test_fac$Electrical)


train_fac <- sapply(train_fac, as.numeric)
test_fac <- sapply(test_fac, as.numeric)

train_tot <- cbind(train_tot, train_fac)
test_tot <- cbind(test_tot, test_fac)


train_r <- train_r[, !(names(train_r) %in% facilities)]


interior <- c("X1stFlrSF", "X2ndFlrSF", "GrLivArea", "FullBath", "HalfBath", "BedroomAbvGr", 
              "KitchenQual", "TotRmsAbvGrd", "WoodDeckSF", "OpenPorchSF")

train_int <- train_r[, interior]
test_int <- test[, interior]

train_int$KitchenQual <- factor(train_int$KitchenQual, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
train_int$KitchenQual <- as.numeric(train_int$KitchenQual)

test_int$KitchenQual <- factor(test_int$KitchenQual, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
test_int$KitchenQual <- as.numeric(test_int$KitchenQual)

train_int <- sapply(train_int, as.numeric)
test_int <- sapply(test_int, as.numeric)

train_tot <- cbind(train_tot, train_int)
test_tot <- cbind(test_tot, test_int)

train_r <- train_r[, !(names(train_r) %in% interior)]


sale <- c("SaleType", "SaleCondition")

train_sale <- train_r[,sale]
test_sale <- test[,sale]

train_sale1 <- train_sale
test_sale1 <- test_sale
for(i in 1:ncol(train_sale1)){
    dummies = model.matrix(~factor(train_sale1[,i]))
    dummies <- as.data.frame(dummies[,2:ncol(dummies)])
    nam <- sapply(names(dummies), function(x)unlist(strsplit(x, ")"))[2])
    names(dummies) <- nam
    train_sale <- cbind(train_sale, dummies)
}
train_sale <- train_sale[, -c(1,2)]
train_sale <- sapply(train_sale, as.numeric)


for(i in 1:ncol(test_sale1)){
    dummies = model.matrix(~factor(test_sale1[,i]))
    dummies <- as.data.frame(dummies[,2:ncol(dummies)])
    nam <- sapply(names(dummies), function(x)unlist(strsplit(x, ")"))[2])
    names(dummies) <- nam
    test_sale <- cbind(test_sale, dummies)
}
test_sale <- test_sale[, -c(1,2)]
test_sale <- sapply(test_sale, as.numeric)

train_tot <- cbind(train_tot, train_sale)
test_tot <- cbind(test_tot, test_sale)

rem <- colnames(train_tot[, !(names(train_tot) %in% names(test_tot))])
rem <- rem[-1]
m <- matrix(0, nrow = nrow(test_lot), ncol = length(rem))
m1 <- as.data.frame(m, row.names = FALSE )
names(m1) <- rem

test_tot <- cbind(test_tot, m1)
sum(apply(test_tot, 2,function(x)sum(is.na(x))/length(x)) >0)
train_r <- train_r[, !(names(train_r) %in% sale)]



dummies = model.matrix(~factor(train_r$MoSold))
dummies <- as.data.frame(dummies[,2:ncol(dummies)])
nam <- sapply(names(dummies), function(x)unlist(strsplit(x, ")"))[2])
names(dummies) <- nam
train_tot <- cbind(train_tot, dummies)

dummies = model.matrix(~factor(test$MoSold))
dummies <- as.data.frame(dummies[,2:ncol(dummies)])
nam <- sapply(names(dummies), function(x)unlist(strsplit(x, ")"))[2])
names(dummies) <- nam
test_tot <- cbind(test_tot, dummies)



train_tot$SalePrice <- log(train$SalePrice+1)


