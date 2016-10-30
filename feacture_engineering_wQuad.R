
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

cat$MSZoning[!(cat$MSZoning %in% c("FV", "RH", "RL", "RM"))] <- "other"
cat$LotShape[!(cat$LotShape %in% c("Reg"))] <- "IR"
cat$LotConfig[!(cat$LotConfig %in% c("Inside"))] <- "other"
cat$Condition1[!(cat$Condition1 %in% c("Norm", "Artery", "Feedr"))] <- "faci"
cat$BldgType[!(cat$BldgType %in% c("1Fam"))] <- "other"
cat$HouseStyle[(cat$HouseStyle %in% c("1.5Fin", "1.5Unf"))] <- "1Story"
cat$HouseStyle[(cat$HouseStyle %in% c("2.5Fin", "2.5Unf", "SFoyer", "SLvl"))] <- "2Story"
cat$RoofStyle[!(cat$RoofStyle %in% c("Gable", "Hip"))] <- "other"
cat$Exterior1st[!(cat$Exterior1st %in% c("HdBoard", "MetalSd", "Plywood", "VinylSd", "Wd Sdng"))] <- "other"
cat$Exterior2nd[!(cat$Exterior2nd %in% c("HdBoard", "MetalSd", "Plywood", "VinylSd", "Wd Sdng"))] <- "other"
cat$MasVnrType[!(cat$MasVnrType %in% c("None", "Stone"))] <- "Brk"
cat$Foundation[(cat$Foundation %in% c("Slab", "Stone", "Wood"))] <- "other"
cat$GarageType[(cat$GarageType %in% c("2Types", "Basment", "CarPort"))] <- "other"
cat$SaleType[!(cat$SaleType %in% c("New", "WD"))] <- "other"
cat$SaleCondition[!(cat$SaleCondition %in% c("Abnorml", "Normal", "Partial"))] <- "other"
# cat$MSSubClass <- factor(data$MSSubClass)
cat$MoSold <- factor(data$MoSold)
#cat$YrSold <- factor(data$YrSold)

exc <- names(cat)

dum <- dummyVars(~., data = cat)
cat <- data.frame(predict(dum, newdata = cat))

# nz <- colnames(cat[,nearZeroVar(cat)])
# cat <- cat[, !names(cat) %in% nz]

# Numerics
#############################################################################################################

num <- data[, sapply(data, is.numeric)]
names(num)
num <- num[,!(names(num) %in% c("Id", "MSSubClass", "MoSold", "YrSold", "age", "epoch", "GarageYrBlt", 
                                "YearBuilt", "YearRemodAdd", "SalePrice", "LotArea"))]
num <- num[, sapply(num, function(x) {range(x)[2]-range(x)[1]>500})]
# 
num2 <- num
num2_col <- sapply(names(num2), function(x) paste0(x, "_2"))
num2 <- data.frame(sapply(num2, function(x) x^(2)))
names(num2) <- num2_col
# 
num3 <- num
num3_col <- sapply(names(num3), function(x) paste0(x, "_3"))
num3 <- data.frame(sapply(num3, function(x) x^(1/2)))
names(num3) <- num3_col
# 
# 
data <- cbind(data,num2)
data <- cbind(data,num3)

data$remodeled <- ifelse(data$YearRemodAdd-data$YearBuilt > 0, 1, 0)
data$age <- ifelse(data$YearRemodAdd-data$YearBuilt > 0, 
                   data$YrSold-data$YearRemodAdd, data$YrSold-data$YearBuilt)

data$garage_age <- data$YrSold-data$GarageYrBlt

data$epoch <- ifelse(data$YrSold-data$YearBuilt >100, 2,ifelse(data$YrSold-data$YearBuilt >50, 1,0))


# data$OverallCond <- ifelse(data$OverallCond> 5, 1, ifelse(data$OverallCond< 5, -1,0))
# #data$OverallQual <- ifelse((data$OverallQual+data$OverallCond) < 0, 0,(data$OverallQual+data$OverallCond))
# 
# # data$OverallQual2 <- data$OverallQual^2
# 
# data$GarageCond <- ifelse(data$GarageCond> 4, 1, ifelse(data$GarageCond< 4, -1,0))
# #data$GarageQual <- ifelse((data$GarageQual+data$GarageCond) < 0, 0,(data$GarageQual+data$GarageCond))
# 
# 
# 
# 
# data$ExterCond <- ifelse(data$ExterCond> 3, 1, ifelse(data$ExterCond< 3, -1,0))
# #data$ExterQual <- ifelse((data$ExterQual+data$ExterCond) < 0, 0,(data$ExterQual+data$ExterCond))
# 
# 
# data$Fireplaces <- ifelse(data$Fireplaces > 1, 2, data$Fireplaces) 



data$space <-  data$X2ndFlrSF + data$X1stFlrSF

data$sec <- data$X2ndFlrSF/data$X1stFlrSF

data$bath <- data$FullBath+data$HalfBath

data$Bsmtbath <- data$BsmtFullBath + data$BsmtHalfBath

data$totbath <- data$Bsmtbath + data$bath

data$bathRoomRel <-  data$TotRmsAbvGrd- data$totbath

data$LotArea <- log(data$LotArea+1)

data <- data[, !(names(data) %in% c("GarageYrBlt", "YearBuilt", "YearRemodAdd", "BsmtFullBath", "BsmtHalfBath",
                                    "FullBath", "HalfBath", "X2ndFlrSF", "data$X1stFlrSF"
                                    #, 
                                    #"OverallCond", "GarageCond", "BsmtCond", "ExterCond" 
                                    ))]


data <- data <- data[, !(names(data) %in% exc)]

data <- cbind(data, cat)




train_tot <- data[1:nrow(train),]
train_tot$SalePrice <- log(y+1)
#train_tot <- train_tot[,-1]

sum(sapply(train_tot, class) != "numeric")

train_tot <- as.data.frame(sapply(train_tot, as.numeric))
test_tot <- data[(nrow(train)+1):nrow(data),]


