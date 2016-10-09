setwd("C:/Users/p054009/Documents/GitHub/Kaggle_House_Prices_AGT")

library(ggplot2)
library(caret)
library(dplyr)
library(corrplot)
library(zoo)

getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}

load("./files/train1.Rda")

train$remodeled <- ifelse(train$YearRemodAdd-train$YearBuilt > 0, "yes", "no")
train$age <- ifelse(train$YearRemodAdd-train$YearBuilt > 0, 
                    train$YrSold-train$YearRemodAdd, train$YrSold-train$YearBuilt)

train_garage <- train[,grep("Garage",names(train))]
train_garage$SalePrice <- train$SalePrice
train_garage$PavedDrive <- train$PavedDrive

train_garage$GarageQual <- factor(train_garage$GarageQual)
levels(train_garage$GarageQual) <- c(5,2,4,0,1,3)
train_garage$GarageCond <- factor(train_garage$GarageCond)
levels(train_garage$GarageCond)  <- c(5,2,4,0,1,3)
head(train_garage)
train_garage$GarageFinish <- factor(train_garage$GarageFinish)
levels(train_garage$GarageFinish) <- c(3,0,2,1)
train_garage$PavedDrive <- factor(train_garage$PavedDrive)
levels(train_garage$PavedDrive) <- c(0,1,2)
train_garage$SalePrice <- log(train$SalePrice+1)


dummies = model.matrix(~factor(train_garage$GarageType))
dummies <- as.data.frame(dummies[,2:ncol(dummies)])
nam <- sapply(names(dummies), function(x)unlist(strsplit(x, ")"))[2])
names(dummies) <- nam
train_garage <- cbind(train_garage[,2:ncol(train_garage)], dummies)
train_garage <- sapply(train_garage, as.numeric)
garage.pca <- prcomp(train_garage[,-7],
                     center = TRUE,
                     scale. = TRUE)

plot(garage.pca, type = "l")

train1 <- garage.pca$x[,1:2]
names(train1) <- c("garage1", "garage2")

# Basement
#############################################################
train_basement <- train[,grep("Bsmt",names(train))]
train_basement$BsmtQual <- factor(train_basement$BsmtQual)
levels(train_basement$BsmtQual) <- c(5,2,4,0,3,1)
train_basement$BsmtExposure <- factor(train_basement$BsmtExposure)
levels(train_basement$BsmtExposure) <- c(3,4,2,1,0)
train_basement$BsmtFinType1 <- factor(train_basement$BsmtFinType1)
levels(train_basement$BsmtFinType1) <- c(5,4,6,2,0,3,1)

train_basement <- sapply(train_basement, as.numeric)
basement.pca <- prcomp(train_basement,
                       center = TRUE,
                       scale. = TRUE)

plot(basement.pca, type = "l")
train_basement2 <- basement.pca$x
train_basement3 <- as.data.frame(train_basement2[,1:2])
names(train_basement3) <- c("bsmt1", "bsmt2")
train1 <- cbind(train1, train_basement3)


train1$Overallqual1 <- train$OverallCond*train$OverallQual
train1$age <- train$age
train1$remodeled <- train$remodeled
train_r <- train[, -(grep("Bsmt|Garage|Id|age|remodeled|SalePrice|PavedDrive|OverallCond|OverallQual",names(train)))]
zones <- c("MSSubClass", "MSZoning", "Neighborhood", "Condition1", "BldgType", "HouseStyle")
train_zones <- train[, zones]

train_z1 <- train_zones
for(i in 1:ncol(train_zones)){
    dummies = model.matrix(~factor(train_zones[,i]))
    dummies <- as.data.frame(dummies[,2:ncol(dummies)])
    nam <- sapply(names(dummies), function(x)unlist(strsplit(x, ")"))[2])
    names(dummies) <- nam
    train_z1 <- cbind(train_z1, dummies)
}
train_z1 <- train_z1[, 7:ncol(train_z1)]
corrplot(cor(train_z1), method="square")
zones.pca <- prcomp(train_z1,
                    center = TRUE,
                    scale. = TRUE)

plot(zones.pca, type = "l")
train_z1 <- zones.pca$x
train_z1 <- as.data.frame(train_z1[,1:4])
head(train_z1)
names(train_z1) <- c("zone1", "zone2", "zone3", "zone4")

train1 <- cbind(train1, train_z1)

train_r <- train_r[, !(names(train_r) %in% zones)]

exterior <- c("RoofStyle", "Exterior1st", "Exterior2nd", "MasVnrType", 
              "MasVnrArea","ExterQual", "ExterCond", "Foundation")
head(train_r[, exterior])
train_exterior <- train_r[, exterior]

train_Ext <- train_exterior[, c(1,2,3,4,8)]
for(i in 1:ncol(train_Ext)){
    dummies = model.matrix(~factor(train_Ext[,i]))
    dummies <- as.data.frame(dummies[,2:ncol(dummies)])
    nam <- sapply(names(dummies), function(x)unlist(strsplit(x, ")"))[2])
    names(dummies) <- nam
    train_exterior <- cbind(train_exterior, dummies)
}
train_exterior <- train_exterior[, -c(1,2,3,4,8)]

levels(factor(train$ExterQual))
train_exterior$ExterQual <- factor(train_exterior$ExterQual)
levels(train_exterior$ExterQual) <- c(4,1,3,2,0)
train_exterior$ExterCond <- factor(train_exterior$ExterCond)
levels(train_exterior$ExterCond) <- c(4,1,3,0,2)
train_exterior <- sapply(train_exterior, as.numeric)


corrplot(cor(train_exterior), method="square")
exterior.pca <- prcomp(train_exterior,
                       center = TRUE,
                       scale. = TRUE)

plot(exterior.pca, type = "l")
train_exterior <- exterior.pca$x
train_exterior <- as.data.frame(train_exterior[,1:3])
names(train_exterior) <- c("exterior1", "exterior2", "exterior3")

train1 <- cbind(train1, train_exterior)

train_r <- train_r[, !(names(train_r) %in% exterior)]

train_lot <- train_r[,c("LotArea","LotShape","LotConfig")]
train_lot1 <- train_lot[, c(2,3)]
for(i in 1:ncol(train_lot1)){
    dummies = model.matrix(~factor(train_lot1[,i]))
    dummies <- as.data.frame(dummies[,2:ncol(dummies)])
    nam <- sapply(names(dummies), function(x)unlist(strsplit(x, ")"))[2])
    names(dummies) <- nam
    train_lot <- cbind(train_lot, dummies)
}

train_lot <- train_lot[, -c(2,3)]
corrplot(cor(train_lot), method="square")
lot.pca <- prcomp(train_lot,
                  center = TRUE,
                  scale. = TRUE)

plot(lot.pca, type = "l")
train_lot <- lot.pca$x

train_lot <- as.data.frame(train_lot[,1:2])
names(train_lot) <- c("lot1", "lot2")

train1 <- cbind(train1, train_lot)

train_r <- train_r[,!(names(train_r) %in% c("LotArea","LotShape","LotConfig"))]

train$Fireplace <- factor(train$FireplaceQu)
levels(train$Fireplace) <- c(5,2,4,0,1,3)
train$Fireplace <- as.numeric(train$Fireplace)*as.numeric(train$Fireplaces)

train1 <- cbind(train1, Fireplace= train$Fireplace)

train_r <- train_r[,!(names(train_r) %in% c("Fireplaces","FireplaceQu"))]

facilities <- c("HeatingQC", "CentralAir", "Electrical")

train_fac <- train_r[, facilities]
head(train_fac)
train_fac$HeatingQC <- factor(train_fac$HeatingQC)
levels(train_fac$HeatingQC) <- c(4,1,3,0,2)
train_fac$CentralAir <- factor(train_fac$CentralAir)
levels(train_fac$CentralAir) <- c(0,1)
train_fac$Electrical <- factor(train_fac$Electrical)
levels(train_fac$Electrical) <- c(3,2,1,0,4)

train_fac <- sapply(train_fac, as.numeric)


facilities.pca <- prcomp(train_fac,
                       center = TRUE,
                       scale. = TRUE)

plot(facilities.pca, type = "l")
train_fac <- facilities.pca$x
train_fac <- as.data.frame(train_fac[,1:2])
names(train_fac) <- c("facilities1", "facilities2")

train1 <- cbind(train1, train_fac)

train_r <- train_r[, !(names(train_r) %in% facilities)]
names(train_r)


interior <- c("X1stFlrSF", "X2ndFlrSF", "GrLivArea", "FullBath", "HalfBath", "BedroomAbvGr", 
              "KitchenQual", "TotRmsAbvGrd", "WoodDeckSF", "OpenPorchSF")

train_int <- train_r[, interior]

train_int$KitchenQual <- factor(train_int$KitchenQual)
levels(train_int$KitchenQual) <- c(4,1,3,2,0)
train_int <- sapply(train_int, as.numeric)

interior.pca <- prcomp(train_int,
                         center = TRUE,
                         scale. = TRUE)

plot(interior.pca, type = "l")
train_int <- interior.pca$x

train_int <- as.data.frame(train_int[,1:3])
names(train_int) <- c("interior1", "interior2", "interior3")

train1 <- cbind(train1, train_int)

train_r <- train_r[, !(names(train_r) %in% interior)]

names(train_r)
head(train_r$SaleCondition)

sale <- c("SaleType", "SaleCondition")
train_sale <- train_r[,sale]
train_sale1 <- train_sale
for(i in 1:ncol(train_sale1)){
    dummies = model.matrix(~factor(train_sale1[,i]))
    dummies <- as.data.frame(dummies[,2:ncol(dummies)])
    nam <- sapply(names(dummies), function(x)unlist(strsplit(x, ")"))[2])
    names(dummies) <- nam
    train_sale <- cbind(train_sale, dummies)
}
train_sale <- train_sale[, -c(1,2)]
train_sale <- sapply(train_sale, as.numeric)

sale.pca <- prcomp(train_sale,
                       center = TRUE,
                       scale. = TRUE)

plot(sale.pca, type = "l")
train_sale <- sale.pca$x

train_sale <- as.data.frame(train_sale[,1:2])
names(train_sale) <- c("sale1", "sale2")

train1 <- cbind(train1, train_sale)

train_r <- train_r[, !(names(train_r) %in% sale)]

train1$SalePrice <- log(train$SalePrice+1)
set.seed(1904)
train2 <- train1[, !(names(train1) %in% c("exterior1", "age", "exterior3"))]
fit_t2 <- train(SalePrice~., data = train2, method = "lm")
summary(fit_t1)


fit2 <-train(SalePrice~., data = train2, method = "rf")

aux <- table(train$Neighborhood, train$MSZoning)
aux <- data.frame(neighbourhood = rownames(aux), cols = colnames(aux)[apply(aux,1,which.max)])
testw <- test
testw[is.na(testw$MSZoning), "MSZoning"] <-  sapply(testw[is.na(testw$MSZoning), "Neighborhood"], function(x)aux[aux$neighbourhood == x,"cols"])

aux <- table(train$Neighborhood, train$Exterior1st)
aux <- data.frame(Exterior2nd = rownames(aux), cols = colnames(aux)[apply(aux,1,which.max)])
test[is.na(test$Exterior1st), "Exterior1st"] <-  sapply(test[is.na(test$Exterior1st), "Exterior2nd"], function(x)aux[aux$Exterior2nd == x,"cols"])

hist(train$LotFrontage)
ggplot(data = train, aes(x = factor(GarageYrBlt), y = SalePrice))+
    geom_point()


