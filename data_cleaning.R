setwd("C:/Users/tiago_000/Documents/GitHub/Kaggle_House_Prices_AGT")

library(caret)

train <- read.csv("./data/train.csv", stringsAsFactors = FALSE)
test <- read.csv("./data/test.csv", stringsAsFactors = FALSE)

summary(train)
str(train)

dim(train)
log(1)

head(train)

nas <- apply(train, 2,function(x)sum(is.na(x))/length(x))
nam <- as.character(names(train))
length(nam)
length(nas)
names(nas) <- nam
nas <- nas[nas < 0.5]
nas

train <- train[,names(nas)]
test <- test[,names(nas)[-length(nas)]]

# Remove duplicates
###########################################
train <- unique(train)


# NAs treatment
###########################################
head(train$FireplaceQu)
head(train$Fireplaces)
train$FireplaceQu[is.na(train$FireplaceQu)] <- "None"

test$FireplaceQu[is.na(test$FireplaceQu)] <- "None"


head(train$LotFrontage,20)
summary(train$LotFrontage)
head(train$LotArea,20)
train$LotFrontage[is.na(train$LotFrontage)] <- mean(train$LotFrontage, na.rm = TRUE)

test$LotFrontage[is.na(test$LotFrontage)] <- mean(test$LotFrontage, na.rm = TRUE)


head(train$MasVnrType,50)
head(train$MasVnrArea,50)
train$MasVnrType[is.na(train$MasVnrType)] <- "None"
train$MasVnrArea[is.na(train$MasVnrArea)] <- 0

test$MasVnrType[is.na(test$MasVnrType)] <- "None"
test$MasVnrArea[is.na(test$MasVnrArea)] <- 0


head(train$BsmtQual)
str(train$BsmtQual)
head(train$BsmtCond)
train$BsmtQual[is.na(train$BsmtQual)] <- "None"
train$BsmtCond[is.na(train$BsmtCond)] <- "None"
train$BsmtExposure[is.na(train$BsmtExposure)] <- "None"
train$BsmtFinType1[is.na(train$BsmtFinType1)] <- "None"
train$BsmtFinType2[is.na(train$BsmtFinType2)] <- "None"

test$BsmtQual[is.na(test$BsmtQual)] <- "None"
test$BsmtCond[is.na(test$BsmtCond)] <- "None"
test$BsmtExposure[is.na(test$BsmtExposure)] <- "None"
test$BsmtFinType1[is.na(test$BsmtFinType1)] <- "None"
test$BsmtFinType2[is.na(test$BsmtFinType2)] <- "None"


head(train$Electrical)
train$Electrical[is.na(train$Electrical)] <- "SBrkr"

test$Electrical[is.na(test$Electrical)] <- "SBrkr"


head(train$GarageType)
train$GarageType[is.na(train$GarageType)] <- "None"
test$GarageType[is.na(test$GarageType)] <- "None"
head(train$GarageCars)
train$GarageCars[is.na(train$GarageCars)] <- 0
test$GarageCars[is.na(test$GarageCars)] <- 0
head(train$GarageArea)
train$GarageArea[is.na(train$GarageArea)] <- 0
test$GarageArea[is.na(test$GarageArea)] <- 0
head(train$GarageQual)
train$GarageQual[is.na(train$GarageQual)] <- "None"
test$GarageQual[is.na(test$GarageQual)] <- "None"
head(train$GarageCond)
train$GarageCond[is.na(train$GarageCond)] <- "None"
test$GarageCond[is.na(test$GarageCond)] <- "None"
head(train$GarageYrBlt)
train$GarageYrBlt[is.na(train$GarageYrBlt)] <- min(train$GarageYrBlt, na.rm = TRUE)
test$GarageYrBlt[is.na(test$GarageYrBlt)] <- min(test$GarageYrBlt, na.rm = TRUE)
head(train$GarageFinish)
train$GarageFinish[is.na(train$GarageFinish)] <- "None"
test$GarageFinish[is.na(test$GarageFinish)] <- "None"

nas2 <- apply(train, 2,function(x)sum(is.na(x))/length(x))
nas2[nas2 >0]

nas2 <- apply(test, 2,function(x)sum(is.na(x))/length(x))
nas2[nas2 >0]

# Imputation test
test[is.na(test$BsmtFinSF1), c("BsmtFinSF1" ,  "BsmtFinSF2",   "BsmtUnfSF",  "TotalBsmtSF", "BsmtFullBath",  "BsmtHalfBath")] <- 0
test[is.na(test$BsmtFullBath), c("BsmtFullBath","BsmtHalfBath")] <- 0

table(test$Fireplaces, test$FireplaceQu, exclude = NULL)

aux <- table(train$Neighborhood, train$MSZoning)
aux <- data.frame(neighbourhood = rownames(aux), cols = colnames(aux)[apply(aux,1,which.max)])
test[is.na(test$MSZoning), "MSZoning"] <-  sapply(test[is.na(test$MSZoning), "Neighborhood"], function(x)aux[aux$neighbourhood == x,"cols"])

aux <- table(train$Neighborhood, train$Exterior1st)
aux <- data.frame(Neighborhood = rownames(aux), cols = colnames(aux)[apply(aux,1,which.max)])
test[is.na(test$Exterior1st), "Exterior1st"] <-  sapply(test[is.na(test$Exterior1st), "Neighborhood"], function(x)aux[aux$Neighborhood == x,"cols"])

aux <- table(train$Neighborhood, train$Exterior2nd)
aux <- data.frame(Neighborhood = rownames(aux), cols = colnames(aux)[apply(aux,1,which.max)])
test[is.na(test$Exterior2nd), "Exterior2nd"] <-  sapply(test[is.na(test$Exterior2nd), "Neighborhood"], function(x)aux[aux$Neighborhood == x,"cols"])

table(train$SaleCondition, train$SaleType)
test[is.na(test$SaleType), "SaleType"] <- "WD" 

table(test$OverallQual, test$KitchenQual, exclude = NULL)
test[is.na(test$KitchenQual), "KitchenQual"] <-  "TA"




nz <- colnames(train[,nearZeroVar(train)])
train <- train[, !names(train) %in% nz]
test <- test[, !names(test) %in% nz]


train <- train[train$GrLivArea < 4000,]

save(train, file = "./files/train1.Rda")
save(test, file = "./files/test1.Rda")