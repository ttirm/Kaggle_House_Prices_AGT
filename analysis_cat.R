setwd("C:/Users/p054009/Documents/GitHub/Kaggle House Prices AGT")

library(vtreat)
library(ggplot2)
library('caret')
library(VIM)

getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}

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

# NAs treatment
###########################################
head(train$FireplaceQu)
head(train$Fireplaces)
train$FireplaceQu[is.na(train$FireplaceQu)] <- "null"

test$FireplaceQu[is.na(test$FireplaceQu)] <- "null"


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
train$Electrical[is.na(train$Electrical)] <- "None"

test$Electrical[is.na(test$Electrical)] <- "None"


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
train$GarageYrBlt[is.na(train$GarageYrBlt)] <- median(train$GarageYrBlt, na.rm = TRUE)
test$GarageYrBlt[is.na(test$GarageYrBlt)] <- median(test$GarageYrBlt, na.rm = TRUE)
head(train$GarageFinish)
train$GarageFinish[is.na(train$GarageFinish)] <- "None"
test$GarageFinish[is.na(test$GarageFinish)] <- "None"

nas2 <- apply(train, 2,function(x)sum(is.na(x))/length(x))
nas2[nas2 >0]

nas2 <- apply(test, 2,function(x)sum(is.na(x))/length(x))
nas2[nas2 >0]

# Imputation test
head(test$MSZoning)

test_cat



for(i in 1:ncol(train)){
    if (class(train[,i]) == "character"){
        train[,i] <- as.factor(train[,i])
    }
}


nums <- sapply(train, is.numeric)
train_cat <- train[, !nums]
ncol(train[, !nums])


nums <- sapply(test, is.numeric)
test_cat <- test[, !nums]

nas2 <- apply(test_cat, 2,function(x)sum(is.na(x))/length(x))
nas2[nas2 >0]

test_cat$MSZoning[is.na(test_cat$MSZoning)] <- getmode(test_cat$MSZoning)
test_cat$Utilities[is.na(test_cat$Utilities)] <- getmode(test_cat$Utilities)
test_cat$Exterior1st[is.na(test_cat$Exterior1st)] <- getmode(test_cat$Exterior1st)
test_cat$Exterior2nd[is.na(test_cat$Exterior2nd)] <- getmode(test_cat$Exterior2nd)
test_cat$KitchenQual[is.na(test_cat$KitchenQual)] <- getmode(test_cat$KitchenQual)
test_cat$Functional[is.na(test_cat$Functional)] <- getmode(test_cat$Functional)
test_cat$SaleType[is.na(test_cat$SaleType)] <- getmode(test_cat$SaleType)


for(i in 1:ncol(test_cat)){
    if (class(test_cat[,i]) == "character"){
        test_cat[,i] <- as.factor(test_cat[,i])
    }
}


sed(1904)
train_cat$SalePrice <- train$SalePrice
fit2 <-train(SalePrice~., data = train_cat, method = "rf", importance = TRUE)
varImp(fit2)


feat <- c("SalePrice", "FireplaceQu", "MSZoning", "ExterQual", "BldgType","HouseStyle",  "BsmtQual", "GarageType", "KitchenQual")

train_cat$SalePrice <- log(train_cat$SalePrice + 1)
fit3 <-train(SalePrice~., data = train_cat[,feat], method = "rf")

res <- predict(fit3, test_cat)
length(res)
submission <- data.frame(Id = test$Id, SalePrice = (exp(res)-1))
write.csv(submission, "./predictions/submission_pca_num_2.csv", row.names=FALSE)
fit1

res5 <- predict(fit2, test_cat)
