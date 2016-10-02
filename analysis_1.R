setwd("C:/Users/p054009/Documents/GitHub/Kaggle House Prices AGT")

library(vtreat)
library(ggplot2)
library(caret)
library(xgboost)

train <- read.csv("./data/train.csv")
test <- read.csv("./data/test.csv")

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
test_num[is.na(test_num$BsmtFinSF1), c("BsmtFinSF1" ,  "BsmtFinSF2",   "BsmtUnfSF",  "TotalBsmtSF", "BsmtFullBath",  "BsmtHalfBath")] <- 0
test_num[is.na(test_num$BsmtFullBath), c("BsmtFullBath","BsmtHalfBath")] <- 0



for(i in 1:ncol(train)){
    if (class(train[,i]) == "character"){
        train[,i] <- as.factor(train[,i])
    }
}


nums <- sapply(train, function(x)(is.numeric(x) | is.integer(x)))
train_num <- train[, nums]
ncol(train[, nums])
head(train[,-nums])

nums <- sapply(test, is.numeric)
test_num <- test[, nums]

nas2 <- apply(test_num, 2,function(x)sum(is.na(x))/length(x))
nas2[nas2 >0]




head(train_num)
ncol(train_num)
ir.pca <- prcomp(train_num[,-c(1,38)],
                 center = TRUE,
                 scale. = TRUE)


tes.pca <- predict(ir.pca, test_num[,-1])
train.pca <- data.frame(ir.pca$x)
class(train.pca)
price <- scale(train_num$SalePrice)
hist(price)
hist(log(train_num$SalePrice+1))
train.pca$SalePrice <- log(train_num$SalePrice + 1)

sed(1904)
fit4 <-train(SalePrice~PC1+PC2+PC3+PC4+PC5+PC6, data = train.pca, method = "rf")
fit1 <-train(SalePrice~PC1+PC2+PC3+PC4+PC5, data = train.pca, method = "lm")
nrow(tes.pca)

res4 <- predict(fit4, tes.pca)
length(res)
submission <- data.frame(Id = test$Id, SalePrice = (exp(res)-1))
write.csv(submission, "./predictions/submission_pca_num_1.csv", row.names=FALSE)

res4 <- predict(fit4, tes.pca)
length(res)
submission <- data.frame(Id = test$Id, SalePrice = (exp(res4)-1))
write.csv(submission, "./predictions/submission_pca_num_4.csv", row.names=FALSE)

res <- predict(fit1, tes.pca)
length(res)
submission <- data.frame(Id = test$Id, SalePrice = (exp(res4)-1)*0.7+(exp(res)-1)*0.3)
write.csv(submission, "./predictions/submission_pca_num_3.csv", row.names=FALSE)

res <- predict(fit1, tes.pca)
length(res)
submission <- data.frame(Id = test$Id, SalePrice = (exp(res4)-1)*0.7+res5*0.3)
write.csv(submission, "./predictions/submission_pca_num_5.csv", row.names=FALSE)



y_train <- train.pca$SalePrice
labels_train <- data.frame(train.pca$SalePrice, y_train)

xgb_train <- xgb.DMatrix(model.matrix(~ PC1+PC2+PC3+PC4+PC5, data = train.pca),
                         label=y_train, missing=NA)

xgb_test <- xgb.DMatrix(model.matrix(~ PC1+PC2+PC3+PC4+PC5, data = data.frame(tes.pca)),
                          missing=NA)

# build model


xgb_model <- xgboost(xgb_train, y_train, nrounds=100, 
                     num_class=5, eval_metric="rmse",
                     early.stop.round=TRUE)


class(tes.pca)
