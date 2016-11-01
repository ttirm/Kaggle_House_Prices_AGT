
set.seed(1904)

train_id <- train_tot[,"Id"]
train_tot <- train_tot[,!(names(train_tot) %in% c("Id"))]

control <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "RMSE"
fit2 <-train(SalePrice~., data = train_tot, method = "rf", metric=metric, trControl=control, preProcess=c("center","scale"))

fit7 <-train(SalePrice~., data = train_tot, method = "rlm", metric=metric, trControl=control, preProcess=c("center","scale", "pca"))

fit8 <-train(SalePrice~., data = train_tot, method = "pls", metric=metric, trControl=control, preProcess=c("center","scale"))

fit_t5 <-train(SalePrice~., data = train_tot, method = "svmLinear", metric=metric, trControl=control, preProcess=c("center","scale"))

fit_t3 <- train(SalePrice~., data = train_tot, method = "lm", trControl=control, preProcess=c("center","scale","pca"))

fit_t1 <- train(SalePrice~., data = train_tot, method = "kknn", trControl=control, preProcess=c("center","scale"))

fit_t4 <- train(SalePrice~., data = train_tot, method = "extraTrees", trControl=control, preProcess=c("center","scale"))



summary(fit_t3)

lm_pred_t <- predict(fit_t3, train_tot)
lm_pred <- predict(fit_t3, test_tot)

rf_pred <- predict(fit2, test_tot)

submission <- data.frame(Id = test$Id, SalePrice = (exp(res)-1))
write.csv(submission, "./predictions/submission_pca_1.csv", row.names=FALSE)


#xgboost
##################################################################################

# cv.ctrl <- trainControl(method = "repeatedcv", repeats = 1,number = 3)
# 
# xgb.grid <- expand.grid(nrounds = 500,
#                         max_depth = c(8, 10),
#                         eta = c(0.01,0.025),
#                         gamma = c(0.0, 0.1, 0.2),
#                         colsample_bytree = c(0.4,0.5),
#                         min_child_weight = 1     #default=1
# )
# 
# xgb_tune <-train(SalePrice ~.,
#                  data=train_tot,
#                  method="xgbTree",
#                  metric = "RMSE",
#                  #trControl=cv.ctrl,
#                  tuneGrid=xgb.grid
# )


train_x <- train_tot[, !(names(train_tot) %in% c("Id", "SalePrice"))]



y_train <- train_tot$SalePrice

#y_test <- train_tot$SalePrice


xgb_train <- xgb.DMatrix(model.matrix(~., data = train_x),
                         label=y_train, missing=NA)


#All
##############################################################################################################
history <- xgb.cv(data = xgb_train, nround=800, nthread = 6, nfold = 10, metrics=list("rmse"),
                  max.depth =4, eta = 0.03, gamma = 0.01, colsample_bytree = 0.1,  objective = "reg:linear")

#Linear
##############################################################################################################
history <- xgb.cv(data = xgb_train, nround=800, nthread = 6, nfold = 10, metrics=list("rmse"),
                  max.depth =6, eta = 0.025, gamma = 0.023, colsample_bytree = 0.2,  objective = "reg:linear")


param<-list(
    objective = "reg:linear",
    eval_metric = "rmse",
    booster = "gbtree",
    max_depth = 4,
    eta = 0.03,
    gamma = 0.01, 
    colsample_bytree = 0.1,
    min_child_weight=1
)

Training <-
    xgb.train(params = param,
              data = xgb_train,
              nrounds = 1000,
              watchlist = list(train = xgb_train),
              verbose = TRUE,
              print_every_n = 50,
              nthread = 6)


xgb_test_t <- xgb.DMatrix(model.matrix(~., data = test_x),
                        missing=NA)
xgb_pred_t <- predict(Training,newdata= xgb_test_t)
# RMSE(y_test,xgb_pred_t)

xgb_test <- xgb.DMatrix(model.matrix(~., data = test_tot[,-1]),
                        missing=NA)

xgb_pred <- predict(Training, newdata=xgb_test)
Submit <- data.frame(Id= test$Id, SalePrice= (exp(xgb_pred)-1))
write.csv(Submit, "./predictions/submission_xgboost2.csv", row.names=FALSE)




# Ridge Regression
##########################################################################################
CARET.TRAIN.CTRL <- trainControl(method="repeatedcv",
                                 number=10,
                                 repeats=10,
                                 verboseIter=FALSE)

lambdas <- seq(1,0,-0.001)

# train model
set.seed(123)  # for reproducibility

model_ridge <- train(SalePrice ~ .,
                     data = train_tot,
                     method="glmnet",
                     metric="RMSE",
                     maximize=FALSE,
                     
                     trControl=CARET.TRAIN.CTRL,
                     tuneGrid=expand.grid(alpha=0, # Ridge regression
                                          lambda=lambdas))


ggplot(data=filter(model_ridge$result,RMSE<0.14)) +
    geom_line(aes(x=lambda,y=RMSE))

mean(model_ridge$resample$RMSE)



ridge_pred_t <- predict(model_ridge,newdata= train_tot)

ridge_pred <- predict(model_ridge,newdata= test_tot)

Submit<-data.frame(Id= test$Id, SalePrice= ridge_pred)
write.csv(Submit, "./predictions/submission_ridge1.csv", row.names=FALSE)

newx = data.matrix(test_tot)
ridge.pred=predict(model_ridge,newx=newx)
preds <- exp(predict(model_lasso,newx=newx)) - 1
Submit<-data.frame(Id= test$Id, SalePrice= preds)
write.csv(Submit, "./predictions/submission_lasso1.csv", row.names=FALSE)


#Lasso
##########################################################################################
set.seed(123)  # for reproducibility
model_lasso <- train(SalePrice ~ .,
                     data = train_tot,
                     method="glmnet",
                     metric="RMSE",
                     maximize=FALSE,
                     trControl=CARET.TRAIN.CTRL,
                     tuneGrid=expand.grid(alpha=1,  # Lasso regression
                                          lambda=c(1,0.1,0.05,0.01,seq(0.009,0.001,-0.001),
                                                   0.00075,0.0005,0.0001)))



ggplot(data=filter(model_lasso$result,RMSE<0.14)) +
    geom_line(aes(x=lambda,y=RMSE))

mean(model_lasso$resample$RMSE)

names(model_lasso)

model_lasso$modelInfo
coef(model_lasso$finalModel)
aux <- coef(model_lasso$finalModel, model_lasso$bestTune[,2])
aux <- as.matrix(aux)
aux <- data.frame(name = names(aux[,1]), val = aux[, 1], val2 = aux[, 1]^2)

aux1 <- aux[abs(aux$val2) >0,]
aux1 <- head(aux1[order(aux1$val2, decreasing = TRUE),"name"],101)

aux1 <- aux1[-1]
train_tot1 <- train_tot[,aux1]
train_tot1$SalePrice <- train_tot$SalePrice

plot(residuals(model_lasso))


mean(resid(model_lasso))

aux <- data.frame(id = train_id,res = residuals(model_lasso))

aux[abs(aux$res) >0.4,"id"]
length(model_lasso$pred)
lasso_pred_t <- predict(model_lasso,newdata= train_tot)

lasso_pred <- predict(model_lasso,newdata= test_tot)


res <- (exp(lasso_pred)-1)
res <- (exp(lasso_pred)-1)*0.5+(exp(xgb_pred)-1)*0.5
Submit<-data.frame(Id= test$Id, SalePrice= res)
write.csv(Submit, "./predictions/submission_ensembled_4.csv", row.names=FALSE)
write.csv(Submit, "./predictions/submission_lasso1.csv", row.names=FALSE)

#nnet
##########################################################################################

set.seed(123)  # for reproducibility
model_nnet <- train(SalePrice ~ .,
                    data = train_tot,
                    method="nnet",
                    metric="RMSE",
                    tuneLength=5,
                    maximize=FALSE,
                    trControl=CARET.TRAIN.CTRL)



nnet_pred <- predict(model_nnet,newdata= test_tot)


res <- (exp(lasso_pred)-1)*0.7+(exp(ridge_pred)-1)*0.3
Submit<-data.frame(Id= test$Id, SalePrice= res)
write.csv(Submit, "./predictions/submission_ensembled_3.csv", row.names=FALSE)
write.csv(Submit, "./predictions/submission_lasso1.csv", row.names=FALSE)


#knn regression
##########################################################################################

trainIndex <- createDataPartition(train_tot$SalePrice, p = .9,
                                  list = FALSE, 
                                  times = 1)

train_x <- train_tot[trainIndex, !(names(train_tot) %in% c("Id", "SalePrice"))]
y_train <- train_tot$SalePrice[trainIndex]

model_knn <- knn.reg(test= train_tot[-trainIndex, names(train_x)], train_x, y = y_train, k = 5)


RMSE(train_tot$SalePrice[-trainIndex], model_knn$pred)

model_knn$pred
length(test$Id)
length(model_knn$pred)
nrow(test_tot)
res <- (exp(model_knn$pred)-1)
Submit<-data.frame(Id= test$Id, SalePrice= res)
write.csv(Submit, "./predictions/submission_knn1.csv", row.names=FALSE)

#Ensemble level 1 - RF
##########################################################################################

train_l2 <- data.frame(cbind(xgb = xgb_pred_t, lasso = lasso_pred_t, SalePrice = train_tot$SalePrice))
test_l2 <- data.frame(cbind(xgb = xgb_pred, lasso = lasso_pred))

control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "RMSE"
fit2 <-train(SalePrice~., data = train_l2, method = "rf", metric=metric, trControl=control)
rf_pred <- predict(fit2, test_l2)
res <- (exp(rf_pred)-1)
submission <- data.frame(Id = test$Id, SalePrice = res)
write.csv(submission, "./predictions/submission_ensembled_7.csv", row.names=FALSE)







#tests
##########################################################################################


predictions <-  data.frame(lasso = (exp(lasso_pred)-1), ridge = (exp(ridge_pred)-1))
ggplot(data = predictions, aes(x = ridge, y = lasso))+
    geom_point()

predictions <-  data.frame(lasso = (exp(lasso_pred)-1), lm = (exp(lm_pred)-1))
ggplot(data = predictions, aes(x = lm, y = lasso))+
    geom_point()

predictions <-  data.frame(lasso = (exp(lasso_pred)-1), xgb = (exp(xgb_pred)-1))
ggplot(data = predictions, aes(x = xgb, y = lasso))+
    geom_point() + xlim(0, 1000000) + ylim(0, 1000000)

predictions <-  data.frame(lasso = (exp(lasso_pred)-1), rf = (exp(rf_pred)-1))
ggplot(data = predictions, aes(x = rf, y = lasso))+
    geom_point() + xlim(0, 1000000) + ylim(0, 1000000)



train_l2 <- data.frame(cbind(xgb = xgb_pred_t, lasso = lasso_pred_t, SalePrice = train_tot$SalePrice))
test_l2 <- data.frame(cbind(xgb = xgb_pred, lasso = lasso_pred))

train_l2 <- train_l2[, !(names(train_l2) %in% c("SalePrice"))]



y_train <- train_tot$SalePrice

#y_test <- train_tot$SalePrice


xgb_train <- xgb.DMatrix(model.matrix(~., data = train_x),
                         label=y_train, missing=NA)



history <- xgb.cv(data = xgb_train, nround=500, nthread = 6, nfold = 10, metrics=list("rmse"),
                  max.depth =6, eta = 0.025, gamma = 0.022, colsample_bytree = 0.25,  objective = "reg:linear")


param<-list(
    objective = "reg:linear",
    eval_metric = "rmse",
    booster = "gbtree",
    max_depth = 6,
    eta = 0.025,
    gamma = 0.022, 
    colsample_bytree = 0.25
    #,
    #min_child_weight=1
)

Training1 <-
    xgb.train(params = param,
              data = xgb_train,
              nrounds = 600,
              watchlist = list(train = xgb_train),
              verbose = TRUE,
              print_every_n = 50,
              nthread = 6)

xgb_test1 <- xgb.DMatrix(model.matrix(~., data = test_l2),
                         missing=NA)

xgb_pred1 <- predict(Training1, newdata=xgb_test1)

control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "RMSE"
fit2 <-train(SalePrice~., data = train_l2, method = "rf", metric=metric, trControl=control)
rf_pred <- predict(fit2, test_l2)
res <- (exp(rf_pred)-1)
submission <- data.frame(Id = test$Id, SalePrice = res)
write.csv(submission, "./predictions/submission_ensembled_7.csv", row.names=FALSE)


train_l1 <- data.frame(cbind(lm = lm_pred_t, xgb = xgb_pred_t, ridge = ridge_pred_t, lasso = lasso_pred_t))
test_l1 <- data.frame(cbind(lm = lm_pred, xgb = xgb_pred, ridge = ridge_pred, lasso = lasso_pred))

y_train <- train_tot$SalePrice

xgb_train <- xgb.DMatrix(model.matrix(~., data = train_l1),
                         label=y_train, missing=NA)

xgb_test <- xgb.DMatrix(model.matrix(~., data = test_l1),
                        missing=NA)

param<-list(
    objective = "reg:linear",
    eval_metric = "rmse",
    booster = "gbtree",
    max_depth = 8,
    eta = 0.123,
    gamma = 0.0385, 
    subsample = 0.734,
    colsample_bytree = 0.512
)

Training_t <-
    xgb.train(params = param,
              data = xgb_train,
              nrounds = 600,
              watchlist = list(train = xgb_train),
              verbose = TRUE,
              print_every_n = 50,
              nthread = 6)



Submit <- predict(Training_t, newdata=xgb_test)
Submit<-data.frame(Id= test$Id, SalePrice= (exp(Submit)-1))
write.csv(Submit, "./predictions/submission_ensembled_2.csv", row.names=FALSE)


test$GrLivArea
boxplot(log(test$GrLivArea+1))

ggplot(data = train, aes(x = (LotArea)^(1/LotArea), y = SalePrice))+
    geom_point()

names(train)

train[train$GrLivArea > 3000 & train$SalePrice > 4e+05,]

ggplot(data = train, aes(x = log(Fireplace+1), y = log(SalePrice+1)))+
    geom_point()

ggplot(data = train, aes(x = as.numeric(factor(train$area)), y = SalePrice))+
    geom_point()


train$area <- cut(train$GrLivArea,unique(quantile(train$GrLivArea)),include.lowest=TRUE)
factor(train$area)
test_int$KitchenQual <- as.numeric(factor(train$area))
test_int$KitchenQual <- as.numeric(test_int$KitchenQual)

ggplot(data = train, aes(x = log(GrLivArea+1), y = (log(GarageArea)-1)))+
    geom_point()

ggplot(data = predictions, aes(x = ridge, y = lasso))+
    geom_point()



head(train[,sapply(train, is.numeric)],10)

train_area <- train[,sapply(names(train), function(x)grepl("Area", x))]
train_area <- sapply(train_area, function(x)log(x+1))
head(train_area)
train_tot[, names(train_area)] <- train_area
hist(train$age)



test_t <- test
test_t$SalePrice <- (exp(ridge_pred)-1)
ggplot(data = train_tot, aes(x = log(LotArea), y = SalePrice))+
    geom_point()+ stat_smooth(method="lm", se=TRUE, fill=NA,
                              formula=y ~ poly(x, 1, raw=TRUE),colour="red")
train_tot$GarageCars

ggplot(data = train_tot, aes(x = Fireplaces, y = SalePrice))+
    geom_point()+ stat_smooth(method="lm", se=TRUE, fill=NA,
                              formula=y ~ poly(x, 3, raw=TRUE),colour="blue")


hist(log(train_tot$LotArea^2))

train_tot$GrLivArea

library(lmSupport)
mod <- lm(log(SalePrice+1) ~ OverallQual, data = train)
summary(mod)


control <- trainControl(method="repeatedcv", number=10, repeats=3)
mod <- train(log(SalePrice+1) ~ LotArea + I(LotArea^2), data = train, method = "lm", trControl = trainControl(method = "cv"))

mod <- lm(log(SalePrice+1) ~ OverallQual+I(OverallQual^(1/2)), data = train)
summary(mod)


fit_t4 <- lm(SalePrice~., data = train_tot1)
summary(fit_t4)
plot(fit_t4)

names(train_tot)

ggplot(data = train_tot, aes(x =GarageQual, y = SalePrice))+
    geom_point()


