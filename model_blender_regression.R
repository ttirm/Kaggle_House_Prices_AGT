
library(glmnet)
library(plyr)

set.seed(29)
folds <- createFolds(train_tot$SalePrice, k=10)


train_folds <- function(fold, dataset, xgb){
    testset <- dataset[fold, !(names(dataset) %in% c("Id", "SalePrice")) ]
    y <- dataset[fold, "SalePrice"]
    id <- dataset[fold, "Id"]
    print(id)
    trainset <- dataset[-fold, -1]
    testid <- test_tot$Id
    print(1)
    if (xgb == TRUE){
        train_x <- trainset[, !(names(trainset) %in% c("Id", "SalePrice"))]
        y_train <-  trainset$SalePrice

        xgb_train <- xgb.DMatrix(model.matrix(~., data = train_x),
                                 label=y_train, missing=NA)
        
        param<-list(
            objective = "reg:linear",
            eval_metric = "rmse",
            booster = "gbtree",
            max_depth = 6,
            eta = 0.025,
            gamma = 0.001, 
            colsample_bytree = 0.2,
            min_child_weight=1
        )
        
        fit <-
            xgb.train(params = param,
                      data = xgb_train,
                      nrounds = 1000,
                      watchlist = list(train = xgb_train),
                      verbose = TRUE,
                      print_every_n = 50,
                      nthread = 6)
        
        
        xgb_test_cv <- xgb.DMatrix(model.matrix(~., data = testset),
                                missing=NA)
        yhat <- predict(fit, newdata=xgb_test_cv)        
        score <- RMSE(y,yhat)
        
        xgb_test <- xgb.DMatrix(model.matrix(~., data = test_tot[,-1]),
                                missing=NA)
        test_yhat <- predict(fit, newdata=xgb_test)
    }else{
        print(2)
        set.seed(825)
        fit <- train(SalePrice ~ .,
                     data = trainset,
                     method=met,
                     trControl=CARET.TRAIN.CTRL,
                     tuneGrid=CARET.TUNE.GRID,
                     preProcess= pre,
                     metric="RMSE",
#                      tuneLength=tun,
#                      linout=lin,
#                      trace=tra,
                     maximize=FALSE)
        
        print(3)
        yhat <- predict(fit,newdata = testset,type = "raw")
        print(4)
        test_yhat <- predict(fit,newdata = test_tot[, -1],type = "raw")
        
        score <- RMSE(y,yhat)
        print(score)
    }

    ans <- list(model=fit,
                score=score,
                predictions=data.frame(ID=id,yhat=yhat,y=y),
                test_predictions=data.frame(ID=testid,yhat=test_yhat))
    
    return(ans)
    
}


#Ridge Model
##############################################################################################################
CARET.TUNE.GRID <-  expand.grid(alpha=0, # Ridge regression
                                lambda= seq(1,0,-0.001))

# model specific training parameter
CARET.TRAIN.CTRL <- trainControl(method="repeatedcv",
                                 number=5,
                                 repeats=10,
                                 verboseIter=FALSE)

pre <- NULL
tun <- NULL
lin <- NULL
tra <- NULL
met <- "glmnet"

ridge_set <- lapply(folds, train_folds,train_tot, FALSE)

#lasso Model
##############################################################################################################
CARET.TUNE.GRID <-  expand.grid(alpha=1,  # Lasso regression
                                lambda=c(1,0.1,0.05,0.01,seq(0.009,0.001,-0.001),
                                         0.00075,0.0005,0.0001))

# model specific training parameter
CARET.TRAIN.CTRL <- trainControl(method="repeatedcv",
                                 number=5,
                                 repeats=10,
                                 verboseIter=FALSE)

pre <- NULL
tun <- NULL
lin <- NULL
tra <- NULL
met <- "glmnet"

lasso_set <- lapply(folds, train_folds,train_tot, FALSE)

#lm Model
##############################################################################################################
CARET.TUNE.GRID <-  NULL

# model specific training parameter
CARET.TRAIN.CTRL <- trainControl(method="repeatedcv",
                                 number=5,
                                 repeats=10,
                                 verboseIter=FALSE)

pre <- c("center","scale","pca")
tun <- NULL
lin <- NULL
tra <- NULL
met <- "lm"

lm_set <- lapply(folds, train_folds,train_tot, FALSE)

#gbm Model
##############################################################################################################
# CARET.TUNE.GRID <-  NULL
# 
# # model specific training parameter
# CARET.TRAIN.CTRL <- trainControl(method="repeatedcv",
#                                  number=5,
#                                  repeats=10,
#                                  verboseIter=FALSE)
# 
# 
# pre <- NULL
# tun <- NULL
# lin <- NULL
# tra <- NULL
# met <- "gbm"
# 
# gbm_set <- lapply(folds, train_folds,train_tot, FALSE)

#nn Model
##############################################################################################################
# CARET.TUNE.GRID <-  NULL
# 
# # model specific training parameter
# CARET.TRAIN.CTRL <- trainControl(method="repeatedcv",
#                                  number=5,
#                                  repeats=10,
#                                  verboseIter=FALSE)
# 
# pre <- NULL
# tun <- 7
# lin <- TRUE
# tra <- FALSE
# met <- "nnet"
# 
# nn_set <- lapply(folds, train_folds,train_tot, FALSE)

#rf Model
##############################################################################################################
# CARET.TUNE.GRID <-  NULL
# 
# # model specific training parameter
# CARET.TRAIN.CTRL <- trainControl(method="repeatedcv",
#                                  number=5,
#                                  repeats=10,
#                                  verboseIter=FALSE)
# 
# pre <- NULL
# met <- "rf"
# 
# rf_set <- lapply(folds, train_folds,train_tot, FALSE)



#xgboost Model
##############################################################################################################

xgboost_set <- lapply(folds, train_folds,train_tot, TRUE)



#Level 1 Model - NNET
##############################################################################################################

ridge_yhat <- do.call(c,lapply(ridge_set,function(x){x$predictions$yhat}))
y <- do.call(c,lapply(ridge_set,function(x){x$predictions$y}))
lasso_yhat <- do.call(c,lapply(lasso_set,function(x){x$predictions$yhat}))
lm_yhat <- do.call(c,lapply(lm_set,function(x){x$predictions$yhat}))
# gbm_yhat <- do.call(c,lapply(gbm_set,function(x){x$predictions$yhat}))
xgboost_yhat <- do.call(c,lapply(xgboost_set,function(x){x$predictions$yhat}))

data_l1 <- data.frame(ridge = ridge_yhat, lasso = lasso_yhat, lm = lm_yhat, xgboost = xgboost_yhat, SalePrice = y)

CARET.TUNE.GRID <-  NULL

# model specific training parameter
CARET.TRAIN.CTRL <- trainControl(method="repeatedcv",
                                 number=5,
                                 repeats=10,
                                 verboseIter=FALSE)



set.seed(123)  # for reproducibility
model_nnet <- train(SalePrice ~ .,
                    data = data_l1,
                    method="nnet",
                    metric="RMSE",
                    maximize=FALSE,
                    tuneLength=7,
                    linout=TRUE,
                    trace=FALSE,
                    trControl=CARET.TRAIN.CTRL)

mean(model_nnet$resample$RMSE)

ridge_test <- apply(do.call(cbind,lapply(ridge_set,function(x){x$test_predictions$yhat})),1,mean)
id <- do.call(c,lapply(ridge_set,function(x){x$test_predictions$ID}))
lasso_test <- apply(do.call(cbind,lapply(lasso_set,function(x){x$test_predictions$yhat})),1,mean)
lm_test <- apply(do.call(cbind,lapply(lm_set,function(x){x$test_predictions$yhat})),1,mean)
# gbm_test <- apply(do.call(cbind,lapply(gbm_set,function(x){x$test_predictions$yhat})),1,mean)
xgboost_test <- apply(do.call(cbind,lapply(xgboost_set,function(x){x$test_predictions$yhat})),1,mean)

test_l1 <- data.frame(ridge = ridge_test, lasso = lasso_test, lm = lm_test, xgboost = xgboost_test)

pred <- predict(model_nnet,newdata= test_l1)


res <- (exp(pred)-1)
Submit<-data.frame(Id= test$Id, SalePrice= res)
write.csv(Submit, "./predictions/submission_stack.csv", row.names=FALSE)

