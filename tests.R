setwd("C:/Users/tiago_000/Documents/GitHub/Kaggle_House_Prices_AGT")


train <- read.csv("./data/train.csv", stringsAsFactors = FALSE)
test <- read.csv("./data/test.csv", stringsAsFactors = FALSE)


t2 <- train[c(31, 89, 411, 463, 496, 524,589, 633, 711, 969, 971, 1299, 1325, 1433),]

lasso_pred_out <- predict(model_lasso,newdata= t2)
t2$pred <- exp(lasso_pred_out)-1

head(train[, "Alley"], 100)
train_tot$LotConfig

data_l1

train_x <- data_l1[, !(names(data_l1) %in% c("Id", "SalePrice"))]



y_train <- data_l1$SalePrice

#y_test <- train_tot$SalePrice


xgb_train <- xgb.DMatrix(model.matrix(~., data = train_x),
                         label=y_train, missing=NA)


#All
##############################################################################################################
history <- xgb.cv(data = xgb_train, nround=800, nthread = 6, nfold = 10, metrics=list("rmse"),
                  max.depth =4, eta = 0.05, gamma = 0.01, colsample_bytree = 0.5,  objective = "reg:linear")


param<-list(
    objective = "reg:linear",
    eval_metric = "rmse",
    booster = "gbtree",
    max_depth = 4,
    eta = 0.05,
    gamma = 0.01, 
    colsample_bytree = 0.5,
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



test_l1 <- data.frame(ridge = ridge_test, lasso = lasso_test, lm = lm_test, xgboost = xgboost_test)

xgb_test <- xgb.DMatrix(model.matrix(~., data = test_l1),
                        missing=NA)

pred <- predict(Training,newdata= xgb_test)


res <- (exp(pred)-1)
Submit<-data.frame(Id= test$Id, SalePrice= res)
write.csv(Submit, "./predictions/submission_stack1.csv", row.names=FALSE)


xgb_pred <- predict(Training, newdata=xgb_test)
Submit <- data.frame(Id= test$Id, SalePrice= (exp(xgb_pred)-1))
write.csv(Submit, "./predictions/submission_xgboost2.csv", row.names=FALSE)