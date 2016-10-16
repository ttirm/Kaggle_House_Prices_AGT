
ind <- sample(1:nrow(train_tot), floor(10*nrow(train_tot)/100),
       replace=FALSE)

train_tot1 <- train_tot[ind,]

train_tot <- train_tot[!(row.names(train_tot) %in% ind),]



train_x <- train_tot[, !(names(train_tot) %in% c("Id", "SalePrice"))]



y_train <- train_tot$SalePrice


xgb_train <- xgb.DMatrix(model.matrix(~., data = train_x),
                         label=y_train, missing=NA)


#All
##############################################################################################################
history <- xgb.cv(data = xgb_train, nround=800, nthread = 6, nfold = 10, metrics=list("rmse"),
                  max.depth =6, eta = 0.021, gamma = 0.025, colsample_bytree = 0.2,  objective = "reg:linear")


param<-list(
    objective = "reg:linear",
    eval_metric = "rmse",
    booster = "gbtree",
    max_depth = 6,
    eta = 0.021,
    gamma = 0.025, 
    colsample_bytree = 0.2
    #,
    #min_child_weight=1
)

Training <-
    xgb.train(params = param,
              data = xgb_train,
              nrounds = 1000,
              watchlist = list(train = xgb_train),
              verbose = TRUE,
              print_every_n = 50,
              nthread = 6)


train_x <- train_tot1[, !(names(train_tot1) %in% c("Id", "SalePrice"))]

xgb_test <- xgb.DMatrix(model.matrix(~., data = train_x),
                        missing=NA)

xgb_pred <- predict(Training, newdata=xgb_test)




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

ridge_pred <- predict(model_ridge,newdata= train_tot1)



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

lasso_pred <- predict(model_lasso,newdata= train_tot1)


train_tot1$xgb_pred <-  xgb_pred
train_tot1$ridge_pred <-  ridge_pred
train_tot1$lasso_pred <-  lasso_pred

train_tot2 <- train_tot1


train_x <- train_tot2[, !(names(train_tot2) %in% c("Id", "SalePrice"))]



y_train <- train_tot2$SalePrice


xgb_train <- xgb.DMatrix(model.matrix(~., data = train_x),
                         label=y_train, missing=NA)


#All
##############################################################################################################
history <- xgb.cv(data = xgb_train, nround=800, nthread = 6, nfold = 10, metrics=list("rmse"),
                  max.depth =6, eta = 0.021, gamma = 0.025, colsample_bytree = 0.2,  objective = "reg:linear")


param<-list(
    objective = "reg:linear",
    eval_metric = "rmse",
    booster = "gbtree",
    max_depth = 6,
    eta = 0.021,
    gamma = 0.025, 
    colsample_bytree = 0.2
    #,
    #min_child_weight=1
)

Training <-
    xgb.train(params = param,
              data = xgb_train,
              nrounds = 1000,
              watchlist = list(train = xgb_train),
              verbose = TRUE,
              print_every_n = 50,
              nthread = 6)


xgb_test <- xgb.DMatrix(model.matrix(~., data = test_tot[,-1]),
                        missing=NA)

xgb_pred <- predict(Training, newdata=xgb_test)
Submit <- data.frame(Id= test$Id, SalePrice= (exp(xgb_pred)-1))
write.csv(Submit, "./predictions/submission_xgb_ensemble.csv", row.names=FALSE)
