

set.seed(1904)

fit2 <-train(SalePrice~., data = train2, method = "rf")

fit_t3 <- train(SalePrice~., data = train_tot, method = "lm", preProcess=c("center","scale","pca"), 
                trControl = trainControl(method = "cv", preProcOptions = list(thresh = 0.95)))
summary(fit_t3)


res <- predict(fit_t3, test_tot)

submission <- data.frame(Id = test$Id, SalePrice = (exp(res)-1))
write.csv(submission, "./predictions/submission_pca_1.csv", row.names=FALSE)


#xgboost
##################################################################################
cv.ctrl <- trainControl(method = "repeatedcv", repeats = 1,number = 3)

xgb.grid <- expand.grid(nrounds = 500,
                        max_depth = seq(6,10),
                        eta = c(0.01,0.3, 1),
                        gamma = c(0.0, 0.2, 1),
                        colsample_bytree = c(0.5,0.8, 1),
                        min_child_weight = 1     #default=1
)

xgb_tune <-train(SalePrice ~.,
                 data=train_tot,
                 method="xgbTree",
                 metric = "RMSE",
                 trControl=cv.ctrl,
                 tuneGrid=xgb.grid
)



y_train <- train_tot$SalePrice

xgb_train <- xgb.DMatrix(model.matrix(~., data = train_tot[,-train_tot$SalePrice]),
                         label=y_train, missing=NA)

xgb_test <- xgb.DMatrix(model.matrix(~., data = test_tot),
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

Training <-
    xgb.train(params = param,
              data = xgb_train,
              nrounds = 600,
              watchlist = list(train = xgb_train),
              verbose = TRUE,
              print_every_n = 50,
              nthread = 6)

Submit<- predict(Training, newdata=xgb_test)
Submit<-data.frame(Id= test$Id, SalePrice= (exp(Submit)-1))
write.csv(Submit, "./predictions/submission_xgboost2.csv", row.names=FALSE)

submission <- data.frame(Id = test$Id, SalePrice = (exp(Submit)-1)*0.5+(exp(res)-1)*0.5)
write.csv(submission, "./predictions/submission_ens_1.csv", row.names=FALSE)


# Ridge Regression
##########################################################################################
CARET.TRAIN.CTRL <- trainControl(method="repeatedcv",
                                 number=5,
                                 repeats=5,
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

preds <- exp(predict(model_ridge,newdata= test_tot)) - 1
Submit<-data.frame(Id= test$Id, SalePrice= preds)
write.csv(Submit, "./predictions/submission_ridge1.csv", row.names=FALSE)

newx = data.matrix(test_tot)
ridge.pred=predict(model_ridge,newx=newx)
preds <- exp(predict(model_lasso,newx=newx)) - 1
Submit<-data.frame(Id= test$Id, SalePrice= preds)
write.csv(Submit, "./predictions/submission_lasso1.csv", row.names=FALSE)


#Lass
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


preds <- exp(predict(model_lasso,newdata= test_tot)) - 1
Submit<-data.frame(Id= test$Id, SalePrice= preds)
write.csv(Submit, "./predictions/submission_lasso1.csv", row.names=FALSE)

newx = data.matrix(test_tot)
preds <- exp(predict(model_lasso,newx=newx)) - 1
Submit<-data.frame(Id= test$Id, SalePrice= preds)
write.csv(Submit, "./predictions/submission_lasso1.csv", row.names=FALSE)
