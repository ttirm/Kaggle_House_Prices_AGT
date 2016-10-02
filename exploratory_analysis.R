setwd("C:/Users/p054009/Documents/GitHub/Kaggle_House_Prices_AGT")

library(ggplot2)
library(caret)

getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}

load("./files/train1.Rda")

colnames(train[,nearZeroVar(train)])
samp_train <- train[1:700,]
# samp_train <- samp_train[, MSZoning == 'A']
samp_train$SalePrice <- log(samp_train$SalePrice + 1)
# Use the original data frame, but put factor() directly in the plot specification
ggplot(data=samp_train, aes(x=YrSold, y=SalePrice, color = YrSold-YearBuilt)) +
    geom_point(aes(size=GrLivArea))+ stat_smooth() + scale_colour_gradientn(colours=c("red", "black"))


nearZeroVar(samp1)

samp1 <- train[train$YrSold == 2007, -nearZeroVar(train)]
colnames(samp1)
h <- head(samp1[order(samp1$SalePrice, decreasing = TRUE),], 10)
h1 <- apply(h,2, getmode)
t <- tail(samp1[order(samp1$SalePrice, decreasing = TRUE),], 10)
t1 <- apply(t,2, getmode)
getmode(h$MSZoning)


ggplot(data=samp_train, aes(x=GrLivArea, y=SalePrice, color = YrSold-YearBuilt)) + 
    geom_point(aes(size=GarageCars)) + scale_colour_gradientn(colours=c("red", "black"))
