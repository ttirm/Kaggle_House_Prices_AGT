setwd("C:/Users/p054009/Documents/GitHub/Kaggle_House_Prices_AGT")

library(ggplot2)
library(caret)
library(dplyr)

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


samp1 <- train[train$YrSold == 2007, -nearZeroVar(train)]
colnames(samp1)
h <- head(samp1[order(samp1$SalePrice, decreasing = TRUE),], 10)
h1 <- apply(h,2, getmode)
t <- tail(samp1[order(samp1$SalePrice, decreasing = TRUE),], 10)
t1 <- apply(t,2, getmode)
getmode(h$MSZoning)


ggplot(data=samp_train, aes(x=GrLivArea, y=SalePrice, color = YrSold-YearBuilt)) + 
    geom_point() + scale_colour_gradientn(colours=c("blue", "green"))

names(train)

head(train$YearRemodAdd, 20)
head(train$YearBuilt, 20)

# The "no" remodeled houses follows a strait line 
################################################################################
train$remodeled <- ifelse(train$YearRemodAdd-train$YearBuilt > 0, "yes", "no")
train$age <- train$YrSold-train$YearBuilt
grouped <- group_by(train, remodeled, age)
res <- summarise(grouped, mean=mean(SalePrice))
ggplot(data=res, aes(x=age, y=mean, color = remodeled)) + 
    geom_point() 


head(train$OverallCond)
ggplot(data=train, aes(x=OverallCond, y=OverallQual, color = SalePrice)) + 
    geom_point()+ scale_colour_gradientn(colours=c("blue", "green"))
grouped <- group_by(train, OverallCond, OverallQual)
res <- summarise(grouped, mean=mean(SalePrice))
ggplot(data=res, aes(x=OverallCond, y=mean, color = OverallQual)) + 
    geom_point() 

# Overall qual and cond can be joined
##########################################################
hist(train$OverallCond*train$OverallQual,20)
ggplot(data=train, aes(x=GrLivArea, y=SalePrice, color = log(train$OverallCond*train$OverallQual))) + 
    geom_point() + scale_colour_gradientn(colours=c("blue", "green"))