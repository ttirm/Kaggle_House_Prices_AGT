setwd("C:/Users/p054009/Documents/GitHub/Kaggle_House_Prices_AGT")

library(ggplot2)
library(caret)
library(dplyr)
library(corrplot)

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
train$age <- ifelse(train$YearRemodAdd-train$YearBuilt > 0, train$YrSold-train$YearRemodAdd, train$YrSold-train$YearBuilt)
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




# Price evolution in different neighborhoods
##########################################################
ggplot(data = train, aes(factor(Neighborhood), SalePrice, fill = factor(YrSold))) + 
    geom_boxplot() + 
    theme(axis.text.x = element_text(angle = 90, hjust =1)) + 
    xlab('Neighborhoods')


# Garage evaluation
##########################################################
train_garage <- train[,grep("Garage",names(train))]
train_garage$SalePrice <- train$SalePrice
train_garage$PavedDrive <- train$PavedDrive
head(train_garage)
ggplot(data=train_garage, aes(x= GarageType, y = SalePrice, fill = factor(GarageFinish))) + 
    geom_bar(position='dodge',stat = "summary", fun.y = "mean") 


ggplot(data=train_garage, aes(x= GarageQual)) + 
    geom_bar(stat = "count")
names(train_garage)


train_garage$GarageQual <- factor(train_garage$GarageQual)
levels(train_garage$GarageQual) <- c(5,2,4,0,1,3)
train_garage$GarageCond <- factor(train_garage$GarageCond)
levels(train_garage$GarageCond)  <- c(5,2,4,0,1,3)
head(train_garage)
train_garage$GarageFinish <- factor(train_garage$GarageFinish)
levels(train_garage$GarageFinish) <- c(3,0,2,1)
train_garage$PavedDrive <- factor(train_garage$PavedDrive)
levels(train_garage$PavedDrive) <- c(0,1,2)
train_garage$SalePrice <- log(train$SalePrice+1)

dummies = model.matrix(~factor(train_garage$GarageType))
head(train_garage)
head(dummies)
class(dummies)
dum <- as.data.frame(dummies)
dum <- dum[,2:7]
names(dum) <- c("Attchd", "Basment", "BuiltIn", "CarPort", "Detchd", "None")
head(dum)
train_garage <- cbind(train_garage[,2:ncol(train_garage)], dum)
head(train_garage)
train_garage <- sapply(train_garage, as.numeric)
garage.pca <- prcomp(train_garage[,-7],
                 center = TRUE,
                 scale. = TRUE)

plot(garage.pca, type = "l")

train_garage2 <- garage.pca$x
train_garage2 <- as.data.frame(train_garage2)
train_garage2$SalePrice <- train_garage[,7]

fit_gar2 <-train(SalePrice~PC1+PC2+PC3+PC4, data = train_garage2, method = "lm")

train_garage2$GrLivArea <- train$GrLivArea
train_garage2$age <- train$age

fit_gar2 <-train(SalePrice~PC1+PC2+
                     GrLivArea+age+(train$OverallCond*train$OverallQual), 
                 data = train_garage2, method = "lm")

corrplot(cor(train_garage2), method="square")

# Basement evaluation
##########################################################
names(train)

train_basement <- train[,grep("Bsmt",names(train))]
head(train_basement)
train_basement$BsmtQual <- factor(train_basement$BsmtQual)
levels(train_basement$BsmtQual) <- c(5,2,4,0,3,1)
train_basement$BsmtExposure <- factor(train_basement$BsmtExposure)
levels(train_basement$BsmtExposure) <- c(3,4,2,1,0)
train_basement$BsmtFinType1 <- factor(train_basement$BsmtFinType1)
levels(train_basement$BsmtFinType1) <- c(5,4,6,2,0,3,1)

train_basement <- sapply(train_basement, as.numeric)
basement.pca <- prcomp(train_basement,
                     center = TRUE,
                     scale. = TRUE)

plot(basement.pca, type = "l")
train_basement2 <- basement.pca$x
train_basement3 <- as.data.frame(train_basement2[,1:2])
head(train_basement3)
names(train_basement3) <- c("bsmt1", "bsmt2")
train3 <- cbind(train_garage2, train_basement3)

fit_join3 <-train(SalePrice~PC1+PC2+
                     GrLivArea+age+(train$OverallCond*train$OverallQual)+bsmt1+bsmt2, 
                 data = train3, method = "lm")

corrplot(cor(train3), method="square")

names(train)
grep("Bsmt|Garage|Id|age|SalePrice",names(train))
train_r <- train[, -(grep("Bsmt|Garage|Id|age|SalePrice|PavedDrive",names(train)))]
train_r$MSSubClass <- train$MSSubClass
train_r$SalePrice <- train$SalePrice
names(train_r)


head(train$MSSubClass)

ggplot(data=train_r, aes(x=MSSubClass, y=SalePrice, )) + 
    geom_point() 

head(train_r$MSZoning)
ggplot(data=train_r, aes(x= MSZoning)) + 
    geom_bar(stat = "count") 

zones <- c("MSSubClass", "MSZoning", "Neighborhood", "Condition1", "BldgType", "HouseStyle")
train_zones <- train[, zones]

train_z1 <- train_zones
for(i in 1:ncol(train_zones)){
    dummies = model.matrix(~factor(train_zones[,i]))
    dummies <- as.data.frame(dummies[,2:ncol(dummies)])
    nam <- sapply(names(dummies), function(x)unlist(strsplit(x, ")"))[2])
    names(dummies) <- nam
    train_z1 <- cbind(train_z1, dummies)
}
train_z1 <- train_z1[, 7:ncol(train_z1)]
corrplot(cor(train_z1), method="square")
zones.pca <- prcomp(train_z1,
                     center = TRUE,
                     scale. = TRUE)

plot(zones.pca, type = "l")
train_z1 <- zones.pca$x
train_z1 <- as.data.frame(train_z1[,1:4])
head(train_z1)
names(train_z1) <- c("zone1", "zone2", "zone3", "zone4")

train4 <- cbind(train3, train_z1)
train4$Overallqual1 <- train$OverallCond+train$OverallQual

fit_join4 <- train(SalePrice~PC1+PC2+
                      GrLivArea+age+Overallqual1+bsmt1+bsmt2+
                      zone1 +zone2 + zone3, 
                  data = train4, method = "lm")


summary(fit_join4)
plot(fit_join4$finalModel)

corrplot(cor(train4), method="square")

names(train_r)
names(train_r[, !(names(train_r) %in% zones)])
exterior <- c("RoofStyle", "Exterior1st", "Exterior2nd", "MasVnrType", 
              "MasVnrArea","ExterQual", "ExterCond", "Foundation")
head(train_r[, exterior])
train_exterior <- train_r[, exterior]

train_Ext <- train_exterior[, c(1,2,3,4,8)]
for(i in 1:ncol(train_Ext)){
    dummies = model.matrix(~factor(train_Ext[,i]))
    dummies <- as.data.frame(dummies[,2:ncol(dummies)])
    nam <- sapply(names(dummies), function(x)unlist(strsplit(x, ")"))[2])
    names(dummies) <- nam
    train_exterior <- cbind(train_exterior, dummies)
}
train_exterior <- train_exterior[, -c(1,2,3,4,8)]

levels(factor(train$ExterQual))
train_exterior$ExterQual <- factor(train_exterior$ExterQual)
levels(train_exterior$ExterQual) <- c(4,1,3,2,0)
train_exterior$ExterCond <- factor(train_exterior$ExterCond)
levels(train_exterior$ExterCond) <- c(4,1,3,0,2)
train_exterior <- sapply(train_exterior, as.numeric)
head(train_exterior)


corrplot(cor(train_exterior), method="square")
exterior.pca <- prcomp(train_exterior,
                    center = TRUE,
                    scale. = TRUE)

plot(exterior.pca, type = "l")
train_exterior <- exterior.pca$x
train_exterior <- as.data.frame(train_exterior[,1:3])
names(train_exterior) <- c("exterior1", "exterior2", "exterior3")

train5 <- cbind(train4, train_exterior)

fit_join5 <- train(SalePrice~PC1+
                       GrLivArea+age+Overallqual1+bsmt1+bsmt2+
                       zone1 +zone2 + zone3+ exterior1 + exterior2, 
                   data = train5, method = "lm")

summary(fit_join5)


train_r <- train_r[, !(names(train_r) %in% exterior)]
train_r <- train_r[, !(names(train_r) %in% zones)]
names(train_r)
aux <- c("OverallQual", "OverallCond", "YearBuilt", 
         "YearRemodAdd", "remodeled", "SalePrice")
train_r <- train_r[,!(names(train_r) %in% aux)]
names(train_r)

head(train$LotConfig)
train_lot <- train_r[,c("LotArea","LotShape","LotConfig")]
train_lot1 <- train_lot[, c(2,3)]
for(i in 1:ncol(train_lot1)){
    dummies = model.matrix(~factor(train_lot1[,i]))
    dummies <- as.data.frame(dummies[,2:ncol(dummies)])
    nam <- sapply(names(dummies), function(x)unlist(strsplit(x, ")"))[2])
    names(dummies) <- nam
    train_lot <- cbind(train_lot, dummies)
}
head(train_lot)
train_lot <- train_lot[, -c(2,3)]
corrplot(cor(train_lot), method="square")
lot.pca <- prcomp(train_lot,
                       center = TRUE,
                       scale. = TRUE)

plot(lot.pca, type = "l")
train_lot <- lot.pca$x

train_lot <- as.data.frame(train_lot[,1:2])
names(train_lot) <- c("lot1", "lot2")

train6 <- cbind(train5, train_lot)

fit_join6 <- train(SalePrice~PC1+
                       GrLivArea+age+Overallqual1+bsmt1+bsmt2+
                       zone1 +zone2 + zone3+ exterior1 + exterior2 + lot1 + lot2, 
                   data = train6, method = "lm")

summary(fit_join6)

train_r <- train_r[,!(names(train_r) %in% c("LotArea","LotShape","LotConfig"))]
names(train_r)
head(train_r$Fireplaces)
train$Fireplace <- factor(train$FireplaceQu)
levels(train$Fireplace) <- c(5,2,4,0,1,3)
train$Fireplace <- as.numeric(train$Fireplace)*as.numeric(train$Fireplaces)

train7 <- cbind(train6, Fireplace= train$Fireplace)

train_r <- train_r[,!(names(train_r) %in% c("Fireplaces","FireplaceQu"))]
names(train_r)

train(SalePrice~PC1+GrLivArea+age+Overallqual1+bsmt1+bsmt2+
                       zone1 +zone2 + zone3+ exterior1 + exterior2 + lot1 + lot2 + Fireplace, 
                   data = train7, method = "lm")

head(train7)

facilities <- c("HeatingQC", "CentralAir", "Electrical")

train_fac <- train_r[, facilities]
head(train_fac)
train_fac$HeatingQC <- factor(train_fac$HeatingQC)
levels(train_fac$HeatingQC) <- c(4,1,3,0,2)
train_fac$CentralAir <- factor(train_fac$CentralAir)
levels(train_fac$CentralAir) <- c(0,1)



train_fac$SalesPrice <- train$SalePrice
train_fac1 <- train_fac[,3:4]
grouped <- group_by(train_fac1, Electrical)
res <- summarise(grouped, mean=mean(SalesPrice))
ggplot(data=res, aes(x=Electrical, y=mean)) + 
    geom_point() 

train_fac$Electrical <- factor(train_fac$Electrical)
levels(train_fac$Electrical) <- c(3,2,1,0,4)


sale <- c("SaleType", "SaleCondition")
train_sale <- train_r[,sale]
train_sale$SalePrice <- train$SalePrice
train_sale1 <- train_sale[,c(2,3)]
grouped <- group_by(train_sale1, SaleCondition)
res <- summarise(grouped, mean=mean(SalePrice))
ggplot(data=res, aes(x=SaleCondition, y=mean)) + 
    geom_point() 



head(train1)
head(train_r)
date <- c("MoSold", "YrSold")
train_date <- train_r[,date]
train_date$yearm <- as.yearmon(paste(train_date$YrSold, train_date$MoSold, sep = "-"))
train_date$SalePrice <- train$SalePrice
train_date <- train_date[,3:4]
grouped <- group_by(train_date, yearm)
res <- summarise(grouped, mean=mean(SalePrice))
x <- zoo(res$mean, res$yearm)
plot(x)


train_date <- train_r[,date]
train_date$yearm <- as.yearmon(paste(train_date$YrSold, train_date$MoSold, sep = "-"))
train_date$SalePrice <- train$SalePrice

ggplot(data=train_date, aes(x= MoSold, y = SalePrice)) + 
    geom_bar(position='dodge',stat = "summary", fun.y = "length") 



train_exterior$SalePrice <- train$SalePrice
ggplot(data=train_exterior, aes(x= ExterQual-ExterCond, y = SalePrice)) + 
    geom_point() 
