# House Prices Competition #
# September 8 #

setwd('/Users/nickbruno/Documents/fall_2018/SYS6018/House_Prices_Competition/files')
library(tidyverse)
train <- read_csv('train.csv')
test <- read_csv('test.csv')

# Convert categorical variables in the training set # 
train$MSSubClass <- as.factor(train$MSSubClass)
train$MSZoning <- as.factor(train$MSZoning)
train$Street <- as.factor(train$Street)
train$Alley <- as.factor(train$Alley)
train$LotShape <- as.factor(train$LotShape)
train$LandContour <- as.factor(train$LandContour)
train$Utilities <- as.factor(train$Utilities)
train$LotConfig <- as.factor(train$LotConfig)
train$LandSlope <- as.factor(train$LandSlope)
train$Neighborhood <- as.factor(train$Neighborhood)
train$Condition1 <- as.factor(train$Condition1)
train$Condition2 <- as.factor(train$Condition2)
train$BldgType <- as.factor(train$BldgType)
train$HouseStyle <- as.factor(train$HouseStyle)
train$OverallCond <- as.factor(train$OverallCond)
train$YearBuilt <- as.factor(train$YearBuilt)
train$YearRemodAdd <- as.factor(train$YearRemodAdd)
train$RoofStyle <- as.factor(train$RoofStyle)
train$RoofMatl <- as.factor(train$RoofMatl)
train$Exterior1st <- as.factor(train$Exterior1st)
train$Exterior2nd <- as.factor(train$Exterior2nd)
train$MasVnrType <- as.factor(train$MasVnrType)
train$ExterQual <- as.factor(train$ExterQual)
train$ExterCond <- as.factor(train$ExterCond)
train$Foundation <- as.factor(train$Foundation)
train$BsmtQual <- as.factor(train$BsmtQual)
train$BsmtCond <- as.factor(train$BsmtCond)
train$BsmtExposure <- as.factor(train$BsmtExposure)
train$BsmtFinType1 <- as.factor(train$BsmtFinType1)
train$BsmtFinType2 <- as.factor(train$BsmtFinType2)
train$Heating <- as.factor(train$Heating)
train$HeatingQC <- as.factor(train$HeatingQC)
train$CentralAir <- as.factor(train$CentralAir)
train$Electrical <- as.factor(train$Electrical)
train$KitchenQual <- as.factor(train$KitchenQual)
train$Functional <- as.factor(train$Functional)
train$FireplaceQu <- as.factor(train$FireplaceQu)
train$GarageType <- as.factor(train$GarageType)
train$GarageYrBlt <- as.factor(train$GarageYrBlt) # can keep as numeric possibly
train$GarageFinish <- as.factor(train$GarageFinish)
train$GarageQual <- as.factor(train$GarageQual)
train$GarageCond <- as.factor(train$GarageCond)
train$PavedDrive <- as.factor(train$PavedDrive)
train$PoolQC <- as.factor(train$PoolQC)
train$Fence <- as.factor(train$Fence)
train$MiscFeature <- as.factor(train$MiscFeature)
train$MoSold <- as.factor(train$MoSold)
train$YrSold <- as.factor(train$YrSold) # could keep as numeric
train$SaleType <- as.factor(train$SaleType)
train$SaleCondition <- as.factor(train$SaleCondition)



##### POSSIBLE CATEGORICAL VARIABLES #####
train['has_pool'] <- ifelse(train$PoolArea==0,0,1) # each value equal to one means there is a pool at that house
train['has_alley_access'] <- ifelse(train$Alley=='NA',0,1)
train['has_basement'] <- ifelse(train$BsmtQual=='NA',0,1)
train['has_fireplace'] <- ifelse(train$FireplaceQu=='NA',0,1)
train['has_garage'] <- ifelse(train$GarageQual=='NA',0,1)
#####                                ######



# Split training data into two datasets to train #
n <- nrow(train)
seventy_five_percent_of_data <- n*.75
sub <- sample(1:n,size=seventy_five_percent_of_data)
train1 <- train[sub,]
train2 <- train[-sub,]

# Create linear models # 

# Linear Model 1 #
lm1 <- lm(SalePrice ~ MSZoning + LandContour + LotConfig + BldgType + HouseStyle + OverallQual + LotArea + 
   OverallCond + YearRemodAdd + ExterQual + Foundation + BsmtQual + 
     BsmtCond + TotalBsmtSF + HeatingQC + CentralAir + GrLivArea + BedroomAbvGr + KitchenAbvGr + 
     KitchenQual + TotRmsAbvGrd + Functional + GarageArea + WoodDeckSF + PoolArea +
     MoSold + YrSold + SaleType + SaleCondition + Neighborhood, data=train1)

summary(lm1) # n = 776
lm1_valid <- predict(lm1, newdata=train2)
mse1 <- mean((lm1_valid$SalePrice)^2)
  # NA

# Linear Model 2 #
lm2 <- lm(SalePrice ~ MSZoning + LandContour + LotConfig + BldgType + HouseStyle + OverallQual + LotArea + 
           BsmtQual + GrLivArea + BedroomAbvGr + KitchenAbvGr + KitchenQual + TotRmsAbvGrd + Functional + 
            WoodDeckSF + Neighborhood, data=train1)
summary(lm2) # n = 1001
lm2_valid <- predict(lm2, newdata=train2)
mse2 <- mean((lm2_valid-train2$SalePrice)^2, na.rm = T)
  # [1] 815547072

# Linear Model 3 #
lm3 <- lm(SalePrice ~ LandContour + LotConfig + BldgType + HouseStyle + OverallQual + LotArea + 
            BsmtQual + GrLivArea + BedroomAbvGr + KitchenAbvGr + KitchenQual + TotRmsAbvGrd + Functional + 
            WoodDeckSF, data=train1)
summary(lm3) # n = 1029
lm3_valid <- predict(lm3, newdata=train2)
mse3 <- mean((lm3_valid-train2$SalePrice)^2, na.rm = T)
  # [1] 965418930

# Linear Model 4 #
lm4 <- lm(SalePrice ~ MSSubClass + LotArea + BldgType + HouseStyle + YearRemodAdd +  Exterior2nd +
            Foundation +  CentralAir + LowQualFinSF + KitchenQual + GarageCars + PavedDrive +  
            YrSold, data=train1)
summary(lm4) # n = 978
lm4_valid <- predict(lm4, newdata=train2)
mse4 <- mean((lm4_valid-train2$SalePrice)^2, na.rm = T)
  # [1] 1973115060

# Linear Model 5 #
lm5 <- lm(SalePrice ~ MSSubClass + LotArea + YearRemodAdd + Foundation + KitchenQual + GarageCars, data=train2)
summary(lm5) # n = 342
lm5_valid <- predict(lm5, newdata=train2)
mse5 <- mean((lm5_valid-train2$SalePrice)^2, na.rm = T)
  # [1] 1190627841

##### Things to discuss in meeting #####

# 1. First, we should turn in a linear model.

# 2. How should we handle 'NA' values? In some cases, NA is a valid option and doesn't mean that the 
# data is omitted. Should we create a seperate column that separates the 'NA' from the other values. For
# example, if the house has a pool or not (PoolQC)

# 3. We need to do a knn regression.

# 4. Shoud we create a github repo?

# 5. Should look into some other non-parametric methods and decide which ones we want to hard code.

# 6. When do we want to meet next and what should we have accomplished by then?

# 7. Which variables should we make categorical and not? Year the house was built?


