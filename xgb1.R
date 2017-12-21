library(data.table)
library(dplyr)
library(xgboost)
library(caret)
library(stringr)
library(glmnet)
library(mice)
library(Boruta)

train <- fread("train.csv", stringsAsFactors = TRUE)
test  <- fread("test.csv", stringsAsFactors = TRUE)

combine <- bind_rows(train, test)

#check for NA 

sapply(combine, function(x) sum(is.na(x)))
# map quality data into numeric data 

Street.map <- c("Grvl"=0, "Pave"=1)
combine$Street <- Street.map[as.character(combine$Street)]
rm(Street.map)

Lotshape.map <-c("IR3"=0, "IR2"=1, "IR1"=2, "Reg"=3 )
combine$LotShape <- Lotshape.map[as.character(combine$LotShape)]
rm(Lotshape.map)

Landcountur.map <- c("Low"=0, "Bnk"=0, "HLS"=1, "Lvl"=2 )
combine$LandContour <- Landcountur.map[as.character(combine$LandContour)]
rm(Landcountur.map)

combine$Utilities <- as.factor(combine$Utilities)
Utilities.map <- c( "NoSeWa"=0,  "AllPub"=1)
combine$Utilities <- Utilities.map[as.character(combine$Utilities)]
rm(Utilities.map)

Landslope.map <- c("Sev"=0, "Mod"=1, "Gtl"=2)
combine$LandSlope <- Landslope.map[as.character(combine$LandSlope)]
rm(Landslope.map)

BldgType.map <- c("TwnhsE"=0, "Twnhs"=1, "Duplex"=2, "2fmCon"=3, "1Fam"=4)
combine$BldgType <- BldgType.map[as.character(combine$BldgType)]
rm(BldgType.map)

combine$OverallQual <- (combine$OverallQual)-1
combine$OverallCond <- (combine$OverallCond)-1

ExterQual.map <- c("Fa"=0, "TA"=1, "Gd"=2, "Ex"=3)
combine$ExterQual <- ExterQual.map[as.character(combine$ExterQual)]
rm(ExterQual.map)

ExterCond.map <- c("Po"=0, "Fa"=1, "TA"=2, "Gd"=3, "Ex"=4)
combine$ExterCond <- ExterCond.map[as.character(combine$ExterCond)]
rm(ExterCond.map)

BsmtQual.map <- c("Fa"=1, "TA"=2, "Gd"=3, "Ex"=4)
combine$BsmtQual <- BsmtQual.map[as.character(combine$BsmtQual)]
rm(BsmtQual.map)
combine$BsmtQual[is.na(combine$BsmtQual)] <- 0

BsmtCond.map <- c("Fa"=1, "TA"=2, "Gd"=3, "Ex"=4)
combine$BsmtCond <- BsmtCond.map[as.character(combine$BsmtCond)]
rm(BsmtCond.map)
combine$BsmtCond[is.na(combine$BsmtCond)] <- 0

BsmtExposure.map <- c("No"=1, "Mn"=2, "Av"=3, "Gd"=4)
combine$BsmtExposure <- BsmtExposure.map[as.character(combine$BsmtExposure)]
rm(BsmtExposure.map)
combine$BsmtExposure[is.na(combine$BsmtExposure)] <- 0 

BsmtFinType1.map <- c("Unf"=1, "LwQ"=2, "Rec"=3, "BLQ"=4, "ALQ"=5, "GLQ"=6)
combine$BsmtFinType1 <- BsmtFinType1.map[as.character(combine$BsmtFinType1)]
rm(BsmtFinType1.map)
combine$BsmtFinType1[is.na(combine$BsmtFinType1)] <- 0

BsmtFinType2.map <- c("Unf"=1, "LwQ"=2, "Rec"=3, "BLQ"=4, "ALQ"=5, "GLQ"=6)
combine$BsmtFinType2 <- BsmtFinType2.map[as.character(combine$BsmtFinType2)]
rm(BsmtFinType2.map)
combine$BsmtFinType2[is.na(combine$BsmtFinType2)] <- 0

HeatingQC.map <- c("Po"=0, "Fa"=1, "TA"=2, "Gd"=3, "Ex"=4)
combine$HeatingQC <- HeatingQC.map[as.character(combine$HeatingQC)]
rm(HeatingQC.map)

CentralAir.map <- c("N"=0, "Y"=1)
combine$CentralAir <- CentralAir.map[as.character(combine$CentralAir)]
rm(CentralAir.map)

combine$Electrical <- as.factor(combine$Electrical)
Electrical.map <- c("Mix"=0, "FuseP"=1, "FuseF"=2, "FuseA"=3, "SBrkr"=4)
combine$Electrical <- Electrical.map[as.character(combine$Electrical)]
rm(Electrical.map)

Kitchen.map <- c( "Fa"=0, "TA"=1, "Gd"=2, "Ex"=3)
combine$KitchenQual <- Kitchen.map[as.character(combine$KitchenQual)]
rm(Kitchen.map)

Functional.map <- c("Sev"=0, "Maj2"=1, "Maj1"=2, "Min2"=3, "Min1"=4, "Typ"=5)
combine$Functional <- Functional.map[as.character(combine$Functional)]
rm(Functional.map)

FireplaceQu.map <- c("Po"=1, "Fa"=2, "TA"=3, "Gd"=4, "Ex"=5)
combine$FireplaceQu <- FireplaceQu.map[as.character(combine$FireplaceQu)]
rm(FireplaceQu.map)
combine$FireplaceQu[is.na(combine$FireplaceQu)] <- 0


GarageFinish.map <- c("Unf"=1, "RFn"=2, "Fin"=3)
combine$GarageFinish <- GarageFinish.map[as.character(combine$GarageFinish)]
rm(GarageFinish.map)
combine$GarageFinish[is.na(combine$GarageFinish)] <- 0

combine$GarageQual <- as.factor(combine$GarageQual)
GarageQual.map <- c("Po"=1, "Fa"=2, "TA"=3, "Gd"=4, "Ex"=5)
combine$GarageQual <- GarageQual.map[as.character(combine$GarageQual)]
rm(GarageQual.map)
combine$GarageQual[is.na(combine$GarageQual)] <- 0 

GarageCond.map <- c("Po"=1, "Fa"=2, "TA"=3, "Gd"=4, "Ex"=5)
combine$GarageCond <- GarageCond.map[as.character(combine$GarageCond)]
rm(GarageCond.map)
combine$GarageCond[is.na(combine$GarageCond)] <- 0 

PavedDrive.map <- c("N"=0, "P"=1, "Y"=2)
combine$PavedDrive <- PavedDrive.map[as.character(combine$PavedDrive)]
rm(PavedDrive.map)

combine$PoolQC <- as.factor(combine$PoolQC)
PoolQC.map <- c("Fa"=1, "Gd"=2, "Ex"=3)
combine$PoolQC <- PoolQC.map[as.character(combine$PoolQC)]
rm(PoolQC.map)
combine$PoolQC[is.na(combine$PoolQC)] <- 0

Fence.map <- c("GdPrv"=4, "GdWo"=2,  "MnPrv"=3, "MnWw"=1 )
combine$Fence <- Fence.map[as.character(combine$Fence)]
rm(Fence.map)
combine$Fence[is.na(combine$Fence)] <- 0 
#maybe have to check
nghr.map <- c('MeadowV' = 0, 'IDOTRR' = 1, 'Sawyer' = 2, 'BrDale' = 3, 'OldTown'= 4,
              'Edwards' = 5, 'BrkSide' = 6, 'Blueste' = 7, 'SWISU' = 8, 'NAmes' = 9,
              'NPkVill' = 10, 'Mitchel' = 11,'SawyerW' = 12, 'Gilbert' = 13, 'NWAmes'=14,
              'Blmngtn' = 15, 'CollgCr' = 16, 'ClearCr' = 17,'Crawfor' =18, 'Veenker'=19,
              'Somerst' = 20, 'Timber' = 21, 'StoneBr' = 22, 'NoRidge'= 23,'NridgHt' =24)
combine$Neighborhood <- nghr.map[as.character(combine$Neighborhood)]
rm(nghr.map)

#drop MSZOning and alley
combine$MSZoning <- NULL
combine$Alley <- NULL

combine$MiscFeature <- NULL
#convert factor and character to numeric
#combine$Alley <- as.numeric(combine$Alley)-1
combine$LotConfig <- as.numeric(combine$LotConfig)-1
combine$Neighborhood <- as.numeric(combine$Neighborhood)
combine$Condition1 <- as.numeric(combine$Condition1)-1
combine$Condition2 <- as.factor(combine$Condition2)
combine$Condition2 <- as.numeric(combine$Condition2)-1
combine$HouseStyle <- as.factor(combine$HouseStyle)
combine$HouseStyle <- as.numeric(combine$HouseStyle)-1
combine$RoofStyle <- as.numeric(combine$RoofStyle)-1
combine$RoofMatl <- as.factor(combine$RoofMatl)
combine$RoofMatl <- as.numeric(combine$RoofMatl)-1
combine$Exterior1st <- as.factor(combine$Exterior1st)
combine$Exterior1st <- as.numeric(combine$Exterior1st)-1
combine$Exterior2nd <- as.factor(combine$Exterior2nd)
combine$Exterior2nd <- as.numeric(combine$Exterior2nd)-1
combine$MasVnrType <- as.numeric(combine$MasVnrType)-1
combine$Foundation <- as.numeric(combine$Foundation)-1
combine$Heating <- as.factor(combine$Heating)
combine$Heating <- as.numeric(combine$Heating)-1
combine$GarageType <- as.numeric(combine$GarageType)-1
#combine$MiscFeature <- as.factor(combine$MiscFeature)
#combine$MiscFeature <- as.numeric(combine$MiscFeature)-1
combine$SaleType <- as.numeric(combine$SaleType)-1
combine$SaleCondition <- as.factor(combine$SaleCondition)
combine$SaleCondition <- as.numeric(combine$SaleCondition)-1

#FEATURE ENGINEERING

combine$Age <- combine$YrSold - combine$YearBuilt
combine$YrSinceRemodel <- combine$YrSold - combine$YearRemodAdd
combine$Total_Floor <- combine$`1stFlrSF` + combine$`2ndFlrSF`
combine$All_living_Area <-  combine$`1stFlrSF` + combine$`2ndFlrSF` + combine$GrLivArea

combine$TotalSF <- combine$TotalBsmtSF + combine$`1stFlrSF` + combine$`2ndFlrSF`
combine$AllSF <- combine$TotalBsmtSF + combine$GrLivArea
combine$OverallGrd <- combine$OverallQual * combine$OverallCond
combine$ExterGrade <- combine$ExterQual * combine$ExterCond
combine$GarageScore <- combine$GarageArea * combine$GarageQual
combine$TotalBath <- combine$BsmtFullBath + combine$BsmtHalfBath + combine$FullBath + combine$HalfBath
########## PCA feature
combine$built <- (combine$YearBuilt) * (combine$BsmtFinSF1)
combine$RemodFlrSF <- (combine$YearRemodAdd) * (combine$`2ndFlrSF`)
combine$Basement <- (combine$TotalBsmtSF) * (combine$BsmtUnfSF)

combine$Ground<- (combine$GrLivArea) * (combine$YearBuilt)

combine$Bath <- (combine$FullBath) * (combine$YearBuilt)

combine$AboveGround <- (combine$TotRmsAbvGrd) * (combine$BedroomAbvGr)

combine$livingArea <- (combine$TotalBsmtSF) * (combine$GrLivArea) 

combine$years <- (combine$YearBuilt) * (combine$YearRemodAdd)

# combine$Lot <- (combine$`1stFlrSF`) * (train$LotFrontage)





#imputation
combine_train <- combine[1:1460,]
combine_test  <- combine[1461:2919,]
#check for NA , 
sapply(combine_train, function(x) sum(is.na(x)))
mice_imp <- mice(combine_train, method = "cart", m=5)
densityplot(mice_imp, ~LotFrontage)
imputed <- complete(mice_imp)

combine_train$LotFrontage <- imputed$LotFrontage
#combine_train$Alley<- imputed$Alley
#combine_train$Utilities <- imputed$Utilities
#combine_train$Exterior1st <- imputed$Exterior1st
#combine_train$Exterior2nd <- imputed$Exterior2nd
combine_train$MasVnrType <- imputed$MasVnrType
combine_train$MasVnrArea <- imputed$MasVnrArea
#combine_train$BsmtFinSF1 <- imputed$BsmtFinSF1
#combine_train$BsmtFinSF2 <- imputed$BsmtFinSF2
#combine_train$BsmtUnfSF <- imputed$BsmtUnfSF
#combine_train$TotalBsmtSF <- imputed$TotalBsmtSF
combine_train$Electrical <- imputed$Electrical
#combine_train$BsmtFullBath <- imputed$BsmtFullBath
#combine_train$BsmtHalfBath <- imputed$BsmtHalfBath
#combine_train$KitchenQual <- imputed$KitchenQual
combine_train$Functional <- imputed$Functional
combine_train$GarageType <- imputed$GarageType
combine_train$GarageYrBlt <- imputed$GarageYrBlt
#combine_train$GarageCars <- imputed$GarageCars
#combine_train$GarageArea <- imputed$GarageArea
#combine_train$MiscFeature <- imputed$MiscFeature
#combine_train$SaleType <- imputed$SaleType

combine <- bind_rows(combine_train, combine_test)






#######################


combine2 <- combine




# delete near zero predictors IT IMPROVED LB
nzv.data <- nearZeroVar(combine2, saveMetrics = TRUE)

# take any of the near-zero-variance perdictors
drop.cols <- rownames(nzv.data)[nzv.data$nzv == TRUE]
combine2 <- as.data.frame(combine2)
combine2 <- combine2[,!names(combine2) %in% drop.cols]
combine2 <- setDT(combine2)

#paste('The dataframe now has', dim(df)[1], 'rows and', dim(df)[2], 'columns')

#feature selection it didn't improve
#boruta_trainST <- combine2[1:1460]
#boruta.train <- Boruta(SalePrice~. , data = boruta_trainST, doTrace = 2, maxRuns= 100)
#finalvars = getSelectedAttributes(boruta.train, withTentative = F)
#plot(boruta.train)
#boruta subset

#combine2         <-    as.data.frame(combine2)            
#combine2 <-  combine2[, names(combine2) %in% finalvars]
#combine3 <- setDT(combine2)
#sale <- combine %>%  select(SalePrice) 
#combine2 <- bind_cols(combine3, sale)

#extract saleprice and append to last column find a more elegant way to do it
sale <- combine %>%  select(SalePrice) 
combine2$SalePrice <- NULL
combine2 <- bind_cols(combine2, sale)

#transform SalePrice log
combine2$SalePrice <- log(combine2$SalePrice)
#combine2$Age <- log(combine2$Age+1)

#combine2$Total_Floor <- log(combine2$Total_Floor+1)
#combine2$All_living_Area <-  log(combine2$All_living_Area+1)

#combine2$TotalSF <- log(combine2$TotalSF+1)
#combine2$AllSF <- log(combine2$AllSF+1)

#combine2$TotalBsmtSF <- log(combine2$TotalBsmtSF+1)
#combine2$LotArea <- log(combine2$LotArea+1)

#combine2$TotalBath <- log(combine2$TotalBath +1)
######################################
#combine2$LotFrontage <-log(combine2$LotFrontage+1)
#combine2$LotArea <- log(combine2$LotArea+1)
#combine2$MasVnrArea <- log(combine2$MasVnrArea+1)
#combine2$`1stFlrSF` <- log(combine2$`1stFlrSF` +1)
#combine2$`2ndFlrSF` <- log(combine2$`2ndFlrSF` +1)
#combine2$GrLivArea <- log(combine2$GrLivArea +1)

#feature elimination

#eliminate outliers
out <-c(1299, 1183, 524, 692, 899, 804, 1170, 1047, 441, 689) #outliers trovati 
combine2 <- combine2[-out]    #10 outliers 


#split again
train_2 <- combine2[1:1450,]  #1460
test_2  <- combine2[1451:2909,] #1461:2919
#generate train label 

train.label <- train_2$SalePrice
test.label <- test_2$SalePrice
#convert dataset to matrix
train_2<- as.matrix(train_2[,2:76])
test_2<- as.matrix(test_2[,2:76])


dtrain <- xgb.DMatrix(data = train_2, label = train.label)
dtest  <- xgb.DMatrix(data = test_2, label = test.label)

# View the number of rows and features of each set
dim(dtrain)
dim(dtest)

set.seed(1234)
# Set our hyperparameters
param <- list(objective   = "reg:linear",
              eval_metric = "rmse",
              max_depth   = 8, #10
              subsample   = 0.399,
              eta         = 0.0107,
              gammma      = 0,  #1
              colsample_bytree = 0.47,
              min_child_weight = 4)



cvFolds <- createFolds(combine2$SalePrice[!is.na(combine2$SalePrice)], k=5, list=TRUE, returnTrain=FALSE)

xgb_cv <- xgb.cv(data = dtrain,
                 params = param,
                 nrounds = 3000,
                 maximize = FALSE,
                 prediction = TRUE,
                 stratified = TRUE,
                 folds = cvFolds,
                 print_every_n = 10,
                 early_stopping_round = 50)

#bayesopt 956
best_iter<- xgb_cv$best_iteration

# Pass in our hyperparameteres and train the model 
system.time(xgb <- xgboost(params  = param,
                           data    = dtrain,
                           label   = train.label, 
                           nrounds = 956,
                           print_every_n = 100,
                           verbose = 1))

system.time(xgb2 <- xgboost(params  = param,
                            data    = dtrain,
                            label   = train.label, 
                            nrounds = best_iter,
                            print_every_n = 100,
                            seed=1235,
                            verbose = 1))

system.time(xgb3 <- xgboost(params  = param,
                            data    = dtrain,
                            label   = train.label, 
                            nrounds = best_iter,
                            print_every_n = 100,
                            seed=1236,
                            verbose = 1))
####################################################################



#################################################





##################################################
# Get the feature real names
names <- dimnames(dtrain)[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model=xgb)[0:20] # View top 20 most important features

# Plot
xgb.plot.importance(importance_matrix)


# Prediction on test and train sets
pred_xgboost_test <- predict(xgb, dtest)
pred_xgboost_train <- predict(xgb, dtrain)

pred_xgboost_test2 <- predict(xgb2, dtest)
pred_xgboost_train2 <- predict(xgb2, dtrain)

pred_xgboost_test3 <- predict(xgb3, dtest)
pred_xgboost_train3 <- predict(xgb3, dtrain)
#transform prediction to exp
pred_xgboost_test <-exp(pred_xgboost_test )
pred_xgboost_test2 <-exp(pred_xgboost_test2 ) 
pred_xgboost_test3 <-exp(pred_xgboost_test3 ) 

#ensamble 3 models
pred_ensamble = (pred_xgboost_test+pred_xgboost_test2+pred_xgboost_test3)/3

submit <- data.frame(PassengerId = combine[1461:2919,c("Id")], SalePrice = pred_ensamble)
write.csv(submit, file = "my_first_xgb3.csv", row.names = FALSE)

combine2[1453:2911,]
submit <- data.frame(PassengerId = combine2[1453:2911,c("Id")], SalePrice = pred_ensamble)




# for numeric feature with excessive skewness, perform log transformation
# first get data type for each feature
feature_classes <- sapply(names(combine2),function(x){class(combine2[[x]])})
numeric_feats <-names(feature_classes[feature_classes != "character" & feature_classes != "factor"])

# determine skew for each numeric feature
skewed_feats <- sapply(numeric_feats,function(x){skewness(combine2[[x]],na.rm=TRUE)})

# keep only features that exceed a threshold for skewness
skewed_feats <- skewed_feats[skewed_feats > 0.75]

# transform excessively skewed features with log(x + 1)
for(x in names(skewed_feats)) {
  combine[[x]] <- log(combine[[x]] + 1)
}
#####################################
combine4 <- combine[1:1460,]
combine4 <- combine2[, 2:68]
mod <- lm(SalePrice ~ ., data=combine4 )

outliers <- car::outlierTest(mod)
outliers <- c(88,462,523,588,632,968,1298,1324) #copiato 
out <-c(1299, 1183, 524, 692, 899, 804, 1170, 1047, 441, 689) #outliers trovati 
# Generate scatterplots with Pearson correlations
pairs.panels(combine4)
# Use function `vif` to calculate variance inflation factors for each variable
# in the model
vif(mod)
combine4$Age <- NULL
combine4$YrSinceRemodel <- NULL
combine4$Total_Floor <- NULL
combine4$All_living_Area <- NULL
combine4$TotalSF <- NULL
combine4$AllSF <- NULL
combine4$TotalBath <- NULL

#the linearly dependent variables
ld.vars <- attributes(alias(mod)$Complete)$dimnames[[1]]

#remove the linearly dependent variables variables
formula.new <- as.formula(
  paste(
    paste(deparse(formula), collapse=""), 
    paste(ld.vars, collapse="-"),
    sep="-"
  )
)

#run model again
fit.new <-lm(formula.new)
vif(fit.new)

combine4 <- combine[-out]
plot(combine4$GrLivArea, combine4$SalePrice)



