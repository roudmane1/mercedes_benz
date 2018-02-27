
# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(data.table)
library(Matrix)
library(xgboost)
library(caret)
library(dplyr)

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory
# Load CSV files
cat("Read data")
d_train <- fread('data/train.csv', sep=",", na.strings = "NA")
d_test  <- fread("data/test.csv" , sep=",", na.strings = "NA")

print(sapply(d_train,class))

#fonction pour le calcul du taux d'erreur
err_rate <-  function(D,prediction){
    #matrice de confusion
    mc <- table(D$chiffre,prediction)
    #taux d'erreur
    #1 -  somme(individus classés correctement) / somme totale individus
      err <-1-sum(diag(mc))/sum(mc)
      print(paste("Error rate :",round(100*err,2),"%"))
    }
#rpart library
library(rpart)
m.tree <-rpart(y ~ ., data = d_train)
print(m.tree)

#variable importance
print(m.tree$variable.importance)
#X0      X314      X261      X127        X2      X313      X136      X232      X263 
#338082.06 281461.26 273561.14 271237.57 146694.45 144680.69  43648.17  43648.17  43648.17 
#X29       X54       X76      X118      X119      X311      X115      X275 
#43648.17  43648.17  43648.17  37553.06  37553.06  32244.80  31567.16  24000.

#onpeut voir les varibles les plus importantes pour la prediction

#calcule des ecart type
sapply(d_train[,11:378],sd)#comme les 8 premiere variable sont des factors alors nous allons les enlever 

#prédiction
y.tree <-  predict(m.tree, newdata = d_test, type = "class")

model_reglin=lm(y~.,data = d_train[,11:378])
summary(model_reglin)
attach(d_train)
X0<-ifelse(X0)

y_pred=predict(model_reglin, d_test[,10:377], se.fit = FALSE, scale = NULL, df = Inf,
        interval = c("none", "confidence", "prediction"),
        level = 0.95, type = c("response", "terms"))
err=d_train[,2]-y_pred
mean(err)
##############################################
##############################################
data = rbind(df_train,df_test,fill=T)

features = colnames(data)

for (f in features){
  if( (class(data[[f]]) == "character") || (class(data[[f]]) == "factor"))
  {
    levels = unique(data[[f]])
    data[[f]] = factor(data[[f]], level = levels)
  }
}

# one-hot-encoding features
data = as.data.frame(data)
ohe_feats = c('X0', 'X1', 'X2', 'X3', 'X4', 'X5','X6', 'X8')
dummies = dummyVars(~ X0 + X1 + X2 + X3 + X4 + X5 + X6 + X8 , data = data)
df_all_ohe <- as.data.frame(predict(dummies, newdata = data))
df_all_combined <- cbind(data[,-c(which(colnames(data) %in% ohe_feats))],df_all_ohe)

data = as.data.table(df_all_combined)

train = data[data$ID %in% df_train$ID,]
y_train <- train[!is.na(y),y]
train = train[,y:=NULL]
train = train[,ID:=NULL]
#train_sparse <- as(data.matrix(train),"dgCMatrix")
train_sparse <- data.matrix(train)

test = data[data$ID  %in% df_test$ID,]
test_ids <- test[,ID]
test[,y:=NULL]
test[,ID:=NULL]
#test_sparse <- as(data.matrix(test),"dgCMatrix")
test_sparse <- data.matrix(test)

dtrain <- xgb.DMatrix(data=train_sparse, label=y_train)
dtest <- xgb.DMatrix(data=test_sparse);

gc()

# Params for xgboost
param <- list(booster = "gbtree",
              eval_metric = "rmse", 
              objective = "reg:linear",
              eta = .1,
              gamma = 1,
              max_depth = 4,
              min_child_weight = 1,
              subsample = .7,
              colsample_bytree = .7)


#cvFoldsList <- createFolds(1:nrow(train), k = 5)

#xgb_cv <- xgb.cv(data = dtrain,
#                 params = param,
#                 nrounds = 1500,
#                 maximize = FALSE,
#                 prediction = TRUE,
#                 folds = cvFoldsList,
#                 print.every.n = 5,
#                 early.stop.round = 50); gc()

#rounds <- which.min(xgb_cv$dt[, test.rmse.mean])
rounds = 52
mpreds = data.table(id=test_ids)

for(random.seed.num in 1:10) {
  print(paste("[", random.seed.num , "] training xgboost begin ",sep=""," : ",Sys.time()))
  set.seed(random.seed.num)
  xgb_model <- xgb.train(data = dtrain,
                         params = param,
                         watchlist = list(train = dtrain),
                         nrounds = rounds,
                         verbose = 1,
                         print.every.n = 5)
  
  vpreds = predict(xgb_model,dtest) 
  mpreds = cbind(mpreds, vpreds)    
  colnames(mpreds)[random.seed.num+1] = paste("pred_seed_", random.seed.num, sep="")
}

mpreds_2 = mpreds[, id:= NULL]
mpreds_2 = mpreds_2[, y := rowMeans(.SD)]

submission = data.table(ID=test_ids, y=mpreds_2$y)
#write.table(submission, "mercedes_xgboost.csv", sep=",", dec=".", quote=FALSE, row.names=FALSE)

