
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(igraph)
library(gbm)


# split response from train
train_labels = df_train3[,'goals']
test_labels = df_test3[,'goals']
vald_labels = df_vald3[,'goals']

df_train4 = subset(df_train3, select = -c(goals))
df_test4 = subset(df_test3, select = -c(goals))
df_vald4 = subset(df_vald3, select = -c(goals))




#######
# XGBOOST
dtrain <- xgb.DMatrix(data = data.matrix(df_train4), label = train_labels)
dvald <- xgb.DMatrix(data = data.matrix(df_vald4), label=vald_labels)

watchlist <- list(train=dtrain, val=dvald)

xgb <- xgb.train(data = dtrain,
               eta = 0.05,
               max_depth = 3,
               watchlist = watchlist,
               nround=130, 
               subsample = 0.5,
               colsample_bytree = 0.5,
               seed = 123,
               eval_metric = "rmse",
               objective = "count:poisson"
)
plot(xgb$evaluation_log$val_rmse)

test_pred <- predict(xgb, data.matrix(df_test4))

test_act_preds <- data.frame(test_labels, test_pred)
test_act_preds <- rename(test_act_preds,c("test_labels"="actual","test_pred"="pred"))
head(test_act_preds)

a<- test_act_preds$actual*7
b<- test_act_preds$pred*7
plot(a, b, xlab = "actual goals", ylab = "predicted goals", xlim = c(0,6), ylim = c(0,6), main = "GBM")
abline(lm(a~b), col="red")
lines(lowess(test_act_preds$pred,test_act_preds$actual), col="blue")

RMSE(test_act_preds$pred, test_act_preds$actual)

xgb.importance(model = xgb)
xgb.ggplot.deepness(model=xgb)


