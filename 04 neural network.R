
library(neuralnet)


custom <- function(x) {log(1+exp(x))}


n <- names(df_train2)
f <- as.formula(paste("goals ~", paste(n[!n %in% "goals"], collapse = " + ")))
nn <- neuralnet(f, data=df_train3, hidden=c(2), stepmax = 5e+05, act.fct = custom,lifesign = 'minimal',lifesign.step = 2e+04, linear.output=T)
# neuralnet(f, data=df_train3, hidden=c(1,1), stepmax = 5e+05, act.fct = custom,lifesign = 'minimal',lifesign.step = 2e+04, linear.output=T) 1.118591
# neuralnet(f, data=df_train3, hidden=c(2), stepmax = 5e+05, act.fct = custom,lifesign = 'minimal',lifesign.step = 2e+04, linear.output=T) = 1.141
# neuralnet(f, data=df_train3, hidden=c(2,1), stepmax = 5e+05,lifesign = 'minimal',lifesign.step = 2e+04, linear.output=T) = 1.16
plot(nn)

pr.nn <- compute(nn,df_test3[,2:26])
pr.nn_ <- pr.nn$net.result#(maxs_train[1] - mins_train[1])+ mins_train[1]
test.r <- (df_test3$goals)#(maxs_train[1] - mins_train[1]) + mins_train[1]
nn_test_act_preds <- data.frame(test.r, pr.nn_)
nn_test_act_preds <- rename(nn_test_act_preds,c("test.r"="actual","pr.nn_"="pred"))
head(nn_test_act_preds)

a_nn <- nn_test_act_preds$actual*7
b_nn <- nn_test_act_preds$pred*7
plot(a_nn, b_nn, xlab = "actual goals", ylab = "predicted goals", xlim = c(0,6), ylim = c(0,6), main = "Neural Network")
abline(lm(a_nn ~ b_nn), col="red")
lines(lowess(nn_test_act_preds$pred,nn_test_act_preds$actual), col="blue")

RMSE(nn_test_act_preds$pred, nn_test_act_preds$actual)



tr_pr.nn <- compute(nn,df_train3[,2:26])
tr_pr.nn_ <- tr_pr.nn$net.result#(maxs_train[1] - mins_train[1])+ mins_train[1]
train.r <- (df_train3$goals)#(maxs_train[1] - mins_train[1]) + mins_train[1]
tr_nn_test_act_preds <- data.frame(train.r, tr_pr.nn_)
tr_nn_test_act_preds <- rename(tr_nn_test_act_preds,c("train.r"="actual","tr_pr.nn_"="pred"))
head(tr_nn_test_act_preds)
RMSE(tr_nn_test_act_preds$pred, tr_nn_test_act_preds$actual)

