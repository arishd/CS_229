
library(glmnet)


####### GLMNET #######

pAlpha <- 0.5 #enet
nLambda <- 10

#create matrix for glmnet
df_train4 = subset(df_train3, select = -c(goals))
train_matrix_model <- as.matrix(df_train4)

df_test4 = subset(df_test3, select = -c(goals))
test_matrix_model <- as.matrix(df_test4)


##MODEL
set.seed(10)

#fit cv glmnet to determine opt lambda value
model_XX_cv <- cv.glmnet(x=train_matrix_model, y=df_train3$goals
                         , family="poisson", alpha=pAlpha, nlambda=nLambda
                         , nfolds=10
                         , type.measure='mae')

#train glmnet across different lambdas for plotting
model_XX_all <- glmnet(x=train_matrix_model, y=df_train3$goals
                       , family="poisson", alpha=pAlpha, nlambda=nLambda)

#train final glmnet
model_XX <- glmnet(x=train_matrix_model, y=df_train3$goals
                   , family="poisson", alpha=pAlpha, lambda=model_XX_cv$lambda.min)

# check results
model_XX$beta
summary(model_XX)

# plot
plot(model_XX_all, xvar="lambda"
     , xlim=c(min(log(model_XX_all$lambda)*1.1) , max(log(model_XX_all$lambda))))
lbs_fun <- function(fit, ...) {
  L <- length(fit$lambda)
  x <- log(fit$lambda[L])
  y <- fit$beta[, L]
  labs <- names(y)
  text(x, y, labels=labs, cex=0.8, ...)
}
lbs_fun(model_XX_all)

preds <- exp(predict.glmnet(model_XX, train_matrix_model))#*(maxs_train[1] - mins_train[1])+ mins_train[1])
glm_train_act_preds <- data.frame(df_train2$goals, preds)#*(maxs_train[1] - mins_train[1])+ mins_train[1]
glm_train_act_preds <- rename(glm_train_act_preds,c("df_train2.goals"="actual","s0"="pred"))
head(glm_train_act_preds)
RMSE(glm_train_act_preds$pred, glm_train_act_preds$actual)


preds <- exp(predict.glmnet(model_XX, test_matrix_model))#*(maxs_train[1] - mins_train[1])+ mins_train[1])
glm_test_act_preds <- data.frame(df_test3$goals, preds)#*(maxs_train[1] - mins_train[1])+ mins_train[1]
glm_test_act_preds <- rename(glm_test_act_preds,c("df_test3.goals"="actual","s0"="pred"))
head(glm_test_act_preds)
RMSE(glm_test_act_preds$pred, glm_test_act_preds$actual)      

a_glm <- glm_test_act_preds$actual
b_glm <- glm_test_act_preds$pred
plot(a_glm, b_glm, xlab = "actual goals", ylab = "predicted goals", xlim = c(0,0.9), ylim = c(0,0.9), main = "Poisson GLM")
abline(lm(a_glm ~ b_glm), col="red")
lines(lowess(glm_test_act_preds$pred,glm_test_act_preds$actual), col="blue")


               
