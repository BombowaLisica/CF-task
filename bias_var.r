
# bias-variance tradeoff for original method and bagging

n <- 2
samp_bagg.n <- 25
samp_train.fun <- train.mf
samp_train.args <- list(t.g = c(0.01, 0.01)
                  , t.lambda = c(0.05, 0.05)
                  , t.k = 45
                  , t.laps = 30
                  , t.wal = wal
                  , t.ucz = ucz
                  , t.n_user = n_user
                  , t.n_movie = n_movie
                  , t.sd = 0.01
                  , t.traincontrol = FALSE)




samp_ucz <- samp_train.args$t.ucz
d <- dim(samp_ucz)[1]

pred_model_org <- matrix(nrow = dim(test)[1], ncol = n)
pred_model_bagg <- matrix(nrow = dim(test)[1], ncol = n)

for (i in 1:n){
  
  inProbe <- sample(1:dim(samp_ucz)[1], replace = T)
  ucz.rand <- samp_ucz[inProbe,]
  
  pred.args <- samp_train.args
  pred.args["t.ucz"] <- NULL
  pred.args$t.ucz <- ucz.rand
  
  
  model_org <- do.call(samp_train.fun, samp_train.args)
  pred_model_org[,i] <- pred(model_org, test)$pred
  
  model_bagg <- do.call(train.bagging, list(samp_bagg.n, samp_train.fun, samp_train.args, test))
  pred_model_bagg[,i] <- pred(model_bagg, test)$pred
  
}


model_org_var <- head(apply(pred_model_org, 1, var))
model_org_bias <- head((test$rate - apply(pred_model_org, 1, mean))^2)

model_bagg_var <- head(apply(pred_model_org, 1, var))
model_bagg_bias <- head((test$rate - apply(pred_model_org, 1, mean))^2)
