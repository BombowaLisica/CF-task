

train.bagging <- function(n
                          , train.fun
                          , train.args
                          , t.test = NULL){
  
  bag.ucz <- train.args$t.ucz
  d <- dim(bag.ucz)[1]
  
  proba <- list()
  if (is.null(t.test) == FALSE) {
    proba$pred <- matrix(nrow = dim(t.test)[1], ncol = n)
    pred_bagg <- matrix(nrow = dim(t.test)[1], ncol = n)
    rmse_bagg <- numeric(n)
  }
  
  for (i in 1:n){
    cat("bagging iteration: ", i, "\n")
    inProbe <- sample(1:d, replace = T)
    ucz.rand <- bag.ucz[inProbe,]
    
    pred_args <- train.args
    pred_args["t.ucz"] <- NULL
    pred_args$t.ucz <- ucz.rand
    
    p <- do.call(train.fun, pred_args)

    if (is.null(t.test) == FALSE) {
      p.pred <- pred(p, t.test)
      proba$pred[,i] <- p.pred$pred
      # no need for those
      proba$rmse <- c(proba$rmse, p.pred$rmse)
      proba$cover <- c(proba$cover, p.pred$cover)
    }
    
    proba$model[[i]] <- p 
  }
  
  if (is.null(t.test) == FALSE) {
    for(i in 2:n){
      pred_bagg[,i] <- rowMeans(proba$pred[,1:i])
      rmse_bagg[i] <- do.call(rmse, list(t.test, pred_bagg[,i]))
    }
    proba$rmse.bagg <- rmse_bagg
    proba$pred.bagg <- pred_bagg
  }
  
  proba$model_class <- substitute(train.fun)
  class(proba) <- "bagging"
  
  return(proba)
}




pred.bagging <- function(t.model, t.test = NULL){
  
  if (is.null(t.test) == T) {
    if (is.null(t.model$pred) == F){
      t.rate <- rowMeans(t.model$pred)
      rmse <- t.model$rmse.bagg[max(t.model$n)]
      cover <- t.model$cover[max(t.model$n)]
    } else{
      stop("No test set or prediction from model provided")
    }
  } else {
    t.rate.mat <- matrix(nrow = dim(t.test)[1], ncol = length(t.model$model))
    for (i in 1:length(t.model$model)){
      model <- t.model$model[[i]]
      t.rate.mat[,i] <- pred(model, t.test)$pred
    }
    t.rate <- rowMeans(t.rate.mat)
    
    rmse <- sqrt(sum((t.test$rate - t.rate)^2, na.rm = T)/length(which(is.na(t.rate)==F)))
    cover <- length(t.rate[is.na(t.rate)==FALSE])/dim(t.test)[1]
  }
  
  
  prediction <- list(pred = t.rate, rmse = rmse, cover = cover)
  class(prediction) <- "bagging.pred"
  
  return(prediction)
  
}


bagg.n <- 3
bagg.train.fun <- train.mf
bagg.train.args <- list(t.g = c(0.01, 0.01)
                    , t.lambda = c(0.05, 0.05)
                    , t.k = 45
                    , t.laps = 30
                    , t.wal = wal
                    , t.ucz = ucz
                    , t.n_user = n_user
                    , t.n_movie = n_movie
                    , t.sd = 0.01
                    , t.traincontrol = FALSE)
bagg.t.test <- test



bagging.model <- train.bagging(n = bagg.n
                               , train.fun = bagg.train.fun
                               , train.args = bagg.train.args
                               , t.test = bagg.t.test)

cat("whole set: ", "\n")
pred_total <- do.call(bagg.train.fun, bagg.train.args)
pred_total.rmse <- pred(pred_total, bagg.t.test)


plot(1:bagg.n, bagging.model$rmse, ylim=c(min(bagging.model$rmse) - 0.2, max(bagging.model$rmse) + 0.2)
     , type= "l"
     , main = "Bagging", xlab = "Number of models", ylab = "rmse")
lines(2:bagg.n, bagging.model$rmse.bagg[2:bagg.n], col = "blue")
lines(1:bagg.n, rep(pred_total.rmse$rmse, bagg.n), col="red")

