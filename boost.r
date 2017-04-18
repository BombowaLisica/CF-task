


train.boosting <- function(n
                           , tresh
                           , train_fun
                           , train_args
                           , t.test = NULL){

  boost.ucz <- train_args$t.ucz
  d <- dim(boost.ucz)[1]
  t <- dim(t.test)[1]
  weight <- rep(1/d, d)
  weight_new <- rep(NA, d)

  proba <- list()
  proba$pred.ucz <- matrix(nrow = d, ncol = n)
  proba$w <- matrix(nrow = d, ncol = n)
  if (is.null(t.test) == FALSE) {
    proba$pred <- matrix(nrow = t, ncol = n)
    pred_boost <- matrix(nrow = t, ncol = n)
    rmse_boost <- numeric(n)
  }
  b <- numeric(n)
  
  for (i in 1:n){
    cat("boosting iteration: ", i, "\n")
    
    pred.args <- train_args
    pred.args$t.w <- weight
    
    p <- do.call(train_fun, pred.args)
    if (is.null(t.test) == FALSE) {
      p.pred <- pred(p, t.test)
      proba$pred[,i] <- p.pred$pred
      # no need for those
      proba$rmse <- c(proba$rmse, p.pred$rmse)
      proba$cover <- c(proba$cover, p.pred$cover)
    }
    p.pred.ucz <- pred(p, boost.ucz)

    ae <- abs(p.pred.ucz$pred - boost.ucz$rate)
    ae.vec <- which(ae > tresh)
    er <- sum(weight[ae.vec])
    b[i] <- (1-er)/er
    
    if (b[i] < 1) {
      weight <- rep(1/d, d)
    } else {
      weight_new[ae.vec] <- weight[ae.vec]*b[i]
      weight_new[-ae.vec] <- weight[-ae.vec]
      z <- sum(weight_new[ae.vec]) + sum(weight_new[-ae.vec])
      weight <- weight_new/z 
    }
        
    proba$pred.ucz[,i] <- p.pred.ucz$pred
    
    proba$w[,i] <- weight
    proba$model[[i]] <- p
    
  }
  
  proba$b <- b

  if (is.null(t.test) == FALSE) {
    bm <- matrix(b, t, n, byrow = T)
    for(i in 2:n){
      pred_boost[,i] <- rowSums(log(1/bm[,1:i])*proba$pred[,1:i])/sum(log(1/b[1:i]))
      rmse_boost[i] <- do.call(rmse, list(t.test, pred_boost[,i]))
    }
    proba$rmse_boost <- rmse_boost
    proba$pred_boost <- pred_boost
  }
  
  proba$model_class <- substitute(train_fun)
  class(proba) <- "boosting"
  
  return(proba)
}

  
boost.n <- 10
boost.train_fun <- train.mf
boost.train_args <- list(t.g = c(0.01, 0.01)
                         , t.lambda = c(0.05, 0.05)
                         , t.k = 45
                         , t.laps = 30
                         , t.wal = wal
                         , t.ucz = ucz
                         , t.n_user = n_user
                         , t.n_movie = n_movie
                         , t.sd = 0.01
                         , t.traincontrol = FALSE)
boost.t.test <- test
  
  
  
boosting.model <- train.boosting(n = boost.n
                                 , tresh = 0.8
                                 , train_fun = boost.train_fun
                                 , train_args = boost.train_args
                                 , t.test = boost.t.test)  


cat("whole set: ", "\n")
pred_total <- do.call(boost.train_fun, boost.train_args)
pred_total.rmse <- pred(pred_total, boost.t.test)

graphics.off()
plot(1:boost.n, boosting.model$rmse, ylim=c(min(boosting.model$rmse) - 0.2, max(boosting.model$rmse) + 0.2)
     , type= "l"
     , main = "Boosting", xlab = "Number of models", ylab = "rmse")
lines(2:boost.n, boosting.model$rmse_boost[2:boost.n], col = "blue")
lines(1:boost.n, rep(pred_total.rmse$rmse, boost.n), col="red")
legend("topright", c("on boost probe", "on n models", "on one model"), fill = c("black", "blue", "red"))


