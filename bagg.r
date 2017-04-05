

bagging <- function(n
                    , train.fun
                    , train.args
                    , pred.fun
                    , t.test
                    , rmse.fun){
  
  bag.ucz <- train.args$t.ucz
  d <- dim(bag.ucz)[1]
  
  proba <- list()
  proba$pred <- matrix(nrow = dim(t.test)[1], ncol = n)
  pred.bagg <- matrix(nrow = dim(t.test)[1], ncol = n)
  rmse.bagg <- numeric(n)
  
  for (i in 1:n){
    cat("bagging iteration: ", i, "\n")
    inProbe <- sample(1:d, replace = T)
    ucz.rand <- bag.ucz[inProbe,]
    
    pred.args <- train.args
    pred.args["t.ucz"] <- NULL
    pred.args$t.ucz <- ucz.rand
    
    time.start <- Sys.time()
    p <- do.call(train.fun, pred.args)
    p.pred <- pred.fun(t.test, p)
    time.stop <- Sys.time()
    time <- time.stop - time.start
    
    proba$n <- c(proba$n, i)
    proba$pred[,i] <- p.pred$pred
    proba$time <- c(proba$time, time)
    proba$rmse <- c(proba$rmse, p.pred$rmse)
    proba$cover <- c(proba$cover, p.pred$cover)
    
  }
  
  for(i in 2:n){
    pred.bagg[,i] <- rowMeans(proba$pred[,1:i])
    rmse.bagg[i] <- do.call(rmse.fun, list(t.test, pred.bagg[,i]))
  }
  
#  pred.bagg <- rowMeans(proba$pred)
#  rmse.bagg <- do.call(rmse.fun, list(t.test, pred.bagg))
  
  cat("whole set: ", "\n")
  pred.total <- do.call(train.fun, train.args)
  pred.total.rmse <- pred.fun(t.test, pred.total)
  
  plot(1:n, proba$rmse, ylim=c(min(proba$rmse) - 0.2, max(proba$rmse) + 0.2))
#  lines(1:n, rep(rmse.bagg, n), col = "blue")
  lines(2:n, rmse.bagg[2:n], col = "blue")
  lines(1:n, rep(pred.total.rmse$rmse, n), col="red")
  
  proba$rmse.bagg <- rmse.bagg
  proba$pred.bagg <- pred.bagg
  
  return(proba)
}


b <- bagging(n = 5
             , train.fun = mf.train
             , train.args = list(t.g = c(0.01, 0.01)
                                 , t.lambda = c(0.05, 0.05)
                                 , t.k =45
                                 , t.laps = 30
                                 , t.wal = wal
                                 , t.ucz = ucz
                                 , t.n_user = n_user
                                 , t.n_movie = n_movie
                                 , t.sd = 0.01
                                 , t.traincontrol = TRUE)
             , pred.fun = mf.predict
             , t.test = test
             , rmse.fun = rmse)





