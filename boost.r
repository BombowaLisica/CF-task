

train.boosting <- function(t.n
                           , t.tresh
                           , t.train_fun
                           , t.train_args
                           , t.test = NULL
                           , t.boostType = "weight"){
  
  boost.ucz <- t.train_args$t.ucz
  d <- dim(boost.ucz)[1]
  t <- dim(t.test)[1]
  weight <- rep(1/d, d)
  weight_new <- rep(NA, d)
  
  proba <- list()
  proba$pred.ucz <- matrix(nrow = d, ncol = t.n)
  proba$w <- matrix(nrow = d, ncol = t.n)
  if (is.null(t.test) == FALSE) {
    proba$pred <- matrix(nrow = t, ncol = t.n)
    pred_boost <- matrix(nrow = t, ncol = t.n)
    rmse_boost <- numeric(t.n)
  }
  b <- numeric(t.n)
  
  for (i in 1:t.n){
    cat("boosting iteration: ", i, "\n")
    
    pred.args <- t.train_args
    if(t.boostType == "weight"){
#      pred.args$t.w <- weight
      inProbe <- 1:d
    }else if (t.boostType == "sample"){
      # losowanie z pstwem
      inProbe <- sample(1:d, d, TRUE, weight)
    }else{
      stop("Please use valid boosting type")
    }
    
    pred.args$t.ucz <- boost.ucz[inProbe,]
    pred.args$t.w <- weight[inProbe]
    
    
    p <- do.call(t.train_fun, pred.args)
    if (is.null(t.test) == FALSE) {
      p.pred <- pred(p, t.test)
      proba$pred[,i] <- p.pred$pred
      # no need for those
      proba$rmse <- c(proba$rmse, p.pred$rmse)
      proba$cover <- c(proba$cover, p.pred$cover)
    }
    p.pred.ucz <- pred(p, boost.ucz)

    
    ### based on https://arxiv.org/ftp/arxiv/papers/1211/1211.2891.pdf
    ae <- abs(p.pred.ucz$pred - boost.ucz$rate)
    ae.vec <- which(ae > t.tresh)
    er <- sum(weight[ae.vec])
    b[i] <- er#(1-er)/er # odst??pstwo od oryginalngo algorytmu

#    if (b[i] < 1) {
#      break
#          weight <- rep(1/d, d)
#    } else {
    # zwi??kszamy wagi ocen z??ych, albo zmniejszamy dobrych
      weight_new[ae.vec] <- weight[ae.vec]/b[i]
      weight_new[-ae.vec] <- weight[-ae.vec]#  * b[i]
      z <- sum(weight_new[ae.vec]) + sum(weight_new[-ae.vec])
      weight <- weight_new/z
#    }
    #####
    

  #   ### orygina??
  #   ae <- abs(p.pred.ucz$pred - boost.ucz$rate)
  #   ae.vec <- which(ae > t.tresh)
  #   er <- sum(weight[ae.vec])
  #   b[i] <- (1-er)/er
  # 
  #     # ten krok generuje regularno????
  #     # if (b[i] < 1) {
  #     # #    break
  #     #         weight <- rep(1/d, d)
  #     #   } else {
  #   # zwi??kszamy wagi ocen z??ych, albo zmniejszamy dobrych
  #         weight_new[ae.vec] <- weight[ae.vec]*b[i]
  #         weight_new[-ae.vec] <- weight[-ae.vec]
  #         z <- sum(weight_new[ae.vec]) + sum(weight_new[-ae.vec])
  #         weight <- weight_new/z
  #     #  }
  #   #####


    proba$pred.ucz[,i] <- p.pred.ucz$pred
    proba$w[,i] <- weight
    proba$model[[i]] <- p

  }
  
  proba$b <- b
  
  if (is.null(t.test) == FALSE) {
    bm <- matrix(b, t, t.n, byrow = T)
    for(i in 2:t.n){
      pred_boost[,i] <- rowSums(log(1/bm[,1:i])*proba$pred[,1:i])/sum(log(1/b[1:i]))
      rmse_boost[i] <- do.call(rmse, list(t.test, pred_boost[,i]))
    }
    proba$rmse_boost <- rmse_boost
    proba$pred_boost <- pred_boost
  }
  
  proba$model_class <- substitute(t.train_fun)
  class(proba) <- "boosting"
  
  return(proba)
}


boost.n <- 30
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
boost.t.test <- wal

tresh = 0.27
boostType = "sample"


start <- Sys.time()  
boosting.model <- train.boosting(t.n = boost.n
                                 , t.tresh = tresh
                                 , t.train_fun = boost.train_fun
                                 , t.train_args = boost.train_args
                                 , t.test = boost.t.test
                                 , t.boostType = boostType)
stop <- Sys.time()
(stop - start)

cat("whole set: ", "\n")
pred_total <- do.call(boost.train_fun, boost.train_args)
pred_total.rmse <- pred(pred_total, boost.t.test)

#graphics.off()
plot(1:length(boosting.model$rmse), boosting.model$rmse, ylim=c(min(boosting.model$rmse) - 0.2, max(boosting.model$rmse) + 0.2)
#     , type= "l"
     , main = paste("Boosting ", boostType, " with treshold ", tresh), xlab = "Number of models", ylab = "rmse")
lines(2:length(boosting.model$rmse), boosting.model$rmse_boost[2:length(boosting.model$rmse)], col = "blue")
lines(1:length(boosting.model$rmse), rep(pred_total.rmse$rmse, length(boosting.model$rmse)), col="red")
legend("topright", c("on boost probe", "on n models", "on one model"), fill = c("black", "blue", "red"))
points(1:length(boosting.model$rmse), boosting.model$b, col = "green")
