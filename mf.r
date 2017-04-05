
# r = q_i*b_u
# bez normalizacji

rmse <- function(t.test, t.pred){
  rmse <- sqrt(sum((t.test$rate - t.pred)^2, na.rm = T)/length(which(is.na(t.pred)==F)))
  return(rmse)
}


mf.predict <- function(t.test, t.model){

  if (is.null(t.test) == T) {
    stop("Test set can not be empty")
  } else {
    t.rate <- rep(NA, dim(t.test)[1])
    tm <- t.test$movie
    tu <- t.test$user
  }
  # jeszcze warunek na klasę modelu
  p_u <- t.model$P
  q_i <- t.model$Q
  rmse <- NA
  cover <- NA
  
  for (i in 1:dim(t.test)[1]){
    t.rate[i] <- q_i[,tm[i]] %*% p_u[tu[i],]
  }
  t.rate <- ifelse(t.rate < 1, 1, t.rate)
  t.rate <-  ifelse(t.rate > 5, 5, t.rate)
  
  rmse <- sqrt(sum((t.test$rate - t.rate)^2, na.rm = T)/length(which(is.na(t.rate)==F)))
  cover <- length(t.rate[is.na(t.rate)==FALSE])/dim(t.test)[1]
  
  return(list(pred = t.rate, rmse = rmse, cover = cover))
  
}


mf.train <- function(t.g = c(0, 0)
                     , t.lambda = c(0, 0)
                     , t.k = 45
                     , t.laps = 30
                     , t.wal = NULL
                     , t.ucz
                     , t.n_user = 0
                     , t.n_movie = 0
                     , t.w = NULL
                     , t.sd = 0.1
                     , t.traincontrol = FALSE
){
  # jakies checki trzeba porobic
  d <- dim(t.ucz)[1]
  
  pred_u <- rep(NA, d)
  p_u <- matrix(NA, t.n_user, t.k)
  q_i <- matrix(NA, t.k, t.n_movie)
  p_u <- apply(p_u, 2, function(x){ x <- runif(t.n_user, -t.sd, t.sd)})
  q_i <- apply(q_i, 2, function(x){ x <- runif(t.k, -t.sd, t.sd)})
  
  l.rmse_u <- rep(NA, t.laps)
  l.rmse_w <- rep(NA, t.laps)
  
  u.rate <- rep(NA, d)
  w.rate <- rep(NA, dim(t.wal)[1])
  
  cover_u <- NA
  cover_w <- NA
  
  if (is.null(t.w) == T) (t.w <- rep(1/d, d))
  
  tm <- t.ucz$movie
  tu <- t.ucz$user
  tr <- t.ucz$rate
  
  # moznaby tu wprowadzic batch?
  for (j in 1:t.laps){
    samp <- sample(1:d)
    for (i in samp){
      pred_u[i] <- p_u[tu[i],] %*% q_i[,tm[i]]
      pred_u[i] <- ifelse(pred_u[i] < 1, 1, pred_u[i])
      pred_u[i] <- ifelse(pred_u[i] > 5, 5, pred_u[i])
      e <- tr[i] - pred_u[i]
      q_i[,tm[i]] <- q_i[,tm[i]] + t.g[1]*d*t.w[i]*(e*p_u[tu[i],] - t.lambda[1]*q_i[,tm[i]])
      p_u[tu[i],] <- p_u[tu[i],] + t.g[2]*d*t.w[i]*(e*q_i[,tm[i]] - t.lambda[2]*p_u[tu[i],])
    }
    
    pred.w <- mf.predict(t.wal, list(Q = q_i, P = p_u))
    l.rmse_w[j] <-  pred.w$rmse
    cover_w <- pred.w$cover

    if (t.traincontrol == TRUE){
      pred.u <- mf.predict(t.ucz, list(Q = q_i, P = p_u))
      l.rmse_u[j] <-  pred.u$rmse
      cover_u <- pred.u$cover
      pred <- pred.u$pred
      kara <- t.lambda[1]*sqrt(sum(q_i[,unique(tm)]^2, na.rm = T)) + t.lambda[2]*sqrt(sum(p_u[unique(tu),]^2, na.rm = T))
      cost <- sum((tr - pred)^2, na.rm = T) + kara
      cat("\n epoch: ", j, "kara: ", kara, "cost: ", cost, "rmse on ucz: ", l.rmse_u[j],"rmse on val: ", l.rmse_w[j], "\n")
    }

    if(j > 4 && abs(l.rmse_w[j] - l.rmse_w[j-1]) < 0.0001) {
      t.laps <- j
      break}
  }
  
  return(list(Q = q_i, P = p_u
              ,laps = t.laps, g = t.g, lambda = t.lambda, k = t.k
              ,rmse_u = l.rmse_u, cover_u = cover_u
              ,rmse_w = l.rmse_w, cover_w = cover_w))
}





time.start <- Sys.time()
mf <- mf.train(t.g = c(0.01, 0.01)
               , t.lambda = c(0.05, 0.05)
               , t.k =45
               , t.laps = 100 # 30
               , t.wal = wal
               , t.ucz = ucz
               , t.n_user = n_user
               , t.n_movie = n_movie
               , t.sd = 0.01
               , t.traincontrol = TRUE)
time.stop <- Sys.time()
(time <- time.stop - time.start)
plot(1:mf$laps, mf$rmse_u[1:mf$laps], type= "l")
lines(1:mf$laps, mf$rmse_w[1:mf$laps], col = "red")

