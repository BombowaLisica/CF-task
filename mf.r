
# r = q_i*b_u
# without normalization

library(Rcpp)


# predict method

#'
#'pred.mf <- function(t.model, t.test){
#'
#'if (is.null(t.test) == T) {
#'  stop("Test set can not be empty")
#'} else {
#'  t.rate <- rep(NA, dim(t.test)[1])
#'  tm <- t.test$movie
#'  tu <- t.test$user
#'}
#'
#'p_u <- t.model$P
#'q_i <- t.model$Q
#'rmse <- NA
#'cover <- NA
#'
#'for (i in 1:dim(t.test)[1]){
#'  t.rate[i] <- q_i[,tm[i]] %*% p_u[tu[i],]
#'}
#'t.rate <- ifelse(t.rate < 1, 1, t.rate)
#'t.rate <-  ifelse(t.rate > 5, 5, t.rate)
#'
#'rmse <- sqrt(sum((t.test$rate - t.rate)^2, na.rm = T)/length(which(is.na(t.rate)==F)))
#'cover <- length(t.rate[is.na(t.rate)==FALSE])/dim(t.test)[1]
#'
#'prediction <- list(pred = t.rate, rmse = rmse, cover = cover)
#'class(prediction) <- "mf.pred"
#'
#'return(prediction)
#'
#'}


cppFunction(
  'NumericVector predFor(NumericMatrix q_i, NumericMatrix p_u
  , NumericVector tm, NumericVector tu, int n){
  int k = p_u.ncol();
  NumericVector t_rate(n);
  int tmi;
  int tui;
  
  for (int i = 0; i < n;  ++i){
    tmi = tm[i];
    tui = tu[i];
    t_rate(i) =  sum(p_u(tui-1,_) * q_i(_,tmi-1));
    if (t_rate(i) < 1){
      t_rate(i) = 1;
    } else if (t_rate(i) > 5){
      t_rate(i) = 5;
    }
  }
  
  return t_rate;
  }')




pred.mf <- function(t.model, t.test){
  
  if (is.null(t.test) == T) {
    stop("Test set can not be empty")
  } else {
    t.rate <- rep(NA, dim(t.test)[1])
    tm <- t.test$movie
    tu <- t.test$user
  }
  
  p_u <- t.model$P
  q_i <- t.model$Q
  rmse <- NA
  cover <- NA
  
  d <- dim(t.test)[1]
  
  t.rate <- predFor(q_i, p_u, tm, tu, d)
#  t.rate <- ifelse(t.rate < 1, 1, t.rate)
#  t.rate <-  ifelse(t.rate > 5, 5, t.rate)
  
  rmse <- sqrt(sum((t.test$rate - t.rate)^2, na.rm = T)/length(which(is.na(t.rate)==F)))
  cover <- length(t.rate[is.na(t.rate)==FALSE])/dim(t.test)[1]
  
  prediction <- list(pred = t.rate, rmse = rmse, cover = cover)
  class(prediction) <- "mf.pred"
  
  return(prediction)
  
}



#train.mf <- function(t.g = c(0, 0)
#                     , t.lambda = c(0, 0)
#                     , t.k = 45
#                     , t.laps = 30
#                     , t.wal = NULL
#                     , t.ucz
#                     , t.n_user = 0
#                     , t.n_movie = 0
#                     , t.w = NULL
#                     , t.sd = 0.1
#                     , t.traincontrol = FALSE
#){
#  # some checks
#  if (is.null(t.ucz) == T) {
#    stop("Training set can not be empty")
#  }
#  
#  d <- dim(t.ucz)[1]
#  
#  pred_u <- rep(NA, d)
#  p_u <- matrix(NA, t.n_user, t.k)
#  q_i <- matrix(NA, t.k, t.n_movie)
#  p_u <- apply(p_u, 2, function(x){ x <- runif(t.n_user, -t.sd, t.sd)})
#  q_i <- apply(q_i, 2, function(x){ x <- runif(t.k, -t.sd, t.sd)})
#  
#  l.rmse_u <- rep(NA, t.laps)
#  l.rmse_w <- rep(NA, t.laps)
#  
#  u.rate <- rep(NA, d)
#  w.rate <- rep(NA, dim(t.wal)[1])
#  
#  cover_u <- NA
#  cover_w <- NA
#  
#  if (is.null(t.w) == T) (t.w <- rep(1/d, d))
#  
#  tm <- t.ucz$movie
#  tu <- t.ucz$user
#  tr <- t.ucz$rate
#  
#  # batch?
#  for (j in 1:t.laps){
#    samp <- sample(1:d)
#    for (i in samp){
#      pred_u[i] <- p_u[tu[i],] %*% q_i[,tm[i]]
#      pred_u[i] <- ifelse(pred_u[i] < 1, 1, pred_u[i])
#      pred_u[i] <- ifelse(pred_u[i] > 5, 5, pred_u[i])
#      e <- tr[i] - pred_u[i]
#      q_i[,tm[i]] <- q_i[,tm[i]] + t.g[1]*d*t.w[i]*(e*p_u[tu[i],] - t.lambda[1]*q_i[,tm[i]])
#      p_u[tu[i],] <- p_u[tu[i],] + t.g[2]*d*t.w[i]*(e*q_i[,tm[i]] - t.lambda[2]*p_u[tu[i],])
#    }
#    
#    mod <- list(Q = q_i, P = p_u)
#    class(mod) <- "mf"
#    
#    pred_w <- pred(mod, t.wal)
#    l.rmse_w[j] <-  pred_w$rmse
#    cover_w <- pred_w$cover
#    
#    if (t.traincontrol == TRUE){
#      mod <- list(Q = q_i, P = p_u)
#      class(mod) <- "mf"
#      prediction <- pred(mod, t.ucz)
#      l.rmse_u[j] <-  prediction$rmse
#      cover_u <- prediction$cover
#      pred_u <- prediction$pred
#      kara <- t.lambda[1]*sqrt(sum(q_i[,unique(tm)]^2, na.rm = T)) + t.lambda[2]*sqrt(sum(p_u[unique(tu),]^2, na.rm = T))
#      cost <- sum((tr - pred_u)^2, na.rm = T) + kara
#      cat("\n epoch: ", j, "kara: ", kara, "cost: ", cost, "rmse on ucz: ", l.rmse_u[j],"rmse on val: ", l.rmse_w[j], "\n")
#    }
#    
#    if(j > 4 && abs(l.rmse_w[j] - l.rmse_w[j-1]) < 0.0001) {
#      t.laps <- j
#      break}
#  }
#  
#  model <- list(Q = q_i, P = p_u
#                ,laps = t.laps, g = t.g, lambda = t.lambda, k = t.k
#                ,rmse_u = l.rmse_u[1:j], cover_u = cover_u
#                ,rmse_w = l.rmse_w[1:j], cover_w = cover_w)
#  class(model) <- "mf"
#  
#  return(model)
#}




cppFunction(
  'List trainFor(NumericMatrix q_i, NumericMatrix p_u
  , NumericVector tm, NumericVector tu, NumericVector tr
  , NumericVector samp
  , NumericVector g, NumericVector lambda, NumericVector w){

  int k = p_u.ncol();
  int n = samp.size();
  int j;
  NumericVector pred_u(n);
  int tmi;
  int tui;
  int tri;
  double e;
  
  for (int i = 0; i < n;  ++i){
    j = samp[i]-1;
    tmi = tm[j];
    tui = tu[j];
    tri = tr[j];

    pred_u(j) =  sum(p_u(tui-1,_) * q_i(_,tmi-1));
    if (pred_u(j) < 1){
      pred_u(j) = 1;
    } else if (pred_u(j) > 5){
      pred_u(j) = 5;
    }

    e = tri - pred_u(j);
    q_i(_,tmi-1) = q_i(_,tmi-1) + g[0]*n*w[j]*(e*p_u(tui-1,_) - lambda[0]*q_i(_,tmi-1));
    p_u(tui-1,_) = p_u(tui-1,_) + g[1]*n*w[j]*(e*q_i(_,tmi-1) - lambda[1]*p_u(tui-1,_));
  }
  
  List ret;
  ret["P"] = p_u;
  ret["Q"] = q_i;
  return ret;
  }')



train.mf <- function(t.g = c(0, 0)
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
  # some checks
  if (is.null(t.ucz) == T) {
    stop("Training set can not be empty")
  }
  
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
  
  # batch?
  for (j in 1:t.laps){
    samp <- sample(1:d)
    mod <- list(Q =NULL, P = NULL)
    mod <- trainFor(q_i, p_u
                    , tm, tu, tr
                    , samp
                    , t.g, t.lambda, t.w)
    
    #    for (i in samp){
    #      pred_u[i] <- p_u[tu[i],] %*% q_i[,tm[i]]
    #      pred_u[i] <- ifelse(pred_u[i] < 1, 1, pred_u[i])
    #      pred_u[i] <- ifelse(pred_u[i] > 5, 5, pred_u[i])
    #      e <- tr[i] - pred_u[i]
    #      q_i[,tm[i]] <- q_i[,tm[i]] + t.g[1]*d*t.w[i]*(e*p_u[tu[i],] - t.lambda[1]*q_i[,tm[i]])
    #      p_u[tu[i],] <- p_u[tu[i],] + t.g[2]*d*t.w[i]*(e*q_i[,tm[i]] - t.lambda[2]*p_u[tu[i],])
    #    }
    
    #    mod <- list(Q = q_i, P = p_u)
    class(mod) <- "mf"
    
    pred_w <- pred(mod, t.wal)
    l.rmse_w[j] <-  pred_w$rmse
    cover_w <- pred_w$cover
    
    if (t.traincontrol == TRUE){
      mod <- list(Q = q_i, P = p_u)
      class(mod) <- "mf"
      prediction <- pred(mod, t.ucz)
      l.rmse_u[j] <-  prediction$rmse
      cover_u <- prediction$cover
      pred_u <- prediction$pred
      kara <- t.lambda[1]*sqrt(sum(q_i[,unique(tm)]^2, na.rm = T)) + t.lambda[2]*sqrt(sum(p_u[unique(tu),]^2, na.rm = T))
      cost <- sum((tr - pred_u)^2, na.rm = T) + kara
      cat("\n epoch: ", j, "kara: ", kara, "cost: ", cost, "rmse on ucz: ", l.rmse_u[j],"rmse on val: ", l.rmse_w[j], "\n")
    }
    
#    if(j > 4 && abs(l.rmse_w[j] - l.rmse_w[j-1]) < 0.0001) {
#      t.laps <- j
#      break}
  }
  
  model <- list(Q = q_i, P = p_u
                ,laps = t.laps, g = t.g, lambda = t.lambda, k = t.k
                ,rmse_u = l.rmse_u[1:j], cover_u = cover_u
                ,rmse_w = l.rmse_w[1:j], cover_w = cover_w)
  class(model) <- "mf"
  
  return(model)
}




##################################################################
### parameters tuning
##################################################################



time.start <- Sys.time()
mf.model <- train.mf(t.g = c(0.01, 0.01)
               , t.lambda = c(0.05, 0.05)
               , t.k = 45
               , t.laps = 30
               , t.wal = wal
               , t.ucz = ucz
               , t.n_user = n_user
               , t.n_movie = n_movie
               , t.sd = 0.01
               , t.traincontrol = TRUE)
time.stop <- Sys.time()
(time <- time.stop - time.start)

graphics.off()
plot(1:mf.model$laps, mf.model$rmse_u[1:mf.model$laps], type= "l"
     , main = "MF", xlab = "epoch", ylab = "rmse")
lines(1:mf.model$laps, mf.model$rmse_w[1:mf.model$laps], col = "red")
legend("topright", c("training set", "wal set"), fill = c("black", "red"))

mf.pred <- pred(mf.model, test)
(rmse.mf <- rmse(test, mf.pred$pred))
summary(mf.pred$pred); summary(test$rate)
par(mfrow=c(2,1))
hist(test$rate); hist(mf.pred$pred, xlim=c(1,5))



##################################################################
### k choise
##################################################################


proba <- list()
k <- 1
for (i in 15:100){
  cat("k= ", k, " i= ", i, "\n")
  mf.model <- train.mf(t.g = c(0.01, 0.01)
                       , t.lambda = c(0.05, 0.05)
                       , t.k = i
                       , t.laps = 30
                       , t.wal = wal
                       , t.ucz = ucz
                       , t.n_user = n_user
                       , t.n_movie = n_movie
                       , t.sd = 0.01
                       , t.traincontrol = FALSE)
  mf.pred <- pred(mf.model, wal)
  rmse.mf <- rmse(test, mf.pred$pred)
  
  proba$n <- c(proba$n, i)
  proba$rmse <- c(proba$rmse,rmse.mf)
  proba$cover <- c(proba$cover, mf.model$cover)
  proba$pred[[k]] <- mf.pred$pred
  k <- k + 1
}


plot(proba$n, proba$rmse, type = "l"
     , main = "MF", xlab = "k", ylab = "rmse")
