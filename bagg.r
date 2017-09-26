train.bagging <- function(n
                          , train.fun
                          , train.args
                          , t.test = NULL
                          , by_user = FALSE # opcja dla RBM - kiedy losowa?? trzeba user??w, nie wiersze
){
  
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
    
    # tutaj dla RBM trzeba bedzie losowa?? userow nie obserwacje!
    # bo jak zrealizowa?? powielone obserwacje? bez powtarzania losowac?
    # ewentualnie losowac ze zwracaniem, ale potem usunac duplikaty
    # (bedzie jak z losowaniem userow, tylko nie ze wszystkimi rekordami)
    # trzeba zabezpieczyc jeszcze b????d w train.rbm
    # Error in rowSums(Reduce("+", batch)) : 
    # 'x' must be an array of at least two dimensions
    if (by_user == TRUE){
      n_user <- unique(bag.ucz$user)
      inProbe <- sample(n_user, replace = T)
      inProbeTab <- bag.ucz[bag.ucz$user %in% sort(inProbe),]
      rn <- rownames(inProbeTab)
      t1 <- data.frame(table(inProbe))
      t2 <- data.frame(table(inProbeTab$user))
      ile <- rep(t1$Freq, t2$Freq)
      ucz.rand <- bag.ucz[rep(rn, ile), ]
      # tu b??dzie r????na liczba obserwacji, ale ta sama user??w
      
      new_user <- sapply(strsplit(row.names(ucz.rand), "[.]"), "[", c(2))
      new_user[is.na(new_user)] <- 0
      new_user <- as.numeric(new_user)
      ucz.rand <- cbind(ucz.rand, new_user)
      ucz.rand <- mutate(ucz.rand, user_new = 1000*new_user + user)
      ucz.rand$user <- ucz.rand$user_new
      ucz.rand <- ucz.rand
      
    } else {
      inProbe <- sample(1:d, replace = T)
      ucz.rand <- bag.ucz[inProbe,]
    }
    
    pred_args <- train.args
    pred_args["t.ucz"] <- NULL
    pred_args$t.ucz <- ucz.rand
    
    p <- do.call(train.fun, pred_args)
    
    if (is.null(t.test) == FALSE) {
      # ??eby rbm mial predykcje ze wszystkich znanych ocen
      # trzeba sprawdzi??, jak zadzia??a z mf - mo??e bedize potrzebny if na class
      p["t.ucz"] <- NULL
      p$t.ucz <- bag.ucz
      
      p.pred <- pred(p, t.test)
      proba$pred[,i] <- p.pred$pred
      # no need for those
      proba$rmse <- c(proba$rmse, p.pred$rmse)
      proba$cover <- c(proba$cover, p.pred$cover)
    }
    
    # mo??e si?? okaza??, ??e nie da si?? tego zbiera??, bo b??dzie za ci????kie
    # w sumie nie jest nawet chyba potrzebne
    #    proba$model[[i]] <- p 
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


bagg.n <- 50
bagg.train.fun <- train.mf_comp
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


start <- Sys.time()
bagging.model <- train.bagging(n = bagg.n
                               , train.fun = bagg.train.fun
                               , train.args = bagg.train.args
                               , t.test = bagg.t.test)
stop <- Sys.time()
(stop - start)
cat("whole set: ", "\n")
pred_total <- do.call(bagg.train.fun, bagg.train.args)
pred_total.rmse <- pred(pred_total, bagg.t.test)


plot(1:bagg.n, bagging.model$rmse, ylim=c(min(bagging.model$rmse) - 0.2, max(bagging.model$rmse) + 0.2)
     , type= "l"
     , main = "Bagging", xlab = "Number of models", ylab = "rmse")
lines(2:bagg.n, bagging.model$rmse.bagg[2:bagg.n], col = "blue")
lines(1:bagg.n, rep(pred_total.rmse$rmse, bagg.n), col="red")



library(compiler)
train.mf_comp <- cmpfun(train.mf)

##### zwykly bagging

### mf zwykla
# Time difference of 1.556889 mins / Time difference of 1.753611 mins
# min rmse 1.037336 vs 1.033333 / 1.035116 vs 1.031539
### mf prekompilowana
# Time difference of 1.499183 mins / Time difference of 1.767338 mins
# min rmse 1.035415 vs 1.062399 / 1.034587 vs 1.044712


### rbm zwyk??y  (losowanie bez zwracania i nie powinno nic zmienic...)
# Time difference of 23.35903 mins
# rmse min 1.103659 vs 1.105797
### rbm prekompilowany  (losowanie bez zwracania i nie powinno nic zmienic...)
# Time difference of 23.18899 mins
# rmse min 1.104834 vs 1.107917

### rbm zwyk??y  (losowanie userow)
# Time difference of 21.60405 mins
# 1.090225 vs 1.103525
