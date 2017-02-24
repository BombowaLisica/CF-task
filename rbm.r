


arr.data <- function(t.data, t.n_user = NULL, t.n_movie = NULL){
  # parametr n_movie jest ważny - jeśli w test pojawi się nowy film, model będzie nieaktualny
  if (missing(t.data) || dim(t.data)[2] != 3) {
    stop("Please use a valid data")
  }
  names(t.data) <- c("user", "movie", "rate")
  mat.pom <- data.frame(t.data)[,1:3]
  
  if (!is.null(t.n_user)){
    if (t.n_user > length(unique(t.data$user))){
      new.user <- subset(1:t.n_user, !1:t.n_user %in% (unique(t.data$user)))
      new.user <- data.frame(user = new.user, movie = 1:length(new.user), rate = rep(0, length(new.user)))
      mat.pom <- rbind(mat.pom, new.user)
    }
  }
  if (!is.null(t.n_movie)){
    if (t.n_movie > length(unique(t.data$movie))){
      new.movie <- subset(1:t.n_movie, !1:t.n_movie %in% (unique(t.data$movie)))
      new.movie <- data.frame(user = 1:length(new.movie), movie = new.movie, rate = rep(0, length(new.movie)))
      mat.pom <- rbind(mat.pom, new.movie)
    }
  }
  
  mat.ucz.long <- spread(mat.pom, movie, rate)
  mat.ucz.long <- mat.ucz.long[,2:dim(mat.ucz.long)[2]]
  mat.ucz.long[is.na(mat.ucz.long)] <- 0
  
  arr.ucz <- array(0, c(dim(mat.ucz.long), 5))
  arr.ucz[,,1] <- ifelse(mat.ucz.long == 1, 1, 0)
  arr.ucz[,,2] <- ifelse(mat.ucz.long == 2, 1, 0)
  arr.ucz[,,3] <- ifelse(mat.ucz.long == 3, 1, 0)
  arr.ucz[,,4] <- ifelse(mat.ucz.long == 4, 1, 0)
  arr.ucz[,,5] <- ifelse(mat.ucz.long == 5, 1, 0)
  
  arr.data <- aperm(arr.ucz, c(2,1,3))
  return(arr.data)
}


rbm.act_hid <- function(t.weight, t.state) {
  if (length(dim(t.weight)) != 3){
    stop("Weight matrix size is invalid!")
  }
  if (length(dim(t.state)) != 3){
    stop("State matrix size is invalid!")
  }
  n_hid <- dim(t.weight)[1]
  n_movie <- dim(t.weight)[2]
  n_rate <- dim(t.weight)[3]
  n_batch <- dim(t.state)[2]
  if (n_movie != dim(t.state)[1]){
    stop("Matrices are non-conformable!")
  }
  
  bias <- rep(1, n_hid)
  act <- list()
  for (i in 1:n_rate){
    act[[i]] <- -t.weight[,,i] %*% t.state[,,i]
  }
  mat <- Reduce("+", act)
  mat <- mat + bias
  return(1/(1+exp(mat)))
}


rbm.act_vis <- function(t.weight, t.state) {
  if (length(dim(t.weight)) != 3){
    stop("Weight matrix size is invalid!")
  }
  if (length(dim(t.state)) > 2){
    stop("State matrix size is invalid!")
  }
  n_hid <- dim(t.weight)[1]
  n_movie <- dim(t.weight)[2]
  n_rate <- dim(t.weight)[3]
  n_batch <- dim(t.state)[2]
  n_hid_state <- dim(t.state)[1]
  if (is.null(n_batch)) {
    n_batch <- 1
    n_hid_state <- length(t.state)
  }
  if (n_hid != n_hid_state){
    stop("Matrices are non-conformable!")
  }
  
  weight_t <- aperm(t.weight, c(2,1,3))
  act <- list()
  for (i in 1:n_rate){
    act[[i]] <- -weight_t[,,i] %*% t.state + 1
  }
  # w pierwszej iteracji wychodzą takie same pstwa dla wszystkich vis, podejrzane
  counter <- lapply(act, exp)
  denom <- Reduce("+", counter)
  ret <- array(0, c(n_movie, n_batch, n_rate))
  for (i in 1:n_rate){
    ret[,,i] <- lapply(counter, function(x) { x <- x/denom})[[i]]
  }
  return(ret)
}


# włączamy i wyłączamy unit z pstwem zdefiniowanym energią
rbm.samp_state <- function(t.mat) {
  dims=dim(t.mat)
  if (length(dims) == 3){
    array(rbinom(prod(dims), size=1, prob=c(t.mat)) ,dims)
  } else if (length(dims) == 2){
    matrix(rbinom(prod(dims), size=1, prob=c(t.mat)) ,dims[1] ,dims[2])
  } else {
    stop("Please use a valid weight matrix")
  }
}



rbm.train <- function(t.n_hid = 45
                   , t.ucz
                   , t.wal = NULL
                   , t.n_user = NULL
                   , t.n_movie = NULL
                   , t.learning_rate = 0.005
                   , t.epochs = 10
                   , t.batch_size = 20
                   , t.momentum = 0.9
                   , t.train_control = F) {
  
  data <- arr.data(t.ucz, t.n_user, t.n_movie)
  
  n_user <- dim(data)[2]  # n
  n_movie <- dim(data)[1]  # p
  n_rate <- dim(data)[3]
  
  if (t.n_hid <= 0) { stop("Please set valid number of epochs") }
  if (t.epochs <= 0) { stop("Please set valid number of hidden units") }
  if (t.batch_size <= 0 || t.batch_size > n_user) { stop("Please set valid size of batch") }
  
  # do obsluzenia stack tutaj pętla po length(t.n_hid)
  weight = array(rnorm(t.n_hid*n_movie*n_rate, 0, 0.01), c(t.n_hid, n_movie, n_rate))
  class(weight) <- "rbm_single"
  momentum_speed = array(0, c(t.n_hid, n_movie, n_rate))
  
  for (epoch in 1:t.epochs) {
    batch_beg = 1
    batch_end <- 0
    while (batch_end < n_user){
      batch_end = batch_beg + t.batch_size - 1
      if (batch_end >= n_user) (batch_end <- n_user)
      batch <- data[, batch_beg:batch_end,]
      batch_t <- aperm(batch, c(2,1,3))
      if (batch_end < n_user) (batch_beg <- batch_end + 1)
      
      visible_data <- rbm.samp_state(batch)  # gdyby to bylo CDn nie CD1 to miałoby sens
      H0 <- rbm.samp_state(rbm.act_hid(weight, batch))
      
      vh0 <- array(0, c(t.n_hid, n_movie, n_rate))
      for (k in 1:n_rate){ vh0[,,k] <- H0 %*% batch_t[,,k] }
      vh0 <- vh0/dim(batch)[2]  # faza positive (hid x nmovie) -uśrednienie po liczbie przypadków
      
      V1 <- rbm.samp_state(rbm.act_vis(weight, H0)) # stany aktywacji visible units wg p-stw (vi x batch x k)
      H1 <- rbm.act_hid(weight, V1)
      
      vh1 <- array(0, c(t.n_hid, n_movie, n_rate))
      for (k in 1:n_rate){ vh1[,,k] <- H1 %*% aperm(V1, c(2,1,3))[,,k] }
      vh1 <- vh1/dim(V1)[2] # faza negative
      
      gradient        <- vh0 - vh1
      momentum_speed  <- t.momentum * momentum_speed + gradient       # a tu algorytm uczenia
      weight           <- weight + momentum_speed * t.learning_rate
    }
    if (t.train_control == T){
      t.pred <- rbm.pred(t.wal, list(weight = weight, ucz = t.ucz))
      cat("\n"); cat("epoch: ", epoch, " rmse on wal: ", rmse(t.wal, t.pred))
    }
    # some early stopping here
  }
  
  model <- list(weight = weight
                , ucz = t.ucz
                , learning_rate = t.learning_rate
                , epochs = t.epochs
                , batch_size = t.batch_size
                , momentum = t.momentum)
  # ale to do obsłużenia jeszcze....
  # każda warstwa musi mieć swoją macierz wag
  if (length(t.n_hid) == 1){
    class(model) <- 'rbm_single'  
  } else {
    class(model) <- 'rbm_stack'
  }
  
  return(model)
}




rbm.pred_hid <- function(t.weight, t.data){
  n_movie <- dim(t.weight)[2]
  if (class(t.data) != "array"){
    data <- arr.data(t.data, t.n_movie = n_movie)
  } else {data <- t.data}
  
  if(class(t.weight) == 'rbm_single'){
    data <- rbm.act_hid(t.weight, data)
  }else if(class(t.weight) == 'rbm_stack'){
    for(i in 1:length(t.weight)){
      # tu będzie problem z weight
      data <- rbm.act_hid(t.weight[[i]]$weight, data)
    }
  }else{
    stop("Please use a valid weight");
  }
  return(t(data))
}


rbm.pred_vis <- function(t.weight, t.data){
  data <- t(t.data)
  if(class(t.weight) == 'rbm_single'){
    data <- rbm.act_vis(t.weight, data)
  }else if(class(t.weight) == 'rbm_stack'){
    for(i in length(t.weight):1){
      data <- rbm.act_vis(t.weight[[i]]$weights, data)
    }
  }else{
    stop("Please use a valid weight");
  }
  return(aperm(data, c(2,1,3)))
}



rbm.pred <- function(t.test, t.model){
  
  data <- subset(t.model$ucz, user %in% t.test$user)
  data <- arr.data(data, t.n_movie = dim(t.model$weight)[2])
  
  user <- t.test$user
  movie <- t.test$movie
  l <- dim(t.test)[1]
  
  pred_hid <- rbm.pred_hid(t.model$weight, data)
  pred_vis <- rbm.pred_vis(t.model$weight, pred_hid)
  
  pred <- numeric(l)
  for (i in 1:l){
    pred[i] <- sum(pred_vis[user[i], movie[i], ] %*% 1:5)/sum(pred_vis[user[i], movie[i], ])
  }
  return(pred)
}







rbm = rbm.train(t.n_hid = 45
                , t.ucz = ucz[,1:3]
                , t.wal = wal[,1:3]
                , t.n_movie = 1682
                , t.learning_rate = 0.0005
                , t.epochs = 20
                , t.batch_size = 20
                , t.momentum = 0.8
                , t.train_control = T)
# błąd tylko rośnie....
pred.rbm <- rbm.pred(test, rbm)
(rmse.rbm <- rmse(test, pred.rbm))
summary(pred.rbm); summary(test$rate)
par(mfrow=c(2,1))
hist(test$rate);hist(pred.rbm, xlim=c(1,5))
# 1.956328
# 1.092   1.631   2.462   2.368   3.009   4.033
