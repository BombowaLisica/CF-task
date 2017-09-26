

arr.data <- function(t.data, t.n_user = NULL, t.n_movie = NULL, t.n_rate = NULL){
  # parametr n_movie jest wa??ny - je??li w test pojawi si?? nowy film, model b??dzie nieaktualny
  #   (w sumie podobnie jak w svd )
  if (missing(t.data) || dim(t.data)[2] < 3) {
    stop("Please use a valid data in data.frame format user:movie:rate")
  }
  names(t.data) <- c("user", "movie", "rate")
  mat.pom <- data.frame(t.data)[,1:3]
  
  # to jest wa??ne - je??li w ucz nie by??o usera z test i ??eby dobrze robic predykcje
  if (!is.null(t.n_user)){
    if (t.n_user > length(unique(t.data$user))){
      new.user <- subset(1:t.n_user, !1:t.n_user %in% (unique(t.data$user)))
      new.user <- data.frame(user = new.user, movie = 1:length(new.user), rate = rep(0, length(new.user)))
      mat.pom <- rbind(mat.pom, new.user)
    }
  }
  # to jest wa??ne - mo??na zdefiniowa?? wi??cej film??w, ni?? trafi do aktualnie wylosowanej pr??by ucz??cej
  if (!is.null(t.n_movie)){
    if (t.n_movie > length(unique(t.data$movie))){
      new.movie <- subset(1:t.n_movie, !1:t.n_movie %in% (unique(t.data$movie)))
      new.movie <- data.frame(user = 1:length(new.movie), movie = new.movie, rate = rep(0, length(new.movie)))
      mat.pom <- rbind(mat.pom, new.movie)
    }
  }
  if (is.null(t.n_rate)){
    n_rate <- sort(unique(t.data$rate))
  } else{
    n_rate <- 1:t.n_rate
  }
  
  mat.ucz.long <- spread(mat.pom, user, rate)
  mat.ucz.long <- mat.ucz.long[,2:dim(mat.ucz.long)[2]]
  mat.ucz.long[is.na(mat.ucz.long)] <- 0
  
  arr.data <- list()
  for (i in n_rate){
    arr.data[[i]] <- ifelse(mat.ucz.long == i, 1, 0)
  }
  
  return(arr.data)
}
### tak się odwołujemy: arr[[i]][1:5,1:5] (rate, movie, user)




### prawdopodobieństwa aktywacji hidden units
#   wynikiem powinien być wektor lub macierz dla batch
rbm.act_hid <- function(t.weight      # lista zawierająca w każdym elemencie macierz wag dla jednej oceny
                        , t.bias_hid  # wektor bias dla warsty hidden
                        , t.state     # stany visible (lista wektorów lub macierzy w przypadku batch)
                        # lapply(arr, function(x) {x[movies,users]})
) {
  
  n_hid   <- dim(t.weight[[1]])[1]  # [[1]], bo zawsze będzie przynajmniej jedna ocena
  n_movie <- dim(t.weight[[1]])[2]
  n_rate  <- length(t.weight)
  
  if (!is.null(dim(t.state[[1]])[2])){
    n_batch <- dim(t.state[[1]])[2]
    if (n_movie != dim(t.state[[1]])[1]){
      stop("Matrices are non-conformable! 1")
    }
  } else{
    n_batch <- 1
    if (n_movie != length(t.state[[1]])){
      stop("Matrices are non-conformable! 2")
    }
  }
  
  act <- list()
  for (i in 1:n_rate){
    act[[i]] <- t.weight[[i]] %*% t.state[[i]]
  }
  # Uwaga! to może być do zmiany jeśli będzie stacked i n_rate=1mat <- Reduce("+", act) i nie będzie to lista ale macierz
  mat <- Reduce("+", act)  
  mat <- mat + t.bias_hid
  return(1/(1+exp(-mat)))
  # zwracana jest macierz n_hid x n_batch
}


### prawdopodobieństwa aktywacji visible units
rbm.act_vis <- function(t.weight      # lista zawierająca w kaądym elemencie macierz wag dla jednej oceny
                        , t.bias_vis  # lista zawierająca w kaądym elemencie wektor bias dla jednej oceny 
                        , t.state     # stany hidden (wektor lub macierz w przypadku batch)
                        # czyli wektor lub macierz, nie lista!
) {
  
  n_hid   <- dim(t.weight[[1]])[1]  # [[1]], bo zawsze będzie przynajmniej jedna ocena
  n_movie <- dim(t.weight[[1]])[2]
  n_rate  <- length(t.weight)   # dim(t.weight)[3]
  
  if (!is.null(dim(t.state)[2])) {
    n_batch <- dim(t.state)[2]
  } else{
    n_batch <- 1
  }
  
  act <- list()
  for (i in 1:n_rate){
    act[[i]] <- t(t.weight[[i]]) %*% t.state+ t.bias_vis[[i]]
  }
  counter <- lapply(act, exp)
  denom <- Reduce("+", counter)
  ret <- list()
  for (i in 1:n_rate){
    ret[[i]] <- lapply(counter, function(x) { x <- x/denom})[[i]]
  }
  return(ret)
}


# włączamy i wyłączamy unit z pstwem zdefiniowanym energią
rbm.samp_state <- function(t.mat, t.ev = TRUE) {
  if (class(t.mat) == "matrix"){
    dims <- dim(t.mat)
    ret <- matrix(rbinom(prod(dims), size=1, prob=c(t.mat)) ,dims[1] ,dims[2])
  } else if (class(t.mat) == "list"){
    n_rate <- length(t.mat)
    dims <- dim(t.mat[[1]])
    ret <- list()
    if (t.ev == TRUE){
      ret_pom <- matrix(0, dims[1], dims[2])
      for (i in 1:n_rate){
        ret_pom <- ret_pom + t.mat[[i]]*i
      }
      ret_pom <- round(ret_pom)
      # żeby mieć jedną ocenę wylosowaną
      for (i in 1:n_rate){
        ret[[i]] <- ifelse(ret_pom == i, 1, 0)
      }
    } else {
      for (i in 1:n_rate){
        ret[[i]] <- matrix(rbinom(prod(dims), size=1, prob=c(t.mat[[i]])), dims[1], dims[2])
      }
    }
  } else {
    stop("Please use a valid weight matrix")
  }
  
  return(ret)
}




# uczenie
train.rbm <- function(t.n_hid = 45
                      , t.ucz
                      , t.wal = NULL
                      , t.n_user = NULL
                      , t.n_movie = NULL
                      , t.n_rate = NULL
                      , t.learning_rate = 0.005
                      , t.lambda = 0.00001
                      , t.epochs = 10
                      , t.batch_size = 20
                      , t.momentum = 0.9
                      , t.train_control = F) {
  
  data <- arr.data(t.ucz, t.n_user, t.n_movie, t.n_rate)
  
  # bez sensu, trzeba to sprawdzic tu i potem juz nie w arr.data
  if (is.null(t.n_user)){
    n_user <- dim(data[[1]])[2]  # n
  } else{
    n_user <- t.n_user
  }
  if (is.null(t.n_movie)){
    n_movie <- dim(data[[1]])[1]  # p    
  } else{
    n_movie <- t.n_movie
  }
  if (is.null(t.n_rate)){
    n_rate <- length(data)
  }
  else{
    n_rate <- t.n_rate    
  }
  
  if (t.n_hid <= 0) { stop("Please set valid number of hidden units") }
  if (t.epochs <= 0) { stop("Please set valid number of epochs") }
  if (t.batch_size <= 0 || t.batch_size > n_user) { stop("Please set valid size of batch") }
  
  # do obsluzenia stack tutaj pętla po length(t.n_hid) (???)
  weight <- list()
  momentum_speed <- list()
  bias_vis <- list()
  for(i in 1:n_rate){
    weight[[i]] <- matrix(rnorm(t.n_hid*n_movie, 0, 0.001/i), nrow=t.n_hid, ncol=n_movie)
    momentum_speed[[i]] <- matrix(0, nrow=t.n_hid, ncol=n_movie)
    bias_vis[[i]] <- rnorm(n_movie, 0, 0.001*i)
    #    pi <- rowSums(data[[i]])
    #    prop <- pi/sum(pi)
    #    prop <- log(prop/(1-prop))
    #    prop[is.infinite(prop)] <- -10 # bo log(0)
    #    bias_vis[[i]] <- prop
  }
  
  bias_hid <- rnorm(t.n_hid, 0, 0.01)
  
  for (epoch in 1:t.epochs) {
    inProbe <- sample(1:n_user)
    data <- lapply(data, function(x) {x[,inProbe]})
    
    batch_beg = 1
    batch_end <- 0
    while (batch_end < n_user){
      batch_end = batch_beg + t.batch_size - 1
      if (batch_end >= n_user) (batch_end <- n_user)
      batch <- lapply(data, function(x) {x[,batch_beg:batch_end]})
      # tu się czasem sypie, trzeba to obsłużyć
      batch_sign <- sign(rowSums(Reduce("+", batch)))
      
      if (batch_end < n_user) (batch_beg <- batch_end + 1)
      
      ### CD1
      H0 <- rbm.samp_state(rbm.act_hid(weight, bias_hid, batch))
      
      vh0 <- list()
      for (k in 1:n_rate){
        vh0[[k]] <- H0 %*% t(batch[[k]])
        vh0[[k]] <- vh0[[k]]/dim(batch[[k]])[2]
      }
      
      # albo pstwa
      V1 <- rbm.samp_state(rbm.act_vis(weight, bias_vis, H0)) # stany aktywacji visible units wg p-stw (vi x batch x k)
      V1 <- lapply(V1, function(x) { x*batch_sign })
      H1 <- rbm.act_hid(weight, bias_hid, V1) # tu już tylko pstwo, nie losowy stan (a jakby nie?)
      
      vh1 <- list()
      for (k in 1:n_rate){
        vh1[[k]] <- H1 %*% t(V1[[k]])
        vh1[[k]] <- vh1[[k]]/dim(V1[[k]])[2]
      }
      
      gradient <- list()
      for(i in 1:n_rate){
        # myśle ze indykatow wag powinien byc przy V0
        gradient[[i]]        <- (vh0[[i]] - vh1[[i]])         #update tylko ocen, kt??re byly
        momentum_speed[[i]]  <- t.momentum * momentum_speed[[i]] + gradient[[i]]       # a tu algorytm uczenia
        weight[[i]]          <- weight[[i]] +  (momentum_speed[[i]] + t.lambda * sum(abs(weight[[i]])))* t.learning_rate
        
        bias_vis[[i]]        <- bias_vis[[i]] + t.learning_rate * (rowSums(batch[[k]] - V1[[k]])/dim(V1[[k]])[2])
      }
      bias_hid <- bias_hid + t.learning_rate * (rowSums(H0 - H1)/dim(H1)[2])
    }
    if (t.train_control == T){
      t.pred <- pred.rbm(list(weight = weight, bias_hid = bias_hid, bias_vis = bias_vis, ucz = t.ucz, n_user = n_user, n_movie = n_movie, n_rate = n_rate), t.wal)
      cat("\n"); cat("epoch: ", epoch, " rmse on wal: ", rmse(t.wal, t.pred$pred), "\n")
    }
  }
  
  model <- list(weight = weight
                , bias_hid = bias_hid
                , bias_vis = bias_vis
                , ucz = t.ucz
                , n_user = n_user
                , n_movie = n_movie
                , n_rate = n_rate
                , learning_rate = t.learning_rate
                , epochs = t.epochs
                , batch_size = t.batch_size
                , momentum = t.momentum)
  class(model) <- 'rbm'
  #  if (n_rate == 1){
  #    class(model) <- 'rbm_single'  
  #  } else {
  #    class(model) <- 'rbm_stack'
  #  }
  
  return(model)
}












rbm.pred_hid <- function(t.weight
                         , t.bias_hid
                         , t.data # lista dla batch, wektor dla jednego usera
){
  n_movie <- dim(t.weight[[1]])[2]
  if (class(t.data) != "list"){
    data <- arr.data(t.data = t.data, t.n_movie = n_movie)
  } else {
    data <- t.data
  }
  data <- rbm.act_hid(t.weight, t.bias_hid, data)
  
  return(data)
}


rbm.pred_vis <- function(t.weight
                         , t.bias_vis
                         , t.data # macierz dla batch, wektor dla jednego usera
){
  data <- rbm.act_vis(t.weight, t.bias_vis, t.data)
  return(data)
}


pred.rbm <- function(t.model, t.test){
  
  # potrzebujemy wszystkich dostępnych ocen usera do predykcji
  # na razie zostawię tak, ale potem być może trzeba będzie zmienić na pełne ucz
  # choć jeśli to będzie ogarnięte w bagg i boost, to nie będzie to problem
  data <- subset(t.model$ucz, user %in% t.test$user)
  data <- arr.data(data, t.n_user = t.model$n_user, t.n_movie = t.model$n_movie, t.n_rate = t.model$n_rate)
  
  user <- t.test$user
  movie <- t.test$movie
  l <- dim(t.test)[1]
  
  n_rate <- t.model$n_rate
  
  pred_hid <- rbm.pred_hid(t.model$weight, t.model$bias_hid, data)
  pred_vis <- rbm.pred_vis(t.model$weight, t.model$bias_vis, pred_hid)
  
  weigh <- list()
  # wartosc oczekiwana
  for(k in 1:n_rate){
    weigh[[k]] <- pred_vis[[k]] * k
  }
  weigh <- Reduce("+", weigh)
  weigh <- weigh/Reduce("+", pred_vis)
  
  t.rate <- numeric(l)
  for (i in 1:l){
    t.rate[i] <- weigh[movie[i], user[i]]
    # a jak nie mamy to co?
  }
  
  rmse <- sqrt(sum((t.test$rate - t.rate)^2, na.rm = T)/length(which(is.na(t.rate)==F)))
  cover <- length(t.rate[is.na(t.rate)==FALSE])/dim(t.test)[1]
  
  prediction <- list(pred = t.rate, rmse = rmse, cover = cover)
  
  return(prediction)
}








start <- Sys.time()
rbm.model = train.rbm(t.n_hid = 45
                      , t.ucz = ucz #subset(ucz, rate==1)
                      , t.wal = wal #subset(wal, rate==1)
                      , t.n_movie = 1682
                      , t.learning_rate = 0.01
                      , t.lambda = 0
                      , t.epochs = 5
                      , t.batch_size = 60
                      , t.momentum = 0.8
                      , t.train_control = T)
stop <- Sys.time()
stop-start
# Time difference of 1.271265 mins
# Time difference of 1.003673 mins

rbm.pred <- pred.rbm(rbm.model, test)
(rmse.rbm <- rmse(test, rbm.pred$pred))
summary(rbm.pred$pred); summary(test$rate)
par(mfrow=c(2,1))
hist(test$rate, breaks = 0:6, xlim = c(0, 5)); hist(rbm.pred$pred, xlim = c(0, 5))

par(mfrow=c(1,2))
boxplot(test$rate); boxplot(rbm.pred$pred, ylim=c(1,5))


