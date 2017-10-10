
library(Rcpp)

cppFunction('
  NumericVector rowSumsC(NumericMatrix x) {
    int nrow = x.nrow(), ncol = x.ncol();
    NumericVector out(nrow);
            
    for (int i = 0; i < nrow; i++) {
      double total = 0;
      for (int j = 0; j < ncol; j++) {
        total += x(i, j);
      }
      out[i] = total;
    }
    return out;
  }')




cppFunction('
  List spreadC(NumericMatrix data, int user, int movie, int rate){
    List outList(rate);
    NumericMatrix out(movie, user);
    int k = data.nrow();

    for(int j=0; j<rate; ++j){
      NumericMatrix outPom(movie, user);
      for(int i=0; i<k; ++i){
        int u = data(i,0);
        int m = data(i,1);
        int r = data(i,2);
        if (r == j+1){ outPom(m-1, u-1) = 1;}
      }
      outList[j] = outPom;
    }

    return(outList);
  }')

# te parametry nie mog¹ byæ nu¿
arr.data <- function(t.data, t.n_user = NULL, t.n_movie = NULL, t.n_rate = NULL){
  # parametr n_movie jest wa¿ny - jeLœli w test pojawi siê nowy film, model bêdzie nieaktualny
  #   (w sumie podobnie jak w mf )
  if (missing(t.data) || dim(t.data)[2] < 3) {
    stop("Please use a valid data in data.frame format user:movie:rate")
  }
  names(t.data) <- c("user", "movie", "rate")
  mat.pom <- data.frame(t.data)[,1:3]
  
  # to jest wa¿ne - jeœli w ucz nie by³o usera z test i ¿eby dobrze robic predykcje
  if (!is.null(t.n_user)){
    if (t.n_user > length(unique(t.data$user))){
      new.user <- subset(1:t.n_user, !1:t.n_user %in% (unique(t.data$user)))
      new.user <- data.frame(user = new.user, movie = 1:length(new.user), rate = rep(0, length(new.user)))
      mat.pom <- rbind(mat.pom, new.user)
    }
  }
  # to jest wa¿ne - mo¿na zdefiniowaæ wiêcej filmAlw, ni¿ trafi do aktualnie wylosowanej prAlby ucz¹cej
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
    n_rate <- t.n_rate
  }
    # tu mo¿na wprowadziæ wagi dla boost
#    mat.pom <- unique(mat.pom)
  dup <- duplicated(mat.pom)
  mat.pom <- mat.pom[!dup,]
  
    
#  mat.ucz.long <- spread(mat.pom, user, rate, fill = 0)
    mat.ucz.long <- spreadC(as.matrix(mat.pom), t.n_user, t.n_movie, n_rate)  
#  mat.ucz.long <- mat.ucz.long[,-c(1)]
#  mat.ucz.long[is.na(mat.ucz.long)] <- 0
  
#  mat <- as.matrix(mat.ucz.long)
#  arr.data <- list()
#  for (i in n_rate){
#  #  arr.data[[i]] <- ifelse(mat == i, 1, 0)
#    ktore <- which(mat==i)
#    arr.data[[i]] <- matrix(0, dim(mat)[1], dim(mat)[2])
#    arr.data[[i]][ktore] <- 1
#  }
  
#  return(arr.data)
  return(mat.ucz.long)
}
### tak siê odwo³ujemy: arr[[i]][1:5,1:5] (rate, movie, user)




### prawdopodobieñstwa aktywacji hidden units
#   wynikiem powinien byæ wektor lub macierz dla batch
rbm.act_hid <- function(t.weight      # lista zawieraj¹ca w ka¿dym elemencie macierz wag dla jednej oceny
                        , t.bias_hid  # wektor bias dla warsty hidden
                        , t.state     # stany visible (lista wektorAlw lub macierzy w przypadku batch)
                        # lapply(arr, function(x) {x[movies,users]})
) {
  
  n_hid   <- dim(t.weight[[1]])[1]  # [[1]], bo zawsze bêdzie przynajmniej jedna ocena
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
  # Uwaga! to mo¿e byæ do zmiany jeœli bêdzie stacked i n_rate=1mat <- Reduce("+", act) i nie bêdzie to lista ale macierz
  mat <- Reduce("+", act)  
  mat <- mat + t.bias_hid
  return(1/(1+exp(-mat)))
  # zwracana jest macierz n_hid x n_batch
}


### prawdopodobieñstwa aktywacji visible units
rbm.act_vis <- function(t.weight      # lista zawieraj¹ca w ka¿dym elemencie macierz wag dla jednej oceny
                        , t.bias_vis  # lista zawieraj¹ca w ka¿dym elemencie wektor bias dla jednej oceny 
                        , t.state     # stany hidden (wektor lub macierz w przypadku batch)
                        # czyli wektor lub macierz, nie lista!
) {
  
  n_hid   <- dim(t.weight[[1]])[1]  # [[1]], bo zawsze bêdzie przynajmniej jedna ocena
  n_movie <- dim(t.weight[[1]])[2]
  n_rate  <- length(t.weight)   # dim(t.weight)[3]
  
  if (!is.null(dim(t.state)[2])) {
    n_batch <- dim(t.state)[2]
  } else{
    n_batch <- 1
  }
  
  act <- list()
  for (i in 1:n_rate){
    act[[i]] <- t(t.weight[[i]]) %*% t.state + t.bias_vis[[i]]
  }
  counter <- lapply(act, exp)
  denom <- Reduce("+", counter)
  ret <- list()
  for (i in 1:n_rate){
  #  ret[[i]] <- lapply(counter, function(x) { x <- x/denom})[[i]]
    ret[[i]] <- counter[[i]]/denom
  }
  return(ret)
}


# w³¹czamy i wy³¹czamy unit z pstwem zdefiniowanym energi¹
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
      # ¿eby mieæ jedn¹ ocenê wylosowan¹
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
  # dla du¿o wiêkszych zbiorAlw danych trzeba bêdzie przenieœæ arr.data do pêtli
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
  
  # do obsluzenia stack tutaj pêtla po length(t.n_hid) (???)
  weight <- list()
  momentum_speed <- list()
  bias_vis <- list()
  rmse_u <- list()
  rmse_w <- list()
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
      # tu siê czasem sypie, trzeba to obs³u¿yæ
      batch_sign <- sign(rowSumsC(Reduce("+", batch)))
        weightPom <- lapply(weight, function(x) { x * batch_sign})
      
      if (batch_end < n_user) (batch_beg <- batch_end + 1)
      
      ### CD1
      # tu jeszcze ok, bo wagi wyzeruje batch
      H0 <- rbm.samp_state(rbm.act_hid(weightPom, bias_hid, batch))
      
      vh0 <- list()
      for (k in 1:n_rate){
        vh0[[k]] <- H0 %*% t(batch[[k]])
        vh0[[k]] <- vh0[[k]]/dim(batch[[k]])[2]
      }
      
      # albo pstwa
      V1 <- rbm.samp_state(rbm.act_vis(weightPom, bias_vis, H0)) # stany aktywacji visible units wg p-stw (vi x batch x k)
#      V1 <- lapply(V1, function(x) { x*batch_sign })
      H1 <- rbm.act_hid(weightPom, bias_hid, V1) # tu ju¿ tylko pstwo, nie losowy stan (a jakby nie?)
      
      vh1 <- list()
      for (k in 1:n_rate){
        vh1[[k]] <- H1 %*% t(V1[[k]])
        vh1[[k]] <- vh1[[k]]/dim(V1[[k]])[2]
      }
      
      gradient <- list()
      for(i in 1:n_rate){
        gradient[[i]]        <- (vh0[[i]] - vh1[[i]])         
        momentum_speed[[i]]  <- t.momentum * momentum_speed[[i]] + gradient[[i]]       # a tu algorytm uczenia
        weight[[i]]          <- weight[[i]] +  (momentum_speed[[i]] + t.lambda * sum(abs(weightPom[[i]])))* t.learning_rate
#        weight[[i]]          <- weight[[i]] * batch_sign  # update tylko ocen, które byly w batch
        bias_vis[[i]]        <- bias_vis[[i]] + t.learning_rate * (rowSumsC(batch[[k]] - V1[[k]])/dim(V1[[k]])[2])
      }
      bias_hid <- bias_hid + t.learning_rate * (rowSumsC(H0 - H1)/dim(H1)[2])
    }
    if (t.train_control == T){
      t.pred_u <- pred.rbm(list(weight = weight, bias_hid = bias_hid, bias_vis = bias_vis, ucz = t.ucz, n_user = n_user, n_movie = n_movie, n_rate = n_rate), t.ucz)
      t.pred_w <- pred.rbm(list(weight = weight, bias_hid = bias_hid, bias_vis = bias_vis, ucz = t.ucz, n_user = n_user, n_movie = n_movie, n_rate = n_rate), t.wal)
      cat("\n"); cat("epoch: ", epoch, " rmse on wal: ", rmse(t.wal, t.pred_w$pred), " rmse on ucz: ", rmse(t.ucz, t.pred_u$pred), "\n")
      rmse_u[[epoch]] <- t.pred_u$rmse
      rmse_w[[epoch]] <- t.pred_w$rmse
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
                , momentum = t.momentum
                , rmse_u = rmse_u
                , rmse_w = rmse_w)
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
  n_user <- dim(t.data)[2]
  if (class(t.data) != "list"){
    data <- arr.data(t.data = t.data, t.n_user = n_user, t.n_movie = n_movie)
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


cppFunction('
  NumericVector predRbm(NumericVector user, NumericVector movie, int n_rate, List pred_vis, int l){
            
    NumericMatrix h = pred_vis[0];
    NumericMatrix weigh(h.nrow(), h.ncol());
    NumericVector pred(l);
            
    for (int k = 0; k < n_rate;  ++k){
      NumericMatrix pom = pred_vis[k];
      int r = pom.nrow(); // mozna zmienic na po kolumnach
      for(int r_i=0; r_i<r; ++r_i){
        weigh(r_i,_) = weigh(r_i,_) + pom(r_i,_) * (k+1);
      }
    }
            
    for (int i=0; i<l; ++i){
      int m = movie[i]-1;
      int u = user[i]-1;
      pred(i) = weigh(m, u);
    }    
            
    return(pred);
  }')
pred.rbm <- function(t.model, t.test){
  
  # potrzebujemy wszystkich dostêpnych ocen usera do predykcji
  # na razie zostawiê tak, ale potem byæ mo¿e trzeba bêdzie zmieniæ na peL‚ne ucz
  # choæ jeœli to bêdzie ogarniête w bagg i boost, to nie bêdzie to problem
  data <- subset(t.model$ucz, user %in% t.test$user)
  data <- arr.data(data, t.n_user = t.model$n_user, t.n_movie = t.model$n_movie, t.n_rate = t.model$n_rate)
  
  user <- t.test$user
  movie <- t.test$movie
  l <- dim(t.test)[1]
  
  n_rate <- t.model$n_rate
  pred_hid <- rbm.pred_hid(t.model$weight, t.model$bias_hid, data)
  pred_vis <- rbm.pred_vis(t.model$weight, t.model$bias_vis, pred_hid)
  
#  weigh <- list()
#  # wartosc oczekiwana
#  for(k in 1:n_rate){
#    weigh[[k]] <- pred_vis[[k]] * k
#  }
#  weigh <- Reduce("+", weigh)
#  weigh <- weigh/Reduce("+", pred_vis)
  
#  t.rate <- numeric(l)
#  for (i in 1:l){
#    t.rate[i] <- weigh[movie[i], user[i]]
#    # a jak nie mamy to co?
#  }
  t.rate <- predRbm(user, movie, n_rate, pred_vis, l)
  
  rmse <- sqrt(sum((t.test$rate - t.rate)^2, na.rm = T)/length(which(is.na(t.rate)==F)))
  cover <- length(t.rate[is.na(t.rate)==FALSE])/dim(t.test)[1]
  
  prediction <- list(pred = t.rate, rmse = rmse, cover = cover)
  
  return(prediction)
}








start <- Sys.time()
rbm.model = train.rbm(t.n_hid = 45
                      , t.ucz = ucz #subset(ucz, rate==1)
                      , t.wal = wal #subset(wal, rate==1)
                      , t.n_user = 943
                      , t.n_movie = n_movie #1682
                      , t.n_rate = 5
                      , t.learning_rate = 0.01
                      , t.lambda = 0
                      , t.epochs = 3
                      , t.batch_size = 100
                      , t.momentum = 0.8
                      , t.train_control = T)
# min rmse jest ok 3-5 epoki, ALE
# im mniej epok, tym mniejszy rozrzut ocen - prawoskoœny
# lepiej wiêcej epok, ale z mniejszym batch
# mo¿na potestowaæ, czy bagging pomo¿e w tym przypadku
# najlepiej to wygl¹da dla 10/100, min przy 3 epoce, ALE rozrzut duzo mniejszy
stop <- Sys.time()
stop-start
# Time difference of 1.271265 mins
# Time difference of 1.003673 mins
# Time difference of 36.86969 secs
# 5/50
# Time difference of 1.017087 mins
# po zmianie arr.data
# Time difference of 44.25542 secs
# Time difference of 46.967 secs

### 3/100
# Time difference of 27.32673 secs
# pred.rbm z rcpp
# Time difference of 26.44071 secs
# arr.data z rcpp
# Time difference of 24.37044 secs
# zmiana rowSums na rowSumsC nic nie daje

### 100/100
# Time difference of 13.03954 mins


par(mfrow=c(1,1))
plot(1:rbm.model$epochs, rbm.model$rmse_u[1:rbm.model$epochs], type= "l"
     , main = "RBM", xlab = "epoch", ylab = "rmse", ylim = c(1, 1.2))
lines(1:rbm.model$epochs, rbm.model$rmse_w[1:rbm.model$epochs], col = "red")
legend("topright", c("training set", "wal set"), fill = c("black", "red"))


rbm.pred <- pred.rbm(rbm.model, test)
(rmse.rbm <- rmse(test, rbm.pred$pred))
summary(rbm.pred$pred); summary(test$rate)
par(mfrow=c(2,1))
hist(test$rate, breaks = 0:6, xlim = c(0, 5)); hist(rbm.pred$pred, xlim = c(0, 5))

par(mfrow=c(1,2))
boxplot(test$rate); boxplot(rbm.pred$pred, ylim=c(1,5))