
# function for rmse calculation
rmse <- function(t.test, t.pred){
  rmse <- sqrt(sum((t.test$rate - t.pred)^2, na.rm = T)/length(which(is.na(t.pred)==F)))
  return(rmse)
}


# generic predict function
pred <- function(x, ...){
  if(is.null(attr(x, "class"))){
    stop("Please, use a valid class model")
  }
  else UseMethod("pred", x)
}

#pred.default <- pred(x)

