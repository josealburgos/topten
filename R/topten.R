#' Building a model with top ten features
#'
#' This function develops a prediction algorithm based on the top ten features in
#' 'x' that are most predictive of 'y'
#'
#' @param x a n x p matrix of n observations and p predictors
#' @param y a vector of length n representing the response
#' @return a vector of coefficients from the final fitted model with top 10 features
#' @author Copied from Roger Lpeng
#' @details This function runs a univariate regression on each predictor
#' @seealso \code{lm}
#' @export
#' @importFrom stats lm

topten <- function(x, y){
  p <- ncol(x)
  if(p < 10)
    stop("there are less than 10 predictors")
  pvalues <- numeric(p)
  for(i in seq_len(p)){
    fit <- lm(y ~ x[,i])
    summ <- summary(fit)
    pvalues[i] <- summ$coefficient[2,4]
  }
  ord <- order(pvalues)
  ord <- ord[1:10]
  x10 <- x[,ord]
  fit <- lm(y ~ x10)
  coef(fit)
}

#' prediction with top ten features
#'
#' This function takes a set of coefficients produced by the \code{topten} function
#' and makes a prediction for each of the values provided in the input X matrix
#'
#' @param X an n x 10 matric containing n new observations
#' @param b
#' @return a numeric vector containing the predicted values
#' @export

predict10 <- function(X, b) {
  X <- cbind(1,X)
  drop(X %*% b)
}
