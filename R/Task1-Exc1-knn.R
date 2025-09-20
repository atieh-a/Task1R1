#' Constructor for an S3 k-NN model
#'
#' @param train_x Matrix of training predictors
#' @param train_y Numeric vector of training responses
#' @param k Number of neighbors
#' @param dist Distance metric: "sse" (L2) or "sad" (L1)
#' @return An object of class knn_s3
#' @export
knn_s3 <- function(train_x, train_y, k = 5L, dist = c("sse", "sad")) {
  dist <- match.arg(dist)
  
  train_x <- as.matrix(train_x)
  train_y <- as.numeric(train_y)
  stopifnot(
    is.numeric(k), k >= 1, k <= nrow(train_x),
    nrow(train_x) == length(train_y)
  )
  structure(
    list(
      train_x = train_x,
      train_y = train_y,
      k       = as.integer(k),
      dist    = dist
    ),
    class = "knn_s3"
  )
}

# Print method
print.knn_s3 <- function(x, ...) {
  cat("knn_s3 model:\n",
      "  n =", nrow(x$train_x),
      "  p =", ncol(x$train_x),
      "  k =", x$k, "\n")
  invisible(x)
}

# Predict method (chooses R or C++ backend)
predict.knn_s3 <- function(object, newdata, method = c("R", "cpp"), ...) {
  method <- match.arg(method)
  Xtest <- as.matrix(newdata)
  if (method != "cpp") {
    n <- nrow(object$train_x)
    m <- nrow(Xtest)
    preds <- numeric(m)
    for (j in seq_len(m)) {
      # compute squared distances to all train points
      if (object$dist == "sse") {
        dists <- rowSums((object$train_x - 
                            matrix(Xtest[j,], n, ncol(Xtest), byrow = TRUE))^2)
      } else if (object$dist == "sad") {
        dists <- rowSums(abs(object$train_x - 
                               matrix(Xtest[j,], n, ncol(Xtest), byrow = TRUE)))
      }
      nn <- order(dists)[1:object$k]
      preds[j] <- mean(object$train_y[nn])
      
    }
    return(preds)
  } else {
    # Call into our Rcpp function
    knn_pred_cpp(object$train_x, object$train_y, Xtest, object$k)
  }
}

