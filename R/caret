caret.HDtweedie <- list()

caret.HDtweedie$label <- 'Grouped Lasso Solution to Tweedie Elastic Net'

caret.HDtweedie$library <- 'HDtweedie'

caret.HDtweedie$type <- 'Regression'

caret.HDtweedie$parameters <- dplyr::data_frame(parameter = c('alpha', 'lambda'),
                                                class = rep('numeric', 2),
                                                label = c('Mixing Percentage', 'Regularization'))

caret.HDtweedie$grid <- function (x, y, len = NULL, search = "grid") 
{
  if (search == "grid") {
    init <- HDtweedie(as.matrix(x), 
                      y, 
                      nlambda = len + 2,
                      p = 1.5,
                      alpha = 0.5)
    lambda <- unique(init$lambda)
    lambda <- lambda[-c(1, length(lambda))]
    lambda <- lambda[1:min(length(lambda), len)]
    out <- expand.grid(alpha = seq(0.1, 1, 
                                   length = len), 
                       lambda = lambda)
  }
  else {
    out <- data.frame(alpha = runif(len, 
                                    min = 0, 1), 
                      lambda = 2^runif(len,
                                       min = -10, 3))
  }
  out
}

caret.HDtweedie$loop <- function (grid) 
{
  alph <- unique(grid$alpha)
  loop <- data.frame(alpha = alph)
  loop$lambda <- NA
  submodels <- vector(mode = "list", length = length(alph))
  for (i in seq(along = alph)) {
    np <- grid[grid$alpha == alph[i], "lambda"]
    loop$lambda[loop$alpha == alph[i]] <- np[which.max(np)]
    submodels[[i]] <- data.frame(lambda = np[-which.max(np)])
  }
  list(loop = loop, submodels = submodels)
}

caret.HDtweedie$fit <- function (x, y, wts, param, lev, last, classProbs, ...) 
{
  theDots <- list(...)

  if (!is.null(wts)) theDots$weights <- wts
  
  if (!(class(x)[1] %in% c("matrix", "sparseMatrix"))) x <- as.matrix(x)
  
  modelArgs <- c(list(x = x, y = y, alpha = param$alpha), theDots)
  
  out <- do.call("HDtweedie", modelArgs)
  
  if (!is.na(param$lambda[1])) out$lambdaOpt <- param$lambda[1]
  
  out
}

caret.HDtweedie$predict <- function (modelFit, newdata, submodels = NULL) 
{
  if (!is.matrix(newdata)) newdata <- as.matrix(newdata)
  
  out <- predict(modelFit, newdata, s = modelFit$lambdaOpt)
  
  if (is.matrix(out)) out <- out[, 1]
  
  if (!is.null(submodels)) {
      tmp <- as.list(as.data.frame(predict(modelFit, newdata, 
                                           s = submodels$lambda)))
  }
  c(list(out), tmp)
}

caret.HDtweedie$predictors <- function (x, lambda = NULL, ...) 
{
  if (is.null(lambda)) {
    if (length(lambda) > 1) 
      stop("Only one value of lambda is allowed right now")
    if (!is.null(x$lambdaOpt)) {
      lambda <- x$lambdaOpt
    }
    else stop("must supply a value of lambda")
  }
  allVar <- if (is.list(x$beta)) 
    rownames(x$beta[[1]])
  else rownames(x$beta)
  out <- unlist(predict(x, s = lambda, type = "link"))
  out <- unique(out)
  if (length(out) > 0) {
    out <- out[!is.na(out)]
    out <- allVar[out]
  }
  out
}

caret.HDtweedie$varImp <- function (object, lambda = NULL, ...) 
{
  if (is.null(lambda)) {
    if (length(lambda) > 1) 
      stop("Only one value of lambda is allowed right now")
    if (!is.null(object$lambdaOpt)) {
      lambda <- object$lambdaOpt
    }
    else stop("must supply a value of lambda")
  }
  beta <- predict(object, s = lambda, type = "link")
  if (is.list(beta)) {
    out <- do.call("cbind", lapply(beta, function(x) x[, 
                                                       1]))
    out <- as.data.frame(out)
  }
  else out <- data.frame(Overall = beta[, 1])
  out <- abs(out[rownames(out) != "(Intercept)", , drop = FALSE])
  out
}

caret.HDtweedie$tags <- c('Generalized Linear Model',
                          'Implicit Feature Selection',
                          'L1 Regularization',
                          'L2 Regularization',
                          'Linear Regression',
                          'Tweedie')

caret.HDtweedie$sort <- function (x) x[order(-x$lambda, x$alpha), ]

caret.HDtweedie$trim <- function (x) 
{
  x$call <- NULL
  x$df <- NULL
  x$dev.ratio <- NULL
  x
}

caret.HDtweedie$prob <- function (modelFit, newdata, submodels = NULL) 
{
  stop('HDtweedie does not do classification. Prediction type = "prob" will not work.')
}
