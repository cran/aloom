fit.method <- function(method,x,y,newx,model.params,missing=NA,seed=1)
{
  if (!is.na(missing))
  {
    name.missing <- rownames(x)[missing]
    x <- x[-missing,]
    y <- y[-missing]
  } else
  {
    name.missing <- NA
  }

  if (method=="rf") fit <- fit.rf(x,y,newx,model.params,seed)
  if (method=="glmnet") fit <- fit.glmnet(x,y,newx,model.params,seed)
  list(predictedY=fit$predictedY,predictedProbabilityY=fit$predictedProbabilityY,name.missing=name.missing)
}

fit.rf <- function(x,y,newx,model.params,seed)
{
  ntree <- model.params$ntree

  set.seed(seed)
  fit.RF <- randomForest::randomForest(x,
                                       y,
                                       ntree=ntree)
  predictedY <- as.vector(predict(fit.RF,newx,type="response"))
  predictedProbs   <- predict(fit.RF,newx,type="prob")
  predictedProbabilityY<-predictedProbs[,2]
  list(predictedY=predictedY,predictedProbabilityY=predictedProbabilityY)
}

fit.glmnet <- function(x,y,newx,model.params,seed)
{
  alpha           <- model.params$alpha
  lambda          <- model.params$lambda
  selected.lambda <- model.params$selected.lambda

  fit.glmnet <- glmnet::glmnet(x,
                               y,
                               family="binomial",
                               alpha=alpha,
                               lambda=lambda)

  idx <- which(lambda==selected.lambda)
  predictedY.all.lambda <- predict(fit.glmnet,as.matrix(newx),type="class")
  predictedY <- predictedY.all.lambda[,idx]
  predictedProbabilityY.all.lambda <- predict(fit.glmnet,as.matrix(newx),type="response")
  predictedProbabilityY <- predictedProbabilityY.all.lambda[,idx]
  list(predictedY=predictedY,predictedProbabilityY=predictedProbabilityY)
}

#' All Leave-One-Out Models
#'
#' Creates a predictive model for a training set, as well as
#' all leave-one-out predictive models. Produces predictions of 
#' all models (original and all leave one-out) for a test set.
#'
#' @param train.x input matrix, of dimension nobs x nvars; each row is an observation
#' vector. 
#' @param train.y response variable; binary factor of the same length as nrow(train.x) 
#' @param test.x Matrix of new values for \code{train.x} at which predictions are to be
#' made. Must be a matrix.
#' @param method name of the model. Currently allowed values are "rf" and "glmnet"
#' @param model.params list of model parameters
#' @param mc.cores number of cores
#' @param seed seed number, default=1
#' @import glmnet
#' @import randomForest
#' @import parallel 
#' @importFrom stats predict
#' @return A list containing predicted.y, predicted.prob.y and aloom.probs
#' @examples
#'
#' library(randomForest)
#' x1 <- matrix(rnorm(100 * 20), 100, 20)
#' x2 <- matrix(rnorm(30 * 20), 30, 20)
#' y1 <- as.factor(sample(c("POS","NEG"), 100, replace = TRUE))
#' vnames <- paste0("V",seq(20))
#' colnames(x1) <- vnames
#' colnames(x2) <- vnames
#' rownames(x1) <- paste0("train",seq(nrow(x1)))
#' rownames(x2) <- paste0("test",seq(nrow(x2)))
#' model.params <- list(ntree=100)
#' fit <- aloom(x1,y1,x2,method="rf",model.params)
#' 
#' @export
aloom <- function(train.x,train.y,test.x,method,model.params,mc.cores=1,seed=1)
{
  if(!is.matrix(train.x)) stop ("train.x should be a matrix")
  np=dim(train.x)
  if(is.null(np)|(np[2]<=1)) stop ("train.x should be a matrix with 2 or more columns")
  if(any(is.na(train.x))) stop ("train.x has missing values; consider using makeX() to impute them")
  if(is.null(colnames(train.x))) stop ("train.x should have column names") 
  if(is.null(rownames(train.x))) stop ("train.x should have row names") 

  if(!is.factor(train.y)) stop ("train.y should be a factor")
  if(nlevels(train.y) != 2) stop ("train.y should be a factor with 2 levels")

  if(!is.matrix(test.x)) stop ("test.x should be a matrix")
  np=dim(test.x)
  if(is.null(np)|(np[2]<=1)) stop ("test.x should be a matrix with 2 or more columns")
  if(any(is.na(test.x))) stop ("test.x has missing values; consider using makeX() to impute them")
  if(is.null(colnames(test.x))) stop ("test.x should have column names") 
  if(is.null(rownames(test.x))) stop ("test.x should have row names") 

  if(ncol(train.x) != ncol(test.x)) stop ("train.x and test.x don't have the same number of columns") 
  if(!identical(sort(colnames(train.x)), sort(colnames(test.x)))) stop ("train.x and test.x have different column names") 

  if (!method%in%c("rf","glmnet")) stop ("method should be either 'rf' or 'glmnet'")
 
  list.results <- fit.method(method,train.x,train.y,test.x,model.params,seed=seed)

  predictedY   <- list.results$predictedY 
  predictedProbabilityY <- list.results$predictedProbabilityY 

  learning.size <- nrow(train.x)
  result_lapply <- parallel::mclapply(1:learning.size, 
                                      function(k) fit.method(method,train.x,train.y,test.x,model.params,k,seed=seed), mc.cores=mc.cores)

  allPredictions   <-matrix(nrow=nrow(test.x),ncol=learning.size)
  names.of.removed <- character(learning.size)
  for (k in 1:learning.size)
  {
    k.result <- result_lapply[[k]]
    allPredictions[,k]    <- k.result$predictedProbabilityY
    names.of.removed[k]   <- k.result$name.missing
  } 

  rownames(allPredictions) <- rownames(test.x)
  colnames(allPredictions) <- names.of.removed  

  list(predicted.y=predictedY, predicted.prob.y=predictedProbabilityY, aloom.probs=allPredictions)
}
