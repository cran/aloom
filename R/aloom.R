#' All Leave-One-Out Models
#'
#' Creates a predictive model for a training set, as well as
#' all leave-one-out predictive models. Produces predictions of 
#' all models (original and all leave one-out) for a test set.
#'
#' @param train.x input matrix, of dimension nobs x nvars; each row is an observation
#' vector. 
#' @param train.y response variable. Expected to be binary factor. 
#' @param test.x Matrix of new values for \code{train.x} at which predictions are to be
#' made. Must be a matrix.
#' @param method name of the model. Currently allowed values are rf and glmnet
#' @param model.params list of model parameters
#' @import glmnet
#' @import randomForest
#' @importFrom stats predict
#' @return A list containing predictedY, predictedProbabilityY and allPredictions
#' @examples
#'
#' library(randomForest)
#' x1 <- matrix(rnorm(100 * 20), 100, 20)
#' x2 <- matrix(rnorm(30 * 20), 30, 20)
#' y1 <- as.factor(sample(c("POS","NEG"), 100, replace = TRUE))
#' model.params <- list(ntree=100)
#' fit <- aloom(x1,y1,x2,method="rf",model.params)
#' 
#' @export
aloom <- function(train.x,train.y,test.x,method,model.params)
{
  if (method == "rf")
  {
      fit.RF <- randomForest::randomForest(train.x,
                                           train.y,
                                           ntree=model.params$ntree)
      predictedY <- as.vector(predict(fit.RF,test.x,type="response"))
      predictedProbs   <- predict(fit.RF,test.x,type="prob")
      predictedProbabilityY<-predictedProbs[,2]
  } else if (method == "glmnet")
  {
    fit.glmnet <- glmnet::glmnet(train.x,
                                 train.y,
                                 family="binomial",
                                 lambda=model.params$lambda,
                                 alpha=0)

    predictedY <- as.vector(predict(fit.glmnet,test.x,type="class"))
    predictedProbabilityY <- as.vector(predict(fit.glmnet,test.x,type="response"))
  }

  learning.size <- nrow(train.x)
  allPredictions <-matrix(nrow=nrow(test.x),ncol=learning.size)
  for (k in 1:learning.size)
  {
    train.x.minus.1 <- train.x[-k,]
    train.y.minus.1 <- as.factor(train.y[-k])
    if (method == "rf")
    {
      fit.NA.RF   <- randomForest::randomForest(train.x.minus.1,
                                                train.y.minus.1,
                                                ntree=model.params$ntree)
      predictedProbs   <- predict(fit.NA.RF,test.x,type="prob")
      predictedAloomProbabilityY<-predictedProbs[,2]
    } else if (method == "glmnet")
    {
      fit.NA.glmnet <- glmnet::glmnet(train.x.minus.1,
                                      train.y.minus.1,
                                      family="binomial",
                                      lambda=model.params$lambda,
                                      alpha=0)

      predictedAloomProbabilityY <- as.vector(predict(fit.NA.glmnet,test.x,type="response"))
    }
      
    allPredictions[,k] <- predictedAloomProbabilityY
  }

  list(predictedY=predictedY, predictedProbabilityY=predictedProbabilityY, allPredictions=allPredictions)
}
