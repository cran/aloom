library(randomForest)
library(aloom)
library(glmnet)

set.seed(1)
x1 <- matrix(rnorm(100 * 20), 100, 20)
x2 <- matrix(rnorm(30 * 20), 30, 20)
y1 <- as.factor(sample(c("POS","NEG"), 100, replace = TRUE))
vnames <- paste0("V",seq(20))
colnames(x1) <- vnames
colnames(x2) <- vnames
rownames(x1) <- paste0("train",seq(nrow(x1)))
rownames(x2) <- paste0("test",seq(nrow(x2)))

test_that(
  "check that all expected outputs are created",
  {
    model.params <- list(ntree=100)
    fit <- aloom(x1,y1,x2,method="rf",model.params)
    expect_true(is.list(fit))
    expect_true(is.matrix(fit$aloom.probs))
  }
)
test_that(
  "check that expected output has correct rownames and colnames",
  {
    model.params <- list(ntree=100)
    fit <- aloom(x1,y1,x2,method="rf",model.params)
    expect_true(identical(sort(colnames(fit$aloom.probs)), sort(rownames(x1))))
    expect_true(identical(sort(rownames(fit$aloom.probs)), sort(rownames(x2))))
  }
)
test_that(
  "test fit.RF internal function",
  {
    set.seed(1)
    fit.RF <- randomForest::randomForest(x1,
                                         y1,
                                         ntree=100)
    predictedY <- as.vector(predict(fit.RF,x2,type="response"))
    predictedProbs   <- predict(fit.RF,x2,type="prob")
    predictedProbabilityY<-predictedProbs[,2]
    model.params <- list(ntree=100)
    list.results <- aloom:::fit.rf(x1,y1,x2,model.params,seed=1)
    expect_equal(predictedY,list.results$predictedY) 
    expect_equal(predictedProbabilityY,list.results$predictedProbabilityY) 
  }
)
test_that(
  "test fit.glmnet internal function",
  {
    set.seed(1)
    fit.glmnet <- glmnet::glmnet(x1,
                                 y1,
                                 family="binomial")

    lambda <- fit.glmnet$lambda
    alpha  <- 1
    idx    <- 50
    selected.lambda <- lambda[idx]

    predictedY.all.lambda <- predict(fit.glmnet,as.matrix(x2),type="class")
    predictedY <- predictedY.all.lambda[,idx]
    predictedProbabilityY.all.lambda <- predict(fit.glmnet,as.matrix(x2),type="response")
    predictedProbabilityY <- predictedProbabilityY.all.lambda[,idx]

    model.params <- list(alpha=alpha, lambda=lambda, selected.lambda=selected.lambda)

    list.results <- aloom:::fit.glmnet(x1,y1,x2,model.params)
    expect_equal(predictedY,list.results$predictedY) 
    expect_equal(predictedProbabilityY,list.results$predictedProbabilityY) 
  }
)
