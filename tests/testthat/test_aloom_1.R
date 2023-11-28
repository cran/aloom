test_that(
  "check that all expected outputs are created",
  {
    library(randomForest)
    library(aloom)

    x1 <- matrix(rnorm(100 * 20), 100, 20)
    x2 <- matrix(rnorm(30 * 20), 30, 20)
    y1 <- as.factor(sample(c("POS","NEG"), 100, replace = TRUE))
    model.params <- list(ntree=100)
    fit <- aloom(x1,y1,x2,method="rf",model.params)
    expect_true(is.list(fit))
  }
)
