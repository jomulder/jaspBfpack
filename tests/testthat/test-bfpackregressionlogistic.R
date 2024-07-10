context("BFpack Logistic Regression")

options <-
  list(
    bfType = "adjusted",
    ciLevel = 0.95,
    coefficientsTable = TRUE,
    complement = TRUE,
    covariates = c("contNormal", "contcor1"),
    dependent = "contBinom",
    interactionTerms = list(
      list(
        includeInteractionEffect = TRUE,
        value = "contNormal:contcor1"
      )
    ),
    iterations = 5000,
    logScale = FALSE,
    manualHypotheses = list(
      list(
        name = "contNormal:contcor1>-.5",
        priorProbManual = "1/2"
      )
    ),
    plots = FALSE,
    priorProbComplement = "1/2",
    runAnalysisBox = TRUE,
    seed = 100,
    specificationTable = FALSE,
    standardHypotheses = list(
      list(priorProb = "1/3", value = "H0: beta = 0 "),
      list(priorProb = "1/3", value = "H1: beta < 0 "),
      list(priorProb = "1/3", value = "H2: beta > 0 ")
    )
  )


set.seed(1)
results <- jaspTools::runAnalysis("bfpackRegressionLogistic", "debug.csv", options)


test_that("Coefficients table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_coefContainer"]][["collection"]][["bfpackContainer_coefContainer_coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("(Intercept)", -0.727543388672256, -0.298554865863755, -0.298554865863755,
                                      0.130433656944746, "contNormal", -0.497926596040128, -0.0668697011221928,
                                      -0.0668697011221928, 0.364187193795742, "contcor1", -0.891660779964622,
                                      -0.436993245210254, -0.436993245210254, 0.0176742895441145,
                                      "contNormal:contcor1", -0.913811579222416, -0.414818496726082,
                                      -0.414818496726082, 0.0841745857702528))
})

test_that("Manual hypotheses legend table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_legendTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("contNormal___X___contcor1&gt;-.5", "H1", "complement", "H2"
                                 ))
})

test_that("Posterior probabilities when testing individual parameters table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_parameterTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("(Intercept)", 0.663546200745916, 0.0290284776394466, 0.307425321614638,
                                      "contNormal", 0.82681422421017, 0.065905062030784, 0.107280713759046,
                                      "contcor1", 0.45887687439431, 0.0161242542570045, 0.524998871348686,
                                      "contNormal:contcor1", 0.570052104657479, 0.0221941456995523,
                                      0.407753749642969))
})

test_that("Evidence matrix (BFs) table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_matrixTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 1.2494281298879, "H1", 0.800366164390519, 1, "H2"))
})

test_that("Posterior model probability table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_postTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("H1", 0.555, "H2", 0.445))
})
