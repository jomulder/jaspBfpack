context("Bfpack One Sample T-Test")

options <-
  list(
    bfType = "fractional",
    ciLevel = 0.1,
    coefficientsTable = TRUE,
    complement = TRUE,
    interactionTerms = list(),
    iterations = 5000,
    logScale = TRUE,
    manualHypotheses = list(list(name = "mu<.5", priorProbManual = "1/2")),
    plots = FALSE,
    priorProbComplement = "1/2",
    runAnalysisBox = TRUE,
    seed = 100,
    specificationTable = TRUE,
    standardHypotheses = list(
      list(priorProb = "1/3", value = "H0: mu = 0 "),
      list(priorProb = "1/3", value = "H1: mu < 0 "),
      list(priorProb = "1/3", value = "H2: mu > 0 ")
    ),
    variables = "contNormal"
  )


set.seed(1)
results <- jaspTools::runAnalysis("bfpackTTestOneSample", "debug.csv", options)


test_that("Coefficients table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_coefContainer"]][["collection"]][["bfpackContainer_coefContainer_coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("mu", -0.202082917611105, -0.18874858754, -0.18874858754, -0.175414257468895
                                 ))
})

test_that("Manual hypotheses legend table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_legendTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("mu&lt;.5", "H1", "complement", "H2"))
})

test_that("Posterior probabilities when testing individual parameters table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_parameterTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("mu", 0.592794450958136, 0.019627260756729, 0.387578288285135
                                 ))
})

test_that("Evidence matrix (BFs) table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_matrixTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 19.4908635074636, "H1", -19.4908635074636, 0, "H2"))
})

test_that("Posterior model probability table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_postTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("H1", 0.999999996570542, "H2", 3.42945832529895e-09))
})

test_that("Specification table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_specTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.46121344999626, 1, 1.46121344999626, 1, 0.684362711293717, 1,
                                      0.999999998418288, "H1", 5.01117064831409e-09, 1, 5.01117064831409e-09,
                                      1, 0.315637288706283, 1, 1.58171231667837e-09, "H2"))
})
