context("BFpack Multivariate One Sample T-Test")

options <-
    list(
      bfType = "fractional",
      ciLevel = 0.95,
      estimatesTable = FALSE,
      complement = TRUE,
      interactionTerms = list(),
      iterations = 5000,
      logScale = FALSE,
      manualHypotheses = list(
        list(
          name = "contNormal>contGamma",
          priorProbManual = "1/2"
        )
      ),
      muValue = 0,
      plots = FALSE,
      priorProbComplement = "1/2",
      runAnalysisBox = TRUE,
      seed = 100,
      specificationTable = FALSE,
      standardHypotheses = list(
        list(priorProb = "1/2", value = "H0: mu = test value"),
        list(priorProb = "1/2", value = "H1: mu ≠ test value")
      ),
      testValues = list(
        list(testValue = 1, value = "contNormal"),
        list(testValue = 0, value = "contGamma")
      ),
      variables = c("contNormal", "contGamma")
    )



set.seed(1)
results <- jaspTools::runAnalysis("bfpackTTestMultiSample", "debug.csv", options)


test_that("Manual hypotheses legend table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_legendTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("contNormal&gt;contGamma", "H1", "complement", "H2"))
})

test_that("Posterior probabilities when testing individual parameters table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_parameterTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(2.58215607191321e-27, 1))
})

test_that("Evidence matrix (BFs) table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_matrixTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 9.23271288425205e-20, "H1", 10831052720221249536, 1, "H2"
                                 ))
})

test_that("Posterior model probability table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_postTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("H1", 9.23271288425205e-20, "H2", 1))
})
