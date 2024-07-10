context("BFpack Paired Samples T-Test")

options <-
    list(
      bfType = "fractional",
      ciLevel = 0.95,
      coefficientsTable = TRUE,
      complement = TRUE,
      interactionTerms = list(),
      iterations = 5000,
      logScale = FALSE,
      muValue = 0,
      manualHypotheses = list(
        list(name = ".1<mu<2", priorProbManual = "1/2"),
        list(name = "mu=2", priorProbManual = "1/2")
      ),
      pairs = list(c("contNormal", "contGamma")),
      plots = TRUE,
      priorProbComplement = "1/2",
      runAnalysisBox = TRUE,
      seed = 100,
      specificationTable = FALSE,
      standardHypotheses = list(
        list(priorProb = "1", value = "H0: delta = 0 "),
        list(priorProb = ".5", value = "H1: delta < 0 "),
        list(priorProb = "1", value = "H2: delta > 0 ")
      )
    )



set.seed(1)
results <- jaspTools::runAnalysis("bfpackTTestPairedSamples", "debug.csv", options)


test_that("Coefficients table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_coefContainer"]][["collection"]][["bfpackContainer_coefContainer_coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("mu", -1.84207727029991, -2.22170938375, -2.22170938375, -2.60134149720009
                                 ))
})

test_that("Manual hypotheses legend table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_legendTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(".1&lt;mu&lt;2", "H1", "mu=2", "H2", "complement", "H3"))
})

test_that("Posterior probabilities when testing individual parameters table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_parameterTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("mu", 9.8559713484495e-18, 1.19550749899889e-19, 1))
})

test_that("Posterior probabilities plot matches", {
  plotName <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_plotContainer"]][["collection"]][["bfpackContainer_plotContainer_postPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "posterior-probabilities")
})

test_that("Prior probabilities plot matches", {
  plotName <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_plotContainer"]][["collection"]][["bfpackContainer_plotContainer_priorPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "prior-probabilities")
})

test_that("Evidence matrix (BFs) table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_matrixTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 64347.4223936863, 1.01119137876107e-32, "H1", 1.55406380364681e-05,
                                      1, 1.57145592029229e-37, "H2", 9.88932482024538e+31, 6.36352561396695e+36,
                                      1, "H3"))
})

test_that("Posterior model probability table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_postTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("H1", 1.01119137876107e-32, "H2", 1.57145592029229e-37, "H3",
                                      1))
})
