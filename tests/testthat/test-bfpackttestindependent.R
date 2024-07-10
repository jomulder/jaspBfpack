context("BFpack Independent T-Test")

options <-
  list(
    bfType = "fractional",
    ciLevel = 0.95,
    coefficientsTable = FALSE,
    complement = FALSE,
    groupingVariable = "facExperim",
    interactionTerms = list(),
    iterations = 5000,
    muValue = 0,
    logScale = TRUE,
    manualHypotheses = list(
      list(name = "difference > 3", priorProbManual = "1/2"),
      list(name = "difference < 0", priorProbManual = "1/2"),
      list(name = "difference < 3", priorProbManual = "1/2")
    ),

    plots = TRUE,
    priorProbComplement = "1/2",
    runAnalysisBox = TRUE,
    seed = 100,
    specificationTable = TRUE,
    standardHypotheses = list(
      list(priorProb = "1/3", value = "H0: delta = 0 "),
      list(priorProb = "1/3", value = "H1: delta < 0 "),
      list(priorProb = "1/3", value = "H2: delta > 0 ")
    ),
    variables = "contNormal"
  )


set.seed(1)
results <- jaspTools::runAnalysis("bfpackTTestIndependentSamples", "debug.csv", options)

test_that("Manual hypotheses legend table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_legendTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("difference&gt;3", "H1", "difference&lt;0", "H2", "difference&lt;3",
                                      "H3"))
})

test_that("Posterior probabilities when testing individual parameters table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_parameterTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("difference", 0.850906132369181, 0.0562612136566447, 0.0928326539741745
                                 ))
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
                                 list(0, "-<unicode>", "-<unicode>", "H1", "<unicode>", 0, 0.00526177151092475,
                                      "H2", "<unicode>", -0.00526177151092475, 0, "H3"))
})

test_that("Posterior model probability table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_postTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("H1", 0, "H2", 0.501315439842767, "H3", 0.498684560157233))
})

test_that("Specification table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_specTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("<unicode>", 1, 0, 1, 0.188267672651421, 1, 0, "H1", "<unicode>",
                                      1, 1.23843243033821, 1, 0.511289453654182, 1, 0.633197440695244,
                                      "H2", "NaN", 1, 1.23193319559709, 1, 0.811732327348579, 1, 1,
                                      "H3"))
})
