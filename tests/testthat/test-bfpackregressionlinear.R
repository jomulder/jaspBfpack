context("BFpack Regression")

options <- list(
  bfType = "adjusted",
  ciLevel = 0.95,
  estimatesTable = TRUE,
  complement = TRUE,
  covariates = c("adverts", "airplay", "attract"),
  dependent = "sales",
  logScale = FALSE,
  manualHypotheses = list(
    list(name = "adverts = airplay = attract", priorProbManual = "2"),
    list(name = "adverts < airplay < attract", priorProbManual = "1")
  ),
  plots = TRUE,
  priorProbComplement = "1",
  runAnalysisBox = TRUE,
  seed = 100,
  specificationTable = TRUE,
  standardHypotheses = list(
    list(priorProb = "1/3", value = "H0: beta = 0 "),
    list(priorProb = "0", value = "H1: beta < 0 "),
    list(priorProb = "1/3", value = "H2: beta > 0 ")
  )
)

set.seed(1)
results <- jaspTools::runAnalysis("bfpackRegressionLinear", "sales.csv", options)


test_that("Coefficients table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_coefContainer"]][["collection"]][["bfpackContainer_coefContainer_estimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("(Intercept)", 7.60369294828812, -26.6129583616794, -26.6129583616794,
                                      -60.8296096716469, "adverts", 0.098537992105009, 0.0848848251534776,
                                      0.0848848251534776, 0.0712316582019461, "airplay", 3.91522847784673,
                                      3.36742517051031, 3.36742517051031, 2.81962186317388, "attract",
                                      15.8941182320123, 11.0863352045519, 11.0863352045519, 6.27855217709152
                                 ))
})

test_that("Manual hypotheses legend table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_legendTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("adverts=airplay=attract", "H1", "adverts&lt;airplay&lt;attract",
                                      "H2", "complement", "H3"))
})

test_that("Posterior probabilities when testing individual parameters table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_parameterTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("(Intercept)", 0.977120691736385, 0.0228793082636149, 0, "adverts",
                                      3.88184781917838e-24, 1, 0, "airplay", 1.01707718602688e-23,
                                      1, 0, "attract", 0.000448094169606805, 0.999551905830393, 0
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
                                 list(1, 1.71078663543365e-27, 5.70648303779357e-24, "H1", 5.84526427368612e+26,
                                      1, 3335.59014292106, "H2", 1.75239283701902e+23, 0.00029979702456018,
                                      1, "H3"))
})

test_that("Posterior model probability table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_postTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("H1", 3.42054780081425e-27, "H2", 0.999700292826759, "H3", 0.000299707173241403
                                 ))
})

test_that("Specification table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_specTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(8.34117905446789e-27, 8.34117905446789e-27, 1, 0.00121418043670202,
                                      1, 1.01276964269636e-29, 1, "H1", 4.87563959275002, 1, 4.87563959275002,
                                      1, 0.204862916411396, 1, 0.998837746341637, "H2", 0.00146170224273426,
                                      1, 0.00146170224273426, 1, 0.795137083588604, 1, 0.00116225365836264,
                                      "H3"))
})
