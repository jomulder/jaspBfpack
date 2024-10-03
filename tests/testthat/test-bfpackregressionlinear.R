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
    list(hypothesisText = "adverts = airplay = attract", priorProbManual = "2", includeHypothesis = TRUE, value = "#"),
    list(hypothesisText = "adverts < airplay < attract", priorProbManual = "1", includeHypothesis = TRUE, value = "#2")
  ),
  plots = TRUE,
  priorProbComplement = "1",
  seed = 100,
  specificationTable = TRUE,
  priorProbStandard = "1",
  priorProbStandard2 = "0",
  priorProbStandard3 = "1"
)

set.seed(1)
results <- jaspTools::runAnalysis("bfpackRegressionLinear", testthat::test_path("sales.csv"), options, makeTests = F)


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
                                 list(8.3411790544916e-27, 8.3411790544916e-27, 1, 3.42054780082398e-27,
                                      0.00121418043670193, 1, 1.01276964269916e-29, 1, "H1", 4.87563959275001,
                                      1, 4.87563959275001, 0.999700292826758, 1, 0.204862916411396,
                                      1, 0.998837746341636, "H2", 0.0014617022427358, 1, 0.0014617022427358,
                                      0.000299707173241719, 1, 0.795137083588604, 1, 0.00116225365836387,
                                      "H3"))
})



# multivariate regression
options <- list(
    bfType = "fractional",
    ciLevel = 0.95,
    complement = TRUE,
    covariates = "contcor1",
    dependent = c("contNormal", "contGamma"),
    estimatesTable = TRUE,
    interactionTerms = list(),
    iterations = 5000,
    logScale = FALSE,
    plots = FALSE,
    manualHypotheses = list(
      list(
        hypothesisText = "contcor1_on_contNormal > contcor1_on_contGamma",
        includeHypothesis = TRUE,
        priorProbManual = "1",
        value = "#"
      )
    ),
    muValue = 0,
    priorProbComplement = "1",
    priorProbStandard = "2",
    priorProbStandard2 = "1",
    priorProbStandard3 = "1",
    seed = 100,
    specificationTable = FALSE
  )

set.seed(1)
results <- jaspTools::runAnalysis("bfpackRegressionLinear", "debug.csv", options, makeTests = T)


test_that("Estimates table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_coefContainer"]][["collection"]][["bfpackContainer_coefContainer_estimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("(Intercept)_on_contNormal", -0.407335959001421, -0.197600108422593,
                                      -0.197600108422593, 0.0121357421562356, "contcor1_on_contNormal",
                                      -0.0395987290269023, 0.168444232730491, 0.168444232730491, 0.376487194487884,
                                      "(Intercept)_on_contGamma", 1.74153509468766, 2.04542009414516,
                                      2.04542009414516, 2.34930509360267, "contcor1_on_contGamma",
                                      -0.538532327714582, -0.237100144583728, -0.237100144583728,
                                      0.064332038547127))
})

test_that("Manual hypotheses legend table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_legendTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("contcor1_on_contNormal&gt;contcor1_on_contGamma", "H1", "complement",
                                      "H2"))
})

test_that("Posterior probabilities when testing individual parameters table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_parameterTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("(Intercept)_on_contNormal", 0.714009012694178, 0.0116265829017178,
                                      0.274364404404104, "contcor1_on_contNormal", 0.791625334215502,
                                      0.194294505154204, 0.0140801606302942, "(Intercept)_on_contGamma",
                                      3.43006409771489e-21, 1, 2.0407452947433e-23, "contcor1_on_contGamma",
                                      0.802403724408777, 0.0145072795906795, 0.183088996000543))
})

test_that("Evidence matrix (BFs) table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_matrixTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 45.1496246486603, "H1", 0.0221485783720612, 1, "H2"))
})

test_that("Posterior model probability table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_postTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("H1", 0.978331351389896, "H2", 0.0216686486101037))
})
