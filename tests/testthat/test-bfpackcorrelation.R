context("Bfpack Correlation")

options <- list(
    bfType = "fractional",
    ciLevel = 0.95,
    coefficientsTable = FALSE,
    complement = TRUE,
    logScale = TRUE,
    manualHypotheses = list(
      list(
        name = "sales_with_adverts = airplay_with_adverts = attract_with_adverts",
        priorProbManual = "1/2"
      ),
      list(
        name = "sales_with_adverts > airplay_with_adverts > attract_with_adverts",
        priorProbManual = "1/2"
      )
    ),
    plots = TRUE,
    priorProbComplement = "10",
    runAnalysisBox = TRUE,
    seed = 100,
    specificationTable = TRUE,
    standardHypotheses = list(
      list(priorProb = "1/3", value = "H0: rho = 0 "),
      list(priorProb = "1/3", value = "H1: rho < 0 "),
      list(priorProb = "1/3", value = "H2: rho > 0 ")
    ),
    variables = c("adverts", "sales", "airplay", "attract")
)

set.seed(1)
results <- jaspTools::runAnalysis("bfpackCorrelation", "sales.csv", options)

test_that("Manual hypotheses legend table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_legendTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("sales_with_adverts=airplay_with_adverts=attract_with_adverts",
                                      "H1", "sales_with_adverts&gt;airplay_with_adverts&gt;attract_with_adverts",
                                      "H2", "complement", "H3"))
})

test_that("Posterior probabilities when testing individual parameters table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_parameterTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("sales_with_adverts", 6.95682073873284e-19, 1, 7.72749359788928e-21,
                                      "airplay_with_adverts", 0.613708178203333, 0.350693123974362,
                                      0.0355986978223052, "attract_with_adverts", 0.681886473207107,
                                      0.274353007399127, 0.0437605193937661, "airplay_with_sales",
                                      3.0843992202434e-19, 1, 3.52085617526518e-21, "attract_with_sales",
                                      2.67708661201684e-05, 0.999972693491486, 5.35642394072382e-07,
                                      "attract_with_airplay", 0.136679140709066, 0.858340106763328,
                                      0.00498075252760626))
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
                                 list(0, -42.0424039108462, -40.1260354383868, "H1", 42.0424039108462,
                                      0, 1.91636847245935, "H2", 40.1260354383868, -1.91636847245935,
                                      0, "H3"))
})

test_that("Posterior model probability table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_postTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("H1", 1.39768879890665e-19, "H2", 0.253626430277925, "H3", 0.746373569722075
                                 ))
})

test_that("Specification table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_specTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.90498754505064e-18, 1.90498754505064e-18, 1, 0.334621459725668,
                                      1, 6.37449713084063e-19, 1, "H1", 3.45681521632752, 1, 3.45681521632752,
                                      1, 0.166666666666667, 1, 0.57613586938792, "H2", 0.508636956734496,
                                      1, 0.508636956734496, 1, 0.833333333333333, 1, 0.42386413061208,
                                      "H3"))
})
