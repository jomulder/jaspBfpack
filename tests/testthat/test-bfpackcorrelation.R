context("BFpack Correlation")

options <- list(
    bfType = "fractional",
    ciLevel = 0.95,
    estimatesTable = FALSE,
    complement = TRUE,
    standardHypothesisBfTable = TRUE,
    logScale = TRUE,
    manualHypotheses = list(
      list(
        hypothesisText = "sales_with_adverts = airplay_with_adverts = attract_with_adverts",
        priorProbManual = ".5",
        includeHypothesis = TRUE,
        value = "#"
      ),
      list(
        hypothesisText = "sales_with_adverts > airplay_with_adverts > attract_with_adverts",
        priorProbManual = ".5",
        includeHypothesis = TRUE,
        value = "#2"
      )
    ),
    manualPlots = TRUE,
    priorProbComplement = "10",
    seed = 100,
    specificationTable = TRUE,
    priorProbStandard = "1",
    priorProbStandard2 = "1",
    priorProbStandard3 = "1",
    variables = c("adverts", "sales", "airplay", "attract"),
    variables.types = c("scale", "scale", "scale", "ordinal"),
    groupingVariable = "",
    posteriorPlot = FALSE,
    traceplot = FALSE
)

set.seed(1)
results <- jaspTools::runAnalysis("bfpackCorrelation", testthat::test_path("sales.csv"), options, makeTests = F)

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
                                 list("sales_with_adverts", 1.65201700207213e-17, 1, 2.00666312501266e-19,
                                      "airplay_with_adverts", 0.574597617593414, 0.3915365627687,
                                      0.033865819637886, "attract_with_adverts", 0.663207200920249,
                                      0.292721148440096, 0.0440716506396544, "airplay_with_sales",
                                      1.15442012027571e-19, 1, 1.31256936240155e-21, "attract_with_sales",
                                      5.99050833635835e-05, 0.999938779265236, 1.31565140052065e-06,
                                      "attract_with_airplay", 0.121545899945412, 0.873913703396449,
                                      0.00454039665813929))
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
                                 list(0, -43.5185990619478, -41.5226811470543, "H1", 43.5185990619478,
                                      0, 1.99591791489352, "H2", 41.5226811470543, -1.99591791489352,
                                      0, "H3"))
})

test_that("Posterior model probability table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_postTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("H1", 3.38710922534774e-20, "H2", 0.268977922238213, "H3", 0.731022077761787
                                 ))
})

test_that("Specification table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_specTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(4.49881577874035e-19, 4.49881577874035e-19, 1, 3.38710922534774e-20,
                                      0.334621459725668, 1, 1.50540030291896e-19, 1, "H1", 3.57261027085372,
                                      1, 3.57261027085372, 0.268977922238213, 1, 0.166666666666667,
                                      1, 0.595435045142286, "H2", 0.485477945829257, 1, 0.485477945829257,
                                      0.731022077761787, 1, 0.833333333333333, 1, 0.404564954857714,
                                      "H3"))
})

test_that("BFs when testing standard hypotheses table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_stdBfTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-37.9488024334818, -42.3594963806197, 0.693147180559945, "sales_with_adverts",
                                      0.993781677754371, -1.83748208711095, 0.610190579978968, "airplay_with_adverts",
                                      1.37076674042817, -1.34050398807247, 0.552899722868173, "attract_with_adverts",
                                      -42.912371428795, -47.3891532106837, 0.693147180559945, "airplay_with_sales",
                                      -9.02954210483927, -12.8479715659353, 0.69314586482886, "attract_with_sales",
                                      -1.2847245088449, -4.57200209974315, 0.687965155633888, "attract_with_airplay"
                                 ))
})



# check plots
options <- list(
  bfType = "fractional",
  ciLevel = 0.95,
  complement = TRUE,
  covariates = list(
    types = list(),
    value = list()
  ),
  estimatesTable = FALSE,
  groupingVariable = "",
  interactionTerms = list(),
  iterations = 5000,
  logScale = FALSE,
  manualHypotheses = list(
    list(
      hypothesisText = "",
      includeHypothesis = FALSE,
      priorProbManual = "1",
      value = "#"
    )
  ),
  manualPlots = FALSE,
  muValue = 0,
  plotHeight = 320,
  plotWidth = 480,
  posteriorPlot = TRUE,
  priorProbComplement = "1",
  priorProbInteractionNonZero = "1",
  priorProbInteractionZero = "1",
  priorProbMainNonZero = "1",
  priorProbMainZero = "1",
  priorProbStandard = "2",
  priorProbStandard2 = "1",
  priorProbStandard3 = "1",
  seed = 100,
  specificationTable = FALSE,
  standardHypothesisBfTable = FALSE,
  traceplot = TRUE,
  variables =  c("contNormal", "contGamma", "contExpon")
  )

set.seed(1)
results <- jaspTools::runAnalysis("bfpackCorrelation", "debug.csv", options, makeTests = F)

test_that("contGamma_with_contNormal plot matches", {
  plotName <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_posteriorPlotContainer"]][["collection"]][["bfpackContainer_posteriorPlotContainer_cor1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "contgamma_with_contnormal_post")
})

test_that("contExpon_with_contNormal plot matches", {
  plotName <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_posteriorPlotContainer"]][["collection"]][["bfpackContainer_posteriorPlotContainer_cor2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "contexpon_with_contnormal_post")
})

test_that("contExpon_with_contGamma plot matches", {
  plotName <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_posteriorPlotContainer"]][["collection"]][["bfpackContainer_posteriorPlotContainer_cor3"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "contexpon_with_contgamma_post")
})

test_that("contGamma_with_contNormal plot matches", {
  plotName <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_traceplotContainer"]][["collection"]][["bfpackContainer_traceplotContainer_cor1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "contgamma_with_contnormal_trace")
})

test_that("contExpon_with_contNormal plot matches", {
  plotName <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_traceplotContainer"]][["collection"]][["bfpackContainer_traceplotContainer_cor2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "contexpon_with_contnormal_trace")
})

test_that("contExpon_with_contGamma plot matches", {
  plotName <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_traceplotContainer"]][["collection"]][["bfpackContainer_traceplotContainer_cor3"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "contexpon_with_contgamma_trace")
})
