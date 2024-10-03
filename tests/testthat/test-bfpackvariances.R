context("BFpack Variances")

options <-
  list(
    bfType = "fractional",
    ciLevel = 0.95,
    estimatesTable = TRUE,
    complement = TRUE,
    groupingVariable = "test",
    interactionTerms = list(),
    iterations = 5000,
    logScale = FALSE,
    manualHypotheses = list(
      list(hypothesisText = "female = male > non", priorProbManual = "1/2", includeHypothesis = TRUE, value = "#"),
      list(hypothesisText = "female < male < non", priorProbManual = "1/2", includeHypothesis = TRUE, value = "#2")
    ),
    plots = FALSE,
    priorProbComplement = "1/2",
    seed = 100,
    specificationTable = FALSE,
    priorProbStandard = "1",
    priorProbStandard2 = "1",
    variables = "libido"
  )

dt <- read.csv(testthat::test_path("viagra.csv"))
dt$test <- as.factor(dt$test)
set.seed(1)
results <- jaspTools::runAnalysis("bfpackVariances", dt, options)


test_that("Manual hypotheses legend table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_legendTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("female=male&gt;non", "H1", "female&lt;male&lt;non", "H2", "complement",
                                      "H3"))
})

test_that("Posterior probabilities when testing individual parameters table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_parameterTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.881165791227818, 0.118834208772182))
})

test_that("Evidence matrix (BFs) table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_matrixTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 0.486946986570694, 1.70163189264454, "H1", 2.05361164064791,
                                      1, 3.49449106283256, "H2", 0.587671166909009, 0.286164703820825,
                                      1, "H3"))
})

test_that("Posterior model probability table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_postTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("H1", 0.274628490246529, "H2", 0.563980264423834, "H3", 0.161391245329637
                                 ))
})

test_that("Coefficients table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_coefContainer"]][["collection"]][["bfpackContainer_coefContainer_estimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("female", 0.928946417051052, 2.975, 2.34406595810759, 8.80245652745594,
                                      "male", 1.45744037811216, 4.25925925925926, 3.47972940855522,
                                      11.7241799647805, "non", 2.30077046690569, 5.36923076923077,
                                      4.73463680355297, 12.1922993366245))
})
