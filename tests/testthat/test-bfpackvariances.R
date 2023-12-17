context("Bfpack Variances")

options <-
  list(
    bfType = "fractional",
    ciLevel = 0.95,
    coefficientsTable = FALSE,
    complement = TRUE,
    groupingVariable = "test",
    interactionTerms = list(),
    iterations = 5000,
    logScale = FALSE,
    manualHypotheses = list(
      list(name = "female = male > non", priorProbManual = "1/2"),
      list(name = "female < male < non", priorProbManual = "1/2")
    ),
    plots = FALSE,
    priorProbComplement = "1/2",
    runAnalysisBox = TRUE,
    seed = 100,
    specificationTable = FALSE,
    standardHypotheses = list(
      list(priorProb = "1/2", value = "H0: delta = 0 "),
      list(priorProb = "1/2", value = "H1: delta ≠ 0 ")
    ),
    variables = "libido"
  )


set.seed(1)

results <- jaspTools::runAnalysis("bfpackVariances", "viagra.csv", options)


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
