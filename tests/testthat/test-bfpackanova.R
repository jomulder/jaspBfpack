context("BFpack ANOVA")

options <- list(
  estimatesTable = FALSE,
  complement = TRUE,
  logScale = FALSE,
  covariates = "facFive",
  dependent = "contNormal",
  fixedFactors = "facGender",
  bfType = "fractional",
  interactionTerms = list(
    list(includeInteractionEffect = TRUE, value = "facGender:facFive")
  ),
  manualHypotheses = list(
    list(
      hypothesisText = "facFive = facGenderf = facGenderm:facFive",
      priorProbManual = "1/2",
      includeHypothesis = TRUE,
      value = "#"
    )
  ),
  plots = TRUE,
  priorProbComplement = "1/2",
  seed = 100,
  specificationTable = FALSE,
  priorProbStandard = "1",
  priorProbStandard2 = "1",
  priorProbStandard3 = "1"
)

set.seed(1)
results <- jaspTools::runAnalysis("bfpackAnova", "debug.csv", options)


test_that("Posterior probabilities for interaction effects table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_iaEffectsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("facGender:facFive", 0.063066057560794, 0.936933942439206))
})

test_that("Manual hypotheses legend table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_legendTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("facFive=facGenderf=facGenderm___X___facFive", "H1", "complement",
                                      "H2"))
})

test_that("Posterior probabilities for main effects table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_mainEffectsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("facGender", 0.0577609635764609, 0.942239036423539))
})

test_that("Posterior probabilities when testing individual parameters table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_parameterTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("facGenderf", 0.836954162230138, 0.0181996831083619, 0.1448461546615,
                                      "facGenderm", 0.793428104163608, 0.0488111923686672, 0.157760703467725,
                                      "facFive", 0.917195072206702, 0.0442606436758533, 0.0385442841174443,
                                      "facGenderm:facFive", 0.882430932670274, 0.0845753731158548,
                                      0.0329936942138711))
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
                                 list(1, 8.64684909014128, "H1", 0.115649063557748, 1, "H2"))
})

test_that("Posterior model probability table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_postTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("H1", 0.896339209761044, "H2", 0.103660790238956))
})


# manova
options <- list(
  bfType = "fractional",
  ciLevel = 0.95,
  estimatesTable = FALSE,
  complement = TRUE,
  covariates = list(),
  dependent = c("contNormal", "contGamma"),
  fixedFactors = "facExperim",
  interactionTerms = list(),
  iterations = 5000,
  logScale = FALSE,
  manualHypotheses = list(
    list(hypothesisText = "", priorProbManual = "1", includeHypothesis = FALSE, value = "#")
  ),
  plotHeight = 320,
  plotWidth = 480,
  plots = TRUE,
  priorProbComplement = "1/2",
  seed = 100,
  specificationTable = FALSE,
  priorProbStandard = "1",
  priorProbStandard2 = "1",
  priorProbStandard3 = "1"
)

set.seed(1)
results <- jaspTools::runAnalysis("bfpackAnova", "debug.csv", options)

test_that("Posterior probabilities for main effects table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_mainEffectsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("facExperim", 0.0144051897194446, 0.985594810280556))
})

test_that("Posterior probabilities when testing individual parameters table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_parameterTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("facExperimcontrol_on_contNormal", 0.691275154361235, 0.0256428704717866,
                                      0.283081975166979, "facExperimexperimental_on_contNormal", 0.795715927651634,
                                      0.0360350225399549, 0.168249049808412, "facExperimcontrol_on_contGamma",
                                      5.60001167204669e-12, 0.999999999994324, 7.5881868854101e-14,
                                      "facExperimexperimental_on_contGamma", 3.47872566274389e-14,
                                      0.999999999999965, 4.5233346175913e-16))
})
