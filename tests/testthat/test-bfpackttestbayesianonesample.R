context("Bfpack One Sample T-Test")

# ==================================================================================================

# library(bfpack)
# data("sesamesim")
# set.seed(100)
# tt <- t_test(sesamesim$age)
# bfpackResult <- bfpack(tt,"x=51;x>51;x<51", fraction = 1)

options <- jaspTools::analysisOptions("BfpackTTestBayesianOneSample")
options$variables <- list("age")
options$testValue <- 51
options$descriptives <- TRUE
options$bayesFactorPlot <- TRUE
options$descriptivesPlot <- TRUE
options$hypothesis <- "equalBiggerSmaller"
options$bayesFactorType <- "BF01"
set.seed(1)
results <- jaspTools::runAnalysis("BfpackTTestBayesianOneSample", "sesame.csv", options)


test_that("age plot matches", {
	plotName <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_descriptivesPlots"]][["collection"]][["bfpackContainer_descriptivesPlots_age"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "age")
})

test_that("Descriptive Statistics table results match", {
	table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_descriptivesTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(240, 50.2165034595135, 51.0125, 6.29171019316781, 0.406128146621706,
			 51.8084965404866, "age"))
})

test_that("Bfpack One Sample T-test table results match", {
	table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("", 15.1135035681881, 15.8743731825382, "age", 0.885613607789322,
			 0.0585975054555462, 0.0557888867551316, "H0: Equal", "H1: Bigger",
			 "H2: Smaller"))
})

test_that("age plot matches", {
	plotName <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_posteriorProbabilityPlot"]][["collection"]][["bfpackContainer_posteriorProbabilityPlot_age"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "age-2")
})

# ==================================================================================================

# library(bfpack)
# data("sesamesim")
# set.seed(900)
# tt <- t_test(sesamesim$postnumb)
# bfpackResult <- bfpack(tt,"x=30",fraction = 4)

test_that("Bfpack One Sample T-test table results match", {
	options <- jaspTools::analysisOptions("BfpackTTestBayesianOneSample")
	options$variables <- list("postnumb")
	options$testValue <- 30
	options$fraction <- 4
	options$hypothesis <- "equalNotEqual"
	options$bayesFactorType <- "BF01"
	set.seed(1)
	results <- jaspTools::runAnalysis("BfpackTTestBayesianOneSample", "sesame.csv", options)
	table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(6.16007603449518, "", "postnumb", "H0: Equal", "H1: Not equal",
			 0.860336678663426, 0.139663321336574))
})

# ==================================================================================================

# library(bfpack)
# data("sesamesim")
# set.seed(900)
# tt <- t_test(sesamesim$postnumb)
# bfpackResult <- bfpack(tt,"x=30;x>30",fraction = 4)

test_that("Bfpack One Sample T-test table results match", {
	options <- jaspTools::analysisOptions("BfpackTTestBayesianOneSample")
	options$variables <- list("postnumb")
	options$testValue <- 30
	options$fraction <- 4
	options$hypothesis <- "equalBigger"
	options$bayesFactorType <- "BF01"
	set.seed(1)
	results <- jaspTools::runAnalysis("BfpackTTestBayesianOneSample", "sesame.csv", options)
	table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(12.3576651279192, "", "postnumb", "H0: Equal", "H1: Bigger", 0.925136617034224,
			 0.0748633829657756))
})

# ==================================================================================================

# library(bfpack)
# data("sesamesim")
# set.seed(900)
# tt <- t_test(sesamesim$postnumb)
# bfpackResult <- bfpack(tt,"x=30;x<30", fraction = 4)

test_that("Bfpack One Sample T-test table results match", {
	options <- jaspTools::analysisOptions("BfpackTTestBayesianOneSample")
	options$variables <- list("postnumb")
	options$testValue <- 30
	options$fraction <- 4
	options$hypothesis <- "equalSmaller"
	options$bayesFactorType <- "BF01"
	set.seed(1)
	results <- jaspTools::runAnalysis("BfpackTTestBayesianOneSample", "sesame.csv", options)
	table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(4.10256609199393, "", "postnumb", "H0: Equal", "H1: Smaller",
			 0.804020176912744, 0.195979823087256))
})

# ==================================================================================================

# library(bfpack)
# data("sesamesim")
# set.seed(900)
# tt <- t_test(sesamesim$postnumb)
# bfpackResult <- bfpack(tt,"x>30;x<30", fraction = 4)

test_that("Bfpack One Sample T-test table results match", {
	options <- jaspTools::analysisOptions("BfpackTTestBayesianOneSample")
	options$variables <- list("postnumb")
	options$testValue <- 30
	options$fraction <- 4
	options$hypothesis <- "biggerSmaller"
	options$bayesFactorType <- "BF01"
	set.seed(1)
	results <- jaspTools::runAnalysis("BfpackTTestBayesianOneSample", "sesame.csv", options)
	table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.331985536873399, "", "postnumb", "H1: Bigger", "H2: Smaller",
			 0.249241097356565, 0.750758902643435))
})

# ==================================================================================================

# library(bfpack)
# data("sesamesim")
# set.seed(900)
# tt <- t_test(sesamesim$postnumb)
# bfpackResult <- bfpack(tt,"x=30;x>30;x<30", fraction = 4)

test_that("Bfpack One Sample T-test table results match", {
	options <- jaspTools::analysisOptions("BfpackTTestBayesianOneSample")
	options$variables <- list("postnumb")
	options$testValue <- 30
	options$fraction <- 4
	options$hypothesis <- "equalBiggerSmaller"
	options$bayesFactorType <- "BF01"
	set.seed(1)
	results <- jaspTools::runAnalysis("BfpackTTestBayesianOneSample", "sesame.csv", options)
	table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("", 12.3576651279192, 4.10256609199393, "postnumb", 0.754904244575,
			 0.061087935039563, 0.184007820385436, "H0: Equal", "H1: Bigger",
			 "H2: Smaller"))
})

# ==================================================================================================