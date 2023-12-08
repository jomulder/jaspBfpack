############ HELPERS ##########

# This is a temporary fix
# TODO: remove it when R will solve this problem!
gettextf <- function(fmt, ..., domain = NULL)  {
  return(sprintf(gettext(fmt, domain = domain), ...))
}

# Clean the input for the order constraints
.bfpackCleanModelInput <- function(input) {
  return(gsub("\n+", ";", input))
}

# Add the Bfpack citations
.bfpackGetCitations <- function() {
  citations <- c(
    "Mulder, J., Williams, D. R., Gu, X., Tomarken, A., Böing-Messing, F., Olsson-Collentine, A., Meijerink, M., Menke, J., Fox, J.-P., Hoijtink, H., Rosseel, Y., Wagenmakers, E.J., and van Lissa, C. (2021). BFpack: Flexible Bayes Factor Testing of Scientific Theories in R. Journal of Statistical Software.",
    "Mulder, J., van Lissa, C., Williams, D.R., Gu, X., Olsson-Collentine, A., Böing-Messing, F., Fox, J.-P., Menke, J., van Aert, R., et al. (2021). BFpack: Flexible Bayes Factor Testing of Scientific Expectations. (Version 1.0.0)",
    "Mulder, J., van Lissa, C., Williams, D.R., Gu, X., Olsson-Collentine, A., Böing-Messing, F., Fox, J.-P., Menke, J., van Aert, R., et al. (2021). BFpack: Flexible Bayes Factor Testing of Scientific Expectations. (Developmental version)"
  )
  return(citations)
}

# Create a container for the results
.bfpackCreateContainer <- function(jaspResults, deps) {
  if (is.null(jaspResults[["bfpackContainer"]])) {
    jaspResults[["bfpackContainer"]] <- createJaspContainer()
    jaspResults[["bfpackContainer"]]$dependOn(options = deps)
    jaspResults[["bfpackContainer"]]$position <- 1
  }
  return(jaspResults[["bfpackContainer"]])
}

# Read the data set
.bfpackReadDataset <- function(options, type, dataset) {
  numerics <- switch(type,
    "onesampleTTest" = options[["variables"]],
    "pairedTTest" = unique(unlist(options[["pairs"]])),
    "independentTTest" = unlist(options[["variables"]]),
    "anova" = options[["dependent"]],
    "ancova" = c(options[["dependent"]], unlist(options[["covariates"]])),
    "regression" = c(options[["dependent"]], unlist(options[["covariates"]])),
    "correlation" = options[["variables"]]
    )
  numerics <- numerics[numerics != ""]
  factors <- switch(type,
    "onesampleTTest" = NULL,
    "pairedTTest" = NULL,
    "independentTTest" = options[["groupingVariable"]],
    "anova" = options[["fixedFactors"]],
    "ancova" = options[["fixedFactors"]],
    "regression" = NULL
    )
  factors <- factors[factors != ""]
  vars <- c(numerics, factors)

  if (is.null(dataset)) {
    trydata <- .readDataSetToEnd(columns.as.numeric = numerics, columns.as.factor = factors)
    missing <- names(which(apply(trydata, 2, function(x) {
      any(is.na(x))
    })))
    if (type == "onesampleTTest") { # For the one sample t test we do not remove the NA's listwise
      dataset <- .readDataSetToEnd(columns.as.numeric = numerics, columns.as.factor = factors)
    } else {
      dataset <- .readDataSetToEnd(columns.as.numeric = numerics, columns.as.factor = factors, exclude.na.listwise = vars)
    }
    if ((type == "anova" || type == "ancova") && options[["fixedFactors"]] != "") {
      if (any(grepl(pattern = " ", x = levels(dataset[, options[["fixedFactors"]]])))) {
        jaspBase:::.quitAnalysis(gettext("Bfpack does not accept factor levels that contain spaces. Please remove the spaces from your factor levels to continue."))
      }
    }
  } else {
    dataset <- .vdf(dataset, columns.as.numeric = numerics, columns.as.factor = factors)
  }


  readList <- list()
  readList[["dataset"]] <- dataset
  readList[["missing"]] <- missing
  return(readList)
}


####### CHECKS #######


# Check if current options allow for analysis
.bfpackOptionsReady <- function(options, type, dataset = NULL) {
  ready <- switch(type,
    "independentTTest" = length(options[["variables"]][options[["variables"]] != ""] > 0) && options[["groupingVariable"]] != "",
    "pairedTTest" = !is.null(unlist(options[["pairs"]])),
    "onesampleTTest" = length(options[["variables"]][options[["variables"]] != ""] > 0),
    "anova" = options[["fixedFactors"]] != "" && options[["dependent"]] != "",
    "ancova" = options[["dependent"]] != "" && options[["fixedFactors"]] != "" && !is.null(unlist(options[["covariates"]])),
    "regression" = (options[["dependent"]] != "" && all(unlist(options[["covariates"]]) != "") && !is.null(unlist(options[["covariates"]]))),
    "correlation" = length(options[["variables"]]) > 1)
  return(ready)
}

# Check if current data allow for analysis
.bfpackDataReady <- function(dataset, options, type) {
  if (type == "independentTTest") {
    factors <- options[["groupingVariable"]]
    factors <- factors[factors != ""]
    if (length(factors) > 0) {
      .hasErrors(dataset,
        type = "factorLevels",
        factorLevels.target = factors, factorLevels.amount = "!= 2",
        exitAnalysisIfErrors = TRUE
      )
    }
  }

  numerics <- switch(type,
    "onesampleTTest" = options[["variables"]],
    "pairedTTest" = unique(unlist(options[["pairs"]])),
    "independentTTest" = unlist(options[["variables"]]),
    "anova" = options[["dependent"]],
    "ancova" = c(options[["dependent"]], unlist(options[["covariates"]])),
    "regression" = c(options[["dependent"]], unlist(options[["covariates"]])),
    "correlation" = options[["variables"]])

  numerics <- numerics[numerics != ""]

  if (length(numerics) > 0) {
    .hasErrors(dataset,
      type = c("infinity", "variance", "observations"),
      all.target = numerics, observations.amount = "< 3",
      exitAnalysisIfErrors = TRUE
    )
  }
}

###### COMPUTE RESULTS ######
# perform the parameter estimation and also return the estimates to the JASP GUI
.bfpackGetParameterEstimates <- function(dataList, options, bfpackContainer, ready, type, jaspResults) {

  if (!is.null(bfpackContainer[["estimatesState"]])) {
    return()
  }

  if (!ready) return()

  dataset <- dataList[["dataset"]]
  missing <- dataList[["missing"]]

  if (bfpackContainer$getError()) {
    return()
  }

  callString <- switch(type,
                       "independentTTest" = "BFpack:::get_estimates.t_test",
                       "pairedTTest" = "BFpack:::get_estimates.t_test",
                       "onesampleTTest" = "BFpack:::get_estimates.t_test",
                       "anova" = "bain::get_estimates",
                       "ancova" = "bain::get_estimates",
                       "regression" = "BFpack:::get_estimates.lm",
                       "correlation" = "BFpack:::get_estimates.cor_test")

  # estimate the correlation
  if (type == "correlation") {
    dataset <- dataList[["dataset"]]
    # decode the colnames otherwise bfpack fails when trying to match hypotheses and estimate names
    colnames(dataset) <- decodeColNames(colnames(dataset))
    result <- try(BFpack::cor_test(dataset))
  }

  if (isTryError(result)) {
    bfpackContainer$setError(gettext("The parameter estimation failed. Error message: %1$s", jaspBase::.extractErrorMessage(result)))
  }

  # save in jaspResults, which is where bfpackContainer is stored.
  # this way we do not have to estimate the parameters twice
  estimatesState <- createJaspState(result)
  estimatesState$dependOn("seed") # are there any new dependencies not already covered in the container?
  bfpackContainer[["estimatesState"]] <- estimatesState

  # the estimate names for the JASP GUI
  estimateNames <- eval(parse(text = callString))(result)
  estimateNames <- as.list(names(estimateNames$estimate))
  namesForQml <- createJaspQmlSource("estimateNamesForQml", estimateNames)
  namesForQml$dependOn("variables")
  # apparently the source only works directly with jaspResults
  jaspResults[["estimateNamesForQml"]] <- namesForQml

}

# compute the posteriors and BFs
# TODO: manual and standard prior
.bfpackComputeResults <- function(dataList, options, bfpackContainer, ready, type) {

  if (!is.null(bfpackContainer[["resultsContainer"]][["resultsState"]])) return()
  if (!ready) return()

  # create a container because both the results and the tables depending on them have the same dependencies
  # start with common deps, and then do the switch
  deps <- c("complement", "logScale", "manualHypotheses", "priorProbManual", "priorProb", "priorProbComplement", "seed",
            "descriptivesTable")
  depsAddOn <- switch(type,
                      "independentTTest" = c("variables", "bayesFactorType", "hypothesis"),
                      "pairedTTest" = c("pairs", "hypothesis", "bayesFactorType"),
                      "onesampleTTest" = c("variables", "hypothesis", "bayesFactorType"),
                      "anova" = "",
                      "ancova" = "",
                      "regression" = "standardized",
                      "correlation" = "")

  resultsContainer <- createJaspContainer()
  resultsContainer$dependOn(c(deps, depsAddOn))

  if (!options[["runAnalysisBox"]]) {
    syncText <- createJaspHtml(text = gettext("<b>Check the 'Run Analysis' box to run the analysis</b>"))
    bfpackContainer[["syncText"]] <- syncText
    syncText$dependOn("runAnalysisBox")
    syncText$position <- 0.01
    return()
  }

  if (!is.null(bfpackContainer[["estimatesState"]])) {
    estimates <- bfpackContainer[["estimatesState"]]$object

    # standard hypotheses priors
    standHypos <- options[["standardHypotheses"]]
    # those standard priors should not be allowed to be empty
    standPrior <- sapply(standHypos, function(x) eval(parse(text = x[["priorProb"]])))

    # check if there are manual hypotheses
    manualHyp <- sapply(options[["manualHypotheses"]], function(x) x[["name"]])
    manualPrior <- sapply(options[["manualHypotheses"]], function(x) x[["priorProbManual"]])

    # strip down the hypos
    # replace with numbers because that is also what qml does if a hypothesis string is deleted...
    # might be the following is analysis-specific
    manualHyp <- gsub("...", "1", manualHyp, fixed = TRUE)

    # keep the hypotheses that are specified
    strs <- which(is.na(as.numeric(manualHyp)))
    manualHyp <- manualHyp[strs]
    manualPrior <- manualPrior[strs]
    # at least one manual hypo specified?
    if (length(manualHyp) > 0) {
      manualHyp <- paste(manualHyp, collapse = ";")
      if (options[["complement"]]) manualPrior <- c(manualPrior, options[["priorProbComplement"]])
      # convert the prior character values to numeric:
      manualPrior <- sapply(manualPrior, function(x) eval(parse(text=x)))
    } else {
      manualHyp <- NULL
      manualPrior <- NULL
    }

    # BF.type depends in the analysis as well
    # seems that except for the correlation and variance, all other models have the adjusted bftype option
    if (!is.null(options[["bfType"]])) {
      if (options[["bfType"]] == "adjusted") {
        bftype <- 2
      } else {
        bftype <- 1
      }
    }

    results <- try(BFpack::BF(estimates, hypothesis = manualHyp,
                             complement = options[["complement"]],
                             prior.hyp.conf = manualPrior,
                             prior.hyp.explo = standPrior,
                             log = options[["logScale"]],
                             BF.type = bftype))

    if (isTryError(results)) {
      bfpackContainer$setError(gettextf("Bfpack failed with the following error message: %1$s", jaspBase::.extractErrorMessage(results)))
    }

    # now saving the results

    resultsState <- createJaspState(results)
    resultsContainer[["resultsState"]] <- resultsState
  }

  bfpackContainer[["resultsContainer"]] <- resultsContainer

  return()
}


####### TABLES #######
# table for the posterior probabilities of the parameter estimates
.bfpackParameterTable <- function(options, bfpackContainer, type, position) {

  # the parameterTable does go into the outer container given it does not depend on the options for the
  # inner container
  if (!is.null(bfpackContainer[["parameterTable"]])) return()

  parameterTable <- createJaspTable(gettext("Parameter posterior probabilities"))
  parameterTable$dependOn("priorProb")
  parameterTable$position <- position

  parameterTable$addColumnInfo(name = "coefficient", type = "string", title = "")
  parameterTable$addColumnInfo(name = "equal", type = "number", title = gettext("Pr(=0)"))
  parameterTable$addColumnInfo(name = "smaller", type = "number", title = gettext("Pr(<0)"))
  parameterTable$addColumnInfo(name = "larger", type = "number", title = gettext("Pr(>0)"))

  bfpackContainer[["parameterTable"]] <- parameterTable

  parPhp <- bfpackContainer[["resultsContainer"]][["resultsState"]]$object$PHP_exploratory
  if (!is.null(parPhp)) {
    dtFill <- data.frame(coefficient = rownames(parPhp))
    dtFill[, c("equal", "smaller", "larger")] <- parPhp
    parameterTable$setData(dtFill)
  }

}


# Create a legend containing the order constrained hypotheses
.bfpackLegendTable <- function(options, type, bfpackContainer, position) {

  if (!is.null(bfpackContainer[["legendTable"]])) return()

  legendTable <- createJaspTable(gettext("Manual hypotheses legend"))

  legendTable$dependOn("manualHypotheses")
  legendTable$position <- position
  legendTable$addColumnInfo(name = "number", type = "string", title = "")
  legendTable$addColumnInfo(name = "hypothesis", type = "string", title = gettext("Hypothesis"))

  bfpackContainer[["legendTable"]] <- legendTable

  hypos <- bfpackContainer[["resultsContainer"]][["resultsState"]]$object$hypotheses

  if (!is.null(hypos)) {
    for (i in seq_len(length(hypos))) {
      row <- list(number = gettextf("H%i", i), hypothesis = hypos[i])
      legendTable$addRows(row)
    }
  }

  return()
}


# table for the posterior probabilities of the parameter estimates
.bfpackMatrixTable <- function(options, bfpackContainer, type, position) {

  if (!is.null(bfpackContainer[["resultsContainer"]][["matrixTable"]])) return()

  matrixTable <- createJaspTable(gettext("Evidence matrix (BFs)"))
  matrixTable$position <- position

  matrixTable$addColumnInfo(name = "hypothesis", title = "", type = "string")
  matrixTable$addColumnInfo(name = "H1", title = gettext("H1"), type = "number")

  bfpackContainer[["resultsContainer"]][["matrixTable"]] <- matrixTable

  bfMatrix <- bfpackContainer[["resultsContainer"]][["resultsState"]]$object$BFmatrix_confirmatory
  if (!is.null(bfMatrix)) {
    if (nrow(bfMatrix) > 1) {
      for (i in 2:nrow(bfMatrix)) {
        matrixTable$addColumnInfo(name = paste0("H", i), title = gettextf("H%i", i), type = "number")
      }
    }
    for (i in seq_len(nrow(bfMatrix))) {
      tmp <- list(hypothesis = gettextf("H%i", i))
      for (j in seq_len(ncol(bfMatrix))) {
        tmp[[paste0("H", j)]] <- bfMatrix[i, j]
      }
      row <- tmp
      matrixTable$addRows(row)
    }
  }

  return()
}



# create the table containing the posterior probabilities for the manual hypotheses
.bfpackPosteriorHypothesesTable <- function(options, bfpackContainer, type, position) {

  if (!is.null(bfpackContainer[["resultsContainer"]][["postTable"]])) return()

  postTable <- createJaspTable(gettext("Posterior model probability"))
  postTable$position <- position

  postTable$addColumnInfo(name = "hypothesis", title = "", type = "string")
  postTable$addColumnInfo(name = "prob", title = gettext("P(H|D)"), type = "number")

  bfpackContainer[["resultsContainer"]][["postTable"]] <- postTable

  php <- bfpackContainer[["resultsContainer"]][["resultsState"]]$object$PHP_confirmatory
  if (!is.null(php)) {
    for (i in seq_len(length(php))) {
      row <- list(hypothesis = gettextf("H%i", i), prob = php[i])
      postTable$addRows(row)
    }
  }

  return()

}


# specification table
.bfpackSpecificationTable <- function(options, bfpackContainer, type, position) {

  if (!is.null(bfpackContainer[["resultsContainer"]][["specTable"]]) ||
      !options[["specificationTable"]]) return()

  specTable <- createJaspTable(gettext("Specification table"))
  specTable$dependOn("specificationTable")
  specTable$position <- position

  specTable$addColumnInfo(name = "hypothesis", title = "", type = "string")
  specTable$addColumnInfo(name = "complex=", title = gettext("Equal-complex"), type = "number")
  specTable$addColumnInfo(name = "complex>", title = gettext("Order-complex"), type = "number")
  specTable$addColumnInfo(name = "fit=", title = gettext("Equal-fit"), type = "number")
  specTable$addColumnInfo(name = "fit>", title = gettext("Order-fit"), type = "number")
  specTable$addColumnInfo(name = "BF=", title = gettext("Equal-BF"), type = "number")
  specTable$addColumnInfo(name = "BF>", title = gettext("Order-BF"), type = "number")
  specTable$addColumnInfo(name = "BF", title = gettext("BF"), type = "number")

  bfpackContainer[["resultsContainer"]][["specTable"]] <- specTable

  spec <- bfpackContainer[["resultsContainer"]][["resultsState"]]$object$BFtable_confirmatory
  if (!is.null(spec)) {
    dtFill <- data.frame(hypothesis = rownames(spec))
    dtFill[, c("complex=", "complex>", "fit=", "fit>", "BF=", "BF>", "BF")] <- spec[, 1:7]
    specTable$setData(dtFill)

  }


  return()
}


.bfpackDescriptivesTable <- function(options, bfpackContainer, type, position) {
  if (!is.null(bfpackContainer[["resultsContainer"]][["descriptivesTable"]]) ||
      !options[["coefficientsTable"]]) return()

  descriptivesTable <- createJaspTable(gettext("Descriptives table"))
  descriptivesTable$dependOn(c("coefficientsTable", "ciLevel"))
  descriptivesTable$position <- position

  interval <- gettextf("%s%% CI", format(100 * options[["ciLevel"]], digits = 3, drop0trailing = TRUE))
  intervalLow <- gettextf("%s lower bound", interval)
  intervalUp <- gettextf("%s upper bound", interval)

  descriptivesTable$addColumnInfo(name = "coefficient", title = "", type = "string")
  descriptivesTable$addColumnInfo(name = "mean", title = gettext("Mean"), type = "number")
  descriptivesTable$addColumnInfo(name = "median", title = gettext("Median"), type = "number")
  descriptivesTable$addColumnInfo(name = "lower", title = intervalLow, type = "number")
  descriptivesTable$addColumnInfo(name = "upper", title = intervalUp, type = "number")

  bfpackContainer[["resultsContainer"]][["descriptivesTable"]] <- descriptivesTable

  outmodel <- bfpackContainer[["resultsContainer"]][["resultsState"]]$object$model
  if (!is.null(outmodel)) {

    dtFill <- data.frame(coefficient = rownames(outmodel$correstimates))
    # the regular bfpack output has the 95% interval bounds
    dtFill[, c("mean", "median", "lower", "upper")] <- outmodel$correstimates

    # we also let users choose the interval bounds
    if (options[["ciLevel"]] != .95) {
      # maybe this first element differs between analyses...???
      draws <- outmodel$corrdraws[[1]]
      bounds <- apply(draws, c(2, 3), function(x) {
        coda::HPDinterval(coda::as.mcmc(x), prob = options[["ciLevel"]])
        })
      boundsLow <- bounds[1, , ]
      boundsUp <- bounds[2, , ]
      # kind of hope this structure never changes so the elements are always in the correct order
      boundsLow <- boundsLow[lower.tri(boundsLow)]
      boundsUp <- boundsUp[lower.tri(boundsUp)]
      dtFill[, c("lower", "upper")] <- cbind(boundsLow, boundsUp)

    }
    descriptivesTable$setData(dtFill)
  }


  return()
}



####### PLOTS ########

# Create the posterior probability plots
.bfpackPosteriorProbabilityPlot <- function(dataset, options, bfpackContainer, ready, type, position) {
  if (!is.null(bfpackContainer[["posteriorProbabilityPlot"]]) || !options[["bayesFactorPlot"]]) {
    return()
  }

  if (type %in% c("independentTTest", "pairedTTest", "onesampleTTest")) {
    container <- createJaspContainer(gettext("Posterior Probabilities"))
    container$dependOn(options = c("variables", "bayesFactorPlot", "hypothesis", "pairs"))
    container$position <- position
    bfpackContainer[["posteriorProbabilityPlot"]] <- container
    if (!ready || bfpackContainer$getError()) {
      return()
    }
    analysisType <- switch(options[["hypothesis"]],
      "equalNotEqual" = 1,
      "equalBigger" = 2,
      "equalSmaller" = 3,
      "biggerSmaller" = 4,
      "equalBiggerSmaller" = 5
    )
    if (type == "onesampleTTest" || type == "independentTTest") {
      for (variable in options[["variables"]]) {
        if (is.null(container[[variable]])) {
          bfpackResult <- .bfpackGetGeneralTestResults(dataset, options, bfpackContainer, ready, type, variable = variable)
          plot <- createJaspPlot(plot = NULL, title = variable, height = 300, width = 400)
          plot$dependOn(optionContainsValue = list("variables" = variable))
          if (isTryError(bfpackResult) || any(is.nan(unlist(bfpackResult[["fit"]])))) {
            plot$setError(gettext("Plotting not possible: the results for this variable were not computed."))
          } else {
            p <- try({
              plot$plotObject <- .plotModelProbabilitiesTTests(bfpackResult, type = analysisType)
            })
            if (isTryError(p)) {
              plot$setError(gettextf("Plotting not possible: %1$s", jaspBase::.extractErrorMessage(p)))
            }
          }
          container[[variable]] <- plot
        }
      }
    } else if (type == "pairedTTest") {
      for (pair in options[["pairs"]]) {
        currentPair <- paste(pair, collapse = " - ")
        if (is.null(container[[currentPair]]) && pair[[2]] != "" && pair[[1]] != pair[[2]]) {
          bfpackResult <- .bfpackGetGeneralTestResults(dataset, options, bfpackContainer, ready, type, pair = pair)
          plot <- createJaspPlot(plot = NULL, title = currentPair, height = 300, width = 400)
          plot$dependOn(optionContainsValue = list("pairs" = pair))
          if (isTryError(bfpackResult) || any(is.nan(unlist(bfpackResult[["fit"]])))) {
            plot$setError(gettext("Plotting not possible: the results for this variable were not computed."))
          } else {
            p <- try({
              plot$plotObject <- .plotModelProbabilitiesTTests(bfpackResult, type = analysisType)
            })
            if (isTryError(p)) {
              plot$setError(gettextf("Plotting not possible: %1$s", jaspBase::.extractErrorMessage(p)))
            }
          }
          container[[currentPair]] <- plot
        }
      }
    }
  } else if (type %in% c("anova", "ancova", "regression", "sem")) {
    if (options[["model"]] == "" || !grepl(pattern = "\n", x = options[["model"]], fixed = TRUE)) {
      height <- 300
      width <- 600
    } else {
      height <- 400
      width <- 800
    }
    plot <- createJaspPlot(plot = NULL, title = gettext("Posterior Probabilities"), height = height, width = width)
    plot$dependOn(options = c("bayesFactorPlot", "standardized"))
    plot$position <- position
    bfpackContainer[["posteriorProbabilityPlot"]] <- plot
    if (!ready || bfpackContainer$getError() || (type == "sem" && options[["model"]] == "")) {
      return()
    }
    bfpackResult <- .bfpackGetGeneralTestResults(dataset, options, bfpackContainer, ready, type)
    if (is.null(bfpackResult) || any(is.nan(unlist(bfpackResult[["fit"]])))) {
      plot$setError(gettext("Plotting not possible: the results could not be computed."))
    } else {
      p <- try({
        plot$plotObject <- .suppressGrDevice(.plotModelProbabilitiesRegression(bfpackResult))
      })
      if (isTryError(p)) {
        plot$setError(gettextf("Plotting not possible: %1$s", jaspBase::.extractErrorMessage(p)))
      }
    }
  }
}

# Create the descriptive plot(s)
.bfpackDescriptivePlots <- function(dataset, options, bfpackContainer, ready, type, position) {
  if (!is.null(bfpackContainer[["descriptivesPlots"]]) || !options[["descriptivesPlot"]]) {
    return()
  }

  if (type %in% c("independentTTest", "pairedTTest", "onesampleTTest")) {
    container <- createJaspContainer(gettext("Descriptive Plots"))
    container$dependOn(options = c("variables", "pairs", "descriptivesPlot", "credibleInterval"))
    container$position <- position
    bfpackContainer[["descriptivesPlots"]] <- container
  } else {
    plot <- createJaspPlot(plot = NULL, title = ifelse(type == "anova", yes = gettext("Descriptives Plot"), no = gettext("Adjusted Means")))
    plot$dependOn(options = c("descriptivesPlot", "credibleInterval"))
    plot$position <- position
    bfpackContainer[["descriptivesPlots"]] <- plot
  }

  if (!ready || bfpackContainer$getError() || (type == "sem" && options[["model"]] == "")) {
    return()
  }

  if (type %in% c("independentTTest", "onesampleTTest")) {
    for (variable in options[["variables"]]) {
      if (is.null(bfpackContainer[["descriptivesPlots"]][[variable]])) {
        bfpackResult <- .bfpackGetGeneralTestResults(dataset, options, bfpackContainer, ready, type, variable = variable)
        if (isTryError(bfpackResult)) {
          container[[variable]] <- createJaspPlot(plot = NULL, title = variable)
          container[[variable]]$dependOn(optionContainsValue = list("variables" = variable))
          container[[variable]]$setError(gettext("Plotting not possible: the results for this variable were not computed."))
        } else {
          if (type == "onesampleTTest") {
            bfpackSummary <- summary(bfpackResult, ci = options[["credibleInterval"]])
            N <- bfpackSummary[["n"]]
            mu <- bfpackSummary[["Estimate"]]
            CiLower <- bfpackSummary[["lb"]]
            CiUpper <- bfpackSummary[["ub"]]
            yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(options[["testValue"]], CiLower, CiUpper), min.n = 4)
            plotData <- data.frame(v = variable, N = N, mean = mu, lowerCI = CiLower, upperCI = CiUpper, index = 1)
            p <- ggplot2::ggplot(plotData, ggplot2::aes(x = index, y = mean)) +
              ggplot2::geom_segment(mapping = ggplot2::aes(x = 0, xend = 2, y = options[["testValue"]], yend = options[["testValue"]]), linetype = "dashed", color = "darkgray") +
              ggplot2::geom_errorbar(mapping = ggplot2::aes(ymin = lowerCI, ymax = upperCI), width = 0.2, color = "black") +
              jaspGraphs::geom_point(size = 4) +
              ggplot2::scale_y_continuous(name = NULL, breaks = yBreaks, limits = range(yBreaks)) +
              ggplot2::scale_x_continuous(name = NULL, limits = c(0, 2)) +
              jaspGraphs::geom_rangeframe(sides = "l") +
              jaspGraphs::themeJaspRaw() +
              ggplot2::theme(axis.ticks.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank())
          } else if (type == "independentTTest") {
            bfpackSummary <- summary(bfpackResult, ci = options[["credibleInterval"]])
            levels <- levels(dataset[[options[["groupingVariable"]]]])
            N <- bfpackSummary[["n"]]
            mu <- bfpackSummary[["Estimate"]]
            CiLower <- bfpackSummary[["lb"]]
            CiUpper <- bfpackSummary[["ub"]]
            yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(CiLower, CiUpper), min.n = 4)
            plotData <- data.frame(v = levels, N = N, mean = mu, lowerCI = CiLower, upperCI = CiUpper, index = 1:length(levels))
            p <- ggplot2::ggplot(plotData, ggplot2::aes(x = index, y = mean)) +
              ggplot2::geom_errorbar(mapping = ggplot2::aes(ymin = lowerCI, ymax = upperCI), colour = "black", width = 0.2) +
              jaspGraphs::geom_line() +
              jaspGraphs::geom_point(size = 4) +
              ggplot2::scale_x_continuous(name = options[["groupingVariable"]], breaks = 1:length(levels), labels = levels) +
              ggplot2::scale_y_continuous(name = variable, breaks = yBreaks, limits = range(yBreaks)) +
              jaspGraphs::geom_rangeframe() +
              jaspGraphs::themeJaspRaw()
          }
          container[[variable]] <- createJaspPlot(plot = p, title = variable)
          container[[variable]]$dependOn(optionContainsValue = list("variables" = variable))
        }
      }
    }
  } else if (type == "pairedTTest") {
    for (pair in options[["pairs"]]) {
      currentPair <- paste(pair, collapse = " - ")

      if (is.null(bfpackContainer[["descriptivesPlots"]][[currentPair]]) && pair[[2]] != "" && pair[[1]] != pair[[2]]) {
        bfpackResult <- .bfpackGetGeneralTestResults(dataset, options, bfpackContainer, ready, type, pair = pair)
        if (isTryError(bfpackResult)) {
          container[[currentPair]] <- createJaspPlot(plot = NULL, title = currentPair)
          container[[currentPair]]$dependOn(optionContainsValue = list("variables" = currentPair))
          container[[currentPair]]$setError(gettext("Plotting not possible: the results for this variable were not computed."))
        } else {
          bfpackSummary <- summary(bfpackResult, ci = options[["credibleInterval"]])
          N <- bfpackSummary[["n"]]
          mu <- bfpackSummary[["Estimate"]]
          CiLower <- bfpackSummary[["lb"]]
          CiUpper <- bfpackSummary[["ub"]]
          yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, CiLower, CiUpper), min.n = 4)
          plotData <- data.frame(v = gettext("Difference"), N = N, mean = mu, lowerCI = CiLower, upperCI = CiUpper, index = 1)
          p <- ggplot2::ggplot(plotData, ggplot2::aes(x = index, y = mean)) +
            ggplot2::geom_segment(mapping = ggplot2::aes(x = 0, xend = 2, y = 0, yend = 0), linetype = "dashed", color = "darkgray") +
            ggplot2::geom_errorbar(mapping = ggplot2::aes(ymin = lowerCI, ymax = upperCI), color = "black", width = 0.2) +
            jaspGraphs::geom_point(size = 4) +
            ggplot2::scale_y_continuous(name = NULL, breaks = yBreaks, labels = yBreaks, limits = range(yBreaks)) +
            ggplot2::scale_x_continuous(name = NULL, breaks = 0:2) +
            jaspGraphs::geom_rangeframe(sides = "l") +
            jaspGraphs::themeJaspRaw() +
            ggplot2::theme(axis.ticks.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank())
          container[[currentPair]] <- createJaspPlot(plot = p, title = currentPair)
          container[[currentPair]]$dependOn(optionContainsValue = list("pairs" = pair))
        }
      }
    }
  } else if (type %in% c("anova", "ancova")) {
    groupCol <- dataset[, options[["fixedFactors"]]]
    varLevels <- levels(groupCol)
    bfpackResult <- bfpackContainer[["bfpackResult"]]$object
    bfpackSummary <- summary(bfpackResult, ci = options[["credibleInterval"]])

    if (type == "ancova") {
      bfpackSummary <- bfpackSummary[seq_along(varLevels), ]
    }

    variable <- bfpackSummary[["Parameter"]]
    N <- bfpackSummary[["n"]]
    mu <- bfpackSummary[["Estimate"]]
    CiLower <- bfpackSummary[["lb"]]
    CiUpper <- bfpackSummary[["ub"]]
    yBreaks <- jaspGraphs::getPrettyAxisBreaks(pretty(c(CiLower, CiUpper)), min.n = 4)
    plotData <- data.frame(v = variable, N = N, mean = mu, lowerCI = CiLower, upperCI = CiUpper, index = seq_along(variable))
    p <- ggplot2::ggplot(plotData, ggplot2::aes(x = index, y = mean)) +
      ggplot2::geom_errorbar(mapping = ggplot2::aes(ymin = lowerCI, ymax = upperCI), color = "black", width = 0.2) +
      jaspGraphs::geom_line() +
      jaspGraphs::geom_point(size = 4) +
      ggplot2::scale_y_continuous(name = options[["dependent"]], breaks = yBreaks, limits = range(yBreaks)) +
      ggplot2::scale_x_continuous(name = options[["fixedFactors"]], breaks = seq_along(varLevels), labels = varLevels) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw()
    plot$plotObject <- p
  }
}

.plotModelProbabilitiesTTests <- function(x, type) {
  if (type == 1 || type == 2 || type == 3) {
    labels <- gettext(c("H0", "H1"))
  } else if (type == 4) {
    labels <- gettext(c("H1", "H2"))
  } else if (type == 5) {
    labels <- gettext(c("H0", "H1", "H2"))
  }

  if (type == 1) {
    postProb <- na.omit(x$fit$PMPb)
  } else {
    postProb <- na.omit(x$fit$PMPa)
  }

  plotData <- data.frame(x = labels, y = postProb)
  yBreaks <- cumsum(rev(postProb)) - rev(postProb) / 2
  p <- ggplot2::ggplot(data = plotData, mapping = ggplot2::aes(x = "", y = y, fill = x)) +
    ggplot2::geom_bar(stat = "identity", width = 1e10, color = "black", size = 1) +
    ggplot2::coord_polar(theta = "y", direction = -1) +
    ggplot2::scale_x_discrete(name = NULL) +
    ggplot2::scale_y_continuous(name = NULL, breaks = yBreaks, labels = rev(plotData[["x"]])) +
    ggplot2::scale_fill_brewer(palette = "Set1") +
    jaspGraphs::themeJaspRaw(legend.position = "none") +
    ggplot2::theme(axis.ticks.y = ggplot2::element_blank())

  return(p)
}

.plotModelProbabilitiesRegression <- function(x) {
  postProbA <- na.omit(x$fit$PMPa)
  postProbB <- na.omit(x$fit$PMPb)
  postProbC <- na.omit(x$fit$PMPc)
  numH <- length(postProbB) - 1
  labels <- paste(gettext("H"), 1:numH, sep = "")
  plotDataA <- data.frame(x = labels, y = postProbA)
  plotDataB <- data.frame(x = c(labels, gettext("Hu")), y = postProbB)
  yBreaksA <- cumsum(rev(postProbA)) - rev(postProbA) / 2
  yBreaksB <- cumsum(rev(postProbB)) - rev(postProbB) / 2
  yBreaksC <- cumsum(rev(postProbC)) - rev(postProbC) / 2
  yLabelsA <- rev(labels)
  yLabelsB <- rev(c(labels, gettext("Hu")))
  yLabelsC <- rev(c(labels, gettext("Hc")))
  complexity <- x[["fit"]]$Com[length(x[["fit"]]$Com)]
  plotMat <- list()

  if (numH > 1) {
    p1 <- ggplot2::ggplot(data = plotDataA, mapping = ggplot2::aes(x = "", y = y, fill = x)) +
      ggplot2::geom_bar(stat = "identity", width = 1e10, color = "black", size = 1) +
      ggplot2::coord_polar(theta = "y", direction = -1) +
      ggplot2::labs(title = gettext("Excluding Hu and Hc"), size = 30) +
      ggplot2::scale_x_discrete(name = NULL) +
      ggplot2::scale_y_continuous(name = NULL, breaks = yBreaksA, labels = yLabelsA) +
      ggplot2::scale_fill_brewer(palette = "Set1") +
      jaspGraphs::themeJaspRaw(legend.position = "none") +
      ggplot2::theme(axis.ticks.y = ggplot2::element_blank())
    plotMat[["p1"]] <- p1
  }

  p2 <- ggplot2::ggplot(data = plotDataB, mapping = ggplot2::aes(x = "", y = y, fill = x)) +
    ggplot2::geom_bar(stat = "identity", width = 1e10, color = "black", size = 1) +
    ggplot2::coord_polar(theta = "y", direction = -1) +
    ggplot2::scale_x_discrete(name = NULL) +
    ggplot2::scale_y_continuous(name = NULL, breaks = yBreaksB, labels = yLabelsB) +
    ggplot2::scale_fill_brewer(palette = "Set1") +
    jaspGraphs::themeJaspRaw(legend.position = "none") +
    ggplot2::theme(axis.ticks.y = ggplot2::element_blank())
  plotMat[["p2"]] <- p2

  if (!is.na(complexity) && complexity >= .05) {
    plotDataC <- data.frame(x = c(labels, gettext("Hc")), y = postProbC)
    p3 <- ggplot2::ggplot(data = plotDataC, mapping = ggplot2::aes(x = "", y = y, fill = x)) +
      ggplot2::geom_bar(stat = "identity", width = 1e10, color = "black", size = 1) +
      ggplot2::geom_col() +
      ggplot2::coord_polar(theta = "y", direction = -1) +
      ggplot2::labs(title = gettext("Including Hc"), size = 30) +
      ggplot2::scale_x_discrete(name = NULL) +
      ggplot2::scale_y_continuous(name = NULL, breaks = yBreaksC, labels = yLabelsC) +
      ggplot2::scale_fill_brewer(palette = "Set1") +
      jaspGraphs::themeJaspRaw(legend.position = "none") +
      ggplot2::theme(axis.ticks.y = ggplot2::element_blank())
    plotMat[["p3"]] <- p3
  }

  if (!is.null(plotMat[["p1"]]) || !is.null(plotMat[["p3"]])) {
    plotMat[["p2"]] <- plotMat[["p2"]] + ggplot2::labs(title = gettext("Including Hu"), size = 30)
  }

  p <- jaspGraphs::ggMatrixPlot(plotList = plotMat, layout = matrix(c(seq_along(plotMat)), nrow = 1))
  return(p)
}



#### MAYBE ####
# Call to bain for 't_test' objects (Welch t-test, Paired t-test, One sample t-test)
.bfpackTTestRaw <- function(options, x, y = NULL, nu = 0, type = 1, paired = FALSE) {

  if (is.null(y) && !paired) {
    x <- x[!is.na(x)] # Here we remove the missing values per dependent variable
    test <- bain::t_test(x = x)
    hypothesis <- switch(type,
                         "1" = paste0("x=", nu),
                         "2" = paste0("x=", nu, "; x>", nu),
                         "3" = paste0("x=", nu, "; x<", nu),
                         "4" = paste0("x>", nu, "; x<", nu),
                         "5" = paste0("x=", nu, "; x>", nu, "; x<", nu)
    )
  }

  if (!is.null(y) && !paired) {
    test <- bain::t_test(x = x, y = y, paired = FALSE, var.equal = FALSE)
    hypothesis <- switch(type,
                         "1" = "difference=0",
                         "2" = "difference=0; difference>0",
                         "3" = "difference=0; difference<0",
                         "4" = "difference>0; difference<0",
                         "5" = "difference=0; difference>0; difference<0"
    )
  }

  if (!is.null(y) && paired) {
    test <- bain::t_test(x = x, y = y, paired = TRUE)
    hypothesis <- switch(type,
                         "1" = "difference=0",
                         "2" = "difference=0;difference>0",
                         "3" = "difference=0;difference<0",
                         "4" = "difference>0;difference<0",
                         "5" = "difference=0;difference>0;difference<0"
    )
  }

  bfpackResult <- BFpack::BF(x = test, hypothesis = hypothesis)

  return(bfpackResult)
}

# Call to bfpack for 'lm' objects (ANOVA, ANCOVA, Regression)
.bfpackRegressionRaw <- function(dataset, options, type) {
  dependent <- options[["dependent"]]
  standardized <- options[["standardized"]]
  hypothesis <- NULL
  if (options[["model"]] != "") {
    hypothesis <- encodeColNames(.bfpackCleanModelInput(options[["model"]]))
  }

  if (type == "regression") {
    ncov <- length(options[["covariates"]])
    covariates <- paste0(options[["covariates"]], collapse = "+")
    formula <- as.formula(paste0(dependent, "~", covariates))
  } else if (type == "anova" || type == "ancova") {
    grouping <- options[["fixedFactors"]]
    dataset[, options[["fixedFactors"]]] <- as.factor(dataset[, options[["fixedFactors"]]])

    if (type == "anova") {
      formula <- as.formula(paste0(dependent, "~", grouping, "-1"))
    } else {
      ncov <- length(options[["covariates"]])
      covariates <- paste0(options[["covariates"]], collapse = "+")
      formula <- as.formula(paste0(dependent, "~", grouping, "+", covariates, "-1"))
    }
  }

  if (type == "regression") {
    # I cannot find out why bfpack won't work in regression with stats::lm(formula = formula, data = dataset)
    # Error: object of type 'closure' is not subsettable
    args <- list(formula = as.formula(paste0(dependent, "~", covariates)), data = dataset)
    fit <- do.call(stats::lm, args)
  } else {
    fit <- stats::lm(formula = formula, data = dataset)
  }

  if (is.null(hypothesis)) {
    if (type == "regression") {
      hypothesis <- paste0(paste0(names(stats::coef(fit))[-1], "=0"), collapse = " & ")
    } else if (type == "anova") {
      hypothesis <- paste0(names(stats::coef(fit)), collapse = "=")
    } else if (type == "ancova") {
      hypothesis <- names(stats::coef(fit))
      hypothesis <- hypothesis[1:(length(hypothesis) - ncov)]
      hypothesis <- paste0(hypothesis, collapse = "=")
    }
  }

  bfpackResult <- BFpack::BF(x = fit, hypothesis = hypothesis, standardize = standardized)
  return(bfpackResult)
}

.bfpackGetGeneralTestResults <- function(dataset, options, bfpackContainer, ready, type, variable = NULL, pair = NULL, testType = NULL) {
  set.seed(options[["seed"]])

  if (type %in% c("onesampleTTest", "independentTTest")) {
    result <- .bfpackGetTTestResults(dataset, options, bfpackContainer, ready, type, variable, testType)
  } else if (type == "pairedTTest") {
    result <- .bfpackGetPairedTTestResults(dataset, options, bfpackContainer, ready, type, pair, testType)
  } else if (type %in% c("anova", "ancova", "regression", "sem")) {
    result <- .bfpackGetRegressionResults(dataset, options, bfpackContainer, ready, type)
  }

  return(result)
}

.bfpackGetTTestResults <- function(dataset, options, bfpackContainer, ready, type, variable, testType) {
  if (!is.null(bfpackContainer[[variable]])) {
    return(bfpackContainer[[variable]]$object)
  }

  variableData <- dataset[[variable]]
  testValue <- format(options[["testValue"]], scientific = FALSE)

  p <- try({
    if (type == "onesampleTTest") {
      bfpackResult <- .bfpackTTestRaw(options, x = variableData, nu = testValue, type = testType)
    } else if (type == "independentTTest") {
      levels <- levels(dataset[[options[["groupingVariable"]]]])
      if (length(levels) != 2) {
        g1 <- "1"
        g2 <- "2"
      } else {
        g1 <- levels[1]
        g2 <- levels[2]
      }
      subDataSet <- dataset[, c(variable, options[["groupingVariable"]])]
      group1 <- subDataSet[subDataSet[[options[["groupingVariable"]]]] == g1, variable]
      group2 <- subDataSet[subDataSet[[options[["groupingVariable"]]]] == g2, variable]
      bfpackResult <- .bfpackTTestRaw(options, x = group1, y = group2, type = testType)
    }
  })

  if (isTryError(p)) {
    return(p)
  }

  bfpackContainer[[variable]] <- createJaspState(bfpackResult, dependencies = c("testValue", "hypothesis"))
  bfpackContainer[[variable]]$dependOn(optionContainsValue = list("variables" = variable))
  return(bfpackContainer[[variable]]$object)
}

.bfpackGetPairedTTestResults <- function(dataset, options, bfpackContainer, ready, type, pair, testType) {
  currentPair <- paste(pair, collapse = " - ")

  if (!is.null(bfpackContainer[[currentPair]])) {
    return(bfpackContainer[[currentPair]]$object)
  }

  if (pair[[2]] != "" && pair[[1]] != pair[[2]] && pair[[1]] != "") {
    subDataSet <- subset(dataset, select = c(pair[[1]], pair[[2]]))
    c1 <- subDataSet[[pair[[1]]]]
    c2 <- subDataSet[[pair[[2]]]]

    p <- try({
      bfpackResult <- .bfpackTTestRaw(options, x = c1, y = c2, type = testType, paired = TRUE)
    })

    if (isTryError(p)) {
      return(p)
    }

    bfpackContainer[[currentPair]] <- createJaspState(bfpackResult, dependencies = "hypothesis")
    bfpackContainer[[currentPair]]$dependOn(optionContainsValue = list("pairs" = pair))
    return(bfpackContainer[[currentPair]]$object)
  }
}

.bfpackGetRegressionResults <- function(dataset, options, bfpackContainer, ready, type) {

  if (!is.null(bfpackContainer[["bfpackResult"]])) {
    return(bfpackContainer[["bfpackResult"]]$object)
  } else if (ready) {
    if (type == "anova" || type == "ancova") {
      groupCol <- dataset[, options[["fixedFactors"]]]
      varLevels <- levels(groupCol)
      if (length(varLevels) > 15) {
        bfpackContainer$setError(gettext("The fixed factor has too many levels for a Bfpack analysis."))
        return()
      }
    }

    p <- try({
      if (type == "sem") {
        bfpackResult <- .bfpackSemRaw(dataset, options, bfpackContainer, ready)
      } else {
        bfpackResult <- .bfpackRegressionRaw(dataset, options, type)
      }
    })

    if (isTryError(p)) {
      bfpackContainer$setError(gettextf("An error occurred in the analysis:<br>%1$s<br><br>Please double check if the variables in the 'Model Contraints' section match the variables in your data set.", jaspBase::.extractErrorMessage(p)))
      return()
    }

    bfpackContainer[["bfpackResult"]] <- createJaspState(bfpackResult)
    return(bfpackContainer[["bfpackResult"]]$object)
  }
}

.bfpackExtractTableValuesFromObject <- function(options, bfpackResult, testType) {
  if (testType == 1) {
    BF_0u <- bfpackResult$fit$BF[1]
    PMP_u <- bfpackResult$fit$PMPb[2]
    PMP_0 <- bfpackResult$fit$PMPb[1]
    if (options$bayesFactorType == "BF10") {
      BF_0u <- 1 / BF_0u
    }
    return(list(BF_0u = BF_0u, PMP_0 = PMP_0, PMP_u = PMP_u))
  } else if (testType == 2) {
    BF_01 <- bfpackResult$BFmatrix[1, 2]
    PMP_1 <- bfpackResult$fit$PMPa[2]
    PMP_0 <- bfpackResult$fit$PMPa[1]
    if (options$bayesFactorType == "BF10") {
      BF_01 <- 1 / BF_01
    }
    return(list(BF_01 = BF_01, PMP_0 = PMP_0, PMP_1 = PMP_1))
  } else if (testType == 3 || testType == 4) {
    BF_01 <- bfpackResult$BFmatrix[1, 2]
    PMP_0 <- bfpackResult$fit$PMPa[1]
    PMP_1 <- bfpackResult$fit$PMPa[2]
    if (options$bayesFactorType == "BF10") {
      BF_01 <- 1 / BF_01
    }
    return(list(BF_01 = BF_01, PMP_0 = PMP_0, PMP_1 = PMP_1))
  } else if (testType == 5) {
    BF_01 <- bfpackResult$BFmatrix[1, 2]
    BF_02 <- bfpackResult$BFmatrix[1, 3]
    BF_12 <- bfpackResult$BFmatrix[2, 3]
    PMP_0 <- bfpackResult$fit$PMPa[1]
    PMP_1 <- bfpackResult$fit$PMPa[2]
    PMP_2 <- bfpackResult$fit$PMPa[3]
    if (options$bayesFactorType == "BF10") {
      BF_01 <- 1 / BF_01
      BF_02 <- 1 / BF_02
      BF_12 <- 1 / BF_12
    }
    return(list(BF_01 = BF_01, BF_02 = BF_02, BF_12 = BF_12, PMP_0 = PMP_0, PMP_1 = PMP_1, PMP_2 = PMP_2))
  }
}

# # Create a table containing the main analysis results
# .bfpackTestResultsTable <- function(dataList, options, bfpackContainer, ready, type, position) {
#
#   if (!is.null(bfpackContainer[["mainResultsTable"]])) {
#     return()
#   }
#
#   dataset <- dataList[["dataset"]]
#   missing <- dataList[["missing"]]
#
#   title <- switch(type,
#                   "independentTTest" = gettext("Bfpack Independent Samples Welch's T-Test"),
#                   "pairedTTest" = gettext("Bfpack Paired Samples T-Test"),
#                   "onesampleTTest" = gettext("Bfpack One Sample T-test"),
#                   "anova" = gettext("Bfpack ANOVA"),
#                   "ancova" = gettext("Bfpack ANCOVA"),
#                   "regression" = gettext("Bfpack Linear Regression"),
#                   "correlation" = gettext("Bfpack Correlation")
#   )
#   deps <- switch(type,
#                  "independentTTest" = c("variables", "bayesFactorType", "hypothesis"),
#                  "pairedTTest" = c("pairs", "hypothesis", "bayesFactorType"),
#                  "onesampleTTest" = c("variables", "hypothesis", "bayesFactorType"),
#                  "anova" = NULL,
#                  "ancova" = NULL,
#                  "regression" = "standardized",
#                  "correlation" = NULL
#   )
#   table <- createJaspTable(title)
#   table$dependOn(options = deps)
#   table$position <- position
#   if (type %in% c("independentTTest", "pairedTTest", "onesampleTTest")) {
#     if (options$hypothesis == "equalBiggerSmaller") {
#       table$addColumnInfo(name = "Variable", type = "string", title = "")
#       table$addColumnInfo(name = "type[equal]", type = "string", title = gettext("Hypothesis"))
#       table$addColumnInfo(name = "BF[equal]", type = "number", title = gettext("BF"))
#       table$addColumnInfo(name = "pmp[equal]", type = "number", format = "dp:3", title = gettext("Posterior probability"))
#       table$addColumnInfo(name = "type[greater]", type = "string", title = gettext("Hypothesis"))
#       table$addColumnInfo(name = "BF[greater]", type = "number", title = gettext("BF"))
#       table$addColumnInfo(name = "pmp[greater]", type = "number", format = "dp:3", title = gettext("Posterior probability"))
#       table$addColumnInfo(name = "type[less]", type = "string", title = gettext("Hypothesis"))
#       table$addColumnInfo(name = "BF[less]", type = "number", title = gettext("BF"))
#       table$addColumnInfo(name = "pmp[less]", type = "number", format = "dp:3", title = gettext("Posterior probability"))
#     } else {
#       table$addColumnInfo(name = "Variable", type = "string", title = "")
#       table$addColumnInfo(name = "hypothesis[type1]", type = "string", title = gettext("Hypothesis"))
#       table$addColumnInfo(name = "BF[type1]", type = "number", title = gettext("BF"))
#       table$addColumnInfo(name = "pmp[type1]", type = "number", format = "dp:3", title = gettext("Posterior probability"))
#       table$addColumnInfo(name = "hypothesis[type2]", type = "string", title = gettext("Hypothesis"))
#       table$addColumnInfo(name = "BF[type2]", type = "number", title = gettext("BF"))
#       table$addColumnInfo(name = "pmp[type2]", type = "number", format = "dp:3", title = gettext("Posterior probability"))
#     }
#     if (type == "onesampleTTest") {
#       message <- switch(options[["hypothesis"]],
#                         "equalNotEqual" = gettextf("The alternative hypothesis H1 specifies that the mean is unequal to %s. The posterior probabilities are based on equal prior probabilities.", options[["testValue"]]),
#                         "equalBigger" = gettextf("The alternative hypothesis H1 specifies that the mean is bigger than %s. The posterior probabilities are based on equal prior probabilities.", options[["testValue"]]),
#                         "equalSmaller" = gettextf("The alternative hypothesis H1 specifies that the mean is smaller than %s. The posterior probabilities are based on equal prior probabilities.", options[["testValue"]]),
#                         "biggerSmaller" = gettextf("The hypothesis H1 specifies that the mean is bigger than %1$s and the hypothesis H2 specifies that the mean is smaller than %1$s. The posterior probabilities are based on equal prior probabilities.", options[["testValue"]]),
#                         "equalBiggerSmaller" = gettextf("The null hypothesis H0 with test value %1$s is tested against the other hypotheses. H1 states that the mean is bigger than %1$s and H2 states that the mean is smaller than %1$s. The posterior probabilities are based on equal prior probabilities.", options[["testValue"]])
#       )
#     }
#     if (type == "independentTTest") {
#       message <- switch(options[["hypothesis"]],
#                         "equalNotEqual" = gettext("The alternative hypothesis H1 specifies that the mean of group 1 is unequal to the mean of group 2. The posterior probabilities are based on equal prior probabilities."),
#                         "equalSmaller" = gettext("The alternative hypothesis H1 specifies that the mean of group 1 is smaller than the mean of group 2. The posterior probabilities are based on equal prior probabilities."),
#                         "equalBigger" = gettext("The alternative hypothesis H1 specifies that mean of group 1 is bigger than the mean of group 2. The posterior probabilities are based on equal prior probabilities."),
#                         "biggerSmaller" = gettext("The hypothesis H1 specifies that the mean of group 1 is bigger than the mean of group 2. The hypothesis H2 specifies that the mean in group 1 is smaller than the mean in group 2. The posterior probabilities are based on equal prior probabilities."),
#                         "equalBiggerSmaller" = gettext("The null hypothesis H0 (equal group means) is tested against H1 (first mean larger than second mean) and H2 (first mean smaller than second mean). The posterior probabilities are based on equal prior probabilities.")
#       )
#     }
#     if (type == "pairedTTest") {
#       message <- switch(options[["hypothesis"]],
#                         "equalNotEqual"       = gettext("The alternative hypothesis H1 specifies that the mean of variable 1 is unequal to the mean of variable 2. The posterior probabilities are based on equal prior probabilities."),
#                         "equalBigger"         = gettext("The alternative hypothesis H1 specifies that the mean of variable 1 is bigger than the mean of variable 2. The posterior probabilities are based on equal prior probabilities."),
#                         "equalSmaller"        = gettext("The alternative hypothesis H1 specifies that the mean of variable 1 is smaller than the mean of variable 2. The posterior probabilities are based on equal prior probabilities."),
#                         "biggerSmaller"       = gettext("The hypothesis H1 specifies that the mean of variable 1 is bigger than the mean of variable 2, while the hypothesis H2 specifies that it is smaller. The posterior probabilities are based on equal prior probabilities."),
#                         "equalBiggerSmaller"  = gettext("The null hypothesis H0 with equal means is tested against the other hypotheses. The alternative hypothesis H1 states that the mean of variable 1 is bigger than the mean of variable 2. The alternative hypothesis H2 states that the mean of variable 1 is smaller than the mean of variable 2. The posterior probabilities are based on equal prior probabilities.")
#       )
#     }
#   } else if (type %in% c("anova", "ancova", "regression", "correlation")) {
#     table$addColumnInfo(name = "hypotheses", type = "string", title = "")
#     table$addColumnInfo(name = "BFu", type = "number", title = gettext("BF.u"))
#     table$addColumnInfo(name = "BF", type = "number", title = gettext("BF.c"))
#     table$addColumnInfo(name = "PMP1", type = "number", title = gettext("PMP a"))
#     table$addColumnInfo(name = "PMP2", type = "number", title = gettext("PMP b"))
#     table$addColumnInfo(name = "PMP3", type = "number", title = gettext("PMP c"))
#     message <- gettext("BF.u denotes the Bayes factor of the hypothesis at hand versus the unconstrained hypothesis Hu. \
#                         BF.c denotes the Bayes factor of the hypothesis at hand versus its complement. \
#                         PMPa contains the posterior model probabilities of the hypotheses specified. \
#                         PMPb adds Hu, the unconstrained hypothesis. \
#                         PMPc adds Hc, the complement of the union of the hypotheses specified. \
#                         All PMPs are based on equal prior model probabilities.")
#   }
#   table$addFootnote(message = message)
#   table$addCitation(.bfpackGetCitations())
#   bfpackContainer[["mainResultsTable"]] <- table
#
#   if (!ready || (type == "sem" && options[["model"]] == "")) {
#     return()
#   }
#
#   if (type %in% c("onesampleTTest", "independentTTest")) {
#     testType <- switch(options[["hypothesis"]],
#                        "equalNotEqual"       = 1,
#                        "equalBigger"         = 2,
#                        "equalSmaller"        = 3,
#                        "biggerSmaller"       = 4,
#                        "equalBiggerSmaller"  = 5
#     )
#
#     table$setExpectedSize(length(options[["variables"]]))
#     startProgressbar(length(options[["variables"]]))
#
#     for (variable in options[["variables"]]) {
#       bfpackResult <- .bfpackGetGeneralTestResults(dataset, options, bfpackContainer, ready, type, variable = variable, testType = testType)
#
#       if (isTryError(bfpackResult)) {
#         table$addRows(list(Variable = variable), rowNames = variable)
#         table$addFootnote(message = gettextf("Results not computed: %1$s.", jaspBase::.extractErrorMessage(bfpackResult)), colNames = "Variable", rowNames = variable)
#         progressbarTick()
#         next
#       }
#
#       if (variable %in% missing) {
#         table$addFootnote(message = gettext("Variable contains missing values, the rows containing these values are removed in the analysis."), colNames = "Variable", rowNames = variable)
#       }
#
#       tableResults <- .bfpackExtractTableValuesFromObject(options, bfpackResult, testType)
#
#       if (options$bayesFactorType == "BF01") {
#         if (options$hypothesis == "equalNotEqual") {
#           row <- list(
#             Variable = variable, "hypothesis[type1]" = gettext("H0: Equal"), "BF[type1]" = tableResults[["BF_0u"]], "pmp[type1]" = tableResults[["PMP_0"]],
#             "hypothesis[type2]" = gettext("H1: Not equal"), "BF[type2]" = "", "pmp[type2]" = tableResults[["PMP_u"]]
#           )
#         } else if (options$hypothesis == "equalBigger") {
#           row <- list(
#             Variable = variable, "hypothesis[type1]" = gettext("H0: Equal"), "BF[type1]" = tableResults[["BF_01"]], "pmp[type1]" = tableResults[["PMP_0"]],
#             "hypothesis[type2]" = gettext("H1: Bigger"), "BF[type2]" = "", "pmp[type2]" = tableResults[["PMP_1"]]
#           )
#         } else if (options$hypothesis == "equalSmaller") {
#           row <- list(
#             Variable = variable, "hypothesis[type1]" = gettext("H0: Equal"), "BF[type1]" = tableResults[["BF_01"]], "pmp[type1]" = tableResults[["PMP_0"]],
#             "hypothesis[type2]" = gettext("H1: Smaller"), "BF[type2]" = "", "pmp[type2]" = tableResults[["PMP_1"]]
#           )
#         } else if (options$hypothesis == "biggerSmaller") {
#           row <- list(
#             Variable = variable, "hypothesis[type1]" = gettext("H1: Bigger"), "BF[type1]" = tableResults[["BF_01"]], "pmp[type1]" = tableResults[["PMP_0"]],
#             "hypothesis[type2]" = gettext("H2: Smaller"), "BF[type2]" = "", "pmp[type2]" = tableResults[["PMP_1"]]
#           )
#         } else if (options$hypothesis == "equalBiggerSmaller") {
#           row <- list(
#             Variable = variable, "type[equal]" = gettext("H0: Equal"), "BF[equal]" = "", "pmp[equal]" = tableResults[["PMP_0"]],
#             "type[greater]" = gettext("H1: Bigger"), "BF[greater]" = tableResults[["BF_01"]], "pmp[greater]" = tableResults[["PMP_1"]],
#             "type[less]" = gettext("H2: Smaller"), "BF[less]" = tableResults[["BF_02"]], "pmp[less]" = tableResults[["PMP_2"]]
#           )
#         }
#       } else if (options$bayesFactorType == "BF10") {
#         if (options$hypothesis == "equalNotEqual") {
#           row <- list(
#             Variable = variable, "hypothesis[type1]" = gettext("H0: Equal"), "BF[type1]" = "", "pmp[type1]" = tableResults[["PMP_0"]],
#             "hypothesis[type2]" = gettext("H1: Not equal"), "BF[type2]" = tableResults[["BF_0u"]], "pmp[type2]" = tableResults[["PMP_u"]]
#           )
#         } else if (options$hypothesis == "equalBigger") {
#           row <- list(
#             Variable = variable, "hypothesis[type1]" = gettext("H0: Equal"), "BF[type1]" = "", "pmp[type1]" = tableResults[["PMP_0"]],
#             "hypothesis[type2]" = gettext("H1: Bigger"), "BF[type2]" = tableResults[["BF_01"]], "pmp[type2]" = tableResults[["PMP_1"]]
#           )
#         } else if (options$hypothesis == "equalSmaller") {
#           row <- list(
#             Variable = variable, "hypothesis[type1]" = gettext("H0: Equal"), "BF[type1]" = "", "pmp[type1]" = tableResults[["PMP_0"]],
#             "hypothesis[type2]" = gettext("H1: Smaller"), "BF[type2]" = tableResults[["BF_01"]], "pmp[type2]" = tableResults[["PMP_1"]]
#           )
#         } else if (options$hypothesis == "biggerSmaller") {
#           row <- list(
#             Variable = variable, "hypothesis[type1]" = gettext("H1: Bigger"), "BF[type1]" = "", "pmp[type1]" = tableResults[["PMP_0"]],
#             "hypothesis[type2]" = gettext("H2: Smaller"), "BF[type2]" = tableResults[["BF_01"]], "pmp[type2]" = tableResults[["PMP_1"]]
#           )
#         } else if (options$hypothesis == "equalBiggerSmaller") {
#           row <- list(
#             Variable = variable, "type[equal]" = gettext("H0: Equal"), "BF[equal]" = "", "pmp[equal]" = tableResults[["PMP_0"]],
#             "type[greater]" = gettext("H1: Bigger"), "BF[greater]" = tableResults[["BF_01"]], "pmp[greater]" = tableResults[["PMP_1"]],
#             "type[less]" = gettext("H2: Smaller"), "BF[less]" = tableResults[["BF_02"]], "pmp[less]" = tableResults[["PMP_2"]]
#           )
#         }
#       }
#       table$addRows(row, rowNames = variable)
#       progressbarTick()
#     }
#   } else if (type == "pairedTTest") {
#     testType <- switch(options[["hypothesis"]],
#                        "equalNotEqual"       = 1,
#                        "equalBigger"         = 2,
#                        "equalSmaller"        = 3,
#                        "biggerSmaller"       = 4,
#                        "equalBiggerSmaller"  = 5
#     )
#
#     table$setExpectedSize(length(options[["pairs"]]))
#
#     startProgressbar(length(options[["pairs"]]))
#
#     for (pair in options[["pairs"]]) {
#       currentPair <- paste(pair, collapse = " - ")
#
#       if (pair[[1]] != "" || pair[[2]] != "") {
#         bfpackResult <- .bfpackGetGeneralTestResults(dataset, options, bfpackContainer, ready, type, pair = pair, testType = testType)
#
#         if (isTryError(bfpackResult)) {
#           table$addRows(list(Variable = currentPair), rowNames = currentPair)
#           table$addFootnote(message = gettextf("Results not computed: %s", jaspBase::.extractErrorMessage(bfpackResult)), colNames = "Variable", rowNames = currentPair)
#           progressbarTick()
#           next
#         }
#
#         if (any(pair %in% missing)) {
#           i <- which(pair %in% missing)
#           if (length(i) > 1) {
#             message <- gettext("Both variables contain missing values, the rows containing these values are removed in the analysis.")
#           } else {
#             message <- gettextf("The variable %s contains missing values, the rows containing these values are removed in the analysis.", pair[i])
#           }
#           table$addFootnote(message = message, colNames = "Variable", rowNames = currentPair)
#         }
#
#         tableResults <- .bfpackExtractTableValuesFromObject(options, bfpackResult, testType)
#
#         if (options$bayesFactorType == "BF01") {
#           if (options$hypothesis == "equalNotEqual") {
#             row <- list(
#               Variable = currentPair, "hypothesis[type1]" = gettext("H0: Equal"), "BF[type1]" = tableResults[["BF_0u"]], "pmp[type1]" = tableResults[["PMP_0"]],
#               "hypothesis[type2]" = gettext("H1: Not equal"), "BF[type2]" = "", "pmp[type2]" = tableResults[["PMP_u"]]
#             )
#           }
#           if (options$hypothesis == "equalSmaller") {
#             row <- list(
#               Variable = currentPair, "hypothesis[type1]" = gettext("H0: Equal"), "BF[type1]" = tableResults[["BF_01"]], "pmp[type1]" = tableResults[["PMP_0"]],
#               "hypothesis[type2]" = gettext("H1: Smaller"), "BF[type2]" = "", "pmp[type2]" = tableResults[["PMP_1"]]
#             )
#           }
#           if (options$hypothesis == "equalBigger") {
#             row <- list(
#               Variable = currentPair, "hypothesis[type1]" = gettext("H0: Equal"), "BF[type1]" = tableResults[["BF_01"]], "pmp[type1]" = tableResults[["PMP_0"]],
#               "hypothesis[type2]" = gettext("H1: Bigger"), "BF[type2]" = "", "pmp[type2]" = tableResults[["PMP_1"]]
#             )
#           }
#           if (options$hypothesis == "biggerSmaller") {
#             row <- list(
#               Variable = currentPair, "hypothesis[type1]" = gettext("H1: Bigger"), "BF[type1]" = tableResults[["BF_01"]], "pmp[type1]" = tableResults[["PMP_0"]],
#               "hypothesis[type2]" = gettext("H2: Smaller"), "BF[type2]" = "", "pmp[type2]" = tableResults[["PMP_1"]]
#             )
#           }
#           if (options$hypothesis == "equalBiggerSmaller") {
#             row <- list(
#               Variable = currentPair,
#               "type[equal]" = gettext("H0: Equal"),
#               "BF[equal]" = "",
#               "pmp[equal]" = tableResults[["PMP_0"]],
#               "type[greater]" = gettext("H1: Bigger"),
#               "BF[greater]" = tableResults[["BF_01"]],
#               "pmp[greater]" = tableResults[["PMP_1"]],
#               "type[less]" = gettext("H2: Smaller"),
#               "BF[less]" = tableResults[["BF_02"]],
#               "pmp[less]" = tableResults[["PMP_2"]]
#             )
#           }
#         } else if (options$bayesFactorType == "BF10") {
#           if (options$hypothesis == "equalNotEqual") {
#             row <- list(
#               Variable = currentPair, "hypothesis[type1]" = gettext("H0: Equal"), "BF[type1]" = "", "pmp[type1]" = tableResults[["PMP_0"]],
#               "hypothesis[type2]" = gettext("H1: Not equal"), "BF[type2]" = tableResults[["BF_0u"]], "pmp[type2]" = tableResults[["PMP_u"]]
#             )
#           } else if (options$hypothesis == "equalSmaller") {
#             row <- list(
#               Variable = currentPair, "hypothesis[type1]" = gettext("H0: Equal"), "BF[type1]" = "", "pmp[type1]" = tableResults[["PMP_0"]],
#               "hypothesis[type2]" = gettext("H1: Smaller"), "BF[type2]" = tableResults[["BF_01"]], "pmp[type2]" = tableResults[["PMP_1"]]
#             )
#           } else if (options$hypothesis == "equalBigger") {
#             row <- list(
#               Variable = currentPair, "hypothesis[type1]" = gettext("H0: Equal"), "BF[type1]" = "", "pmp[type1]" = tableResults[["PMP_0"]],
#               "hypothesis[type2]" = gettext("H1: Bigger"), "BF[type2]" = tableResults[["BF_01"]], "pmp[type2]" = tableResults[["PMP_1"]]
#             )
#           } else if (options$hypothesis == "biggerSmaller") {
#             row <- list(
#               Variable = currentPair, "hypothesis[type1]" = gettext("H1: Bigger"), "BF[type1]" = "", "pmp[type1]" = tableResults[["PMP_0"]],
#               "hypothesis[type2]" = gettext("H2: Smaller"), "BF[type2]" = tableResults[["BF_01"]], "pmp[type2]" = tableResults[["PMP_1"]]
#             )
#           } else if (options$hypothesis == "equalBiggerSmaller") {
#             row <- list(
#               Variable = currentPair,
#               "type[equal]" = gettext("H0: Equal"),
#               "BF[equal]" = "",
#               "pmp[equal]" = tableResults[["PMP_0"]],
#               "type[greater]" = gettext("H1: Bigger"),
#               "BF[greater]" = tableResults[["BF_01"]],
#               "pmp[greater]" = tableResults[["PMP_1"]],
#               "type[less]" = gettext("H2: Smaller"),
#               "BF[less]" = tableResults[["BF_02"]],
#               "pmp[less]" = tableResults[["PMP_2"]]
#             )
#           }
#         }
#       } else {
#         if (options$hypothesis == "equalBiggerSmaller") {
#           row <- list(
#             Variable = currentPair, "type[equal]" = ".", "BF[equal]" = ".", "pmp[equal]" = ".",
#             "type[greater]" = ".", "BF[greater]" = ".", "pmp[greater]" = ".",
#             "type[less]" = ".", "BF[less]" = ".", "pmp[less]" = "."
#           )
#         } else {
#           row <- list(
#             Variable = currentPair, "hypothesis[type1]" = ".", "BF[type1]" = ".", "pmp[type1]" = ".",
#             "hypothesis[type2]" = ".", "BF[type2]" = ".", "pmp[type2]" = "."
#           )
#         }
#       }
#       table$addRows(row, rowNames = currentPair)
#
#       if (pair[[1]] == pair[[2]]) {
#         table$addFootnote(message = gettext("Results not computed: The variables in this pair are the same."), colNames = "Variable", rowNames = currentPair)
#       } else if (pair[[1]] == "" || pair[[2]] == "") {
#         table$addFootnote(message = gettext("Results not computed: The pair is incomplete."), colNames = "Variable", rowNames = currentPair)
#       }
#       progressbarTick()
#     }
#   } else if (type %in% c("anova", "ancova", "regression", "sem")) {
#     if (bfpackContainer$getError()) {
#       return()
#     }
#     variables <- switch(type,
#                         "anova" = c(options[["dependent"]], options[["fixedFactors"]]),
#                         "ancova" = c(options[["dependent"]], options[["fixedFactors"]], unlist(options[["covariates"]])),
#                         "regression" = c(options[["dependent"]], unlist(options[["covariates"]])),
#                         "sem" = .bfpackSemGetUsedVars(.bfpackSemTranslateModel(options[["syntax"]]$model, dataset), colnames(dataset))
#     )
#     if (any(variables %in% missing)) {
#       i <- which(variables %in% missing)
#       if (length(i) > 1) {
#         if (type == "regression" || type == "ancova" || type == "sem") {
#           table$addFootnote(message = gettextf("The variables %1$s contain missing values, the rows containing these values are removed in the analysis.", paste(variables[i], collapse = ", ")), symbol = gettext("<b>Warning.</b>"))
#         } else if (type == "anova") {
#           table$addFootnote(message = gettextf("The variables %1$s and %2$s contain missing values, the rows containing these values are removed in the analysis.", variables[1], variables[2]), symbol = gettext("<b>Warning.</b>"))
#         }
#       } else if (length(i) == 1) {
#         table$addFootnote(message = gettextf("The variable %1$s contains missing values, the rows containing these values are removed in the analysis.", variables[i]), symbol = gettext("<b>Warning.</b>"))
#       }
#     }
#     bfpackResult <- .bfpackGetGeneralTestResults(dataset, options, bfpackContainer, ready, type)
#     for (i in 1:(length(bfpackResult[["fit"]]$BF) - 2)) {
#       row <- list(hypotheses = gettextf("H%1$i", i), BFu = bfpackResult[["fit"]]$BF.u[i], BF = bfpackResult[["fit"]]$BF.c[i], PMP1 = bfpackResult[["fit"]]$PMPa[i], PMP2 = bfpackResult[["fit"]]$PMPb[i], PMP3 = bfpackResult[["fit"]]$PMPc[i])
#       table$addRows(row)
#     }
#     rows <- list(
#       list(hypotheses = gettext("Hu"), BFu = "", BF = "", PMP1 = "", PMP2 = bfpackResult[["fit"]]$PMPb[length(bfpackResult[["fit"]]$BF) - 1], PMP3 = ""),
#       list(hypotheses = gettext("Hc"), BFu = bfpackResult[["fit"]]$BF.u[length(bfpackResult[["fit"]]$BF.u)], BF = "", PMP1 = "", PMP2 = "", PMP3 = bfpackResult[["fit"]]$PMPc[length(bfpackResult[["fit"]]$BF)])
#     )
#     table$addRows(rows)
#   }
#   if (!isTryError(bfpackResult) && any(is.nan(unlist(bfpackResult[["fit"]])))) {
#     table$addFootnote(symbol = gettext("<b>Warning</b>"), message = gettext("The entered model constraints are incompatible with the data and therefore the computed results contain NaNs."))
#   }
#   if (type %in% c("anova", "ancova", "regression", "sem") && !is.null(bfpackResult)) {
#     complexity <- bfpackResult[["fit"]]$Com[length(bfpackResult[["fit"]]$Com)]
#     if (!is.na(complexity) && complexity < .05) {
#       table$addFootnote(gettext("<b>Your hypotheses (almost) completely cover the parameter space. Therefore instead of PMPc, you should interpret PMPa.</b>"))
#     }
#   }
# }
#
# # Create the Bayes factor matrix
# .bfpackBfMatrix <- function(dataset, options, bfpackContainer, ready, type, position) {
#   if (!is.null(bfpackContainer[["bayesFactorMatrix"]]) || !options[["bayesFactorMatrix"]]) {
#     return()
#   }
#
#   bayesFactorMatrix <- createJaspTable(gettext("Bayes Factor Matrix"))
#   bayesFactorMatrix$position <- position
#   bayesFactorMatrix$dependOn(options = c("bayesFactorMatrix", "standardized"))
#   bayesFactorMatrix$addColumnInfo(name = "hypothesis", title = "", type = "string")
#   bayesFactorMatrix$addColumnInfo(name = "H1", title = gettext("H1"), type = "number")
#   bfpackContainer[["bayesFactorMatrix"]] <- bayesFactorMatrix
#
#   if (!ready || bfpackContainer$getError() || (type == "sem" && options[["model"]] == "")) {
#     row <- data.frame(hypothesis = gettext("H1"), H1 = ".")
#     bayesFactorMatrix$addRows(row)
#     return()
#   }
#
#   bfpackResult <- .bfpackGetGeneralTestResults(dataset, options, bfpackContainer, ready, type)
#   BFmatrix <- bfpackResult[["BFmatrix"]]
#   if (nrow(BFmatrix) > 1) {
#     for (i in 2:nrow(BFmatrix)) {
#       bayesFactorMatrix$addColumnInfo(name = paste0("H", i), title = gettextf("H%i", i), type = "number")
#     }
#   }
#   for (i in seq_len(nrow(BFmatrix))) {
#     tmp <- list(hypothesis = gettextf("H%i", i))
#     for (j in seq_len(ncol(BFmatrix))) {
#       tmp[[paste0("H", j)]] <- BFmatrix[i, j]
#     }
#     row <- tmp
#     bayesFactorMatrix$addRows(row)
#   }
# }
#
# # Create the descriptive statistics table
# .bfpackDescriptivesTable <- function(dataset, options, bfpackContainer, ready, type, position) {
#   if (!is.null(bfpackContainer[["descriptivesTable"]]) || !options[["descriptives"]]) {
#     return()
#   }
#
#   if (type == "ancova") {
#     title <- gettext("Coefficients for Groups plus Covariates")
#   } else if (type == "regression" || type == "sem") {
#     title <- gettext("Coefficients for Parameters")
#   } else {
#     title <- gettext("Descriptive Statistics")
#   }
#   meanTitle <- ifelse(type %in% c("ancova", "regression"), yes = gettext("Coefficient"), no = gettext("Mean"))
#   table <- createJaspTable(title)
#   table$dependOn(options = c("variables", "descriptives", "credibleInterval", "pairs"))
#   table$position <- position
#   overTitle <- gettextf("%.0f%% Credible Interval", 100 * options[["credibleInterval"]])
#   table$addColumnInfo(name = "v", title = "", type = "string")
#   if (type == "independentTTest") {
#     table$addColumnInfo(name = "group", title = gettext("Group"), type = "string")
#   }
#   table$addColumnInfo(name = "N", title = gettext("N"), type = "integer")
#   table$addColumnInfo(name = "mean", title = meanTitle, type = "number")
#   if (type %in% c("independentTTest", "pairedTTest", "onesampleTTest", "anova")) {
#     table$addColumnInfo(name = "sd", title = gettext("SD"), type = "number")
#   }
#   table$addColumnInfo(name = "se", title = gettext("SE"), type = "number")
#   table$addColumnInfo(name = "lowerCI", title = gettext("Lower"), type = "number", overtitle = overTitle)
#   table$addColumnInfo(name = "upperCI", title = gettext("Upper"), type = "number", overtitle = overTitle)
#   bfpackContainer[["descriptivesTable"]] <- table
#   if (type == "regression" || type == "sem") {
#     table$addFootnote(message = ifelse(options[["standardized"]], yes = gettext("The displayed coefficients are standardized."), no = gettext("The displayed coefficients are unstandardized.")))
#   }
#
#   if (!ready || bfpackContainer$getError()) {
#     return()
#   }
#
#   if (type == "independentTTest") {
#     table$setExpectedSize(length(options[["variables"]]) * 2)
#     levels <- levels(dataset[[options[["groupingVariable"]]]])
#     if (length(levels) != 2) {
#       g1 <- "1"
#       g2 <- "2"
#     } else {
#       g1 <- levels[1]
#       g2 <- levels[2]
#     }
#     for (variable in options[["variables"]]) {
#       bfpackResult <- .bfpackGetGeneralTestResults(dataset, options, bfpackContainer, ready, type, variable = variable)
#       if (isTryError(bfpackResult)) {
#         table$addRows(list(v = variable), rowNames = variable)
#         table$addFootnote(message = gettext("The results for this variable were not computed."), colNames = "v", rowNames = variable)
#       } else {
#         bfpackSummary <- summary(bfpackResult, ci = options[["credibleInterval"]])
#
#         N <- bfpackSummary[["n"]]
#         mu <- bfpackSummary[["Estimate"]]
#         CiLower <- bfpackSummary[["lb"]]
#         CiUpper <- bfpackSummary[["ub"]]
#         sd <- aggregate(dataset[, variable], list(dataset[, options[["groupingVariable"]]]), sd)[, 2]
#         se <- sqrt(diag(bfpackResult[["posterior"]]))
#         row <- data.frame(v = variable, group = g1, N = N[1], mean = mu[1], sd = sd[1], se = se[1], lowerCI = CiLower[1], upperCI = CiUpper[1])
#         table$addRows(row)
#         row <- data.frame(v = "", group = g2, N = N[2], mean = mu[2], sd = sd[2], se = se[2], lowerCI = CiLower[2], upperCI = CiUpper[2])
#         table$addRows(row)
#       }
#     }
#   } else if (type == "pairedTTest") {
#     table$setExpectedSize(length(options[["pairs"]]))
#     for (pair in options[["pairs"]]) {
#       if (pair[[2]] != "" && pair[[1]] != pair[[2]]) {
#         subDataSet <- subset(dataset, select = c(pair[[1]], pair[[2]]))
#         c1 <- subDataSet[[pair[[1]]]]
#         c2 <- subDataSet[[pair[[2]]]]
#         difference <- c1 - c2
#         currentPair <- paste(pair, collapse = " - ")
#         testType <- switch(options[["hypothesis"]],
#                            "equalNotEqual"       = 1,
#                            "equalBigger"         = 2,
#                            "equalSmaller"        = 3,
#                            "biggerSmaller"       = 4,
#                            "equalBiggerSmaller"  = 5
#         )
#         bfpackResult <- .bfpackGetGeneralTestResults(dataset, options, bfpackContainer, ready, type, pair = pair, testType = testType)
#         if (isTryError(bfpackResult)) {
#           table$addRows(list(v = currentPair), rowNames = currentPair)
#           table$addFootnote(message = gettext("The results for this variable were not computed."), colNames = "v", rowNames = currentPair)
#         } else {
#           bfpackSummary <- summary(bfpackResult, ci = options[["credibleInterval"]])
#           N <- bfpackSummary[["n"]]
#           mu <- bfpackSummary[["Estimate"]]
#           CiLower <- bfpackSummary[["lb"]]
#           CiUpper <- bfpackSummary[["ub"]]
#           se <- sqrt(diag(bfpackResult[["posterior"]]))
#           sd <- sd(difference, na.rm = TRUE)
#           row <- list(v = currentPair, N = N, mean = mu, sd = sd, se = se, lowerCI = CiLower, upperCI = CiUpper)
#           table$addRows(row)
#         }
#       }
#     }
#   } else if (type == "onesampleTTest") {
#     table$setExpectedSize(length(options[["variables"]]))
#     for (variable in options[["variables"]]) {
#       bfpackResult <- .bfpackGetGeneralTestResults(dataset, options, bfpackContainer, ready, type, variable = variable)
#       if (isTryError(bfpackResult)) {
#         table$addRows(list(v = variable), rowNames = variable)
#         table$addFootnote(message = gettext("The results for this variable were not computed."), colNames = "v", rowNames = variable)
#       } else {
#         bfpackSummary <- summary(bfpackResult, ci = options[["credibleInterval"]])
#         N <- bfpackSummary[["n"]]
#         mu <- bfpackSummary[["Estimate"]]
#         CiLower <- bfpackSummary[["lb"]]
#         CiUpper <- bfpackSummary[["ub"]]
#         sd <- sd(dataset[, variable], na.rm = TRUE)
#         se <- sqrt(diag(bfpackResult[["posterior"]]))
#         row <- list(v = variable, N = N, mean = mu, sd = sd, se = se, lowerCI = CiLower, upperCI = CiUpper)
#         table$addRows(row)
#       }
#     }
#   } else if (type %in% c("anova", "ancova")) {
#     groupCol <- dataset[, options[["fixedFactors"]]]
#     bfpackResult <- .bfpackGetGeneralTestResults(dataset, options, bfpackContainer, ready, type)
#     bfpackSummary <- summary(bfpackResult, ci = options[["credibleInterval"]])
#     sigma <- diag(bfpackResult[["posterior"]])
#     variable <- bfpackSummary[["Parameter"]]
#     N <- bfpackSummary[["n"]]
#     mu <- bfpackSummary[["Estimate"]]
#     CiLower <- bfpackSummary[["lb"]]
#     CiUpper <- bfpackSummary[["ub"]]
#     se <- sqrt(sigma)
#     row <- data.frame(v = variable, N = N, mean = mu, se = se, lowerCI = CiLower, upperCI = CiUpper)
#     if (type == "anova") {
#       sd <- aggregate(dataset[, options[["dependent"]]], list(dataset[, options[["fixedFactors"]]]), sd)[, 2]
#       row <- cbind(row, sd = sd)
#     }
#     table$addRows(row)
#   } else if (type %in% c("regression", "sem")) {
#     bfpackResult <- .bfpackGetGeneralTestResults(dataset, options, bfpackContainer, ready, type)
#     if (is.null(bfpackResult)) {
#       return()
#     }
#     bfpackSummary <- summary(bfpackResult, ci = options[["credibleInterval"]])
#     groups <- bfpackSummary[["Parameter"]]
#     N <- bfpackSummary[["n"]]
#     mu <- bfpackSummary[["Estimate"]]
#     CiLower <- bfpackSummary[["lb"]]
#     CiUpper <- bfpackSummary[["ub"]]
#     se <- sqrt(diag(bfpackResult[["posterior"]]))
#     row <- data.frame(v = groups, N = N, mean = mu, se = se, lowerCI = CiLower, upperCI = CiUpper)
#     table$addRows(row)
#   }
# }
#
