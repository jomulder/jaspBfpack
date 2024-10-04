############ HELPERS ##########

# Clean the input for the order constraints
.bfpackCleanModelInput <- function(input) {
  return(gsub("\n+", ";", input))
}

# Add the BFpack citations
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

# also needs adjustment if there is a new analysis that includes interactions
# give back the interactions to the qml interface
.bfpackFeedbackInteractions <- function(jaspResults, options, type) {

  if (!is.null(jaspResults[["interactionTerms"]])) return()

  deps <- switch(type,
                 "regression" = "covariates",
                 "anova" = c("fixedFactors", "covariates"),
                 "regressionLogistic" = "covariates")

  if (type %in% c("regression", "regressionLogistic")) {
    nams <- decodeColNames(unlist(options[["covariates"]]))
    if (length(nams) < 2) return()
  } else if (type == "anova") {
    nams <- decodeColNames(c(unlist(options[["fixedFactors"]]), unlist(options[["covariates"]])))
    if (length(nams) < 2) return()
  }

  # allow more than two-way interactions
  inters <- list()
  for (i in 2:length(nams)) {
    interTmp <- combn(nams, m = i)
    interTmp <- as.data.frame(interTmp)
    tmp <- lapply(interTmp, function(x) paste0(x, collapse = ":"))
    # somehow this wont work wihtout this:
    names(tmp) <- NULL
    inters <- append(inters, tmp)
  }

  outSource <- createJaspQmlSource("interactionSource", inters)
  outSource$dependOn(c(deps, "interactionTerms"))
  jaspResults[["interactionTerms"]] <- outSource

  return()
}

# this function needs updating when there is a new analysis added
# Read the data set
.bfpackReadDataset <- function(options, type, dataset) {

  vars <- c(unlist(options[["variables"]]),
            unlist(options[["pairs"]]),
            options[["groupingVariable"]],
            unlist(options[["dependent"]]),
            unlist(options[["covariates"]]),
            unlist(options[["fixedFactors"]])
  )

  vars <- vars[vars != ""]
  if (is.null(dataset)) {
    if (type == "onesampleTTest") { # For the one sample t test we do not remove the NA's listwise
      dataset <- .readDataSetToEnd(columns = vars)
    } else {
      dataset <- .readDataSetToEnd(columns = vars, exclude.na.listwise = vars)
    }
  } else {
    dataset <- .vdf(dataset, columns = vars)
  }


  return(dataset)
}

.bfpackUnwrapInteractions <- function(options) {

  iaTerms <- options[["interactionTerms"]]
  iaTrue <- lapply(iaTerms, function(x) {
    if (x[["includeInteractionEffect"]]) x[["value"]] else NULL
  })

  iaTrue <- unlist(iaTrue[which(!sapply(iaTrue, is.null))])
  if (length(iaTrue) > 0) {
    iaString <- paste0(iaTrue, collapse = "+")
  } else {
    iaString <- NULL
  }
  return(iaString)
}

# function from Joris (BFpack) to get variances interval bounds
.bfpackBartlettHelper <- function(x, ciLevel) {
  # Step 1: Obtain parameter estimates
  get_est <- BFpack:::get_estimates.bartlett_htest(x)
  # Step 3: Extract estimated parameter (possibly variance)
  s2 <- get_est$estimate
  # Step 4: Extract sample sizes
  n <- c(x$n)
  # Step 5: Calculate values of 'b'
  b <- 2 / n
  # Step 6: Assign the length of 'n' to 'J'
  J <- length(n)
  # Step 7: Extract names of coefficients
  names_coef <- names(get_est$estimate)
  # Step 8: Calculate scale parameter for the inverse gamma distribution
  scale.post_group <- s2 * (n - 1) / 2
  # Step 9: Calculate shape parameter for the inverse gamma distribution
  shape.post_group <- (n - 1) / 2
  # Step 10: Create a matrix to store posterior estimates
  lower <- (1 - ciLevel) / 2
  upper <- 1 - lower
  postestimates <- cbind(
    NA,
    extraDistr::qinvgamma(0.5, alpha = shape.post_group, beta = scale.post_group),
    extraDistr::qinvgamma(lower, alpha = shape.post_group, beta = scale.post_group),
    extraDistr::qinvgamma(upper, alpha = shape.post_group, beta = scale.post_group)
  )
  # Step 11: Identify groups with shape parameters greater than 1
  which.means <- which(shape.post_group > 1)
  # Step 12: Calculate mean for groups with shape parameters > 1
  postestimates[which.means, 1] <- scale.post_group[which.means] / (shape.post_group[which.means] - 1)
  # Step 13: Assign coefficient names as row names
  row.names(postestimates) <- names_coef
  # Step 14: Assign column names to the matrix
  colnames(postestimates) <- c("mean", "median", "lower", "upper")
  postestimates <- postestimates[, c("lower", "upper")]
  # Print the resulting matrix
  return(postestimates)

}

# another function from Joris to get the ciLevel bounds for the mv t-test
.bfpackMultiTTestHelper <- function(x, ciLevel) {
  x.lm <- x
  class(x.lm) <- c("mlm","lm")
  P <- ncol(x.lm$residuals)
  N <- nrow(x.lm$residuals)
  K <- length(x.lm$coefficients)/P
  Xmat <- model.matrix(x.lm)
  Ymat <- model.matrix(x.lm)%*%x.lm$coefficients + x.lm$residuals
  tXXi <- solve(t(Xmat)%*%Xmat)
  BetaHat <- tXXi%*%t(Xmat)%*%Ymat
  S <- t(Ymat - Xmat%*%BetaHat)%*%(Ymat - Xmat%*%BetaHat)
  dfN <- N-K-P+1
  ScaleN <- kronecker(S,tXXi)/(N-K-P+1)
  meanN <- as.matrix(c(BetaHat))
  names_coef1 <- row.names(x$coefficients)
  names_coef2 <- colnames(x$coefficients)
  names_coef <- unlist(lapply(1:P,function(p){
    lapply(1:K,function(k){
      paste0(names_coef1[k],"_on_",names_coef2[p])
    })
  }))
  row.names(meanN) <- names_coef
  lower <- (1 - ciLevel) / 2
  upper <- 1 - lower
  postestimates <- cbind(meanN,meanN,
                         t(matrix(unlist(lapply(1:length(meanN),function(coef){
                           ub <- qt(p=upper,df=dfN)*sqrt(ScaleN[coef,coef])+meanN[coef,1]
                           lb <- qt(p=lower,df=dfN)*sqrt(ScaleN[coef,coef])+meanN[coef,1]
                           return(c(lb,ub))
                         })),nrow=2))
  )
  row.names(postestimates) <- names_coef
  colnames(postestimates) <- c("mean", "median", "lower", "upper")
  postestimates <- postestimates[, c("lower", "upper")]
  return(postestimates)
}


####### CHECKS #######

# this function needs updating when there is a new analysis added
# Check if current options allow for analysis
.bfpackOptionsReady <- function(options, type) {

  ready <- switch(type,
    "independentTTest" = options[["variables"]] != "" && options[["groupingVariable"]] != "",
    "pairedTTest" = sum(unlist(options[["pairs"]]) != "") > 1,
    "onesampleTTest" = options[["variables"]] != "",
    "anova" = length(unlist(options[["dependent"]])) > 0 && length(unlist(options[["fixedFactors"]])) > 0,
    "regression" = sum(unlist(options[["dependent"]]) != "") > 0 && length(unlist(options[["covariates"]])) > 0,
    "correlation" = length(unlist(options[["variables"]])) > 1,
    "variances" = options[["variables"]] != "" && options[["groupingVariable"]] != "",
    "regressionLogistic" = options[["dependent"]] != "" && length(unlist(options[["covariates"]])) > 0,
    "multiSampleTTest" = length(unlist(options[["variables"]])) > 1
    )

  return(ready)
}

# this function needs updating when there is a new analysis added
# Check if current data allow for analysis
.bfpackDataReady <- function(dataset, options, type, ready) {

  if (!ready) return()

  findex <- which(sapply(dataset, is.factor))
  if (length(findex > 0)) {
    factors <- colnames(dataset)[findex]
    .hasErrors(dataset,
               type = "factorLevels",
               factorLevels.target = factors, factorLevels.amount = '< 2',
               exitAnalysisIfErrors = TRUE
    )

    if (any(grepl(pattern = " ", x = levels(dataset[, findex])))) {
      jaspBase:::.quitAnalysis(gettext("BFpack does not accept factor levels that contain spaces. Please remove the spaces from your factor levels to continue."))
    }
  }

  nonfactors <- colnames(dataset)[-findex]
  if (length(nonfactors) > 0) {
    .hasErrors(dataset,
      type = c("infinity", "variance", "observations"),
      all.target = nonfactors, observations.amount = "< 3",
      exitAnalysisIfErrors = TRUE
    )
  }

  return()
}

###### COMPUTE RESULTS ######
# this function needs updating when there is a new analysis added
# perform the parameter estimation and also return the estimates to the JASP GUI
.bfpackGetParameterEstimates <- function(dataset, options, bfpackContainer, ready, type, jaspResults) {

  if (!is.null(bfpackContainer[["estimatesState"]])) {
    return()
  }

  if (!ready) return()

  dataset <- dataset
  # decode the colnames otherwise bfpack fails when trying to match hypotheses and estimate names
  colnames(dataset) <- decodeColNames(colnames(dataset))

  if (bfpackContainer$getError()) {
    return()
  }

  callString <- switch(type,
                       "independentTTest" = "BFpack:::get_estimates.t_test",
                       "pairedTTest" = "BFpack:::get_estimates.t_test",
                       "onesampleTTest" = "BFpack:::get_estimates.t_test",
                       "anova" = "bain::get_estimates",
                       "regression" = "BFpack:::get_estimates.lm",
                       "correlation" = "BFpack:::get_estimates.cor_test",
                       "variances" = "BFpack:::get_estimates.bartlett_htest",
                       "regressionLogistic" = "BFpack:::get_estimates.glm",
                       "multiSampleTTest" = "BFpack:::get_estimates.mvt_test"
                       )

  # special dependency ciLevel, because for some cor, reg, aov when the ciLevel is changed
  # we can just use the fitted object and change the interval, but for t-test we need to change it
  # in the t-test call (or at least it is easiest)
  deps <- switch(type,
                 "independentTTest" = c("ciLevel", "muValue"),
                 "pairedTTest" = c("ciLevel", "muValue"),
                 "onesampleTTest" = c("ciLevel", "muValue"),
                 "anova" = c("interactionTerms", "includeInteractionEffect"),
                 "regression" = c("interactionTerms", "includeInteractionEffect"),
                 "regressionLogistic"= c("interactionTerms", "includeInteractionEffect"),
                 "multiSampleTTest" = "testValues",
                 NULL)

  # estimate the correlation
  if (type == "correlation") {

    covariates <- unlist(options[["covariates"]])
    covariates <- decodeColNames(covariates)

    if (length(covariates) > 0) {
      form <- eval(parse(text = paste0("~", paste0(covariates, collapse = "+"))))
    } else {
      form <- NULL
    }

    if (options[["groupingVariable"]] == "") {
      result <- try(BFpack::cor_test(dataset, formula = form))

    } else {
      groupName <- decodeColNames(options[["groupingVariable"]])
      levs <- levels(dataset[[groupName]])
      dataNames <- vector("character", 0L)
      for (i in 1:length(levs)) {
        if (length(covariates) > 0) {
          select <- c(decodeColNames(options[["variables"]]), covariates)
        } else {
          select <- c(decodeColNames(options[["variables"]]))
        }
        dtmp <- subset(dataset,
                       subset = eval(parse(text = groupName)) == levs[i],
                       select = select)
        # create the data frames in the environment
        assign(paste0("dtGroup", i), dtmp)
        dataNames <- c(dataNames, paste0("dtGroup", i))
      }
      dataString <- paste0(dataNames, collapse = ",")
      # this is a bit hacky, because cor_test takes multiple data frames names separated by commas,
      # but we do not know how many...
      cor_test_call <- paste("try(BFpack::cor_test(", dataString, ", formula = ", paste0(form, collapse = ""), "))", sep = "")

      result <- eval(parse(text = cor_test_call))
    }

  # regression
  } else if (type %in%  c("regression", "regressionLogistic")) {

    dependent <- decodeColNames(unlist(options[["dependent"]]))
    covariates <- decodeColNames(unlist(options[["covariates"]]))
    ncov <- length(covariates)
    covariateString <- paste0(covariates, collapse = "+")
    # handle the interactions
    iastring <- .bfpackUnwrapInteractions(options)
    if (!is.null(iastring)) {
      covariateString <- paste0(covariateString, "+", iastring)
    }

    if (length(dependent) > 1) { # means  multivariate linear regression
      depString <- paste0("cbind(", paste0(dependent, collapse = ","), ")")
    } else {
      depString <- dependent
    }

    formula <- as.formula(paste0(depString, "~", covariateString))

    if (type == "regression") {
      result <- try(lm(formula, data = dataset))
    } else if (type == "regressionLogistic") {
      # TODO: more families?
      result <- try(glm(formula, data = dataset, family = "binomial"))
    }

  } else if (type == "anova") {

    dependent <- decodeColNames(options[["dependent"]])
    factorVar <- decodeColNames(options[["fixedFactors"]])
    covariates <- decodeColNames(options[["covariates"]])

    # treat the independent variables
    istring <- paste0(factorVar, collapse = " + ")
    if (length(covariates) > 0) { # ANCOVA
      covString <- paste0(covariates, collapse = " + ")
      istring <- paste0(istring, " + ", covString)
    }

    # handle the interactions
    iastring <- .bfpackUnwrapInteractions(options)
    if (!is.null(iastring)) {
      istring <- paste0(istring, "+", iastring)
    }

    if (length(dependent) == 1) { # ANOVA

      formula <- as.formula(paste0(dependent, "~", istring, "-1"))
      result <- try(aov(formula, data = dataset))

    } else { # manova
      formula <- as.formula(paste0("cbind(", paste0(dependent, collapse = ","), ") ~ ", istring, "-1"))
      result <- try(manova(formula, data = dataset))
    }

  } else if (type == "variances") {

    variable <- decodeColNames(options[["variables"]])
    grouping <- decodeColNames(options[["groupingVariable"]])

    result <- try(BFpack::bartlett_test(dataset[, variable], dataset[, grouping]))

  } else if (type == "independentTTest") {

    variable <- decodeColNames(options[["variables"]])
    grouping <- decodeColNames(options[["groupingVariable"]])
    levels <- levels(dataset[[grouping]])
    # take only the first two levels, give a table footnote if there are more than two levels
    g1 <- levels[1]
    g2 <- levels[2]
    group1 <- dataset[dataset[[grouping]] == g1, variable]
    group2 <- dataset[dataset[[grouping]] == g2, variable]
    result <- try(bain::t_test(x = group1, y = group2, paired = FALSE, var.equal = FALSE,
                               conf.level = options[["ciLevel"]], mu = options[["muValue"]]))

  } else if (type == "onesampleTTest") {

    variable <- decodeColNames(options[["variables"]])
    result <- try(bain::t_test(x = dataset[, variable], paired = FALSE, var.equal = FALSE,
                               conf.level = options[["ciLevel"]],
                               mu = options[["muValue"]]))

  } else if (type == "pairedTTest") {
    variables <- decodeColNames(unlist(unlist(options[["pairs"]])))
    result <- try(bain::t_test(x = dataset[, variables[1]], y = dataset[, variables[2]],
                               paired = TRUE, var.equal = FALSE,
                               conf.level = options[["ciLevel"]],
                               mu = options[["muValue"]]))

  } else if (type == "multiSampleTTest") {

    testValues <- sapply(options[["testValues"]], function(x) x[["testValue"]])
    variables <- decodeColNames(options[["variables"]])
    result <- try(BFpack::mvt_test(X = dataset[, variables], conf.level = options[["ciLevel"]],
                  null = testValues))

  }

  if (isTryError(result)) {
    bfpackContainer$setError(gettextf("The parameter estimation failed. Error message: %1$s", jaspBase::.extractErrorMessage(result)))
    return()
  }

  # save in jaspResults, which is where bfpackContainer is stored.
  # this way we do not have to estimate the parameters twice
  estimatesState <- createJaspState(result)
  estimatesState$dependOn(deps) # are there any new dependencies not already covered in the container?
  bfpackContainer[["estimatesState"]] <- estimatesState

  # the estimate names for the JASP GUI
  estimateNames <- eval(parse(text = callString))(result)

  estimateNames <- as.list(names(estimateNames$estimate))

  # new dependencies for the qml source since it is not part of bfpackContainer but jaspResults
  deps2 <- switch(type,
                 "independentTTest" = c("variables", "groupingVariable"),
                 "pairedTTest" = "pairs",
                 "onesampleTTest" = "variables",
                 "anova" = c("dependent", "fixedFactors", "covariates"),
                 "regression" = c("dependent", "covariates"),
                 "correlation" = c("variables", "groupingVariable"),
                 "variances" = c("variables", "groupingVariable"),
                 "regressionLogistic" = c("dependent", "covariates"))

  namesForQml <- createJaspQmlSource("estimateNamesForQml", estimateNames)
  namesForQml$dependOn(deps2)
  # apparently the source only works directly with jaspResults
  jaspResults[["estimateNamesForQml"]] <- namesForQml

  return()
}

# compute the posteriors and BFs
# TODO: manual and standard prior
.bfpackComputeResults <- function(dataset, options, bfpackContainer, ready, type) {

  if (!is.null(bfpackContainer[["resultsContainer"]][["resultsState"]])) return()
  if (!ready) return()

  # create a container because both the results and the tables depending on them have the same dependencies
  # start with common deps, and then do the switch
  deps <- c("complement", "logScale", "manualHypotheses", "priorProbManual",
            "priorProbStandard", "priorProbStandard2", "priorProbStandard3",
            "priorProbComplement", "seed", "bfType", "includeHypothesis")

  specDeps <- switch(type,
                     "regression" = c("interactionTerms", "includeInteractionEffect"),
                     "anova" = c("interactionTerms", "includeInteractionEffect", "priorProbMainZero",
                                 "priorProbMainNonZero", "priorProbInteractionZero", "priorProbInteractionNonZero"),
                     "regressionLogistic" = c("interactionTerms", "includeInteractionEffect"),
                     NULL)
  deps <- c(deps, specDeps)

  resultsContainer <- createJaspContainer()
  resultsContainer$dependOn(optionsFromObject = bfpackContainer[["estimatesState"]], options = deps)

  bfpackContainer[["resultsContainer"]] <- resultsContainer

  if (!is.null(bfpackContainer[["estimatesState"]])) {
    estimates <- bfpackContainer[["estimatesState"]]$object

    # standard hypotheses priors
    if (type %in% c("variances", "multiSampleTTest")) {
      standPrior <- sapply(parse(text = c(options[["priorProbStandard"]], options[["priorProbStandard2"]])), eval)
    } else if (type == "anova") {
      standPrior <- list(sapply(parse(text = c(options[["priorProbStandard"]], options[["priorProbStandard2"]],
                                          options[["priorProbStandard3"]])), eval),
                         sapply(parse(text = c(options[["priorProbMainZero"]],
                                               options[["priorProbMainNonZero"]])), eval),
                         sapply(parse(text = c(options[["priorProbInteractionZero"]],
                                               options[["priorProbInteractionNonZero"]])), eval)
                         )
    } else {
      standPrior <- sapply(parse(text = c(options[["priorProbStandard"]], options[["priorProbStandard2"]],
                                          options[["priorProbStandard3"]])), eval)
    }

    # check if there are manual hypotheses
    manualHypInclude <- sapply(options[["manualHypotheses"]], function(x) x[["includeHypothesis"]])
    manualHyp <- sapply(options[["manualHypotheses"]], function(x) x[["hypothesisText"]])
    manualPrior <- sapply(options[["manualHypotheses"]], function(x) x[["priorProbManual"]])

    # keep the hypotheses that are included
    manualHyp <- manualHyp[manualHypInclude]
    if (length(manualHyp) == 0) {
      manualHyp <- NULL
      manualPrior <- NULL
    } else {
      manualPrior <- manualPrior[manualHypInclude]
      manualHyp <- paste(manualHyp, collapse = ";")
      if (options[["complement"]]) manualPrior <- c(manualPrior, options[["priorProbComplement"]])
      # convert the prior character values to numeric:
      manualPrior <- sapply(manualPrior, function(x) eval(parse(text=x)))

      # special treatment for variances, see if levels of the grouping variable start with a numeric charaacter, which would crash BFpack
      findex <- which(sapply(dataset, is.factor))
      if (length(findex > 0)) {
        if (type == "variances") {
          # there is only one factor for variances
          levs <- levels(dataset[, findex])
          if (any(grepl("^[0-9]", levs))) {
            jaspBase:::.quitAnalysis(gettext("BFpack does not accept factor levels that start with a number. Please remove the numbers from your factor levels to continue."))
          }
        }
      }

    }

    # BF.type depends in the analysis as well
    # seems that except for the correlation and variance, all other models have the adjusted bftype option
    if (!is.null(options[["bfType"]])) {
      if (options[["bfType"]] == "fractional") {
        bftype <- "FBF"
      } else {
        bftype <- "AFBF"
      }
    }

    results <- try(BFpack::BF(estimates, hypothesis = manualHyp,
                             complement = options[["complement"]],
                             prior.hyp.conf = manualPrior,
                             prior.hyp.explo = standPrior,
                             log = options[["logScale"]],
                             BF.type = bftype))

    if (isTryError(results)) {

      if (grepl("parse_hyp$hyp_mat", results, fixed = TRUE)) {
        bfpackContainer$setError(gettext("BFpack failed because of an issue with the specification of the manual hypotheses. Please check that you specified them correctly. You probably have to refresh the analysis."))
      } else {
        bfpackContainer$setError(gettextf("BFpack failed with the following error message: %1$s", jaspBase::.extractErrorMessage(results)))
      }

    }

    # now saving the results

    resultsState <- createJaspState(results)
    resultsContainer[["resultsState"]] <- resultsState
  }

  return()
}


####### TABLES #######
# table for the posterior probabilities of the parameter estimates
.bfpackParameterTable <- function(options, bfpackContainer, type, dataset, position) {

  # the parameterTable does go into the outer container given it does not depend on the options for the
  # inner container
  if (!is.null(bfpackContainer[["parameterTable"]])) return()

  parameterTable <- createJaspTable(gettext("Posterior probabilities when testing individual parameters"))
  parameterTable$dependOn(optionsFromObject = bfpackContainer[["resultsContainer"]], options = "priorProb")
  parameterTable$position <- position

  if (type %in% c("variances", "multiSampleTTest")) {

    if (type == "variances") {
      title1 <- gettext("Homogeneity of variances")
      title2 <- gettext("No homogeneity of variances")
    } else if (type == "multiSampleTTest") {
      # testValues <- sapply(options[["testValues"]] function(x) x[["testValue"]]))
      title1 <- gettext("Pr(=test values)")
      title2 <- gettext("Pr(≠test values)")
    }
    parameterTable$addColumnInfo(name = "equal", type = "number", title = title1)
    parameterTable$addColumnInfo(name = "unequal", type = "number", title = title2)

  } else {
    if (type %in% c("onesampleTTest", "pairedTTest", "independentTTest")) {
      title1 <- gettextf("Pr(=%1$s)", options[["muValue"]])
      title2 <- gettextf("Pr(<%1$s)", options[["muValue"]])
      title3 <- gettextf("Pr(>%1$s)", options[["muValue"]])
    } else {
      title1 <- gettext("Pr(=0)")
      title2 <- gettext("Pr(<0)")
      title3 <- gettext("Pr(>0)")

      if (type == "correlation" && options[["groupingVariable"]] != "") {
        groupName <- options[["groupingVariable"]]
        levs <- levels(dataset[[groupName]])
        footnote <- ""
        for (i in 1:length(levs)) {
          footnote <- gettextf("%1$sGroup %2$s corresponds to level %3$s in variable %4$s. ", footnote, paste0("g", i), levs[i], groupName)
        }
        parameterTable$addFootnote(footnote)
      }
    }

    parameterTable$addColumnInfo(name = "coefficient", type = "string", title = "")
    parameterTable$addColumnInfo(name = "equal", type = "number", title = title1)
    parameterTable$addColumnInfo(name = "smaller", type = "number", title = title2)
    parameterTable$addColumnInfo(name = "larger", type = "number", title = title3)
  }


  # assigning the table to the container here means we already display an empty table even if it is not yet
  # filled with data
  bfpackContainer[["parameterTable"]] <- parameterTable

  if (!bfpackContainer$getError()) {
    parPhp <- bfpackContainer[["resultsContainer"]][["resultsState"]]$object$PHP_exploratory
    if (!is.null(parPhp)) {
      if (type %in% c("variances", "multiSampleTTest")) {
        dtFill <- data.frame(equal = parPhp[1], unequal = parPhp[2])
      } else {
        dtFill <- data.frame(coefficient = rownames(parPhp))
        dtFill[, c("equal", "smaller", "larger")] <- parPhp
      }
      parameterTable$setData(dtFill)
    }

  }

  if (type == "independentTTest") {
    levels <- levels(dataset[[options[["groupingVariable"]]]])
    if (length(levels) > 2) {
      parameterTable$addFootnote(gettext("The number of factor levels in the grouping is greater than 2.
                                         Only the first two levels were used."))
    }
  }

  return()
}


# table for the posterior probabilities of the effects for anova models
.bfpackMainEffectsTable <- function(options, bfpackContainer, type, position) {

  # the mainEffectsTable does go into the outer container given it does not depend on the options for the
  # inner container
  if (!is.null(bfpackContainer[["mainEffectsTable"]])) return()

  mainEffectsTable <- createJaspTable(gettext("Posterior probabilities for main effects"))
  mainEffectsTable$dependOn(optionsFromObject = bfpackContainer[["resultsContainer"]])
  mainEffectsTable$position <- position

  mainEffectsTable$addColumnInfo(name = "coefficient", type = "string", title = "")
  mainEffectsTable$addColumnInfo(name = "noEffect", type = "number", title = gettext("Pr(no effect)"))
  mainEffectsTable$addColumnInfo(name = "fullModel", type = "number", title = gettext("Pr(full model)"))

  bfpackContainer[["mainEffectsTable"]] <- mainEffectsTable

  if (!bfpackContainer$getError()) {
    php <- bfpackContainer[["resultsContainer"]][["resultsState"]]$object$PHP_main
    if (!is.null(php)) {
      dtFill <- data.frame(coefficient = rownames(php))
      dtFill[, c("noEffect", "fullModel")] <- php
      mainEffectsTable$setData(dtFill)
    }

  }

  return()
}

# table for the posterior probabilities of the effects for anova models
.bfpackInteractionEffectsTable <- function(options, bfpackContainer, type, position) {

  # the iaEffectsTable does go into the outer container given it does not depend on the options for the
  # inner container
  if (!is.null(bfpackContainer[["iaEffectsTable"]])) return()

  iaEffectsTable <- createJaspTable(gettext("Posterior probabilities for interaction effects"))
  iaEffectsTable$dependOn(optionsFromObject = bfpackContainer[["resultsContainer"]])
  iaEffectsTable$position <- position

  iaEffectsTable$addColumnInfo(name = "coefficient", type = "string", title = "")
  iaEffectsTable$addColumnInfo(name = "noEffect", type = "number", title = gettext("Pr(no effect)"))
  iaEffectsTable$addColumnInfo(name = "fullModel", type = "number", title = gettext("Pr(full model)"))

  if (!bfpackContainer$getError()) {
    php <- bfpackContainer[["resultsContainer"]][["resultsState"]]$object$PHP_interaction
    if (!is.null(php)) {
      bfpackContainer[["iaEffectsTable"]] <- iaEffectsTable

      dtFill <- data.frame(coefficient = rownames(php))
      dtFill[, c("noEffect", "fullModel")] <- php
      iaEffectsTable$setData(dtFill)
    }
  }

  return()
}


# Create a legend containing the order constrained hypotheses
.bfpackLegendTable <- function(options, type, bfpackContainer, position) {

  if (!is.null(bfpackContainer[["legendTable"]])) return()

  legendTable <- createJaspTable(gettext("Manual hypotheses legend"))

  legendTable$dependOn("manualHypotheses")
  legendTable$position <- position
  legendTable$addColumnInfo(name = "number", type = "string", title = "")
  legendTable$addColumnInfo(name = "hypothesis", type = "string", title = gettext("Hypothesis"))

  if (!bfpackContainer$getError()) {
    # if there is a string in the hypotheses field but the suer forgot to check the include box we want an empty table
    # with a footnote
    manualHyp <- sapply(options[["manualHypotheses"]], function(x) x[["hypothesisText"]])
    manualHypInclude <- sapply(options[["manualHypotheses"]], function(x) x[["includeHypothesis"]])
    if (paste0(manualHyp, collapse = "") != "" && !any(manualHypInclude)) {
      legendTable$addFootnote(gettext("Check the 'Include' box to test the hypothesis."))
      # putting the assignment to the container here means the table is only displayed if it is filled with data
      bfpackContainer[["legendTable"]] <- legendTable
    }

    hypos <- bfpackContainer[["resultsContainer"]][["resultsState"]]$object$hypotheses
    if (!is.null(hypos)) {
      for (i in seq_len(length(hypos))) {
        row <- list(number = gettextf("H%i", i), hypothesis = hypos[i])
        legendTable$addRows(row)
      }

      bfpackContainer[["legendTable"]] <- legendTable
    }
  }


  return()
}


# table for the posterior probabilities of the parameter estimates
.bfpackMatrixTable <- function(options, bfpackContainer, type, position) {

  if (!is.null(bfpackContainer[["resultsContainer"]][["matrixTable"]])) return()

  tbTitle <- ifelse(options[["logScale"]], gettext("Evidence matrix (log BFs)"), gettext("Evidence matrix (BFs)"))
  matrixTable <- createJaspTable(tbTitle)
  matrixTable$position <- position
  # matrixTable$dependOn()

  matrixTable$addColumnInfo(name = "hypothesis", title = "", type = "string")
  matrixTable$addColumnInfo(name = "H1", title = gettext("H1"), type = "number")

  if (!bfpackContainer$getError()) {
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

      bfpackContainer[["resultsContainer"]][["matrixTable"]] <- matrixTable
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

  if (!bfpackContainer$getError()) {
    php <- bfpackContainer[["resultsContainer"]][["resultsState"]]$object$PHP_confirmatory
    if (!is.null(php)) {
      for (i in seq_len(length(php))) {
        row <- list(hypothesis = gettextf("H%i", i), prob = php[i])
        postTable$addRows(row)
      }

      bfpackContainer[["resultsContainer"]][["postTable"]] <- postTable
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
  specTable$addColumnInfo(name = "PHP", title = gettext("Posterior prob."), type = "number")


  bfpackContainer[["resultsContainer"]][["specTable"]] <- specTable

  if (!bfpackContainer$getError()) {
    spec <- bfpackContainer[["resultsContainer"]][["resultsState"]]$object$BFtable_confirmatory
    if (!is.null(spec)) {
      dtFill <- data.frame(hypothesis = paste0(gettext("H"), seq(1:nrow(spec))))
      dtFill[, c("complex=", "complex>", "fit=", "fit>", "BF=", "BF>", "BF", "PHP")] <- spec[, 1:8]
      specTable$setData(dtFill)
    }

  }


  return()
}


.bfpackEstimatesTable <- function(options, bfpackContainer, type, position = 1) {
  if (!is.null(bfpackContainer[["coefContainer"]][["estimatesTable"]]) ||
      !options[["estimatesTable"]]) return()

  # create a container so the estimatesTable is added at the end of the output
  coefContainer <- createJaspContainer()
  coefContainer$dependOn(optionsFromObject = bfpackContainer[["estimatesState"]], options = c("estimatesTable", "ciLevel"))
  bfpackContainer[["coefContainer"]] <- coefContainer

  estimatesTable <- createJaspTable(gettext("Estimates table"))
  estimatesTable$position <- position

  interval <- gettextf("%s%% CI", format(100 * options[["ciLevel"]], digits = 3, drop0trailing = TRUE))
  intervalLow <- gettextf("%s lower bound", interval)
  intervalUp <- gettextf("%s upper bound", interval)


  estimatesTable$addColumnInfo(name = "coefficient", title = "", type = "string")
  estimatesTable$addColumnInfo(name = "mean", title = gettext("Mean"), type = "number")
  estimatesTable$addColumnInfo(name = "median", title = gettext("Median"), type = "number")
  estimatesTable$addColumnInfo(name = "lower", title = intervalLow, type = "number")
  estimatesTable$addColumnInfo(name = "upper", title = intervalUp, type = "number")


  bfpackContainer[["coefContainer"]][["estimatesTable"]] <- estimatesTable

  if (!bfpackContainer$getError()) {

    estimates <- bfpackContainer[["resultsContainer"]][["resultsState"]]$object$estimates
    #### TODO: estimates is NULL for independent TTest
    if (!is.null(estimates)) {
      dtFill <- data.frame(coefficient = rownames(estimates))

      dtFill[, c("mean", "median", "lower", "upper")] <- estimates


      if (options[["ciLevel"]] != .95) {

        fitObj <- bfpackContainer[["estimatesState"]]$object
        if (type %in% c("regression", "anova", "correlation", "regressionLogistic")) {

          if (type == "correlation") {
            draws <- fitObj$corrdraws[[1]]
            bounds <- apply(draws, c(2, 3), function(x) {
              coda::HPDinterval(coda::as.mcmc(x), prob = options[["ciLevel"]])
            })
            boundsLow <- bounds[1, , ]
            boundsUp <- bounds[2, , ]
            # kind of hope this structure never changes so the elements are always in the correct order
            boundsLow <- boundsLow[lower.tri(boundsLow)]
            boundsUp <- boundsUp[lower.tri(boundsUp)]

          } else if (type %in% c("regression", "regressionLogistic", "anova")) {

            int <- confint(fitObj, level = options[["ciLevel"]])
            boundsLow <- int[, 1]
            boundsUp <- int[, 2]
          }

        } else if (type %in% c("onesampleTTest", "pairedTTest", "independentTTest")) {

          int <- fitObj[["conf.int"]]
          boundsLow <- int[1]
          boundsUp <- int[2]

        } else if (type == "variances") {
          int <- .bfpackBartlettHelper(fitObj, options[["ciLevel"]])
          boundsLow <- int[, 1]
          boundsUp <- int[, 2]

        } else if (type == "multiSampleTTest") {
          int <- .bfpackMultiTTestHelper(fitObj, options[["ciLevel"]])
          boundsLow <- int[, 1]
          boundsUp <- int[, 2]
        }


        dtFill[, c("lower", "upper")] <- cbind(boundsLow, boundsUp)
      }

      estimatesTable$setData(dtFill)
      # footnt <- ifelse(type == "correlation",
      #                  gettext("The uncertainty interval is a highest posterior density credible interval."),
      #                  gettext("The uncertainty interval is a confidence interval."))
      # estimatesTable$addFootnote(footnt)
    }
  }

  return()
}



####### PLOTS ########

.bfpackPriorPosteriorPlot <- function(options, bfpackContainer, type, position = 1) {
  if (!is.null(bfpackContainer[["plotContainer"]][["priorPlot"]]) || !options[["plots"]]) {
    return()
  }

  plotContainer <- createJaspContainer(gettext("Prior and Posterior Probabilities"))
  plotContainer$dependOn(optionsFromObject = bfpackContainer[["resultsContainer"]])
  bfpackContainer[["plotContainer"]] <- plotContainer

  result <- bfpackContainer[["resultsContainer"]][["resultsState"]]$object

  if (!bfpackContainer$getError() && !is.null(result$PHP_confirmatory)) {
    post <- result$PHP_confirmatory
    prior <- result$prior.hyp.conf

    priorPlot <- .plotHelper(prior, gettext("Prior probabilities"))
    plotContainer[["priorPlot"]] <- priorPlot

    postPlot <- .plotHelper(post, gettext("Posterior probabilities"))
    plotContainer[["postPlot"]] <- postPlot

  }

}

.plotHelper <- function(probs, name) {

  dat <- data.frame(y = probs, group = paste0(gettext("H"), seq_len(length(probs))))

  pl <- ggplot2::ggplot(data = dat, mapping = ggplot2::aes(x = "", y = y, fill = group)) +
    ggplot2::geom_bar(width = 1, stat = "identity", show.legend = TRUE, color = "black", linewidth = 1.5) +
    ggplot2::coord_polar(direction = -1, theta = "y") +
    jaspGraphs::getEmptyTheme() +
    ggplot2::scale_fill_discrete(name = gettext("Hypothesis")) +
    ggplot2::theme(legend.key.size = ggplot2::unit(1, 'cm'), #change legend key size
          legend.key.height = ggplot2::unit(1, 'cm'), #change legend key height
          legend.key.width = ggplot2::unit(1, 'cm'), #change legend key width
          legend.title = ggplot2::element_text(size=14), #change legend title font size
          legend.text = ggplot2::element_text(size=14)) #change legend text font size

  out <- createJaspPlot(pl, title = name, width = 250, height = 250)

  return(out)
}

