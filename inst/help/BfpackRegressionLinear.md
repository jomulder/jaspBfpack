BFpack Linear (Multivariate) Regression
==========================

The analysis allows to test exploratory hypotheses (e.g., equal vs negative vs positive) and confirmatory hypotheses (with equality and/or order constraints) using Bayes factors and posterior probabilities under commonly used statistical models. For the linear regression model that means one can test hypotheses relating to the regression coefficients on the same dependent variable or across different dependent variables. For details, see Mulder et al. (2021).

## Input
### Main Window
- Dependent Variables: Input one or more variables that are continuous (if they are not, they will be treated as such); if there is more than one DV, a multivariate multiple regression is considered.
- Covariates: Predictors for the regression(s); they can have any scaling, that is, continuous, ordinal, nominal. In the case of ordinal and nominal predictor variables, dummy variables are automatically created.

#### Standard Hypothesis Test
- Hypotheses: Test the hypotheses that each separate parameter (‘beta’) is equal to, smaller than, or larger than 0.
- Prior Weights: Specify how to weigh each hypothesis. The default corresponds to a standard setting when testing a two-sided hypothesis test where the null hypothesis has an equal prior weight as the two-sided alternative hypothesis. Because the two-sided alternative is split to the left side and right side, the default prior weight of the null (H0) is 2, and each prior weight for the left-sided and right-sided hypotheses (H1 and H2, respectively) is 1.

#### Parameters
This box contains the names (labels) of the parameters on which equality/one-sided constraints can be formulated in the ‘manual hypothesis test’ box. The names of the effects depend on the names of the predictors/covariates and the dependent variable (in case multiple dependent variables are included).

#### Manual Hypothesis Test
- Hypotheses: Specify a manual hypothesis with equality and/or one-sided constraints on the parameters; see the tooltip for more info. Specify the prior weights and do not forget to include each hypothesis via the check box. For a regression model, this could be something like "var1 > var2 > var2" implying that the effect of variable ‘var1’ is larger than the effect of ‘var2’, and the effect of ‘var2’ is larger than the effect of ‘var3’ on the dependent variable. Or (in the case of two dependent variables ‘dv1’ and ‘dv2’), “var1_on_dv1> var1_on_dv2” implies that the effect of ‘var1’ on dependent variable ‘dv1’ is larger than the effect of ‘var1’ on dependent variable ‘dv2’.
- Use The "+" To Add More Hypotheses
- Complement: The complement hypothesis (which covers the range of the parameters that are not covered by the above specified hypotheses); prior weight and include.

### Options
#### Bayes Factor
- Log Scale: Reports the natural logarithm of the Bayes factors.
- Bayes Factor Type: The default is the fractional BF, alternatively choose the adjusted fractional BF. The adjusted fractional BF was proposed in the case all hypotheses of interest only contain one-sided (order) constraints.

#### Tables
- BFs for standard hypothesis test: Print a table that compares each standard hypothesis with its complement.
- BFs: Manual Hypotheses: Print the specification table with different parts of the (Savage-Dickey) Bayes factors.
- Estimates With Uncertainty Interval: Print a table with the point estimates and uncertainty intervals (default credibility intervals for regression models) for all regression effects.

#### Plots
- Manual Hypothesis Plots: Produces plots depicting the prior and posterior probabilities of the manual hypotheses.

#### Additional Options
- Seed

#### Interaction Terms
- Box that displays possible two-way interaction terms (if there is more than one covariate); they are by default included in the analysis.

## Output

### Tables
#### Posterior Probabilities When Testing Standard Hypotheses
- Posterior probabilities for the standard hypotheses.

#### BFs: Standard Hypotheses Table
- BF(0u): Bayes factor of the standard H0 vs the unconstrained hypothesis
- BF(-u): Bayes factor of the standard H- vs the unconstrained hypothesis
- BF(+u): Bayes factor of the standard H+ vs the unconstrained hypothesis
- BF(u0): Bayes factor of the unconstrained hypothesis vs the standard H0
- BF(u-): Bayes factor of the unconstrained hypothesis vs the standard H-
- BF(u+): Bayes factor of the unconstrained hypothesis vs the standard H+

#### Manual Hypotheses Legend
- Denotes the manual hypotheses.

#### Evidence Matrix (BFs)
- BF matrix with the hypotheses: If the BF for H1 vs H2 is smaller than 1, evidence is in favor of H2, if it is larger than 1 evidence is in favor of H1. If “Log scale” is checked, the printed BFs are on a natural logarithm scale.

#### Posterior Model Probability
- Prints the posterior probabilityies for each hypothesis for the manual hypothesis test.

#### BFs: Manual Hypotheses Table
- Equal-Complex: Quantifies the relative complexity of the equality constraints of a hypothesis (the prior density at the equality constraints in the extended Savage Dickey density ratio)
- Order-Complex: Quantifies the relative complexity of the order constraints of a hypothesis (the prior probability of the order constraints in the extended Savage Dickey density ratio)
- Equal-Fit: Quantifies the relative fit of the equality constraints of a hypothesis (the posterior density at the equality constraints in the extended Savage Dickey density ratio)
- Order-Fit: Quantifies the relative fit of the order constraints of a hypothesis (the posterior probability of the order constraints in the extended Savage Dickey density ratio)
- Equal-BF: Contains the Bayes factor of the equality constraints against the unconstrained hypothesis
- Order-BF: Contains the Bayes factor of the order constraints against the unconstrained hypothesis
- BF: Contains the Bayes factor of the constrained hypothesis against the unconstrained hypothesis
- Posterior Prob.: Contains the posterior probabilities of the hypotheses

#### Estimates Table
- Posterior means, medians, and CrI bounds using noninformative (Jeffreys) priors. 

### Plots
#### Prior And Posterior Probability 
- Pizza plots for the manual hypotheses

### References

- Mulder, J., & Gu, X. (2022). Bayesian testing of scientific expectations under multivariate normal linear models. Multivariate Behavioral Research, 57(5), 767-783. [https://doi.org/10.1080/02664763.2021.1992360](https://doi.org/10.1080/00273171.2021.1904809)
- Mulder, J., Williams, D. R., Gu, X., Tomarken, A., Böing-Messing, F., Olsson-Collentine, A., Meijerink, M., Menke, J., Fox, J.-P., Hoijtink, H., Rosseel, Y., Wagenmakers, E.J., and van Lissa, C. (2021). BFpack: Flexible Bayes Factor Testing of Scientific Theories in R. *Journal of Statistical Software, 100*(18), 1-63. https://doi.org/10.18637/jss.v100.i18.
- O'Hagan, A. (1995). Fractional Bayes factors for model comparison. Journal of the Royal Statistical Society: Series B (Methodological), 57(1), 99-118. https://doi.org/10.1111/j.2517-6161.1995.tb02017.x
