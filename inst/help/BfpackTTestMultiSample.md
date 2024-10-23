BFpack Multivariate T-Test
==========================

The analysis allows to test exploratory hypotheses (e.g., equal vs negative vs positive) and confirmatory hypotheses (with equality and/or order constraints) using Bayes factors and posterior probabilities under commonly used statistical models. For the multivariate t-test that means one can test hypotheses on the means of multiple variables simultaneously. For details, see Mulder et al. (2021).

## Input
### Main Window
- Variables: Input variables that are continuous (if they are not, they will be treated as such).

#### Standard Hypothesis Test
- Hypotheses: Test the hypothesis that the means of the variables are equal or unequal to the test values.
- Prior Weights: Specify how to weigh the null hypothesis and the multivatriate two-sided alternative. The default sets equal prior weights for both hypotheses. 
- Test Values: Test values to for the variable means. The default test values are 0.

#### Parameters
This box contains the names (labels) of the parameters on which equality/one-sided constraints can be formulated in the ‘manual hypothesis test’ box. For a multivariate t-test, the parameters are the means of the variables which are labeled as the names of the variables.

#### Manual Hypothesis Test
- Hypotheses: Specify a manual hypothesis with equality and/or one-sided constraints on the parameters; see the tooltip for more info. Specify the prior weights and do not forget to check the include box to test the hypothesis. For the multivariate t-test this could be something like "var1Name > var2Name".
- Use The "+" To Add More Hypotheses.
- Complement: The complement hypothesis (which covers the range of the parameters that are not covered by the above specified hypotheses); prior weight and include.

### Options
#### Bayes Factor
- Log Scale: Reports the natural logarithm of the Bayes factors.
- Bayes Factor Type: The default is the fractional BF, alternatively choose the adjusted fractional BF. The adjusted fractional BF was proposed in the case all hypotheses of interest only contain one-sided (order) constraints.

#### Tables
- BFs for standard hypothesis test: Print a table that compares each standard hypothesis with its complement.
- BFs: Manual Hypotheses: Print the specification table with different parts of the (Savage-Dickey) Bayes factors.
- Estimates with uncertainty interval: Print a table with the point estimates and uncertainty intervals (default credibility intervals for the multivariate t-test) for the parameter(s) of interest.

#### Plots
- Manual hypothesis plots: Produces plots depicting the prior and posterior probabilities of the manual hypotheses

#### Additional options: 
- Seed

## Output

### Tables
#### Posterior probabilites when testing standard hypotheses
- Posterior probabilities for the standard hypotheses

#### BFs: Standard Hypotheses Table
- BF(0u): Bayes factor of the standard H0 vs the unconstrained hypothesis
- BF(-u): Bayes factor of the standard H- vs the unconstrained hypothesis
- BF(+u): Bayes factor of the standard H+ vs the unconstrained hypothesis
- BF(u0): Bayes factor of the unconstrained hypothesis vs the standard H0
- BF(u-): Bayes factor of the unconstrained hypothesis vs the standard H-
- BF(u+): Bayes factor of the unconstrained hypothesis vs the standard H+

#### Manual hypotheses legend
- Denotes the manual hypotheses

#### Evidence matrix (BFs)
- BF matrix with the hypotheses: If the BF for H1 vs H2 is smaller than 1, evidence is in favor of H2, if it is larger than 1 evidence is in favor of H1. If “Log scale” is checked, the printed BFs are on a natural logarithm scale.

#### Posterior model probabilities for the manual hypothesis test
- Prints the posterior probabilities for each hypothesis in the manual hypothesis test.

#### BFs: Manual hypotheses table
- Equal-complex: Quantifies the relative complexity of the equality constraints of a hypothesis (the prior density at the equality constraints in the extended Savage Dickey density ratio)
- Order-complex: Quantifies the relative complexity of the order constraints of a hypothesis (the prior probability of the order constraints in the extended Savage Dickey density ratio)
- Equal-fit: Quantifies the relative fit of the equality constraints of a hypothesis (the posterior density at the equality constraints in the extended Savage Dickey density ratio)
- Order-fit: quantifies the relative fit of the order constraints of a hypothesis (the posterior probability of the order constraints in the extended Savage Dickey density ratio)
- Equal-BF: contains the Bayes factor of the equality constraints against the unconstrained hypothesis
- Order-BF: contains the Bayes factor of the order constraints against the unconstrained hypothesis
- BF: contains the Bayes factor of the constrained hypothesis against the unconstrained hypothesis
- Posterior prob.: contains the posterior probabilities of the hypotheses

#### Estimates table:
- Posterior means, medians, and CrI bounds using noninformative (Jeffreys) priors.

### Plots
#### Prior and posterior probability 
- Pizza plots for the manual hypotheses

### References

- Mulder, J., & Gu, X. (2022). Bayesian testing of scientific expectations under multivariate normal linear models. Multivariate Behavioral Research, 57(5), 767-783. https://doi.org/10.1080/00273171.2021.1904809
- Mulder, J., Williams, D. R., Gu, X., Tomarken, A., Böing-Messing, F., Olsson-Collentine, A., Meijerink, M., Menke, J., Fox, J.-P., Hoijtink, H., Rosseel, Y., Wagenmakers, E.J., and van Lissa, C. (2021). BFpack: Flexible Bayes Factor Testing of Scientific Theories in R. *Journal of Statistical Software, 100*(18), 1-63. https://doi.org/10.18637/jss.v100.i18
- O'Hagan, A. (1995). Fractional Bayes factors for model comparison. Journal of the Royal Statistical Society: Series B (Methodological), 57(1), 99-118. https://doi.org/10.1111/j.2517-6161.1995.tb02017.x
