BFpack Variances (Bartlett Test)
==========================

The analysis allows to test exploratory hypotheses (e.g., equal vs negative vs positive) and confirmatory hypotheses (with equality and/or order constraints) using Bayes factors and posterior probabilities under commonly used statistical models. For the variances that means one can test hypotheses relating to the homogeneity of variances. For details, see Mulder et al. (2021).

## Input
### Main Window
- Variable: Input a continuous or ordinal variable (if they are not, they will be treated as such).
- Grouping Variable: Input variable that is nominal.

#### Standard Hypothesis Test
- Hypotheses: Test the hypothesis that the variances are equal or not.
- Prior Weights: Specify how to weigh each hypothesis. The default is that the null hypothesis (which assumes that the variances are equal) has an equal weight as the alternative.

#### Parameters
This box contains the names (labels) of the parameters on which equality/one-sided constraints can be formulated in the ‘manual hypothesis test’ box. For the variance test, the variance names depend on the labels of the grouping variable. The labels should not start with a number when formulating hypotheses in the manual hypothesis test.

#### Manual Hypothesis Test
- Hypotheses: Specify a manual hypothesis with equality and/or one-sided constraints on the parameters, see the tooltip for more info. Specify the prior weights and do not forget to include each hypothesis via the check box. For the variances this could be something like "label1 > label2 > label3" implying that the variance of the group with ‘label1’ is larger than the variance of the group with ‘label2’ which is larger than the variance of the group with ‘label3’.
- Use The "+" To Add More Hypotheses.
- Complement: The complement hypothesis (which covers the range of the parameters that are not covered by the above specified hypotheses); prior weight and include.

### Options
#### Bayes Factor
- Log Scale: Reports the natural logarithm of the Bayes factors.

#### Tables
- BFs for standard hypothesis test: Print a table that compares each standard hypothesis with its complement
- BFs: Manual Hypotheses: Print the specification table with different parts of the Bayes factors.
- Estimates With Uncertainty Interval: Print a table with the point estimates and uncertainty intervals (credibility interval for the variances) for the parameter(s) of interest.

#### Plots
- Manual Hypothesis Plots: Produces plots depicting the prior and posterior probabilities of the manual hypotheses

#### Additional Options
- Seed

## Output

### Tables
#### Posterior Probabilities When Testing Standard Hypotheses
- Posterior probabilities for the standard hypotheses

#### BFs: Standard Hypotheses Table
- BF(0u): Bayes factor of the standard H0 vs the unconstrained hypothesis
- BF(-u): Bayes factor of the standard H- vs the unconstrained hypothesis
- BF(+u): Bayes factor of the standard H+ vs the unconstrained hypothesis
- BF(u0): Bayes factor of the unconstrained hypothesis vs the standard H0
- BF(u-): Bayes factor of the unconstrained hypothesis vs the standard H-
- BF(u+): Bayes factor of the unconstrained hypothesis vs the standard H+

#### Manual Hypotheses Legend
- Denotes the manual hypotheses

#### Evidence Matrix (BFs)
- BF matrix with the hypotheses: If the BF for H1 vs H2 is smaller than 1, evidence is in favor of H2, if it is larger than 1 evidence is in favor of H1. If “Log scale” is checked, the printed BFs are on a natural logarithm scale.

#### Posterior Model Probabilities for the Manual Hypothesis Test
- Provides the posterior probability for each hypothesis for the manual hypothesis test.

#### BFs: Manual Hypotheses Table
- Equal-Complex: Not available when testing variances.
- Order-Complex: Quantifies the relative complexity of the order constraints of a hypothesis (the prior probability of the order constraints in the extended Savage Dickey density ratio)
- Equal-Fit: Not available when testing variances.
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

- Böing-Messing, F., & Mulder, J. (2018). Automatic Bayes factors for testing equality-and inequality-constrained hypotheses on variances. Psychometrika, 83(3), 586-617. https://doi.org/10.1007/s11336-018-9615-z
- Böing-Messing, F., van Assen, M. A., Hofman, A. D., Hoijtink, H., & Mulder, J. (2017). Bayesian evaluation of constrained hypotheses on variances of multiple independent groups. Psychological Methods, 22(2), 262. https://doi.org/10.1037/met0000116
- Mulder, J., Williams, D. R., Gu, X., Tomarken, A., Böing-Messing, F., Olsson-Collentine, A., Meijerink, M., Menke, J., Fox, J.-P., Hoijtink, H., Rosseel, Y., Wagenmakers, E.J., and van Lissa, C. (2021). BFpack: Flexible Bayes Factor Testing of Scientific Theories in R. *Journal of Statistical Software, 100*(18), 1-63. https://doi.org/10.18637/jss.v100.i18
