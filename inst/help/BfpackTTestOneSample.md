BFpack One Sample T-Test
==========================

The analysis allows to test exploratory hypotheses (e.g., equal vs negative vs positive) and confirmatory hypotheses (with equality and/or order constraints) using Bayes factors and posterior probabilities under commonly used statistical models. For the one sample t-test that means one can test hypotheses relating to mean of a group on a single continuous variable. For details, see Mulder et al. (2021).

## Input
### Main Window
- Variable: Input a variable that is continuous (if they are not, they will be treated as such)

#### Standard Hypothesis Test
- Hypotheses: Test the hypothesis that the parameter is equal to, smaller than, or larger than a specific value; for t-test the parameter is called ‘mu’ and denotes mean of the variable.
- Test Value: Test value for the mean parameter. The default test value is 0.
- Prior Weights: Specify how to weigh each hypothesis. The default corresponds to a standard setting when testing a two-sided hypothesis test where the null hypothesis has an equal prior weight as the two-sided alternative hypothesis. Because the two-sided alternative is split to the left side and right side, the default prior weight of the null (H0) is 2, and each prior weight for the left-sided and right-sided hypotheses (H1 and H2, respectively) is 1.

#### Parameters
This box contains the names (labels) of the parameters on which equality/one-sided constraints can be formulated in the ‘manual hypothesis test’ box. For an independent samples t-test, the only parameter on which constraints can be formulated is the ‘mu’.

#### Manual Hypothesis Test
- Hypotheses: Specify a manual hypothesis with equality and/or one-sided constraints on the parameters; see the tooltip for more info. Specify the prior weights and do not forget to check the include box to test the hypothesis. For the independent samples t-test this could be something like "1>mu>-1", which would correspond to an interval hypotheses where the mean deviates maximally 1 from 0 in absolute sense.
- Use The "+" To Add More Hypotheses.
- Complement: The complement hypothesis (which covers the range of the parameters that are not covered by the above specified hypotheses); prior weight and include.

### Options
#### Bayes Factor
- Log Scale: Reports the natural logarithm of the Bayes factors.
- Bayes Factor Type: The default is the fractional BF, alternatively choose the adjusted fractional BF. The adjusted fractional BF was proposed in the case all hypotheses of interest only contain one-sided (order) constraints.

#### Tables
- BFs for standard hypothesis test: Print a table that compares each standard hypothesis with its complement.
- BFs: Manual Hypotheses: Print the specification table with different parts of the (Savage-Dickey) Bayes factors.
- Estimates With Uncertainty Interval: Print a table with the point estimates and uncertainty intervals (default credibility intervals for the t-test) for the parameter(s) of interest. 

#### Plots
- Manual Hypothesis Plots: Produces plots depicting the prior and posterior probabilities of the manual hypotheses.

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
- Denotes the manual hypotheses.

#### Evidence Matrix (BFs)
- BF matrix with the hypotheses: If the BF for H1 vs H2 is smaller than 1, evidence is in favor of H2, if it is larger than 1 evidence is in favor of H1. If “Log scale” is checked, the printed BFs are on a natural logarithm scale.

#### Posterior Model Probabilities for the Manual Hypothesis Test
- Prints the posterior probabilities for each hypothesis for the manual hypothesis test.

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
- Posterior mean, median, and CrI bounds using noninformative (Jeffreys) priors. For the t-test, they are identical to the classical estimates and confidence intervals.

### Plots
#### Prior And Posterior Probability 
- Pizza plots for the manual hypotheses

### References

- Mulder, J. (2014). Prior adjusted default Bayes factors for testing (in) equality constrained hypotheses. Computational Statistics & Data Analysis, 71, 448-463. https://doi.org/10.1016/j.csda.2013.07.017
- Mulder, J., Williams, D. R., Gu, X., Tomarken, A., Böing-Messing, F., Olsson-Collentine, A., Meijerink, M., Menke, J., Fox, J.-P., Hoijtink, H., Rosseel, Y., Wagenmakers, E.J., and van Lissa, C. (2021). BFpack: Flexible Bayes Factor Testing of Scientific Theories in R. *Journal of Statistical Software, 100*(18), 1-63. https://doi.org/10.18637/jss.v100.i18
- O'Hagan, A. (1995). Fractional Bayes factors for model comparison. Journal of the Royal Statistical Society: Series B (Methodological), 57(1), 99-118. https://doi.org/10.1111/j.2517-6161.1995.tb02017.x
