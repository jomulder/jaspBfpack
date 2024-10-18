BFpack Multivariate T-Test
==========================

The analysis allows to test exploratory hypotheses (e.g., equal vs negative vs positive) and confirmatory hypotheses (with equality and/or order constraints) using Bayes factors and posterior probabilities under commonly used statistical models. For the paired samples t-test that means one can test hypotheses relating to mean of a group on a single variable. For details, see Mulder et al. (2021).

## Input
### Main Window
- Variable: Input variables that are continuous (if they are not, they will be treated as such)

#### Standard Hypothesis Test
- Hypotheses: Test hypothesis that the mean of the variable is equal or unequal to the test value
- Prior Weights: Specify how to weigh each hypothesis; defaults to the null hypothesis being weighted twice as much as the alternatives 
- Specify Test Value: Test value for the hypothesis of equality/inequality

#### Parameters
Once the variables are filled in, the parameters that can be used in the specification of the manual hypotheses show up here

#### Manual Hypothesis Test
- Hypotheses: Specify a manual hypothesis, see the tooltip for more info; Specify the prior weight and do not forget to check the include box to test the hypothesis. For the t-test this could be something like "var1Name > var2Name"
- Use The "+" To Add More Hypotheses
- Complement: The complement hypothesis; prior weight and include

### Options
#### Bayes Factor
- Log Scale: Reports the log BF
- Bayes Factor Type: Default is the fractional BF, alternatively choose the adjusted fractional BF

#### Tables
- BFs: Standard Hypotheses: Print table that compares each standard hypothesis with its complement
- BFs: Manual Hypotheses: Print the specification table
- Estimates with uncertainty interval: Print a table with the point estimates and uncertainty intervals (confidence interval for t-test) for the parameter(s) of interest.

#### Plots
- Manual hypothesis plots: Produces plots depicting the prior and posterior probabilities of the manual hypotheses

#### Additional options: 
- Seed

## Output

### Tables
#### Posterior probabilites when testing standard hypotheses
- Posterior probs for the standard hypotheses

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
- BF matrix with the hypotheses: If the BF for H1vH2 is smaller than 1, evidence is in favor of H2, if it is larger than 1 evidence is in favor of H1

#### Posterior model probability
- provides the posterior probability for each hypothesis

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
- Mean, median, and CI bounds. For the t-test they are confidence intervals.

### Plots
#### Prior and posterior probability 
- Pizza plots for the manual hypotheses

### References

- Mulder, J., & Gelissen, J. P. T. M. (2023). Bayes factor testing of equality and order constraints on measures of association in social research. *Journal of Applied Statistics, 50*(2), 315–351. https://doi.org/10.1080/02664763.2021.1992360
- Mulder, J., Williams, D. R., Gu, X., Tomarken, A., Böing-Messing, F., Olsson-Collentine, A., Meijerink, M., Menke, J., Fox, J.-P., Hoijtink, H., Rosseel, Y., Wagenmakers, E.J., and van Lissa, C. (2021). BFpack: Flexible Bayes Factor Testing of Scientific Theories in R. *Journal of Statistical Software, 100*(18), 1-63. https://doi.org/10.18637/jss.v100.i18
