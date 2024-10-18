BFpack Independent Samples T-Test
==========================

The analysis allows to test exploratory hypotheses (e.g., equal vs negative vs positive) and confirmatory hypotheses (with equality and/or order constraints) using Bayes factors and posterior probabilities under commonly used statistical models. For the independent samples t-test that means one can test hypotheses relating to the difference between two groups measured on a continuous variable. For details, see Mulder et al. (2021).

## Input
### Main Window
- Variables: Input one variable that is continuous (if they are not, they will be treated as such)
- Grouping Variable: Input one variable that is nominal

#### Standard Hypothesis Test
- Hypotheses: Test hypothesis that the parameter is equal to smaller or larger than a specific value; for t-test the parameter is called delta and denotes the difference between the independent groups
- Test Value: Test value for the hypothesis of equality/inequality
- Prior Weights: Specify how to weigh each hypothesis; defaults to the null hypothesis being weighted twice as much as the alternatives 

#### Parameters
Once the variables are filled in, the parameters that can be used in the specification of the manual hypotheses show up here

#### Manual Hypothesis Test
- Hypotheses: Specify a manual hypothesis, see the tooltip for more info; Specify the prior weight and do not forget to check the include box to test the hypothesis. For the t-test this could be something like "1>difference>0"
- Use The "+" To Add More Hypotheses
- Complement: The complement hypothesis; prior weight and include

### Options
#### Bayes Factor
- Log Scale: Reports the log BF
- Bayes Factor Type: Default is the fractional BF, alternatively choose the adjusted fractional BF

#### Tables
- BFs: Standard Hypotheses: Print table that compares each standard hypothesis with its complement
- BFs: Manual Hypotheses: Print the specification table
- Estimates With Uncertainty Interval: Print a table with the point estimates and uncertainty intervals (confidence interval for t-test) for the parameter(s) of interest.

#### Plots
- Manual Hypothesis Plots: Produces plots depicting the prior and posterior probabilities of the manual hypotheses

#### Additional Options
- Seed

## Output

### Tables
#### Posterior Probabilities When Testing Standard Hypotheses
- Posterior probs for the standard hypotheses

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
- BF matrix with the hypotheses: If the BF for H1vH2 is smaller than 1, evidence is in favor of H2, if it is larger than 1 evidence is in favor of H1

#### Posterior Model Probability
- Provides the posterior probability for each hypothesis

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
- Mean, median, and CI bounds. For the t-test they are confidence intervals.

### Plots
#### Prior And Posterior Probability 
- Pizza plots for the manual hypotheses

### References

- Mulder, J., & Gelissen, J. P. T. M. (2023). Bayes factor testing of equality and order constraints on measures of association in social research. *Journal of Applied Statistics, 50*(2), 315–351. https://doi.org/10.1080/02664763.2021.1992360
- Mulder, J., Williams, D. R., Gu, X., Tomarken, A., Böing-Messing, F., Olsson-Collentine, A., Meijerink, M., Menke, J., Fox, J.-P., Hoijtink, H., Rosseel, Y., Wagenmakers, E.J., and van Lissa, C. (2021). BFpack: Flexible Bayes Factor Testing of Scientific Theories in R. *Journal of Statistical Software, 100*(18), 1-63. https://doi.org/10.18637/jss.v100.i18
