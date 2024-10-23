BFpack (M)AN(C)OVA
==========================

The analysis allows to test exploratory hypotheses (e.g., equal vs negative vs postive) and confirmatory hypotheses (with equality and/or order constraints) using Bayes factors and posterior probabilities under commonly used statistical models. For the (M)AN(C)OVA that means one can test hypotheses relating to differences between groups measured on one or multiple variables and with or without covariates. For details, see Mulder et al. (2021).

## Input
### Main Window
- Dependent Variables: Input one or more variables that are continuous (if they are not, they will be treated as such)
- Fixed Factors: Input one or more variable that is nominal
- Covariates: Input one or more variables that are continuous (if they are not, they will be treated as such)
- In the case of one dependent variable: AN(C)OVA. In the case of multiple dependent variables: MAN(C)OVA.
- In the case of one or more covariate(s): (M)ANCOVA. In the case of no covariate(s): (M)ANOVA.

#### Standard Hypothesis Test
- Hypotheses: Test the hypotheses that each separate parameter is equal to, smaller than, or larger than 0. For a (M)AN(C)OVA, additional omnibus tests are performed of whether each main effect and each interaction effect to be absent or not.
- Prior Weights: Specify how to weigh each hypothesis. For the tests of the separate parameters, the default corresponds to a standard setting when testing a two-sided hypothesis test where the null hypothesis has an equal prior weight as the two-sided alternative hypothesis. Because the two-sided alternative is split to the left side and right side, the default prior weight of the null (H0) is 2, and each prior weight for the left-sided and right-sided hypotheses (H1 and H2, respectively) is 1. For the omnibus tests of the main and interaction effects, the default sets equal prior weights of 1 to the two hypotheses. The prior weights for the omnibus tests can be changed under the ‘options’ .

#### Parameters
This box contains the names (labels) of the parameters on which equality/one-sided constraints can be formulated in the ‘manual hypothesis test’ box. For a (M)AN(C)OVA, the parameter are the (adjusted) means and effect of the covariates. The names depend on the names of the variables.

#### Manual Hypothesis Test
- Hypotheses: Specify a manual hypothesis with equality and/or one-sided constraints on the parameters; see the tooltip for more info; Specify the prior weights and do not forget to include each hypothesis via the check box. For the (M)AN(C)OVA this could be something like "var1NameGroup1 > var1NameGroup2 > var2NameGroup1 > var2NameGroup2", which assumes a specific order of the effects of certain variables across groups.
- Use The "+" To Add More Hypotheses.
- Complement: The complement hypothesis (which covers the range of the parameters that are not covered by the above specified hypotheses); prior weight and include.

### Options
#### Bayes Factor
- Log Scale: Reports the natural logarithm of the Bayes factors.
- Bayes Factor Type: The default is the fractional BF, alternatively choose the adjusted fractional BF. The adjusted fractional BF was proposed in the case all hypotheses of interest only contain one-sided (order) constraints.

#### Tables
- BFs for standard hypothesis test: Print a table that compares each standard hypothesis with its complement
- BFs: Manual Hypotheses: Print the specification table with different parts of the (Savage-Dickey) Bayes factors.
- Estimates with uncertainty interval: Print a table with the point estimates and uncertainty intervals (default credibility intervals for a (M)AN(C)OVA) for the parameter(s) of interest.

#### Plots
- Manual hypothesis plots: Produces plots depicting the prior and posterior probabilities of the manual hypotheses

#### Additional options: 
- Seed

#### Interaction terms
- Box that displays possible two-way interaction terms (if there are more than one dependent variable/covariate); they are by default included in the analysis

#### Prior weights for additional standard hypothesis tests for a (M)AN(C)OVA
- Specify the prior weights for the main and interaction effects being absent or not.


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


#### Posterior Probabilities For Main Effects
- Omnibus test for each main effect to be absent or not.

#### Posterior Probabilities For Interaction Effects
- Omnibus test for each interaction effect to be absent or not.

#### Manual Hypotheses Legend
- Denotes the manual hypotheses.

#### Evidence Matrix (BFs)
- BF matrix with the hypotheses: If the BF for H1 vs H2 is smaller than 1, evidence is in favor of H2, if it is larger than 1 evidence is in favor of H1. If “Log scale” is checked, the printed BFs are on a natural logarithm scale.

#### Posterior Model Probabilities for the Manual Hypothesis Test
- Prints the posterior probabilities for each hypothesis in the manual hypothesis test.

#### BFs: Manual Hypotheses Table
- Equal-Complex: Quantifies the relative complexity of the equality constraints of a hypothesis (the prior density at the equality constraints in the extended Savage Dickey density ratio)
- Order-Complex: Quantifies the relative complexity of the order constraints of a hypothesis (the prior probability of the order constraints in the extended Savage Dickey density ratio)
- Equal-Fit: Quantifies the relative fit of the equality constraints of a hypothesis (the posterior density at the equality constraints in the extended Savage Dickey density ratio)
- Order-Fit: Quantifies the relative fit of the order constraints of a hypothesis (the posterior probability of the order constraints in the extended Savage Dickey density ratio)
- Equal-BF: Contains the Bayes factor of the equality constraints against the unconstrained hypothesis
- Order-BF: Contains the Bayes factor of the order constraints against the unconstrained hypothesis
- BF: Contains the Bayes factor of the constrained hypothesis against the unconstrained hypothesis
- Posterior Prob.: Contains the posterior probabilities of the hypotheses

#### Estimates Table:
- Posterior means, medians, and CrI bounds of the separate parameters using noninformative (Jeffreys) priors.

### Plots
#### Prior and posterior probability 
- Pizza plots for the manual hypotheses

### References

- Mulder, J., & Gu, X. (2022). Bayesian testing of scientific expectations under multivariate normal linear models. Multivariate Behavioral Research, 57(5), 767-783. https://doi.org/10.1080/00273171.2021.1904809
- Mulder, J., Williams, D. R., Gu, X., Tomarken, A., Böing-Messing, F., Olsson-Collentine, A., Meijerink, M., Menke, J., Fox, J.-P., Hoijtink, H., Rosseel, Y., Wagenmakers, E.J., and van Lissa, C. (2021). BFpack: Flexible Bayes Factor Testing of Scientific Theories in R. *Journal of Statistical Software, 100*(18), 1-63. https://doi.org/10.18637/jss.v100.i18
