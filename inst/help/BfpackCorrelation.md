BFpack Correlation
==========================

The analysis allows to test exploratory hypotheses (e.g., equal vs negative vs postive) and confirmatory hypotheses (with equality and/or order constraints) using Bayes factors and posterior probabilities under commonly used statistical models. For the correlation that means one can test hypotheses relating to correlation coefficients. The correlations include biserial, polyserial, tetrachoric, polychoric, and product-moment coefficients. Moreover, the correlations can be overlapping, nonoverlapping, and independent (across groups). For details, see Mulder et al. (2021).

## Input
### Main Window
- Variables: Input at least two variables that are nominal (with 2 levels), ordinal, or continuous.
- Grouping variable: This allows testing independent correlations.
- Covariates: Input continuous or ordinal variables. This allows testing correlations while correcting for certain variables.

#### Standard hypothesis test
- Hypotheses: Test the hypotheses that each correslation (rho) is equal to, smaller than, or larger than 0.
- Prior weights: Specify how to weigh each hypothesis. The default corresponds to a standard setting when testing a two-sided hypothesis test where the null hypothesis has an equal prior weight as the two-sided alternative hypothesis. Because the two-sided alternative is split to the left side and right side, the default prior weight of the null (H0) is 2, and each prior weight for the left-sided and right-sided hypotheses (H1 and H2, respectively) is 1.

#### Parameters
This box contains the names (labels) of the parameters on which equality/one-sided constraints can be formulated in the ‘manual hypothesis test’ box. When testing correlations, the names of the correlations depend on the names of the variables, and (in the case a grouping variable is present) to which group the correlation belongs. In case of two variables, which are labeled 'var1' and 'var2', the correlation would be labeled as 'var1_with_var2'.

#### Manual hypothesis test
- Hypotheses: Specify a manual hypothesis with equality and/or one-sided constraints on the parameters; see the tooltip for more info. Specify the prior weights and do not forget to include each hypothesis via the check box . For the correlation this could be something like "var1_with_var2 > var1_with_var3 > var1_with_var4 > 0” in case it is expected that the correlation between ‘var1’ and ‘var2’ is larger than the correlation between ‘var1’ and ‘var3’, which is larger than the correlation between ‘var1’ and ‘var4’, which are all positive. 
- Use the "+" to add more hypotheses.
- Complement: The complement hypothesis (which covers the range of the correlations that are not covered by the above specified manual hypotheses); prior weight and include.

### Options
#### Bayes Factor: 
- Log scale: Reports the natural logarithm of the Bayes factors.

#### Tables
- BFs for standard hypothesis test: Print a table that compares each standard hypothesis with its complement
- BFs: Manual hypotheses: Print the specification table with different parts of the (Savage-Dickey) Bayes factors.
- Estimates with uncertainty interval: Print a table with the point estimates and uncertainty intervals (credible intervals for correlations) for the parameter(s) of interest. Estimates are based on the posterior distribution of the correlation coefficient under the full model.

#### Plots
- Manual hypothesis plots: Produces plots depicting the prior and posterior probabilities of the manual hypotheses
- Posterior plot: Produces the posterior distribution(s) of the sampled correlation(s)
- Traceplot: Produces the traceplot(s) of the sampled correlation(s)

#### Additional options: 
- No. iterations for parameter estimation: default is 5000 samples from the posterior distribution 
- Seed

## Output

### Tables
#### Posterior probabilites when testing standard hypotheses
- Posterior probabilities for the standard hypotheses.

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

#### Posterior model probabilities for the manual hypothesis test
- Prints the posterior probability for each hypothesis for the manual hypothesis test.

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
- •	Posterior means, medians, and CI bounds.

### Plots
#### Prior and posterior probability 
- Pizza plots for the manual hypotheses

#### Posterior distribution plots
- Plot the posterior distribution of each correlation with the credible interval

#### Traceplots
- Plot the trace of the posterior samples of each correlation. Should resemble a "hairy caterpillar"

### References

- Mulder, J., & Gelissen, J. P. T. M. (2023). Bayes factor testing of equality and order constraints on measures of association in social research. *Journal of Applied Statistics, 50*(2), 315–351. https://doi.org/10.1080/02664763.2021.1992360
- Mulder, J., Williams, D. R., Gu, X., Tomarken, A., Böing-Messing, F., Olsson-Collentine, A., Meijerink, M., Menke, J., Fox, J.-P., Hoijtink, H., Rosseel, Y., Wagenmakers, E.J., and van Lissa, C. (2021). BFpack: Flexible Bayes Factor Testing of Scientific Theories in R. *Journal of Statistical Software, 100*(18), 1-63. https://doi.org/10.18637/jss.v100.i18
- Mulder, J. (2016). Bayes factors for testing order-constrained hypotheses on correlations. Journal of Mathematical Psychology, 72, 104-115. https://doi.org/10.1016/j.jmp.2014.09.004
