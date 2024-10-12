BFpack Linear (Multivariate) Regression
==========================

The analysis allows to test exploratory hypotheses (e.g., equal vs negative vs postive) and confirmatory hypotheses (with equality and/or order constraints) using Bayes factors and posterior probabilities under commonly used statistical models. For the linear regression that means one can test hypotheses relating to the regression coefficients within a single multiple regression or between the coefficients of more than one regression.

## Input
### Main Window
- Dependent Variables: Input one or more variables that are continuous (if they are not, they will be treated as such); if there is more than one DV it is a multivariate regression
- Covariates: Predictors for the regression(s); they can have any scaling, that is, continuous, ordinal, nominal

#### Standard hypothesis test
- Hypotheses: Test hypothesis that the parameter is equal to smaller or larger than a specific value; for regression the parameters are all called beta and they are tested to be zero or smaller or larger
- Prior weights: Specify how to weigh each hypothesis; defaults to the nullhypothesis being weighted twice as much as the alternatives 

#### Parameters
Once the variables are filled in, the parameters that can be used in the specification of the manual hypotheses show up here

#### Manual hypothesis test
- Hypotheses: Specify a manual hypothesis, see the tooltip for more info; Specify the prior weight and do not forget to check the include box to test the hypothesis. For the regression this could be something like "pred1Name_on_dep1Name > pred2Name_on_dep1Name" or "pred1Name_on_dep1Name > pred1Name_on_dep2Name"
- Use the "+" to add more hypotheses
- Complement: The complement hypothesis; prior weight and include

### Options
#### Bayes Factor: 
- Log scale: reports the log BF
- Bayes factor type: Default is the fractional BF, alternatively choose the adjusted fractional BF

#### Tables
- Standard hypothesis BFs: Print table that compares each standard hypothesis with its complement
- Specification: Print the specification table
- Estimates with uncertainty interval: Print a table with the point estimates and uncertainty intervals (confidence interval for regression) for the parameter(s) of interest. 

#### Plots
- Manual hypothesis plots: Produces plots depicting the prior and posterior probabilities of the manual hypotheses

#### Additional options: 
- Seed

#### Interaction terms
- Box that displays possible two-way interaction terms (if there is more than one covariate); they are by default included in the analysis

## Output

### Tables
#### Posterior probabilites when testing standard hypotheses
- Posterior probs for the standard hypotheses

#### BFs when testing standard hypotheses
- Bayes factors for the standard hypotheses and their complements

#### Manual hypotheses legend
- Denotes the manual hypotheses

#### Evidence matrix (BFs)
- BF matrix with the hypotheses: If the BF for H1vH2 is smaller than 1, evidence is in favor of H2, if it is larger than 1 evidence is in favor of H1

#### Posterior model probability
- provides the posterior probability for each hypothesis

#### Specification table
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
