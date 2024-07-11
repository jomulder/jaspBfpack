# The BFpack Module

A module for computing Bayes factors for equality, inequality, and order constrained hypotheses.

## Core Functionality

The BFpack module is an graphical interface around the `R` package `BFpack`, which implements Bayes factors for equality, inequality, and order constrained hypotheses. The module offers a variety of standard statistical analyses, in which contraints can be formulated for the relevant parameters under investigation.

## Module Structure

The analyses in the BFpack module are structured in JASP in the following way:

```
--- BFpack
    -- T-Tests
       - Welch's T-Test
       - Paired Samples T-Test
       - One Sample T-Test
    -- ANOVA
       - ANOVA
       - ANCOVA
    -- Regression
       - Linear Regression
       - Structural Equation Modeling
```