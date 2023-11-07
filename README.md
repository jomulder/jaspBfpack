# The Bfpack Module

A module for computing Bayes factors for equality, inequality, and order constrained hypotheses.

## Core Functionality

The Bfpack module is an graphical interface around the `R` package `Bfpack`, which implements Bayes factors for equality, inequality, and order constrained hypotheses. The module offers a variety of standard statistical analyses, in which contraints can be formulated for the relevant parameters under investigation.

## Module Structure

The analyses in the Bfpack module are structured in JASP in the following way:

```
--- Bfpack
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