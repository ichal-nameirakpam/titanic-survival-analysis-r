# Week 3: Statistical Analysis and Predictive Modeling in R
**Data Analyst Internship — Skill India**

Predicting Titanic passenger survival using hypothesis testing and logistic regression in R.

## Overview
This project performs a full statistical analysis and predictive modeling workflow on the Titanic dataset (891 passengers, Kaggle):

- **Hypothesis testing**: chi-square tests (Sex, Pclass, Embarked vs. Survival), Shapiro-Wilk normality tests, Wilcoxon rank-sum tests (Age, Fare vs. Survival)
- **Predictive model**: logistic regression predicting `Survived` from Pclass, Sex, Age, Fare, Embarked, FamilySize, and Cabin_Known
- **Validation**: 10-fold cross-validation and an untouched 20% held-out test set
- **Diagnostics**: confusion matrix, precision/recall/F1/AUC, ROC curve, deviance residuals, Cook's distance, VIF (multicollinearity check)

## Results
| Metric | Value |
|---|---|
| Test Accuracy | 0.838 |
| Precision | 0.870 |
| Recall | 0.681 |
| Test AUC | 0.867 |
| Mean CV Accuracy (10-fold) | 0.806 |
| Mean CV AUC | 0.855 |

Sex and Pclass are the strongest predictors of survival; Fare and Embarked lose significance once Pclass is controlled for (confounding). Full interpretation and discussion of strengths/limitations is in the report.

## Repo Contents
- `analysis_script.R` — complete, runnable R script (data prep, hypothesis tests, model, CV, diagnostics)
- `Week3_Titanic_Statistical_Analysis.docx` — full written report with methodology, tables, and figures
- `plots/` — exported diagnostic plots (ROC curve, Q-Q plots, residuals, Cook's distance)

## Tools
R (base R + tidyverse), run in Posit Cloud. Note: `caret`, `pROC`, and `car` were unavailable in the runtime environment, so cross-validation, AUC, and VIF were implemented manually in base R (see script comments and report Section 6.1 for details).

## Dataset
[Titanic - Machine Learning from Disaster](https://www.kaggle.com/c/titanic) (Kaggle)
