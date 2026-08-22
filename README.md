# Titanic Survival Analysis in R
**Data Analyst Internship — Skill India**

A weekly progression of data analysis work on the Titanic passenger dataset (891 passengers), moving from data cleaning through visualization to statistical modeling — all in R.

## Repo Structure

| Folder | Focus |
|---|---|
| [Week1](./Week1) | Data cleaning, missing value handling, outlier treatment, exploratory analysis |
| [Week2](./Week2) | Data visualization and insight communication (ggplot2) |
| [Week3](./Week3) | Hypothesis testing and logistic regression predictive modeling |

---

## Week 1: Data Cleaning and Preliminary Analysis
Cleaned the raw Titanic dataset — imputed missing Age (median) and Embarked (mode), flagged missing Cabin instead of dropping it outright, detected and capped Fare outliers (IQR method + winsorizing), normalized numeric variables, and encoded categorical variables. Includes full exploratory analysis (summary statistics, survival breakdowns, correlation matrix).
- `titanic.csv` — raw dataset
- `titanic_cleaned.csv` — cleaned, encoded, scaled dataset
- `analysis.R` — full cleaning + EDA script
- `Week1_Titanic_Data_Cleaning_Report.docx` — full write-up

## Week 2: Data Visualization and Insight Communication
Built five visualizations (bar chart, scatter plot, log-scaled histogram, line chart, stacked bar chart) on the Week 1 cleaned dataset, each paired with a plain-language interpretation aimed at a non-technical audience.
- `titanic_week2_final.csv` — dataset with Week 2 derived columns (age groups, family size)
- `analysis_week2.R` — full visualization script
- `Week2_Titanic_Visualization_Report.docx` — full write-up

## Week 3: Statistical Analysis and Predictive Modeling
Performed hypothesis testing (chi-square, Shapiro-Wilk, Wilcoxon) and built a logistic regression model predicting survival from Pclass, Sex, Age, Fare, Embarked, FamilySize, and Cabin_Known. Validated with 10-fold cross-validation and a held-out test set; diagnostics include confusion matrix, ROC/AUC, Cook's distance, and VIF.

**Results:**

| Metric | Value |
|---|---|
| Test Accuracy | 0.838 |
| Precision | 0.870 |
| Recall | 0.681 |
| Test AUC | 0.867 |
| Mean CV Accuracy (10-fold) | 0.806 |
| Mean CV AUC | 0.855 |

Sex and Pclass are the strongest predictors of survival; Fare and Embarked lose significance once Pclass is controlled for (confounding). Full interpretation in the report.
- `analysis_script.R` — complete hypothesis testing + modeling script
- `Week3_Titanic_Statistical_Analysis.docx` — full write-up
- `plots/` — diagnostic plots (ROC curve, Q-Q plots, residuals, Cook's distance)

---

## Tools
R (base R + tidyverse), run in Posit Cloud. `caret`, `pROC`, and `car` were unavailable in the runtime environment, so cross-validation, AUC, and VIF were implemented manually in base R (see Week 3 script comments and report Section 6.1).

## Dataset
[Titanic - Machine Learning from Disaster](https://www.kaggle.com/c/titanic) (Kaggle)
