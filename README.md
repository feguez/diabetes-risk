# Diabetes Risk Indicators | Statistical Analysis



An exploratory statistical analysis of health indicators associated with diabetes risk, using 253,680 observations from the 2015 Behavioral Risk Factor Surveillance System (BRFSS).



**Authors:** Fabrizio Eguez, Tayler Menke, James Chen



## Project Overview



This project investigates relationships between body mass index (BMI), high blood pressure, diabetes status, age, and income using R.



The analysis applies descriptive statistics, confidence intervals, hypothesis testing, one-way ANOVA, and multiple linear regression.



## Key Findings



- **High blood pressure:** 42.9% of respondents reported high blood pressure.

- **BMI and blood pressure:** Respondents reporting high blood pressure had a higher average BMI (30.01) than those without high blood pressure (27.16).

- **BMI and diabetes:** Mean BMI was 31.94 among respondents reporting diabetes, compared with 27.74 among those without diabetes.

- **Age-group differences:** ANOVA identified statistically significant BMI differences across age groups, although age explained only approximately 1.55% of BMI variation.

- **Regression:** Age and income categories jointly explained approximately 3.31% of observed BMI variation.



## Analysis Report



The project includes a Quarto HTML report with statistical tables, visualizations, and interpretations.



- `diabetes_statistical_analysis.html` : Rendered report

- `diabetes_statistical_analysis.qmd` : Quarto report source

- `diabetes_statistical_analysis.R` : Original statistical analysis



An interactive web link to the report will be added after GitHub Pages deployment.



## Methodology



The analysis includes:



1. Exploratory data analysis and visualization

2. Descriptive statistics and group comparisons

3. Confidence interval estimation

4. One-sample and Welch two-sample t-tests

5. One-way ANOVA and effect-size analysis

6. Multiple linear regression with categorical predictors



**Limitations:** Analyses use conventional unweighted statistical methods and do not account for the BRFSS complex survey design. Findings represent descriptive associations, not causal relationships or survey-design-adjusted national estimates.



## Reproducibility



The original dataset is included in `originals/`.



Run `diabetes_statistical_analysis.R` from the repository root using R, or render `diabetes_statistical_analysis.qmd` using Quarto with R and the `knitr` package installed.



## Upcoming Work



XGBoost model + Python analysis pending upload - cleaning/reviewing first before adding to the repo. Coming soon.


