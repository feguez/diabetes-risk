# ============================================================
# DIABETES RISK INDICATORS: STATISTICAL ANALYSIS
# ============================================================
# Authors: Fabrizio Eguez, Tayler Menke, James Chen
# Dataset: 2015 BRFSS Diabetes Health Indicators
# Language: R
#
# Exploratory and inferential analysis of BMI, blood
# pressure, diabetes status, age, and income.
#
# Companion project: Diabetes Risk Modeling with XGBoost
# ============================================================


# ------------------------------------------------------------
# DATA LOADING & VALIDATION
# ------------------------------------------------------------

# Source: 2015 BRFSS Diabetes Health Indicators dataset
# 253,680 respondents, 21 health/demographic indicators,
# and one three-class diabetes outcome.

data <- read.csv("originals/diabetes_012_health_indicators_BRFSS2015.csv")

# Verify dataset dimensions and missing values
stopifnot(
  nrow(data) == 253680,
  ncol(data) == 22,
  !anyNA(data)
)

cat("Dataset loaded:", nrow(data), "rows and",
    ncol(data), "columns\n")

# ------------------------------------------------------------
# 1. EXPLORATORY ANALYSIS: HIGH BLOOD PRESSURE
# ------------------------------------------------------------

# HighBP is a binary indicator:
# 0 = No high blood pressure
# 1 = High blood pressure

bp_counts <- table(
  factor(data$HighBP,
         levels = c(0, 1),
         labels = c("No high BP", "High BP"))
)

bp_percent <- round(prop.table(bp_counts) * 100, 1)

# Frequency and percentage table
bp_summary <- data.frame(
  Status = names(bp_counts),
  Count = as.integer(bp_counts),
  Percent = as.numeric(bp_percent)
)

print(bp_summary)

# Frequency visualization
bp_bars <- barplot(
  bp_counts,
  col = c("#A8BED0", "#2E6487"),
  border = NA,
  ylim = c(0, max(bp_counts) * 1.15),
  main = "Self-Reported High Blood Pressure",
  xlab = "High Blood Pressure Status",
  ylab = "Number of Respondents"
)

# Add frequency and percentage labels above bars
text(
  x = bp_bars,
  y = as.numeric(bp_counts),
  labels = paste0(
    format(as.integer(bp_counts), big.mark = ","),
    " (", bp_percent, "%)"
  ),
  pos = 3,
  cex = 0.9
)



# ------------------------------------------------------------
# 2. EXPLORATORY ANALYSIS: BODY MASS INDEX (BMI)
# ------------------------------------------------------------

# Summary statistics for BMI
bmi_stats <- c(
  Min = min(data$BMI),
  Median = median(data$BMI),
  Max = max(data$BMI),
  Mean = mean(data$BMI),
  SD = sd(data$BMI),
  Range = diff(range(data$BMI))
)

print(round(bmi_stats, 2))

# Visualize BMI distribution
hist(
  data$BMI,
  breaks = 30,
  col = "#A8BED0",
  border = "white",
  main = "Distribution of Body Mass Index",
  xlab = "BMI",
  ylab = "Number of Respondents"
)

# ------------------------------------------------------------
# 3. BMI BY HIGH BLOOD PRESSURE STATUS
# ------------------------------------------------------------

# Compare BMI distributions across HighBP categories
bmi_by_bp <- data.frame(
  Status = c("No high BP", "High BP"),
  Count = c(
    sum(data$HighBP == 0),
    sum(data$HighBP == 1)
  ),
  Mean_BMI = c(
    mean(data$BMI[data$HighBP == 0]),
    mean(data$BMI[data$HighBP == 1])
  ),
  Median_BMI = c(
    median(data$BMI[data$HighBP == 0]),
    median(data$BMI[data$HighBP == 1])
  ),
  SD_BMI = c(
    sd(data$BMI[data$HighBP == 0]),
    sd(data$BMI[data$HighBP == 1])
  )
)

# Round statistics for readability
bmi_by_bp[, 3:5] <- round(bmi_by_bp[, 3:5], 2)

print(bmi_by_bp)

# Compare BMI distributions visually
boxplot(
  BMI ~ factor(HighBP,
               levels = c(0, 1),
               labels = c("No high BP", "High BP")),
  data = data,
  col = c("#A8BED0", "#2E6487"),
  border = "#334155",
  outline = FALSE,
  main = "BMI by High Blood Pressure Status",
  xlab = "High Blood Pressure Status",
  ylab = "BMI"
)


# ------------------------------------------------------------
# 4. EXPLORATORY FINDINGS
# ------------------------------------------------------------

# Of the 253,680 respondents, 42.9% reported high blood
# pressure and 57.1% did not.

# Respondents reporting high blood pressure had a higher
# mean BMI (30.01) than those without high BP (27.16),
# a descriptive difference of approximately 2.85 BMI points.

# This association motivates further investigation of
# health indicators and diabetes status. It does not
# establish that high blood pressure causes higher BMI,
# or vice versa.


# ------------------------------------------------------------
# 5. CONFIDENCE INTERVALS
# ------------------------------------------------------------

# 95% Confidence Interval for Mean BMI
# BMI is an observed quantitative variable in the BRFSS dataset.

bmi_values <- data$BMI

n <- length(bmi_values)
mean_bmi <- mean(bmi_values)
sd_bmi <- sd(bmi_values)
se_bmi <- sd_bmi / sqrt(n)

# 95% confidence interval using Student's t distribution
t_critical <- qt(0.975, df = n - 1)
margin_error <- t_critical * se_bmi

ci95_bmi <- mean_bmi + c(-1, 1) * margin_error

round(ci95_bmi, 2)

# Verification using R's built-in t-test
t.test(bmi_values, conf.level = 0.95)


# Interpretation:
# The sample mean BMI is 28.38, with a conventional
# 95% confidence interval of approximately [28.36, 28.41].

# This interval is calculated using standard unweighted
# methods and does not account for BRFSS's complex
# survey design. It should not be presented as a
# survey-design-adjusted national estimate.

## We use a 95% confidence level (alpha = 0.05)
## For the younger group (18-24)
bmi_age1 = data$BMI[data$Age == 1] #BMI where age group is 1
n1 = length(bmi_age1) #n for age group 1
xbar1 = mean(bmi_age1) #sample mean
s1 = sd(bmi_age1) #sample standard deviation
se1 = s1/sqrt(n1) #standard error
df1 = n1 - 1 #degrees of freedom
t.alpha1 = qt(0.025, df = df1, lower.tail = FALSE) #t test
MoE1 = t.alpha1 * se1 #margin of error
ci95_1 = xbar1 + c(-1, 1) * MoE1 #confidence intervals = xbar +- MoE
round(ci95_1, 4)
#can also use t test to explicitly state confidence level
t.test(bmi_age1, conf.level = 0.95)

## For the middle aged group (50-54)
bmi_age7 = data$BMI[data$Age == 7]
n2 = length(bmi_age7)
xbar2 = mean(bmi_age7)
s2 = sd(bmi_age7)
se2 = s2/sqrt(n2)
df2 = n2 - 1
t.alpha2 = qt(0.025, df = df2, lower.tail = FALSE)
MoE2 = t.alpha2 * se2
ci95_2 = xbar2 + c(-1, 1) * MoE2
round(ci95_2, 4)
#can also use t test to explicitly state confidence level
t.test(bmi_age7, conf.level = 0.95)


# ------------------------------------------------------------
# AGE-GROUP CONFIDENCE INTERVAL INTERPRETATION
# ------------------------------------------------------------

# Age Group 1 (18-24):
# Mean BMI = 26.09
# 95% CI = [25.92, 26.26]
# n = 5,700

# Age Group 7 (50-54):
# Mean BMI = 28.94
# 95% CI = [28.86, 29.03]
# n = 26,314

# Respondents aged 50-54 have a mean BMI approximately
# 2.85 points higher than respondents aged 18-24.

# The conventional confidence intervals do not overlap,
# indicating a clear difference in sample means.

# The younger group has a wider confidence interval,
# consistent with its smaller sample size.

# These estimates are unweighted and do not incorporate
# the complex BRFSS survey sampling design.

# ------------------------------------------------------------
# 6. HYPOTHESIS TESTING
# ------------------------------------------------------------

####### One Sample T-test #######

# Hypothesized Question:
# Is the average BMI of the population represented by our sample 
# greater than 25?

# We use 25 as the benchmark for this hypothesis test.

# Null hypothesis (H0):
#  population mean BMI less than or equal to 25
#  μ <= 25

# Alternative hypothesis (Ha):
#  population mean BMI is greater than 25
#  μ > 25

# Using this question of greater than 25 results in a one-tailed (right-tailed)
# t-test.

# T-test
mu = 25

t.test(data$BMI, 
       mu = 25,
       alternative = 'greater')


# Interpretation:
# The sample mean BMI (28.38) is statistically
# significantly greater than the benchmark of 25
# (t = 257.78, p < 0.001).

# The benchmark is used for statistical comparison;
# it is not a diagnostic assessment of respondents.

# The large sample size contributes to the very
# small p-value. The observed difference of 3.38
# BMI points is more informative than significance alone.


####### Two Sample T-test #######

# Hypothesis Question:

# Is the average BMI different between people without diabetes(0) and 
# people with diabetes(2)?

# Null hypothesis (H0): 
# mean BMI is the the same for people without diabetes and people with diabetes
# μ0 = μ2

# Alternative hypothesis (Ha): 
# mean BMI is different between people without diabetes and people with diabetes
# μ0 != μ2

table(data$Diabetes_012)

# we only want to look at the variables showing diabetes and no diabetes

no_diabetes <- data$BMI[data$Diabetes_012 == 0]
diabetes <- data$BMI[data$Diabetes_012 == 2]

t.test(no_diabetes, diabetes)


# Interpretation:
# Respondents reporting diabetes had a higher mean
# BMI (31.94) than those reporting no diabetes (27.74).

# The mean difference is approximately 4.20 BMI points,
# with a statistically significant Welch t-test
# (t = -101.38, p < 0.001).

# Respondents reporting prediabetes (Diabetes_012 = 1)
# were excluded from this two-group comparison.

# This is an association between BMI and reported
# diabetes status, not evidence of causation.


####### BMI Differences Across Age Groups ########

# Age contains 13 categorical age groups (1 through 13).
# We use the original age categories rather than
# artificially generated exact ages.

# Average BMI for each age group
age_bmi_summary <- aggregate(BMI ~ Age, data = data, FUN = mean)

print(age_bmi_summary)

# Visualize mean BMI across age groups
plot(age_bmi_summary$Age,
     age_bmi_summary$BMI,
     type = "b",
     pch = 19,
     main = "Mean BMI by Age Group",
     xlab = "Age Group (1 = 18-24, 13 = 80+)",
     ylab = "Mean BMI")

# One-way ANOVA:
# H0: All age groups have the same population mean BMI.
# Ha: At least one age group has a different mean BMI.

age_anova <- aov(BMI ~ factor(Age), data = data)

summary(age_anova)


# Calculate eta-squared (proportion of BMI variation
# explained by age-group membership).

anova_table <- summary(age_anova)[[1]]

eta_squared <- anova_table$`Sum Sq`[1] /
  sum(anova_table$`Sum Sq`)

cat("ANOVA eta-squared:", round(eta_squared, 4), "\n")

# Interpretation:
# Mean BMI differs significantly across age groups
# (F(12, 253667) = 332.1, p < 0.001).

# Eta-squared is approximately 0.0155, indicating
# that age-group membership explains about 1.55%
# of the observed variation in BMI.

# The pattern is nonlinear, with mean BMI generally
# increasing through middle age before declining
# among older age groups.


###### Multiple Linear Regression ######
# Predict BMI using the original Age and Income categories.
# These categories represent ranges, not exact ages or incomes.

# Convert original variables into categorical factors
data$Age_group <- factor(data$Age, levels = 1:13)
data$Income_group <- factor(data$Income, levels = 1:8)

# Fit multiple linear regression
bmi_group_model <- lm(
  BMI ~ Age_group + Income_group,
  data = data
)

# Model summary
summary(bmi_group_model)

# 95% confidence intervals for coefficients
confint(bmi_group_model, level = 0.95)

# Variance explained
summary(bmi_group_model)$r.squared
summary(bmi_group_model)$adj.r.squared


# MULTIPLE REGRESSION INTERPRETATION

# The model examines how BMI differs across original
# age and income categories, without constructing exact
# ages or income amounts.

# Model results:
# R-squared: 0.03307
# Adjusted R-squared: 0.03299
# F-statistic: 456.6
# p-value: < 2.2e-16

# Age Group 1 (18-24) and Income Group 1 (<$10,000)
# are the reference categories.

# Age Group 6 (45-49) has an estimated BMI approximately
# 3.44 points higher than Age Group 1, holding income
# category constant.

# Income Group 8 ($75,000+) has an estimated BMI
# approximately 2.40 points lower than Income Group 1,
# holding age category constant.

# Age Group 13 (80+) is not significantly different
# from Age Group 1 after adjusting for income
# (p = 0.191).

# Although the overall regression is statistically
# significant, it explains only approximately 3.31%
# of the variation in BMI.

# The results describe associations, not causation.
# Statistical inference is based on conventional
# unweighted methods, without accounting for the
# complex BRFSS survey design.


