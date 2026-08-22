# ============================================================
# Week 1 Task: Data Cleaning and Preliminary Analysis with R
# Dataset: Titanic Passenger Data (public dataset)
# Source: https://raw.githubusercontent.com/datasciencedojo/datasets/master/titanic.csv
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)

setwd("/home/claude/internship_week1")
dir.create("plots", showWarnings = FALSE)

# ------------------------------------------------------------
# 1. LOAD DATA
# ------------------------------------------------------------
titanic <- read.csv("titanic.csv", stringsAsFactors = FALSE, na.strings = c("", "NA"))

cat("=== STRUCTURE (str) ===\n")
str(titanic)

cat("\n=== FIRST 6 ROWS ===\n")
print(head(titanic))

cat("\n=== DIMENSIONS ===\n")
print(dim(titanic))

# ------------------------------------------------------------
# 2. MISSING VALUE ANALYSIS
# ------------------------------------------------------------
cat("\n=== MISSING VALUES PER COLUMN ===\n")
missing_counts <- colSums(is.na(titanic))
missing_pct <- round(100 * missing_counts / nrow(titanic), 2)
missing_table <- data.frame(Column = names(missing_counts),
                             Missing_Count = missing_counts,
                             Missing_Percent = missing_pct)
missing_table <- missing_table[order(-missing_table$Missing_Count), ]
print(missing_table, row.names = FALSE)

# Visualize missingness
png("plots/01_missing_values.png", width = 800, height = 500)
barplot(missing_table$Missing_Percent, names.arg = missing_table$Column,
        las = 2, col = "steelblue", ylab = "Missing (%)",
        main = "Percentage of Missing Values per Column")
dev.off()

# ------------------------------------------------------------
# 3. DATA CLEANING
# ------------------------------------------------------------

# --- 3a. Drop columns that are not useful for analysis or almost entirely missing ---
# Cabin is ~77% missing -> create a flag instead of imputing, then drop raw column
titanic$Cabin_Known <- ifelse(is.na(titanic$Cabin), 0, 1)
titanic_clean <- titanic %>% select(-Cabin, -Ticket, -Name, -PassengerId)

# --- 3b. Impute missing Age using median (robust to outliers, standard for skewed data) ---
age_median <- median(titanic_clean$Age, na.rm = TRUE)
titanic_clean$Age[is.na(titanic_clean$Age)] <- age_median
cat("\nAge missing values imputed with median:", age_median, "\n")

# --- 3c. Impute missing Embarked using mode (categorical) ---
get_mode <- function(v) {
  v <- v[!is.na(v)]
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
embarked_mode <- get_mode(titanic_clean$Embarked)
titanic_clean$Embarked[is.na(titanic_clean$Embarked)] <- embarked_mode
cat("Embarked missing values imputed with mode:", embarked_mode, "\n")

# --- 3d. Confirm no missing values remain ---
cat("\n=== MISSING VALUES AFTER CLEANING ===\n")
print(colSums(is.na(titanic_clean)))

# ------------------------------------------------------------
# 4. OUTLIER DETECTION (Fare, Age) using IQR method
# ------------------------------------------------------------
detect_outliers <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  sum(x < lower | x > upper)
}

cat("\n=== OUTLIER COUNTS (IQR method) ===\n")
cat("Fare outliers:", detect_outliers(titanic_clean$Fare), "\n")
cat("Age outliers:", detect_outliers(titanic_clean$Age), "\n")

png("plots/02_boxplot_fare_age.png", width = 800, height = 500)
par(mfrow = c(1, 2))
boxplot(titanic_clean$Fare, main = "Fare - Boxplot", col = "tomato", ylab = "Fare")
boxplot(titanic_clean$Age, main = "Age - Boxplot", col = "lightgreen", ylab = "Age")
dev.off()

# Cap Fare outliers at the 99th percentile (winsorizing) rather than deleting rows,
# since extreme fares are real first-class ticket prices, not data errors
fare_cap <- quantile(titanic_clean$Fare, 0.99, na.rm = TRUE)
titanic_clean$Fare_Capped <- ifelse(titanic_clean$Fare > fare_cap, fare_cap, titanic_clean$Fare)
cat("\nFare 99th percentile cap applied at:", round(fare_cap, 2), "\n")

# ------------------------------------------------------------
# 5. NORMALIZATION (Min-Max scaling of numeric variables)
# ------------------------------------------------------------
min_max_scale <- function(x) (x - min(x)) / (max(x) - min(x))

titanic_clean$Age_Scaled  <- min_max_scale(titanic_clean$Age)
titanic_clean$Fare_Scaled <- min_max_scale(titanic_clean$Fare_Capped)

cat("\n=== SUMMARY OF SCALED VARIABLES ===\n")
print(summary(titanic_clean[, c("Age_Scaled", "Fare_Scaled")]))

# ------------------------------------------------------------
# 6. ENCODING CATEGORICAL VARIABLES
# ------------------------------------------------------------
# Label encode Sex (binary)
titanic_clean$Sex_Encoded <- ifelse(titanic_clean$Sex == "male", 1, 0)

# One-hot encode Embarked (3 categories: C, Q, S)
embarked_dummies <- model.matrix(~ Embarked - 1, data = titanic_clean)
titanic_clean <- cbind(titanic_clean, embarked_dummies)

# Convert Pclass, Survived to factors (they are categorical despite numeric coding)
titanic_clean$Pclass_Factor   <- factor(titanic_clean$Pclass, labels = c("1st", "2nd", "3rd"))
titanic_clean$Survived_Factor <- factor(titanic_clean$Survived, labels = c("No", "Yes"))

cat("\n=== STRUCTURE AFTER ENCODING ===\n")
str(titanic_clean)

# ------------------------------------------------------------
# 7. EXPLORATORY DATA ANALYSIS
# ------------------------------------------------------------
cat("\n=== SUMMARY STATISTICS (summary) ===\n")
print(summary(titanic_clean[, c("Age", "Fare", "SibSp", "Parch")]))

cat("\n=== SURVIVAL RATE OVERALL ===\n")
print(round(prop.table(table(titanic_clean$Survived_Factor)) * 100, 2))

cat("\n=== SURVIVAL RATE BY SEX ===\n")
print(round(prop.table(table(titanic_clean$Sex, titanic_clean$Survived_Factor), margin = 1) * 100, 2))

cat("\n=== SURVIVAL RATE BY CLASS ===\n")
print(round(prop.table(table(titanic_clean$Pclass_Factor, titanic_clean$Survived_Factor), margin = 1) * 100, 2))

# Correlation matrix (numeric variables only)
num_vars <- titanic_clean %>% select(Survived, Pclass, Age, SibSp, Parch, Fare, Sex_Encoded)
corr_matrix <- cor(num_vars, use = "complete.obs")
cat("\n=== CORRELATION MATRIX ===\n")
print(round(corr_matrix, 2))

png("plots/03_correlation_matrix.png", width = 700, height = 600)
corrplot(corr_matrix, method = "color", type = "upper", addCoef.col = "black",
         tl.col = "black", tl.srt = 45, number.cex = 0.8,
         title = "Correlation Matrix of Numeric Variables", mar = c(0,0,2,0))
dev.off()

# --- Visualization 1: Age distribution ---
p1 <- ggplot(titanic_clean, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Passenger Age", x = "Age", y = "Count") +
  theme_minimal()
ggsave("plots/04_age_distribution.png", p1, width = 7, height = 5)

# --- Visualization 2: Survival count by class and sex ---
p2 <- ggplot(titanic_clean, aes(x = Pclass_Factor, fill = Survived_Factor)) +
  geom_bar(position = "dodge") +
  facet_wrap(~Sex) +
  labs(title = "Survival Count by Passenger Class and Sex",
       x = "Passenger Class", y = "Count", fill = "Survived") +
  theme_minimal()
ggsave("plots/05_survival_by_class_sex.png", p2, width = 8, height = 5)

# --- Visualization 3: Fare distribution by class ---
p3 <- ggplot(titanic_clean, aes(x = Pclass_Factor, y = Fare_Capped, fill = Pclass_Factor)) +
  geom_boxplot() +
  labs(title = "Fare Distribution by Passenger Class (Outlier-Capped)",
       x = "Passenger Class", y = "Fare") +
  theme_minimal() + theme(legend.position = "none")
ggsave("plots/06_fare_by_class.png", p3, width = 7, height = 5)

# --- Visualization 4: Survival rate by Embarked port ---
p4 <- ggplot(titanic_clean, aes(x = Embarked, fill = Survived_Factor)) +
  geom_bar(position = "fill") +
  labs(title = "Survival Rate by Port of Embarkation",
       x = "Port Embarked", y = "Proportion", fill = "Survived") +
  theme_minimal()
ggsave("plots/07_survival_by_embarked.png", p4, width = 7, height = 5)

# ------------------------------------------------------------
# 8. SAVE CLEANED DATASET
# ------------------------------------------------------------
write.csv(titanic_clean, "titanic_cleaned.csv", row.names = FALSE)
cat("\nCleaned dataset saved as titanic_cleaned.csv\n")
cat("\n=== SCRIPT COMPLETE ===\n")
