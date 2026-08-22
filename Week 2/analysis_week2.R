# ============================================================
# Week 2 Task: Data Visualization and Insight Communication
# Dataset: Cleaned Titanic dataset (continued from Week 1)
# ============================================================

library(dplyr)
library(ggplot2)

setwd("/home/claude/internship_week2")
dir.create("plots", showWarnings = FALSE)

titanic_clean <- read.csv("titanic_cleaned.csv", stringsAsFactors = FALSE)
titanic_clean$Pclass_Factor   <- factor(titanic_clean$Pclass_Factor, levels = c("1st","2nd","3rd"))
titanic_clean$Survived_Factor <- factor(titanic_clean$Survived_Factor, levels = c("No","Yes"))

# ------------------------------------------------------------
# Chart 1: Bar chart - Survival rate by class
# ------------------------------------------------------------
survival_by_class <- titanic_clean %>%
  group_by(Pclass_Factor) %>%
  summarise(Survival_Rate = mean(Survived) * 100)
cat("=== Survival by Class ===\n"); print(survival_by_class)

p1 <- ggplot(survival_by_class, aes(x = Pclass_Factor, y = Survival_Rate, fill = Pclass_Factor)) +
  geom_col() +
  geom_text(aes(label = round(Survival_Rate, 1)), vjust = -0.5) +
  labs(title = "Survival Rate by Passenger Class", x = "Passenger Class", y = "Survival Rate (%)") +
  theme_minimal() + theme(legend.position = "none")
ggsave("plots/w2_01_survival_by_class.png", p1, width = 7, height = 5)

# ------------------------------------------------------------
# Chart 2: Scatter - Age vs Fare colored by survival
# ------------------------------------------------------------
p2 <- ggplot(titanic_clean, aes(x = Age, y = Fare_Capped, color = Survived_Factor)) +
  geom_point(alpha = 0.5) +
  labs(title = "Age vs Fare, Colored by Survival", x = "Age", y = "Fare (capped)", color = "Survived") +
  theme_minimal()
ggsave("plots/w2_02_age_vs_fare_scatter.png", p2, width = 7, height = 5)

# ------------------------------------------------------------
# Chart 3: Histogram - Fare distribution, log scale
# ------------------------------------------------------------
p3_raw <- ggplot(titanic_clean, aes(x = Fare)) +
  geom_histogram(binwidth = 20, fill = "coral", color = "white") +
  labs(title = "Fare Distribution (Raw)", x = "Fare", y = "Count") +
  theme_minimal()
ggsave("plots/w2_03a_fare_raw.png", p3_raw, width = 7, height = 5)

p3_log <- ggplot(titanic_clean, aes(x = Fare)) +
  geom_histogram(bins = 30, fill = "coral", color = "white") +
  scale_x_log10() +
  labs(title = "Fare Distribution (Log Scale)", x = "Fare (log scale)", y = "Count") +
  theme_minimal()
ggsave("plots/w2_03b_fare_log.png", p3_log, width = 7, height = 5)

# ------------------------------------------------------------
# Chart 4: Line chart - Survival rate across age groups
# ------------------------------------------------------------
titanic_clean$Age_Group <- cut(titanic_clean$Age,
                                breaks = c(0,10,20,30,40,50,60,80),
                                labels = c("0-10","11-20","21-30","31-40","41-50","51-60","61-80"))
survival_by_age <- titanic_clean %>%
  group_by(Age_Group) %>%
  summarise(Survival_Rate = mean(Survived) * 100)
cat("\n=== Survival by Age Group ===\n"); print(survival_by_age)

p4 <- ggplot(survival_by_age, aes(x = Age_Group, y = Survival_Rate, group = 1)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 3) +
  labs(title = "Survival Rate Across Age Groups", x = "Age Group", y = "Survival Rate (%)") +
  theme_minimal()
ggsave("plots/w2_04_survival_by_age_group.png", p4, width = 7, height = 5)

# ------------------------------------------------------------
# Chart 5: Stacked bar - Family size vs survival
# ------------------------------------------------------------
titanic_clean$Family_Size <- titanic_clean$SibSp + titanic_clean$Parch + 1
titanic_clean$Family_Group <- cut(titanic_clean$Family_Size,
                                   breaks = c(0,1,4,11),
                                   labels = c("Alone","Small (2-4)","Large (5+)"))
survival_by_family <- titanic_clean %>%
  group_by(Family_Group) %>%
  summarise(Survival_Rate = mean(Survived) * 100, N = n())
cat("\n=== Survival by Family Group ===\n"); print(survival_by_family)

p5 <- ggplot(titanic_clean, aes(x = Family_Group, fill = Survived_Factor)) +
  geom_bar(position = "fill") +
  labs(title = "Survival Proportion by Family Size", x = "Family Size", y = "Proportion", fill = "Survived") +
  theme_minimal()
ggsave("plots/w2_05_survival_by_family.png", p5, width = 7, height = 5)

write.csv(titanic_clean, "titanic_week2_final.csv", row.names = FALSE)
cat("\n=== SCRIPT COMPLETE ===\n")
