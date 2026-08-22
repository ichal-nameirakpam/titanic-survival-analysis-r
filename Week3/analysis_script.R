# Week 3: Statistical Analysis and Predictive Modeling in R
# Dataset: Titanic (Kaggle) - cleaned in Weeks 1-2

library(tidyverse)

# ---- 1. Load and prepare data ----
titanic <- read.csv("titanic_clean.csv")

titanic$Survived <- as.factor(titanic$Survived)
titanic$Pclass   <- as.factor(titanic$Pclass)
titanic$Sex      <- as.factor(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked)

titanic <- titanic %>%
  group_by(Pclass, Sex) %>%
  mutate(Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age)) %>%
  ungroup()

titanic$Embarked[titanic$Embarked == "" | is.na(titanic$Embarked)] <- "S"
titanic$Embarked <- droplevels(titanic$Embarked)
titanic$FamilySize <- titanic$SibSp + titanic$Parch + 1
titanic$Survived <- factor(titanic$Survived, labels = c("Died", "Survived"))

# ---- 2. Hypothesis testing ----
chisq.test(table(titanic$Sex, titanic$Survived))
chisq.test(table(titanic$Pclass, titanic$Survived))
chisq.test(table(titanic$Embarked, titanic$Survived))

shapiro.test(titanic$Age[titanic$Survived == "Died"])
shapiro.test(titanic$Age[titanic$Survived == "Survived"])

par(mfrow = c(1,2))
qqnorm(titanic$Age[titanic$Survived == "Died"], main = "Q-Q: Died")
qqline(titanic$Age[titanic$Survived == "Died"])
qqnorm(titanic$Age[titanic$Survived == "Survived"], main = "Q-Q: Survived")
qqline(titanic$Age[titanic$Survived == "Survived"])
par(mfrow = c(1,1))

wilcox.test(Age ~ Survived, data = titanic)
wilcox.test(Fare ~ Survived, data = titanic)

cor(titanic[, c("Age","Fare","FamilySize")], use = "complete.obs")

# ---- 3. Train/test split (stratified, base R) ----
set.seed(42)
died_idx     <- which(titanic$Survived == "Died")
survived_idx <- which(titanic$Survived == "Survived")
train_died     <- sample(died_idx, size = floor(0.8 * length(died_idx)))
train_survived <- sample(survived_idx, size = floor(0.8 * length(survived_idx)))
train_index <- c(train_died, train_survived)
train_data  <- titanic[train_index, ]
test_data   <- titanic[-train_index, ]

# ---- 4. Fit logistic regression ----
model <- glm(Survived ~ Pclass + Sex + Age + Fare + Embarked + FamilySize + Cabin_Known,
             data = train_data, family = binomial(link = "logit"))
summary(model)
exp(coef(model))
exp(confint(model))

# Manual VIF (car package unavailable in this environment)
X <- model.matrix(model)[, -1]
vif_manual <- sapply(colnames(X), function(col) {
  fit <- lm(X[, col] ~ X[, -which(colnames(X) == col)])
  1 / (1 - summary(fit)$r.squared)
})
vif_manual

# ---- 5. 10-fold cross-validation (manual, base R) ----
manual_auc <- function(probs, actual) {
  pos <- probs[actual == "Survived"]
  neg <- probs[actual == "Died"]
  n_pos <- length(pos); n_neg <- length(neg)
  ranks <- rank(c(pos, neg))
  rank_sum_pos <- sum(ranks[1:n_pos])
  (rank_sum_pos - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
}

set.seed(42)
k <- 10
folds <- sample(rep(1:k, length.out = nrow(train_data)))
cv_accuracy <- numeric(k)
cv_auc <- numeric(k)

for (i in 1:k) {
  fold_train <- train_data[folds != i, ]
  fold_test  <- train_data[folds == i, ]
  fold_model <- glm(Survived ~ Pclass + Sex + Age + Fare + Embarked + FamilySize + Cabin_Known,
                     data = fold_train, family = binomial)
  fold_probs <- predict(fold_model, newdata = fold_test, type = "response")
  fold_pred  <- ifelse(fold_probs > 0.5, "Survived", "Died")
  cv_accuracy[i] <- mean(fold_pred == fold_test$Survived)
  cv_auc[i] <- manual_auc(fold_probs, fold_test$Survived)
}

data.frame(Fold = 1:k, Accuracy = round(cv_accuracy, 3), AUC = round(cv_auc, 3))
cat("Mean CV Accuracy:", round(mean(cv_accuracy), 3), "\n")
cat("Mean CV AUC:", round(mean(cv_auc), 3), "\n")

# ---- 6. Held-out test set evaluation ----
test_probs <- predict(model, newdata = test_data, type = "response")
test_pred  <- factor(ifelse(test_probs > 0.5, "Survived", "Died"), levels = c("Died","Survived"))

conf_matrix <- table(Predicted = test_pred, Actual = test_data$Survived)
conf_matrix

TP <- conf_matrix["Survived","Survived"]; TN <- conf_matrix["Died","Died"]
FP <- conf_matrix["Survived","Died"];     FN <- conf_matrix["Died","Survived"]

accuracy    <- (TP + TN) / sum(conf_matrix)
precision   <- TP / (TP + FP)
recall      <- TP / (TP + FN)
specificity <- TN / (TN + FP)
f1_score    <- 2 * (precision * recall) / (precision + recall)
test_auc    <- manual_auc(test_probs, test_data$Survived)

cat("Accuracy:", round(accuracy,3), "\n")
cat("Precision:", round(precision,3), "\n")
cat("Recall:", round(recall,3), "\n")
cat("Specificity:", round(specificity,3), "\n")
cat("F1 Score:", round(f1_score,3), "\n")
cat("Test AUC:", round(test_auc,3), "\n")

# Manual ROC curve
thresholds <- seq(0, 1, by = 0.01)
tpr <- numeric(length(thresholds)); fpr <- numeric(length(thresholds))
for (j in seq_along(thresholds)) {
  pred_j <- ifelse(test_probs > thresholds[j], "Survived", "Died")
  tp_j <- sum(pred_j == "Survived" & test_data$Survived == "Survived")
  fn_j <- sum(pred_j == "Died" & test_data$Survived == "Survived")
  fp_j <- sum(pred_j == "Survived" & test_data$Survived == "Died")
  tn_j <- sum(pred_j == "Died" & test_data$Survived == "Died")
  tpr[j] <- tp_j / (tp_j + fn_j)
  fpr[j] <- fp_j / (fp_j + tn_j)
}
plot(fpr, tpr, type = "l", col = "steelblue", lwd = 2,
     xlab = "False Positive Rate", ylab = "True Positive Rate",
     main = paste0("ROC Curve (AUC = ", round(test_auc, 3), ")"))
abline(a = 0, b = 1, lty = 2, col = "gray")

# ---- 7. Diagnostics ----
plot(fitted(model), residuals(model, type = "deviance"),
     xlab = "Fitted values", ylab = "Deviance residuals", main = "Residuals vs Fitted")
abline(h = 0, lty = 2, col = "red")

par(mfrow = c(2,2)); plot(model); par(mfrow = c(1,1))

cooksd <- cooks.distance(model)
plot(cooksd, type = "h", main = "Cook's Distance", ylab = "Cook's D")
abline(h = 4/nrow(train_data), col = "red", lty = 2)
influential <- which(cooksd > 4/nrow(train_data))
length(influential)
