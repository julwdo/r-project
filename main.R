# Loading required libraries
library(foreign)
library(dplyr)
library(ggplot2)
library(correlation)
library(lmtest)
library(sandwich)
library(caret)
library(xgboost)

# Loading the data set
path <- "D:/Studies/Materials/Second-cycle/I year/I semester/Coding for DS and DM/R/r-project"

data <- read.spss(paste0(path, "/bankloan.sav"), to.data.frame = TRUE) %>%
  select(-matches("preddef"))

str(data)

# Summary statistics
summary(data)

# Missing values
sapply(data, function(x) sum(is.na(x)))

# Visualizations
# Example: Age distribution
ggplot(data, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Age Distribution") +
  theme_minimal()

# Creating binary variables out of "ed" and "default"
data <- data %>%
  mutate(
    ed_1 = as.factor(as.numeric(ed == "Did not complete high school")),
    ed_2 = as.factor(as.numeric(ed == "High school degree")),
    ed_3 = as.factor(as.numeric(ed == "Some college")),
    ed_4 = as.factor(as.numeric(ed == "College degree")),
    ed_5 = as.factor(as.numeric(ed == "Post-undergraduate degree")),
    default_num = as.factor(as.numeric(default == "Yes"))
    )

# Preparing data for estimation
data.1 <- data %>%
  select(-ed, -ed_1, -default) %>% # Remove "ed_1" to avoid perfect multicollinearity
  filter(!is.na(default_num))

# Splitting the data into training and testing sets
set.seed(123)
indices <- sample(nrow(data.1), size = 0.7*nrow(data.1))
data.train <- data.1[indices, ]
data.test <- data.1[-indices, ]

dim(data.train)
dim(data.test)

# Correlation between independent variables
data.train %>% select(-default_num) %>% correlation()

# Logistic Regression
lr.full <- glm(default_num ~ ., family = binomial(), data = data.train)
summary(lr.full)

lr.robust.std <- coeftest(lr.full, vcov = vcovHC(lr.full, type = "HC"))
lr.robust.std

lr.reduced <- glm(default_num ~ employ + address + debtinc + creddebt + ed_2,
                  family = binomial(), data = data.train)
summary(lr.reduced)

# XGBoost
X.train <- as.matrix(data.train %>%
                       select(-default_num) %>%
                       mutate(across(starts_with("ed"),
                                     ~as.numeric(.) - 1)))
X.test <- as.matrix(data.test %>%
                      select(-default_num) %>%
                      mutate(across(starts_with("ed"),
                                    ~as.numeric(.) - 1)))
y.train <- as.numeric(data.train$default_num) - 1
y.test <- as.numeric(data.test$default_num) - 1

d.train <- xgb.DMatrix(X.train, label = y.train)

params <- list(objective = "binary:logistic", eval_metric = "logloss")
xgb <- xgboost(params = params, data = d.train, nrounds = 100)

# Predictions
lr.prob.pred <- predict(lr.reduced, newdata = data.test, type = "response")
xgb.prob.pred <- predict(xgb, newdata = X.test)

lr.y.pred <- as.numeric(lr.prob.pred > 0.5)
xgb.y.pred <- as.numeric(xgb.prob.pred > 0.5)

# Confusion matrix
confusionMatrix(table(lr.y.pred, data.test$default_num), positive = "1")
confusionMatrix(table(xgb.y.pred, y.test), positive = "1")
