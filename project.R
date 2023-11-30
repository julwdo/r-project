library(foreign)
library(tidyverse)
library(caret)
library(lmtest)
library(sandwich)
library(car)

setwd("D:/Studies/Materials/Second-cycle/I year/I semester/Coding for DS and DM/R/r-project")

data <- read.spss("bankloan.sav", to.data.frame = TRUE) %>%
  select(-matches("preddef"))
head(data)

data %>%
  summarize_all(~sum(is.na(.)))

data %>%
  filter(!is.na(default)) %>% # Filter out future clients
  ggplot(., aes(x = age)) +
  geom_histogram(alpha = 0.2, col = "black", fill = "blue") +
  labs(title = "Number of customers by age",
       x = "Age", y = "Number of customers") +
  scale_x_continuous(breaks = seq(20, 60, 5)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

data %>%
  filter(!is.na(default)) %>% # Filter out future clients
  ggplot(., aes(x = ed)) +
  geom_bar(alpha = 0.2, col = "black", fill = "yellow") +
  labs(title = "Number of customers by level of education",
       x = "Level of education", y = "Number of customers") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

data %>%
  filter(!is.na(default)) %>% # Filter out future clients
  ggplot(., aes(x = default)) +
  geom_bar(alpha = 0.2, col = "black", fill = "red") +
  labs(title = "Number of customers by default",
       x = "Default", y = "Number of customers") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

data <- data %>%
  mutate(
    ed_1 = as.numeric(ed == "Did not complete high school"),
    ed_2 = as.numeric(ed == "High school degree"),
    ed_3 = as.numeric(ed == "Some college"),
    ed_4 = as.numeric(ed == "College degree"),
    ed_5 = as.numeric(ed == "Post-undergraduate degree"),
    default = as.numeric(default == "Yes")
  ) %>%
  select(-ed)

data %>%
  summarize(across(c("ed_1", "ed_2", "ed_3", "ed_4", "ed_5"), ~sum(., na.rm = T)))

data.1 <- data %>%
  select(-ed_1) %>% # To avoid perfect multicollinearity
  filter(!is.na(default)) # Filter out future clients

set.seed(123)
index <- createDataPartition(data.1$default, p = 0.7, list = FALSE)
train <- data[index, ]
test <- data[-index, ]

fit.full <- glm(default ~ ., family = binomial(), data = train)
summary(fit.full)

fit.robust <- coeftest(fit.full, vcov = vcovHC(fit.full, type = "HC"))
fit.robust

linearHypothesis(fit.full, c("income", "othdebt", "ed_2", "ed_3", "ed_4", "ed_5"))
