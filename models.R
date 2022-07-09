# Models

# look at bad mental health days with
# LR

data <- b1 %>%
  dplyr:::mutate(ment14d = ifelse(ment14d == "0", 0, 1))

# model
mod1 <- glm(formula = factor(ment14d) ~ ., data = data, family = "binomial")
coef(mod1)
summary(mod1)

# accuracy?
pred <- predict(mod1, dplyr::select(data,  -ment14d), type = "response")
cmatrix <- data.frame(y = data$ment14d, pred = round(pred)) %>%
  dplyr::mutate(result = ifelse(y == pred, 1, 0))
table(actual = cmatrix$y, pred = cmatrix$pred)
sum(cmatrix$result)/nrow(cmatrix)

# remove lowest predictors

data <- data %>%
  dplyr::select(-X_INCOMG, -metro, -genhealth, -age)

# model
mod2 <- glm(formula = factor(ment14d) ~ ., data = data, family = "binomial")
coef(mod2)
summary(mod2)

# accuracy?
pred <- predict(mod2, dplyr::select(data,  -ment14d), type = "response")
cmatrix <- data.frame(y = data$ment14d, pred = round(pred)) %>%
  dplyr::mutate(result = ifelse(y == pred, 1, 0))
table(actual = cmatrix$y, pred = cmatrix$pred)
sum(cmatrix$result)/nrow(cmatrix)

#  model for facilities 

