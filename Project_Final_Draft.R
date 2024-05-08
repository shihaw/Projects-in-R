### ECON 3750 Project Final Draft ###
library(dplyr)
df <- read.csv('2023_nba_player_stats.csv')

df <- na.omit(df)

df <- subset(df, select = -c(POS, Team, W, L, FP))

names(df) 
dim(df)

# add dummy variable for classification on salary 
df$med_salary <- ifelse(df$Salary > median(df$Salary), 1, 0)

# since there's a lot of overlapping variables, we will get rid of unnecessary ones to avoid collinearity
df <- subset(df, select = -c(FGM, FGA, X3PM, X3PA, FTM, FTA, OREB, DREB))

### preliminary regression analysis ###
library(MASS)
library(car)
sub_df <- subset(df, select = -c(1))

set.seed(1)
test = sample(dim(sub_df)[1], dim(sub_df)[1]/5)
nba.train = sub_df[-test, ]
nba.test = sub_df[test, ]

# use all variables 
lm.fit <- lm(Salary ~ ., data = nba.train)
vif(lm.fit)
summary(lm.fit)

# use variables with vif before 4
lm.fit2 <- lm(Salary ~ Age + FG. + X3P. + FT. + BLK + TD3 + X..., data = nba.train)
summary(lm.fit2)

# use only significant variables 
lm.fit3 <- lm(Salary ~ Age + FT. + BLK + TD3 + X..., data = nba.train)
summary(lm.fit3)

# interaction term BLK*REB since most tall players get blocks and rebounds
# interaction term FT.*FG. because if a player make field goals at a high percentage, they make free throws at a high percentage
# interaction term TD3*DD2 because if a player gets a double double, they will likely get a triple double 
lm.fit4 <- lm(Salary ~ Age + (FT.*FG.) + (BLK*REB) + (TD3*DD2) + X..., data = nba.train)
summary(lm.fit4)

# lm.fit has the highest adjusted R^2 
# adj R^2 kept going down with each fit 
par(mfrow = c(2,2))
plot(lm.fit)
mean(residuals(lm.fit))
# linear model doesn't seem to be good fit because of all the outliers and high leverage points 

# lets see how it predicts 
pred <- predict(lm.fit, nba.test)
mse <- mean((nba.test$Salary - pred)^2)
mse
# not bad
pred <- predict(lm.fit2, nba.test)
mse <- mean((nba.test$Salary - pred)^2)
mse
# worse
pred <- predict(lm.fit3, nba.test)
mse <- mean((nba.test$Salary - pred)^2)
mse
# little better 
pred <- predict(lm.fit4, nba.test)
mse <- mean((nba.test$Salary - pred)^2)
mse

# adding complexity to linear model gave a better MSE 


### Classification Analysis ###

# Logistic Regression #
# leave out salary column
glm.fit <- glm(med_salary ~ ., data = sub_df[,-1], family = binomial)
summary(glm.fit)

glm.probs <- predict(glm.fit, type = "response")
glm.preds <- rep(0, length(glm.probs))
glm.preds[glm.probs > 0.5] <- 1
table(glm.preds, sub_df$med_salary)
(219+205)/535
1 - ((219+205)/535)
# The percentage of correct predictions on the training data is 79.25234%. 
# 20.74766% is athe test error rate 

set.seed(1)
train <- sample(535, 535*(4/5))
nba_test_df <- sub_df[-train,]
test.med_salary<- sub_df$med_salary[-train]

# Age and GP were the only significant predictors from the previous model 
glm.fits2 <- glm(med_salary ~ Age + GP, data = sub_df[,-1], family = binomial, subset = train)
summary(glm.fits2)

glm.probs2 <- predict(glm.fits2, nba_test_df[-1], type = "response")
glm.preds2 <- rep(0, length(glm.probs2))
glm.preds2[glm.probs2 > 0.5] <- 1
table(glm.preds2, test.med_salary)
(38+40)/107
1 - ((38+40)/107)
# The percentage of correct predictions on the test data is 72.8972%. 
# 27.1028% is the test error rate which is worse than before


# LDA # 
lda.fit <- lda(med_salary ~ Age + GP, data = sub_df[-1], subset = train)
lda.fit
lda.pred <- predict(lda.fit, nba_test_df[-1])
table(lda.pred$class, test.med_salary)
(37+40)/107
1 - ((37+40)/107)
# The percentage of correct predictions on the test data is 71.96262%. 
# 28.03738% is the test error rate which is about the same

# QDA # 
qda.fit <- qda(med_salary ~ Age + GP, data = sub_df[-1], subset = train)
qda.fit
qda.pred <- predict(qda.fit, nba_test_df[-1])
table(qda.pred$class, test.med_salary)
(35+40)/107
1 - ((35+40)/107)
# The percentage of correct predictions on the test data is 70.09346%. 
# 29.90654% is the test error rate which is about the same

# KNN # (try scaling)
library(class)

train.X <- cbind(sub_df$Age[train], sub_df$GP[train])
test.X <- cbind(sub_df$Age[-train], sub_df$GP[-train])
train.med_salary <- sub_df$med_salary[train]

set.seed(1)
knn.pred <- knn(train.X, test.X, train.med_salary, k = 1)
table(knn.pred, test.med_salary)
(37+29)/107
1 - ((37+29)/107)
# The percentage of correct predictions on the test data is 61.68224%. 
# 38.31776% is the test error rate which is much worse

# k = 5
set.seed(1)
knn.pred <- knn(train.X, test.X, train.med_salary, k = 5)
table(knn.pred, test.med_salary)
1 - ((32+24)/107)
# 47.66355% is the test error rate which is much worse

# k = 10
set.seed(1)
knn.pred <- knn(train.X, test.X, train.med_salary, k = 10)
table(knn.pred, test.med_salary)
1 - ((32+33)/107)
# 39.25234% is the test error rate which is much worse

# Naive Bayes # 
library(e1071)

nb.fit <- naiveBayes(med_salary ~ Age + GP, data = sub_df[-1], subset = train)
nb.fit
nb.class <- predict(nb.fit, nba_test_df[-1])
table(nb.class, test.med_salary)
(34+40)/107
1 - ((34+40)/107)
# The percentage of correct predictions on the test data is 69.15888%. 
# 30.84112% is the test error rate which is much worse

# Of all models, logistic regression performed the best

glm.fits3 <- glm(med_salary ~ Age + GP + (Age*GP), data = sub_df[,-1], family = binomial, subset = train)
summary(glm.fits3)

glm.probs3 <- predict(glm.fits3, nba_test_df[-1], type = "response")
glm.preds3 <- rep(0, length(glm.probs3))
glm.preds3[glm.probs3 > 0.5] <- 1
table(glm.preds3, test.med_salary)
(40+40)/107
1 - ((40+40)/107)
# The percentage of correct predictions on the test data is 74.76636%. 
# 25.23364% is the test error rate which is better than the glm.fits2
# adding the interaction term reduced the test error rate 

# Evaluate Above Model # 
# LOOCV # 
sub_df2 <- sub_df[,-1]
correct_pred <- rep(0, nrow(sub_df2))

for(i in 1:nrow(sub_df2)) {
  sub_df2_no_i <- sub_df2[-i,]
  glm.fit4 <- glm(med_salary ~ Age + GP + (Age*GP), data=sub_df2_no_i,
                  family="binomial")
  pred_Direct <- ifelse(predict(glm.fit4, sub_df2[i,], type = "response") > 0.5, 1,
                        0)
  correct_pred[i] <- ifelse(pred_Direct == sub_df2[i,]$med_salary, 1, 0)
}
1-mean(correct_pred)
1-mean(sub_df2$med_salary == 1)
# the LOOCV estimate for the test error rate is 0.2971963
# if we simply guessed that a player's salary was above the median every time, 
# our model would have performed much worse
# Therefore, our model performs pretty well

# k-fold CV #
library(boot)
set.seed(1)

cv.error.10 <- rep(0,10)
for (i in 1:10) { 
  glm.fit <- glm(med_salary ~ poly(Age, i) + poly(GP, i) + poly(Age*GP, i), data = sub_df2)
  cv.error.10[i] <- cv.glm(sub_df2, glm.fit, K = 10)$delta[1]
}
plot(cv.error.10)
# we can see that adding a quadratic term doesn't improve the error compared to the simple linear model

cv.error.10 <- rep(0,10)
for (i in 1:10) { 
  glm.fit <- glm(med_salary ~ poly(Age, i) + poly(GP, i) + poly(Age*GP, i), data = sub_df2)
  cv.error.10[i] <- cv.glm(sub_df2, glm.fit, K = 5)$delta[1]
}
plot(cv.error.10)
# the same thing happens for k = 5

# bootstrapping #
# linear model 
boot.fn <- function(data, index) {
  coef(lm(Salary ~ Age + (FT.*FG.) + (BLK*REB) + (TD3*DD2) + X..., data = data, subset = index))
}

boot.fn(sub_df, 1:535)
lm(Salary ~ Age + (FT.*FG.) + (BLK*REB) + (TD3*DD2) + X..., data = sub_df)


boot.fn(sub_df, sample(535,535,replace = T))

boot(sub_df, boot.fn, 1000)

summary(lm(Salary ~ Age + (FT.*FG.) + (BLK*REB) + (TD3*DD2) + X..., data = sub_df))$coef

# glm model 
boot.fn <- function(data, index) {
  coef(glm(med_salary ~ Age + GP + (Age*GP), data = data, subset = index, family = binomial))
}

boot.fn(sub_df2, train)
glm(med_salary ~ Age + GP + (Age*GP), data = sub_df2, subset = train, family = binomial)


boot(sub_df2, boot.fn, 1000)
summary(glm(med_salary ~ Age + GP + (Age*GP), data = sub_df2, family = binomial))$coef

### Linear Model Selection and Regularization ###

# best subset model selection
library(leaps)
# no med_salary
sub_df3 <- sub_df[,-18]

set.seed(1)
test = sample(dim(sub_df3)[1], dim(sub_df3)[1]/5)
nba.train = sub_df3[-test, ]
nba.test = sub_df3[test, ]

regfit.best <- regsubsets(Salary ~ ., data = nba.train, nvmax = 16)
test.mat <- model.matrix(Salary ~ ., data = nba.test)

val.errors <- rep(NA, 16)

for(i in 1:16) {
  coefi <- coef(regfit.best, id = i)
  pred <- test.mat[, names(coefi)] %*% coefi
  val.errors[i] <- mean((nba.test$Salary - pred)^2)
}

val.errors
which.min(val.errors)
# The 12th model performed the best

coef(regfit.best, 12)

#find the best 12 variable model using the entire data
regfit.best <- regsubsets(Salary ~ ., data = sub_df3, nvm = 16)
coef(regfit.best, 12)

# Now, do CV to perform best subset selection 
k <- 10
n <- nrow(sub_df3)
set.seed(1)
folds <- sample(rep(1:k, length = n))
cv.errors <- matrix(NA, k, 16, dimnames = list(NULL, paste(1:16)))

predict.regsubsets <- function(object, newdata, id, ...)  {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}

for(j in 1:k) {
  best.fit <- regsubsets(Salary ~ ., data = sub_df3[folds != j,], nvmax = 16)
  for(i in 1:16) {
    pred <- predict(best.fit, sub_df3[folds == j, ], id = i)
    cv.errors[j, i] <- mean((sub_df3$Salary[folds == j] - pred)^2)
  }
}

mean.cv.errors <- apply(cv.errors, 2, mean)
which.min(mean.cv.errors)
#Looking at these results, the 6-variable model performs best. We will do
#best subset selection on the full dataset and pick the best 6-variable model.
regfit.best <- regsubsets(Salary ~ ., data = sub_df3, nvm = 16)
coef(regfit.best, 6)

#Principal Components Regression
library(pls)
set.seed(1)

pcr.fit <- pcr(Salary ~ ., data = sub_df3, scale = TRUE, validation = "CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type = "MSEP")
#The smallest CV error occurs when M = 12 

set.seed(1)

pcr.fit <- pcr(Salary ~ ., data = sub_df3, subset = train, scale = TRUE,
               validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")

pcr.pred <- predict(pcr.fit, sub_df3[-train,], ncomp = 12)
mean((pcr.pred - sub_df3$Salary[-train])^2)
# test MSE of 39,669,196,327,533.6

# Now fit PCR on the entire dataset using M = 12
pcr.fit <- pcr(Salary ~., data = sub_df3,scale = TRUE, ncomp = 12)
summary(pcr.fit)

# partial least squares 
set.seed(1)
pls.fit <- plsr(Salary ~ ., data = sub_df3, scale = TRUE, validation = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP")
#It looks like the lowest CV error occurs when M = 5 PLS directions are used.
#We will now evaluate the corresponding test set MSE

pls.pred <- predict(pls.fit, sub_df3[test,], ncomp = 5)
mean((pls.pred - sub_df3$Salary[-train])^2)
# test MSE: 38,077,352,390,903.6
# lower than for PCR

pcr.fit <- pcr(Salary ~., data = sub_df3,scale = TRUE, ncomp = 5)
summary(pcr.fit)

### NonLinear Models ###
# already seen adding more quadratic terms doesn't make the model better, so perform splines 
library(splines)

# cubic spline
agelims <- range(sub_df3$Age)
age.grid <- seq(from = agelims[1], to = agelims[2])

fit <- lm(Salary ~ bs(Age, knots = c(22, 26, 30)),
          data = sub_df3)
pred <- predict(fit, newdata = list(Age = age.grid),
                se = T)
mean((pred$se.fit^2 + pred$residual.scale^2))
plot(sub_df3$Age, sub_df3$Salary, col = "gray")
lines(age.grid, pred$fit, lwd = 2)
lines(age.grid, pred$fit + 2 * pred$se, lty = "dashed")
lines(age.grid, pred$fit - 2 * pred$se, lty = "dashed")

GPlims <- range(sub_df3$GP)
GP.grid <- seq(from = GPlims[1], to = GPlims[2])


fit2 <- lm(Salary ~ bs(GP, knots = c(35, 52, 70)),
          data = sub_df3)
pred2 <- predict(fit2, newdata = list(GP = GP.grid),
                se = T)
mean((pred2$se.fit^2 + pred2$residual.scale^2))
plot(sub_df3$GP, sub_df3$Salary, col = "gray")
lines(GP.grid, pred2$fit, lwd = 2)
lines(GP.grid, pred2$fit + 2 * pred2$se, lty = "dashed")
lines(GP.grid, pred2$fit - 2 * pred2$se, lty = "dashed") 

# natural spline
fit3 <- lm(Salary ~ ns(Age, df = 6), data = sub_df3)
pred3 <- predict(fit3, newdata = list(Age = age.grid), se = T)
mean((pred3$se.fit^2 + pred3$residual.scale^2))
lines(age.grid, pred3$fit, col = "red", lwd = 2)

fit4 <- lm(Salary ~ ns(GP, df = 6), data = sub_df3)
pred4 <- predict(fit4, newdata = list(GP = GP.grid), se = T)
mean((pred4$se.fit^2 + pred4$residual.scale^2))
lines(GP.grid, pred4$fit, col = "red", lwd = 2)

# smoothing spline
plot(sub_df3$Age, sub_df3$Salary, xlim = agelims, cex = 0.5,
     col = "darkgrey")
fit <- smooth.spline(sub_df3$Age, sub_df3$Salary, df = 16)
fit2 <- smooth.spline(sub_df3$Age, sub_df3$Salary, cv = TRUE)
fit2$df
lines(fit, col = "red", lwd = 2)
lines(fit2, col = "blue", lwd = 2)
legend("topright", legend = c("16 DF", "5 DF"),
       col = c("red", "blue"), lty = 1, lwd = 2, cex = 0.8)

plot(sub_df3$GP, sub_df3$Salary, xlim = GPlims, cex = 0.5,
     col = "darkgrey")
fit <- smooth.spline(sub_df3$GP, sub_df3$Salary, df = 16)
fit2 <- smooth.spline(sub_df3$GP, sub_df3$Salary, cv = TRUE)
fit2$df
lines(fit, col = "red", lwd = 2)
lines(fit2, col = "blue", lwd = 2)
legend("topright", legend = c("16 DF", "5.4 DF"),
       col = c("red", "blue"), lty = 1, lwd = 2, cex = 0.8)


### Tree-Based Methods ###
library(tree)
# predict whether salary is high 
sub_df3$High <- factor(ifelse(sub_df3$Salary <= 5000000, "No", "Yes"))
tree.nba <- tree(High ~ . - Salary, data = sub_df3)
summary(tree.nba)
#Training error rate is 16.64%
#The residual mean deviance is 535 - 14 = 521
plot(tree.nba)
text(tree.nba, pretty = 0)

# estimate test error
set.seed(1)
train <- sample(1:nrow(sub_df3), 428)
nba.test <- sub_df3[-train,]
High.test <- sub_df3$High[-train]

tree.nba <- tree(High ~ . - Salary, data = sub_df3, subset = train)
tree.pred <- predict(tree.nba, nba.test, type = "class")
table(tree.pred, High.test)
(52+29)/107
# 0.7570093

set.seed(1)
cv.nba <- cv.tree(tree.nba, FUN = prune.misclass)
#The tree with 22 terminal nodes has the fewest errors
par(mfrow=c(1,2))
plot(cv.nba$size, cv.nba$dev, type = "b")
plot(cv.nba$k, cv.nba$dev, type = "b")

#Apply prune.misclass() to prune our tree to have 22 nodes
prune.nba <- prune.misclass(tree.nba, best = 22)
par(mfrow=c(1,1))
plot(prune.nba)
text(prune.nba, pretty = 0)

tree.pred <- predict(prune.nba, nba.test, type = "class")
table(tree.pred, High.test)
(52 + 29) / 107
#now have a tree that is more interpretable with the same accuracy

# fitting regression trees
tree.nba <- tree(Salary ~ .- High, data = sub_df3, subset = train)
summary(tree.nba)
# 8 predictors used to construct tree
# sum of squared errors: 2.42e+13
plot(tree.nba)
text(tree.nba, pretty = 0)

# prune tree
cv.nba<- cv.tree(tree.nba)
plot(cv.nba$size, cv.nba$dev, type = "b")
#The tree with 5 terminal nodes has the fewest errors

yhat <- predict(tree.nba, newdata = sub_df3[-train,])
nba.test <- sub_df3[-train, "Salary"]
plot(yhat, nba.test)
abline(0, 1)
mean((yhat - nba.test)^2)
# The test MSE is 52,252,207,002,138.4

# baggin and random forests 
library(randomForest)
sub_df4 <- sub_df[,-18]
set.seed(1)
bag.nba <- randomForest(Salary ~ ., data = sub_df4, subset = train,
                           mtry = 16, importance = TRUE)
bag.nba

yhat.bag <- predict(bag.nba, newdata = sub_df4[-train,])
plot(yhat.bag, nba.test)
abline(0, 1)
mean((yhat.bag - nba.test)^2)
# test MSE: 36,038,359,561,760

# random forest 
set.seed(1)
rf.nba <- randomForest(Salary ~ ., data = sub_df4, subset = train,
                          mtry = 8, importance = TRUE)
yhat.rf <- predict(rf.nba, newdata = sub_df4[-train,])
mean((yhat.rf - nba.test)^2)
# test MSE: 38,191,961,244,149.2 
# a little worse
importance(rf.nba)
varImpPlot(rf.nba)

# boosting 
library(gbm)

set.seed(1)
boost.nba <- gbm(Salary ~ ., data = sub_df4[train,],
                    distribution = "gaussian", n.trees = 5000,
                    interaction.depth = 4)
summary(boost.nba)
# PTS and Age are most important variables 
yhat.boost <- predict(boost.nba, newdata = sub_df4[-train,],
                      n.trees = 5000)
mean((yhat.boost - nba.test)^2)
# test MSE: 45,069,844,851,721.9 
# boosting does worse than bagging and random forest 

# Bayesian Additive Regression Trees 
library(BART)

X <- sub_df4[, 2:17]
Y <- sub_df4[, "Salary"]
X_train <- X[train,]
Y_train <- Y[train]
X_test <- X[-train,]
Y_test <- Y[-train]

set.seed(1)
bartfit <- gbart(X_train, Y_train, x.test = X_test)

yhat.bart <- bartfit$yhat.test.mean
mean((Y_test - yhat.bart)^2)
# test MSE: 35,452,686,849,298.2 


