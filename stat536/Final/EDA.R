library(splines)

dt <- read.csv('Germination.csv')
dt <- dt[, -c(1,3)]
dt$Germinated <- ifelse(dt$Germinated == 'Y', 1, 0)
dt <- dt[-which(dt$Population == 12), ]

# None of the seeds from Population 12 ever germinated
length(which(dt$Population == 12 & dt$Germinated == 'Y'))


pops <- unique(dt$Population)
years <- unique(dt$YearCollected)
chills <- unique(dt$ChillingTime)

for (pop in pops) {
  for (year in years) {
    for (chill in chills) {
      rows <- dt[which(dt$Population == pop & dt$YearCollected == year & dt$ChillingTime == chill), ]
      n <- nrow(rows)
      if (n == 0) {
        next
      }
      Y <- length(which(rows$Germinated == 1))
      cat('Pop:', pop, 'Year:', year, 'Chill:', chill, 'n:', n, 'Y:', Y, '\n')
    }
  }
}

library(MASS)
mod1 <- glm(Germinated ~ . + Population*YearCollected + Population*ChillingTime + YearCollected*ChillingTime + Population*YearCollected*ChillingTime, data = dt, family = binomial)
summary(mod1)
fin_mod1 <- stepAIC(mod1, direction = "both")
coefs <- coef(fin_mod1)
summary(fin_mod1)


# Plots of each population's ChillingTime vs. Germinated ratio
par(mfrow = c(3, 4))
for (pop in 1:12) {
  pop_dat <- dt[which(dt$Population == pop), ]
  means <- aggregate(Germinated ~ ChillingTime, data = pop_dat, FUN = mean)
  title <- paste0('Population: ', pop)
  plot(means$ChillingTime, means$Germinated, main = title, xlab = 'ChillingTime', ylab = 'GerminatedRatio', ylim = c(0,1))
}
par(mfrow = c(1, 1))

for (year in years) {
  dat <- dt[which(dt$YearCollected == year), ]
  means <- aggregate(Germinated ~ ChillingTime, data = dat, FUN = mean)
  title <- paste0('Year: ', year)
  plot(means$ChillingTime, means$Germinated, main = title, xlab = 'ChillingTime', ylab = 'GerminatedRatio', ylim = c(0,1))
}




library(ranger)
library(vip)
library(pdp)
library(ggplot2)

set.seed(1337)
train <- df <- data.frame(Population = integer(), ChillingTime = integer(), Germinated = numeric())
test <- df <- data.frame(Population = integer(), ChillingTime = integer(), Germinated = numeric()) 

for (i in 1:11) {
  ind <- sample(1:210, 21)
  pop_dat <- dt[which(dt$Population == i), ]
  train <- rbind(train, pop_dat[-ind, ])
  test <- rbind(test, pop_dat[ind, ])
}
train$Germinated <- as.factor(train$Germinated)
train$Population <- as.factor(train$Population)
test$Population <- as.factor(test$Population)

log_mod <- glm(Germinated ~ Population + Population*bs(ChillingTime, degree = 2), data = train, family = binomial)

rf <- ranger(Germinated ~ ., data = train, num.trees = 200, importance = "impurity", probability = T)  
# Levels: '0' '1'

log_cutoff <- .5
log_is_p <- ifelse(log_mod$fitted.values >= log_cutoff, 1, 0)
log_oos_p <- ifelse(predict(log_mod, newdata = test, type = 'response') >= log_cutoff, 1, 0)
log_is_acc <- mean(log_is_p == train$Germinated)
log_oos_acc <- mean(log_oos_p == test$Germinated)

rf_cutoff <- .5
rf_oos_p <- ifelse(predict(rf, data = test, type = 'response')$predictions[ ,2] >= rf_cutoff, 1, 0)
rf_is_acc <- 1 - rf$prediction.error
rf_oos_acc <- mean(rf_oos_p == test$Germinated)

summary(log_mod)

library(rpart)
library(rpart.plot)
library(tree)
model_tree <- rpart(Germinated ~ ., data = train, control=list(cp = .01))
rpart.plot(model_tree)
plotcp(model_tree)
install.packages('tree')
