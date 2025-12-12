library(splines)

dt <- read.csv('Germination.csv')
dt <- dt[, -1]
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

dat <- dt[which(dt$YearCollected == 2015), ]
means <- aggregate(Germinated ~ ChillingTime, data = dat, FUN = mean)

year_2016 <- dt[which(dt$YearCollected == 2016), ]
unique(year_2016$)

means <- aggregate(Germinated ~ YearCollected, data = dt, FUN = mean)
plot(means$YearCollected, means$Germinated, xlab = 'Year', ylab = 'GerminatedRatio', ylim = c(0,1), xlim = c(2013, 2017))


mod1 <- glm(Germinated ~ Population + bs(ChillingTime, degree = 2) + Population*ChillingTime, data = dt, family = binomial)
summary(mod1)
