# EEB 338/538 Analysis and Visualization of Biological Data
# Class 12: Linear models and interaction terms

# Start of part 1

## Sex interaction effect example -------------------

y <- c(89, 88, 66, 59, 93, 73, 82, 77, 100, 67, 57, 68, 69, 59, 62, 59, 56, 66, 72)
x <- c(28, 27, 24, 23, 29, 25, 29, 25, 30, 23, 29, 32, 35, 31, 29, 26, 28, 33, 33)
m <- c(rep(0, 10), rep(1, 9))
dat <- data.frame(y = y, x = x, male = m)

plot(y ~ x, cex = 2, pch = ifelse(dat$male == 1, 16, 16),
     col = ifelse(dat$male == 1, "blue", "orange")) 


#1a Run a model of y as a function of x that ignores sex:

#b Examine a summary of the model

#c Make a plot of the data from the model above add the regression line of best fit


#2a Now write a model that includes sex as main effect:

#b Examine a summary of the model

#c Make a plot of the data from the model above add the regression line of best fit for each sex


#3a Now write a model that includes an interaction between x and sex

#b Examine a summary of the model

#c Make a plot of the data from the model above add the regression line of best fit for each sex



## An example - run through this code to see what's going on -------------------

# Data on economic indicators and terrain ruggedness, adapted from dataset used in McElreath (2016)
d <- read.csv("class12_rugged.csv", stringsAsFactors = T)

str(d)  

# Select only countries with GDP data

d2 <- d[complete.cases(d$rgdppc_2000),]

# log outcome variable
d2$GDP <- log(d2$rgdppc_2000)


# Plot full data set
plot(d2$GDP ~ d2$rugged,
     col = 1, pch =16)
m <- lm(GDP~rugged, data = d2)
summary(m)
abline(m, lwd = 2)

xpoints <- seq(min(d2$rugged), max(d2$rugged), length.out = 100)
ypoints <- data.frame(predict(m, newdata = data.frame(rugged = xpoints), interval = "confidence", level = 0.95))
lines(ypoints$lwr ~ xpoints, lty = 2)
lines(ypoints$upr ~ xpoints, lty = 2)



# Split Africa and non-Africa, plot
d.A1 <- d2[d2$cont_africa == 1, ]
d.A0 <- d2[d2$cont_africa == 0, ]

par(mfrow=c(2,1))

plot(log(d.A1$rgdppc_2000) ~ d.A1$rugged,
     col = "orange", pch =16,
     xlab = "terrain ruggedness index",
     ylab = "log(GDP)")
m.A1 <- lm(log(d.A1$rgdppc_2000) ~ d.A1$rugged)
abline(m.A1, col = "orange", lwd = 2)

plot(log(d.A0$rgdppc_2000) ~ d.A0$rugged,
     col = "blue", pch =16,
     xlab = "terrain ruggedness index",
     ylab = "log(GDP)")
m.A1 <- lm(log(d.A0$rgdppc_2000) ~ d.A0$rugged)
abline(m.A1, col = "blue", lwd = 2)

# Dummy variable doesn't work (because slope can't differ)
m.dummy <- lm(GDP ~ rugged + cont_africa, data = d2)

summary(m.dummy)

par(mfrow=c(1,1))

plot(log(d.A1$rgdppc_2000) ~ d.A1$rugged,
     col = "orange", pch =16,
     xlim = c(0, 6.5),
     ylim = c(6, 11),
     xlab = "terrain ruggedness index",
     ylab = "log(GDP)")

points(log(d.A0$rgdppc_2000) ~ d.A0$rugged,
       col = "blue", pch =16)
abline(a = coefficients(m.dummy)[1], 
       b = coefficients(m.dummy)[2],
       col="blue", lwd = 2) # not Africa
abline(a = coefficients(m.dummy)[1] +
               coefficients(m.dummy)[3],
       b = coefficients(m.dummy)[2] ,
       col="orange", lwd = 2) # Africa



# Interaction allows effect of ruggedness to depend on continent

m.interaction <- lm(GDP ~ rugged + cont_africa + cont_africa * rugged, data = d2)

summary(m.interaction)

plot(log(d.A1$rgdppc_2000) ~ d.A1$rugged,
     col = "orange", pch =16,
     xlim = c(0, 6.5),
     ylim = c(6, 11),
     xlab = "terrain ruggedness index",
     ylab = "log(GDP)")
points(log(d.A0$rgdppc_2000) ~ d.A0$rugged,
       col = "blue", pch =16)
abline(a = coefficients(m.interaction)[1], 
       b = coefficients(m.interaction)[2],
       col="blue", lwd = 2) # not Africa
abline(a = coefficients(m.interaction)[1] +
               coefficients(m.interaction)[3],
       b = coefficients(m.interaction)[2] + 
               coefficients(m.interaction)[4],
       col="orange", lwd = 2) # Africa


# End of part 1

## Data transformation----------------------------------

age <- rexp(600, rate=0.1)

hist(age, xlab= "age at death", col = "orange")
lines(c(mean(age), mean(age)), c(0, 250), col="blue", lwd = 4)

hist(age, xlab= "age at death", col = c("darkgreen", "orange", "orange", "orange", "orange", "orange", "orange", "orange", "orange", "orange", "orange", "orange", "orange"))
lines(c(mean(age), mean(age)), c(0, 250), col="blue", lwd = 2)

hist(log(age), xlab="log(age at death)", col = "orange", breaks = 13)
lines(c(mean(log(age)), mean(log(age))), c(0, 250), col="blue", lwd = 4)

male <- c(rep(0, 300), rep(1, 300))
age <- c(rexp(300, rate = 0.08), rexp(300, rate = 0.11))

boxplot(age ~ male, xlab = "male", ylab = "age at death", col = "orange")

boxplot(log(age) ~ male, xlab = "male", ylab = "log(age at death)", col = "orange")

# fit model
m <- lm(log(age) ~ male)
summary(m)

# Convert coef back to probability scale
exp(coef(m)['male'])

# Works for confidence intervals too
exp(confint(m)['male',])

(male.pred <- exp(log(40)-0.419))
male.pred/40


