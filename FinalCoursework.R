# Set up
rm(list=ls())
setwd("~/PUBL0055")
library(plm)
library(lmtest)
library(sandwich)
library(texreg)

# Part 1

# Part 2
## Question 1
a <- read.csv("https://uclspp.github.io/datasets/data/vdem.csv")
summary(a)
names(a)

### Drop missing values
a <- a[ !is.na(a$child_mortality), ]
a <- a[ !is.na(a$inequality_gini), ]
a <- a[ !is.na(a$healthcare), ]
a <- a[ !is.na(a$political_stability), ]
a <- a[ !is.na(a$media_censorship), ]
a <- a[ !is.na(a$government_effectiveness), ]
a <- a[ !is.na(a$polity), ]
a <- a[ !is.na(a$education15), ]

### Descriptive statistics
head(a, n=10)
mean(a$child_mortality)
sd(a$child_mortality)
mean(a$inequality_gini)
sd(a$inequality_gini)
mean(a$political_stability)
sd(a$political_stability)
mean(a$healthcare)
sd(a$healthcare)
mean(a$media_censorship)
sd(a$media_censorship)
mean(a$government_effectiveness)
sd(a$government_effectiveness)
median(a$polity)
max(a$polity)-min(a$polity)
quantile(a$polity, 0.75) - quantile(a$polity, 0.25)
mean(a$education15)
sd(a$education15)

### Plots
plot(
  child_mortality ~ political_stability, 
  data = a,
  pch = 20,
  frame.plot = FALSE,
  col = "sky blue",
  main = "Relationship between child mortality and political stability",
  ylab = "Child Mortality",
  xlab = "Political Stability"
)

m1 <- lm(child_mortality ~ political_stability, data =a)
m2 <- lm(child_mortality ~ poly(political_stability, 2), data =a)
range(a$political_stability)
mock <- seq(from = -3.32, to = 1.95, length = 100)
b <- data.frame( political_stability = mock)
y_hats <- predict(m2,newdata=b)
abline(m1, col = "black")
lines(x = mock, y = y_hats, lwd=2, col = "red")

plot(
  child_mortality ~ healthcare, 
  data = a,
  pch = 20,
  frame.plot = FALSE,
  col = "sky blue",
  main = "Relationship between child mortality and healthcare",
  ylab = "Child Mortality",
  xlab = "Healthcare"
)

m1 <- lm(child_mortality ~ healthcare, data =a)
m2 <- lm(child_mortality ~ poly(healthcare, 2), data =a)
range(a$healthcare)
mock <- seq(from = -3.39, to = 4.76, length = 100)
b <- data.frame( healthcare = mock)
y_hats <- predict(m2,newdata=b)
abline(m1, col = "black")
lines(x = mock, y = y_hats, lwd =2, col = "red")

plot(
  child_mortality ~ media_censorship, 
  data = a,
  pch = 20,
  frame.plot = FALSE,
  col = "sky blue",
  main = "Relationship between child mortality and media censorship",
  ylab = "Child Mortality",
  xlab = "Media Censorship"
)

plot(
  child_mortality ~ government_effectiveness, 
  data = a,
  pch = 20,
  frame.plot = FALSE,
  col = "sky blue",
  main = "Relationship between child mortality and government effectiveness",
  ylab = "Child Mortality",
  xlab = "Government Effectiveness"
)

m1 <- lm(child_mortality ~ government_effectiveness, data =a)
m2 <- lm(child_mortality ~ poly(government_effectiveness, 2), data =a)
range(a$government_effectiveness)
mock <- seq(from = -2.78, to = 2.50, length = 100)
b <- data.frame( government_effectiveness = mock)
y_hats <- predict(m2,newdata=b)
abline(m1, col = "black")
lines(x = mock, y = y_hats, lwd =2, col = "red")

plot(
  child_mortality ~ polity, 
  data = a,
  pch = 20,
  frame.plot = FALSE,
  col = "sky blue",
  main = "Relationship between child mortality and polity",
  ylab = "Child Mortality",
  xlab = "Polity"
)

plot(
  child_mortality ~ education15, 
  data = a,
  pch = 20,
  frame.plot = FALSE,
  col = "sky blue",
  main = "Relationship between child mortality and education level",
  ylab = "Child Mortality",
  xlab = "Education Level"
)

m1 <- lm(child_mortality ~ education15, data =a)
m2 <- lm(child_mortality ~ poly(education15, 2), data =a)
range(a$education15)
mock <- seq(from = 0, to = 13.5, length = 100)
b <- data.frame( education15 = mock)
y_hats <- predict(m2,newdata=b)
abline(m1, col = "black")
lines(x = mock, y = y_hats, lwd =2, col = "red")

### Model 1 and 2
m1 <- lm(child_mortality ~ inequality_gini, data = a)
m2 <- lm(child_mortality ~ inequality_gini + healthcare + political_stability + media_censorship + polity + poly(government_effectiveness,2) + + poly(education15,2), data = a)
screenreg(list(m1,m2))

summary(m1)
summary(m2)

range(a$political_stability)

### Fitted values

#### An increase in government effectiveness from -2 to -1.5 would lead a huge decrease in child mortality (gov effectiveness <0)
m3 <- lm(child_mortality ~ poly(government_effectiveness, 2), data =a)
y_hat1 <- predict(m3, newdata = data.frame(government_effectiveness = -2))
y_hat2 <- predict(m3, newdata = data.frame(government_effectiveness = -1.5))
y_hat1 - y_hat2

#### An increase in government effectivenss from 0.5 to 1 would lead to a smaller decrease in child mortality (when gov effectiveness >0)
y_hat3 <- predict(m3, newdata = data.frame(government_effectiveness = -0.14))
y_hat4 <- predict(m3, newdata = data.frame(government_effectiveness = 0.36))
y_hat3-y_hat4

#### An increase in education level from 2 to 4
m4 <- lm(child_mortality ~ poly(education15, 2), data =a)
y_hat5 <- predict(m4, newdata = data.frame(education15 = 2))
y_hat6 <- predict(m4, newdata = data.frame(education15 = 4))
y_hat5 - y_hat6

#### An increase in education level from 8 to 10
y_hat7 <- predict(m4, newdata = data.frame(education15 = 8))
y_hat8 <- predict(m4, newdata = data.frame(education15 = 10))
y_hat7 - y_hat8

### Residuals

a$residuals <- residuals(m1)
a$residuals2 <- residuals(m2)
par(mfrow=c(1,2))

plot(
  residuals ~ inequality_gini, data = a,
  main = "Model 1 Residuals",
  xlab = "Inequality",
  ylab = "Residuals",
  pch = 20,
  frame.plot = FALSE,
  col = "LightSkyBlue"
)
abline(h = 0,lwd=3)

plot(
  residuals2 ~ inequality_gini, data = a,
  main = "Model 2 Residuals",
  xlab = "Inequality",
  ylab = "Residuals",
  pch = 20,
  frame.plot = FALSE,
  col = "LightSkyBlue"
)
abline(h = 0,lwd=3)

#### Model Fit (Heteroskedasticity)
bptest(m1)
bptest(m2)
m3 <- coeftest(m1,vcov = vcovHC(m1))
m4 <- coeftest(m2,vcov = vcovHC(m2))
screenreg(list(m1,m3,m2,m4), custom.model.names = c("Model 1", "Mode 1 (robust)", "Model2", "Model 2 (robust)"))

## Question 2

### Unit fixed effects
m1 <- plm(
  child_mortality ~ inequality_gini + healthcare + political_stability + media_censorship + polity + government_effectiveness + education15,
  data = a,
  index = c("country_name", "year"),
  effect = "individual"
)
summary(m1)

### Check for unit fixed effects
plmtest(m1)

### Time fixed effects
m2 <- plm(
  child_mortality ~ inequality_gini + healthcare + political_stability + media_censorship + polity + government_effectiveness + education15,
  data = a,
  index = c("country_name", "year"),
  effect = "time"
)
summary(m2)

### check for the presence of time fixed effects
plmtest(m2, effect="time")

### Two way effects model
m3 <- plm(
  child_mortality ~ inequality_gini + healthcare + political_stability + media_censorship + polity + government_effectiveness + education15,
  data = a,
  index = c("country_name", "year"),
  effect = "twoways"
)
summary(m3)

#### screenreg
screenreg(
  list(m1, m2, m3), 
  custom.model.names = c("Country Fixed Effects", "Time Fixed Effects", "Twoway Fixed Effects")
)

### Test for serial correlation
pbgtest(m3)

#### Correct standard errors for heteroskedasticity and autocorrelation
m3.robust <- coeftest(
  m3,
  vcov = vcovHC(m3, method = "arellano"), type = "HC3"
)

### Test for spatial dependence
pcdtest(m3)

#### Adjust standard errors for autocorrelation and heteroskedasticity and spatial dependence
m3.robust2 <- coeftest(
  m3,
  vcov = vcovSCC(m3, type = "HC3", cluster = "group")
)

### Final correct model
screenreg(m3.robust2, 
          custom.model.names = c("Twoway Fixed Effects (HAC)")
)

# Part 3
b <- read.csv("https://uclspp.github.io/datasets/data/ess.csv")
summary(b)
names(b)

## Question 1

### Logistic regression model
m3 <- glm(
  leave ~ eu_integration + years_education + news_consumption + country_attach + trust_politicians,
  data = b,
  family = binomial(link ="logit")
)
screenreg(m3)

### Predicted probabilities

#### Predict eu_integration
summary(b$eu_integration)
eu_integration_4 <- predict(
  m3, 
  newdata = data.frame(
    eu_integration = 4,
    years_education = mean(b$years_education),
    news_consumption = mean(b$news_consumption),
    country_attach = median(b$country_attach),
    trust_politicians = median(b$trust_politicians)),
  type = "response"
)
eu_integration_4
eu_integration_5 <- predict(
  m3, 
  newdata = data.frame(
    eu_integration = 5,
    years_education = mean(b$years_education),
    news_consumption = mean(b$news_consumption),
    country_attach = median(b$country_attach),
    trust_politicians = median(b$trust_politicians)),
  type = "response"
)
eu_integration_5
eu_integration_4 - eu_integration_5

#### Predict years_education
summary(b$years_education)
years.seq <- seq(from = 0, to = 54, length.out = 100)
x <- data.frame(
  eu_integration = median(b$eu_integration),
  years_education = years.seq,
  news_consumption = mean(b$news_consumption),
  country_attach = median(b$country_attach),
  trust_politicians = median(b$trust_politicians)
)
x$leave_pp <- predict (m3, newdata =x, type = "response")
par(mfrow=c(1,1))
plot(
  leave_pp ~ years_education,
  data = x,
  col = "skyblue",
  lwd =3,
  type ="l",
  ylim = c(0,1),
  main = "Effects of Education on Leave-Vote",
  xlab = "Years of Education",
  ylab = "Predicted probability of Leave-Vote"
)
range(x$leave_pp)

years_education_0 <- predict(
  m3, 
  newdata = data.frame(
    eu_integration = median(b$eu_integration),
    years_education = 9,
    news_consumption = mean(b$news_consumption),
    country_attach = median(b$country_attach),
    trust_politicians = median(b$trust_politicians)),
  type = "response"
)
years_education_1 <- predict(
  m3, 
  newdata = data.frame(
    eu_integration = median(b$eu_integration),
    years_education = 10,
    news_consumption = mean(b$news_consumption),
    country_attach = median(b$country_attach),
    trust_politicians = median(b$trust_politicians)),
  type = "response"
)
years_education_0 - years_education_1

#### Predict country_attached
country_attached_0 <- predict(
  m3, 
  newdata = data.frame(
    eu_integration = median(b$eu_integration),
    years_education = mean(b$years_education),
    news_consumption = mean(b$news_consumption),
    country_attach = 0,
    trust_politicians = median(b$trust_politicians)),
  type = "response"
)
country_attached_0
country_attached_1 <- predict(
  m3, 
  newdata = data.frame(
    eu_integration = median(b$eu_integration),
    years_education = mean(b$years_education),
    news_consumption = mean(b$news_consumption),
    country_attach = 1,
    trust_politicians = median(b$trust_politicians)),
  type = "response"
)
country_attached_0 - country_attached_1

#### Predict trust_politicians
trust_politicians_3 <- predict(
  m3, 
  newdata = data.frame(
    eu_integration = median(b$eu_integration),
    years_education = mean(b$years_education),
    news_consumption = mean(b$news_consumption),
    country_attach = median(b$country_attach),
    trust_politicians = 3),
  type = "response"
)
trust_politicians_3
trust_politicians_4 <- predict(
  m3, 
  newdata = data.frame(
    eu_integration = median(b$eu_integration),
    years_education = mean(b$years_education),
    news_consumption = mean(b$news_consumption),
    country_attach = median(b$country_attach),
    trust_politicians = 4),
  type = "response"
)
trust_politicians_4
trust_politicians_3 - trust_politicians_4

### Model Fit for model 1
pp <- predict(m3, newdata = b, type = "response")
evs <- ifelse(pp > 0.5, yes = 1, no = 0)
confusion_logit1 <- table(actual = b$leave, expected.value = evs)
confusion_logit1
sum(diag(confusion_logit1)) / sum(confusion_logit1)
mean(b$leave)


## Question 2
m4 <- glm(
  leave ~ eu_integration + years_education + news_consumption + country_attach + trust_politicians + immig_econ + immig_culture,
  data = b,
  family = binomial(link ="logit")
)

### Screen reg
screenreg(list(m3,m4))

### Fit statistics for model 2
pp <- predict(m4, newdata = b, type = "response")
evs <- ifelse(pp > 0.5, yes = 1, no = 0)
confusion_logit2 <- table(actual = b$leave, expected.value = evs)
confusion_logit2
sum(diag(confusion_logit2)) / sum(confusion_logit2)

### Predictive probabilities

#### Predict immig_econ 
head(b$immig_econ, n=10)
summary(b$immig_econ)
econ.seq <- seq(from = 0, to = 10, length.out = 100)
x <- data.frame(
  eu_integration = median(b$eu_integration),
  years_education = mean(b$years_education),
  news_consumption = mean(b$news_consumption),
  country_attach = median(b$country_attach),
  trust_politicians = median(b$trust_politicians),
  immig_econ = econ.seq,
  immig_culture = median(b$immig_culture)
)
x$leave_pp <- predict (m4, newdata =x, type = "response")
range(x$leave_pp)
18.394071-9.069996

#### Predict immig_culture
head(b$immig_culture, n=10)
summary(b$immig_culture)
culture.seq <- seq(from = 0, to = 10, length.out = 100)
y <- data.frame(
  eu_integration = median(b$eu_integration),
  years_education = mean(b$years_education),
  news_consumption = mean(b$news_consumption),
  country_attach = median(b$country_attach),
  trust_politicians = median(b$trust_politicians),
  immig_econ = median(b$immig_econ),
  immig_culture = culture.seq
)
y$leave_pp <- predict (m4, newdata =y, type = "response")
range(y$leave_pp)
18.28633 - 10.29888

#### Plot to compare econ concerns and cultural concerns about immigration
plot(
  leave_pp ~ immig_econ,
  data = x,
  col = "skyblue",
  lwd =3,
  type ="l",
  ylim = c(0,1),
  main = "Effects of Economic concerns and Cultural concerns on Leave-Vote",
  xlab = "Views on immigration",
  ylab = "Predicted probability of Leave-Vote"
)
lines(leave_pp ~ immig_culture, data = y, col = "red")


immig_econ_2 <- predict(
  m3, 
  newdata = data.frame(
    eu_integration = median(b$eu_integration),
    years_education = mean(b$years_education),
    news_consumption = mean(b$news_consumption),
    country_attach = median(b$country_attach),
    trust_politicians = median(b$trust_politicians),
    immig_econ = 2,
    immig_culture = median(b$immig_culture)
    ),
  type = "response"
)
immig_econ_2
immig_econ_3 <- predict(
  m3, 
  newdata = data.frame(
    eu_integration = median(b$eu_integration),
    years_education = mean(b$years_education),
    news_consumption = mean(b$news_consumption),
    country_attach = median(b$country_attach),
    trust_politicians = median(b$trust_politicians),
    immig_econ = 3,
    immig_culture = median(b$immig_culture)
  ),
  type = "response"
)
immig_econ_3
immig_econ_2 -immig_econ_3
