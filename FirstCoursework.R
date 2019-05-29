# Part 1
rm(list=ls())
setwd("~/PUBL0055")

## 1a
d <- c(170, 448, 369, 182, 394, 206, 258, 433, 503, 426, 409, 421, 355, 516, 226, 535, 489, 337, 464, 508, 325, 521, 533, 533, 237, 476, 312, 493, 411, 464, 192, 689, 398)
r <- c(377, 373, 284, 249, 402, 336, 377, 263, 352, 341, 290, 365, 416, 299, 334, 353, 363, 346, 285, 371, 387, 333, 431, 349, 307, 294, 374, 254, 366, 416, 274, 343, 415)
mean(d)
s1 <- sd(d)
mean(r)
s2 <- sd(r)

## 1b
n1 <- 33
n2 <- 33
se <- sqrt((125.18^2/n1)+(49.70^2/n2))
se

# Part 2
pmdata <- read.csv("https://uclspp.github.io/datasets/data/pmgb2012_2014.csv")

## 2a 

### central tendency and dispersion of edu_level3
mean(pmdata$edu_level3)
sd(pmdata$edu_level3)

### central tendency and dispersion of edu_level4
mean(pmdata$edu_level4)
sd(pmdata$edu_level4)

### central tendecy and dispersion of pop_density
table(pmdata$pop_density)
sd(pmdata$pop_density, na.rm =TRUE)

## 2b

### scatter plot
plot(
  x = pmdata$edu_level4,
  y = pmdata$pmdeaths_total,
  pch = 20,
  main = "Relationship of premature mortality and level of education in Great Britain",
  xlab = "Degree-level education",
  ylab = "Premature mortality"
)

### box plot
pmdata$country <- factor(pmdata$country, labels = c("England", "Scotland", "Wales"))

boxplot(
  pmdata$pmdeaths_total ~ pmdata$country, 
  main = "Premature mortality across England, Scotland and Wales",
  ylab = "Premature mortality", 
  frame.plot = FALSE, col = "darkgray"
)

## 2c

### first difference
fd <- mean(pmdata$pmdeaths_male) - mean(pmdata$pmdeaths_female)
fd

### t-test 
t.test(pmdata$pmdeaths_male, pmdata$pmdeaths_female, mu = 0, alt = "two.sided")

## 2d
plot(
  x = pmdata$mean_income,
  y = pmdata$pmdeaths_total
)
m2 <- lm(pmdeaths_total ~ mean_income, data = pmdata )
summary(m2)
library(texreg)
screenreg(m2)

# Part 3
## 3a

### t value of salary in model 1 (significant)
-0.56/0.10
### t value of salary in model 2 (significant)
-0.61/0.13
### t value of term_limits (not significant)
0.26/0.84
### t value of income (not significant)
-0.03/0.05
### t value of income_inequality (significant)
-0.26/0.11
### t value of poverty_rate (not significant)
-0.05/0.07
### t value of pct_union (significant)
0.12/0.04
### t value of black (significant)
-0.06/0.04
### t value of pct_urban (not significant)
-0.03/0.02
