# World Values Survey Wave 7 2017-2022
#
# Dependent variable (coded two different ways - two different models)
# Q111 = Protecting environment vs. economic growth
# Country code for Japan = 392
#
# Dependent variable:
# Which of following should be given priority: Protection of environment vs. Protection of economic growth?
#
# Independent variables:
# Age, gender, education, income, ideology, interest of politics, [rural vs suburban], prospective economic evaluations
#
# Research questions: 
#  1.) What predicts the view that we should protect the environment over promotion economic growth in Japan?
#  2.) How does inclusion of “don’t know” responses help us better who is not being represented on surveys about climate change? 
#

if (!require(foreign)) install.packages("foreign", dependencies = TRUE)
if (!require(dplyr)) install.packages("dplyr", dependencies = TRUE)
if (!require(car)) install.packages("car", dependencies = TRUE)
if (!require(nnet)) install.packages("nnet", dependencies = TRUE)
if (!require(summarytools)) install.packages("summarytools", dependencies = TRUE)
if (!require(stargazer)) install.packages("stargazer", dependencies = TRUE)

library(foreign)
library(dplyr)
library(car)
library(nnet)
library(summarytools)
library(stargazer)

# Read the file and choose only Japan as variable.

dat <- read.dta(file.choose(), convert.factors=FALSE)
jpn<-dat%>%filter(B_COUNTRY==392)

# Let's disable package "dplyr" because otherwise the recode function from "car" package doesn't work for some reason.

detach("package:dplyr")

#################### Variables ####################

# Dependent variable(s)
# Q111 = Protecting environment vs. economic growth

# Binary

table(jpn$Q111)
jpn$dunnos <- recode(jpn$Q111, "1=0; 2=0; 3=0; else=1")
table(jpn$dunnos)

jpn$pronatu <- recode(jpn$Q111, "1=1; 2:3=0; else=0")
table(jpn$pronatu)

jpn$proecon <- recode(jpn$Q111, "2=1; 1=0; 3=0; else=0")
table(jpn$proecon)

# Preference variable for multimod

jpn$preference <- recode(jpn$Q111, "1 = 'Protect the Economy'; 2 = 'Protect the Nature'; 3 = 'Other'; else = 'Dont know'")
jpn$pref <- factor(jpn$preference, levels=c("Dont know", "Protect the Economy", "Protect the Nature"))

table(jpn$pref)

# Independent Variables

# Age

table(jpn$Q262)
jpn$age <- recode(jpn$Q262, "-5=NA; -2=NA; -1=NA")
table(jpn$age)

# Gender

table(jpn$Q260)
jpn$gender <- recode(jpn$Q260, "1=0; 2=1; else=NA")
table(jpn$gender)

# Education

table(jpn$Q275)
jpn$educ <- recode(jpn$Q275, "-5=NA; -2=NA; -1=NA; 0=0; 1:3=1; 4:5=2; 6:7=3; 8=4")
table(jpn$educ)

# Income

table(jpn$Q288)
jpn$income <- recode(jpn$Q288, "-5=NA; -2=NA; -1=NA")
table(jpn$income)

# Interest of Politics

table(jpn$Q199)
jpn$polint <- recode(jpn$Q199, "-5=NA; -2=NA; -1=NA; 1:2=1; 3:4=0")
table(jpn$polint)

# Ideology

table(jpn$Q240)
jpn$ideology <- recode(jpn$Q240, "-5=NA; -4=NA; -2=NA; -1=NA")
table(jpn$ideology)

# rural vs suburban

table(jpn$H_URBRURAL)
jpn$urbrural <- recode(jpn$H_URBRURAL, "-5=NA; 2=0; 1=1")
table(jpn$urbrural)

# Economic satisfaction -> this? Prospective economic evaluations

table(jpn$Q50)
jpn$econsat <- recode(jpn$Q50, "-5=NA; -2=NA; -1=NA")
table(jpn$econsat)

####### Means and standard deviations #############

mat1 <- data.frame(
  dunno = jpn$dunnos,
  natu = jpn$pronatu,
  econ = jpn$proecon
)
cor(na.omit(mat1))

descr(jpn$dunnos)
descr(jpn$pronatu)
descr(jpn$proecon)

########## Generalized Linear Model ###############

dunnos.mod <- glm(dunnos ~ age  + gender + educ + income + polint + ideology + urbrural + econsat, family = binomial, data = jpn)
summary(dunnos.mod)

pronatu.mod <- glm(pronatu ~ age + gender + educ + income + polint + ideology  + urbrural + econsat, family = binomial, data = jpn)
summary(pronatu.mod)

proecon.mod <- glm(proecon ~ age + gender + educ + income + polint + ideology  + urbrural + econsat, family = binomial, data = jpn)
summary(proecon.mod)

####### Multinomial Logistic Regression ###########

multi.mod1 <- multinom(pref ~ age  + gender + educ + income + polint + ideology + urbrural + econsat, family = binomial, data = jpn)
summary(multi.mod1)

####### Stargazer Charts for the Models ###########

stargazer(dunnos.mod, type = "html", dep.var.labels = c("Answered dont know"), covariate.labels = c("Age", "Gender", "Education", "Income", "Interest in Politics", "Ideology", "Rural / Urban", "Economic satistfaction"), no.space = TRUE, star.cutoffs = c(0.05, 0.01, 0.001), out = "dunno_res")

stargazer(pronatu.mod, type = "html", dep.var.labels = c("Protect the Environment"), covariate.labels = c("Age", "Gender", "Education", "Income", "Interest in Politics", "Ideology", "Rural / Urban", "Economic satistfaction"), no.space = TRUE, star.cutoffs = c(0.05, 0.01, 0.001), out = "pronatu_res")

stargazer(proecon.mod, type = "html", dep.var.labels = c("Protect the Economy"), covariate.labels = c("Age", "Gender", "Education", "Income", "Interest in Politics", "Ideology", "Rural / Urban", "Economic satistfaction"), no.space = TRUE, star.cutoffs = c(0.05, 0.01, 0.001), out = "proecon_res")

stargazer(multi.mod1, type = "html", dep.var.labels = c("Protecting the environment", "Economic growth"), covariate.labels = c("Age", "Gender", "Education", "Income", "Interest in Politics", "Ideology", "Rural / Urban", "Economic satistfaction"), no.space = TRUE, star.cutoffs = c(0.05, 0.01, 0.001), out = "multimod_res")