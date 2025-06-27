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

# Check if the packages are downloaded, and if not, then download them.

packages <- c("foreign", "dplyr", "car", "nnet", "summarytools", "stargazer")
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) install.packages(packages[!installed])

# Load the packages.
lapply(packages, library, character.only = TRUE)

# Read the file and choose only Japan as variable.

dat <- read.dta(file.choose(), convert.factors=FALSE)
jpn<-dat%>%filter(B_COUNTRY==392)

# Let's disable package "dplyr" because otherwise the recode function from "car" package doesn't work for some reason.

detach("package:dplyr")

#################### Variables ####################

# Dependent variable(s)


# Binary

# Q111 = Protecting environment vs. economic growth
# Here are two statements people sometimes make when discussing the environment and economic growth. Which of them comes closer to your own point of view?
# Protecting the environment should be given priority, even if it causes slower economic growth and some loss of jobs. 2. Economic growth and creating jobs should be the top priority, even if the environment suffers to some extent. 3. Other answer.

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

# Q262 = Age

table(jpn$Q262)
jpn$age <- recode(jpn$Q262, "-5=NA; -2=NA; -1=NA")
table(jpn$age)

# Q260 = Gender
# 1. Male 2. Female

table(jpn$Q260)
jpn$gender <- recode(jpn$Q260, "1=0; 2=1; else=NA")
table(jpn$gender)

# Q275 = Education
# What is the highest educational level that you, your spouse, your mother and your father have attained?
# 0. No education 1. Primary education 2. Lower secondary education 3. Upper secondary education 4. Post-secondary non-tertiary education 5. Short-cycle tertiary education 6. Bachelor or equivalent 7. Master or equivalent 8. Doctoral or equivalent 

table(jpn$Q275)
jpn$educ <- recode(jpn$Q275, "-5=NA; -2=NA; -1=NA; 0=0; 1:3=1; 4:5=2; 6:8=3")
table(jpn$educ)

# Q288 = Income
# On this card is an income scale on which 1 indicates the lowest income group and 10 the highest income group in your country. We would like to know in what group your household is. Please, specify the appropriate number, counting all wages, salaries, pensions and other incomes that come in
# Lowest group 1 ... 10 Highest group

table(jpn$Q288)
jpn$income <- recode(jpn$Q288, "-5=NA; -2=NA; -1=NA")
table(jpn$income)

# Q199 = Interest of Politics
# How interested would you say you are in politics? Are you...
# 1. Very interested 2.Somewhat interested 3. Not very interested 4. Not at all interested

table(jpn$Q199)
jpn$polint <- recode(jpn$Q199, "-5=NA; -2=NA; -1=NA; 1:2=1; 3:4=0")
table(jpn$polint)

# Q240 = Ideology
# In political matters, people talk of "the left" and "the right." How would you place your views on this scale,generally speaking?
# Left 1 ... 10 Right

table(jpn$Q240)
jpn$ideology <- recode(jpn$Q240, "-5=NA; -4=NA; -2=NA; -1=NA")
table(jpn$ideology)

# rural vs suburban
# 1 = Urban (city, town), 2 = rural (village)

table(jpn$H_URBRURAL)
jpn$urbrural <- recode(jpn$H_URBRURAL, "-5=NA; 2=0; 1=1")
table(jpn$urbrural)

# Q50 = Economic satisfaction -> this? Prospective economic evaluations
# Completely dissatisfied 1 ... 10 Completely satisfied

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