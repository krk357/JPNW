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

install.packages("foreign", dependencies=TRUE)
install.packages("dplyr")
install.packages("car")
install.packages("nnet")
install.packages("summarytools")
install.packages("stargazer")

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
