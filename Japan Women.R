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