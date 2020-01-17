library(ggplot2)
library(shiny)
library(plotly)
library(stargazer)
library(compare)
library(survival)
library(shinythemes)

##################################################################
#### You may need to edit the following lines
#### if data or the model are not defined correctly
##################################################################

data <- readRDS('/Users/carolinaferreiraatuesta/R/dataset.rds')
model <- coxph(Surv(time_sz, sz) ~ auras + time_begin + drugs + gtcs, data = data)
modellabels <- c("SPSs before beginning AED withdrawal (Yes=1, No=0)", "Time to begin AED whithdrawal (Years)", "Number of AEDs at time of surgery", "GTCSs anytime before surgery (Yes=1, No=0)")

covariate <- 'numeric'