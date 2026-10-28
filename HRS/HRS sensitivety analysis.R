library(ggplot2) 
library(dplyr) 
library(ggalluvial)
library(ggsci)
library(tableone)
library(kableExtra)
library(sjPlot)
library(geepack)
library(tidyr)
library(DT)

setwd('D:\\code book\\SF and cognitive funtion\\HRS')

df <- read.csv('hrs_cleaning.csv')
###frailty_status 0, 1, 2-5
df$frailty_status <- ifelse(df$frailty_score == 0, "non-frailty",
                            ifelse(df$frailty_score == 1, "pre-frailty",
                                   "frailty"))

df$frailty_status <- factor(df$frailty_status, levels = c("non-frailty", 
                                                            "pre-frailty", 
                                                            "frailty"))
table(df$frailty_status)
####lm model####
#covariates
model4_fgcp <- lm(fgcp ~ frailty_status + age + gender + race + H_edu+ urbanicity +  n_chronic_category + drink_now + smoke_now+ any_vigoral_activities, data = df)
summary(model4_fgcp)

coef_table <- summary(model4_fgcp)$coefficients
conf_int <- confint(model4_fgcp)

model4_fexf <- lm(fexf ~ frailty_status + age + gender + race + H_edu+ urbanicity +  n_chronic_category + drink_now + smoke_now+ any_vigoral_activities, data = df)
model4_flang <- lm(flang ~ frailty_status + age + gender + race + H_edu+ urbanicity +  n_chronic_category + drink_now + smoke_now+ any_vigoral_activities, data = df)
model4_forient <- lm(forient ~ frailty_status + age + gender + race + H_edu+ urbanicity +  n_chronic_category + drink_now + smoke_now+ any_vigoral_activities, data = df)
model4_fmem <- lm(fmem ~ frailty_status + age + gender + race + H_edu+ urbanicity +  n_chronic_category + drink_now + smoke_now+ any_vigoral_activities, data = df)
tab_model(model4_fgcp, model4_fexf, model4_flang, model4_forient, model4_fmem, show.se = T, show.ci = F)

