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

setwd('D:\\code book\\SF and cognitive funtion\\ELSA')

df <- read.csv('elsa_cleaning.csv')

table(df$race)
# only 1 and 4 1.White, 4.non-white
df$race <- cut(df$race, breaks=c(-Inf,2,Inf),labels=c("White","non-White"))
table(df$race)


#frailty_status 0, 1, 2-5
df$frailty_status <- ifelse(df$frailty_score == 0, "non-frailty",
                            ifelse(df$frailty_score == 1, "pre-frailty",
                                   "frailty"))


vars_to_factor <- c(  "gender","race",
                      "finace_satified1",  "people_living_with", "week_contact", "soc_activity", "loneliness","frailty_status2",
                      "n_chronic_category",
                      "drink_now","smoke_now","any_vigoral_activities", "H_edu")
for (var in vars_to_factor) {
  df[[var]] <- as.factor(df[[var]])
}

df$frailty_status2 <- factor(df$frailty_status2, levels = c("non-frailty", 
                                                            "pre-frailty", 
                                                            "frailty"))
df$frailty_status <- factor(df$frailty_status, levels = c("non-frailty", 
                                                            "pre-frailty", 
                                                            "frailty"))

table(df$frailty_status)
####TABLE####
#overall


t1 <- CreateTableOne(vars = c("age.y", "gender", "race",
                              "finace_satified1",  "people_living_with", "week_contact", "soc_activity", "loneliness",
                              "frailty_status2","frailty_score",
                              "n_chronic_category",
                              "drink_now","smoke_now","any_vigoral_activities", "H_edu",
                              "fgcp", "fexf", "flang", "fmem", "forient"
), 
data = df)

html_output1 <- print(t1, showAllLevels = TRUE, catDigits = 2, printToggle = FALSE, html = TRUE)
print(t1, showAllLevels = TRUE, catDigits=2, printToggle = FALSE) %>% 
  knitr::kable(caption = "Descriptive charateristics",
               booktabs = T, linesep = '') %>% 
  kable_styling(latex_options = "hold_position")

#stratified by gender
t2 <- CreateTableOne(vars = c("age.y",  "race",
                              "finace_satified1",  "people_living_with", "week_contact", "soc_activity", "loneliness",
                              "frailty_status2","frailty_score",
                              "n_chronic_category",
                              "drink_now","smoke_now","any_vigoral_activities", "H_edu",
                              "fgcp", "fexf", "flang", "fmem", "forient"), 
                     data = df, 
                     strata = "gender")

html_output2 <- print(t2, showAllLevels = TRUE, catDigits = 2, printToggle = FALSE, html = TRUE)
print(t2, showAllLevels = TRUE, catDigits=2, printToggle = FALSE) %>% 
  knitr::kable(caption = "Descriptive charateristics by gender",
               booktabs = T, linesep = '') %>% 
  kable_styling(latex_options = "hold_position")

####lm model####
#covariates
model1_fgcp <- lm(fgcp ~ frailty_status2 + age.y + gender + race + H_edu+  n_chronic_category + drink_now + smoke_now+ any_vigoral_activities, data = df)
summary(model1_fgcp)

coef_table <- summary(model1_fgcp)$coefficients
conf_int <- confint(model1_fgcp)

model1_fexf <- lm(fexf ~ frailty_status2 + age.y + gender + race + H_edu +  n_chronic_category + drink_now + smoke_now+ any_vigoral_activities, data = df)
model1_flang <- lm(flang ~ frailty_status2 + age.y + gender + race + H_edu +  n_chronic_category + drink_now + smoke_now+ any_vigoral_activities, data = df)
model1_forient <- lm(forient ~ frailty_status2 + age.y + gender + race + H_edu +  n_chronic_category + drink_now + smoke_now+ any_vigoral_activities, data = df)
model1_fmem <- lm(fmem ~ frailty_status2 + age.y + gender + race + H_edu +  n_chronic_category + drink_now + smoke_now+ any_vigoral_activities, data = df)
tab_model(model1_fgcp, model1_fexf, model1_flang, model1_forient, model1_fmem, show.se = T, show.ci = F)

#gender interactive
model2_fgcp <- lm(fgcp ~ frailty_status2 * gender + age.y + race + H_edu +  n_chronic_category + drink_now + smoke_now+ any_vigoral_activities, data = df)
summary(model2_fgcp)

coef_table <- summary(model1_fgcp)$coefficients
conf_int <- confint(model1_fgcp)

model2_fexf <- lm(fexf ~ frailty_status2 * gender + age.y + race + H_edu +  n_chronic_category + drink_now + smoke_now+ any_vigoral_activities, data = df)
model2_flang <- lm(flang ~ frailty_status2 * gender + age.y + race + H_edu +  n_chronic_category + drink_now + smoke_now+ any_vigoral_activities, data = df)
model2_forient <- lm(forient ~ frailty_status2 * gender + age.y + race + H_edu +  n_chronic_category + drink_now + smoke_now+ any_vigoral_activities, data = df)
model2_fmem <- lm(fmem ~ frailty_status2 * gender + age.y + race + H_edu +  n_chronic_category + drink_now + smoke_now+ any_vigoral_activities, data = df)
tab_model(model2_fgcp, model2_fexf, model2_flang, model2_forient, model2_fmem, show.se = T, show.ci = F)


####sensitivity analysis####
####sampling weight####
summary(df$r8wtrespb)
df$WEIGHT <- df$r8wtrespb /sum(df[, 'r8wtrespb'])*772
model3_fgcp <- lm(fgcp ~ frailty_status2 + age.y + gender + race + H_edu+  n_chronic_category + drink_now + smoke_now+ any_vigoral_activities, data = df, weights = WEIGHT)
summary(model3_fgcp)

coef_table <- summary(model3_fgcp)$coefficients
conf_int <- confint(model3_fgcp)

model3_fexf <- lm(fexf ~ frailty_status2 + age.y + gender + race + H_edu +  n_chronic_category + drink_now + smoke_now+ any_vigoral_activities, data = df, weights = WEIGHT)
model3_flang <- lm(flang ~ frailty_status2 + age.y + gender + race + H_edu +  n_chronic_category + drink_now + smoke_now+ any_vigoral_activities, data = df, weights = WEIGHT)
model3_forient <- lm(forient ~ frailty_status2 + age.y + gender + race + H_edu +  n_chronic_category + drink_now + smoke_now+ any_vigoral_activities, data = df, weights = WEIGHT)
model3_fmem <- lm(fmem ~ frailty_status2 + age.y + gender + race + H_edu +  n_chronic_category + drink_now + smoke_now+ any_vigoral_activities, data = df, weights = WEIGHT)
tab_model(model3_fgcp, model3_fexf, model3_flang, model3_forient, model3_fmem, show.se = T, show.ci = F)


####social fraiity 0;1;2-5####
model4_fgcp <- lm(fgcp ~ frailty_status + age.y + gender + race + H_edu+  n_chronic_category + drink_now + smoke_now+ any_vigoral_activities, data = df)
summary(model4_fgcp)

coef_table <- summary(model4_fgcp)$coefficients
conf_int <- confint(model4_fgcp)

model4_fexf <- lm(fexf ~ frailty_status + age.y + gender + race + H_edu +  n_chronic_category + drink_now + smoke_now+ any_vigoral_activities, data = df)
model4_flang <- lm(flang ~ frailty_status + age.y + gender + race + H_edu +  n_chronic_category + drink_now + smoke_now+ any_vigoral_activities, data = df)
model4_forient <- lm(forient ~ frailty_status + age.y + gender + race + H_edu +  n_chronic_category + drink_now + smoke_now+ any_vigoral_activities, data = df)
model4_fmem <- lm(fmem ~ frailty_status + age.y + gender + race + H_edu +  n_chronic_category + drink_now + smoke_now+ any_vigoral_activities, data = df)
tab_model(model4_fgcp, model4_fexf, model4_flang, model4_forient, model4_fmem, show.se = T, show.ci = F)

####gender group####
table(df$gender)
table(df$frailty_status2,df$gender)
#man
model5_fgcp_men <- lm(fgcp ~ frailty_status2 + age.y  + race + H_edu+  n_chronic_category + drink_now + smoke_now+ any_vigoral_activities, data = df[df$gender == "men",])

model5_fexf_men <- lm(fexf ~ frailty_status2 + age.y  + race + H_edu +  n_chronic_category + drink_now + smoke_now+ any_vigoral_activities, data = df[df$gender == "men",])
model5_flang_men <- lm(flang ~ frailty_status2 + age.y  + race + H_edu +  n_chronic_category + drink_now + smoke_now+ any_vigoral_activities, data = df[df$gender == "men",])
model5_forient_men <- lm(forient ~ frailty_status2 + age.y  + race + H_edu +  n_chronic_category + drink_now + smoke_now+ any_vigoral_activities, data = df[df$gender == "men",])
model5_fmem_men <- lm(fmem ~ frailty_status2 + age.y  + race + H_edu +  n_chronic_category + drink_now + smoke_now+ any_vigoral_activities, data = df[df$gender == "men",])
tab_model(model5_fgcp_men,model5_fexf_men,model5_flang_men,model5_forient_men,model5_fmem_men,show.se = T)

#women
model5_fgcp_women <- lm(fgcp ~ frailty_status2 + age.y  + race + H_edu+  n_chronic_category + drink_now + smoke_now+ any_vigoral_activities, data = df[df$gender == "women",])

model5_fexf_women <- lm(fexf ~ frailty_status2 + age.y  + race + H_edu +  n_chronic_category + drink_now + smoke_now+ any_vigoral_activities, data = df[df$gender == "women",])
model5_flang_women <- lm(flang ~ frailty_status2 + age.y  + race + H_edu +  n_chronic_category + drink_now + smoke_now+ any_vigoral_activities, data = df[df$gender == "women",])
model5_forient_women <- lm(forient ~ frailty_status2 + age.y  + race + H_edu +  n_chronic_category + drink_now + smoke_now+ any_vigoral_activities, data = df[df$gender == "women",])
model5_fmem_women <- lm(fmem ~ frailty_status2 + age.y  + race + H_edu +  n_chronic_category + drink_now + smoke_now+ any_vigoral_activities, data = df[df$gender == "women",])
tab_model(model5_fgcp_women,model5_fexf_women,model5_flang_women,model5_forient_women,model5_fmem_women,show.se = T)
