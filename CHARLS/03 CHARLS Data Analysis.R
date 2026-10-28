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

setwd('D:\\code book\\SF and cognitive funtion\\CHARLS')
df <- read.csv("charls_cleaning.csv")


vars_to_factor <- c( "sex","hukou",
                     "wealth_quartile",  "people_living_with", "week_contact", "soc_activity", "loneliness",
                     "urbanicity", 
                     "lb_status", "marital_status","n_chronic_category","vigor_anyact",
                     "drink_now","smoke_now","num_smoke","education_level")
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
t1 <- CreateTableOne(vars = c("age", "sex","hukou","urbanicity", "education_level", 
                              "wealth_quartile", "people_living_with", "week_contact", "soc_activity", "loneliness","frailty_status2","frailty_score",
                              "marital_status","n_chronic_category","drink_now","smoke_now",
                              "vigor_anyact",
                              "fgcp", "fexf", "flang", "fmem", "forient"
), 
data = df)

html_output1 <- print(t1, showAllLevels = TRUE, catDigits = 2, printToggle = FALSE, html = TRUE)
print(t1, showAllLevels = TRUE, catDigits=2, printToggle = FALSE) %>% 
  knitr::kable(caption = "Descriptive charateristics",
               booktabs = T, linesep = '') %>% 
  kable_styling(latex_options = "hold_position")

#stratified by sex
t2 <- CreateTableOne(vars = c("age", "hukou","urbanicity", "education_level", 
                              "wealth_quartile", "people_living_with", "week_contact", "soc_activity", "loneliness","frailty_status2","frailty_score",
                              "marital_status","n_chronic_category","drink_now","smoke_now",
                              "vigor_anyact","moder_act",
                              "fgcp", "fexf", "flang", "fmem", "forient"), 
                     data = df, 
                     strata = "sex")

html_output2 <- print(t2, showAllLevels = TRUE, catDigits = 2, printToggle = FALSE, html = TRUE)
print(t2, showAllLevels = TRUE, catDigits=2, printToggle = FALSE) %>% 
  knitr::kable(caption = "Descriptive charateristics by sex",
               booktabs = T, linesep = '') %>% 
  kable_styling(latex_options = "hold_position")

####lm model####
#covariates
model1_fgcp <- lm(fgcp ~ frailty_status2 + age + sex + hukou + urbanicity + n_chronic_category + drink_now + smoke_now+ vigor_anyact , data = df)
summary(model1_fgcp)

coef_table <- summary(model1_fgcp)$coefficients
conf_int <- confint(model1_fgcp)

model1_fexf <- lm(fexf ~ frailty_status2 + age + sex + hukou + urbanicity + n_chronic_category + drink_now + smoke_now+ vigor_anyact , data = df)
model1_flang <- lm(flang ~ frailty_status2 + age + sex + hukou + urbanicity + n_chronic_category + drink_now + smoke_now+ vigor_anyact , data = df)
model1_forient <- lm(forient ~ frailty_status2 + age + sex + hukou + urbanicity + n_chronic_category + drink_now + smoke_now+ vigor_anyact , data = df)
model1_fmem <- lm(fmem ~ frailty_status2 + age + sex + hukou + urbanicity + n_chronic_category + drink_now + smoke_now+ vigor_anyact , data = df)
tab_model(model1_fgcp, model1_fexf, model1_flang, model1_forient, model1_fmem, show.se = T, show.ci = F)

#gender interactive
model2_fgcp <- lm(fgcp ~ frailty_status2 * sex + age + hukou + urbanicity + n_chronic_category + drink_now + smoke_now+ vigor_anyact , data = df)
summary(model2_fgcp)

coef_table <- summary(model1_fgcp)$coefficients
conf_int <- confint(model1_fgcp)

model2_fexf <- lm(fexf ~ frailty_status2 * sex + age + hukou + urbanicity + n_chronic_category + drink_now + smoke_now+ vigor_anyact , data = df)
model2_flang <- lm(flang ~ frailty_status2 * sex + age + hukou + urbanicity + n_chronic_category + drink_now + smoke_now+ vigor_anyact , data = df)
model2_forient <- lm(forient ~ frailty_status2 * sex + age + hukou + urbanicity + n_chronic_category + drink_now + smoke_now+ vigor_anyact , data = df)
model2_fmem <- lm(fmem ~ frailty_status2 * sex + age + hukou + urbanicity + n_chronic_category + drink_now + smoke_now+ vigor_anyact , data = df)
tab_model(model2_fgcp, model2_fexf, model2_flang, model2_forient, model2_fmem, show.se = T, show.ci = F)

####sensitivity analysis####
####sampling weight####

s_weight_sum <- sum(df$s_weight, na.rm = TRUE)
df$WEIGHT <- df$s_weight / s_weight_sum * 4879

model3_fgcp <- lm(fgcp ~ frailty_status2 + age + sex + hukou + urbanicity + n_chronic_category + drink_now + smoke_now+ vigor_anyact , data = df, weights = WEIGHT)
summary(model3_fgcp)

coef_table <- summary(model3_fgcp)$coefficients
conf_int <- confint(model3_fgcp)

model3_fexf <- lm(fexf ~ frailty_status2 + age + sex + hukou + urbanicity + n_chronic_category + drink_now + smoke_now+ vigor_anyact , data = df, weights = WEIGHT)
model3_flang <- lm(flang ~ frailty_status2 + age + sex + hukou + urbanicity + n_chronic_category + drink_now + smoke_now+ vigor_anyact , data = df, weights = WEIGHT)
model3_forient <- lm(forient ~ frailty_status2 + age + sex + hukou + urbanicity + n_chronic_category + drink_now + smoke_now+ vigor_anyact , data = df, weights = WEIGHT)
model3_fmem <- lm(fmem ~ frailty_status2 + age + sex + hukou + urbanicity + n_chronic_category + drink_now + smoke_now+ vigor_anyact , data = df, weights = WEIGHT)
tab_model(model3_fgcp, model3_fexf, model3_flang, model3_forient, model3_fmem, show.se = T, show.ci = F)

####social fraiity 0;1;2-5####
model4_fgcp <- lm(fgcp ~ frailty_status + age + sex + hukou + urbanicity + n_chronic_category + drink_now + smoke_now+ vigor_anyact , data = df)
summary(model4_fgcp)

coef_table <- summary(model4_fgcp)$coefficients
conf_int <- confint(model4_fgcp)

model4_fexf <- lm(fexf ~ frailty_status + age + sex + hukou + urbanicity + n_chronic_category + drink_now + smoke_now+ vigor_anyact , data = df)
model4_flang <- lm(flang ~ frailty_status + age + sex + hukou + urbanicity + n_chronic_category + drink_now + smoke_now+ vigor_anyact , data = df)
model4_forient <- lm(forient ~ frailty_status + age + sex + hukou + urbanicity + n_chronic_category + drink_now + smoke_now+ vigor_anyact , data = df)
model4_fmem <- lm(fmem ~ frailty_status + age + sex + hukou + urbanicity + n_chronic_category + drink_now + smoke_now+ vigor_anyact , data = df)
tab_model(model4_fgcp, model4_fexf, model4_flang, model4_forient, model4_fmem, show.se = T, show.ci = F)

####gender group####
table(df$sex)
table(df$frailty_status2,df$sex)
#men
model5_fgcp_men <- lm(fgcp ~ frailty_status2 + age  + hukou + urbanicity + n_chronic_category + drink_now + smoke_now+ vigor_anyact , data = df[df$sex == "men",])

model5_fexf_men <- lm(fexf ~ frailty_status2 + age  + hukou + urbanicity + n_chronic_category + drink_now + smoke_now+ vigor_anyact , data = df[df$sex == "men",])
model5_flang_men <- lm(flang ~ frailty_status2 + age  + hukou + urbanicity + n_chronic_category + drink_now + smoke_now+ vigor_anyact , data = df[df$sex == "men",])
model5_forient_men <- lm(forient ~ frailty_status2 + age  + hukou + urbanicity + n_chronic_category + drink_now + smoke_now+ vigor_anyact , data = df[df$sex == "men",])
model5_fmem_men <- lm(fmem ~ frailty_status2 + age  + hukou + urbanicity + n_chronic_category + drink_now + smoke_now+ vigor_anyact , data = df[df$sex == "men",])
tab_model(model5_fgcp_men,model5_fexf_men,model5_flang_men,model5_forient_men,model5_fmem_men,show.se = T)

#women
model5_fgcp_women <- lm(fgcp ~ frailty_status2 + age  + hukou + urbanicity + n_chronic_category + drink_now + smoke_now+ vigor_anyact , data = df[df$sex == "women",])

model5_fexf_women <- lm(fexf ~ frailty_status2 + age  + hukou + urbanicity + n_chronic_category + drink_now + smoke_now+ vigor_anyact , data = df[df$sex == "women",])
model5_flang_women <- lm(flang ~ frailty_status2 + age  + hukou + urbanicity + n_chronic_category + drink_now + smoke_now+ vigor_anyact , data = df[df$sex == "women",])
model5_forient_women <- lm(forient ~ frailty_status2 + age  + hukou + urbanicity + n_chronic_category + drink_now + smoke_now+ vigor_anyact , data = df[df$sex == "women",])
model5_fmem_women <- lm(fmem ~ frailty_status2 + age  + hukou + urbanicity + n_chronic_category + drink_now + smoke_now+ vigor_anyact , data = df[df$sex == "women",])
tab_model(model5_fgcp_women,model5_fexf_women,model5_flang_women,model5_forient_women,model5_fmem_women,show.se = T)
