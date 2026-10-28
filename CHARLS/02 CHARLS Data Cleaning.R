# Load necessary libraries
library(dplyr)
library(haven)
library(tableone)
library(mice)

setwd('D:\\code book\\SF and cognitive funtion\\CHARLS')
# Load the first dataset (charls_survey.dta)
df <- read.csv("D:\\code book\\SF and cognitive funtion\\CHARLS\\charls_data.csv")

##define sample size; n = 9755
sum(df$inw4)
df <- df[df$inw4 == 1,]

##exclude age >= 50; n = 6538
df <- df[which(df$age >= 65), ]

##delete na in social fraity index; n = 4879

vars <- c("household_wealth", "people_living_with", "week_contact", "soc_activity", "loneliness")
missing_values <- sapply(vars, function(var) sum(is.na(df[[var]])))
print(missing_values)

df <- subset(df, !(is.na(household_wealth)))
df <- subset(df, !(is.na(people_living_with)))
df <- subset(df, !(is.na(week_contact)))
df <- subset(df, !(is.na(soc_activity)))
df <- subset(df, !(is.na(loneliness)))


##delete na in cognitive function; n = 4879
vars <- c("fgcp", "fexf", "flang", "forient", "fmem")
missing_values <- sapply(vars, function(var) sum(is.na(df[[var]])))
print(missing_values)
df <- subset(df, !(is.na(fgcp)))
# df <- subset(df, !(is.na(fexf)))
# df <- subset(df, !(is.na(flang)))
# df <- subset(df, !(is.na(forient)))
# df <- subset(df, !(is.na(fmem)))

####construct the outcome: social fraity####
#1. household_wealth
#1.1 50%
df$equivalized_wealth <- ifelse(is.na(df$people_living_with) | is.na(df$household_wealth), NA, df$household_wealth / sqrt(df$people_living_with))

df$equivalized_wealth <- ifelse(is.na(df$equivalized_wealth), NA,round(df$equivalized_wealth / 1000, 2))

summary(df$equivalized_wealth)
quantile_breaks <- quantile(df$equivalized_wealth, probs = c(0, 0.25, 1), na.rm = TRUE)
df$wealth_quartile <- cut(df$equivalized_wealth, 
                          breaks = quantile_breaks,
                          include.lowest = TRUE,
                          labels = c("1", "0"))  

table(df$wealth_quartile, useNA = "ifany")


#2. people_living_with
df <- df %>%
  mutate(people_living_with = case_when(
    people_living_with == 1 ~ "1",
    people_living_with >= 2 ~ "0",
    TRUE ~ NA_character_
  ))
table(df$people_living_with)

#3. week_contact 0.no, 1.yes
df <- df %>%
  mutate(week_contact = case_when(
    week_contact == 0 ~ "1",
    week_contact == 1 ~ "0",
    TRUE ~ NA_character_
  ))
table(df$week_contact)

#4. soc_activity #1=Yes, 0=none
df <- df %>%
  mutate(soc_activity = case_when(
    soc_activity == 0 ~ "1",
    soc_activity == 1 ~ "0",
    TRUE ~ NA_character_
  ))
table(df$soc_activity)

#5.loneliness 
# 1.Rarely or none of the time < 1 day;2.Some or a little of the time 1-2 days;3.Occasionally or a moderate amount of 3; 4.Most or all of the time 5-7 days
df <- df %>%
  mutate(loneliness = case_when(
    loneliness %in% c(3, 4) ~ "1",
    loneliness %in% c(1, 2) ~ "0",
    TRUE ~ NA_character_
  ))

table(df$loneliness, exclude = NULL)


vars <- c("wealth_quartile",  "people_living_with", "week_contact", "soc_activity", "loneliness")

df[vars] <- lapply(df[vars], function(x) {
  if (is.factor(x)) {
    as.numeric(as.character(x))  
  } else {
    as.numeric(x) 
  }
})

str(df[, vars])
df <- df %>%
  mutate(across(all_of(vars), ~as.numeric(as.character(.))))
df$frailty_score <- rowSums(df[, vars], na.rm = FALSE)

# #frailty_status 0, 1, 2-5
df$frailty_status <- ifelse(df$frailty_score == 0, "non-frailty",
                            ifelse(df$frailty_score == 1, "pre-frailty",
                                   "frailty"))
#frailty_status 0, 1-2,3-5
df$frailty_status2 <- ifelse(df$frailty_score == 0, "non-frailty",
                             ifelse(df$frailty_score %in% c(1, 2), "pre-frailty",
                                    "frailty"))

table(df$frailty_status, exclude = NULL)
table(df$frailty_status2, exclude = NULL)
table(df$frailty_score, exclude = NULL)

df$frailty_status <- factor(df$frailty_status, levels = c("non-frailty",
                                                          "pre-frailty",
                                                          "frailty"))
df$frailty_status2 <- factor(df$frailty_status2, levels = c("non-frailty", 
                                                            "pre-frailty", 
                                                            "frailty"))

####covariates####
#recode variabels
#sex: 1 is men; 2 is women
table(df$sex, exclude = NULL)
df$sex <- ifelse(df$sex==1, "men", "women")
df$sex <- factor(df$sex)

#urbanicity
df <- df %>%
  rename(urbanicity = h4rural)

df <- df %>%
  mutate(urbanicity = case_when(
    urbanicity == 0 ~ "urban",
    urbanicity == 1 ~ "rural",
    TRUE ~ NA_character_
  ))
table(df$urbanicity)
df$urbanicity <- factor(df$urbanicity)


#hukou
table(df$hukou)
#1.Agricultual hukou, 2.Non-agricultural hukou, 3.Unified residence hukou, 4.Do not have hukou
#combine 1 and 4
df$hukou <- ifelse(df$hukou==1|df$hukou==4, 1, 0)
table(df$hukou)


#labour force status
table(df$lb_status)
#1.Agri employed, 2.Agri self-employed, 3.Non-agri employed, 4.Non-agri self-employed
#5.Non-agri family business, 6.Unemployed, 7.Retired, 8.Never work
df$lb_status <- ifelse(df$lb_status==1|df$lb_status==2|df$lb_status==3|df$lb_status==4|df$lb_status==5,"employed", "other")

#chronic diseases
disease <- subset(df, select = c(r4arthre, #  r ever had arthritis
                                 r4cancre, #  r ever had cancer
                                 r4hibpe,  #  r Ever had high blood pressure
                                 r4diabe,  #  r ever had diabetes
                                 r4lunge,  #  r ever had lung disease
                                 r4hearte, #  r ever had heart problem
                                 r4stroke, #  r ever had stroke
                                 r4psyche  #  r ever had psych problem
))
disease <- data.frame(disease) 
disease$n_chronic <- rowSums(disease) 

disease_numeric <- disease %>%
  mutate(n_chronic_category = case_when(
    n_chronic == 0 | n_chronic == 1~ "0 or 1 chronic",
    n_chronic >= 2 & n_chronic <= 8 ~ "more than 2 chronic",
    TRUE ~ NA_character_  
  ))
df <- cbind(df, n_chronic_category = disease_numeric$n_chronic_category)

#drink
df$drink_now <- ifelse(df$num_drink == 0, 0, 1)  
table(df$drink_now, exclude = NULL)

#smoke status
table(df$smoke_now, exclude = NULL) #1=Yes, 0=none


#marital status: 1.married; 3.partnered; 4.separated; 5.divorced; 7.widowed; 8.never married
df$marital_status<- ifelse(df$marital_status==1|df$marital_status==3, "married/partnered", "other")
table(df$marital_status, exclude = NULL)

#ADL
table(df$ADL)
table(df$IADL)


#any vigorous activity 0.No,1.Yes
table(df$vigor_anyact, exclude = NULL)


#education
df <- df %>%
  rename(education_level = raeducl)
table(df$education_level)


variables <- c("age", "sex","hukou",
               "wealth_quartile", "urbanicity", "people_living_with", "week_contact", "soc_activity", "loneliness",
               "lb_status", "marital_status","n_chronic_category",
               "drink_now","smoke_now","num_smoke","ADL","IADL","vigor_anyact","education_level"
)
for (var in variables) {
  missing_count <- sum(is.na(df[[var]]))
  missing_percentage <- (missing_count / nrow(df)) * 100
  cat("variable", var, "missing", missing_count, "\n")
}


#covariates imputation
df_converted <- df[, c("age", "sex","hukou",
                        "urbanicity", 
                       "lb_status", "marital_status","n_chronic_category",
                       "drink_now","smoke_now","num_smoke","ADL","IADL","vigor_anyact","education_level")]
classes <- sapply(df_converted, class)
labelled_vars <- names(classes[classes == "labelled"])
df_converted <- df_converted %>%
  mutate_if(names(.) %in% c( "sex","hukou",
                            "urbanicity", 
                            "lb_status", "marital_status","n_chronic_category",
                            "drink_now","smoke_now","num_smoke","vigor_anyact","education_level"), as.factor)
set.seed(1005)
mice_mod <- mice(df_converted, method = "cart", m =1, maxit = 5)
imputed_data <- complete(mice_mod)
common_cols <- intersect(names(df), names(imputed_data))
df[common_cols] <- imputed_data[common_cols]

write.csv(df,"charls_cleaning.csv")
