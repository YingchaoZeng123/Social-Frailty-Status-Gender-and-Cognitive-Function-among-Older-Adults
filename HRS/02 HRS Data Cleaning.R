# Load necessary libraries
library(dplyr)
library(haven)
library(tableone)
library(mice)

setwd('D:\\code book\\SF and cognitive funtion\\HRS')

df <- read.csv('hrs_data.csv')
##define sample size; n = 3347 
sum(df$inw13)
df <- df[df$inw13 == 1,]

##exclude age >= 65; n = ? 
#df <- df[which(df$age >= 65), ]

##delete na in social fraity index; n = 1178
## combine rel,soc_activity
df <- df %>%
  mutate(
    # social participation
    no_participate_socwk = case_when(
      soc_activity == 0 ~ 1,
      soc_activity == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # rebel participation 0.no,1.yes
    no_participate_rel = case_when(
      rel_activity== 0 ~ 1,
      rel_activity == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # soc_activity 0.no,1.yes
    soc_activity = case_when(
      is.na(no_participate_socwk) & is.na(no_participate_rel) ~ NA_real_,
      no_participate_socwk == 1 & no_participate_rel == 1 ~ 1, 
      (no_participate_socwk==1&is.na(no_participate_rel))~1,
    (no_participate_rel==1&is.na(no_participate_socwk))~ 1,
      TRUE ~ 0  
    )
  )

vars <- c("finace_satified", "people_living_with", "week_contact", "soc_activity", "loneliness")
missing_values <- sapply(vars, function(var) sum(is.na(df[[var]])))
print(missing_values)

df <- subset(df, !(is.na(finace_satified)))
df <- subset(df, !(is.na(people_living_with)))
df <- subset(df, !(is.na(week_contact)))
df <- subset(df, !(is.na(soc_activity)))
df <- subset(df, !(is.na(loneliness)))
#df <- subset(df, !(is.na(household_wealth)))

##delete na in cognitive function; n = 1178
vars <- c("fgcp", "fexf", "flang", "forient", "fmem")
missing_values <- sapply(vars, function(var) sum(is.na(df[[var]])))
print(missing_values)
df <- subset(df, !(is.na(fgcp)))
# df <- subset(df, !(is.na(fexf)))
# df <- subset(df, !(is.na(flang)))
# df <- subset(df, !(is.na(forient)))
# df <- subset(df, !(is.na(fmem)))


####construct the outcome: social fraity####
# finace_satified, 1. Not at all satisfied; 2. Not very satisfied; 3. Somewhat satisfied; 
#  4. Very satisfied; 5. Completely satisfied

#1.
df <- df %>%
  mutate(
    finace_satified = case_when(
      finace_satified %in% c(1, 2) ~ 1,
      finace_satified %in% c(3, 4, 5) ~ 0,
      TRUE ~ NA_real_
    )
  )

table(df$finace_satified)

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
# df <- df %>%
#   mutate(soc_activity = case_when(
#     soc_activity == 0 ~ "1",
#     soc_activity == 1 ~ "0",
#     TRUE ~ NA_character_
#   ))
table(df$soc_activity)

#5.loneliness 
# 0.No, 1.YES
df <- df %>%
  mutate(loneliness = case_when(
    loneliness == 1 ~ "1",
    loneliness == 0 ~ "0",
    TRUE ~ NA_character_
  ))

table(df$loneliness)


vars <- c("finace_satified",  "people_living_with", "week_contact", "soc_activity", "loneliness")


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



#frailty_status 0, 1-2,3-5
df$frailty_status2 <- ifelse(df$frailty_score == 0, "non-frailty",
                             ifelse(df$frailty_score %in% c(1, 2), "pre-frailty",
                                    "frailty"))


table(df$frailty_status2, exclude = NULL)
table(df$frailty_score, exclude = NULL)

# df$frailty_status <- factor(df$frailty_status, levels = c("non-frailty", 
#                                                           "pre-frailty", 
#                                                           "frailty"))
df$frailty_status2 <- factor(df$frailty_status2, levels = c("non-frailty", 
                                                            "pre-frailty", 
                                                            "frailty"))
####covariates####
#recode variabels
#gender: 1 is men; 2 is women
table(df$gender, exclude = NULL)
df$gender <- ifelse(df$gender==1, "men", "women")
df$gender <- factor(df$gender)

#urbanicity 0.urban 1rural
df <- df %>%
  mutate(urbanicity = case_when(
    urbanicity == 0 ~ "urban",
    urbanicity == 1 ~ "rural",
    TRUE ~ NA_character_
  ))
table(df$urbanicity)
df$urbanicity <- factor(df$urbanicity)


#labour force status
# table(df$lb_status)
# df$lb_status <- ifelse(!is.na(df$lb_status) & df$lb_status == 1, "employed", 
#                        ifelse(!is.na(df$lb_status), "other", NA))



#chronic diseases
disease <- subset(df, select = c(r13arthre,
                                 r13cancre,
                                 r13diabe,
                                 r13hearte,
                                 r13hibpe,
                                 r13lunge,
                                 r13psyche,
                                 r13stroke
                                 
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
df <- df %>%
  mutate(
    drink_now = case_when(
      drink_now == 0 ~ 0,
      drink_now %in% 1:7 ~ 1,
      TRUE ~ NA_real_
    )
  )
table(df$drink_now, exclude = NULL)

#smoke status
table(df$smoke_now, exclude = NULL) #1=Yes, 0=none


#days of vigorous/moderate activity 1.3+ per week 2.1-2 per week 3.1-3 per mon 4.Lt 1 per mon 5.Never

table(df$vigor_act, exclude = NULL)

df <- df %>%
  mutate(
    any_vigoral_activities = case_when(
      vigor_act %in% 1:4 ~ 1,
      vigor_act == 5 ~ 0,
      TRUE ~ NA_real_
    )
  )

#race
table(df$race)
#race and hispanic
#Generate a combined race: 0 non-hisp white, 1 non-hisp black, 2 hispanic, 3 other 
df$race <- ifelse(df$race == 1 & 
                    df$hispanic == 0, 0,
                  ifelse(df$race == 2 & 
                           df$hispanic == 0, 1,
                         ifelse(df$hispanic == 1, 2, 3)))

df$race <- ifelse(df$race == 0, "Non-Hispanic White" , 
                  ifelse(df$race == 1, "Non-Hispanic Black",
                         ifelse(df$race == 2, "Hispanic","Other")))
table(df$race)

#education
table(df$H_edu)

variables <- c("age", "gender",
               "finace_satified", "urbanicity", "people_living_with", "week_contact", "soc_activity", "loneliness",
               "n_chronic_category","race",
               "drink_now","smoke_now","any_vigoral_activities", "H_edu"
)
for (var in variables) {
  missing_count <- sum(is.na(df[[var]]))
  missing_percentage <- (missing_count / nrow(df)) * 100
  cat("variable", var, "missing", missing_count, "\n")
}

df_converted <- df[, c("age", "gender",
                       "urbanicity", "n_chronic_category","race",
                       "drink_now","smoke_now","any_vigoral_activities", "H_edu")]
classes <- sapply(df_converted, class)
labelled_vars <- names(classes[classes == "labelled"])
df_converted <- df_converted %>%
  mutate_if(names(.) %in% c(  "gender",
                              "urbanicity", "n_chronic_category","race",
                              "drink_now","smoke_now","any_vigoral_activities", "H_edu"), as.factor)
set.seed(1005)
mice_mod <- mice(df_converted, method = "cart", m =1, maxit = 5)
imputed_data <- complete(mice_mod)
common_cols <- intersect(names(df), names(imputed_data))
df[common_cols] <- imputed_data[common_cols]

write.csv(df,"hrs_cleaning.csv")
