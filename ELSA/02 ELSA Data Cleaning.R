# Load necessary libraries
library(dplyr)
library(haven)
library(tableone)
library(mice)


setwd('D:\\code book\\SF and cognitive funtion\\ELSA')
data_h <- read_dta('D:\\code book\\wave_9_ifs_derived_variables.dta')
df_merged <- read.csv('elsa_data.csv')

data_h_subset <- data_h %>%
  select(ID = idauniq, findiff) %>%
  mutate(
    findiff = ifelse(findiff %in% c(-8, -9, -1), NA, findiff), # finace_satified1 exclude average
    finace_satified1 = case_when(
      findiff %in% c(4, 5, 6) ~ 1,
      findiff %in% c(1, 2, 3) ~ 0,
      TRUE ~ NA_real_
    ),
    finace_satified2 = case_when(
      findiff %in% c(3, 4, 5, 6) ~ 1,
      findiff %in% c(1, 2) ~ 0,
      TRUE ~ NA_real_
    )
  )

table(data_h_subset$findiff)
table(data_h_subset$finace_satified1)
table(data_h_subset$finace_satified2)

df_merged <- df_merged %>%
  mutate(ID = as.character(ID))

data_h_subset <- data_h_subset %>%
  mutate(ID = as.character(ID))

df_merged <- df_merged %>%
  left_join(data_h_subset, by = "ID")

table(df_merged$finace_satified1, useNA = "ifany")

##define sample size; n = 1238 
sum(df_merged$inw8)
df_merged <- df_merged[df_merged$inw8 == 1,]

##exclude age.y >= 65; n = 1238  
#df_merged <- df_merged[which(df_merged$age >= 65), ]
table(df_merged$age.y)

##exclude expoure NA; n = 772
vars <- c("finace_satified1", "people_living_with", "week_contact", "soc_activity", "loneliness")
missing_values <- sapply(vars, function(var) sum(is.na(df_merged[[var]])))
print(missing_values)

df_merged <- subset(df_merged, !(is.na(finace_satified1)))
df_merged <- subset(df_merged, !(is.na(people_living_with)))
df_merged <- subset(df_merged, !(is.na(week_contact)))
df_merged <- subset(df_merged, !(is.na(soc_activity)))
df_merged <- subset(df_merged, !(is.na(loneliness)))
#df_merged <- subset(df_merged, !(is.na(household_wealth)))

##delete na in cognitive function; n = 772
vars <- c("fgcp", "fexf", "flang", "forient", "fmem")
missing_values <- sapply(vars, function(var) sum(is.na(df_merged[[var]])))
print(missing_values)
df_merged <- subset(df_merged, !(is.na(fgcp)))
# df_merged <- subset(df_merged, !(is.na(fexf)))
# df_merged <- subset(df_merged, !(is.na(flang)))
# df_merged <- subset(df_merged, !(is.na(forient)))
# df_merged <- subset(df_merged, !(is.na(fmem)))


####construct the outcome: social fraity####
# finace_satified1

# #1.
# df_merged <- df_merged %>%
#   mutate(
#     finace_satified = case_when(
#       finace_satified %in% c(1, 2) ~ 1,
#       finace_satified %in% c(3, 4, 5) ~ 0,
#       TRUE ~ NA_real_
#     )
#   )
# 
# table(df_merged$finace_satified)

#2. people_living_with
df_merged <- df_merged %>%
  mutate(people_living_with = case_when(
    people_living_with == 1 ~ "1",
    people_living_with >= 2 ~ "0",
    TRUE ~ NA_character_
  ))
table(df_merged$people_living_with)

#3. week_contact 0.no, 1.yes
df_merged <- df_merged %>%
  mutate(week_contact = case_when(
    week_contact == 0 ~ "1",
    week_contact == 1 ~ "0",
    TRUE ~ NA_character_
  ))
table(df_merged$week_contact)

#4. soc_activity #1=Yes, 0=none
df_merged <- df_merged %>%
  mutate(soc_activity = case_when(
    soc_activity == 0 ~ "1",
    soc_activity == 1 ~ "0",
    TRUE ~ NA_character_
  ))
table(df_merged$soc_activity)

#5.loneliness 
# 0.No, 1.YES
df_merged <- df_merged %>%
  mutate(loneliness = case_when(
    loneliness == 1 ~ "1",
    loneliness == 0 ~ "0",
    TRUE ~ NA_character_
  ))

table(df_merged$loneliness)


vars <- c("finace_satified1",  "people_living_with", "week_contact", "soc_activity", "loneliness")


df_merged[vars] <- lapply(df_merged[vars], function(x) {
  if (is.factor(x)) {
    as.numeric(as.character(x))  
  } else {
    as.numeric(x) 
  }
})

str(df_merged[, vars])
df_merged <- df_merged %>%
  mutate(across(all_of(vars), ~as.numeric(as.character(.))))
df_merged$frailty_score <- rowSums(df_merged[, vars], na.rm = FALSE)

#frailty_status 0, 1, 2-5
# df_merged$frailty_status <- ifelse(df_merged$frailty_score == 0, "non-frailty",
#                             ifelse(df_merged$frailty_score == 1, "pre-frailty",
#                                    "frailty"))

#frailty_status 0, 1-2,3-5
df_merged$frailty_status2 <- ifelse(df_merged$frailty_score == 0, "non-frailty",
                             ifelse(df_merged$frailty_score %in% c(1, 2), "pre-frailty",
                                    "frailty"))


table(df_merged$frailty_status2, exclude = NULL)
table(df_merged$frailty_score, exclude = NULL)

# df_merged$frailty_status <- factor(df_merged$frailty_status, levels = c("non-frailty", 
#                                                           "pre-frailty", 
#                                                           "frailty"))
df_merged$frailty_status2 <- factor(df_merged$frailty_status2, levels = c("non-frailty", 
                                                            "pre-frailty", 
                                                            "frailty"))
####covariates####
#recode variabels
#gender: 1 is men; 2 is women
table(df_merged$gender, exclude = NULL)
df_merged$gender <- ifelse(df_merged$gender==1, "men", "women")
df_merged$gender <- factor(df_merged$gender)

#urbanicity 0.urban 1rural
# df_merged <- df_merged %>%
#   mutate(urbanicity = case_when(
#     urbanicity == 0 ~ "urban",
#     urbanicity == 1 ~ "rural",
#     TRUE ~ NA_character_
#   ))
# table(df_merged$urbanicity)
# df_merged$urbanicity <- factor(df_merged$urbanicity)


#labour force status
# table(df_merged$lb_status)
# df_merged$lb_status <- ifelse(!is.na(df_merged$lb_status) & df_merged$lb_status == 1, "employed", 
#                        ifelse(!is.na(df_merged$lb_status), "other", NA))



#chronic diseases
disease <- subset(df_merged, select = c(r8arthre, #  r ever had arthritis
                                 r8cancre, #  r ever had cancer
                                 r8hibpe,  #  r Ever had high blood pressure
                                 r8diabe,  #  r ever had diabetes
                                 r8lunge,  #  r ever had lung disease
                                 r8hearte, #  r ever had heart problem
                                 r8stroke, #  r ever had stroke
                                 r8psyche  #  r ever had psych problem
))
disease <- data.frame(disease) 
disease$n_chronic <- rowSums(disease) 

disease_numeric <- disease %>%
  mutate(n_chronic_category = case_when(
    n_chronic == 0 | n_chronic == 1~ "0 or 1 chronic",
    n_chronic >= 2 & n_chronic <= 8 ~ "more than 2 chronic",
    TRUE ~ NA_character_  
  ))
df_merged <- cbind(df_merged, n_chronic_category = disease_numeric$n_chronic_category)

#drink
df_merged <- df_merged %>%
  mutate(drink_now = case_when(
    as.numeric(drink_now) == 1 ~ "Yes",
    as.numeric(drink_now) == 0 ~ "No",
    TRUE ~ NA_character_
  ))

table(df_merged$drink_now, exclude = NULL)



#smoke status
table(df_merged$smoke_now, exclude = NULL) #1=Yes, 0=none


#days of vigorous/moderate activity
table(df_merged$vigor_act, exclude = NULL)
table(df_merged$moder_act, exclude = NULL)
df_merged <- df_merged %>%
  mutate(any_vigoral_activities = case_when(
    vigor_act %in% 2:4 ~ 1,
    vigor_act == 5 ~ 0,
    TRUE ~ NA_real_
  )) %>%
  mutate(any_vigoral_activities = as.factor(any_vigoral_activities))
table(df_merged$any_vigoral_activities, useNA = "ifany")

#race
table(df_merged$race)
df_merged <- df_merged %>%
  mutate(
    race_binary = case_when(
      race == 1 ~ 1,
      !is.na(race) ~ 0,
      TRUE ~ NA_real_
    )
  )

#education
table(df_merged$H_edu)

variables <- c("age.y", "gender",
               "finace_satified1",  "people_living_with", "week_contact", "soc_activity", "loneliness",
               "n_chronic_category","race",
               "drink_now","smoke_now","any_vigoral_activities", "H_edu"
)
for (var in variables) {
  missing_count <- sum(is.na(df_merged[[var]]))
  missing_percentage <- (missing_count / nrow(df_merged)) * 100
  cat("variable", var, "missing", missing_count, "\n")
}

df_merged_converted <- df_merged[, c("age.y", "gender",
                        "n_chronic_category","race",
                       "drink_now","smoke_now","any_vigoral_activities", "H_edu")]
classes <- sapply(df_merged_converted, class)
labelled_vars <- names(classes[classes == "labelled"])
df_merged_converted <- df_merged_converted %>%
  mutate_if(names(.) %in% c(  "gender",
                               "n_chronic_category","race",
                              "drink_now","smoke_now","any_vigoral_activities", "H_edu"), as.factor)
set.seed(1005)
mice_mod <- mice(df_merged_converted, method = "cart", m =1, maxit = 5)
imputed_data <- complete(mice_mod)
common_cols <- intersect(names(df_merged), names(imputed_data))
df_merged[common_cols] <- imputed_data[common_cols]

write.csv(df_merged,"elsa_cleaning.csv")
