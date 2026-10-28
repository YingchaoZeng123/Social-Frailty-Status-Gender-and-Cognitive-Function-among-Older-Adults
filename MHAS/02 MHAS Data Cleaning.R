# Load necessary libraries
library(dplyr)
library(haven)
library(tableone)
library(mice)

setwd('D:\\code book\\SF and cognitive funtion\\MHAS')

df <- read.csv('mhas_data.csv')
##define sample size; n = 2042 
sum(df$inw5)
df <- df[df$inw5 == 1,]

##exclude age >= 65; n = 1256 
df <- df[which(df$age.x >= 65), ]

##delete na in social fraity index; n = 1174

df <- df %>%
  mutate(
    no_participate_socwk = case_when(
      soc_activity == 0 ~ 1,
      soc_activity == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    no_participate_rel = case_when(
      rel_activity== 0 ~ 1,
      rel_activity == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    soc_activity = case_when(
      is.na(no_participate_socwk) & is.na(no_participate_rel) ~ NA_real_,    
      no_participate_socwk == 1 & no_participate_rel == 1 ~ 1,               
      no_participate_socwk == 1 & is.na(no_participate_rel) ~ 1,           
      no_participate_rel == 1 & is.na(no_participate_socwk) ~ 1,           
      TRUE ~ 0                                                               
    )
  )
table(df$soc_activity)


vars <- c("finace_satified", "people_living_with", "week_contact", "soc_activity", "loneliness")
missing_values <- sapply(vars, function(var) sum(is.na(df[[var]])))
print(missing_values)

df <- subset(df, !(is.na(finace_satified)))
df <- subset(df, !(is.na(people_living_with)))
df <- subset(df, !(is.na(week_contact)))
df <- subset(df, !(is.na(soc_activity)))
df <- subset(df, !(is.na(loneliness)))
#df <- subset(df, !(is.na(household_wealth)))

##delete na in cognitive function; n = 1174
vars <- c("fgcp", "fexf", "flang", "forient", "fmem")
missing_values <- sapply(vars, function(var) sum(is.na(df[[var]])))
print(missing_values)
df <- subset(df, !(is.na(fgcp)))
# df <- subset(df, !(is.na(fexf)))
# df <- subset(df, !(is.na(flang)))
# df <- subset(df, !(is.na(forient)))
# df <- subset(df, !(is.na(fmem)))


####construct the outcome: social fraity####
# finace_satified
#1.3
# table(df$finace_satified)
# df$finace_satified1 <- case_when(
#   df$finace_satified %in% 1:3 ~ 0,
#   df$finace_satified %in% 4:5 ~ 1,
#   TRUE ~ NA_real_  # 使用数值型的 NA
# )
# table(df$finace_satified1)

#1.4 1 Excellent; 2 Very good; 3 Good; 4 Fair; 5 Poor; 8 RF; 9 DK
df$finace_satified2 <- case_when(
  df$finace_satified %in% 1:4 ~ 0,
  df$finace_satified %in% 5 ~ 1,
  TRUE ~ NA_real_  
)

table(df$finace_satified2)

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


#5.loneliness 
# 0.No, 1.YES
df <- df %>%
  mutate(loneliness = case_when(
    loneliness == 1 ~ "1",
    loneliness == 0 ~ "0",
    TRUE ~ NA_character_
  ))

table(df$loneliness)


vars <- c("finace_satified2",  "people_living_with", "week_contact", "soc_activity", "loneliness")


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

#frailty_status 0, 1, 2-5
df$frailty_status <- ifelse(df$frailty_score == 0, "non-frailty",
                            ifelse(df$frailty_score == 1, "pre-frailty",
                                   "frailty"))

#frailty_status 0, 1-2,3-5
df$frailty_status2 <- ifelse(df$frailty_score == 0, "non-frailty",
                             ifelse(df$frailty_score %in% c(1, 2), "pre-frailty",
                                    "frailty"))


table(df$frailty_status2, exclude = NULL)
table(df$frailty_status, exclude = NULL)
table(df$frailty_score, exclude = NULL)

df$frailty_status <- factor(df$frailty_status, levels = c("non-frailty", 
                                                           "pre-frailty", 
                                                           "frailty"))
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

#orientation
summary(df$orient)

#word recall
summary(df$r1tr20)

df <- df %>%
  mutate(
    cognitive_impairment = ifelse(
      orient < mean(orient, na.rm = TRUE) - 1.5 * sd(orient, na.rm = TRUE) &
        r1tr20 < mean(r1tr20, na.rm = TRUE) - 1.5 * sd(r1tr20, na.rm = TRUE),
      "Impaired", 
      "Normal"
    )
  ) %>%
  ungroup() 



#chronic diseases
disease <- subset(df, select = c(r5arthre, #  r ever had arthritis
                                 r5cancre, #  r ever had cancer
                                 r5hibpe,  #  r Ever had high blood pressure
                                 r5diabe,  #  r ever had diabetes
                                 r5lunge,  #  r ever had lung disease
                                 r5hearte, #  r ever had heart problem
                                 r5stroke #  r ever had stroke
))
disease <- data.frame(disease) 
disease$n_chronic <- rowSums(disease) 

disease_numeric <- disease %>%
  mutate(n_chronic_category = case_when(
    n_chronic == 0 | n_chronic == 1~ "0 or 1 chronic",
    n_chronic >= 2 & n_chronic <= 7 ~ "more than 2 chronic",
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

#days of vigorous/moderate activity
table(df$vigor_anyact, exclude = NULL)

#education
table(df$H_edu)

#age.x=age.y
variables <- c("age.x", "gender",
               "finace_satified2", "urbanicity", "people_living_with", "week_contact", "soc_activity", "loneliness",
                "marital_status","n_chronic_category",
               "drink_now","smoke_now","vigor_anyact", "H_edu"
)
for (var in variables) {
  missing_count <- sum(is.na(df[[var]]))
  missing_percentage <- (missing_count / nrow(df)) * 100
  cat("variable", var, "missing", missing_count, "\n")
}

df_converted <- df[, c("age.x", "gender",
                        "urbanicity", "n_chronic_category",
                       "drink_now","smoke_now","num_smoke","vigor_anyact", "H_edu")]
classes <- sapply(df_converted, class)
labelled_vars <- names(classes[classes == "labelled"])
df_converted <- df_converted %>%
  mutate_if(names(.) %in% c(  "gender",
                             "urbanicity", "n_chronic_category",
                             "drink_now","smoke_now","vigor_anyact", "H_edu"), as.factor)
set.seed(1005)
mice_mod <- mice(df_converted, method = "cart", m =1, maxit = 5)
imputed_data <- complete(mice_mod)
common_cols <- intersect(names(df), names(imputed_data))
df[common_cols] <- imputed_data[common_cols]

write.csv(df,"mhas_cleaning.csv")
