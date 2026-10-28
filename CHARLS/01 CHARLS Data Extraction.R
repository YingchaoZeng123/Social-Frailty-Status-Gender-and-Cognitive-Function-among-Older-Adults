library(reshape2)
library(haven)
library(dplyr)


setwd('D:\\code book\\SF and cognitive funtion\\CHARLS')
data_h <- read_dta('D:\\code book\\H_CHARLS_D_Data.dta')

# load the hcap data (2018)
charls_com<- read.csv("D:\\HCAP\\charls_survey.csv")


subdat_h <- subset(data_h, select = c(inw4,ID,householdID,
                                      r4iwstat, #w4 r interview status
                                      r4wtresp, #w4  without the non-response adjustment
                                      r4wtrespb, #w4 individual weight with HH/ind non-response adju
                                      #demographics
                                      r4lbrf_c, # r labor force status
                                      r4mstat, #marital status
                                      #health related
                                      r4shlta,#r Self-report of health alt
                                      r4tr20,# words recall
                                      r4orient, #orientation
                                      r4arthre, #  r ever had arthritis
                                      r4cancre, #  r ever had cancer
                                      r4hibpe,  #  r Ever had high blood pressure
                                      r4diabe,  #  r ever had diabetes
                                      r4lunge,  # r ever had lung disease
                                      r4hearte, #  r ever had heart problem
                                      r4stroke, #  r ever had stroke
                                      r4psyche,  # r ever had psych problem
                                      r4vgact_c,  #w4 r any vigorous physical activity 
                                      r4mdact_c,  #w4 r any moderate physical activity
                                      r4vgactx_c,  # r days/wk vigorous physical activity or exer
                                      r4mdactx_c, #r # days/wk moderate physical activity or exer
                                      r4drinkl, #r ever drinks any alcohol last year
                                      r4drinkr_c, # r range of # drinks/day
                                      r4smoken, # w4 r smoke now
                                      r4smokef, # w4 r # cigarettes/day
                                      r4adlab_c, #w4 r Some Diff-6 item ADL scale
                                      r4iadlza, #w4 r Some Diff-IADLs:summary /0-5
                                      #outcome, General Resources
                                      h4atotb, #Asset: r+s total wealth
                                      #outcome, Social Resources
                                      h4hhres, #number of people living in this household
                                      h4kcnt, #w4 any weekly contact w/ children in person/phone/ema
                                      #outcome, Social Behaviors or Activities
                                      r4socwk, # w4 r participate in social activities
                                      #outcome, Fulfillment of Basic Social Needs
                                      r4flonel #w4 r CESD: Felt lonely
                                      
))

names(subdat_h) <- c("inw4","ID","hhid",
                     "r4iwstat", "s_weight",
                     "r4wtrespb", 
                    "lb_status","marital_status",
                     "SHLT","r1tr20","orient",
                     "r4arthre","r4cancre","r4hibpe","r4diabe","r4lunge","r4hearte","r4stroke","r4psyche",
                     "vigor_anyact", "moder_anyact","vigor_act", "moder_act", "drink_now",  "num_drink", "smoke_now","num_smoke","ADL", "IADL", 
                     "household_wealth", 
                     "people_living_with","week_contact",
                     "soc_activity",
                     "loneliness"
)


subdat_h$ID <- as.numeric(subdat_h$ID)
charls_data <- inner_join(subdat_h, charls_com, by = c("ID" = "ID"))


# Save the modified data
write.csv(charls_data, "D:\\code book\\SF and cognitive funtion\\CHARLS\\charls_data.csv")
