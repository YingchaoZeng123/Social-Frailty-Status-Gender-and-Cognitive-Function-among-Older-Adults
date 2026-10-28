library(reshape2)
library(haven)
library(dplyr)


setwd('D:\\code book\\SF and cognitive funtion\\ELSA')
data_h <- read_dta('D:\\code book\\Educational mobility\\ELSA\\H_ELSA_g3.dta')

# load the hcap data (2018)
hacp <- read_stata("D:\\HCAP\\interim-HcapHarmoniz-305.dta")
elsa <- filter(hacp, study == 2)
elsa_com <- elsa %>%
  select(id_elsa, 
         age,
         fgcp,
         fexf,
         flang,
         forient,
         fmem)

subdat_h <- subset(data_h, select = c(inw8,idauniq,hh8hhid,
                                      r8iwstat, #w4 r interview status
                                      r8cwtresp, #w4 individual weight with HH/ind non-response adju
                                      rabyear,# birth year
                                      #demographics
                                      r8agey,#age at interview
                                      ragender,#gender
                                      raracem, #race
                                      r8lbrf_e, # r labor force status
                                      r8mstat, #marital status
                                      #health related
                                      r8shlt,#r Self-report of health alt
                                      r8tr20,# words recall
                                      r8orient, #orientation
                                      r8arthre, #  r ever had arthritis
                                      r8cancre, #  r ever had cancer
                                      r8hibpe,  #  r Ever had high blood pressure
                                      r8diabe,  #  r ever had diabetes
                                      r8lunge,  # r ever had lung disease
                                      r8hearte, #  r ever had heart problem
                                      r8stroke, #  r ever had stroke
                                      r8psyche,  # r ever had psych problem
                                      r8vgactx_e,  # r days/wk vigorous physical activity or exer
                                      r8mdactx_e, #r # days/wk moderate physical activity or exer
                                      r8drink, #r ever drinks any alcohol last year
                                      r8drinkd_e, # r range of # drinks/day
                                      r8smoken, # w4 r smoke now
                                      r8smokef, # w4 r # cigarettes/day
                                      r8adltot6, #w4 r Some Diff-6 item ADL scale
                                      r8iadlza, #w4 r Some Diff-IADLs:summary /0-5
                                      #exposre
                                      raeduc_e,raeducl,#edu
                                      h8atotb, #Asset: r+s total wealth
                                      h8hhres, #number of people living in this household
                                      r8kcnt, #w4 any weekly contact w/ children in person/phone/ema
                                      #outcome, Social Behaviors or Activities
                                      r8socyr, # w4 r participate in social activities
                                      #outcome, Fulfillment of Basic Social Needs
                                      r8flone #w4 r CESD: Felt lonely
                                      
))

names(subdat_h) <- c("inw8","ID","hhid",
                     "r8iwstat",
                     "r8wtrespb", "birth_year",
                     "age", "gender", "race", "lb_status","marital_status",
                     "SHLT","r1tr20","orient",
                     "r8arthre","r8cancre","r8hibpe","r8diabe","r8lunge","r8hearte","r8stroke","r8psyche",
                     "vigor_act", "moder_act", "drink_now",  "num_drink", "smoke_now","num_smoke","ADL", "IADL", 
                     "years_edu","H_edu",
                     "household_wealth",
                     "people_living_with","week_contact",
                     "soc_activity",
                     "loneliness"
)


subdat_h$ID <- as.character(subdat_h$ID)
elsa_data <- inner_join(subdat_h, elsa_com, by = c("ID" = "id_elsa"))


# Save the modified data
write.csv(elsa_data, 'elsa_data.csv')
