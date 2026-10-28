library(reshape2)
library(haven)
library(dplyr)


setwd('D:\\code book\\SF and cognitive funtion\\MHAS')
data_h <- read_dta('D:\\code book\\Educational mobility\\MHAS\\H_MHAS_c.dta')

# load the hcap data (2018)
hacp <- read_stata("D:\\HCAP\\interim-HcapHarmoniz-305.dta")
mhas <- filter(hacp, study == 3)
mhas_com <- mhas %>%
  select(id_mexcog, 
         age,
         fgcp,
         fexf,
         flang,
         forient,
         fmem)

data_h <- read_dta('D:\\code book\\Educational mobility\\MHAS\\H_MHAS_c.dta')
data_o <- read_dta('D:\\code book\\MHAS_Origin_2015.dta')

subdat_h0 <- subset(data_h, select = c(inw4,unhhid,unhhidnp, h4hhid,np,rahhidnp,
                                       r4iwstat, #w4 r interview status
                                       r4wtresp, #w4 individual weight with HH/ind non-response adju
                                       rabyear,# birth year
                                       #demographics
                                       r4agey,#age at interview
                                       ragender,#gender4
                                       r4lbrf_m, # r labor force status
                                       r4mstat, #marital status
                                       #health related
                                       r4tr8_m,# words recall
                                       r4orient_m, #orientation
                                       r4arthre, #  r ever had arthritis
                                       r4cancre, #  r ever had cancer
                                       r4hibpe,  #  r Ever had high blood pressure
                                       r4diabe,  #  r ever had diabetes
                                       r4lunge_m,  # r ever had lung disease
                                       r4hrtatte, #  r Ever had heart attack
                                       r4stroke, #  r ever had stroke
                                       r4vigact, #w4 R Wtr vigorus phys act 3+/wk
                                       r4drink, # w4 R Ever drinks any alcohol
                                       r4drinkd, # w4 R Number of days/week drinks
                                       r4smoken, #w4 R Smokes now
                                       r4smokef, #w4 R Number of cigarettes/day
                                       #exposre
                                       raedyrs,raeducl,#edu
                                       rameduc_m,rafeduc_m,#parental edu level,1.None,2.Some primary,3.Primary,4.More than primary
                                       #outcome, General Resources
                                       h4atotb, #Asset: r+s total wealth
                                       h4rural, #live in rural/urban
                                       #outcome, Social Resources
                                       h4hhresp, #number of people living in this household
                                       h4kcnt, #w4 any weekly contact w/ children in person/phone/ema
                                       #outcome, Social Behaviors or Activities
                                       r4socwk, # w4 r participate in social activities
                                       r4relgwk, #Any weekly participation in religious service
                                       #outcome, Fulfillment of Basic Social Needs
                                       r4flone #w4 r CESD: Felt lonely
                                       
))

subdat_o <- subset(data_o, select = c(cunicah,np,i30_15))
subdat_o <- subdat_o %>%
  rename(unhhid = cunicah)
subdat_h <- merge(subdat_h0,subdat_o, by = c("unhhid","np"))

names(subdat_h) <- c("unhhid","np","inw5","ID","hhid","rahhidnp",
                     "r5iwstat",
                     "r5wtrespb", "birth_year",
                     "age", "gender",  "lb_status","marital_status",
                     "r1tr20","orient",
                     "r5arthre","r5cancre","r5hibpe","r5diabe","r5lunge","r5hearte","r5stroke",
                     "vigor_anyact",  "drink_now",  "num_drink", "smoke_now","num_smoke",
                     "years_edu","H_edu","H_edu_m","H_edu_f",
                     "household_wealth", "urbanicity",
                     "people_living_with","week_contact",
                     "soc_activity", "rel_activity",
                     "loneliness","finace_satified"
)


subdat_h$rahhidnp <- as.character(subdat_h$rahhidnp)
mhas_data <- inner_join(subdat_h, mhas_com, by = c("rahhidnp" = "id_mexcog"))


# Save the modified data
write.csv(mhas_data, 'mhas_data.csv')
