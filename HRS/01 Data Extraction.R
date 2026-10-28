library(reshape2)
library(haven)
library(dplyr)


setwd('D:\\code book\\SF and cognitive funtion\\HRS')

#load the hcap data (2018)
# hacp <- read_stata("D:\\HCAP\\interim-HcapHarmoniz-305-v2-20230725-hrshcap.dta")
# hrs_hcap <- hacp %>%
#   select(id_hrs,
#          fgcp,
#          fexf,
#          flang,
#          forient,
#          fmem)
# hrs_hcap <- hrs_hcap %>%
#   rename(ID = id_hrs)

data_h <- read_dta('D:\\code book\\H_HRS_c.dta')
data_o <- read_dta('D:\\code book\\randhrs1992_2020v2.dta')
hrs_factors <- read_dta("D:\\code book\\HCAP factor data\\HRS factor scores.dta")

subdat_h <- subset(data_h, select = c(hhidpn, hhid,
                                       h13kcnt,
                                       r13socwk,
                                       r13relgwk,
                                       r13adltot_h,
                                       r13iadltotm_h,
                                       h13rural,
                                       raeducl,
                                       r13smokef
                                       
))

names(subdat_h) <- c("ID","hhid",
                     "week_contact",
                     "soc_activity",
                     "rel_activity",
                     "ADL", "IADL",
                     "urbanicity",
                     "H_edu",
                     "num_smoke"
                     
)


subdat_r <- subset(data_o, select = c(inw13,hhidpn, hhid,r13wtresp,
                                       r13lbsatfin,
                                       h13hhres,
                                       r13flone,
                                       ragender,
                                       r13agey_m,
                                       r13mstat,
                                       raracem,rahispan, #race and ethnicity
                                       r13arthre,
                                       r13cancre,
                                       r13diabe,
                                       r13hearte,
                                       r13hibpe,
                                       r13lunge,
                                       r13psyche,
                                       r13stroke,
                                       r13vgactx,
                                       r13mdactx,
                                       r13smoken,
                                       raedyrs,
                                       r13lbrf,
                                       rabyear,
                                       r13drinkd
                                       
                                       
                                       
))

names(subdat_r) <- c("inw13","ID", "hhid","s_weight",
                      "finace_satified",
                      "people_living_with",
                      "loneliness",
                      "gender",
                      "age",
                      "marital_status",
                      "race","hispanic",
                      "r13arthre",
                      "r13cancre",
                      "r13diabe",
                      "r13hearte",
                      "r13hibpe",
                      "r13lunge",
                      "r13psyche",
                      "r13stroke",
                      "vigor_act",
                      "moder_act",
                      "smoke_now",
                      "years_edu",
                      "lb_status",
                      "birth_year",
                      "drink_now"
                      
)

hrs_data0 <- merge(subdat_r, subdat_h, by = c("ID"))
hrs_data0$ID <- as.character(hrs_data0$ID)

# Rename 'id_hrs' to ID
# hrs_hcap <- hrs_hcap %>%
#   rename(ID = id_hrs)
hrs_factors <- hrs_factors %>%
  rename(ID = id_hrs)

# Merge the datasets by 'id_hrs'
#merged_data_hrs <- merge(hrs_data0, hrs_hcap, by = "ID")
merged_data_hrs <- merge(hrs_data0, hrs_factors, by = "ID")



# Save the modified data
write.csv(merged_data_hrs, 'hrs_data.csv')
