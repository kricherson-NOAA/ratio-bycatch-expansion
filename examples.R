#Arbitrary example 1: say we want to get annual expanded Dungeness crab bycatch estimates for the LE trawl fishery and pink shrimp fishery by state

library(dplyr)
library(janitor)

source("~/observer/Input/load_data_2019-05-31.R") #


load_data(c("WCGOP","FT"))

#subset the data to what we're interested in, make sure strata are compatible across OB/FT, add column for target MT (not needed if not using different denominators/expansion factors across strata)
ft_leps <- FTOrig_Proc %>% 
  clean_names() %>% 
  filter(sector %in% c("Pink Shrimp", "Limited Entry Trawl")) %>% 
  mutate(state = case_when(agency_code == "C" ~ "CA",
                           agency_code == "O" ~ "OR",
                           agency_code == "W" ~ "WA"),
         tgt_mt = case_when(sector == "Pink Shrimp" ~ ps_mt,
                            sector ==  "Limited Entry Trawl" ~ gfr_mt))

ob_leps <- OBOrig_Proc %>% 
  clean_names() %>% 
  filter(sector %in% c("Limited Entry Trawl", "Pink Shrimp")) %>% 
  mutate(dcrb_weight = ifelse(spid_eqv == "DCRB" & catch_disposition == "D", exp_sp_wt, 0),
         state = r_state,
         tgt_mt = case_when(sector == "Pink Shrimp" ~ ps_mt,
                            sector ==  "Limited Entry Trawl" ~ gfr_mt)) 
#Expand
leps_exp <- do_ratio_est(ob_leps, ft_leps, c("sector", "year", "state"), tgt_mt, dcrb_weight)


