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


#################Example using do_ratio_multi
leps_exp <- do_ratio_est(ob_leps, ft_leps, c("sector", "year", "state"), tgt_mt, dcrb_weight)

ob_leps <- OBOrig_Proc %>% 
  clean_names() %>% 
  filter(sector %in% c("Limited Entry Trawl", "Pink Shrimp")) %>% 
  mutate(dcrb_weight = ifelse(spid_eqv == "DCRB" & catch_disposition == "D", exp_sp_wt, 0),
         state = r_state,
         tgt_mt = case_when(sector == "Pink Shrimp" ~ ps_mt,
                            sector ==  "Limited Entry Trawl" ~ gfr_mt)) 

test<-do_ratio_multi(ob_data = ob_leps, strata = c("year", "sector", "state"), tgt_mt, bycatchspp = c("DCRB", "EULC"), dis_mt)

test2<-do_ratio_multi(ob_data = ob_leps, strata = c("year", "sector", "state"), tgt_mt, bycatchspp = c("DCRB", "EULC", "YEYE"), dis_mt)

hmm1<-select(leps_exp,sector, year, state, byc_ratio, byc_ratio_lower,byc_ratio_upper) %>% 
  mutate(byc_ratio = byc_ratio*0.000453592, byc_ratio_lower = byc_ratio_lower*0.000453592, byc_ratio_upper = byc_ratio_upper*0.000453592) %>% 
  rename(byc_ratio_old = byc_ratio, byc_ratio_lower_old = byc_ratio_lower, byc_ratio_upper_old = byc_ratio_upper) %>% 
  full_join(select(filter(test,spid_eqv == "DCRB"),sector, year, state, byc_ratio, byc_ratio_lower,byc_ratio_upper)) %>% 
  mutate(r1 = byc_ratio_old/byc_ratio, r2 = byc_ratio_lower/byc_ratio_lower, r3 = byc_ratio_upper/byc_ratio_upper)

ob_test <- OBOrig_Proc %>% 
  clean_names() %>% 
  filter(sector %in% c("Catch Shares") & year >2015) %>% 
  mutate(state = r_state,
         tgt_mt = case_when(sector == "Catch Shares" ~ gfr_mt)) 

test3<-do_ratio_multi(ob_data = ob_test, strata = c("year", "sector", "gear", "state", "drvid"), expfactor = tgt_mt, bycatchspp = c("DCRB", "EULC", "YEYE"), bycatchunit = dis_mt)

#Make sure new ratios match old?
ol

