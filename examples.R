#Arbitrary example 1: say we want to get annual expanded Dungeness crab and stripetail rockfish bycatch estimates for the LE trawl fishery and pink shrimp fishery by management group, state, and year

library(dplyr)
library(janitor)

#Load observer and fish ticket data
source("~/observer/Input/load_data_2020-08-14.R")
load_data(c("WCGOP_proc", "FT_proc"))

#Subset the data to what we're interested in, make sure strata are compatible across OB/FT
ft_leps <- FTOrig_Proc %>% 
  clean_names() %>% 
  filter(sector %in% c("Pink Shrimp", "Limited Entry Trawl")) %>% 
  mutate(state = case_when(agency_code == "C" ~ "CA",
                           agency_code == "O" ~ "OR",
                           agency_code == "W" ~ "WA"))

ob_leps <- OBOrig_Proc %>% 
  clean_names() %>% 
  filter(sector %in% c("Limited Entry Trawl", "Pink Shrimp")) %>% 
  mutate(state = r_state) 

#Expand
leps_exp <- do_ratio_est_multi(ob_dat = ob_leps, ft_dat = ft_leps, strata = c("sector", "year", "state"), bycatchspp = c("Dungeness Crab", "Stripetail Rockfish"), expfactor = "tgt_mt", bycatchunit = "dis_mt", management_groups = TRUE)

#Arbitrary example 2: say we want all salmon bycatch ratios counts in the LE trawl fishery by year
salmon_spp <- c("Chinook (King) Salmon", "Chum (Dog) Salmon", "Coho (Silver) Salmon", "Pink (Humpback) Salmon","Sockeye (Red) Salmon", "Salmon Unid")

#Subset the data to what we're interested in, make sure strata are compatible across OB/FT
ft_le<- FTOrig_Proc %>% 
  clean_names() %>% 
  filter(sector == "Limited Entry Trawl") %>% 
  mutate(state = case_when(agency_code == "C" ~ "CA",
                           agency_code == "O" ~ "OR",
                           agency_code == "W" ~ "WA"))

ob_le <- OBOrig_Proc %>% 
  clean_names() %>% 
  filter(sector == "Limited Entry Trawl") %>% 
  mutate(state = r_state) %>% 
  mutate(dis_sal_ct = ifelse(species %in% salmon_spp & catch_disposition == "D", exp_sp_ct, 0))

#Expand
le_exp <- do_ratio_est_multi(ob_dat = ob_le, ft_dat = ft_le, strata = "year", bycatchspp = salmon_spp, expfactor = "tgt_mt", bycatchunit = "dis_sal_ct", management_groups = FALSE)

