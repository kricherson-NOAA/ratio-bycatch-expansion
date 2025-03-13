#/opt/R/64-bit/R-4.0.1/bin/R


library(tidyverse)
library(devtools)
library(janitor)

out_drive <- "Y:/Output/Richerson requests/2022_kent_tope_shark/"
if(!dir.exists(out_drive)) {dir.create(out_drive)}
tday <- gsub("-", "", Sys.Date())
# source("~/observer/Input/load_data_2022-03-24.R")
# load_data(c("WCGOP_proc", "FT_proc"))


load("Y:/Input/OB_2002-2021_Proc_2022-03-23.RDA")
load("Y:/Input/FTD_2002-2021_Proc_2022-03-02.RDA")

# 
# 
source_url("https://raw.githubusercontent.com/kricherson-NOAA/ratio-bycatch-expansion/master/do_ratio_multi.R")
# 
source_url("https://raw.githubusercontent.com/kricherson-NOAA/ratio-bycatch-expansion/master/do_ratio_est_multi.R")

ob<-OBOrig_Proc %>% clean_names()

ft <- FTOrig_Proc %>% clean_names()

tope_sectors <- filter(ob, species == "Soupfin Shark") %>% distinct(sector, gear)


tope_expanded <- do_ratio_est_multi(ob_dat = ob %>% 
                                      filter(!sector %in% c("Catch Shares", "Midwater Rockfish","Midwater Hake","Shoreside Hake") & sector %in% tope_sectors$sector) %>% 
                                      mutate(state = r_state), 
                                    ft_dat = ft %>% 
                                      filter(!sector %in% c("Catch Shares", "Midwater Rockfish","Midwater Hake","Shoreside Hake") & sector %in% tope_sectors$sector) %>% 
                                      mutate(state = case_when(agency_code == "C" ~ "CA",
                                                               agency_code == "O" ~ "OR",
                                                               agency_code == "W" ~ "WA")),
                                    strata = c("year", "gear","sector","state"), 
                                    expfactor = "tgt_mt", 
                                    bycatchspp_col = "species", 
                                    bycatchspp = c("Soupfin Shark"), 
                                    bycatchunit = "dis_mt", 
                                    management_groups = FALSE) %>% 
  arrange(year, gear)

filter(tope_expanded, total_byc>0 & !is.na(total_byc)) %>% arrange(desc(est_byc)) %>% View()
#Will need to figure out what to to with LE CHLB in 2011-2012
#Also need to fix 

#Make sure the NA's are where they should be....where we have NA for all fields except fleet_expf, this is because they weren't observed, right?
test1<-filter(tope_expanded, is.na(n_obs_ves)) %>% distinct(year, gear, sector) %>% mutate(all = paste0(year, gear, sector))
test2<-ob %>% filter(datatype == "Analysis Data" & !is.na(haul_id)) %>%  distinct(year, gear, sector) %>% mutate(all = paste0(year, gear, sector))

full_join(test1, test2, by = "all") %>% 
  filter(!is.na(year.y) & !is.na(year.x))
#I think this means we're ok....




