#Testing ratio expansion functions

##%######################################################%##
#                                                          #
####              testing do_ratio_multi.R              ####
#                                                          #
##%######################################################%##

##########Testing with CS data, groupings###################

#First, do the calculation "manually" (as I have in previous reports). Let's use stripetail rockfish as an example since it's a fairly common species. Look at bycatch ratios by gear within the CS sector and make sure they match. 

#Get CS stripetail data
ob_cs_st <- OBOrig_Proc %>%
  clean_names() %>% 
  filter(datatype == "Analysis Data" & 
           sector == "Catch Shares") %>% 
  mutate(st_north_dis = ifelse(species == "Stripetail Rockfish" & ifq_grouping == "Minor Shelf Rockfish (North of 40\xb010' N. lat.)", dis_mt, 0),
         st_south_dis = ifelse(species == "Stripetail Rockfish" & ifq_grouping == "Minor Shelf Rockfish (South of 40\xb010' N. lat.)", dis_mt, 0))

#Summarise by haul
ob_cs_st_byhaul <- ob_cs_st %>% 
  group_by(haul_id, year, trip_id, drvid, gear) %>% 
  summarise(st_north_dis = sum(st_north_dis),
            st_south_dis = sum(st_south_dis),
            gfr_mt = sum(gfr_mt, na.rm=T))

sum(is.na(ob_cs_st$haul_id)) #Double check there are no NA haul_ids (should be 0)

#Summarise haul-level retained groundfish and GS bycatch by year, state, and season
ob_cs_st_bystrata <- ob_cs_st_byhaul %>% 
  group_by(year, gear) %>% 
  summarise(total_st_north = sum(st_north_dis),
            total_st_south = sum(st_south_dis),
            mean_st_north = mean(st_north_dis),
            mean_st_south = mean(st_south_dis),
            se_st_north = sqrt(var(st_north_dis)/length(st_north_dis)),
            se_st_south = sqrt(var(st_south_dis)/length(st_south_dis)),
            total_gfr_mt = sum(gfr_mt, na.rm=T),
            mean_gfr_mt = mean(gfr_mt, na.rm=T),
            se_gfr_mt = sqrt(var(gfr_mt, na.rm=T)/length(gfr_mt)), 
            n_obs_ves = n_distinct(drvid),
            n_obs_trips = n_distinct(trip_id),
            n_obs_hauls = n_distinct(haul_id),
            byc_ratio_north = total_st_north / total_gfr_mt,
            byc_ratio_south = total_st_south / total_gfr_mt,
            byc_se_north = ratio.se(mean_st_north, mean_gfr_mt, se_st_north, se_gfr_mt),
            byc_se_south = ratio.se(mean_st_south, mean_gfr_mt, se_st_south, se_gfr_mt),
            byc_ratio_north_lower = byc_ratio_north - 1.96 * byc_se_north, 
            byc_ratio_north_upper = byc_ratio_north + 1.96 * byc_se_north,
            byc_ratio_south_lower = byc_ratio_south - 1.96 * byc_se_south, 
            byc_ratio_south_upper = byc_ratio_south + 1.96 * byc_se_south) %>% 
  as.data.frame() %>% 
  arrange(year, gear)

#Now use the function
ob_cs_st_bystrata2 <- do_ratio_multi(ob_dat = ob_cs_st, strata = c("year", "gear"), expfactor = "gfr_mt", bycatchspp = "Stripetail Rockfish", bycatchunit = "dis_mt", management_groups = TRUE) %>% 
  arrange(year, gear)

#Do the north group bycatch ratios match? Yes.
all.equal(ob_cs_st_bystrata$byc_ratio_north, filter(ob_cs_st_bystrata2, grouping == "Minor shelf rockfish (North of 40<b0>10' N. lat.)")$byc_ratio)

#Do the south group bycatch ratios match? Yes.
all.equal(ob_cs_st_bystrata$byc_ratio_south, filter(ob_cs_st_bystrata2, grouping == "Minor shelf rockfish (South of 40<b0>10' N. lat.)")$byc_ratio)

#Do the north ratio SEs match? Yes.
all.equal(ob_cs_st_bystrata$byc_se_north, filter(ob_cs_st_bystrata2, grouping == "Minor shelf rockfish (North of 40<b0>10' N. lat.)")$byc_se)

#Do the south ratio SEs match? Yes.
all.equal(ob_cs_st_bystrata$byc_se_south, filter(ob_cs_st_bystrata2, grouping == "Minor shelf rockfish (South of 40<b0>10' N. lat.)")$byc_se)

#Do the number of observed vessels match? Yes.
all.equal(ob_cs_st_bystrata$n_obs_ves, filter(ob_cs_st_bystrata2, grouping == "Minor shelf rockfish (North of 40<b0>10' N. lat.)")$n_obs_ves)

#Do the observed trips match?  Yes. 
all.equal(ob_cs_st_bystrata$n_obs_trips, filter(ob_cs_st_bystrata2, grouping == "Minor shelf rockfish (South of 40<b0>10' N. lat.)")$n_obs_trips)
all.equal(ob_cs_st_bystrata$n_obs_trips, filter(ob_cs_st_bystrata2, grouping == "Minor shelf rockfish (North of 40<b0>10' N. lat.)")$n_obs_trips)

#Do the observed hauls match? Yes. 
all.equal(ob_cs_st_bystrata$n_obs_hauls, filter(ob_cs_st_bystrata2, grouping == "Minor shelf rockfish (South of 40<b0>10' N. lat.)")$n_obs_hauls)
all.equal(ob_cs_st_bystrata$n_obs_hauls, filter(ob_cs_st_bystrata2, grouping == "Minor shelf rockfish (North of 40<b0>10' N. lat.)")$n_obs_hauls)

##########Testing with NCS data, no groupings, multiple species##################

#Let's look at sablefish, yelloweye discard in several NCS sectors (to capture more common, less common species)

#First, use the old manual method
#Summarise by haul. Doing this slightly differently from above to be more efficient (basically repeat the data for each discard species so that it can be a grouping variable)
ob_ncs_yeye <- OBOrig_Proc %>%
  clean_names() %>% 
  filter(datatype == "Analysis Data" & 
           sector %in% c("Nearshore", "Limited Entry Sablefish", "OA Fixed Gear")) %>% 
  mutate(my_dis = ifelse(species == "Yelloweye Rockfish", dis_mt, 0),
         my_species = "Yelloweye Rockfish")

ob_ncs_sabl <- OBOrig_Proc %>%
  clean_names() %>% 
  filter(datatype == "Analysis Data" & 
           sector %in% c("Nearshore", "Limited Entry Sablefish", "OA Fixed Gear")) %>% 
  mutate(my_dis = ifelse(species == "Sablefish", dis_mt, 0),
         my_species = "Sablefish")

ob_ncs <- bind_rows(ob_ncs_yeye, ob_ncs_sabl)


#Summarise by haul.
ob_ncs_byhaul<- ob_ncs %>% 
  group_by(sector, my_species, haul_id, year, trip_id, drvid, gear) %>% 
  summarise(my_dis = sum(my_dis),
            tgt_mt = sum(tgt_mt, na.rm=T))

sum(is.na(ob_ncs_byhaul$haul_id)) #Double check there are no NA haul_ids (should be 0)

#Summarise haul-level retained groundfish and GS bycatch by year, state, and season
ncs_bystrata <- ob_ncs_byhaul %>% 
  group_by(sector, my_species, year, gear) %>% 
  summarise(total_byc = sum(my_dis),
            mean_byc = mean(my_dis),
            se_byc = sqrt(var(my_dis)/length(my_dis)),
            total_expf = sum(tgt_mt, na.rm=T),
            mean_expf = mean(tgt_mt, na.rm=T),
            se_expf = sqrt(var(tgt_mt, na.rm=T)/length(tgt_mt)), 
            n_obs_ves = n_distinct(drvid),
            n_obs_trips = n_distinct(trip_id),
            n_obs_hauls = n_distinct(haul_id),
            byc_ratio = total_byc / total_expf,
            byc_se = ratio.se(mean_byc, mean_expf, se_byc, se_expf),
            byc_ratio_lower = byc_ratio - 1.96 * byc_se, 
            byc_ratio_upper = byc_ratio + 1.96 * byc_se,
            n_hauls_byc = n_distinct(haul_id[my_dis > 0]),
            pct_hauls_byc = round((n_hauls_byc/n_obs_hauls)*100, 2)) %>% 
  as.data.frame() %>%
  rename(species = my_species) %>% 
  arrange(species, sector, year, gear)

#Repeat with function

ncs_bystrata2 <- do_ratio_multi(ob_dat = OBOrig_Proc %>%
                                     clean_names() %>% 
                                     filter(datatype == "Analysis Data" & 
                                              sector %in% c("Nearshore", "Limited Entry Sablefish", "OA Fixed Gear")), 
                                   strata = c("sector", "year", "gear"), 
                                   expfactor = "tgt_mt", 
                                   bycatchspp = c("Yelloweye Rockfish", "Sablefish"), 
                                   bycatchunit = "dis_mt", 
                                   management_groups = FALSE) %>% 
  as.data.frame() %>% 
  arrange(species, sector, year, gear) %>% 
  select(sector,species, year, gear, total_byc, mean_byc, se_byc, total_expf, mean_expf, se_expf, n_obs_ves, n_obs_trips, n_obs_hauls, byc_ratio, byc_se, byc_ratio_lower, byc_ratio_upper, n_hauls_byc, pct_hauls_byc)

#All the same? Yes
all.equal(ncs_bystrata, ncs_bystrata2)

##%######################################################%##
#                                                          #
####             Testing do_ratio_est.multi             ####
#                                                          #
##%######################################################%##

####Testing with NCS, groupings############################

#Let's look at expanded stripetail estimates in the pink shrimp and LE trawl fishery

#Summarise by haul. Doing this slightly differently from first stripetail test to be more efficient (basically repeat the data for each discard species/group so that it can be a grouping variable)
ob_ncs_stn <- OBOrig_Proc %>%
  clean_names() %>% 
  filter(datatype == "Analysis Data" & 
           sector %in% c("Pink Shrimp", "Limited Entry Trawl")) %>% 
  mutate(my_dis = ifelse(species == "Stripetail Rockfish" & grouping == "Minor shelf rockfish (North of 40\xb010' N. lat.)", dis_mt, 0),
         my_species = "Stripetail Rockfish N")

ob_ncs_sts <- OBOrig_Proc %>%
  clean_names() %>% 
  filter(datatype == "Analysis Data" & 
            sector %in% c("Pink Shrimp", "Limited Entry Trawl")) %>% 
  mutate(my_dis = ifelse(species == "Stripetail Rockfish" & grouping == "Minor shelf rockfish (South of 40\xb010' N. lat.)", dis_mt, 0),
         my_species = "Stripetail Rockfish S")

ob_ncs_st <- bind_rows(ob_ncs_stn, ob_ncs_sts)

#Summarise by haul.
ob_ncs_st_byhaul<- ob_ncs_st %>% 
  group_by(sector, my_species, haul_id, year, trip_id, drvid, gear) %>% 
  summarise(my_dis = sum(my_dis),
            tgt_mt = sum(tgt_mt, na.rm=T))

sum(is.na(ob_ncs_st_byhaul$haul_id)) #Double check there are no NA haul_ids (should be 0)

#Summarise haul-level retained groundfish and GS bycatch by year, state, and season
ncs_st_bystrata <- ob_ncs_st_byhaul %>% 
  group_by(sector, my_species, year, gear) %>% 
  summarise(total_byc = sum(my_dis),
            mean_byc = mean(my_dis),
            se_byc = sqrt(var(my_dis)/length(my_dis)),
            total_expf = sum(tgt_mt, na.rm=T),
            mean_expf = mean(tgt_mt, na.rm=T),
            se_expf = sqrt(var(tgt_mt, na.rm=T)/length(tgt_mt)), 
            n_obs_ves = n_distinct(drvid),
            n_obs_trips = n_distinct(trip_id),
            n_obs_hauls = n_distinct(haul_id),
            byc_ratio = total_byc / total_expf,
            byc_se = ratio.se(mean_byc, mean_expf, se_byc, se_expf),
            byc_ratio_lower = byc_ratio - 1.96 * byc_se, 
            byc_ratio_upper = byc_ratio + 1.96 * byc_se,
            n_hauls_byc = n_distinct(haul_id[my_dis > 0]),
            pct_hauls_byc = round((n_hauls_byc/n_obs_hauls)*100, 2)) %>% 
  as.data.frame() %>%
  mutate(species = "Stripetail Rockfish",
         grouping = ifelse(my_species == "Stripetail Rockfish N", 
                           "Minor shelf rockfish (North of 40\xb010' N. lat.)", 
                           "Minor shelf rockfish (South of 40\xb010' N. lat.)")) %>%
  dplyr::select(-my_species) %>% 
  arrange(species, grouping, sector, year, gear)

#Calculate total FT expansion factor
ft_ncs_st_bystrata <- FTOrig_Proc %>% 
  clean_names() %>% 
  filter(sector %in% c("Pink Shrimp", "Limited Entry Trawl")) %>% 
  group_by(sector, year, gear) %>% 
  summarise(fleet_expf = sum(tgt_mt, na.rm=T))

ncs_st_exp <- ncs_st_bystrata %>% 
  full_join(ft_ncs_st_bystrata, by = c("sector", "year", "gear")) %>% 
  mutate(pct_cvg = round((total_expf / fleet_expf) * 100, 2),
         est_byc = byc_ratio * fleet_expf,
         est_byc_lower = byc_ratio_lower * fleet_expf,
         est_byc_upper = byc_ratio_upper * fleet_expf) %>% 
  select(sector, species, grouping, year, gear, total_byc, mean_byc, se_byc, total_expf, mean_expf, se_expf, n_obs_ves, n_obs_trips, n_obs_hauls, byc_ratio, byc_se, byc_ratio_lower, byc_ratio_upper, n_hauls_byc, pct_hauls_byc, pct_cvg, est_byc, est_byc_lower, est_byc_upper) %>% 
  arrange(species, grouping, sector, year, gear)
  


ncs_st_exp2 <- do_ratio_est_multi(ob_dat = OBOrig_Proc %>%
                                  clean_names() %>% 
                                  filter(datatype == "Analysis Data" & 
                                           sector %in% c("Pink Shrimp", "Limited Entry Trawl")), 
                                  ft_dat = FTOrig_Proc %>% 
                                    clean_names() %>% 
                                    filter(sector %in% c("Pink Shrimp", "Limited Entry Trawl")),
                                strata = c("sector", "year", "gear"), 
                                expfactor = "tgt_mt", 
                                bycatchspp = c("Stripetail Rockfish"), 
                                bycatchunit = "dis_mt", 
                                management_groups = TRUE) %>% 
  as.data.frame() %>% 
  arrange(species, grouping, sector, year, gear) %>% 
  select(sector, species, grouping, year, gear, total_byc, mean_byc, se_byc, total_expf, mean_expf, se_expf, n_obs_ves, n_obs_trips, n_obs_hauls, byc_ratio, byc_se, byc_ratio_lower, byc_ratio_upper, n_hauls_byc, pct_hauls_byc, pct_cvg, est_byc, est_byc_lower, est_byc_upper)

#Do they match? Yes
all.equal(ncs_st_exp, ncs_st_exp2)

##%######################################################%##
#                                                          #
####               Testing do_boot_multi                ####
#                                                          #
##%######################################################%##

#Comparing bootstrap ratio estimates using LE trawl GSTG as an example (manual version taken from 2017 GSTG report, except skipping season strata)

library(boot)
library(purrr)

#First, use the old manual method

ob <- OBOrig_Proc %>% 
  clean_names() %>% 
  mutate(gstg_count = ceiling(ifelse((spid_eqv == "GSTG" | species == "Green Sturgeon") & catch_disposition == "D",
                                     exp_sp_ct, 0)), #Add column for expanded discard count. Rounding up following YWL.
         season = ifelse(rmonth %in% 5:10,
                         "summer", "winter"),
         state= r_state)  #Add column for season, following YWL
#We check for retained GSTG below

ft <- FTOrig_Proc %>% 
  clean_names() %>% 
  mutate(state = case_when(agency_code == "C" ~ "CA",
                           agency_code == "O" ~ "OR",
                           agency_code == "W" ~ "WA"), 
         season = ifelse(landing_month %in% 5:10,
                         "summer", "winter")) #Add column for season, followingt YWL

# set.seed(42)
# 
# test<-do_boot_multi(ob_dat = filter(ob, sector == "Limited Entry Trawl"), ft_dat = filter(ft, sector == "Limited Entry Trawl"), strata = c("year", "state", "season"), expfactor = "tgt_mt", bycatchspp = "Green Sturgeon", bycatchunit = "gstg_count") %>% 
#   arrange(year, state, season)

####################################################################################################################################################
#Use old method

set.seed(42)

ob_le <- ob %>% 
  filter(datatype == "Analysis Data" & 
           sector == "Limited Entry Trawl") #Note that following YWL methods we don't limit this to bottom trawl gear

#Summarise by haul
ob_le_byhaul <- ob_le %>% 
  group_by(haul_id, year, r_state, season, trip_id, drvid) %>% 
  summarise(gstg_count = sum(gstg_count),
            gfr_mt = sum(gfr_mt, na.rm=T))

sum(is.na(ob_le$haul_id)) #Double check there are no NA haul_ids (should be 0)

#Summarise haul-level retained groundfish and GS bycatch by year, state, and season
ob_le_byseason <- ob_le_byhaul %>% 
  group_by(year, state=r_state, season) %>% 
  summarise(total_gstg_count = sum(gstg_count, na.rm=T),
            mean_gstg_count = mean(gstg_count, na.rm=T),
            se_gstg_count = sqrt(var(gstg_count, na.rm=T)/length(gstg_count)), #matches YWL
            total_gfr_mt = sum(gfr_mt, na.rm=T),
            mean_gfr_mt = mean(gfr_mt, na.rm=T),
            se_gfr_mt = sqrt(var(gfr_mt, na.rm=T)/length(gfr_mt)), #Matches YWL
            n_obs_ves = n_distinct(drvid),
            n_obs_trips = n_distinct(trip_id), #matchs YWL
            n_obs_hauls = n_distinct(haul_id)) 

#Summarise fts for this sector by year, state, and season  
ft_le<-filter(ft,sector=="Limited Entry Trawl") #

ob_le_fts <- unlist(strsplit(filter(ob_le, fish_tickets != "")$fish_tickets, ";"))

#What proportion of OBSERVED fts are in the le_ft data?
sum(unique(ob_le_fts)%in%ft_le$ftid)/length(unique(ob_le_fts))
# 0.9835076

#IF we want to add (some of) the missing fish tickets into our LE Trawl totals, set add_missing_le_tix=TRUE (note then will be slightly off from numbers generated by YWL code)
add_missing_le_tix <- FALSE

if(add_missing_le_tix == TRUE){
  
  missing_le_fts<-unique(ob_le_fts[!ob_le_fts%in%ft_le$ftid]) #FTIDs that are not in ft_le
  
  le_fts_to_add <- ob_le %>% 
    filter(grepl(paste(missing_le_fts,collapse="|"), fish_tickets)) %>% #find ob entries without corresponding LE FTs
    mutate(ft1 = substr(fish_tickets, 1, 7), #Put ob FTIDs in their own columns
           ft2 = ifelse(nchar(fish_tickets) > 7,
                        substr(fish_tickets, 9, 15), NA),
           ft3 = ifelse(nchar(fish_tickets) > 15,
                        substr(fish_tickets, 17, 23), NA),
           ft4 = ifelse(nchar(fish_tickets) > 23,
                        substr(fish_tickets, 25, 31), NA)) %>% 
    gather(ft_seq_no, ftid, ft1:ft4) %>% #Put all FTIDs into one column
    filter(!is.na(ftid)) %>% 
    select(ftid, year, rmonth, rday) %>% #Let's say we 
    rename(landing_month = rmonth, landing_day = rday) %>% #For ease of joining
    distinct() %>% 
    left_join(ft,by=c("ftid", "year", "landing_month", "landing_day")) %>%  #FTIDs MAY be repteated, but assume we've got the right one if all these things match
    filter(ftid %in% missing_le_fts) %>%  #Some LE Trawl FTIDs may have snuck in with missing FTIDSrange()
    filter(!is.na(agency_code)) #There are still ~50 that don't have apparent matches, so this just removes those. 
  
  length(unique(le_fts_to_add))/length(missing_le_fts) #What proportion of missing tix did we account for? Only ~60%
  
}else{
  le_fts_to_add<-NULL
}

ft_byseason <- ft %>% 
  filter(sector == "Limited Entry Trawl") %>%
  bind_rows(le_fts_to_add) %>% #Add in missing FTs (if this DF was created above)
  group_by(year, state, season) %>%
  summarise(total_ft_gfr_mt = sum(gfr_mt),
            mean_ft_gfr_mt = mean(gfr_mt),
            se_ft_gfr_mt = sqrt(var(gfr_mt)/length(gfr_mt)))

#Combine OB and FT summaries and calculate coverage, bycatch ratio, etc
le_byseason <- ob_le_byseason %>% 
  full_join(ft_byseason, by=c("year", "state", "season")) %>% 
  mutate(pct_cvg = (total_gfr_mt / total_ft_gfr_mt) * 100, 
         byc_ratio = total_gstg_count / total_gfr_mt,
         byc_se = ratio.se(mean_gstg_count, mean_gfr_mt, se_gstg_count, se_gfr_mt),
         byc_ratio_lower = byc_ratio - 1.96 * byc_se, #Not sure if we will use these but keeping to check against YWL
         byc_ratio_upper = byc_ratio + 1.96 * byc_se,
         expand_gstg_ct = byc_ratio * total_ft_gfr_mt) %>% 
  arrange(season, year, state)

nrow(le_byseason)==nrow(ob_le_byseason) #should be equal

########Bootstrap confidence intervals###############
#Get OB data by vessel
ob_le_byvessel <- ob_le %>% 
  group_by(year, state=r_state, season, drvid) %>% 
  summarise(gstg_count = sum(gstg_count),
            gfr_mt = sum(gfr_mt, na.rm=T)) %>% 
  arrange(year, state, season, drvid)

#Get FT data by vessel
ft_le_byvessel <- ft_le %>%
  bind_rows(le_fts_to_add) %>% #Add in missing FTs (if this DF was created above)
  group_by(year, state, season, drvid) %>% 
  summarise(gfr_ft_mt = sum(gfr_mt, na.rm=T)) %>% 
  group_by(year, state, season) %>% 
  mutate(total_ft_gfr_mt = sum(gfr_ft_mt)) %>% 
  arrange(year, state, season, drvid)

#This function feeds into boot::boot as the statistic argument. It takes a data frame of vessel-level data and calculates the bycatch ratio according to the vessels sampled by the bootstrapping routine
byc_ratio_fun <- function(vdata, indices){ #vdata is the vessel-level data, indices will be the rows sampled by the boot function
  
  sum(vdata$gstg_count[indices]) / sum(vdata$gfr_mt[indices])
  
}

le_booted <- ob_le_byvessel %>% 
  left_join(ft_le_byvessel, by=c("year", "state", "season", "drvid")) %>% 
  filter(gfr_mt>0) %>% #So that we don't end up dividing by 0 -- there are two vessel/season/state combos with 0 retained groundfish (and 0 gstg)
  filter(!is.na(gfr_ft_mt)) %>% #Because of missing LE FTs, there are some NAs. Ignore these few cases (3) where a vessel apparently had observed landings, but no FT landings. There could be a better way of doing things?
  group_by(year,state,season) %>% 
  nest() %>% #This creates a row for each year/state/season, with a data column that contains a df with all the data for that year/state/season
  mutate(booted = map(.x = data, # The list-column with the vessel-level data
                      ~ boot(data = .x, # The <S3 tibble> being sampled
                             statistic = byc_ratio_fun, # The user-defined function
                             R = 10000, # The number of replicates
                             stype = "i"))) %>% 
  rowwise() %>% #"rowwise() is used for the results of do() when you create list-variables." (presumably similar for map()-created list variables)
  mutate(point_est_ratio = booted$t0,  #Get point estimate, median, lower 2.5th percentile, upper 97.5th percentile of bootstrapped ratios
         median_ratio = median(booted$t),
         lower_ci = quantile(booted$t, 0.025),
         upper_ci = quantile(booted$t, 0.975)) %>% 
  # Drop the list-columns (no longer needed)
  dplyr::select(-data, -booted) %>%
  # Unnest the dataframe
  tidyr::unnest()

le_booted_gstg<-le_byseason %>% 
  left_join(le_booted, by=c("year", "state", "season")) %>% 
  dplyr::select(state, year, season, total_gstg_count, total_gfr_mt, total_ft_gfr_mt, pct_cvg, byc_ratio, lower_ci, upper_ci, expand_gstg_ct, n_obs_ves) %>%
  mutate(lower_gstg_ct = lower_ci * total_ft_gfr_mt,
         upper_gstg_ct = upper_ci * total_ft_gfr_mt) %>% 
  arrange(year, state, season) %>% 
  mutate_at(4:14, funs(round(., 6))) %>% 
  mutate(pct_cvg = round((total_gfr_mt / total_ft_gfr_mt) * 100, 2))

#now use function

le_booted_gstg2 <- do_boot_multi(ob_dat = ob_le, ft_dat = ft_le, strata = c("year", "state", "season"), expfactor = "tgt_mt", bycatchspp = "Green Sturgeon", bycatchunit = "gstg_count", seed =42) %>% 
  arrange(year, state, season) %>% 
  dplyr::select(state, year, season, total_byc, total_expf, fleet_expf, pct_cvg, byc_ratio, lower_ci, upper_ci, est_byc, n_obs_ves, est_byc_lower, est_byc_upper) %>%
  ungroup() %>% 
  mutate(year  = as.numeric(year)) %>% 
  mutate_at(4:14, funs(round(., 6))) 

#compare
names(le_booted_gstg2) <- names(le_booted_gstg)

# all match (though depending on exactly how this gets implemented, it's not suprising to see small differences due to randomness of bootstrap. Seed must be set the same, dfs ordered the same, etc)
all.equal(le_booted_gstg, le_booted_gstg2) 
