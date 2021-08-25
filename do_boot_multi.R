#This function creates ratio estimates of bycatch with bootstrapped confidence intervals

#For now, it does not have an option to include management groups
#arguments: 
# ob_dat (object name) is observer data
# ft_dat (object name) is fish ticket data
# strata (character vector) are the reporting strata 
# vessel_strata (character vector) are the strata that vessels should be aggregated at for bootstrapping. May be different from strata if pooling across strata for bootstrapping
# expfactor (Character) is the name of the column with the expansion factor
# bycatchspp_col is the name of the column with the bycatch species of interest
# bycatchspp (character vector) are the bycatch species of interest
# bycatchunit (character) is the name of the column with the units of bycatch
# seed (number) is the randomization seed

do_boot_multi <- function(ob_dat, ft_dat, strata, vessel_strata = strata, expfactor = "tgt_mt", bycatchspp_col = species, bycatchspp, bycatchunit = "dis_mt", seed = 42)
{
  require(dplyr)
  require(tidyr)
  require(rlang)
  require(janitor)
  require(boot)
  
  set.seed(seed)
  
  bycatchspp_col <- enquo(bycatchspp_col)
  bycatchspp_string <- gsub("~","", deparse(bycatchspp_col)) #make into string for grouping 
  
  #Identify whether the input data is ASHOP or WCGOP (won't ever be bootstrapping ASHOP but leaving this in for now)
  if(any(ob_dat$sector %in% c("CATCHER-PROCESSOR", "MOTHERSHIP", "ATSEA TRIBAL"))){program <- "ASHOP"} else {
    program <- "WCGOP"
  }
  
  #Do a little bit of data prep. Maybe would be better NOT to have clean_names in here?
  if(program == "WCGOP")
  {
    ob_data <- clean_names(ob_dat) %>% 
      filter(datatype == "Analysis Data")
  }
  
  #For ASHOP, we put in NAs for trip_id and just remove later. 
  if(program == "ASHOP")
  {
    ob_data <- clean_names(ob_dat) %>% 
      mutate(tgt_mt = pwht_mt) %>% 
      mutate(trip_id = NA)
  }
  
  #coverage and ratio
  #First, summarise the expansion factor data by strata.
  obdf_expf <- ob_data %>% 
    filter(!is.na(haul_id)) %>% #removes any trip without catch
    #Haul-level summary
    group_by_at(c(strata, "haul_id", "trip_id", "drvid")) %>% 
    summarise(expf_haul = sum(!!sym(expfactor), na.rm=T)) %>%
    ungroup() %>%
    #Get the expansion factor by strata
    group_by_at(strata) %>% 
    summarise(total_expf = sum(expf_haul, na.rm=T),
              mean_expf = mean(expf_haul, na.rm=T),
              se_expf = sqrt(var(expf_haul, na.rm=T)/length(expf_haul)),
              n_obs_ves = n_distinct(drvid),
              n_obs_trips = n_distinct(trip_id), 
              n_obs_hauls = n_distinct(haul_id)) %>% 
    #Now cobble together a column that contains the bycatch species so that we can join bycatch info without losing any strata with 0 bycatch (maybe not needed? but for completeness...). Clumsy but ok I guess.
    mutate(all_byc_species = paste(bycatchspp, sep="", collapse=",")) %>% 
    tidyr::separate(all_byc_species, bycatchspp, sep = ",") %>% 
    tidyr::gather(!!bycatchspp_col, whatev, all_of(bycatchspp)) %>% 
    dplyr::select(-whatev)
  
  
  #Get unique combination of bycatch species, hauls, to make sure we account for hauls with 0 bycatch below. This seemed simpler than dealing with nested tidyr::expand.
  unique_hauls_sp <- ob_data %>% 
    filter(!is.na(haul_id)) %>% #removes any trip without catch
    dplyr::select(!!!syms(strata), haul_id, trip_id, drvid) %>% 
    distinct() %>% 
    #This is a hacky way of making sure each row contains strata, trip, haul, drvid, and bycatch species
    mutate(all_byc_species = paste(bycatchspp, sep="", collapse=",")) %>% 
    tidyr::separate(all_byc_species, bycatchspp, sep = ",") %>% 
    tidyr::gather(!!bycatchspp_col, whatev, all_of(bycatchspp)) %>% 
    dplyr::select(-whatev)
  
  #Now summarise bycatch data by strata
  obdf_byc <- ob_data %>% 
    #Haul-level summary  
    #Filter to only species of interest
    filter(!!bycatchspp_col %in% bycatchspp) %>% #
    #Joining all the unique haul/bycatch species combinations ensures that hauls with 0 catch are preserved.
    right_join(unique_hauls_sp) %>% 
    group_by_at(c(strata, bycatchspp_string, "haul_id", "trip_id", "drvid")) %>% 
    summarise(byc_haul = sum(!!sym(bycatchunit), na.rm=T)) %>% 
    ungroup() %>% 
    #Summarise the observed bycatch by strata, including the number of hauls that encountered bycatch
    group_by_at(c(strata, bycatchspp_string)) %>% 
    summarise(total_byc = sum(byc_haul),
              mean_byc = mean(byc_haul),
              se_byc = sqrt(var(byc_haul)/length(byc_haul)),
              n_hauls_byc = n_distinct(haul_id[byc_haul > 0]))
  
  obdf <- obdf_expf %>% 
    full_join(obdf_byc) %>% 
    ungroup() %>% 
    mutate(byc_ratio = total_byc / total_expf,
           #byc_se = ratio.se(mean_byc, mean_expf, se_byc, se_expf),
           #byc_ratio_lower = byc_ratio - 1.96 * byc_se, 
           #byc_ratio_upper = byc_ratio + 1.96 * byc_se,
           pct_hauls_byc = round((n_hauls_byc/n_obs_hauls)*100, 2))
  
  #Bootstrap portion
  ob_byvessel_expf <- ob_data %>% 
    group_by_at(c(vessel_strata, "drvid")) %>% 
    summarise(ves_expf = sum(!!sym(expfactor), na.rm=T)) %>% 
    #Now cobble together a column that contains the bycatch species so that we can join bycatch info without losing any strata with 0 bycatch (maybe not needed? but for completeness...). Clumsy but ok I guess.
    mutate(all_byc_species = paste(bycatchspp, sep="", collapse=",")) %>% 
    tidyr::separate(all_byc_species, bycatchspp, sep = ",") %>% 
    tidyr::gather(!!bycatchspp_col, whatev, all_of(bycatchspp)) %>% 
    dplyr::select(-whatev)
  
  ob_byvessel_byc <- ob_data %>% 
    filter(!!bycatchspp_col %in% bycatchspp) %>% 
    group_by_at(c(vessel_strata, "drvid", bycatchspp_string)) %>% 
    summarise(ves_byc = sum(!!sym(bycatchunit), na.rm=T))
  
  ob_byvessel <- full_join(ob_byvessel_expf, ob_byvessel_byc, by = c(vessel_strata, "drvid", bycatchspp_string)) %>% 
    mutate(ves_byc = ifelse(is.na(ves_byc), 0, ves_byc))
  
  
  #This function feeds into boot::boot as the statistic argument. It takes a data frame of vessel-level data and calculates the bycatch ratio according to the vessels sampled by the bootstrapping routine
  byc_ratio_fun <- function(vdata, indices){ #vdata is the vessel-level data, indices will be the rows sampled by the boot function
    
    sum(vdata$ves_byc[indices]) / sum(vdata$ves_expf[indices])
    
  }
  
  booted <- ob_byvessel %>% 
    filter(ves_expf>0) %>% #So that we don't end up dividing by 0 -- there are two vessel/season/state combos with 0 retained groundfish (and 0 gstg)
    group_by_at(c(strata, bycatchspp_string))%>% 
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
    ungroup()
  
  #create expansion factors data frame
  exp_factors <- ft_dat %>% 
    mutate(r_state = state) %>% 
    group_by_at(strata) %>% 
    summarise(fleet_expf = sum(!!sym(expfactor), na.rm=T))
  
  out <- exp_factors %>% 
    full_join(booted, by = strata) %>% 
    full_join(obdf_expf, by = c(strata, bycatchspp_string)) %>% 
    full_join(obdf_byc, by = c(strata, bycatchspp_string)) %>% 
    mutate(pct_cvg = round((total_expf / fleet_expf) * 100, 2),
           pct_hauls_byc = round((n_hauls_byc/n_obs_hauls)*100, 2),
           byc_ratio = total_byc / total_expf,
           est_byc = byc_ratio * fleet_expf,
           est_byc_lower = lower_ci* fleet_expf,
           est_byc_lower_trunc = ifelse(est_byc_lower < total_byc, total_byc, est_byc_lower), #Truncate at observed value
           est_byc_upper = upper_ci * fleet_expf,
    ) %>% 
    ungroup()
  
  return(out) 
  
}