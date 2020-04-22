#This creates a data frame of bycatch ratios for selected strata, given a data frame of OB data

do_ratio_multi <- function(ob_data, strata, expfactor, bycatchspp, bycatchunit)
{
  require(dplyr)
  require(rlang)
  require(janitor)
  
  ob_data <- clean_names(ob_data)
  
  expf <- enquo(expfactor) 
  
  byc_unit <- enquo(bycatchunit)
  
  #First, summarise the expansion factor data.
  obdf_expf <- ob_data %>% 
    filter(!is.na(haul_id)) %>% #removes any trip without catch
    #Haul-level summary
    group_by_at(c(strata, "haul_id", "trip_id", "drvid")) %>% 
    summarise(expf_haul = sum(!!expf, na.rm=T)) %>% 
      #expf = sum(tgt_mt, na.rm=T)) %>% #!! unquotes the quosure
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
    tidyr::separate(all_byc_species, bycatchspp) %>% 
    tidyr::gather(spid_eqv, whatev, bycatchspp) %>% 
    select(-whatev)
  
  #Get unique combination of bycatch species, hauls, to make sure we account for hauls with 0 bycatch below
  unique_hauls_sp <- ob_data %>% 
    filter(!is.na(haul_id)) %>% #removes any trip without catch
    select(!!!syms(strata), haul_id, trip_id, drvid) %>% 
    distinct() %>% 
    #This is a hacky way of making sure each row contains strata, trip, haul, drvid, and bycatch species
    mutate(all_byc_species = paste(bycatchspp, sep="", collapse=",")) %>% 
    tidyr::separate(all_byc_species, bycatchspp) %>% 
    tidyr::gather(spid_eqv, whatev, bycatchspp) %>% 
    select(-whatev)
  
  #Get bycatch data
  obdf_byc <- ob_data %>% 
    #Haul-level summary  
    #Filter to only species of interest
    filter(spid_eqv %in% bycatchspp) %>% #
    #Joining all the unique haul/bycatch species combinations ensures that hauls with 0 catch are preserved.
    right_join(unique_hauls_sp) %>% 
    group_by_at(c(strata, "spid_eqv", "haul_id", "trip_id", "drvid")) %>% 
    summarise(byc_haul = sum(!!byc_unit, na.rm=T)) %>% 
      #byc = sum(dis_mt, na.rm = T)) %>% #!! unquotes the quosure
    ungroup() %>% 
    #Fill in 0s for strata without observed bycatch, for completeness. "Nesting" should return only strata combinations present in the data. 
    #tidyr::complete(tidyr::nesting(!!!syms(strata)), spid_eqv, fill = list(byc = 0)) %>% 
    #Summarise the observed bycatch by strata, 
    group_by_at(c(strata, "spid_eqv")) %>% 
    summarise(total_byc = sum(byc_haul),
              mean_byc = mean(byc_haul),
              se_byc = sqrt(var(byc_haul)/length(byc_haul)))
  
  
  obdf <- obdf_expf %>% 
    full_join(obdf_byc) %>% 
    ungroup() %>% 
    mutate(byc_ratio = total_byc / total_expf,
           byc_se = ratio.se(mean_byc, mean_expf, se_byc, se_expf),
           byc_ratio_lower = byc_ratio - 1.96 * byc_se, 
           byc_ratio_upper = byc_ratio + 1.96 * byc_se)
  
  return(obdf)

  
}