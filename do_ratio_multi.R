#This creates a data frame of bycatch ratios and associated information for selected strata, given a data frame of WCGOP or ASHOP data

#Arguments are as follows:
#ob_dat = Data frame of observer data. This can be either ASHOP or WCGOP data. Any strata of interest not already present in the data should be added as well. 
#strata = A vector of strata to calculate the ratios over. Note that I change everything to snake case (using janitor::clean_names), so strata should be listed accordingly (e.g. strata = c("sector", "year", "r_state") NOT strata = c("sector", "YEAR", "R_STATE")
#expfactor = The name of the column containing the expansion factor (e.g. gfr_mt). Defaults to tgt_mt, assuming this has been added to the input observer data
#bycatchsp = A vector containing all the bycatch species of interest. For now, we're assuming these come from the species column. Later I want to make this flexible.
#bycatchunit = The name of the column with the measure of bycatch to be expanded. Defaults to dis_mt, but could be counts, etc. 
#management_groups = do we want to calculate ratios by managment grouping? Defaults to TRUE.


do_ratio_multi_test <- function(ob_dat = ob, strata = c("sector", "gear", "year"), expfactor = "tgt_mt", bycatchspp_col = "species", bycatchspp, bycatchunit = "dis_mt", management_groups = FALSE) #haul_id trip_id drvid
{
  require(dplyr)
  require(tidyr)
  require(rlang)
  require(janitor)
  options(dplyr.summarise.inform = FALSE)
  
  ratio.se <- function(x.mean, y.mean, x.se, y.se){ sqrt(  (x.mean/y.mean)^2 *  (  (x.se/x.mean)^2  +  (y.se/y.mean)^2  -  ( (x.se/x.mean)^2 * (y.se/y.mean)^2 ) ) ) }
  
  
  #Do we want management groupings or not? Seems easiest (but long) to do it this way,
  if(!management_groups)
  {
    #First, summarise the expansion factor data by strata.
    obdf_expf <- ob_dat %>% 
      filter(!is.na(haul_id) & 
               datatype == "Analysis Data") %>% #removes any trip without catch
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
    unique_hauls_sp <- ob_dat %>%
      filter(!is.na(haul_id)) %>% #removes any trip without catch
      dplyr::select(!!!syms(strata), haul_id, trip_id, drvid) %>%
      distinct() %>%
      #This is a hacky way of making sure each row contains strata, trip, haul, drvid, and bycatch species
      mutate(all_byc_species = paste(bycatchspp, sep="", collapse=",")) %>%
      tidyr::separate(all_byc_species, bycatchspp, sep = ",") %>%
      tidyr::gather(!!bycatchspp_col, whatev, all_of(bycatchspp)) %>%
      dplyr::select(-whatev)
    
    # #Now summarise bycatch data by strata
    obdf_byc <- ob_dat %>%
      #Haul-level summary
      #Filter to only species of interest
      filter(!!sym(bycatchspp_col) %in% bycatchspp) %>% #
      #Joining all the unique haul/bycatch species combinations ensures that hauls with 0 catch are preserved.
      right_join(unique_hauls_sp, by = c(strata, "haul_id", "trip_id", "drvid", bycatchspp_col)) %>%
      group_by_at(c(strata, bycatchspp_col, "haul_id", "trip_id", "drvid")) %>%
      summarise(byc_haul = sum(!!sym(bycatchunit), na.rm=T)) %>%
      ungroup() %>%
      #Summarise the observed bycatch by strata, including the number of hauls that encountered bycatch
      group_by_at(c(strata, bycatchspp_col)) %>%
      summarise(total_byc = sum(byc_haul),
                mean_byc = mean(byc_haul),
                se_byc = sqrt(var(byc_haul)/length(byc_haul)),
                n_hauls_byc = n_distinct(haul_id[byc_haul > 0]))
    
    
    obdf <- obdf_expf %>%
      full_join(obdf_byc, by = c(strata, bycatchspp_col)) %>%
      ungroup() %>%
      mutate(byc_ratio = total_byc / total_expf,
             byc_se = ratio.se(mean_byc, mean_expf, se_byc, se_expf),
             byc_ratio_lower = byc_ratio - 1.96 * byc_se,
             byc_ratio_upper = byc_ratio + 1.96 * byc_se,
             pct_hauls_byc = round((n_hauls_byc/n_obs_hauls)*100, 2))
  } #end if !management_groups statement
  
  
  #If we want management groups we do things slightly differently
  if(management_groups)
  {
    #First, summarise the expansion factor data by strata.
    obdf_expf <- ob_dat %>% 
      filter(!is.na(haul_id) & 
               datatype == "Analysis Data") %>% #removes any trip without catch
      #Haul-level summary
      group_by_at(c(strata, "haul_id", "trip_id", "drvid")) %>% 
      ###summarise(expf_haul = sum(!!exp_fac, na.rm=T)) %>% 
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
    
    #Get the groupings we may need to preserve (any that are present in the data and contain our species of interest)
    spp_grps <- filter(ob_dat, !!sym(bycatchspp_col) %in% bycatchspp) %>% 
      dplyr::select(!!bycatchspp_col, grouping) %>% 
      distinct()
    
    #Get unique combination of bycatch species, hauls, management groups to make sure we account for hauls with 0 bycatch below. This seemed simpler than dealing with nested tidyr::expand.
    unique_hauls_sp <- ob_dat %>% 
      filter(!is.na(haul_id)) %>% #removes any trip without catch
      dplyr::select(!!!syms(strata), haul_id, trip_id, drvid) %>% 
      distinct() %>% 
      #This is a hacky way of making sure each row contains strata, trip, haul, drvid, and bycatch species
      mutate(all_byc_species = paste(bycatchspp, sep="", collapse=",")) %>% 
      tidyr::separate(all_byc_species, bycatchspp, sep = ",") %>% 
      tidyr::gather(!!bycatchspp_col, whatev, bycatchspp) %>% 
      dplyr::select(-whatev)%>% 
      full_join(spp_grps, by = bycatchspp_col)
    
    #Now summarise bycatch data by strata
    obdf_byc <- ob_dat %>% 
      #Haul-level summary  
      #Filter to only species of interest
      filter(!!sym(bycatchspp_col) %in% bycatchspp) %>%
      #Joining all the unique haul/bycatch species combinations ensures that hauls with 0 catch are preserved.
      right_join(unique_hauls_sp,by = c(strata, "haul_id", "trip_id", "drvid", bycatchspp_col,"grouping")) %>% 
      group_by_at(c(strata, "grouping", bycatchspp_col, "haul_id", "trip_id", "drvid")) %>% 
      summarise(byc_haul = sum(!!sym(bycatchunit), na.rm=T)) %>%
      ungroup() %>% 
      #Fill in 0s for strata without observed bycatch, for completeness. "Nesting" should return only strata combinations present in the data. 
      #tidyr::complete(tidyr::nesting(!!!syms(strata)), spid_eqv, fill = list(byc = 0)) %>% 
      #Summarise the observed bycatch by strata, including number of hauls with bycatch
      group_by_at(c(strata, "grouping", bycatchspp_col)) %>% 
      summarise(total_byc = sum(byc_haul),
                mean_byc = mean(byc_haul),
                se_byc = sqrt(var(byc_haul)/length(byc_haul)),
                n_hauls_byc = n_distinct(haul_id[byc_haul > 0]))
    
    
    obdf <- obdf_expf %>% 
      full_join(obdf_byc, by = c(strata, bycatchspp_col)) %>% 
      ungroup() %>% 
      mutate(byc_ratio = total_byc / total_expf,
             byc_se = ratio.se(mean_byc, mean_expf, se_byc, se_expf),
             byc_ratio_lower = byc_ratio - 1.96 * byc_se, 
             byc_ratio_upper = byc_ratio + 1.96 * byc_se, 
             pct_hauls_byc = round((n_hauls_byc/n_obs_hauls)*100, 2))
  } #end if management_groups statement
  
  return(as.data.frame(obdf))
  
  
}
