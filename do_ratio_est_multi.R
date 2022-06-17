#This creates a data frame of bycatch ratios, expanded bycatch estimates, and associated information for selected strata, given a data frame of WCGOP  data

#Arguments are as follows:
#ob_dat = Data frame of WCGOP observer data. Any strata of interest not already present in the data should be added as well. 
#strata = A vector of strata to calculate the ratios over. Note that I change everything to snake case (using janitor::clean_names), so strata should be listed accordingly (e.g. strata = c("sector", "year", "r_state") NOT strata = c("sector", "YEAR", "R_STATE")
#expfactor = The name of the column containing the expansion factor (e.g. gfr_mt). Defaults to tgt_mt, assuming this has been added to the input observer data
#bycatchsp = A vector containing all the bycatch species of interest.
#bycatchunit = The name of the column with the measure of bycatch to be expanded. Defaults to dis_mt, but could be counts, etc. 
#management_groups = do we want to calculate ratios by managment grouping? Defaults to TRUE.
#cleanup = do we want to do some post-processing where we round values, replace NAs with 0s, replace lower CI with observed bycatch, etc

do_ratio_est_multi <- function(ob_dat, ft_dat, strata, expfactor, bycatchspp_col, bycatchspp, bycatchunit, management_groups = TRUE, cleanup = TRUE)
{
  require(dplyr)
  require(tidyr)
  require(rlang)
  require(janitor)
  
  
  #Create data frame of ratios using the do_raio_multi function
  byc_ratios <- do_ratio_multi(ob_dat = ob_dat, strata = strata, expfactor = expfactor, bycatchspp_col = bycatchspp_col, bycatchspp = bycatchspp, bycatchunit = bycatchunit, management_groups = management_groups)
  
  #create expansion factors data frame
  exp_factors <- ft_dat %>% 
    clean_names() %>% 
    #mutate(r_state = state) %>% 
    group_by_at(strata) %>% 
    summarise(fleet_expf = sum(!!sym(expfactor), na.rm=T)) %>% 
    rowid_to_column()
  
  #In order to have a row for each species-strata combination when there is 0 coverage of a strata (ie there are fish tickets in the strata, but no observations), I'm going to duplicate the exp_factors data 1x for each species
  #TODO: add groupings option here
  exp_factors_spp_key <- expand.grid(bycatchspp, exp_factors$rowid)
  names(exp_factors_spp_key) <- c(bycatchspp_col, "rowid")

  
  exp_factors_long <- exp_factors_spp_key %>% 
    full_join(exp_factors, by = c("rowid"))
  
  out <- exp_factors_long %>% 
    full_join(byc_ratios, by = c(strata, bycatchspp_col)) %>% 
    mutate(pct_cvg = round((total_expf / fleet_expf) * 100, 2),
           est_byc = byc_ratio * fleet_expf,
           est_byc_lower = byc_ratio_lower * fleet_expf,
           est_byc_upper = byc_ratio_upper * fleet_expf)
  
  if(management_groups)
  {
    out <- out %>% 
      mutate(grouping = gsub("<b0>", "\xb0", grouping))
  }
  
  if(cleanup)
  {
    if(management_groups)
    {
      out <- out %>% 
        select(!!sym(bycatchspp_col), grouping, !!!syms(strata), obs_byc = total_byc, obs_expf = total_expf, fleet_expf, pct_cvg, n_obs_ves, n_obs_trips, n_obs_hauls, n_hauls_byc, pct_hauls_byc, byc_ratio, est_byc, est_byc_lower, est_byc_upper)
    }else{
      out <- out %>% 
        select(!!sym(bycatchspp_col), !!!syms(strata), obs_byc = total_byc, obs_expf = total_expf, fleet_expf, pct_cvg, n_obs_ves, n_obs_trips, n_obs_hauls, n_hauls_byc, pct_hauls_byc, byc_ratio, est_byc, est_byc_lower, est_byc_upper)
    }
  }
  
  return(ungroup(out))
  
  
}