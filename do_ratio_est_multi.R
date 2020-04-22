#An alternate version of the ratio estimator function that creates bycatch estimates for multiple species
#bycatchspp is the name of all species we want to do expansions for. Question: use species or SPID_EQV?
#bycatchunit is the name of the column that has the weight or count of bycatch 

do_ratio_est_multi <- function(ob_data, ft_data, strata, expfactor, bycatchspp, bycatchunit)
{
  require(dplyr)
  require(rlang)
  
  expf <- enquo(expfactor) 
  
  #byc <- enquo(bycatchspp)
  
  byc_unit <- enquo(bycatchunit)
  
  obdf <- ob_data %>% 
    filter(SPID_EQV %in% bycatchspp) %>% 
    group_by_at(c(strata, "SPID_EQV", "haul_id", "trip_id", "drvid")) %>% 
    summarise(byc = sum(!!byc),
              expf = sum(!!expf, na.rm=T)) %>% #!! unquotes the quosure
    ungroup() %>% 
    group_by_at(strata) %>% 
    summarise(total_byc = sum(byc),
              mean_byc = mean(byc),
              se_byc = sqrt(var(byc)/length(byc)),
              total_expf = sum(expf, na.rm=T),
              mean_expf = mean(expf, na.rm=T),
              se_expf = sqrt(var(expf, na.rm=T)/length(expf)), 
              n_obs_ves = n_distinct(drvid),
              n_obs_trips = n_distinct(trip_id), 
              n_obs_hauls = n_distinct(haul_id))
  
  ftdf <- ft_data %>% 
    group_by_at(strata) %>% 
    summarise(total_ft_expf = sum(!!expf),
              mean_ft_expf = mean(!!expf),
              se_ft_expf = sqrt(var(!!expf)/length(!!expf)))
  
  obft <- obdf %>% 
    full_join(ftdf, by=strata) %>% 
    replace(., is.na(.), 0) %>% #Just in case we have unobserved strata with FT landings
    mutate(pct_cvg = (total_expf / total_ft_expf) * 100, 
           byc_ratio = total_byc / total_expf,
           byc_se = ratio.se(mean_byc, mean_expf, se_byc, se_expf),
           byc_ratio_lower = byc_ratio - 1.96 * byc_se, 
           byc_ratio_upper = byc_ratio + 1.96 * byc_se,
           expand_byc = byc_ratio * total_ft_expf,
           expand_byc_lower = byc_ratio_lower * total_ft_expf,
           expand_byc_upper = byc_ratio_upper * total_ft_expf) %>% 
    mutate(expand_byc_lower = ifelse(is.na(expand_byc_lower), 0, expand_byc_lower),
           expand_byc_upper = ifelse(is.na(expand_byc_upper), 0, expand_byc_upper),
           expand_byc_lower = ifelse(expand_byc_lower < total_byc, total_byc, expand_byc_lower))
  
  if(min(obft$n_obs_ves) < 3 & min(obft$n_obs_ves)>0) {message("Warning: some strata have < 3 observed vessels")}else{
    message("All strata have >=3 observed vessels")}
  
  return(obft)
}

