#This creates a data frame of bycatch ratios, expanded bycatch estimates, and associated information for selected strata, given a data frame of WCGOP  data

#Arguments are as follows:
#ob_dat = Data frame of WCGOP observer data. Any strata of interest not already present in the data should be added as well. 
#strata = A vector of strata to calculate the ratios over. Note that I change everything to snake case (using janitor::clean_names), so strata should be listed accordingly (e.g. strata = c("sector", "year", "r_state") NOT strata = c("sector", "YEAR", "R_STATE")
#expfactor = The name of the column containing the expansion factor (e.g. gfr_mt). Defaults to tgt_mt, assuming this has been added to the input observer data
#bycatchsp = A vector containing all the bycatch species of interest.
#bycatchunit = The name of the column with the measure of bycatch to be expanded. Defaults to dis_mt, but could be counts, etc. 
#management_groups = do we want to calculate ratios by managment grouping? Defaults to TRUE.

do_ratio_est_multi <- function(ob_dat, ft_dat, strata, expfactor, bycatchspp_col, bycatchspp, bycatchunit, management_groups = TRUE)
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
    summarise(fleet_expf = sum(!!sym(expfactor), na.rm=T))
  
  out <- exp_factors %>% 
    full_join(byc_ratios, by = strata) %>% 
    mutate(pct_cvg = round((total_expf / fleet_expf) * 100, 2),
           est_byc = byc_ratio * fleet_expf,
           est_byc_lower = byc_ratio_lower * fleet_expf,
           est_byc_upper = byc_ratio_upper * fleet_expf)
  
  if(management_groups)
  {
    out <- out %>% 
      mutate(grouping = gsub("<b0>", "\xb0", grouping))
  }
  
  return(out)
  
  
}