
################################################################################################
#----------------------------------------------------------------------------------------------#
## add the codes to extract the pids from the scenario 0 --------------------------------------#
## so the scenario 1 can use the list of pids who uptake the drug from sc0 --------------------#
## save the list of pids under each iteration (mc) --------------------------------------------#
#----------------------------------------------------------------------------------------------#
################################################################################################

pid_uptake <- function(lc_path,
                       output_d = paste0(getwd(), "/inputs/uptake"),
                       N_treat = 250,
                       N_pop_target = 50000,
                       pct_treat = 0.10,
                       start_year = 25,
                       end_year   = 44,
                       seed = 123) { 
  
  #--------------------------------------------------
  # Load lifecourse
  #--------------------------------------------------
  lc <- fread(lc_path)  # lc_path is a single file path for a lifecourse (one file path per iteration)
  
  # Extract iteration number (mc)
  mc_id <- unique(lc$mc)
  if (length(mc_id) != 1L) {
    stop("Multiple or missing mc values in lifecourse file: ", lc_path)
  }
  
  # Set seed (unique per iteration) 
  set.seed(seed + mc_id)
  # iteration-specific seeds, make sure the random draw of pids is identical across scenarios 
  # Across iterations, the seeds will be different, that represents stochastic uncertainty in population sampling
  
  #--------------------------------------------------
  # Eligibility definition
  #--------------------------------------------------
  # Main:       BMI>35 and no diabetes
  # Secondary:  30<BMI<35 and CHD/Stroke and no diabetes
  # plus:       30<Age<80
  lc[, eligible_bi := ifelse((age<=80 & bmi_curr_xps>=35 & t2dm_prvl==0), 1, 0)] ## only keeping main
  
  #==================================================
  # CEA: Eligible in 2025
  #==================================================
  # Find pids who are eligible at the year of 2025
  # eligible_cea <- unique(lc[year == start_year & eligible_bi == 1, pid])
  cea_wt <- lc[year == start_year & eligible_bi == 1, .(pid, wt)]
  
 # persons_cea <- data.table(
 #   pid = eligible_cea,
 #   uptake_year = start_year,
 #   uptake_group = 1L
 # )
  
  persons_cea <- cea_wt[, .(
    pid,
    uptake_year  = start_year,
    wt_uptake    = wt,
    uptake_group = 1L
  )]
  
  #==================================================
  # Helper function for BIA uptake (weight-aware)
  #==================================================
  run_bia_uptake <- function(target_fun, group_id) {
    
    persons <- unique(lc[, .(pid)])
    persons[, `:=`(
      uptake_year = NA_integer_,
      wt_uptake   = NA_real_
    )]
    
    for (yr in start_year:end_year) {
      
      # 1. YEAR‑SPECIFIC lifecourse slice
      lc_yr <- lc[year == yr & eligible_bi == 1, .(pid, wt)]
      
      # 2. Eligible & untreated
      eligible_dt <- lc_yr[pid %in% persons[is.na(uptake_year), pid]]
      if (nrow(eligible_dt) == 0L) next
      
      eligible_pids <- eligible_dt$pid
      weights <- eligible_dt$wt   # one weight per pid
      
      # 3. Compute the year's target mass
      target_weight <- target_fun(weights)
      if (target_weight <= 0) next
      
      # 4. Weighted random ordering (prob ∝ weight)
      ord <- order(runif(length(weights)) / weights)
      
      # 5. Cumulative weight
      cum_w <- cumsum(weights[ord])
      
      # 6. Select all pids until target mass reached
      sampled <- eligible_pids[ord][cum_w <= target_weight]
      
      # Guarantee at least one if target > 0
      if (length(sampled) == 0L) {
        sampled <- eligible_pids[ord][1]
      }
      
      # Build a lookup pid -> wt for this year
      sampled_wt <- eligible_dt[pid %in% sampled, .(pid, wt)]
      
      persons[sampled_wt, on = "pid", `:=`(
        uptake_year = yr,
        wt_uptake   = i.wt
      )]
      
    }
    
    persons[!is.na(uptake_year), ][
      , `:=`(
        uptake_group = group_id
      )
    ]
  }
  
  #--------------------------------------------------
  # BIA scenario 1: Fixed N per year
  #--------------------------------------------------
  persons_bia_N <- run_bia_uptake(
    target_fun = function(weights) N_pop_target,
    group_id = 2L
  )
  
  #--------------------------------------------------
  # BIA scenario 2: Percentage per year
  #--------------------------------------------------
  persons_bia_pct <- run_bia_uptake(
    target_fun = function(weights) pct_treat * sum(weights),
    group_id = 3L
  )
  
  out_cea     <- file.path(output_d, paste0(mc_id, "_uptake_cea.csv"))
  out_bia_N   <- file.path(output_d, paste0(mc_id, "_uptake_bia_N.csv"))
  out_bia_pct <- file.path(output_d, paste0(mc_id, "_uptake_bia_pct.csv"))
  
  if (file.exists(out_cea) && file.exists(out_bia_N) && file.exists(out_bia_pct)) {
    message("ℹ Uptake files already exist for mc ", mc_id, " — skipping regeneration")
    return(invisible(NULL))
  }
  
  #==================================================
  # Save outputs
  #==================================================
  if (!dir.exists(output_d)) dir.create(output_d, recursive = TRUE)
  
  fwrite(persons_cea,
         file.path(output_d, paste0(mc_id, "_uptake_cea.csv")))
  
  fwrite(persons_bia_N,
         file.path(output_d, paste0(mc_id, "_uptake_bia_N.csv")))
  
  fwrite(persons_bia_pct,
         file.path(output_d, paste0(mc_id, "_uptake_bia_pct.csv")))
  
  message("✅ Processed iteration ", mc_id, " — saved all uptake scenarios")
  
}




