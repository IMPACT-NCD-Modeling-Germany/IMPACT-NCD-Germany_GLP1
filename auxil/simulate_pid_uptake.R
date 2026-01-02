
################################################################################################
#----------------------------------------------------------------------------------------------#
## add the codes to extract the pids from the scenario 0 --------------------------------------#
## so the scenario 1 can use the list of pids who uptake the drug from sc0 --------------------#
## save the list of pids under each iteration (mc) --------------------------------------------#
#----------------------------------------------------------------------------------------------#
################################################################################################

pid_uptake <- function(lc_path,
                       output_d = paste0(getwd(), "/inputs/uptake"),
                       N_treat = 500,
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
  lc[, eligible_bi := ifelse((age<=80 & bmi_curr_xps>=35 & t2dm_prvl==0)|
                               (age<=80 & bmi_curr_xps>=30 & bmi_curr_xps<35 & t2dm_prvl==0 & chd_prvl>0)|
                               (age<=80 & bmi_curr_xps>=30 & bmi_curr_xps<35 & t2dm_prvl==0 & stroke_prvl>0), 1, 0)]
  
  #==================================================
  # CEA: Eligible in 2025
  #==================================================
  # Find pids who are eligible at the year of 2025
  eligible_cea <- unique(lc[year == start_year & eligible_bi == 1, pid])
  
  persons_cea <- data.table(
    pid = eligible_cea,
    uptake_year = start_year,
    mc = mc_id
  )
  
  #==================================================
  # Helper function for BIA uptake
  #==================================================
  run_bia_uptake <- function(sample_size_fun) {
    # run_bia_uptake() is a function whose input is another function
    # This is called a higher-order function
    # Think of 'sample_size_fun' as a “rule”: Given the number of eligible patients this year (n_eligible), 
    #                                         apply the rule sample_size_fun to decide how many people to sample.”
    
    # Step 1. Initialize a person-level table to record uptake year (one row per pid, and their potential uptake year)
    persons <- unique(lc[, .(pid)])
    persons[, uptake_year := NA_integer_] # Everyone starts with NA as uptake year
    
    # Step 2. Loop through all rollout years (2025-2044)
    for (yr in start_year:end_year) {
      
      # eligible & not previously treated pids in this year: 
      # --'eligible_pids' is a vector of person IDs who meet the eligibility criteria in year yr
      eligible_pids <- unique(lc[year == yr & eligible_bi == 1, pid])
      
      # remove already treated people year by year
      eligible_pids <- eligible_pids[is.na(persons[match(eligible_pids, pid), uptake_year])]
      # --- It ends up as: eligible_pids[c(TRUE, FALSE, TRUE, FALSE)]
      # --- match(x, y) returns the row positions in y where each element of x appears.
      # --- This line keeps only those *eligible_pids* whose uptake_year is still NA in the persons table.
      # --- From the currently eligible people, drop anyone who has already taken up the medication in a previous year.
      
      n_eligible <- length(eligible_pids)
      if (n_eligible == 0L) next
      
      N_to_sample <- sample_size_fun(n_eligible)
      if (N_to_sample <= 0L) next
      
      sampled <- sample(eligible_pids, min(N_to_sample, n_eligible))
      
      persons[pid %in% sampled, uptake_year := yr]
    }
    
    persons[!is.na(uptake_year), ][, mc := mc_id]
  }
  
  #--------------------------------------------------
  # BIA scenario 1: Fixed N per year
  #--------------------------------------------------
  persons_bia_N <- run_bia_uptake(
    function(n_eligible) min(N_treat, n_eligible)
  )
  # This is where 'sample_size_fun' is defined.
  # It is defined at the moment you call run_bia_uptake().
  # sample_size_fun = (function(n_eligible) min(N_treat, n_eligible))
  
  #--------------------------------------------------
  # BIA scenario 2: Percentage per year
  #--------------------------------------------------
  persons_bia_pct <- run_bia_uptake(
    function(n_eligible) floor(pct_treat * n_eligible)
  )
  # This is where 'sample_size_fun' is defined.
  # It is defined at the moment you call run_bia_uptake().
  # sample_size_fun = (function(n_eligible) floor(pct_treat * n_eligible))
  
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
  
  #==================================================
  # Return all results
  #==================================================
  return(list(
    cea      = persons_cea,
    bia_N    = persons_bia_N,
    bia_pct  = persons_bia_pct
  ))
  
}

# List all lifecourse files
fl <- list.files(lifecourse_dir, pattern = "_lifecourse.csv.gz$", full.names = TRUE)
### Test
### fl <- list.files(paste0(getwd()), pattern = "_lifecourse\\.csv\\.gz$", full.names = TRUE) --> Works!!!

# Loop through all lifecourse files and simulate uptake
for (lc in fl) {
  # The variable lc takes one lifecourse of fl, i.e. one file path.
  # 'lc' is passed into the function as the 'lc_path' argument
  pid_uptake(lc, 
             N_treat = 500,
             pct_treat = 0.10)
  
}




