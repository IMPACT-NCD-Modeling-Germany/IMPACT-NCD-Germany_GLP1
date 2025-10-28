
################################################################################################
#----------------------------------------------------------------------------------------------#
## add the codes to extract the pids from the scenario 0 --------------------------------------#
## so the scenario 1 can use the list of pids who uptake the drug from sc0 --------------------#
## save the list of pids under each iteration (mc) --------------------------------------------#
#----------------------------------------------------------------------------------------------#
################################################################################################

pid_uptake <- function(lc_path, output_dir = paste0(getwd(), "/inputs/uptake"), N_treat = 500, 
                       start_year = 25, end_year = 44, seed = 123) { 
  
  # Load lifecourse datasets
  lc <- fread(lc_path)       # lc_path is a single file path (one file path per iteration)
  
  # Extract iteration number (mc)
  mc_id <- unique(lc$mc)
  if (length(mc_id) != 1L) {
    stop("Multiple or missing mc values in lifecourse file: ", lc_path)
  }
  
  # Set seed (unique per iteration) 
  set.seed(seed + mc_id)
  # iteration-specific seeds, make sure the random draw of pids is identical across scenarios 
  # Across iterations, the seeds will be different, that represents stochastic uncertainty in population sampling
  
  # Eligibility definition
  # Main:       BMI>35 and no diabetes
  # Secondary:  30<BMI<35 and CHD/Stroke and no diabetes
  lc[, eligible_bi := ifelse((age<=80 & bmi_curr_xps>=35 & t2dm_prvl==0)|
                               (age<=80 & bmi_curr_xps>=30 & bmi_curr_xps<35 & t2dm_prvl==0 & chd_prvl>0)|
                               (age<=80 & bmi_curr_xps>=30 & bmi_curr_xps<35 & t2dm_prvl==0 & stroke_prvl>0), 1, 0)]
  
  # Step 1. Initialize a person-level table to record uptake year (one row per pid, and their potential uptake year)
  persons <- unique(lc[, .(pid)])
  persons[, uptake_year := NA_integer_] # NA => not yet treated
  # Everyone starts with NA as uptake year
  
  # Step 2. Loop through all rollout years (2025-2044)
  for (yr in start_year:end_year) {
    
    # eligible & not previously treated pids in this year
    eligible_pids <- unique(lc[year == yr & eligible_bi == 1, pid])
    # ---'eligible_pids' is a vector of person IDs who meet the eligibility criteria in year yr
    
    # remove already treated people
    eligible_pids <- eligible_pids[is.na(persons[match(eligible_pids, persons$pid), uptake_year])]
    # --- It ends up as: eligible_pids[c(TRUE, FALSE, TRUE, FALSE)]
    # --- In R, when you use a logical vector inside [...], it keeps the elements where the value is TRUE.
    # --- This line: keeps only those eligible_pids for which the person’s uptake_year is still NA.
    
    # check the number of eligible patients in this year
    n_eligible <- length(eligible_pids)
    if (n_eligible == 0L) next
    # --- A logical statement for whether or not to proceed the loop
    # --- i.e., if there's no eligible patient in one year, then nothing to do this year
    
    # sample N patients from the eligible pool to uptake the drug
    N_to_sample <- min(N_treat, n_eligible)
    # --- cap the number of uptake patients if the eligible pool is smaller than N
    sampled <- sample(eligible_pids, N_to_sample)
    
    # record uptake year for those sampled
    persons[pid %in% sampled, uptake_year := yr]
    
  }
  
  # Step 3. Add mc column in each selected-pid table
  #         Also delete NAs from persons file
  persons <- persons[!is.na(uptake_year), ]
  persons[, mc := mc_id]
  
  # Step 4. I also need a variable for the year someone enters the lifecourse
  lc[, entry_year:= min(year), by = pid]
  entry <- unique(lc[, .(pid, entry_year)], by = "pid")
  #entry <- unique[lc, by = 'pid'][ , c('pid','entry_year')]
  persons <- merge(persons, entry, by = "pid", all.x = TRUE)
  
  # Check if output dir exists
  if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # Save output
  out_file <- file.path(output_dir, paste0(mc_id, "_uptake.csv")) # not just a directory, but with file name included
  fwrite(persons, out_file) # writing persons into this file name from file.path()
  message("✅ Processed iteration ", mc_id, " — saved to ", out_file)
  
  return(persons)
  # This chunk of codes eventually output a table with each pid and their uptake year.
  
}

# List all lifecourse files
fl <- list.files(lifecourse_dir, pattern = "_lifecourse\\.csv\\.gz$", full.names = TRUE)
### Test
### fl <- list.files(paste0(getwd()), pattern = "_lifecourse\\.csv\\.gz$", full.names = TRUE) --> Works!!!

# Loop through all lifecourse files and simulate uptake
for (lc in fl) {
  # The variable lc takes one lifecourse of fl, i.e. one file path.
  # 'lc' is passed into the function as the 'lc_path' argument
  pid_uptake(lc, N_treat = 500)
  
}




