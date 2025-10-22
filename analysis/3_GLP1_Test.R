
#### Analysis script IMPACT NCD Germany SSB Tax modeling ----

# Load packages
source("./global.R")

# Load scenario and sensitivity analyses functions
source("./auxil/scenarios_GLP_uncertain.R")

# Initiate .Random.seed for safety
runif(1)

# New runs?
new_runs <- TRUE
new_export <- TRUE


if(new_runs){
  
  # Create batches for batched simulation
  batch_size <- 10
  iterations <- 5
  first_iteration <- 1
  batches <- split(seq(first_iteration, iterations + first_iteration - 1),
                   f = findInterval(seq(first_iteration, iterations + first_iteration - 1),
                                    vec = seq(first_iteration, iterations + first_iteration - 1, batch_size)))
}


analysis_name <- "GLP_Test" ### create a folder to store all the output in this folder

IMPACTncd <- Simulation$new("./inputs/sim_design_docker.yaml", analysis_name)  ### load the model environment
#IMPACTncd <- Simulation$new("./inputs/sim_design_docker.yaml")

if(new_runs){
  for(i in batches){
    
    scenario_fn <- scenario_0_fn
    
    IMPACTncd$
      run(i, multicore = TRUE, "sc0", m_zero_trend = -0.03, p_zero_trend = 0) # What is the output of this line?
    
    ##########################################   NEW  ##############################################
    ## add the codes to extract the pids from the scenario 0
    ## so the scenario 1 can use the list of pids who uptake the drug from sc0
    ## save the list of pids under each iteration (mc)
    ################################################################################################
    #----------------------------------------------------------------------------------------------#
    # a variable for defining individual's eligibility for accessing Semaglutide by BMI
    # Eligibility criteria:
    # Main:       BMI>35 and no diabetes
    # Additional: 30<BMI<35 and CHD/Stroke and no diabetes
    sp$pop[, eligible_bi := ifelse((age<=80 & bmi_curr_xps>=35 & t2dm_prvl==0)|
                                     (age<=80 & bmi_curr_xps>=30 & bmi_curr_xps<35 & t2dm_prvl==0 & chd_prvl>0)|
                                     (age<=80 & bmi_curr_xps>=30 & bmi_curr_xps<35 & t2dm_prvl==0 & stroke_prvl>0), 1, 0)]
    ##############################################################
    #--------- Calculate the constant N_treat each year ---------#
    ##############################################################
    #test <- sp$pop[eligible_bi==1, ]
    #n_distinct(test$pid)
    #----- There are 201500 people who would ever become eligible
    #----- Over 20 years, assuming 10% uptake rate each year, we will have overall 75% of eligible people uptake the drug
    #----- Disperse these patient who uptake the drug over 20 year, to get a constant N patients every year for uptake
    #----- 201500*0.75/20 = 7556.25
    ##############################################################
    # Step 1. Let's set some essential parameters
    start_year <- 25
    end_year   <- 44
    N_treat    <- 6000
    set.seed(123)
    
    # Step 2. Initialize a person-level table to record uptake year (one row per pid, and their potential uptake year)
    persons <- unique(sp$pop[, .(pid)])
    persons[, uptake_year := NA_integer_] # NA => not yet treated
    # Everyone starts with NA as uptake year
    
    # Step 3. Loop through all rollout years (2025-2044)
    for (yr in start_year:end_year) {
      
      # eligible & not previously treated pids in this year
      eligible_pids <- unique(sp$pop[year == yr & eligible_bi == 1, pid])
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
    # This chunk of codes eventually output a table with each pid and their uptake year.
    #----------------------------------------------------------------------------------------------#
    ##########################################   NEW  ##############################################

    scenario_fn <- scenario_1_fn

    IMPACTncd$
      run(i, multicore = TRUE, "sc1", m_zero_trend = -0.03, p_zero_trend = 0)

    # scenario_fn <- scenario_2_fn
    # 
    # IMPACTncd$
    #   run(i, multicore = TRUE, "sc2", m_zero_trend = -0.03, p_zero_trend = 0)
    # 
    # scenario_fn <- scenario_3_fn
    # 
    # IMPACTncd$
    #   run(i, multicore = TRUE, "sc3", m_zero_trend = -0.03, p_zero_trend = 0)
    # 
    # scenario_fn <- scenario_4_fn
    # 
    # IMPACTncd$
    #   run(i, multicore = TRUE, "sc4", m_zero_trend = -0.03, p_zero_trend = 0)
    
  }
}

if(new_export){
  IMPACTncd$export_summaries(multicore = TRUE) 
}
 
