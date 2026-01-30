
#### Analysis script IMPACT NCD Germany GLP1 modeling ----

# Define directories
lifecourse_dir <- "/outputs/GLP_Test/lifecourse"

# Initiate .Random.seed for safety
runif(1)

# New runs?
new_runs <- TRUE
new_export <- TRUE


if(new_runs){
  
  # Create batches for batched simulation
  batch_size <- 2
  iterations <- 8
  first_iteration <- 1
  batches <- split(seq(first_iteration, iterations + first_iteration - 1),
                   f = findInterval(seq(first_iteration, iterations + first_iteration - 1),
                                    vec = seq(first_iteration, iterations + first_iteration - 1, batch_size)))
}


analysis_name <- "GLP_Test" ### create a folder to store all the output in this folder

IMPACTncd <- Simulation$new("./inputs/sim_design.yaml", analysis_name)  ### load the model environment

if(new_runs){

  for (i in batches){
    
    message("Running iteration ", i)
    
    
    scenario_fn <- scenario_0_fn
    
    IMPACTncd$
      run(i, multicore = TRUE, scenario_nam = "sc0", m_zero_trend = -0.03, p_zero_trend = 0) 
    
    ######################################################################################################
    # Load affected population selection function
    source("./auxil/simulate_pid_uptake.R", echo = TRUE)
    
    # Load lifecourse
    for (mc in as.integer(i)) {
      
      lc_path <- file.path(
        lifecourse_dir,
        paste0(mc, "_lifecourse.csv.gz")
      )
      
      if (!file.exists(lc_path)) {
        stop("Missing lifecourse file: ", lc_path)
      }
    
    ### Run the pid selection function, and generate the pid files
    pid_uptake(
               lc_path,
               output_d = paste0(getwd(), "/inputs/uptake"),
               N_treat = 250,
               N_pop_target = 50000,
               pct_treat = 0.10,
               start_year = 25,
               end_year   = 44,
               seed = 123
      )
    }
    ######################################################################################################
    
    scenario_fn <- scenario_1_fn
    
    IMPACTncd$
      run(i, multicore = TRUE, scenario_nam ="sc1", m_zero_trend = -0.03, p_zero_trend = 0)
    
    
    scenario_fn <- scenario_2_fn
    
    IMPACTncd$
      run(i, multicore = TRUE, scenario_nam ="sc2", m_zero_trend = -0.03, p_zero_trend = 0)
    
  }
}