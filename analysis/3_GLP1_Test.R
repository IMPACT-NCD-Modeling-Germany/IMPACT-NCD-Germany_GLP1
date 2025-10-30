
#### Analysis script IMPACT NCD Germany SSB Tax modeling ----

# Load packages
source("./global.R")

# Load scenario and sensitivity analyses functions
source("./auxil/scenarios_GLP_uncertain.R")

# Define directories
lifecourse_dir <- "/media/php-workstation/Storage_2/IMPACT_Storage/GLP1/outputs/GLP_Test/lifecourse"

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

IMPACTncd <- Simulation$new("./inputs/sim_design.yaml", analysis_name)  ### load the model environment
                                                                        ### #IMPACTncd <- Simulation$new("./inputs/sim_design_docker.yaml")

if(new_runs){
  for(i in batches){
    
    message("Running iteration ", i)
    
    # scenario_fn <- scenario_base_fn
    
    # IMPACTncd$
    #   run(1:3, multicore = TRUE, "baseline", m_zero_trend = -0.03, p_zero_trend = 0) 
    
    # scenario_fn <- scenario_0_fn
    
    # IMPACTncd$
    #   run(1:3, multicore = TRUE, "sc0", m_zero_trend = -0.03, p_zero_trend = 0) 
    
    ######################################################################################################
    ### To generate the files with pids who uptake the drug
    # source("./auxil/simulate_pid_uptake.R", echo = TRUE)
    ######################################################################################################
    
    # scenario_fn <- scenario_1_fn
    
    # IMPACTncd$
    #   run(1:3, multicore = TRUE, "sc1", m_zero_trend = -0.03, p_zero_trend = 0) 
    
     scenario_fn <- scenario_0_fn

     IMPACTncd$
       run(i, multicore = TRUE, "sc0", m_zero_trend = -0.03, p_zero_trend = 0) 
    
    ######################################################################################################
    ### To generate the files with pids who uptake the drug
     source("./auxil/simulate_pid_uptake.R", echo = TRUE)
    ######################################################################################################
    
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
 
