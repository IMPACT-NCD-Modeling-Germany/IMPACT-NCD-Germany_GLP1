
#### Analysis script IMPACT NCD Germany SSB Tax modeling ----

# Load packages
source("./global.R")

# Load scenario and sensitivity analyses functions
source("./auxil/scenarios_GLP_uncertain.R")

# Define directories
lifecourse_dir <- "/media/php-workstation/Storage_1/IMPACT_Storage/GLP1/outputs/GLP_Test/lifecourse"

# Initiate .Random.seed for safety
runif(1)

# New runs?
new_runs <- FALSE
new_export <- TRUE


if(new_runs){
  
  # Create batches for batched simulation
  batch_size <- 10
  iterations <- 3
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
    

     scenario_fn <- scenario_2_fn
     
     IMPACTncd$
       run(i, multicore = TRUE, "sc2", m_zero_trend = -0.03, p_zero_trend = 0)
    
     
     scenario_fn <- scenario_3_fn
     
     IMPACTncd$
       run(i, multicore = TRUE, "sc3", m_zero_trend = -0.03, p_zero_trend = 0)
    
     
     scenario_fn <- scenario_4_fn
     
     IMPACTncd$
      run(i, multicore = TRUE, "sc4", m_zero_trend = -0.03, p_zero_trend = 0)
     
     scenario_fn <- scenario_5_fn
     
     IMPACTncd$
       run(i, multicore = TRUE, "sc5", m_zero_trend = -0.03, p_zero_trend = 0)
     
     
     scenario_fn <- scenario_6_fn
     
     IMPACTncd$
       run(i, multicore = TRUE, "sc6", m_zero_trend = -0.03, p_zero_trend = 0)
     
     
     scenario_fn <- scenario_7_fn
     
     IMPACTncd$
       run(i, multicore = TRUE, "sc7", m_zero_trend = -0.03, p_zero_trend = 0)
     
     
     scenario_fn <- scenario_8_fn
     
     IMPACTncd$
       run(i, multicore = TRUE, "sc8", m_zero_trend = -0.03, p_zero_trend = 0)
     
     scenario_fn <- scenario_9_fn
     
     IMPACTncd$
       run(i, multicore = TRUE, "sc9", m_zero_trend = -0.03, p_zero_trend = 0)
     
     
     scenario_fn <- scenario_10_fn
     
     IMPACTncd$
       run(i, multicore = TRUE, "sc10", m_zero_trend = -0.03, p_zero_trend = 0)
     
     
     scenario_fn <- scenario_11_fn
     
     IMPACTncd$
       run(i, multicore = TRUE, "sc11", m_zero_trend = -0.03, p_zero_trend = 0)
     
     
     scenario_fn <- scenario_12_fn
     
     IMPACTncd$
       run(i, multicore = TRUE, "sc12", m_zero_trend = -0.03, p_zero_trend = 0)
     
     
     scenario_fn <- scenario_13_fn
     
     IMPACTncd$
       run(i, multicore = TRUE, "sc13", m_zero_trend = -0.03, p_zero_trend = 0)
     
     
     scenario_fn <- scenario_14_fn
     
     IMPACTncd$
       run(i, multicore = TRUE, "sc14", m_zero_trend = -0.03, p_zero_trend = 0)
     
     
     scenario_fn <- scenario_15_fn
     
     IMPACTncd$
       run(i, multicore = TRUE, "sc15", m_zero_trend = -0.03, p_zero_trend = 0)
    
  }
}

if(new_export){
  IMPACTncd$export_summaries(multicore = TRUE) 
} 

# Selective Exports
export_type = "cea"
if(new_export){
  IMPACTncd$export_summaries(multicore = TRUE, type = export_type)
}

 
