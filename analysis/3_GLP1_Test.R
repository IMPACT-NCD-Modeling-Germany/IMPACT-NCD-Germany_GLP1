
#### Analysis script IMPACT NCD Germany SSB Tax modeling ----

# Load packages
source("./global.R")

# Load scenario and sensitivity analyses functions
source("./auxil/scenarios_GLP_uncertain.R")

# Load affected population selection function
# source("./auxil/simulate_pid_uptake.R", echo = TRUE)

# Define directories
lifecourse_dir <- "/media/php-workstation/Storage_1/IMPACT_Storage/GLP1/outputs/GLP_Test/lifecourse"

# Initiate .Random.seed for safety
runif(1)

# New runs?
new_runs <- TRUE
new_export <- TRUE


if(new_runs){
  
  # Create batches for batched simulation
  batch_size <- 10
  iterations <- 8
  first_iteration <- 1
  batches <- split(seq(first_iteration, iterations + first_iteration - 1),
                   f = findInterval(seq(first_iteration, iterations + first_iteration - 1),
                                    vec = seq(first_iteration, iterations + first_iteration - 1, batch_size)))
}


analysis_name <- "GLP_Test" ### create a folder to store all the output in this folder

IMPACTncd <- Simulation$new("./inputs/sim_design.yaml", analysis_name)  ### load the model environment
                                                                        ### #IMPACTncd <- Simulation$new("./inputs/sim_design_docker.yaml")

if(new_runs){
#  for (batch in batches) { 
#    for (mc in as.integer(batch)){
  for (i in batches){
      
    message("Running iteration ", i)
    
    
     scenario_fn <- scenario_0_fn

     IMPACTncd$
       run(i, multicore = TRUE, scenario_nam = "sc0", m_zero_trend = -0.03, p_zero_trend = 0) 
    
    ######################################################################################################
    ### Build the lifecourse path for this iteration
    source("./auxil/simulate_pid_uptake.R", echo = TRUE)
    ######################################################################################################
    
     scenario_fn <- scenario_1_fn
    
     IMPACTncd$
      run(i, multicore = TRUE, scenario_nam ="sc1", m_zero_trend = -0.03, p_zero_trend = 0)
    

     scenario_fn <- scenario_2_fn
     
     IMPACTncd$
       run(i, multicore = TRUE, scenario_nam ="sc2", m_zero_trend = -0.03, p_zero_trend = 0)
    
     
     scenario_fn <- scenario_3_fn
     
     IMPACTncd$
       run(i, multicore = TRUE, scenario_nam ="sc3", m_zero_trend = -0.03, p_zero_trend = 0)
    
     
     scenario_fn <- scenario_4_fn
     
     IMPACTncd$
       run(i, multicore = TRUE, scenario_nam ="sc4", m_zero_trend = -0.03, p_zero_trend = 0)
     
     scenario_fn <- scenario_5_fn
     
     IMPACTncd$
       run(i, multicore = TRUE, scenario_nam ="sc5", m_zero_trend = -0.03, p_zero_trend = 0)
     
     
     scenario_fn <- scenario_6_fn
     
     IMPACTncd$
       run(i, multicore = TRUE, scenario_nam ="sc6", m_zero_trend = -0.03, p_zero_trend = 0)
     
     
     scenario_fn <- scenario_7_fn
     
     IMPACTncd$
       run(i, multicore = TRUE, scenario_nam ="sc7", m_zero_trend = -0.03, p_zero_trend = 0)
     
     
     scenario_fn <- scenario_8_fn
     
     IMPACTncd$
       run(i, multicore = TRUE, scenario_nam = "sc8", m_zero_trend = -0.03, p_zero_trend = 0)
     
     scenario_fn <- scenario_9_fn
     
     IMPACTncd$
       run(i, multicore = TRUE, scenario_nam ="sc9", m_zero_trend = -0.03, p_zero_trend = 0)
     
     
     scenario_fn <- scenario_10_fn
     
     IMPACTncd$
       run(i, multicore = TRUE, scenario_nam ="sc10", m_zero_trend = -0.03, p_zero_trend = 0)
     
     
     scenario_fn <- scenario_11_fn
     
     IMPACTncd$
       run(i, multicore = TRUE, scenario_nam ="sc11", m_zero_trend = -0.03, p_zero_trend = 0)
     
     
     scenario_fn <- scenario_12_fn
     
     IMPACTncd$
       run(i, multicore = TRUE, scenario_nam ="sc12", m_zero_trend = -0.03, p_zero_trend = 0)
     
     
     scenario_fn <- scenario_13_fn
     
     IMPACTncd$
       run(i, multicore = TRUE, scenario_nam ="sc13", m_zero_trend = -0.03, p_zero_trend = 0)
     
     
     scenario_fn <- scenario_14_fn
     
     IMPACTncd$
       run(i, multicore = TRUE, scenario_nam ="sc14", m_zero_trend = -0.03, p_zero_trend = 0)
     
     
     scenario_fn <- scenario_15_fn
     
     IMPACTncd$
       run(i, multicore = TRUE, scenario_nam ="sc15", m_zero_trend = -0.03, p_zero_trend = 0)
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

 
