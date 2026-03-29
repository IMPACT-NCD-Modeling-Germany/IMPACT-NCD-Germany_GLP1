
#### Analysis script IMPACT NCD Germany GLP1 modeling ----

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
  batch_size <- 30
  iterations <- 90
  first_iteration <- 1
  batches <- split(seq(first_iteration, iterations + first_iteration - 1),
                   f = findInterval(seq(first_iteration, iterations + first_iteration - 1),
                                    vec = seq(first_iteration, iterations + first_iteration - 1, batch_size)))
}

analysis_name <- "Mortality_calibration" ### create a folder to store all the output in this folder

IMPACTncd <- Simulation$new("./inputs/sim_design_mort_clbr.yaml", analysis_name)

IMPACTncd$del_outputs()

if(new_runs){

  for (i in batches){
    
    message("Running iteration ", i)
    
    scenario_fn <- scenario_0_fn
    
    IMPACTncd$
      run(i, multicore = TRUE, "sc0", m_zero_trend = -0.03, p_zero_trend = 0) 
    
  }
}

if(new_export){
  IMPACTncd$export_summaries(multicore = TRUE, type = "dis_mrtl") 
} 

source("./validation_internal/validation.R")
