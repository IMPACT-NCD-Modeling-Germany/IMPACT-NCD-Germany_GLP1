
#### Analysis script IMPACT NCD Germany GLP1 modeling ----

# Load model packages
source("./global.R")

# Load scenario and sensitivity analyses functions
# source("./auxil/scenarios_GLP_uncertain.R") # Done: split up into different files

# Load eligible and uptake population function
source("./auxil/simulate_pid_uptake.R", echo = TRUE)


# Initiate .Random.seed for safety
runif(10)
set.seed(12345)

# New runs?
new_runs <- FALSE
new_export <- TRUE


if(new_runs){
  
  # Create batches for batched simulation
  batch_size <- 5
  iterations <- 250
  first_iteration <- 251
  batches <- split(seq(first_iteration, iterations + first_iteration - 1),
                   f = findInterval(seq(first_iteration, iterations + first_iteration - 1),
                                    vec = seq(first_iteration, iterations + first_iteration - 1, batch_size)))
}

###################################################################################################
#-------------------------------------------------------------------------------------------------#
#----------------------- Step 1: generate the lifecourse with only sc0 ---------------------------#
#-------------------------------------------------------------------------------------------------#
###################################################################################################

analysis_name <- "GLP_final_sc0" 
### create a folder to store all the output of this analysis

# Define directories
lifecourse_dir <- paste0("/mnt/Storage_1/IMPACT_Storage/GLP1/outputs/", analysis_name, "/lifecourse")

IMPACTncd_sc0 <- Simulation$new("./inputs/sim_design.yaml", analysis_name)

# TODO load scenario script here   
source("./auxil/scenarios_GLP_uncertain_sc0.R") 

if(new_runs){
  
  for (i in batches){
    
    message("Running iteration ", i)
    
    scenario_fn <- scenario_0_fn
    
    IMPACTncd_sc0$
      run(i, multicore = TRUE, "sc0", m_zero_trend = -0.03, p_zero_trend = 0) 
    
    ######################################################################################################
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
        output_d = paste0("/mnt/Storage_1/IMPACT_Storage/GLP1/inputs/uptake"),
        N_treat = 250,
        N_pop_target = 50000,
        pct_treat = 0.10,
        start_year = 25,
        end_year   = 44,
        seed = 123
      )
    }
    
  }
}

#TODO find out which suammaries are needed!
if(new_export){
  IMPACTncd_sc0$export_summaries(multicore = TRUE, type = c("ly",
                                                            "prvl", "incd",
                                                            "mrtl",  "dis_mrtl", 
                                                            "stroke_risk_10y", "chd_risk_10y"))
} 

###################################################################################################
#-------------------------------------------------------------------------------------------------#
#--------------------- Step 2: generate the lifecourse for CEA (sc1-sc5) -------------------------#
#-------------------------------------------------------------------------------------------------#
###################################################################################################

analysis_name <- "GLP_final_cea" 
### create a folder to store all the output of this analysis

IMPACTncd_cea <- Simulation$new("./inputs/sim_design.yaml", analysis_name)

# TODO load scenario script here    
source("./auxil/scenarios_GLP_uncertain_cea.R") 

if(new_runs){
  
  for (i in batches){
    
    message("Running iteration ", i)
    
    scenario_fn <- scenario_0_fn
    
    IMPACTncd_cea$
      run(i, multicore = TRUE, "sc0", m_zero_trend = -0.03, p_zero_trend = 0) 
    
    scenario_fn <- scenario_1_fn
    
    IMPACTncd_cea$
      run(i, multicore = TRUE, "sc1", m_zero_trend = -0.03, p_zero_trend = 0)
    
    scenario_fn <- scenario_2_fn
    
    IMPACTncd_cea$
      run(i, multicore = TRUE, "sc2", m_zero_trend = -0.03, p_zero_trend = 0)
    
    scenario_fn <- scenario_3_fn
    
    IMPACTncd_cea$
      run(i, multicore = TRUE, "sc3", m_zero_trend = -0.03, p_zero_trend = 0)
    
    scenario_fn <- scenario_4_fn
    
    IMPACTncd_cea$
      run(i, multicore = TRUE, "sc4", m_zero_trend = -0.03, p_zero_trend = 0)
    
  }
}


if(new_export){
  IMPACTncd_cea$export_summaries(multicore = TRUE, type = c("ly",
                                                            "prvl", "incd",
                                                            "mrtl",  "dis_mrtl", 
                                                            "xps", "cea")) 
} 


if(new_export){
  IMPACTncd_cea$export_summaries(multicore = TRUE, type = c("cea")) 
} 

###################################################################################################
#-------------------------------------------------------------------------------------------------#
#------------------- Step 3: generate the lifecourse for BIA_N (sc6-sc10) ------------------------#
#-------------------------------------------------------------------------------------------------#
###################################################################################################

analysis_name <- "GLP_final_bia_num" 
### create a folder to store all the output of this analysis

IMPACTncd_biaN <- Simulation$new("./inputs/sim_design.yaml", analysis_name)

# TODO load scenario script here    
source("./auxil/scenarios_GLP_uncertain_biaN.R") 

if(new_runs){
  
  for (i in batches){
    
    message("Running iteration ", i)
    
    scenario_fn <- scenario_0_fn
    
    IMPACTncd_biaN$
      run(i, multicore = TRUE, "sc0", m_zero_trend = -0.03, p_zero_trend = 0) 
    
    scenario_fn <- scenario_1_fn
    
    IMPACTncd_biaN$
      run(i, multicore = TRUE, "sc1", m_zero_trend = -0.03, p_zero_trend = 0)
    
    
    scenario_fn <- scenario_2_fn
    
    IMPACTncd_biaN$
      run(i, multicore = TRUE, "sc2", m_zero_trend = -0.03, p_zero_trend = 0)
    
    
    scenario_fn <- scenario_3_fn
    
    IMPACTncd_biaN$
      run(i, multicore = TRUE, "sc3", m_zero_trend = -0.03, p_zero_trend = 0)
    
    
    scenario_fn <- scenario_4_fn
    
    IMPACTncd_biaN$
      run(i, multicore = TRUE, "sc4", m_zero_trend = -0.03, p_zero_trend = 0)
    
  }
}


if(new_export){
  IMPACTncd_biaN$export_summaries(multicore = TRUE, type = c("ly",
                                                            "prvl", "incd",
                                                            "mrtl",  "dis_mrtl", 
                                                            "xps", "cea")) 
} 

if(new_export){
  IMPACTncd_biaN$export_summaries(multicore = TRUE, type = c("cea")) 
} 

###################################################################################################
#-------------------------------------------------------------------------------------------------#
#------------------- Step 4: generate the lifecourse for BIA_% (sc11-sc15) -----------------------#
#-------------------------------------------------------------------------------------------------#
###################################################################################################

analysis_name <- "GLP_final_bia_perc" 
### create a folder to store all the output of this analysis

IMPACTncd_biaP <- Simulation$new("./inputs/sim_design.yaml", analysis_name)

# TODO load scenario script here  
source("./auxil/scenarios_GLP_uncertain_biaP.R") 

if(new_runs){
  
  for (i in batches){
    
    message("Running iteration ", i)
    
    scenario_fn <- scenario_0_fn
    
    IMPACTncd_biaP$
      run(i, multicore = TRUE, "sc0", m_zero_trend = -0.03, p_zero_trend = 0)
    
    
    scenario_fn <- scenario_1_fn
    
    IMPACTncd_biaP$
      run(i, multicore = TRUE, "sc1", m_zero_trend = -0.03, p_zero_trend = 0)
    
    
    scenario_fn <- scenario_2_fn
    
    IMPACTncd_biaP$
      run(i, multicore = TRUE, "sc2", m_zero_trend = -0.03, p_zero_trend = 0)
    
    
    scenario_fn <- scenario_3_fn
    
    IMPACTncd_biaP$
      run(i, multicore = TRUE, "sc3", m_zero_trend = -0.03, p_zero_trend = 0)
    
    
    scenario_fn <- scenario_4_fn
    
    IMPACTncd_biaP$
      run(i, multicore = TRUE, "sc4", m_zero_trend = -0.03, p_zero_trend = 0)
    
  }
}


if(new_export){
  IMPACTncd_biaP$export_summaries(multicore = TRUE, type = c("ly",
                                                            "prvl", "incd",
                                                            "mrtl",  "dis_mrtl", 
                                                            "xps", "cea")) 
} 

