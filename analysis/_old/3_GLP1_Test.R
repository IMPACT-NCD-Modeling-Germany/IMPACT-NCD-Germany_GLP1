  ### load the model environment
                                                                        ### #IMPACTncd <- Simulation$new("./inputs/sim_design_docker.yaml")

if(new_runs){
#  for (batch in batches) { 
#    for (mc in as.integer(batch)){
  for (i in batches){
      
    message("Running iteration ", i)
    
    
     scenario_fn <- scenario_0_fn

     IMPACTncd$
       run(i, multicore = TRUE, "sc0", m_zero_trend = -0.03, p_zero_trend = 0) 
    
    ######################################################################################################
    ### Run the pid selection function, and generate the pid files
     pid_uptake(lc_path,
                iterations = NULL, 
                output_d = paste0(getwd(), "/inputs/uptake"),
                N_treat = 250,
                N_pop_target = 50000,
                pct_treat = 0.10,
                start_year = 25,
                end_year   = 44,
                seed = 123)
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

# Selective Exports
export_type = "BC"
if(new_export){
  IMPACTncd$export_summaries(multicore = TRUE, type = export_type)
}

 
