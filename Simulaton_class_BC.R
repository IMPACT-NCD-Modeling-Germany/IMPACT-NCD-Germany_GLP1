

if("bc" %in% type){ # Baseline Characteristics for Table 1
  
  if (self$design$sim_prm$logs) message("Exporting baseline char...")
  
  lc[, age_cat := cut(age,
                      breaks = c(-Inf, 49, 69, 90),
                      labels = c("30-49", "50-69", "70-90"))]
  
  lc[, bmi_cat := cut(bmi_curr_xps,
                      breaks = c(-Inf, 30, 35, 40, Inf),
                      labels = c("<30", "30-35", "35-40", ">40"))]
  
  lc[scenario == 'sc0', .(
    popsize        = sum(wt),                                              # total weighted population
    mean_age       = weighted.mean(age, wt, na.rm = TRUE),                 # weighted mean age
    mean_bmi       = weighted.mean(bmi_curr_xps, wt, na.rm = TRUE),        # weighted mean BMI
    mean_sbp       = weighted.mean(sbp_curr_xps, wt, na.rm = TRUE),        # weighted mean SBP
    mean_chol      = weighted.mean(tchol_curr_xps, wt, na.rm = TRUE),      # weighted mean cholesterol
    
    pct_male       = 100 * sum((sex == "men") * wt, na.rm = TRUE) / sum(wt),      # weighted % male
    pct_t2dm       = 100 * sum((t2dm_prvl > 0) * wt, na.rm = TRUE) / sum(wt),    # weighted % diabetes
    pct_chd        = 100 * sum((chd_prvl > 0) * wt, na.rm = TRUE) / sum(wt),     # weighted % CHD
    pct_stroke     = 100 * sum((stroke_prvl > 0) * wt, na.rm = TRUE) / sum(wt),  # weighted % stroke
    pct_obesity    = 100 * sum((obesity_prvl > 0) * wt, na.rm = TRUE) / sum(wt)  # weighted % obesity
  )]
  
  lc[scenario == 'sc0', .(
    numerator = sum(wt),
    denominator = sum(lc_test_2_cs[, wt]),
    pct = 100 * sum(wt) / sum(lc_test_2_cs[, wt])
  ), by = age_cat]
  
  lc[scenario == 'sc0', .(
    numerator = sum(wt),
    denominator = sum(lc_test_2_cs[, wt]),
    pct = 100 * sum(wt) / sum(lc_test_2_cs[, wt])
  ), by = bmi_cat]

  fwrite_safe(lc[all_cause_mrtl > 0, .("popsize" = sum(wt), LE = weighted.mean(age, wt)),  keyby = strata],
              private$output_dir(paste0("summaries", "/le_scaled_up.csv.gz"
              )))

  # Note: for less aggregation use wtd.mean with popsize i.e le_out[, weighted.mean(LE, popsize), keyby = year]
}