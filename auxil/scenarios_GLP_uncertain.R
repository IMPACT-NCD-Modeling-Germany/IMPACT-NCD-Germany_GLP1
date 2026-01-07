
##################################################################################################################
#----------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------   Scenario 0   -------------------------------------------------#
#----------------------------------    Base case scenario: Nothing happens   ------------------------------------#
#----------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------#
##################################################################################################################
# scenario_base_fn <- function(sp) {

#   sp$pop[, c("bmi_delta", "sbp_delta", "tchol_delta") := 0]

# } 

#scenario_0_fn <- function(sp) {
  
#  sp$pop[, `:=`(
#           bmi_delta   = 0,
#           sbp_delta   = 0,
#           tchol_delta = 0,
#           trtm_theo   = 0,
           
#           bmi_cate = fcase(
#             bmi_curr_xps <30,                    "1",
#             bmi_curr_xps>=30 & bmi_curr_xps<35,  "2",
#             bmi_curr_xps>=35,                    "3")
#         )
#  ]
#}

scenario_0_fn <- function(sp) {
  
  sp$pop[, `:=`(
    bmi_delta    = 0,
    sbp_delta    = 0,
    tchol_delta  = 0,
    anchor_year  = NA,
    trtm_theo    = 0,
    uptake_group = 0
  )
  ]
}

#scenario_0_fn(sp)

##################################################################################################################
#----------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------   Scenario 1    ------------------------------------------------#
#----------------------   Nutrition therapy for CEA: uptake 100% at 2025, 2 years treatment  --------------------#
#----------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------#
##################################################################################################################
######## Nutrition therapy:---------------------------------------------------------------------------------------
######## 1. Treatment effects:------------------------------------------------------------------------------------
########    We assume that the effects on weight loss from the nutrition therapy is nearly identical to effects
########    from the placebo arm in the STEP trials, which is caused by the lifestyle intervention and placebo effects.
########    Therefore, we use the effects observed in the placebo arms from STEP 1 and 5 trial.
########    BMI --> 2.4% at year 1,     2.6% at year 2
########    SBP --> 1.1 mmHG at year 1, 1.6 mmHG at year 2
########    We assume the weight rebound to baseline at year 3
########    ?? For the lifetime scenario, shall we also extend the treatment effect of nutrition therapy for 20 years? 
######## 2. Treatment duration & uptake:--------------------------------------------------------------
########    Nutrition therapy is assumed as the Standard-of-Care for obese patients in Germany, therefore the 
########    treatment duration and uptake should reflect the status-quo of the current treatment, which is ~2 years 
########    the treatment uptake of nutrition therapy is unknown --> when we assume certain uptake scenario for GLP1
########    ?? Shall we apply these uptake to the nutrition therapy?
######## ---------------------------------------------------------------------------------------------------------

scenario_1_fn <- function(sp) {
  
  ####################################################################################################
  # Loading uncertainty files 
  # Efficacy for bmi at 1st year after treatment
  tbl <- read_fst("./inputs/other_parameters/bmi_1y_samples_nutr.fst", as.data.table = TRUE)
  bmi_1y <- as.numeric(tbl[mc == sp$mc_aggr, "bmi_1y"])
  
  # Efficacy for bmi at 2nd year after treatment
  tbl <- read_fst("./inputs/other_parameters/bmi_2y_samples_nutr.fst", as.data.table = TRUE)
  bmi_2y <- as.numeric(tbl[mc == sp$mc_aggr, "bmi_2y"])
  
  # Efficacy for SBP at 1st year after treatment
  tbl <- read_fst("./inputs/other_parameters/sbp_1y_samples_nutr.fst", as.data.table = TRUE)
  sbp_1y <- as.numeric(tbl[mc == sp$mc_aggr, "sbp_1y"])
  
  # Efficacy for SBP at 2nd year of treatment
  tbl <- read_fst("./inputs/other_parameters/sbp_2y_samples_nutr.fst", as.data.table = TRUE)
  sbp_2y <- as.numeric(tbl[mc == sp$mc_aggr, "sbp_2y"])
  
  #####################################################################################################
  # Load the pids of uptake patients for CEA scenario
  #---------------------------------------------------------------------------------------------------------#
  # Eligibility definition
  # Main:       BMI>35 and no diabetes
  # Secondary:  30<BMI<35 and CHD/Stroke and no diabetes
  # plus:       30<Age<80
  # lc[, eligible_bi := ifelse((age<=80 & bmi_curr_xps>=35 & t2dm_prvl==0)|
  #                             (age<=80 & bmi_curr_xps>=30 & bmi_curr_xps<35 & t2dm_prvl==0 & chd_prvl>0)|
  #                             (age<=80 & bmi_curr_xps>=30 & bmi_curr_xps<35 & t2dm_prvl==0 & stroke_prvl>0), 1, 0)]
  
  # For Cost Effectiveness Analysis: Find pids who are eligible at the year of 2025
  # eligible_cea <- unique(lc[year == 25 & eligible_bi == 1, pid])
  #---------------------------------------------------------------------------------------------------------#
  persons_cea <- fread(paste0("./inputs/uptake/", sp$mc_aggr, "_uptake_cea.csv"))
  ### fread() is highly optimized: it’s faster than read_csv(), and it returns a data.table automatically.
  ###########################################################################################################
  
  # Join uptake info back to lifecourse rows
  sp$pop <- merge(sp$pop, persons_cea, by = "pid", all.x = TRUE)
  
  # Create a row-level indicator for uptake event (1 only in the uptake year row)
  sp$pop[, uptake_one := as.integer(!is.na(uptake_year) & year == uptake_year)]
  # --- !is.na(uptake_year): checks whether the uptake_year column is no NA, returns 'TRUE' or 'FALSE'
  # --- year == uptake_year: checks whether the value in 'year' column is equal to the value in 'uptake_year' column.
  #                          also returns 'TRUE' or 'FALSE'
  # --- as.integer(...):     converts the result of the logical expression into an integer.
  #                          In R, as.integer(TRUE) becomes 1 and as.integer(False) becomes 0.
  
  # a variable for the year someone uptaking the drug
  sp$pop[, anchor_year:=year[uptake_one == 1], by = pid]
  
  # a variable for the year someone enters the synthetic population
  # sp$pop[, entry_year:= min(year), by = pid]
  
  # a variable for theorectical treatment trajectory
  ### Step 0: Initialize the column
  sp$pop[, trtm_theo := NA_integer_]
  ### Step 1: Identify first year of treatment uptake
  sp$pop[uptake_one == 1, trtm_theo := 0]
  ### Step 2: Fill the sequence in subsequent years
  sp$pop[, trtm_theo := fifelse(cumsum(!is.na(trtm_theo)) > 0,   # cumsum(): computes a cumulative count of non-NA entries:
                                # fill in a sequence starting from the first non-NA value
                                seq_len(.N) - min(which(!is.na(trtm_theo))),
                                NA_integer_),
         by = pid]
  ### Step 3: Replace NAs with 0
  sp$pop[, trtm_theo := fifelse(is.na(trtm_theo), 0, trtm_theo)]
  
  ########################################################################################
  ################################          BMI         ##################################
  ########################################################################################
  # a variable for the change in BMI after the uptake of semaglutide
  sp$pop[, `:=`(
    bmi_delta = fcase(
      ### All values until year 5, are % weight loss relative to baseline
      trtm_theo == 0, 0,       ### Baseline year, the year patients become eligible
      trtm_theo == 1, bmi_1y,
      trtm_theo == 2, bmi_2y,
      trtm_theo >= 3, 0        ### weight return to baseline weight by 3rd year treatment cessation (uncertainty?)
      ### The efficacy inputs are assigned until here, the rest we will revert to the original exposure column (bmi_curr_xps)
    ))
  ]
  
  # a variable for each individual's baseline weight (baseline: year == 24 / ys_rollout == 0)
  # *but, some individuals in the dataset does not have entry at baseline_yr, they came in later than year 24
  # *and, some individuals came in later than year 24, and because they have a chance to uptake drug begining year 25
  # *they might already uptake the drug at the first year, which means they won't have a year (anchor-1),
  # *so what shall we use as a baseline BMI value here...
  
  sp$pop[, baseline_bmi := {              # sp$pop[, ... , by = pid]: grouping the operation by pid
    anchor <- unique(anchor_year)         # So we can distinguish people who have entry at year 24 and who do not
    if (is.na(anchor)) {                  # If no anchor is found,
      bmi_curr_xps                        # the function simply returns the original bmi_curr_xps as-is
    } else {                              # If yes anchor is found
      bmi_curr_xps[year == (year[trtm_theo == 1][1] - 1)] # baseline_bmi will take the BMI at the year before trtm_theo==1
    }
  }, by = pid] #Works!
  
  ###############################################################################################################
  #----------------------------  Tutorial: if_else function inside data.table  ---------------------------------#
  ###############################################################################################################
  sp$pop[, bmi_shift := {             # sp$pop[, ... , by = pid]: grouping the operation by pid
    # { ... } operetion per group: inside {}, you can run any custom R code, it will
    # return a vector that gets assigned to 'bmi_shift' for the rows in that group
    anchor <- unique(anchor_year)     # This line finds a single anchor year (e.g., year 30) for each person.
    if (is.na(anchor)) {              # If no anchor is found (to avoid breaking your logic when a person doesn't uptake the drug)
      bmi_curr_xps                    # the function simply returns the original bmi_curr_xps as-is
    } else {                          # If yes anchor is found, You shift the bmi_curr_xps backward in time (type='lag')
      shift(bmi_curr_xps, type = "lag", n = 3)
    }
  }, by = pid] #Works!
  ###############################################################################################################
  #--------------------------------------------  Tutorial: End  ------------------------------------------------#
  ###############################################################################################################
  
  # a variable for new weight after applying efficacy input during treatment influenced period
  sp$pop[, new_bmi := {
    anchor <- unique(anchor_year)                             # This line finds a single anchor year for each person.
    if (is.na(anchor)) {                                      # If no anchor is found,
      bmi_curr_xps                                            # the function simply returns the original bmi_curr_xps as-is
    } else {                                                  # If yes anchor is found,
      fcase(                                                  # We need to assign value to the' new_bmi' based on the time periods
        year < anchor,                       bmi_curr_xps,    # Before anchor year/uptake: original values (including baseline year)
        year >= anchor & year <= anchor + 2, baseline_bmi*(1 + bmi_delta),  # During the 3 years where treatment effects manifest
        year >= anchor + 3,                   bmi_shift        # After rebound to baseline: revert to the original bmi_curr_xps
      )
    }
  }, by = pid]
  
  # Overwriting the original bmi exposure column with the newly created bmi column
  #sp$pop[, bmi_curr_xps := new_bmi]
  
  ########################################################################################
  ################################          SBP         ##################################
  ########################################################################################
  # a variable for the change in SBP after the uptake of semaglutide
  sp$pop[, `:=`(
    sbp_delta = fcase(
      ### All values until year 5, are % weight loss relative to baseline
      trtm_theo == 0, 0,       ### Baseline year, the year patients become eligible
      trtm_theo == 1, sbp_1y,
      trtm_theo == 2, sbp_2y,
      trtm_theo >= 3, 0        ### STEP 1 extension: assume mean SBP will revert to baseline level after one year without treatment
      ### The efficacy inputs are assigned until here, the rest we will revert to the original exposure column (sbp_curr_xps)
    ))
  ]
  
  # a variable for each individual's baseline SBP (baseline: year == 24 / ys_rollout == 0)
  # *but, some individuals in the dataset does not have entry at baseline_yr, they came in later than year 24
  sp$pop[, baseline_sbp := {                # sp$pop[, ... , by = pid]: grouping the operation by pid
    anchor <- unique(anchor_year)           # So we can distinguish people who have entry at year 24 and who do not
    if (is.na(anchor)) {                    # If no anchor is found,
      sbp_curr_xps                          # the function simply returns the original sbp_curr_xps as-is
    } else {                                # If yes anchor is found, baseline_sbp will take the sbp at year 24
      sbp_curr_xps[year == (year[trtm_theo == 1][1] - 1)]  # The baseline year will be (anchor - 1): one year before uptake
    }
  }, by = pid] #Works!
  
  ###############################################################################################################
  #----------------------------  Tutorial: if_else function inside data.table  ---------------------------------#
  ###############################################################################################################
  sp$pop[, sbp_shift := {                 # sp$pop[, ... , by = pid]: grouping the operation by pid
    # { ... } operetion per group: inside {}, you can run any custom R code, it will
    # return a vector that gets assigned to 'sbp_shift' for the rows in that group
    
    anchor <- unique(anchor_year)        # This line finds a single anchor year (e.g., year 30) for each person.
    if (is.na(anchor)) {                 # If no anchor is found, (This is useful to avoid breaking your logic when a person doesn't meet your anchor condition)
      sbp_curr_xps                       # the function simply returns the original sbp_curr_xps as-is
    } else {                             # If yes anchor is found, You shift the sbp_curr_xps backward in time (type='lag')
      shift(sbp_curr_xps, type = "lag", n = 3) # by 3 years: 3 years is the duration of efficacy inputs (decrease in SBP and rebound)
    }
  }, by = pid] #Works!
  
  ###############################################################################################################
  
  # a variable for new weight after applying efficacy input during treatment influenced period
  sp$pop[, new_sbp := {
    anchor <- unique(anchor_year)                             # This line finds a single anchor year for each person.
    if (is.na(anchor)) {                                      # If no anchor is found,
      sbp_curr_xps                                            # the function simply returns the original sbp_curr_xps as-is
    } else {                                                  # If yes anchor is found,
      fcase(                                                  # We need to assign value to the 'new_sbp' based on the time periods
        year < anchor,                       sbp_curr_xps,    # Before anchor year/uptake: original values (including baseline year)
        year >= anchor & year <= anchor + 2, baseline_sbp + sbp_delta,  # During the 2 years where treatment effect manifest
        year >= anchor + 3,                  sbp_shift        # After sbp rebound to baseline: revert to the original sbp_curr_xps
      )
    }
  }, by = pid]
  
  # Overwriting the original sbp exposure column with the newly created sbp column
  sp$pop[, sbp_curr_xps := new_sbp]
  
  
  ########################################################################################
  #-------------------------------        Tchol         ---------------------------------#
  #-------------------      Nothing happens to Tchol in sc0        ----------------------#
  ########################################################################################
  sp$pop[, c("tchol_delta") := 0]
  
  ##################### Get rid of unnecessary variables ##########################
  # Delete unnecessary variables from synthpop #
  sp$pop[, c("rankstat_sbp", "rankstat_tchol", "ys_rollout", "uptake_rate",
             "uptake_psyr", "uptake_one", 
             "entry_year", "baseline_bmi", "bmi_shift", "new_bmi",
             "baseline_sbp", "sbp_shift", "new_sbp", "baseline_tchol",
             "tchol_shift", "new_tchol") := NULL]

}

#scenario_0_fn(sp)
##############################################################################################

##################################################################################################################
#----------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------   Scenario 2    ------------------------------------------------#
#---------------------   Semaglutide 2.4mg for CEA: uptake 100% at 2025, 2 years treatment   --------------------#
#----------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------#
##################################################################################################################

scenario_2_fn <- function(sp) {
  
  ###########################################################################################################
  #------------------------------------- Loading uncertainty files -----------------------------------------#
  # Efficacy for bmi at 1st year after treatment
  tbl <- read_fst("./inputs/other_parameters/bmi_1y_samples_semag.fst", as.data.table = TRUE)
  bmi_1y <- as.numeric(tbl[mc == sp$mc_aggr, "bmi_1y"])
  
  # Efficacy for bmi at 2nd year after treatment
  tbl <- read_fst("./inputs/other_parameters/bmi_2y_samples_semag.fst", as.data.table = TRUE)
  bmi_2y <- as.numeric(tbl[mc == sp$mc_aggr, "bmi_2y"])
  
  # Efficacy for bmi at 3rd year after treatment
  tbl <- read_fst("./inputs/other_parameters/bmi_3y_samples_semag.fst", as.data.table = TRUE)
  bmi_3y <- as.numeric(tbl[mc == sp$mc_aggr, "bmi_3y"])
  
  # Efficacy for bmi at 4th year after treatment
  tbl <- read_fst("./inputs/other_parameters/bmi_4y_samples_semag.fst", as.data.table = TRUE)
  bmi_4y <- as.numeric(tbl[mc == sp$mc_aggr, "bmi_4y"])
  
  # Efficacy for SBP at 1st year after treatment
  tbl <- read_fst("./inputs/other_parameters/sbp_1y_samples_semag.fst", as.data.table = TRUE)
  sbp_1y <- as.numeric(tbl[mc == sp$mc_aggr, "sbp_1y"])
  
  # Efficacy for SBP at 2nd year of treatment
  tbl <- read_fst("./inputs/other_parameters/sbp_2y_samples_semag.fst", as.data.table = TRUE)
  sbp_2y <- as.numeric(tbl[mc == sp$mc_aggr, "sbp_2y"])
  
  # Efficacy for total cholesterol at 1st year of treatment
  tbl <- read_fst("./inputs/other_parameters/tchol_1y_samples_semag.fst", as.data.table = TRUE)
  tchol_1y <- as.numeric(tbl[mc == sp$mc_aggr, "tchol_1y"])
  
  # Efficacy for total cholesterol at 2nd year of treatment
  tbl <- read_fst("./inputs/other_parameters/tchol_2y_samples_semag.fst", as.data.table = TRUE)
  tchol_2y <- as.numeric(tbl[mc == sp$mc_aggr, "tchol_2y"])
  ###########################################################################################################
  
  # Load the pids of uptake patients for each iteration
  persons_cea <- fread(paste0("./inputs/uptake/", sp$mc_aggr, "_uptake_cea.csv"))
  ### fread() is highly optimized: it’s faster than read_csv(), and it returns a data.table automatically.
  
  # Join uptake info back to lifecourse rows
  sp$pop <- merge(sp$pop, persons_cea, by = "pid", all.x = TRUE)
  
  # Create a row-level indicator for uptake event (1 only in the uptake year row)
  sp$pop[, uptake_one := as.integer(!is.na(uptake_year) & year == uptake_year)]
  # --- !is.na(uptake_year): checks whether the uptake_year column is no NA, returns 'TRUE' or 'FALSE'
  # --- year == uptake_year: checks whether the value in 'year' column is equal to the value in 'uptake_year' column.
  #                          also returns 'TRUE' or 'FALSE'
  # --- as.integer(...):     converts the result of the logical expression into an integer.
  #                          In R, as.integer(TRUE) becomes 1 and as.integer(False) becomes 0.
  
  # a variable for the year someone uptaking the drug
  sp$pop[, anchor_year:=year[uptake_one == 1], by = pid]
  
  # a variable for the year someone enters the synthetic population
  # sp$pop[, entry_year:= min(year), by = pid]
  
  # a variable for theorectical treatment trajectory
  ### Step 0: Initialize the column
  sp$pop[, trtm_theo := NA_integer_]
  ### Step 1: Identify first year of treatment uptake
  sp$pop[uptake_one == 1, trtm_theo := 0]
  ### Step 2: Fill the sequence in subsequent years
  sp$pop[, trtm_theo := fifelse(cumsum(!is.na(trtm_theo)) > 0,   # cumsum(): computes a cumulative count of non-NA entries:
                                # fill in a sequence starting from the first non-NA value
                                seq_len(.N) - min(which(!is.na(trtm_theo))),
                                NA_integer_),
         by = pid]
  ### Step 3: Replace NAs with 0
  sp$pop[, trtm_theo := fifelse(is.na(trtm_theo), 0, trtm_theo)]
  
  ########################################################################################
  ################################          BMI         ##################################
  ########################################################################################
  # a variable for the change in BMI after the uptake of semaglutide
  sp$pop[, `:=`(
    bmi_delta = fcase(
      ### All values until year 5, are % weight loss relative to baseline
      trtm_theo == 0, 0,       ### Baseline year, the year patients become eligible
      trtm_theo == 1, bmi_1y,  ### replace '-0.149' with 'bmi_1y', and link to the uncertainty file: mc + bmi_1y
      ### 1st year of treatment: ITT effects from STEP 1 (1 year f/u): 14.9% weight loss
      trtm_theo == 2, bmi_2y,  ### replace '-0.152' with 'bmi_2y', and link to the uncertainty file: mc + bmi_2y
      ### 2nd year of treatment: ITT effects from STEP 5 (2 year f/u): 15.2% weight loss
      trtm_theo == 3, bmi_3y,  ### STEP 1 extension: regain 2/3 of weight loss in 1st year treatment cessation
      ###
      trtm_theo == 4, bmi_4y,  ### weight regain at 2nd year treatment cessation (uncertainty?)
      trtm_theo >= 5, 0        ### weight return to baseline weight by 3rd year treatment cessation (uncertainty?)
      ### The efficacy inputs are assigned until here, the rest we will revert to the original exposure column (bmi_curr_xps)
    ))
  ]
  
  # a variable for each individual's baseline weight (baseline: year == 24 / ys_rollout == 0)
  # *but, some individuals in the dataset does not have entry at baseline_yr, they came in later than year 24
  # *and, some individuals came in later than year 24, and because they have a chance to uptake drug begining year 25
  # *they might already uptake the drug at the first year, which means they won't have a year (anchor-1),
  # *so what shall we use as a baseline BMI value here...
  sp$pop[, baseline_bmi := {              # sp$pop[, ... , by = pid]: grouping the operation by pid
    anchor <- unique(anchor_year)         # So we can distinguish people who have entry at year 24 and who do not
    if (is.na(anchor)) {                  # If no anchor is found,
      bmi_curr_xps                        # the function simply returns the original bmi_curr_xps as-is
    } else {                              # If yes anchor is found
      bmi_curr_xps[year == (year[trtm_theo == 1][1] - 1)] # baseline_bmi will take the BMI at the year before trtm_theo==1
    }
  }, by = pid] #Works!
  
  ###############################################################################################################
  #----------------------------  Tutorial: if_else function inside data.table  ---------------------------------#
  ###############################################################################################################
  sp$pop[, bmi_shift := {             # sp$pop[, ... , by = pid]: grouping the operation by pid
    # { ... } operetion per group: inside {}, you can run any custom R code, it will
    # return a vector that gets assigned to 'bmi_shift' for the rows in that group
    anchor <- unique(anchor_year)     # This line finds a single anchor year (e.g., year 30) for each person.
    if (is.na(anchor)) {              # If no anchor is found (to avoid breaking your logic when a person doesn't uptake the drug)
      bmi_curr_xps                    # the function simply returns the original bmi_curr_xps as-is
    } else {                          # If yes anchor is found, You shift the bmi_curr_xps backward in time (type='lag')
      shift(bmi_curr_xps, type = "lag", n = 5) # by 5 years: 5 years is the duration of efficacy inputs (weight loss and reboud)
    }
  }, by = pid] #Works!
  ###############################################################################################################
  #--------------------------------------------  Tutorial: End  ------------------------------------------------#
  ###############################################################################################################
  
  # a variable for new weight after applying efficacy input during treatment influenced period
  sp$pop[, new_bmi := {
    anchor <- unique(anchor_year)                             # This line finds a single anchor year for each person.
    if (is.na(anchor)) {                                      # If no anchor is found,
      bmi_curr_xps                                            # the function simply returns the original bmi_curr_xps as-is
    } else {                                                  # If yes anchor is found,
      fcase(                                                  # We need to assign value to the' new_bmi' based on the time periods
        year < anchor,                       bmi_curr_xps,    # Before anchor year/uptake: original values (including baseline year)
        year >= anchor & year <= anchor + 4, baseline_bmi*(1 + bmi_delta),  # During the 5 years where treatment effects manifest
        year > anchor + 4,                   bmi_shift        # After rebound to baseline: revert to the original bmi_curr_xps
      )
    }
  }, by = pid]
  
  # Overwriting the original bmi exposure column with the newly created bmi column
  sp$pop[, bmi_curr_xps := new_bmi]
  
  ########################################################################################
  ################################          SBP         ##################################
  ########################################################################################
  # a variable for the change in SBP after the uptake of semaglutide
  sp$pop[, `:=`(
    sbp_delta = fcase(
      ### All values until year 5, are % weight loss relative to baseline
      trtm_theo == 0, 0,       ### Baseline year, the year patients become eligible
      trtm_theo == 1, sbp_1y,    ### 1st year of treatment: ITT effects from STEP 1 (1 year f/u): mean decrease of 6.2 mmHg from baseline
      trtm_theo == 2, sbp_2y,    ### 2nd year of treatment: ITT effects from STEP 5 (2 year f/u): mean decrease of 5.7 mmHg from baseline
      trtm_theo >= 3, 0        ### STEP 1 extension: assume mean SBP will revert to baseline level after one year without treatment
      ### The efficacy inputs are assigned until here, the rest we will revert to the original exposure column (sbp_curr_xps)
    ))
  ]
  
  # a variable for each individual's baseline SBP (baseline: year == 24 / ys_rollout == 0)
  # *but, some individuals in the dataset does not have entry at baseline_yr, they came in later than year 24
  sp$pop[, baseline_sbp := {                # sp$pop[, ... , by = pid]: grouping the operation by pid
    anchor <- unique(anchor_year)           # So we can distinguish people who have entry at year 24 and who do not
    if (is.na(anchor)) {                    # If no anchor is found,
      sbp_curr_xps                          # the function simply returns the original sbp_curr_xps as-is
    } else {                                # If yes anchor is found, baseline_sbp will take the sbp at year 24
      sbp_curr_xps[year == (year[trtm_theo == 1][1] - 1)]  # The baseline year will be (anchor - 1): one year before uptake
    }
  }, by = pid] #Works!
  
  ###############################################################################################################
  #----------------------------  Tutorial: if_else function inside data.table  ---------------------------------#
  ###############################################################################################################
  sp$pop[, sbp_shift := {                 # sp$pop[, ... , by = pid]: grouping the operation by pid
    # { ... } operetion per group: inside {}, you can run any custom R code, it will
    # return a vector that gets assigned to 'sbp_shift' for the rows in that group
    
    anchor <- unique(anchor_year)        # This line finds a single anchor year (e.g., year 30) for each person.
    if (is.na(anchor)) {                 # If no anchor is found, (This is useful to avoid breaking your logic when a person doesn't meet your anchor condition)
      sbp_curr_xps                       # the function simply returns the original sbp_curr_xps as-is
    } else {                             # If yes anchor is found, You shift the sbp_curr_xps backward in time (type='lag')
      shift(sbp_curr_xps, type = "lag", n = 3) # by 3 years: 3 years is the duration of efficacy inputs (decrease in SBP and rebound)
    }
  }, by = pid] #Works!
  
  #  A if_else function inside of :={} for creating a new variable
  #  if(condition) {
  #   what should happen if condition is TRUE
  #  } else {
  #   what should happen if condition is FALSE
  #  }
  ###############################################################################################################
  #--------------------------------------------  Tutorial: End  ------------------------------------------------#
  ###############################################################################################################
  
  # a variable for new weight after applying efficacy input during treatment influenced period
  sp$pop[, new_sbp := {
    anchor <- unique(anchor_year)                             # This line finds a single anchor year for each person.
    if (is.na(anchor)) {                                      # If no anchor is found,
      sbp_curr_xps                                            # the function simply returns the original sbp_curr_xps as-is
    } else {                                                  # If yes anchor is found,
      fcase(                                                  # We need to assign value to the 'new_sbp' based on the time periods
        year < anchor,                       sbp_curr_xps,    # Before anchor year/uptake: original values (including baseline year)
        year >= anchor & year <= anchor + 2, baseline_sbp + sbp_delta,  # During the 2 years where treatment effect manifest
        year >= anchor + 3,                  sbp_shift        # After sbp rebound to baseline: revert to the original sbp_curr_xps
      )
    }
  }, by = pid]
  
  # Overwriting the original sbp exposure column with the newly created sbp column
  sp$pop[, sbp_curr_xps := new_sbp]
  
  ########################################################################################
  ###############################          Tchol         #################################
  ########################################################################################
  # a variable for the change in SBP after the uptake of semaglutide
  sp$pop[, `:=`(
    tchol_delta = fcase(
      trtm_theo == 0, 0,        ### Baseline year, the year patients become eligible
      trtm_theo == 1, tchol_1y,    ### 1st year of treatment: ITT effects from STEP 1: mean decrease of 3% from baseline
      trtm_theo == 2, tchol_2y,   ### 2nd year of treatment: ITT effects from STEP 5: mean decrease of 3.3% from baseline
      trtm_theo >= 3, 0         ### STEP 1 extension: assume mean tchol will revert to baseline level after one year without treatment
      ### The efficacy inputs are assigned until here, the rest we will revert to the original exposure column (tchol_curr_xps)
    ))
  ]
  
  # a variable for each individual's baseline Tchol (baseline: year == 24 / ys_rollout == 0)
  # *but, some individuals in the dataset does not have entry at baseline_yr, they came in later than year 24
  sp$pop[, baseline_tchol := {                 # sp$pop[, ... , by = pid]: grouping the operation by pid
    anchor <- unique(anchor_year)           # So we can distinguish people who have entry at year 24 and who do not
    if (is.na(anchor)) {                    # If no anchor is found,
      tchol_curr_xps                        # the function simply returns the original tchol_curr_xps as-is
    } else {                                # If yes anchor is found, baseline_tchol will take the tchol at year 24
      tchol_curr_xps[year == (year[trtm_theo == 1][1] - 1)]  # The baseline year will be (anchor - 1): one year before uptake
    }
  }, by = pid] #Works!
  
  ###############################################################################################################
  #----------------------------  Tutorial: if_else function inside data.table  ---------------------------------#
  ###############################################################################################################
  sp$pop[, tchol_shift := {                 # sp$pop[, ... , by = pid]: grouping the operation by pid
    # { ... } operetion per group: inside {}, you can run any custom R code, it will
    # return a vector that gets assigned to 'tchol_shift' for the rows in that group
    
    anchor <- unique(anchor_year)        # This line finds a single anchor year (e.g., year 30) for each person.
    if (is.na(anchor)) {                 # If no anchor is found, (This is useful to avoid breaking your logic when a person doesn't meet your anchor condition)
      tchol_curr_xps                       # the function simply returns the original tchol_curr_xps as-is
    } else {                             # If yes anchor is found, You shift the tchol_curr_xps backward in time (type='lag')
      shift(tchol_curr_xps, type = "lag", n = 3) # by 3 years: 3 years is the duration of efficacy inputs (decrease in tchol and reboud)
    }
  }, by = pid] #Works!
  
  # test <- sp$pop[pid %in% c(156, 175)]
  ###############################################################################################################
  #--------------------------------------------  Tutorial: End  ------------------------------------------------#
  ###############################################################################################################
  
  # a variable for new weight after applying efficacy input during treatment influenced period
  sp$pop[, new_tchol := {
    anchor <- unique(anchor_year)                             # This line finds a single anchor year for each person.
    if (is.na(anchor)) {                                      # If no anchor is found,
      tchol_curr_xps                                          # the function simply returns the original tchol_curr_xps as-is
    } else {                                                  # If yes anchor is found,
      fcase(                                                  # We need to assign value to the 'new_tchol' based on the time periods
        year < anchor,                       tchol_curr_xps,  # Before anchor year/uptake: original values (including baseline year)
        year >= anchor & year <= anchor + 2, baseline_tchol*(1 + tchol_delta),  # During the 2 years where treatment effect manifest
        year >= anchor + 3,                  tchol_shift      # After tchol rebound to baseline: revert to the original tchol_curr_xps
      )
    }
  }, by = pid]
  
  # Overwriting the original tchol exposure column with the newly created tchol column
  sp$pop[, tchol_curr_xps := new_tchol]
  
  ##################### Get rid of unnecessary variables ##########################
  # Delete unnecessary variables from synthpop #
  sp$pop[, c("rankstat_sbp", "rankstat_tchol", "ys_rollout", "uptake_rate",
             "uptake_psyr", "uptake_one", 
             "entry_year", "baseline_bmi", "bmi_shift", "new_bmi",
             "baseline_sbp", "sbp_shift", "new_sbp", "baseline_tchol",
             "tchol_shift", "new_tchol") := NULL]
  
}
##################################################################################################################
#----------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------   Scenario 3    ------------------------------------------------#
#----------------------   Semaglutide 2.4mg for CEA: uptake 100% at 2025, lifetime treatment  -------------------#
#----------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------#
##################################################################################################################
######## Lifetime treatment:--------------------------------------------------------------------------------------
######## 1. Treatment effects:------------------------------------------------------------------------------------
########    We assume that the effects inputs from the 3rd year on will carry forward effect at year 2
########    BMI at Year 1:   -14.9%
########    BMI from Year 2: -15.2%
########    SBP at Year 1:   -6.2 
########    SBP from Year 2: -5.7
########    TCL at Year 1:   -0.03
########    TCL from Year 2: -0.033
######## ---------------------------------------------------------------------------------------------------------

scenario_3_fn <- function(sp) {
  
  ###########################################################################################################
  #------------------------------------- Loading uncertainty files -----------------------------------------#
  #------ For lifetime treatment, we only need the effects at year 1&2, because weight do not rebound ------#
  #---------------------------------------------------------------------------------------------------------#
  # Efficacy for bmi at 1st year after treatment
  tbl <- read_fst("./inputs/other_parameters/bmi_1y_samples_semag.fst", as.data.table = TRUE)
  bmi_1y <- as.numeric(tbl[mc == sp$mc_aggr, "bmi_1y"])
  
  # Efficacy for bmi at 2nd year after treatment
  tbl <- read_fst("./inputs/other_parameters/bmi_2y_samples_semag.fst", as.data.table = TRUE)
  bmi_2y <- as.numeric(tbl[mc == sp$mc_aggr, "bmi_2y"])
  
  # Efficacy for SBP at 1st year after treatment
  tbl <- read_fst("./inputs/other_parameters/sbp_1y_samples_semag.fst", as.data.table = TRUE)
  sbp_1y <- as.numeric(tbl[mc == sp$mc_aggr, "sbp_1y"])
  
  # Efficacy for SBP at 2nd year of treatment
  tbl <- read_fst("./inputs/other_parameters/sbp_2y_samples_semag.fst", as.data.table = TRUE)
  sbp_2y <- as.numeric(tbl[mc == sp$mc_aggr, "sbp_2y"])
  
  # Efficacy for total cholesterol at 1st year of treatment
  tbl <- read_fst("./inputs/other_parameters/tchol_1y_samples_semag.fst", as.data.table = TRUE)
  tchol_1y <- as.numeric(tbl[mc == sp$mc_aggr, "tchol_1y"])
  
  # Efficacy for total cholesterol at 2nd year of treatment
  tbl <- read_fst("./inputs/other_parameters/tchol_2y_samples_semag.fst", as.data.table = TRUE)
  tchol_2y <- as.numeric(tbl[mc == sp$mc_aggr, "tchol_2y"])
  ###########################################################################################################
  
  # Load the pids of uptake patients for each iteration
  persons_cea <- fread(paste0("./inputs/uptake/", sp$mc_aggr, "_uptake_cea.csv"))
  ### fread() is highly optimized: it’s faster than read_csv(), and it returns a data.table automatically.
  
  # Join uptake info back to lifecourse rows
  sp$pop <- merge(sp$pop, persons_cea, by = "pid", all.x = TRUE)
  
  # Create a row-level indicator for uptake event (1 only in the uptake year row)
  sp$pop[, uptake_one := as.integer(!is.na(uptake_year) & year == uptake_year)]
  # --- !is.na(uptake_year): checks whether the uptake_year column is no NA, returns 'TRUE' or 'FALSE'
  # --- year == uptake_year: checks whether the value in 'year' column is equal to the value in 'uptake_year' column.
  #                          also returns 'TRUE' or 'FALSE'
  # --- as.integer(...):     converts the result of the logical expression into an integer.
  #                          In R, as.integer(TRUE) becomes 1 and as.integer(False) becomes 0.
  
  # a variable for the year someone uptaking the drug
  sp$pop[, anchor_year:=year[uptake_one == 1], by = pid]
  
  # a variable for the year someone enters the synthetic population
  # sp$pop[, entry_year:= min(year), by = pid]
  
  # a variable for theorectical treatment trajectory
  ### Step 0: Initialize the column
  sp$pop[, trtm_theo := NA_integer_]
  ### Step 1: Identify first year of treatment uptake
  sp$pop[uptake_one == 1, trtm_theo := 0]
  ### Step 2: Fill the sequence in subsequent years
  sp$pop[, trtm_theo := fifelse(cumsum(!is.na(trtm_theo)) > 0,   
                                # cumsum(): computes a cumulative count of non-NA entries:
                                # fill in a sequence starting from the first non-NA value
                                seq_len(.N) - min(which(!is.na(trtm_theo))),
                                NA_integer_),
         by = pid]
  ### Step 3: Replace NAs with 0
  sp$pop[, trtm_theo := fifelse(is.na(trtm_theo), 0, trtm_theo)]
  
  ########################################################################################
  ################################          BMI         ##################################
  ########################################################################################
  # a variable for the change in BMI after the uptake of semaglutide
  sp$pop[, `:=`(
    bmi_delta = fcase(
      ### All values until year 5, are % weight loss relative to baseline
      trtm_theo == 0, 0,       ### Baseline year, the year patients become eligible
      trtm_theo == 1, bmi_1y,  ### replace '-0.149' with 'bmi_1y', and link to the uncertainty file: mc + bmi_1y
      ### 1st year of treatment: ITT effects from STEP 1 (1 year f/u): 14.9% weight loss
      trtm_theo >= 2, bmi_2y  ### replace '-0.152' with 'bmi_2y', and link to the uncertainty file: mc + bmi_2y
      ### 2nd year of treatment: ITT effects from STEP 5 (2 year f/u): 15.2% weight loss
    ))
  ]
  
  # a variable for each individual's baseline weight (baseline: year == 24 / ys_rollout == 0)
  # *but, some individuals in the dataset does not have entry at baseline_yr, they came in later than year 24
  # *and, some individuals came in later than year 24, and because they have a chance to uptake drug begining year 25
  # *they might already uptake the drug at the first year, which means they won't have a year (anchor-1),
  # *so what shall we use as a baseline BMI value here...
  sp$pop[, baseline_bmi := {              # sp$pop[, ... , by = pid]: grouping the operation by pid
    anchor <- unique(anchor_year)         # So we can distinguish people who have entry at year 24 and who do not
    if (is.na(anchor)) {                  # If no anchor is found,
      bmi_curr_xps                        # the function simply returns the original bmi_curr_xps as-is
    } else {                              # If yes anchor is found
      bmi_curr_xps[year == (year[trtm_theo == 1][1] - 1)] # baseline_bmi will take the BMI at the year before trtm_theo==1
    }
  }, by = pid] #Works!
  
  
  # a variable for new weight after applying efficacy input during treatment influenced period
  sp$pop[, new_bmi := {
    anchor <- unique(anchor_year)                             # This line finds a single anchor year for each person.
    if (is.na(anchor)) {                                      # If no anchor is found,
      bmi_curr_xps                                            # the function simply returns the original bmi_curr_xps as-is
    } else {                                                  # If yes anchor is found,
      fcase(                                                  # We need to assign value to the' new_bmi' based on the time periods
        year < anchor,      bmi_curr_xps,                   # Before anchor year/uptake: original values (including baseline year)
        year >= anchor,     baseline_bmi*(1 + bmi_delta)    # After anchor year, the lifetime treatment lead to lifetime effects
      )
    }
  }, by = pid]
  
  # Overwriting the original bmi exposure column with the newly created bmi column
  sp$pop[, bmi_curr_xps := new_bmi]
  
  ########################################################################################
  ################################          SBP         ##################################
  ########################################################################################
  # a variable for the change in SBP after the uptake of semaglutide
  sp$pop[, `:=`(
    sbp_delta = fcase(
      ### All values until year 5, are % weight loss relative to baseline
      trtm_theo == 0, 0,       ### Baseline year, the year patients become eligible
      trtm_theo == 1, sbp_1y,    ### 1st year of treatment: ITT effects from STEP 1 (1 year f/u): mean decrease of 6.2 mmHg from baseline
      trtm_theo >= 2, sbp_2y     ### 2nd year of treatment: ITT effects from STEP 5 (2 year f/u): mean decrease of 5.7 mmHg from baseline
    ))
  ]
  
  # a variable for each individual's baseline SBP (baseline: year == 24 / ys_rollout == 0)
  # *but, some individuals in the dataset does not have entry at baseline_yr, they came in later than year 24
  sp$pop[, baseline_sbp := {                # sp$pop[, ... , by = pid]: grouping the operation by pid
    anchor <- unique(anchor_year)           # So we can distinguish people who have entry at year 24 and who do not
    if (is.na(anchor)) {                    # If no anchor is found,
      sbp_curr_xps                          # the function simply returns the original sbp_curr_xps as-is
    } else {                                # If yes anchor is found, baseline_sbp will take the sbp at year 24
      sbp_curr_xps[year == (year[trtm_theo == 1][1] - 1)]  # The baseline year will be (anchor - 1): one year before uptake
    }
  }, by = pid] #Works!
  
  # a variable for new weight after applying efficacy input during treatment influenced period
  sp$pop[, new_sbp := {
    anchor <- unique(anchor_year)                           # This line finds a single anchor year for each person.
    if (is.na(anchor)) {                                    # If no anchor is found,
      sbp_curr_xps                                          # the function simply returns the original sbp_curr_xps as-is
    } else {                                                # If yes anchor is found,
      fcase(                                                # We need to assign value to the 'new_sbp' based on the time periods
        year < anchor,  sbp_curr_xps,              # Before anchor year/uptake: original values (including baseline year)
        year >= anchor, baseline_sbp + sbp_delta  # After anchor year, the lifetime treatment lead to lifetime effects
      )
    }
  }, by = pid]
  
  # Overwriting the original sbp exposure column with the newly created sbp column
  sp$pop[, sbp_curr_xps := new_sbp]
  
  ########################################################################################
  ###############################          Tchol         #################################
  ########################################################################################
  # a variable for the change in SBP after the uptake of semaglutide
  sp$pop[, `:=`(
    tchol_delta = fcase(
      trtm_theo == 0, 0,          ### Baseline year, the year patients become eligible
      trtm_theo == 1, tchol_1y,   ### 1st year of treatment: ITT effects from STEP 1: mean decrease of 3% from baseline
      trtm_theo >= 2, tchol_2y    ### 2nd year of treatment: ITT effects from STEP 5: mean decrease of 3.3% from baseline
    ))
  ]
  
  # a variable for each individual's baseline Tchol (baseline: year == 24 / ys_rollout == 0)
  # *but, some individuals in the dataset does not have entry at baseline_yr, they came in later than year 24
  sp$pop[, baseline_tchol := {                 # sp$pop[, ... , by = pid]: grouping the operation by pid
    anchor <- unique(anchor_year)           # So we can distinguish people who have entry at year 24 and who do not
    if (is.na(anchor)) {                    # If no anchor is found,
      tchol_curr_xps                        # the function simply returns the original tchol_curr_xps as-is
    } else {                                # If yes anchor is found, baseline_tchol will take the tchol at year 24
      tchol_curr_xps[year == (year[trtm_theo == 1][1] - 1)]  # The baseline year will be (anchor - 1): one year before uptake
    }
  }, by = pid] #Works!
  
  # a variable for new weight after applying efficacy input during treatment influenced period
  sp$pop[, new_tchol := {
    anchor <- unique(anchor_year)                             # This line finds a single anchor year for each person.
    if (is.na(anchor)) {                                      # If no anchor is found,
      tchol_curr_xps                                          # the function simply returns the original tchol_curr_xps as-is
    } else {                                                  # If yes anchor is found,
      fcase(                                                  # We need to assign value to the 'new_tchol' based on the time periods
        year < anchor,    tchol_curr_xps,                  # Before anchor year/uptake: original values (including baseline year)
        year >= anchor,   baseline_tchol*(1 + tchol_delta) # After anchor year, the lifetime treatment lead to lifetime effects
      )
    }
  }, by = pid]
  
  # Overwriting the original tchol exposure column with the newly created tchol column
  sp$pop[, tchol_curr_xps := new_tchol]
  ### Jane Aug 2025, test scenario on sp$pop --> Looks like until here everything worked!!!
  
  ##################### Get rid of unnecessary variables ##########################
  # Delete unnecessary variables from synthpop #
  sp$pop[, c("rankstat_sbp", "rankstat_tchol", "ys_rollout", "uptake_rate",
             "uptake_psyr", "uptake_one", 
             "entry_year", "baseline_bmi", "bmi_shift", "new_bmi",
             "baseline_sbp", "sbp_shift", "new_sbp", "baseline_tchol",
             "tchol_shift", "new_tchol") := NULL]
  
}

##################################################################################################################
#----------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------   Scenario 4    ------------------------------------------------#
#----------------------   Tirzepatide 15mg for CEA: uptake 100% at 2025, 2 years treatment   --------------------#
#----------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------#
##################################################################################################################

scenario_4_fn <- function(sp) {
  
  ###########################################################################################################
  #------------------------------------- Loading uncertainty files -----------------------------------------#
  # Efficacy for bmi at 1st year after treatment
  tbl <- read_fst("./inputs/other_parameters/bmi_1y_samples_tirzp.fst", as.data.table = TRUE)
  bmi_1y <- as.numeric(tbl[mc == sp$mc_aggr, "bmi_1y"])
  
  # Efficacy for bmi at 2nd year after treatment
  tbl <- read_fst("./inputs/other_parameters/bmi_1y_samples_tirzp.fst", as.data.table = TRUE)
  bmi_2y <- as.numeric(tbl[mc == sp$mc_aggr, "bmi_1y"])
  
  # Efficacy for bmi at 3rd year after treatment
  tbl <- read_fst("./inputs/other_parameters/bmi_3y_samples_tirzp.fst", as.data.table = TRUE)
  bmi_3y <- as.numeric(tbl[mc == sp$mc_aggr, "bmi_3y"])
  
  # Efficacy for bmi at 4th year after treatment
  tbl <- read_fst("./inputs/other_parameters/bmi_4y_samples_tirzp.fst", as.data.table = TRUE)
  bmi_4y <- as.numeric(tbl[mc == sp$mc_aggr, "bmi_4y"])
  
  # Efficacy for SBP at 1st year after treatment
  tbl <- read_fst("./inputs/other_parameters/sbp_1y_samples_tirzp.fst", as.data.table = TRUE)
  sbp_1y <- as.numeric(tbl[mc == sp$mc_aggr, "sbp_1y"])
  
  # Efficacy for SBP at 2nd year of treatment
  tbl <- read_fst("./inputs/other_parameters/sbp_1y_samples_tirzp.fst", as.data.table = TRUE)
  sbp_2y <- as.numeric(tbl[mc == sp$mc_aggr, "sbp_1y"])
  
  # Efficacy for total cholesterol at 1st year of treatment
  tbl <- read_fst("./inputs/other_parameters/tchol_1y_samples_tirzp.fst", as.data.table = TRUE)
  tchol_1y <- as.numeric(tbl[mc == sp$mc_aggr, "tchol_1y"])
  
  # Efficacy for total cholesterol at 2nd year of treatment
  tbl <- read_fst("./inputs/other_parameters/tchol_1y_samples_tirzp.fst", as.data.table = TRUE)
  tchol_2y <- as.numeric(tbl[mc == sp$mc_aggr, "tchol_1y"])
  ###########################################################################################################
  
  # Load the pids of uptake patients for each iteration
  persons_cea <- fread(paste0("./inputs/uptake/", sp$mc_aggr, "_uptake_cea.csv"))
  ### fread() is highly optimized: it’s faster than read_csv(), and it returns a data.table automatically.
  
  # Join uptake info back to lifecourse rows
  sp$pop <- merge(sp$pop, persons_cea, by = "pid", all.x = TRUE)
  
  # Create a row-level indicator for uptake event (1 only in the uptake year row)
  sp$pop[, uptake_one := as.integer(!is.na(uptake_year) & year == uptake_year)]
  # --- !is.na(uptake_year): checks whether the uptake_year column is no NA, returns 'TRUE' or 'FALSE'
  # --- year == uptake_year: checks whether the value in 'year' column is equal to the value in 'uptake_year' column.
  #                          also returns 'TRUE' or 'FALSE'
  # --- as.integer(...):     converts the result of the logical expression into an integer.
  #                          In R, as.integer(TRUE) becomes 1 and as.integer(False) becomes 0.
  
  # a variable for the year someone uptaking the drug
  sp$pop[, anchor_year:=year[uptake_one == 1], by = pid]
  
  # a variable for the year someone enters the synthetic population
  # sp$pop[, entry_year:= min(year), by = pid]
  
  # a variable for theorectical treatment trajectory
  ### Step 0: Initialize the column
  sp$pop[, trtm_theo := NA_integer_]
  ### Step 1: Identify first year of treatment uptake
  sp$pop[uptake_one == 1, trtm_theo := 0]
  ### Step 2: Fill the sequence in subsequent years
  sp$pop[, trtm_theo := fifelse(cumsum(!is.na(trtm_theo)) > 0,   # cumsum(): computes a cumulative count of non-NA entries:
                                # fill in a sequence starting from the first non-NA value
                                seq_len(.N) - min(which(!is.na(trtm_theo))),
                                NA_integer_),
         by = pid]
  ### Step 3: Replace NAs with 0
  sp$pop[, trtm_theo := fifelse(is.na(trtm_theo), 0, trtm_theo)]
  
  ########################################################################################
  ################################          BMI         ##################################
  ########################################################################################
  # a variable for the change in BMI after the uptake of Tirzepatide
  sp$pop[, `:=`(
    bmi_delta = fcase(
      ### All values until year 5, are % weight loss relative to baseline
      trtm_theo == 0, 0,       ### Baseline year, the year patients become eligible
      trtm_theo == 1, bmi_1y,  ### replace '-0.209' with 'bmi_1y', and link to the uncertainty file: mc + bmi_1y
      ### 1st year of treatment: ITT effects from SURMOUNT-1 (1 year f/u): 20.9% weight loss
      trtm_theo == 2, bmi_2y,  ### replace '-0.209' with 'bmi_2y', and link to the uncertainty file: mc + bmi_2y
      ### 2nd year of treatment: ITT effects from SURMOUNT-1 (2 year f/u): 20.9% weight loss
      trtm_theo == 3, bmi_3y,  ### SURMOUNT-4: regain ~2/3 of weight loss in 1st year treatment cessation
      ###
      trtm_theo == 4, bmi_4y,  ### weight regain at 2nd year treatment cessation
      trtm_theo >= 5, 0        ### weight return to baseline weight by 3rd year treatment cessation
      ### The efficacy inputs are assigned until here, the rest we will revert to the original exposure column (bmi_curr_xps)
    ))
  ]
  
  # a variable for each individual's baseline weight (baseline: year == 24 / ys_rollout == 0)
  # *but, some individuals in the dataset does not have entry at baseline_yr, they came in later than year 24
  # *and, some individuals came in later than year 24, and because they have a chance to uptake drug begining year 25
  # *they might already uptake the drug at the first year, which means they won't have a year (anchor-1),
  # *so what shall we use as a baseline BMI value here...
  sp$pop[, baseline_bmi := {              # sp$pop[, ... , by = pid]: grouping the operation by pid
    anchor <- unique(anchor_year)         # So we can distinguish people who have entry at year 24 and who do not
    if (is.na(anchor)) {                  # If no anchor is found,
      bmi_curr_xps                        # the function simply returns the original bmi_curr_xps as-is
    } else {                              # If yes anchor is found
      bmi_curr_xps[year == (year[trtm_theo == 1][1] - 1)] # baseline_bmi will take the BMI at the year before trtm_theo==1
    }
  }, by = pid] #Works!
  
  ###############################################################################################################
  #----------------------------  Tutorial: if_else function inside data.table  ---------------------------------#
  ###############################################################################################################
  sp$pop[, bmi_shift := {             # sp$pop[, ... , by = pid]: grouping the operation by pid
    # { ... } operetion per group: inside {}, you can run any custom R code, it will
    # return a vector that gets assigned to 'bmi_shift' for the rows in that group
    anchor <- unique(anchor_year)     # This line finds a single anchor year (e.g., year 30) for each person.
    if (is.na(anchor)) {              # If no anchor is found (to avoid breaking your logic when a person doesn't uptake the drug)
      bmi_curr_xps                    # the function simply returns the original bmi_curr_xps as-is
    } else {                          # If yes anchor is found, You shift the bmi_curr_xps backward in time (type='lag')
      shift(bmi_curr_xps, type = "lag", n = 5) # by 5 years: 5 years is the duration of efficacy inputs (weight loss and reboud)
    }
  }, by = pid] #Works!
  ###############################################################################################################
  #--------------------------------------------  Tutorial: End  ------------------------------------------------#
  ###############################################################################################################
  
  # a variable for new weight after applying efficacy input during treatment influenced period
  sp$pop[, new_bmi := {
    anchor <- unique(anchor_year)                             # This line finds a single anchor year for each person.
    if (is.na(anchor)) {                                      # If no anchor is found,
      bmi_curr_xps                                            # the function simply returns the original bmi_curr_xps as-is
    } else {                                                  # If yes anchor is found,
      fcase(                                                  # We need to assign value to the' new_bmi' based on the time periods
        year < anchor,                       bmi_curr_xps,    # Before anchor year/uptake: original values (including baseline year)
        year >= anchor & year <= anchor + 4, baseline_bmi*(1 + bmi_delta),  # During the 5 years where treatment effects manifest
        year > anchor + 4,                   bmi_shift        # After rebound to baseline: revert to the original bmi_curr_xps
      )
    }
  }, by = pid]
  
  # Overwriting the original bmi exposure column with the newly created bmi column
  sp$pop[, bmi_curr_xps := new_bmi]
  
  ########################################################################################
  ################################          SBP         ##################################
  ########################################################################################
  # a variable for the change in SBP after the uptake of semaglutide
  sp$pop[, `:=`(
    sbp_delta = fcase(
      ### All values until year 5, are % weight loss relative to baseline
      trtm_theo == 0, 0,       ### Baseline year, the year patients become eligible
      trtm_theo == 1, sbp_1y,    ### 1st year of treatment: ITT effects from STEP 1 (1 year f/u): mean decrease of 6.2 mmHg from baseline
      trtm_theo == 2, sbp_2y,    ### 2nd year of treatment: ITT effects from STEP 5 (2 year f/u): mean decrease of 5.7 mmHg from baseline
      trtm_theo >= 3, 0        ### STEP 1 extension: assume mean SBP will revert to baseline level after one year without treatment
      ### The efficacy inputs are assigned until here, the rest we will revert to the original exposure column (sbp_curr_xps)
    ))
  ]
  
  # a variable for each individual's baseline SBP (baseline: year == 24 / ys_rollout == 0)
  # *but, some individuals in the dataset does not have entry at baseline_yr, they came in later than year 24
  sp$pop[, baseline_sbp := {                # sp$pop[, ... , by = pid]: grouping the operation by pid
    anchor <- unique(anchor_year)           # So we can distinguish people who have entry at year 24 and who do not
    if (is.na(anchor)) {                    # If no anchor is found,
      sbp_curr_xps                          # the function simply returns the original sbp_curr_xps as-is
    } else {                                # If yes anchor is found, baseline_sbp will take the sbp at year 24
      sbp_curr_xps[year == (year[trtm_theo == 1][1] - 1)]  # The baseline year will be (anchor - 1): one year before uptake
    }
  }, by = pid] #Works!
  
  ###############################################################################################################
  #----------------------------  Tutorial: if_else function inside data.table  ---------------------------------#
  ###############################################################################################################
  sp$pop[, sbp_shift := {                 # sp$pop[, ... , by = pid]: grouping the operation by pid
    # { ... } operetion per group: inside {}, you can run any custom R code, it will
    # return a vector that gets assigned to 'sbp_shift' for the rows in that group
    
    anchor <- unique(anchor_year)        # This line finds a single anchor year (e.g., year 30) for each person.
    if (is.na(anchor)) {                 # If no anchor is found, (This is useful to avoid breaking your logic when a person doesn't meet your anchor condition)
      sbp_curr_xps                       # the function simply returns the original sbp_curr_xps as-is
    } else {                             # If yes anchor is found, You shift the sbp_curr_xps backward in time (type='lag')
      shift(sbp_curr_xps, type = "lag", n = 3) # by 3 years: 3 years is the duration of efficacy inputs (decrease in SBP and rebound)
    }
  }, by = pid] #Works!
  
  #  A if_else function inside of :={} for creating a new variable
  #  if(condition) {
  #   what should happen if condition is TRUE
  #  } else {
  #   what should happen if condition is FALSE
  #  }
  ###############################################################################################################
  #--------------------------------------------  Tutorial: End  ------------------------------------------------#
  ###############################################################################################################
  
  # a variable for new weight after applying efficacy input during treatment influenced period
  sp$pop[, new_sbp := {
    anchor <- unique(anchor_year)                             # This line finds a single anchor year for each person.
    if (is.na(anchor)) {                                      # If no anchor is found,
      sbp_curr_xps                                            # the function simply returns the original sbp_curr_xps as-is
    } else {                                                  # If yes anchor is found,
      fcase(                                                  # We need to assign value to the 'new_sbp' based on the time periods
        year < anchor,                       sbp_curr_xps,    # Before anchor year/uptake: original values (including baseline year)
        year >= anchor & year <= anchor + 2, baseline_sbp + sbp_delta,  # During the 2 years where treatment effect manifest
        year >= anchor + 3,                  sbp_shift        # After sbp rebound to baseline: revert to the original sbp_curr_xps
      )
    }
  }, by = pid]
  
  # Overwriting the original sbp exposure column with the newly created sbp column
  sp$pop[, sbp_curr_xps := new_sbp]
  
  ########################################################################################
  ###############################          Tchol         #################################
  ########################################################################################
  # a variable for the change in SBP after the uptake of semaglutide
  sp$pop[, `:=`(
    tchol_delta = fcase(
      trtm_theo == 0, 0,        ### Baseline year, the year patients become eligible
      trtm_theo == 1, tchol_1y,    ### 1st year of treatment: ITT effects from STEP 1: mean decrease of 3% from baseline
      trtm_theo == 2, tchol_2y,   ### 2nd year of treatment: ITT effects from STEP 5: mean decrease of 3.3% from baseline
      trtm_theo >= 3, 0         ### STEP 1 extension: assume mean tchol will revert to baseline level after one year without treatment
      ### The efficacy inputs are assigned until here, the rest we will revert to the original exposure column (tchol_curr_xps)
    ))
  ]
  
  # a variable for each individual's baseline Tchol (baseline: year == 24 / ys_rollout == 0)
  # *but, some individuals in the dataset does not have entry at baseline_yr, they came in later than year 24
  sp$pop[, baseline_tchol := {                 # sp$pop[, ... , by = pid]: grouping the operation by pid
    anchor <- unique(anchor_year)           # So we can distinguish people who have entry at year 24 and who do not
    if (is.na(anchor)) {                    # If no anchor is found,
      tchol_curr_xps                        # the function simply returns the original tchol_curr_xps as-is
    } else {                                # If yes anchor is found, baseline_tchol will take the tchol at year 24
      tchol_curr_xps[year == (year[trtm_theo == 1][1] - 1)]  # The baseline year will be (anchor - 1): one year before uptake
    }
  }, by = pid] #Works!
  
  ###############################################################################################################
  #----------------------------  Tutorial: if_else function inside data.table  ---------------------------------#
  ###############################################################################################################
  sp$pop[, tchol_shift := {                 # sp$pop[, ... , by = pid]: grouping the operation by pid
    # { ... } operetion per group: inside {}, you can run any custom R code, it will
    # return a vector that gets assigned to 'tchol_shift' for the rows in that group
    
    anchor <- unique(anchor_year)        # This line finds a single anchor year (e.g., year 30) for each person.
    if (is.na(anchor)) {                 # If no anchor is found, (This is useful to avoid breaking your logic when a person doesn't meet your anchor condition)
      tchol_curr_xps                       # the function simply returns the original tchol_curr_xps as-is
    } else {                             # If yes anchor is found, You shift the tchol_curr_xps backward in time (type='lag')
      shift(tchol_curr_xps, type = "lag", n = 3) # by 3 years: 3 years is the duration of efficacy inputs (decrease in tchol and reboud)
    }
  }, by = pid] #Works!
  
  # test <- sp$pop[pid %in% c(156, 175)]
  ###############################################################################################################
  #--------------------------------------------  Tutorial: End  ------------------------------------------------#
  ###############################################################################################################
  
  # a variable for new weight after applying efficacy input during treatment influenced period
  sp$pop[, new_tchol := {
    anchor <- unique(anchor_year)                             # This line finds a single anchor year for each person.
    if (is.na(anchor)) {                                      # If no anchor is found,
      tchol_curr_xps                                          # the function simply returns the original tchol_curr_xps as-is
    } else {                                                  # If yes anchor is found,
      fcase(                                                  # We need to assign value to the 'new_tchol' based on the time periods
        year < anchor,                       tchol_curr_xps,  # Before anchor year/uptake: original values (including baseline year)
        year >= anchor & year <= anchor + 2, baseline_tchol*(1 + tchol_delta),  # During the 2 years where treatment effect manifest
        year >= anchor + 3,                  tchol_shift      # After tchol rebound to baseline: revert to the original tchol_curr_xps
      )
    }
  }, by = pid]
  
  # Overwriting the original tchol exposure column with the newly created tchol column
  sp$pop[, tchol_curr_xps := new_tchol]
  
  ##################### Get rid of unnecessary variables ##########################
  # Delete unnecessary variables from synthpop #
  sp$pop[, c("rankstat_sbp", "rankstat_tchol", "ys_rollout", "uptake_rate",
             "uptake_psyr", "uptake_one", 
             "entry_year", "baseline_bmi", "bmi_shift", "new_bmi",
             "baseline_sbp", "sbp_shift", "new_sbp", "baseline_tchol",
             "tchol_shift", "new_tchol") := NULL]
  
}
##################################################################################################################
#----------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------   Scenario 5    ------------------------------------------------#
#----------------------   Tirzepatide 15mg for CEA: uptake 100% at 2025, lifetime treatment  --------------------#
#----------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------#
##################################################################################################################
######## Lifetime treatment:--------------------------------------------------------------------------------------
######## 1. Treatment effects:------------------------------------------------------------------------------------
########    We assume that the effects inputs from the 3rd year on will carry forward effect at year 2
########    BMI at Year 1:   -14.9%
########    BMI from Year 2: -15.2%
########    SBP at Year 1:   -6.2 
########    SBP from Year 2: -5.7
########    TCL at Year 1:   -0.03
########    TCL from Year 2: -0.033
######## ---------------------------------------------------------------------------------------------------------

scenario_5_fn <- function(sp) {
  
  ###########################################################################################################
  #------------------------------------- Loading uncertainty files -----------------------------------------#
  #------ For lifetime treatment, we only need the effects at year 1&2, because weight do not rebound ------#
  #---------------------------------------------------------------------------------------------------------#
  # Efficacy for bmi at 1st year after treatment
  tbl <- read_fst("./inputs/other_parameters/bmi_1y_samples_tirzp.fst", as.data.table = TRUE)
  bmi_1y <- as.numeric(tbl[mc == sp$mc_aggr, "bmi_1y"])
  
  # Efficacy for bmi at 2nd year after treatment
  tbl <- read_fst("./inputs/other_parameters/bmi_1y_samples_tirzp.fst", as.data.table = TRUE)
  bmi_2y <- as.numeric(tbl[mc == sp$mc_aggr, "bmi_1y"])
  
  # Efficacy for SBP at 1st year after treatment
  tbl <- read_fst("./inputs/other_parameters/sbp_1y_samples_tirzp.fst", as.data.table = TRUE)
  sbp_1y <- as.numeric(tbl[mc == sp$mc_aggr, "sbp_1y"])
  
  # Efficacy for SBP at 2nd year of treatment
  tbl <- read_fst("./inputs/other_parameters/sbp_1y_samples_tirzp.fst", as.data.table = TRUE)
  sbp_2y <- as.numeric(tbl[mc == sp$mc_aggr, "sbp_1y"])
  
  # Efficacy for total cholesterol at 1st year of treatment
  tbl <- read_fst("./inputs/other_parameters/tchol_1y_samples_tirzp.fst", as.data.table = TRUE)
  tchol_1y <- as.numeric(tbl[mc == sp$mc_aggr, "tchol_1y"])
  
  # Efficacy for total cholesterol at 2nd year of treatment
  tbl <- read_fst("./inputs/other_parameters/tchol_1y_samples_tirzp.fst", as.data.table = TRUE)
  tchol_2y <- as.numeric(tbl[mc == sp$mc_aggr, "tchol_1y"])
  ###########################################################################################################
  
  # Load the pids of uptake patients for each iteration
  persons_cea <- fread(paste0("./inputs/uptake/", sp$mc_aggr, "_uptake_cea.csv"))
  ### fread() is highly optimized: it’s faster than read_csv(), and it returns a data.table automatically.
  
  # Join uptake info back to lifecourse rows
  sp$pop <- merge(sp$pop, persons_cea, by = "pid", all.x = TRUE)
  
  # Create a row-level indicator for uptake event (1 only in the uptake year row)
  sp$pop[, uptake_one := as.integer(!is.na(uptake_year) & year == uptake_year)]
  # --- !is.na(uptake_year): checks whether the uptake_year column is no NA, returns 'TRUE' or 'FALSE'
  # --- year == uptake_year: checks whether the value in 'year' column is equal to the value in 'uptake_year' column.
  #                          also returns 'TRUE' or 'FALSE'
  # --- as.integer(...):     converts the result of the logical expression into an integer.
  #                          In R, as.integer(TRUE) becomes 1 and as.integer(False) becomes 0.
  
  # a variable for the year someone uptaking the drug
  sp$pop[, anchor_year:=year[uptake_one == 1], by = pid]
  
  # a variable for the year someone enters the synthetic population
  # sp$pop[, entry_year:= min(year), by = pid]
  
  # a variable for theorectical treatment trajectory
  ### Step 0: Initialize the column
  sp$pop[, trtm_theo := NA_integer_]
  ### Step 1: Identify first year of treatment uptake
  sp$pop[uptake_one == 1, trtm_theo := 0]
  ### Step 2: Fill the sequence in subsequent years
  sp$pop[, trtm_theo := fifelse(cumsum(!is.na(trtm_theo)) > 0,   
                                # cumsum(): computes a cumulative count of non-NA entries:
                                # fill in a sequence starting from the first non-NA value
                                seq_len(.N) - min(which(!is.na(trtm_theo))),
                                NA_integer_),
         by = pid]
  ### Step 3: Replace NAs with 0
  sp$pop[, trtm_theo := fifelse(is.na(trtm_theo), 0, trtm_theo)]
  
  ########################################################################################
  ################################          BMI         ##################################
  ########################################################################################
  # a variable for the change in BMI after the uptake of semaglutide
  sp$pop[, `:=`(
    bmi_delta = fcase(
      ### All values until year 5, are % weight loss relative to baseline
      trtm_theo == 0, 0,       ### Baseline year, the year patients become eligible
      trtm_theo == 1, bmi_1y,  ### replace '-0.149' with 'bmi_1y', and link to the uncertainty file: mc + bmi_1y
      ### 1st year of treatment: ITT effects from STEP 1 (1 year f/u): 14.9% weight loss
      trtm_theo >= 2, bmi_2y  ### replace '-0.152' with 'bmi_2y', and link to the uncertainty file: mc + bmi_2y
      ### 2nd year of treatment: ITT effects from STEP 5 (2 year f/u): 15.2% weight loss
    ))
  ]
  
  # a variable for each individual's baseline weight (baseline: year == 24 / ys_rollout == 0)
  # *but, some individuals in the dataset does not have entry at baseline_yr, they came in later than year 24
  # *and, some individuals came in later than year 24, and because they have a chance to uptake drug begining year 25
  # *they might already uptake the drug at the first year, which means they won't have a year (anchor-1),
  # *so what shall we use as a baseline BMI value here...
  sp$pop[, baseline_bmi := {              # sp$pop[, ... , by = pid]: grouping the operation by pid
    anchor <- unique(anchor_year)         # So we can distinguish people who have entry at year 24 and who do not
    if (is.na(anchor)) {                  # If no anchor is found,
      bmi_curr_xps                        # the function simply returns the original bmi_curr_xps as-is
    } else {                              # If yes anchor is found
      bmi_curr_xps[year == (year[trtm_theo == 1][1] - 1)] # baseline_bmi will take the BMI at the year before trtm_theo==1
    }
  }, by = pid] #Works!
  
  
  # a variable for new weight after applying efficacy input during treatment influenced period
  sp$pop[, new_bmi := {
    anchor <- unique(anchor_year)                             # This line finds a single anchor year for each person.
    if (is.na(anchor)) {                                      # If no anchor is found,
      bmi_curr_xps                                            # the function simply returns the original bmi_curr_xps as-is
    } else {                                                  # If yes anchor is found,
      fcase(                                                  # We need to assign value to the' new_bmi' based on the time periods
        year < anchor,      bmi_curr_xps,                   # Before anchor year/uptake: original values (including baseline year)
        year >= anchor,     baseline_bmi*(1 + bmi_delta)    # After anchor year, the lifetime treatment lead to lifetime effects
      )
    }
  }, by = pid]
  
  # Overwriting the original bmi exposure column with the newly created bmi column
  sp$pop[, bmi_curr_xps := new_bmi]
  
  ########################################################################################
  ################################          SBP         ##################################
  ########################################################################################
  # a variable for the change in SBP after the uptake of semaglutide
  sp$pop[, `:=`(
    sbp_delta = fcase(
      ### All values until year 5, are % weight loss relative to baseline
      trtm_theo == 0, 0,       ### Baseline year, the year patients become eligible
      trtm_theo == 1, sbp_1y,    ### 1st year of treatment: ITT effects from STEP 1 (1 year f/u): mean decrease of 6.2 mmHg from baseline
      trtm_theo >= 2, sbp_2y     ### 2nd year of treatment: ITT effects from STEP 5 (2 year f/u): mean decrease of 5.7 mmHg from baseline
    ))
  ]
  
  # a variable for each individual's baseline SBP (baseline: year == 24 / ys_rollout == 0)
  # *but, some individuals in the dataset does not have entry at baseline_yr, they came in later than year 24
  sp$pop[, baseline_sbp := {                # sp$pop[, ... , by = pid]: grouping the operation by pid
    anchor <- unique(anchor_year)           # So we can distinguish people who have entry at year 24 and who do not
    if (is.na(anchor)) {                    # If no anchor is found,
      sbp_curr_xps                          # the function simply returns the original sbp_curr_xps as-is
    } else {                                # If yes anchor is found, baseline_sbp will take the sbp at year 24
      sbp_curr_xps[year == (year[trtm_theo == 1][1] - 1)]  # The baseline year will be (anchor - 1): one year before uptake
    }
  }, by = pid] #Works!
  
  # a variable for new weight after applying efficacy input during treatment influenced period
  sp$pop[, new_sbp := {
    anchor <- unique(anchor_year)                           # This line finds a single anchor year for each person.
    if (is.na(anchor)) {                                    # If no anchor is found,
      sbp_curr_xps                                          # the function simply returns the original sbp_curr_xps as-is
    } else {                                                # If yes anchor is found,
      fcase(                                                # We need to assign value to the 'new_sbp' based on the time periods
        year < anchor,  sbp_curr_xps,              # Before anchor year/uptake: original values (including baseline year)
        year >= anchor, baseline_sbp + sbp_delta  # After anchor year, the lifetime treatment lead to lifetime effects
      )
    }
  }, by = pid]
  
  # Overwriting the original sbp exposure column with the newly created sbp column
  sp$pop[, sbp_curr_xps := new_sbp]
  
  ########################################################################################
  ###############################          Tchol         #################################
  ########################################################################################
  # a variable for the change in SBP after the uptake of semaglutide
  sp$pop[, `:=`(
    tchol_delta = fcase(
      trtm_theo == 0, 0,          ### Baseline year, the year patients become eligible
      trtm_theo == 1, tchol_1y,   ### 1st year of treatment: ITT effects from STEP 1: mean decrease of 3% from baseline
      trtm_theo >= 2, tchol_2y    ### 2nd year of treatment: ITT effects from STEP 5: mean decrease of 3.3% from baseline
    ))
  ]
  
  # a variable for each individual's baseline Tchol (baseline: year == 24 / ys_rollout == 0)
  # *but, some individuals in the dataset does not have entry at baseline_yr, they came in later than year 24
  sp$pop[, baseline_tchol := {                 # sp$pop[, ... , by = pid]: grouping the operation by pid
    anchor <- unique(anchor_year)           # So we can distinguish people who have entry at year 24 and who do not
    if (is.na(anchor)) {                    # If no anchor is found,
      tchol_curr_xps                        # the function simply returns the original tchol_curr_xps as-is
    } else {                                # If yes anchor is found, baseline_tchol will take the tchol at year 24
      tchol_curr_xps[year == (year[trtm_theo == 1][1] - 1)]  # The baseline year will be (anchor - 1): one year before uptake
    }
  }, by = pid] #Works!
  
  # a variable for new weight after applying efficacy input during treatment influenced period
  sp$pop[, new_tchol := {
    anchor <- unique(anchor_year)                             # This line finds a single anchor year for each person.
    if (is.na(anchor)) {                                      # If no anchor is found,
      tchol_curr_xps                                          # the function simply returns the original tchol_curr_xps as-is
    } else {                                                  # If yes anchor is found,
      fcase(                                                  # We need to assign value to the 'new_tchol' based on the time periods
        year < anchor,    tchol_curr_xps,                  # Before anchor year/uptake: original values (including baseline year)
        year >= anchor,   baseline_tchol*(1 + tchol_delta) # After anchor year, the lifetime treatment lead to lifetime effects
      )
    }
  }, by = pid]
  
  # Overwriting the original tchol exposure column with the newly created tchol column
  sp$pop[, tchol_curr_xps := new_tchol]
  ### Jane Aug 2025, test scenario on sp$pop --> Looks like until here everything worked!!!
  
  ##################### Get rid of unnecessary variables ##########################
  # Delete unnecessary variables from synthpop #
  sp$pop[, c("rankstat_sbp", "rankstat_tchol", "ys_rollout", "uptake_rate",
             "uptake_psyr", "uptake_one", 
             "entry_year", "baseline_bmi", "bmi_shift", "new_bmi",
             "baseline_sbp", "sbp_shift", "new_sbp", "baseline_tchol",
             "tchol_shift", "new_tchol") := NULL]
  
}

##################################################################################################################
####                                                                                                          ####
####-------------------------------------    Budget Impact Analysis:    --------------------------------------####
####                                                                                                          ####
####              Two assumptions/scenarios on the time-varying uptake rate:                                  ####
####              1) We assume 100,000 eligible patients uptake each year                                     ####
####              2) We assume a 10% uptake rate every year                                                   ####
####                 * This gives us a 58% overall uptake prevalence over 10 years, 75% over 20 ys            ####
####                                                                                                          ####
##################################################################################################################

##################################################################################################################
#----------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------#
#--------------------------------------------    Scenario 6    --------------------------------------------------#
#----------------------------    Nutrition therapy for 10% uptake every year    ---------------------------------#
#----------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------#
##################################################################################################################
######## Nutrition therapy:---------------------------------------------------------------------------------------
######## For budget impact analysis, we assume certain uptake rates for GLP1 medication.
######## To have a comparison on the treatment effectiveness (e.g., prevented disease outcomes), 
######## we need a scenario where the affected population receive SoC/placebo treatment -- the nutrition therapy
######## Therefore, despite the uptake scenario for GLP1: (1) 10% every year (2) 100,000 every year
######## we will have the same people receiving nutrition therapy as the comparison
######## ---------------------------------------------------------------------------------------------------------

scenario_6_fn <- function(sp) {
  
  ###########################################################################################################
  #------------------------------------- Loading uncertainty files -----------------------------------------#
  # Efficacy for bmi at 1st year after treatment
  tbl <- read_fst("./inputs/other_parameters/bmi_1y_samples_nutr.fst", as.data.table = TRUE)
  bmi_1y <- as.numeric(tbl[mc == sp$mc_aggr, "bmi_1y"])
  
  # Efficacy for bmi at 2nd year after treatment
  tbl <- read_fst("./inputs/other_parameters/bmi_2y_samples_nutr.fst", as.data.table = TRUE)
  bmi_2y <- as.numeric(tbl[mc == sp$mc_aggr, "bmi_2y"])
  
  # Efficacy for SBP at 1st year after treatment
  tbl <- read_fst("./inputs/other_parameters/sbp_1y_samples_nutr.fst", as.data.table = TRUE)
  sbp_1y <- as.numeric(tbl[mc == sp$mc_aggr, "sbp_1y"])
  
  # Efficacy for SBP at 2nd year of treatment
  tbl <- read_fst("./inputs/other_parameters/sbp_2y_samples_nutr.fst", as.data.table = TRUE)
  sbp_2y <- as.numeric(tbl[mc == sp$mc_aggr, "sbp_2y"])
  
  ###########################################################################################################
  
  # Load the pids of uptake patients for each iteration
  persons_bia_pct <- fread(paste0("./inputs/uptake/", sp$mc_aggr, "_uptake_bia_pct.csv"))
  ### fread() is highly optimized: it’s faster than read_csv(), and it returns a data.table automatically.
  
  # Join uptake info back to lifecourse rows
  sp$pop <- merge(sp$pop, persons_bia_pct, by = "pid", all.x = TRUE)
  
  # Create a row-level indicator for uptake event (1 only in the uptake year row)
  sp$pop[, uptake_one := as.integer(!is.na(uptake_year) & year == uptake_year)]
  # --- !is.na(uptake_year): checks whether the uptake_year column is no NA, returns 'TRUE' or 'FALSE'
  # --- year == uptake_year: checks whether the value in 'year' column is equal to the value in 'uptake_year' column.
  #                          also returns 'TRUE' or 'FALSE'
  # --- as.integer(...):     converts the result of the logical expression into an integer.
  #                          In R, as.integer(TRUE) becomes 1 and as.integer(False) becomes 0.
  
  # a variable for the year someone uptaking the drug
  sp$pop[, anchor_year:=year[uptake_one == 1], by = pid]
  
  # a variable for the year someone enters the synthetic population
  # sp$pop[, entry_year:= min(year), by = pid]
  
  # a variable for theorectical treatment trajectory
  ### Step 0: Initialize the column
  sp$pop[, trtm_theo := NA_integer_]
  ### Step 1: Identify first year of treatment uptake
  sp$pop[uptake_one == 1, trtm_theo := 0]
  ### Step 2: Fill the sequence in subsequent years
  sp$pop[, trtm_theo := fifelse(cumsum(!is.na(trtm_theo)) > 0,   # cumsum(): computes a cumulative count of non-NA entries:
                                # fill in a sequence starting from the first non-NA value
                                seq_len(.N) - min(which(!is.na(trtm_theo))),
                                NA_integer_),
         by = pid]
  ### Step 3: Replace NAs with 0
  sp$pop[, trtm_theo := fifelse(is.na(trtm_theo), 0, trtm_theo)]
  
  ########################################################################################
  ################################          BMI         ##################################
  ########################################################################################
  # a variable for the change in BMI after the uptake of semaglutide
  sp$pop[, `:=`(
    bmi_delta = fcase(
      ### All values until year 5, are % weight loss relative to baseline
      trtm_theo == 0, 0,       ### Baseline year, the year patients become eligible
      trtm_theo == 1, bmi_1y,
      trtm_theo == 2, bmi_2y,
      trtm_theo >= 3, 0        ### weight return to baseline weight by 3rd year treatment cessation (uncertainty?)
      ### The efficacy inputs are assigned until here, the rest we will revert to the original exposure column (bmi_curr_xps)
    ))
  ]
  
  # a variable for each individual's baseline weight (baseline: year == 24 / ys_rollout == 0)
  # *but, some individuals in the dataset does not have entry at baseline_yr, they came in later than year 24
  # *and, some individuals came in later than year 24, and because they have a chance to uptake drug begining year 25
  # *they might already uptake the drug at the first year, which means they won't have a year (anchor-1),
  # *so what shall we use as a baseline BMI value here...
  sp$pop[, baseline_bmi := {              # sp$pop[, ... , by = pid]: grouping the operation by pid
    anchor <- unique(anchor_year)         # So we can distinguish people who have entry at year 24 and who do not
    if (is.na(anchor)) {                  # If no anchor is found,
      bmi_curr_xps                        # the function simply returns the original bmi_curr_xps as-is
    } else {                              # If yes anchor is found
      bmi_curr_xps[year == (year[trtm_theo == 1][1] - 1)] # baseline_bmi will take the BMI at the year before trtm_theo==1
    }
  }, by = pid] #Works!
  
  ###############################################################################################################
  #----------------------------  Tutorial: if_else function inside data.table  ---------------------------------#
  ###############################################################################################################
  sp$pop[, bmi_shift := {             # sp$pop[, ... , by = pid]: grouping the operation by pid
    # { ... } operetion per group: inside {}, you can run any custom R code, it will
    # return a vector that gets assigned to 'bmi_shift' for the rows in that group
    anchor <- unique(anchor_year)     # This line finds a single anchor year (e.g., year 30) for each person.
    if (is.na(anchor)) {              # If no anchor is found (to avoid breaking your logic when a person doesn't uptake the drug)
      bmi_curr_xps                    # the function simply returns the original bmi_curr_xps as-is
    } else {                          # If yes anchor is found, You shift the bmi_curr_xps backward in time (type='lag')
      shift(bmi_curr_xps, type = "lag", n = 3)
    }
  }, by = pid] #Works!
  ###############################################################################################################
  #--------------------------------------------  Tutorial: End  ------------------------------------------------#
  ###############################################################################################################
  
  # a variable for new weight after applying efficacy input during treatment influenced period
  sp$pop[, new_bmi := {
    anchor <- unique(anchor_year)                             # This line finds a single anchor year for each person.
    if (is.na(anchor)) {                                      # If no anchor is found,
      bmi_curr_xps                                            # the function simply returns the original bmi_curr_xps as-is
    } else {                                                  # If yes anchor is found,
      fcase(                                                  # We need to assign value to the' new_bmi' based on the time periods
        year < anchor,                       bmi_curr_xps,    # Before anchor year/uptake: original values (including baseline year)
        year >= anchor & year <= anchor + 2, baseline_bmi*(1 + bmi_delta),  # During the 3 years where treatment effects manifest
        year >= anchor + 3,                   bmi_shift        # After rebound to baseline: revert to the original bmi_curr_xps
      )
    }
  }, by = pid]
  
  # Overwriting the original bmi exposure column with the newly created bmi column
  #sp$pop[, bmi_curr_xps := new_bmi]
  
  ########################################################################################
  ################################          SBP         ##################################
  ########################################################################################
  # a variable for the change in SBP after the uptake of semaglutide
  sp$pop[, `:=`(
    sbp_delta = fcase(
      ### All values until year 5, are % weight loss relative to baseline
      trtm_theo == 0, 0,       ### Baseline year, the year patients become eligible
      trtm_theo == 1, sbp_1y,
      trtm_theo == 2, sbp_2y,
      trtm_theo >= 3, 0        ### STEP 1 extension: assume mean SBP will revert to baseline level after one year without treatment
      ### The efficacy inputs are assigned until here, the rest we will revert to the original exposure column (sbp_curr_xps)
    ))
  ]
  
  # a variable for each individual's baseline SBP (baseline: year == 24 / ys_rollout == 0)
  # *but, some individuals in the dataset does not have entry at baseline_yr, they came in later than year 24
  sp$pop[, baseline_sbp := {                # sp$pop[, ... , by = pid]: grouping the operation by pid
    anchor <- unique(anchor_year)           # So we can distinguish people who have entry at year 24 and who do not
    if (is.na(anchor)) {                    # If no anchor is found,
      sbp_curr_xps                          # the function simply returns the original sbp_curr_xps as-is
    } else {                                # If yes anchor is found, baseline_sbp will take the sbp at year 24
      sbp_curr_xps[year == (year[trtm_theo == 1][1] - 1)]  # The baseline year will be (anchor - 1): one year before uptake
    }
  }, by = pid] #Works!
  
  ###############################################################################################################
  #----------------------------  Tutorial: if_else function inside data.table  ---------------------------------#
  ###############################################################################################################
  sp$pop[, sbp_shift := {                 # sp$pop[, ... , by = pid]: grouping the operation by pid
    # { ... } operetion per group: inside {}, you can run any custom R code, it will
    # return a vector that gets assigned to 'sbp_shift' for the rows in that group
    
    anchor <- unique(anchor_year)        # This line finds a single anchor year (e.g., year 30) for each person.
    if (is.na(anchor)) {                 # If no anchor is found, (This is useful to avoid breaking your logic when a person doesn't meet your anchor condition)
      sbp_curr_xps                       # the function simply returns the original sbp_curr_xps as-is
    } else {                             # If yes anchor is found, You shift the sbp_curr_xps backward in time (type='lag')
      shift(sbp_curr_xps, type = "lag", n = 3) # by 3 years: 3 years is the duration of efficacy inputs (decrease in SBP and rebound)
    }
  }, by = pid] #Works!
  
  #  A if_else function inside of :={} for creating a new variable
  #  if(condition) {
  #   what should happen if condition is TRUE
  #  } else {
  #   what should happen if condition is FALSE
  #  }
  ###############################################################################################################
  #--------------------------------------------  Tutorial: End  ------------------------------------------------#
  ###############################################################################################################
  
  # a variable for new weight after applying efficacy input during treatment influenced period
  sp$pop[, new_sbp := {
    anchor <- unique(anchor_year)                             # This line finds a single anchor year for each person.
    if (is.na(anchor)) {                                      # If no anchor is found,
      sbp_curr_xps                                            # the function simply returns the original sbp_curr_xps as-is
    } else {                                                  # If yes anchor is found,
      fcase(                                                  # We need to assign value to the 'new_sbp' based on the time periods
        year < anchor,                       sbp_curr_xps,    # Before anchor year/uptake: original values (including baseline year)
        year >= anchor & year <= anchor + 2, baseline_sbp + sbp_delta,  # During the 2 years where treatment effect manifest
        year >= anchor + 3,                  sbp_shift        # After sbp rebound to baseline: revert to the original sbp_curr_xps
      )
    }
  }, by = pid]
  
  # Overwriting the original sbp exposure column with the newly created sbp column
  sp$pop[, sbp_curr_xps := new_sbp]
  
  
  ########################################################################################
  ###############################          Tchol         #################################
  ###################         Nothing happens to Tchol in sc0         ####################
  ########################################################################################
  sp$pop[, c("tchol_delta") := 0]
  
  ##################### Get rid of unnecessary variables ##########################
  # Delete unnecessary variables from synthpop #
  sp$pop[, c("rankstat_sbp", "rankstat_tchol", "ys_rollout", "uptake_rate",
             "uptake_psyr", "uptake_one", 
             "entry_year", "baseline_bmi", "bmi_shift", "new_bmi",
             "baseline_sbp", "sbp_shift", "new_sbp", "baseline_tchol",
             "tchol_shift", "new_tchol") := NULL]
  
}

##################################################################################################################
#----------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------#
#--------------------------------------------    Scenario 7    --------------------------------------------------#
#-------------------------   Semaglutide for 10% uptake every year, 2 years treatment   -------------------------#
#----------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------#
##################################################################################################################

scenario_7_fn <- function(sp) {
  
  # Efficacy for bmi at 1st year after treatment
  tbl <- read_fst("./inputs/other_parameters/bmi_1y_samples_semag.fst", as.data.table = TRUE)
  bmi_1y <- as.numeric(tbl[mc == sp$mc_aggr, "bmi_1y"])
  
  # Efficacy for bmi at 2nd year after treatment
  tbl <- read_fst("./inputs/other_parameters/bmi_2y_samples_semag.fst", as.data.table = TRUE)
  bmi_2y <- as.numeric(tbl[mc == sp$mc_aggr, "bmi_2y"])
  
  # Efficacy for bmi at 3rd year after treatment
  tbl <- read_fst("./inputs/other_parameters/bmi_3y_samples_semag.fst", as.data.table = TRUE)
  bmi_3y <- as.numeric(tbl[mc == sp$mc_aggr, "bmi_3y"])
  
  # Efficacy for bmi at 4th year after treatment
  tbl <- read_fst("./inputs/other_parameters/bmi_4y_samples_semag.fst", as.data.table = TRUE)
  bmi_4y <- as.numeric(tbl[mc == sp$mc_aggr, "bmi_4y"])
  
  # Efficacy for SBP at 1st year after treatment
  tbl <- read_fst("./inputs/other_parameters/sbp_1y_samples_semag.fst", as.data.table = TRUE)
  sbp_1y <- as.numeric(tbl[mc == sp$mc_aggr, "sbp_1y"])
  
  # Efficacy for SBP at 2nd year of treatment
  tbl <- read_fst("./inputs/other_parameters/sbp_2y_samples_semag.fst", as.data.table = TRUE)
  sbp_2y <- as.numeric(tbl[mc == sp$mc_aggr, "sbp_2y"])
  
  # Efficacy for total cholesterol at 1st year of treatment
  tbl <- read_fst("./inputs/other_parameters/tchol_1y_samples_semag.fst", as.data.table = TRUE)
  tchol_1y <- as.numeric(tbl[mc == sp$mc_aggr, "tchol_1y"])
  
  # Efficacy for total cholesterol at 2nd year of treatment
  tbl <- read_fst("./inputs/other_parameters/tchol_2y_samples_semag.fst", as.data.table = TRUE)
  tchol_2y <- as.numeric(tbl[mc == sp$mc_aggr, "tchol_2y"])
  ###########################################################################################################
  
  # Load the pids of uptake patients for each iteration
  persons_bia_pct <- fread(paste0("./inputs/uptake/", sp$mc_aggr, "_uptake_bia_pct.csv"))
  ### fread() is highly optimized: it’s faster than read_csv(), and it returns a data.table automatically.
  
  # Join uptake info back to lifecourse rows
  sp$pop <- merge(sp$pop, persons_bia_pct, by = "pid", all.x = TRUE)
  
  # Create a row-level indicator for uptake event (1 only in the uptake year row)
  sp$pop[, uptake_one := as.integer(!is.na(uptake_year) & year == uptake_year)]
  # --- !is.na(uptake_year): checks whether the uptake_year column is no NA, returns 'TRUE' or 'FALSE'
  # --- year == uptake_year: checks whether the value in 'year' column is equal to the value in 'uptake_year' column.
  #                          also returns 'TRUE' or 'FALSE'
  # --- as.integer(...):     converts the result of the logical expression into an integer.
  #                          In R, as.integer(TRUE) becomes 1 and as.integer(False) becomes 0.
  
  # a variable for the year someone uptaking the drug
  sp$pop[, anchor_year:=year[uptake_one == 1], by = pid]
  
  # a variable for the year someone enters the synthetic population
  # sp$pop[, entry_year:= min(year), by = pid]
  
  # a variable for theorectical treatment trajectory
  ### Step 0: Initialize the column
  sp$pop[, trtm_theo := NA_integer_]
  ### Step 1: Identify first year of treatment uptake
  sp$pop[uptake_one == 1, trtm_theo := 0]
  ### Step 2: Fill the sequence in subsequent years
  sp$pop[, trtm_theo := fifelse(cumsum(!is.na(trtm_theo)) > 0,   # cumsum(): computes a cumulative count of non-NA entries:
                                # fill in a sequence starting from the first non-NA value
                                seq_len(.N) - min(which(!is.na(trtm_theo))),
                                NA_integer_),
         by = pid]
  ### Step 3: Replace NAs with 0
  sp$pop[, trtm_theo := fifelse(is.na(trtm_theo), 0, trtm_theo)]
  
  ########################################################################################
  ################################          BMI         ##################################
  ########################################################################################
  # a variable for the change in BMI after the uptake of semaglutide
  sp$pop[, `:=`(
    bmi_delta = fcase(
      ### All values until year 5, are % weight loss relative to baseline
      trtm_theo == 0, 0,       ### Baseline year, the year patients become eligible
      trtm_theo == 1, bmi_1y,  ### 1st year of treatment: ITT effects from STEP 1 (1 year f/u): 14.9% weight loss (uncertainty?)
      trtm_theo == 2, bmi_2y,  ### 2nd year of treatment: ITT effects from STEP 5 (2 year f/u): 15.2% weight loss (uncertainty?)
      trtm_theo == 3, bmi_3y,  ### STEP 1 extension: regain 2/3 of weight loss in 1st year treatment cessation (uncertainty?)
      trtm_theo == 4, bmi_4y,  ### weight regain at 2nd year treatment cessation (uncertainty?)
      trtm_theo >= 5, 0        ### weight return to baseline weight by 3rd year treatment cessation (uncertainty?)
      ### The efficacy inputs are assigned until here, the rest we will revert to the original exposure column (bmi_curr_xps)
    ))
  ]
  
  # a variable for each individual's baseline weight (baseline: year == 24 / ys_rollout == 0)
  # ------------------------------------------ Aug 22, 2025, Jane ------------------------------------------------- #
  # *but, some individuals in the dataset does not have entry at baseline_yr, they came in later than year 24
  # *and, some individuals came in later than year 24, and because they have a chance to uptake drug begining year 25
  # *they might already uptake the drug at the first year, which means they won't have a year (anchor-1),
  # *so what shall we use as a baseline BMI value here...
  sp$pop[, baseline_bmi := {              # sp$pop[, ... , by = pid]: grouping the operation by pid
    anchor <- unique(anchor_year)         # So we can distinguish people who have entry at year 24 and who do not
    if (is.na(anchor)) {                  # If no anchor is found,
      bmi_curr_xps                        # the function simply returns the original bmi_curr_xps as-is
    } else {                              # If yes anchor is found
      bmi_curr_xps[year == (year[trtm_theo == 1][1] - 1)] # baseline_bmi will take the BMI at the year before trtm_theo==1
    }
  }, by = pid] #Works!
  
  ###############################################################################################################
  #----------------------------  Tutorial: if_else function inside data.table  ---------------------------------#
  ###############################################################################################################
  sp$pop[, bmi_shift := {             # sp$pop[, ... , by = pid]: grouping the operation by pid
    # { ... } operetion per group: inside {}, you can run any custom R code, it will
    # return a vector that gets assigned to 'bmi_shift' for the rows in that group
    anchor <- unique(anchor_year)     # This line finds a single anchor year (e.g., year 30) for each person.
    if (is.na(anchor)) {              # If no anchor is found (to avoid breaking your logic when a person doesn't uptake the drug)
      bmi_curr_xps                    # the function simply returns the original bmi_curr_xps as-is
    } else {                          # If yes anchor is found, You shift the bmi_curr_xps backward in time (type='lag')
      shift(bmi_curr_xps, type = "lag", n = 5) # by 5 years: 5 years is the duration of efficacy inputs (weight loss and reboud)
    }
  }, by = pid] #Works!
  ###############################################################################################################
  #--------------------------------------------  Tutorial: End  ------------------------------------------------#
  ###############################################################################################################
  
  # a variable for new weight after applying efficacy input during treatment influenced period
  sp$pop[, new_bmi := {
    anchor <- unique(anchor_year)                             # This line finds a single anchor year for each person.
    if (is.na(anchor)) {                                      # If no anchor is found,
      bmi_curr_xps                                            # the function simply returns the original bmi_curr_xps as-is
    } else {                                                  # If yes anchor is found,
      fcase(                                                  # We need to assign value to the' new_bmi' based on the time periods
        year < anchor,                       bmi_curr_xps,    # Before anchor year/uptake: original values (including baseline year)
        year >= anchor & year <= anchor + 4, baseline_bmi*(1 + bmi_delta),  # During the 5 years where treatment effects manifest
        year > anchor + 4,                   bmi_shift        # After rebound to baseline: revert to the original bmi_curr_xps
      )
    }
  }, by = pid]
  
  # Overwriting the original bmi exposure column with the newly created bmi column
  sp$pop[, bmi_curr_xps := new_bmi]
  
  ########################################################################################
  ################################          SBP         ##################################
  ########################################################################################
  # a variable for the change in SBP after the uptake of semaglutide
  sp$pop[, `:=`(
    sbp_delta = fcase(
      ### All values until year 5, are % weight loss relative to baseline
      trtm_theo == 0, 0,       ### Baseline year, the year patients become eligible
      trtm_theo == 1, sbp_1y,    ### 1st year of treatment: ITT effects from STEP 1 (1 year f/u): mean decrease of 6.2 mmHg from baseline
      trtm_theo == 2, sbp_2y,    ### 2nd year of treatment: ITT effects from STEP 5 (2 year f/u): mean decrease of 5.7 mmHg from baseline
      trtm_theo >= 3, 0        ### STEP 1 extension: assume mean SBP will revert to baseline level after one year without treatment
      ### The efficacy inputs are assigned until here, the rest we will revert to the original exposure column (sbp_curr_xps)
    ))
  ]
  
  # a variable for each individual's baseline SBP (baseline: year == 24 / ys_rollout == 0)
  # *but, some individuals in the dataset does not have entry at baseline_yr, they came in later than year 24
  sp$pop[, baseline_sbp := {                # sp$pop[, ... , by = pid]: grouping the operation by pid
    anchor <- unique(anchor_year)           # So we can distinguish people who have entry at year 24 and who do not
    if (is.na(anchor)) {                    # If no anchor is found,
      sbp_curr_xps                          # the function simply returns the original sbp_curr_xps as-is
    } else {                                # If yes anchor is found, baseline_sbp will take the sbp at year 24
      sbp_curr_xps[year == (year[trtm_theo == 1][1] - 1)]  # The baseline year will be (anchor - 1): one year before uptake
    }
  }, by = pid] #Works!
  
  ###############################################################################################################
  #----------------------------  Tutorial: if_else function inside data.table  ---------------------------------#
  ###############################################################################################################
  sp$pop[, sbp_shift := {                 # sp$pop[, ... , by = pid]: grouping the operation by pid
    # { ... } operetion per group: inside {}, you can run any custom R code, it will
    # return a vector that gets assigned to 'sbp_shift' for the rows in that group
    
    anchor <- unique(anchor_year)        # This line finds a single anchor year (e.g., year 30) for each person.
    if (is.na(anchor)) {                 # If no anchor is found, (This is useful to avoid breaking your logic when a person doesn't meet your anchor condition)
      sbp_curr_xps                       # the function simply returns the original sbp_curr_xps as-is
    } else {                             # If yes anchor is found, You shift the sbp_curr_xps backward in time (type='lag')
      shift(sbp_curr_xps, type = "lag", n = 3) # by 3 years: 3 years is the duration of efficacy inputs (decrease in SBP and rebound)
    }
  }, by = pid] #Works!
  
  #  A if_else function inside of :={} for creating a new variable
  #  if(condition) {
  #   what should happen if condition is TRUE
  #  } else {
  #   what should happen if condition is FALSE
  #  }
  ###############################################################################################################
  #--------------------------------------------  Tutorial: End  ------------------------------------------------#
  ###############################################################################################################
  
  # a variable for new weight after applying efficacy input during treatment influenced period
  sp$pop[, new_sbp := {
    anchor <- unique(anchor_year)                             # This line finds a single anchor year for each person.
    if (is.na(anchor)) {                                      # If no anchor is found,
      sbp_curr_xps                                            # the function simply returns the original sbp_curr_xps as-is
    } else {                                                  # If yes anchor is found,
      fcase(                                                  # We need to assign value to the 'new_sbp' based on the time periods
        year < anchor,                       sbp_curr_xps,    # Before anchor year/uptake: original values (including baseline year)
        year >= anchor & year <= anchor + 2, baseline_sbp + sbp_delta,  # During the 2 years where treatment effect manifest
        year >= anchor + 3,                  sbp_shift        # After sbp rebound to baseline: revert to the original sbp_curr_xps
      )
    }
  }, by = pid]
  
  # Overwriting the original sbp exposure column with the newly created sbp column
  sp$pop[, sbp_curr_xps := new_sbp]
  
  ########################################################################################
  ###############################          Tchol         #################################
  ########################################################################################
  # a variable for the change in SBP after the uptake of semaglutide
  sp$pop[, `:=`(
    tchol_delta = fcase(
      trtm_theo == 0, 0,        ### Baseline year, the year patients become eligible
      trtm_theo == 1, tchol_1y,    ### 1st year of treatment: ITT effects from STEP 1: mean decrease of 3% from baseline
      trtm_theo == 2, tchol_2y,   ### 2nd year of treatment: ITT effects from STEP 5: mean decrease of 3.3% from baseline
      trtm_theo >= 3, 0         ### STEP 1 extension: assume mean tchol will revert to baseline level after one year without treatment
      ### The efficacy inputs are assigned until here, the rest we will revert to the original exposure column (tchol_curr_xps)
    ))
  ]
  
  # a variable for each individual's baseline Tchol (baseline: year == 24 / ys_rollout == 0)
  # *but, some individuals in the dataset does not have entry at baseline_yr, they came in later than year 24
  sp$pop[, baseline_tchol := {                 # sp$pop[, ... , by = pid]: grouping the operation by pid
    anchor <- unique(anchor_year)           # So we can distinguish people who have entry at year 24 and who do not
    if (is.na(anchor)) {                    # If no anchor is found,
      tchol_curr_xps                        # the function simply returns the original tchol_curr_xps as-is
    } else {                                # If yes anchor is found, baseline_tchol will take the tchol at year 24
      tchol_curr_xps[year == (year[trtm_theo == 1][1] - 1)]  # The baseline year will be (anchor - 1): one year before uptake
    }
  }, by = pid] #Works!
  
  ###############################################################################################################
  #----------------------------  Tutorial: if_else function inside data.table  ---------------------------------#
  ###############################################################################################################
  sp$pop[, tchol_shift := {                 # sp$pop[, ... , by = pid]: grouping the operation by pid
    # { ... } operetion per group: inside {}, you can run any custom R code, it will
    # return a vector that gets assigned to 'tchol_shift' for the rows in that group
    
    anchor <- unique(anchor_year)        # This line finds a single anchor year (e.g., year 30) for each person.
    if (is.na(anchor)) {                 # If no anchor is found, (This is useful to avoid breaking your logic when a person doesn't meet your anchor condition)
      tchol_curr_xps                       # the function simply returns the original tchol_curr_xps as-is
    } else {                             # If yes anchor is found, You shift the tchol_curr_xps backward in time (type='lag')
      shift(tchol_curr_xps, type = "lag", n = 3) # by 3 years: 3 years is the duration of efficacy inputs (decrease in tchol and reboud)
    }
  }, by = pid] #Works!
  
  # test <- sp$pop[pid %in% c(156, 175)]
  ###############################################################################################################
  #--------------------------------------------  Tutorial: End  ------------------------------------------------#
  ###############################################################################################################
  
  # a variable for new weight after applying efficacy input during treatment influenced period
  sp$pop[, new_tchol := {
    anchor <- unique(anchor_year)                             # This line finds a single anchor year for each person.
    if (is.na(anchor)) {                                      # If no anchor is found,
      tchol_curr_xps                                          # the function simply returns the original tchol_curr_xps as-is
    } else {                                                  # If yes anchor is found,
      fcase(                                                  # We need to assign value to the 'new_tchol' based on the time periods
        year < anchor,                       tchol_curr_xps,  # Before anchor year/uptake: original values (including baseline year)
        year >= anchor & year <= anchor + 2, baseline_tchol*(1 + tchol_delta),  # During the 2 years where treatment effect manifest
        year >= anchor + 3,                  tchol_shift      # After tchol rebound to baseline: revert to the original tchol_curr_xps
      )
    }
  }, by = pid]
  
  # Overwriting the original tchol exposure column with the newly created tchol column
  sp$pop[, tchol_curr_xps := new_tchol]
  ### Jane Aug 2025, test scenario on sp$pop --> Looks like until here everything worked!!!
  
  ##################### Get rid of unnecessary variables ##########################
  # Delete unnecessary variables from synthpop #
  sp$pop[, c("rankstat_sbp", "rankstat_tchol", "ys_rollout", "uptake_rate",
             "uptake_psyr", "uptake_one", 
             "entry_year", "baseline_bmi", "bmi_shift", "new_bmi",
             "baseline_sbp", "sbp_shift", "new_sbp", "baseline_tchol",
             "tchol_shift", "new_tchol") := NULL]
  
}

##################################################################################################################
#----------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------#
#--------------------------------------------    Scenario 8    --------------------------------------------------#
#-------------------------   Semaglutide for 10% uptake every year, lifetime treatment   ------------------------#
#----------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------#
##################################################################################################################
######## Lifetime treatment:--------------------------------------------------------------------------------------
######## 1. Treatment effects:------------------------------------------------------------------------------------
########    We assume that the effects inputs from the 3rd year on will carry forward effect at year 2
########    BMI at Year 1:   -14.9%
########    BMI from Year 2: -15.2%
########    SBP at Year 1:   -6.2 
########    SBP from Year 2: -5.7
########    TCL at Year 1:   -0.03
########    TCL from Year 2: -0.033
######## ---------------------------------------------------------------------------------------------------------

scenario_8_fn <- function(sp) {
  
  ###########################################################################################################
  #------------------------------------- Loading uncertainty files -----------------------------------------#
  #------ For lifetime treatment, we only need the effects at year 1&2, because weight do not rebound ------#
  #---------------------------------------------------------------------------------------------------------#
  # Efficacy for bmi at 1st year after treatment
  tbl <- read_fst("./inputs/other_parameters/bmi_1y_samples_semag.fst", as.data.table = TRUE)
  bmi_1y <- as.numeric(tbl[mc == sp$mc_aggr, "bmi_1y"])
  
  # Efficacy for bmi at 2nd year after treatment
  tbl <- read_fst("./inputs/other_parameters/bmi_2y_samples_semag.fst", as.data.table = TRUE)
  bmi_2y <- as.numeric(tbl[mc == sp$mc_aggr, "bmi_2y"])
  
  # Efficacy for SBP at 1st year after treatment
  tbl <- read_fst("./inputs/other_parameters/sbp_1y_samples_semag.fst", as.data.table = TRUE)
  sbp_1y <- as.numeric(tbl[mc == sp$mc_aggr, "sbp_1y"])
  
  # Efficacy for SBP at 2nd year of treatment
  tbl <- read_fst("./inputs/other_parameters/sbp_2y_samples_semag.fst", as.data.table = TRUE)
  sbp_2y <- as.numeric(tbl[mc == sp$mc_aggr, "sbp_2y"])
  
  # Efficacy for total cholesterol at 1st year of treatment
  tbl <- read_fst("./inputs/other_parameters/tchol_1y_samples_semag.fst", as.data.table = TRUE)
  tchol_1y <- as.numeric(tbl[mc == sp$mc_aggr, "tchol_1y"])
  
  # Efficacy for total cholesterol at 2nd year of treatment
  tbl <- read_fst("./inputs/other_parameters/tchol_2y_samples_semag.fst", as.data.table = TRUE)
  tchol_2y <- as.numeric(tbl[mc == sp$mc_aggr, "tchol_2y"])
  ###########################################################################################################
  
  # Load the pids of uptake patients for each iteration
  persons_bia_pct <- fread(paste0("./inputs/uptake/", sp$mc_aggr, "_uptake_bia_pct.csv"))
  ### fread() is highly optimized: it’s faster than read_csv(), and it returns a data.table automatically.
  
  # Join uptake info back to lifecourse rows
  sp$pop <- merge(sp$pop, persons_bia_pct, by = "pid", all.x = TRUE)
  
  # Create a row-level indicator for uptake event (1 only in the uptake year row)
  sp$pop[, uptake_one := as.integer(!is.na(uptake_year) & year == uptake_year)]
  # --- !is.na(uptake_year): checks whether the uptake_year column is no NA, returns 'TRUE' or 'FALSE'
  # --- year == uptake_year: checks whether the value in 'year' column is equal to the value in 'uptake_year' column.
  #                          also returns 'TRUE' or 'FALSE'
  # --- as.integer(...):     converts the result of the logical expression into an integer.
  #                          In R, as.integer(TRUE) becomes 1 and as.integer(False) becomes 0.
  
  # a variable for the year someone uptaking the drug
  sp$pop[, anchor_year:=year[uptake_one == 1], by = pid]
  
  # a variable for the year someone enters the synthetic population
  # sp$pop[, entry_year:= min(year), by = pid]
  
  # a variable for theorectical treatment trajectory
  ### Step 0: Initialize the column
  sp$pop[, trtm_theo := NA_integer_]
  ### Step 1: Identify first year of treatment uptake
  sp$pop[uptake_one == 1, trtm_theo := 0]
  ### Step 2: Fill the sequence in subsequent years
  sp$pop[, trtm_theo := fifelse(cumsum(!is.na(trtm_theo)) > 0,   
                                # cumsum(): computes a cumulative count of non-NA entries:
                                # fill in a sequence starting from the first non-NA value
                                seq_len(.N) - min(which(!is.na(trtm_theo))),
                                NA_integer_),
         by = pid]
  ### Step 3: Replace NAs with 0
  sp$pop[, trtm_theo := fifelse(is.na(trtm_theo), 0, trtm_theo)]
  
  ########################################################################################
  ################################          BMI         ##################################
  ########################################################################################
  # a variable for the change in BMI after the uptake of semaglutide
  sp$pop[, `:=`(
    bmi_delta = fcase(
      ### All values until year 5, are % weight loss relative to baseline
      trtm_theo == 0, 0,       ### Baseline year, the year patients become eligible
      trtm_theo == 1, bmi_1y,  ### replace '-0.149' with 'bmi_1y', and link to the uncertainty file: mc + bmi_1y
      ### 1st year of treatment: ITT effects from STEP 1 (1 year f/u): 14.9% weight loss
      trtm_theo >= 2, bmi_2y  ### replace '-0.152' with 'bmi_2y', and link to the uncertainty file: mc + bmi_2y
      ### 2nd year of treatment: ITT effects from STEP 5 (2 year f/u): 15.2% weight loss
    ))
  ]
  
  # a variable for each individual's baseline weight (baseline: year == 24 / ys_rollout == 0)
  # *but, some individuals in the dataset does not have entry at baseline_yr, they came in later than year 24
  # *and, some individuals came in later than year 24, and because they have a chance to uptake drug begining year 25
  # *they might already uptake the drug at the first year, which means they won't have a year (anchor-1),
  # *so what shall we use as a baseline BMI value here...
  sp$pop[, baseline_bmi := {              # sp$pop[, ... , by = pid]: grouping the operation by pid
    anchor <- unique(anchor_year)         # So we can distinguish people who have entry at year 24 and who do not
    if (is.na(anchor)) {                  # If no anchor is found,
      bmi_curr_xps                        # the function simply returns the original bmi_curr_xps as-is
    } else {                              # If yes anchor is found
      bmi_curr_xps[year == (year[trtm_theo == 1][1] - 1)] # baseline_bmi will take the BMI at the year before trtm_theo==1
    }
  }, by = pid] #Works!
  
  
  # a variable for new weight after applying efficacy input during treatment influenced period
  sp$pop[, new_bmi := {
    anchor <- unique(anchor_year)                             # This line finds a single anchor year for each person.
    if (is.na(anchor)) {                                      # If no anchor is found,
      bmi_curr_xps                                            # the function simply returns the original bmi_curr_xps as-is
    } else {                                                  # If yes anchor is found,
      fcase(                                                  # We need to assign value to the' new_bmi' based on the time periods
        year < anchor,      bmi_curr_xps,                   # Before anchor year/uptake: original values (including baseline year)
        year >= anchor,     baseline_bmi*(1 + bmi_delta)    # After anchor year, the lifetime treatment lead to lifetime effects
      )
    }
  }, by = pid]
  
  # Overwriting the original bmi exposure column with the newly created bmi column
  sp$pop[, bmi_curr_xps := new_bmi]
  
  ########################################################################################
  ################################          SBP         ##################################
  ########################################################################################
  # a variable for the change in SBP after the uptake of semaglutide
  sp$pop[, `:=`(
    sbp_delta = fcase(
      ### All values until year 5, are % weight loss relative to baseline
      trtm_theo == 0, 0,       ### Baseline year, the year patients become eligible
      trtm_theo == 1, sbp_1y,    ### 1st year of treatment: ITT effects from STEP 1 (1 year f/u): mean decrease of 6.2 mmHg from baseline
      trtm_theo >= 2, sbp_2y     ### 2nd year of treatment: ITT effects from STEP 5 (2 year f/u): mean decrease of 5.7 mmHg from baseline
    ))
  ]
  
  # a variable for each individual's baseline SBP (baseline: year == 24 / ys_rollout == 0)
  # *but, some individuals in the dataset does not have entry at baseline_yr, they came in later than year 24
  sp$pop[, baseline_sbp := {                # sp$pop[, ... , by = pid]: grouping the operation by pid
    anchor <- unique(anchor_year)           # So we can distinguish people who have entry at year 24 and who do not
    if (is.na(anchor)) {                    # If no anchor is found,
      sbp_curr_xps                          # the function simply returns the original sbp_curr_xps as-is
    } else {                                # If yes anchor is found, baseline_sbp will take the sbp at year 24
      sbp_curr_xps[year == (year[trtm_theo == 1][1] - 1)]  # The baseline year will be (anchor - 1): one year before uptake
    }
  }, by = pid] #Works!
  
  # a variable for new weight after applying efficacy input during treatment influenced period
  sp$pop[, new_sbp := {
    anchor <- unique(anchor_year)                           # This line finds a single anchor year for each person.
    if (is.na(anchor)) {                                    # If no anchor is found,
      sbp_curr_xps                                          # the function simply returns the original sbp_curr_xps as-is
    } else {                                                # If yes anchor is found,
      fcase(                                                # We need to assign value to the 'new_sbp' based on the time periods
        year < anchor,  sbp_curr_xps,              # Before anchor year/uptake: original values (including baseline year)
        year >= anchor, baseline_sbp + sbp_delta  # After anchor year, the lifetime treatment lead to lifetime effects
      )
    }
  }, by = pid]
  
  # Overwriting the original sbp exposure column with the newly created sbp column
  sp$pop[, sbp_curr_xps := new_sbp]
  
  ########################################################################################
  ###############################          Tchol         #################################
  ########################################################################################
  # a variable for the change in SBP after the uptake of semaglutide
  sp$pop[, `:=`(
    tchol_delta = fcase(
      trtm_theo == 0, 0,          ### Baseline year, the year patients become eligible
      trtm_theo == 1, tchol_1y,   ### 1st year of treatment: ITT effects from STEP 1: mean decrease of 3% from baseline
      trtm_theo >= 2, tchol_2y    ### 2nd year of treatment: ITT effects from STEP 5: mean decrease of 3.3% from baseline
    ))
  ]
  
  # a variable for each individual's baseline Tchol (baseline: year == 24 / ys_rollout == 0)
  # *but, some individuals in the dataset does not have entry at baseline_yr, they came in later than year 24
  sp$pop[, baseline_tchol := {                 # sp$pop[, ... , by = pid]: grouping the operation by pid
    anchor <- unique(anchor_year)           # So we can distinguish people who have entry at year 24 and who do not
    if (is.na(anchor)) {                    # If no anchor is found,
      tchol_curr_xps                        # the function simply returns the original tchol_curr_xps as-is
    } else {                                # If yes anchor is found, baseline_tchol will take the tchol at year 24
      tchol_curr_xps[year == (year[trtm_theo == 1][1] - 1)]  # The baseline year will be (anchor - 1): one year before uptake
    }
  }, by = pid] #Works!
  
  # a variable for new weight after applying efficacy input during treatment influenced period
  sp$pop[, new_tchol := {
    anchor <- unique(anchor_year)                             # This line finds a single anchor year for each person.
    if (is.na(anchor)) {                                      # If no anchor is found,
      tchol_curr_xps                                          # the function simply returns the original tchol_curr_xps as-is
    } else {                                                  # If yes anchor is found,
      fcase(                                                  # We need to assign value to the 'new_tchol' based on the time periods
        year < anchor,    tchol_curr_xps,                  # Before anchor year/uptake: original values (including baseline year)
        year >= anchor,   baseline_tchol*(1 + tchol_delta) # After anchor year, the lifetime treatment lead to lifetime effects
      )
    }
  }, by = pid]
  
  # Overwriting the original tchol exposure column with the newly created tchol column
  sp$pop[, tchol_curr_xps := new_tchol]
  ### Jane Aug 2025, test scenario on sp$pop --> Looks like until here everything worked!!!
  
  ##################### Get rid of unnecessary variables ##########################
  # Delete unnecessary variables from synthpop #
  sp$pop[, c("rankstat_sbp", "rankstat_tchol", "ys_rollout", "uptake_rate",
             "uptake_psyr", "uptake_one", 
             "entry_year", "baseline_bmi", "bmi_shift", "new_bmi",
             "baseline_sbp", "sbp_shift", "new_sbp", "baseline_tchol",
             "tchol_shift", "new_tchol") := NULL]
  
}

##################################################################################################################
#----------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------#
#--------------------------------------------    Scenario 9    --------------------------------------------------#
#----------------   Semaglutide for 100,000 patients uptake every year, 2 years treatment   ---------------------#
#----------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------#
##################################################################################################################

scenario_9_fn <- function(sp) {
  
  # Efficacy for bmi at 1st year after treatment
  tbl <- read_fst("./inputs/other_parameters/bmi_1y_samples_semag.fst", as.data.table = TRUE)
  bmi_1y <- as.numeric(tbl[mc == sp$mc_aggr, "bmi_1y"])
  
  # Efficacy for bmi at 2nd year after treatment
  tbl <- read_fst("./inputs/other_parameters/bmi_2y_samples_semag.fst", as.data.table = TRUE)
  bmi_2y <- as.numeric(tbl[mc == sp$mc_aggr, "bmi_2y"])
  
  # Efficacy for bmi at 3rd year after treatment
  tbl <- read_fst("./inputs/other_parameters/bmi_3y_samples_semag.fst", as.data.table = TRUE)
  bmi_3y <- as.numeric(tbl[mc == sp$mc_aggr, "bmi_3y"])
  
  # Efficacy for bmi at 4th year after treatment
  tbl <- read_fst("./inputs/other_parameters/bmi_4y_samples_semag.fst", as.data.table = TRUE)
  bmi_4y <- as.numeric(tbl[mc == sp$mc_aggr, "bmi_4y"])
  
  # Efficacy for SBP at 1st year after treatment
  tbl <- read_fst("./inputs/other_parameters/sbp_1y_samples_semag.fst", as.data.table = TRUE)
  sbp_1y <- as.numeric(tbl[mc == sp$mc_aggr, "sbp_1y"])
  
  # Efficacy for SBP at 2nd year of treatment
  tbl <- read_fst("./inputs/other_parameters/sbp_2y_samples_semag.fst", as.data.table = TRUE)
  sbp_2y <- as.numeric(tbl[mc == sp$mc_aggr, "sbp_2y"])
  
  # Efficacy for total cholesterol at 1st year of treatment
  tbl <- read_fst("./inputs/other_parameters/tchol_1y_samples_semag.fst", as.data.table = TRUE)
  tchol_1y <- as.numeric(tbl[mc == sp$mc_aggr, "tchol_1y"])
  
  # Efficacy for total cholesterol at 2nd year of treatment
  tbl <- read_fst("./inputs/other_parameters/tchol_2y_samples_semag.fst", as.data.table = TRUE)
  tchol_2y <- as.numeric(tbl[mc == sp$mc_aggr, "tchol_2y"])
  ###########################################################################################################
  
  # Load the pids of uptake patients for each iteration
  persons_bia_N <- fread(paste0("./inputs/uptake/", sp$mc_aggr, "_uptake_bia_N.csv"))
  ### fread() is highly optimized: it’s faster than read_csv(), and it returns a data.table automatically.
  
  # Join uptake info back to lifecourse rows
  sp$pop <- merge(sp$pop, persons_bia_N, by = "pid", all.x = TRUE)
  
  # Create a row-level indicator for uptake event (1 only in the uptake year row)
  sp$pop[, uptake_one := as.integer(!is.na(uptake_year) & year == uptake_year)]
  # --- !is.na(uptake_year): checks whether the uptake_year column is no NA, returns 'TRUE' or 'FALSE'
  # --- year == uptake_year: checks whether the value in 'year' column is equal to the value in 'uptake_year' column.
  #                          also returns 'TRUE' or 'FALSE'
  # --- as.integer(...):     converts the result of the logical expression into an integer.
  #                          In R, as.integer(TRUE) becomes 1 and as.integer(False) becomes 0.
  
  # a variable for the year someone uptaking the drug
  sp$pop[, anchor_year:=year[uptake_one == 1], by = pid]
  
  # a variable for the year someone enters the synthetic population
  # sp$pop[, entry_year:= min(year), by = pid]
  
  # a variable for theorectical treatment trajectory
  ### Step 0: Initialize the column
  sp$pop[, trtm_theo := NA_integer_]
  ### Step 1: Identify first year of treatment uptake
  sp$pop[uptake_one == 1, trtm_theo := 0]
  ### Step 2: Fill the sequence in subsequent years
  sp$pop[, trtm_theo := fifelse(cumsum(!is.na(trtm_theo)) > 0,   # cumsum(): computes a cumulative count of non-NA entries:
                                # fill in a sequence starting from the first non-NA value
                                seq_len(.N) - min(which(!is.na(trtm_theo))),
                                NA_integer_),
         by = pid]
  ### Step 3: Replace NAs with 0
  sp$pop[, trtm_theo := fifelse(is.na(trtm_theo), 0, trtm_theo)]
  
  ########################################################################################
  ################################          BMI         ##################################
  ########################################################################################
  # a variable for the change in BMI after the uptake of semaglutide
  sp$pop[, `:=`(
    bmi_delta = fcase(
      ### All values until year 5, are % weight loss relative to baseline
      trtm_theo == 0, 0,       ### Baseline year, the year patients become eligible
      trtm_theo == 1, bmi_1y,  ### 1st year of treatment: ITT effects from STEP 1 (1 year f/u): 14.9% weight loss (uncertainty?)
      trtm_theo == 2, bmi_2y,  ### 2nd year of treatment: ITT effects from STEP 5 (2 year f/u): 15.2% weight loss (uncertainty?)
      trtm_theo == 3, bmi_3y,  ### STEP 1 extension: regain 2/3 of weight loss in 1st year treatment cessation (uncertainty?)
      trtm_theo == 4, bmi_4y,  ### weight regain at 2nd year treatment cessation (uncertainty?)
      trtm_theo >= 5, 0        ### weight return to baseline weight by 3rd year treatment cessation (uncertainty?)
      ### The efficacy inputs are assigned until here, the rest we will revert to the original exposure column (bmi_curr_xps)
    ))
  ]
  
  # a variable for each individual's baseline weight (baseline: year == 24 / ys_rollout == 0)
  # ------------------------------------------ Aug 22, 2025, Jane ------------------------------------------------- #
  # *but, some individuals in the dataset does not have entry at baseline_yr, they came in later than year 24
  # *and, some individuals came in later than year 24, and because they have a chance to uptake drug begining year 25
  # *they might already uptake the drug at the first year, which means they won't have a year (anchor-1),
  # *so what shall we use as a baseline BMI value here...
  sp$pop[, baseline_bmi := {              # sp$pop[, ... , by = pid]: grouping the operation by pid
    anchor <- unique(anchor_year)         # So we can distinguish people who have entry at year 24 and who do not
    if (is.na(anchor)) {                  # If no anchor is found,
      bmi_curr_xps                        # the function simply returns the original bmi_curr_xps as-is
    } else {                              # If yes anchor is found
      bmi_curr_xps[year == (year[trtm_theo == 1][1] - 1)] # baseline_bmi will take the BMI at the year before trtm_theo==1
    }
  }, by = pid] #Works!
  
  ###############################################################################################################
  #----------------------------  Tutorial: if_else function inside data.table  ---------------------------------#
  ###############################################################################################################
  sp$pop[, bmi_shift := {             # sp$pop[, ... , by = pid]: grouping the operation by pid
    # { ... } operetion per group: inside {}, you can run any custom R code, it will
    # return a vector that gets assigned to 'bmi_shift' for the rows in that group
    anchor <- unique(anchor_year)     # This line finds a single anchor year (e.g., year 30) for each person.
    if (is.na(anchor)) {              # If no anchor is found (to avoid breaking your logic when a person doesn't uptake the drug)
      bmi_curr_xps                    # the function simply returns the original bmi_curr_xps as-is
    } else {                          # If yes anchor is found, You shift the bmi_curr_xps backward in time (type='lag')
      shift(bmi_curr_xps, type = "lag", n = 5) # by 5 years: 5 years is the duration of efficacy inputs (weight loss and reboud)
    }
  }, by = pid] #Works!
  ###############################################################################################################
  #--------------------------------------------  Tutorial: End  ------------------------------------------------#
  ###############################################################################################################
  
  # a variable for new weight after applying efficacy input during treatment influenced period
  sp$pop[, new_bmi := {
    anchor <- unique(anchor_year)                             # This line finds a single anchor year for each person.
    if (is.na(anchor)) {                                      # If no anchor is found,
      bmi_curr_xps                                            # the function simply returns the original bmi_curr_xps as-is
    } else {                                                  # If yes anchor is found,
      fcase(                                                  # We need to assign value to the' new_bmi' based on the time periods
        year < anchor,                       bmi_curr_xps,    # Before anchor year/uptake: original values (including baseline year)
        year >= anchor & year <= anchor + 4, baseline_bmi*(1 + bmi_delta),  # During the 5 years where treatment effects manifest
        year > anchor + 4,                   bmi_shift        # After rebound to baseline: revert to the original bmi_curr_xps
      )
    }
  }, by = pid]
  
  # Overwriting the original bmi exposure column with the newly created bmi column
  sp$pop[, bmi_curr_xps := new_bmi]
  
  ########################################################################################
  ################################          SBP         ##################################
  ########################################################################################
  # a variable for the change in SBP after the uptake of semaglutide
  sp$pop[, `:=`(
    sbp_delta = fcase(
      ### All values until year 5, are % weight loss relative to baseline
      trtm_theo == 0, 0,       ### Baseline year, the year patients become eligible
      trtm_theo == 1, sbp_1y,    ### 1st year of treatment: ITT effects from STEP 1 (1 year f/u): mean decrease of 6.2 mmHg from baseline
      trtm_theo == 2, sbp_2y,    ### 2nd year of treatment: ITT effects from STEP 5 (2 year f/u): mean decrease of 5.7 mmHg from baseline
      trtm_theo >= 3, 0        ### STEP 1 extension: assume mean SBP will revert to baseline level after one year without treatment
      ### The efficacy inputs are assigned until here, the rest we will revert to the original exposure column (sbp_curr_xps)
    ))
  ]
  
  # a variable for each individual's baseline SBP (baseline: year == 24 / ys_rollout == 0)
  # *but, some individuals in the dataset does not have entry at baseline_yr, they came in later than year 24
  sp$pop[, baseline_sbp := {                # sp$pop[, ... , by = pid]: grouping the operation by pid
    anchor <- unique(anchor_year)           # So we can distinguish people who have entry at year 24 and who do not
    if (is.na(anchor)) {                    # If no anchor is found,
      sbp_curr_xps                          # the function simply returns the original sbp_curr_xps as-is
    } else {                                # If yes anchor is found, baseline_sbp will take the sbp at year 24
      sbp_curr_xps[year == (year[trtm_theo == 1][1] - 1)]  # The baseline year will be (anchor - 1): one year before uptake
    }
  }, by = pid] #Works!
  
  ###############################################################################################################
  #----------------------------  Tutorial: if_else function inside data.table  ---------------------------------#
  ###############################################################################################################
  sp$pop[, sbp_shift := {                 # sp$pop[, ... , by = pid]: grouping the operation by pid
    # { ... } operetion per group: inside {}, you can run any custom R code, it will
    # return a vector that gets assigned to 'sbp_shift' for the rows in that group
    
    anchor <- unique(anchor_year)        # This line finds a single anchor year (e.g., year 30) for each person.
    if (is.na(anchor)) {                 # If no anchor is found, (This is useful to avoid breaking your logic when a person doesn't meet your anchor condition)
      sbp_curr_xps                       # the function simply returns the original sbp_curr_xps as-is
    } else {                             # If yes anchor is found, You shift the sbp_curr_xps backward in time (type='lag')
      shift(sbp_curr_xps, type = "lag", n = 3) # by 3 years: 3 years is the duration of efficacy inputs (decrease in SBP and rebound)
    }
  }, by = pid] #Works!
  
  #  A if_else function inside of :={} for creating a new variable
  #  if(condition) {
  #   what should happen if condition is TRUE
  #  } else {
  #   what should happen if condition is FALSE
  #  }
  ###############################################################################################################
  #--------------------------------------------  Tutorial: End  ------------------------------------------------#
  ###############################################################################################################
  
  # a variable for new weight after applying efficacy input during treatment influenced period
  sp$pop[, new_sbp := {
    anchor <- unique(anchor_year)                             # This line finds a single anchor year for each person.
    if (is.na(anchor)) {                                      # If no anchor is found,
      sbp_curr_xps                                            # the function simply returns the original sbp_curr_xps as-is
    } else {                                                  # If yes anchor is found,
      fcase(                                                  # We need to assign value to the 'new_sbp' based on the time periods
        year < anchor,                       sbp_curr_xps,    # Before anchor year/uptake: original values (including baseline year)
        year >= anchor & year <= anchor + 2, baseline_sbp + sbp_delta,  # During the 2 years where treatment effect manifest
        year >= anchor + 3,                  sbp_shift        # After sbp rebound to baseline: revert to the original sbp_curr_xps
      )
    }
  }, by = pid]
  
  # Overwriting the original sbp exposure column with the newly created sbp column
  sp$pop[, sbp_curr_xps := new_sbp]
  
  ########################################################################################
  ###############################          Tchol         #################################
  ########################################################################################
  # a variable for the change in SBP after the uptake of semaglutide
  sp$pop[, `:=`(
    tchol_delta = fcase(
      trtm_theo == 0, 0,        ### Baseline year, the year patients become eligible
      trtm_theo == 1, tchol_1y,    ### 1st year of treatment: ITT effects from STEP 1: mean decrease of 3% from baseline
      trtm_theo == 2, tchol_2y,   ### 2nd year of treatment: ITT effects from STEP 5: mean decrease of 3.3% from baseline
      trtm_theo >= 3, 0         ### STEP 1 extension: assume mean tchol will revert to baseline level after one year without treatment
      ### The efficacy inputs are assigned until here, the rest we will revert to the original exposure column (tchol_curr_xps)
    ))
  ]
  
  # a variable for each individual's baseline Tchol (baseline: year == 24 / ys_rollout == 0)
  # *but, some individuals in the dataset does not have entry at baseline_yr, they came in later than year 24
  sp$pop[, baseline_tchol := {                 # sp$pop[, ... , by = pid]: grouping the operation by pid
    anchor <- unique(anchor_year)           # So we can distinguish people who have entry at year 24 and who do not
    if (is.na(anchor)) {                    # If no anchor is found,
      tchol_curr_xps                        # the function simply returns the original tchol_curr_xps as-is
    } else {                                # If yes anchor is found, baseline_tchol will take the tchol at year 24
      tchol_curr_xps[year == (year[trtm_theo == 1][1] - 1)]  # The baseline year will be (anchor - 1): one year before uptake
    }
  }, by = pid] #Works!
  
  ###############################################################################################################
  #----------------------------  Tutorial: if_else function inside data.table  ---------------------------------#
  ###############################################################################################################
  sp$pop[, tchol_shift := {                 # sp$pop[, ... , by = pid]: grouping the operation by pid
    # { ... } operetion per group: inside {}, you can run any custom R code, it will
    # return a vector that gets assigned to 'tchol_shift' for the rows in that group
    
    anchor <- unique(anchor_year)        # This line finds a single anchor year (e.g., year 30) for each person.
    if (is.na(anchor)) {                 # If no anchor is found, (This is useful to avoid breaking your logic when a person doesn't meet your anchor condition)
      tchol_curr_xps                       # the function simply returns the original tchol_curr_xps as-is
    } else {                             # If yes anchor is found, You shift the tchol_curr_xps backward in time (type='lag')
      shift(tchol_curr_xps, type = "lag", n = 3) # by 3 years: 3 years is the duration of efficacy inputs (decrease in tchol and reboud)
    }
  }, by = pid] #Works!
  
  # test <- sp$pop[pid %in% c(156, 175)]
  ###############################################################################################################
  #--------------------------------------------  Tutorial: End  ------------------------------------------------#
  ###############################################################################################################
  
  # a variable for new weight after applying efficacy input during treatment influenced period
  sp$pop[, new_tchol := {
    anchor <- unique(anchor_year)                             # This line finds a single anchor year for each person.
    if (is.na(anchor)) {                                      # If no anchor is found,
      tchol_curr_xps                                          # the function simply returns the original tchol_curr_xps as-is
    } else {                                                  # If yes anchor is found,
      fcase(                                                  # We need to assign value to the 'new_tchol' based on the time periods
        year < anchor,                       tchol_curr_xps,  # Before anchor year/uptake: original values (including baseline year)
        year >= anchor & year <= anchor + 2, baseline_tchol*(1 + tchol_delta),  # During the 2 years where treatment effect manifest
        year >= anchor + 3,                  tchol_shift      # After tchol rebound to baseline: revert to the original tchol_curr_xps
      )
    }
  }, by = pid]
  
  # Overwriting the original tchol exposure column with the newly created tchol column
  sp$pop[, tchol_curr_xps := new_tchol]
  ### Jane Aug 2025, test scenario on sp$pop --> Looks like until here everything worked!!!
  
  ##################### Get rid of unnecessary variables ##########################
  # Delete unnecessary variables from synthpop #
  sp$pop[, c("rankstat_sbp", "rankstat_tchol", "ys_rollout", "uptake_rate",
             "uptake_psyr", "uptake_one", 
             "entry_year", "baseline_bmi", "bmi_shift", "new_bmi",
             "baseline_sbp", "sbp_shift", "new_sbp", "baseline_tchol",
             "tchol_shift", "new_tchol") := NULL]
  
}

##################################################################################################################
#----------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------#
#--------------------------------------------    Scenario 10    -------------------------------------------------#
#-----------------   Semaglutide for 100,000 patients uptake every year, lifetime treatment   -------------------#
#----------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------#
##################################################################################################################

scenario_10_fn <- function(sp) {
  
  ###########################################################################################################
  #------------------------------------- Loading uncertainty files -----------------------------------------#
  #------ For lifetime treatment, we only need the effects at year 1&2, because weight do not rebound ------#
  #---------------------------------------------------------------------------------------------------------#
  # Efficacy for bmi at 1st year after treatment
  tbl <- read_fst("./inputs/other_parameters/bmi_1y_samples_semag.fst", as.data.table = TRUE)
  bmi_1y <- as.numeric(tbl[mc == sp$mc_aggr, "bmi_1y"])
  
  # Efficacy for bmi at 2nd year after treatment
  tbl <- read_fst("./inputs/other_parameters/bmi_2y_samples_semag.fst", as.data.table = TRUE)
  bmi_2y <- as.numeric(tbl[mc == sp$mc_aggr, "bmi_2y"])
  
  # Efficacy for SBP at 1st year after treatment
  tbl <- read_fst("./inputs/other_parameters/sbp_1y_samples_semag.fst", as.data.table = TRUE)
  sbp_1y <- as.numeric(tbl[mc == sp$mc_aggr, "sbp_1y"])
  
  # Efficacy for SBP at 2nd year of treatment
  tbl <- read_fst("./inputs/other_parameters/sbp_2y_samples_semag.fst", as.data.table = TRUE)
  sbp_2y <- as.numeric(tbl[mc == sp$mc_aggr, "sbp_2y"])
  
  # Efficacy for total cholesterol at 1st year of treatment
  tbl <- read_fst("./inputs/other_parameters/tchol_1y_samples_semag.fst", as.data.table = TRUE)
  tchol_1y <- as.numeric(tbl[mc == sp$mc_aggr, "tchol_1y"])
  
  # Efficacy for total cholesterol at 2nd year of treatment
  tbl <- read_fst("./inputs/other_parameters/tchol_2y_samples_semag.fst", as.data.table = TRUE)
  tchol_2y <- as.numeric(tbl[mc == sp$mc_aggr, "tchol_2y"])
  ###########################################################################################################
  
  # Load the pids of uptake patients for each iteration
  persons_bia_N <- fread(paste0("./inputs/uptake/", sp$mc_aggr, "_uptake_bia_N.csv"))
  ### fread() is highly optimized: it’s faster than read_csv(), and it returns a data.table automatically.
  
  # Join uptake info back to lifecourse rows
  sp$pop <- merge(sp$pop, persons_bia_N, by = "pid", all.x = TRUE)
  
  # Create a row-level indicator for uptake event (1 only in the uptake year row)
  sp$pop[, uptake_one := as.integer(!is.na(uptake_year) & year == uptake_year)]
  # --- !is.na(uptake_year): checks whether the uptake_year column is no NA, returns 'TRUE' or 'FALSE'
  # --- year == uptake_year: checks whether the value in 'year' column is equal to the value in 'uptake_year' column.
  #                          also returns 'TRUE' or 'FALSE'
  # --- as.integer(...):     converts the result of the logical expression into an integer.
  #                          In R, as.integer(TRUE) becomes 1 and as.integer(False) becomes 0.
  
  # a variable for the year someone uptaking the drug
  sp$pop[, anchor_year:=year[uptake_one == 1], by = pid]
  
  # a variable for the year someone enters the synthetic population
  # sp$pop[, entry_year:= min(year), by = pid]
  
  # a variable for theorectical treatment trajectory
  ### Step 0: Initialize the column
  sp$pop[, trtm_theo := NA_integer_]
  ### Step 1: Identify first year of treatment uptake
  sp$pop[uptake_one == 1, trtm_theo := 0]
  ### Step 2: Fill the sequence in subsequent years
  sp$pop[, trtm_theo := fifelse(cumsum(!is.na(trtm_theo)) > 0,   
                                # cumsum(): computes a cumulative count of non-NA entries:
                                # fill in a sequence starting from the first non-NA value
                                seq_len(.N) - min(which(!is.na(trtm_theo))),
                                NA_integer_),
         by = pid]
  ### Step 3: Replace NAs with 0
  sp$pop[, trtm_theo := fifelse(is.na(trtm_theo), 0, trtm_theo)]
  
  ########################################################################################
  ################################          BMI         ##################################
  ########################################################################################
  # a variable for the change in BMI after the uptake of semaglutide
  sp$pop[, `:=`(
    bmi_delta = fcase(
      ### All values until year 5, are % weight loss relative to baseline
      trtm_theo == 0, 0,       ### Baseline year, the year patients become eligible
      trtm_theo == 1, bmi_1y,  ### replace '-0.149' with 'bmi_1y', and link to the uncertainty file: mc + bmi_1y
      ### 1st year of treatment: ITT effects from STEP 1 (1 year f/u): 14.9% weight loss
      trtm_theo >= 2, bmi_2y  ### replace '-0.152' with 'bmi_2y', and link to the uncertainty file: mc + bmi_2y
      ### 2nd year of treatment: ITT effects from STEP 5 (2 year f/u): 15.2% weight loss
    ))
  ]
  
  # a variable for each individual's baseline weight (baseline: year == 24 / ys_rollout == 0)
  # *but, some individuals in the dataset does not have entry at baseline_yr, they came in later than year 24
  # *and, some individuals came in later than year 24, and because they have a chance to uptake drug begining year 25
  # *they might already uptake the drug at the first year, which means they won't have a year (anchor-1),
  # *so what shall we use as a baseline BMI value here...
  sp$pop[, baseline_bmi := {              # sp$pop[, ... , by = pid]: grouping the operation by pid
    anchor <- unique(anchor_year)         # So we can distinguish people who have entry at year 24 and who do not
    if (is.na(anchor)) {                  # If no anchor is found,
      bmi_curr_xps                        # the function simply returns the original bmi_curr_xps as-is
    } else {                              # If yes anchor is found
      bmi_curr_xps[year == (year[trtm_theo == 1][1] - 1)] # baseline_bmi will take the BMI at the year before trtm_theo==1
    }
  }, by = pid] #Works!
  
  
  # a variable for new weight after applying efficacy input during treatment influenced period
  sp$pop[, new_bmi := {
    anchor <- unique(anchor_year)                             # This line finds a single anchor year for each person.
    if (is.na(anchor)) {                                      # If no anchor is found,
      bmi_curr_xps                                            # the function simply returns the original bmi_curr_xps as-is
    } else {                                                  # If yes anchor is found,
      fcase(                                                  # We need to assign value to the' new_bmi' based on the time periods
        year < anchor,      bmi_curr_xps,                   # Before anchor year/uptake: original values (including baseline year)
        year >= anchor,     baseline_bmi*(1 + bmi_delta)    # After anchor year, the lifetime treatment lead to lifetime effects
      )
    }
  }, by = pid]
  
  # Overwriting the original bmi exposure column with the newly created bmi column
  sp$pop[, bmi_curr_xps := new_bmi]
  
  ########################################################################################
  ################################          SBP         ##################################
  ########################################################################################
  # a variable for the change in SBP after the uptake of semaglutide
  sp$pop[, `:=`(
    sbp_delta = fcase(
      ### All values until year 5, are % weight loss relative to baseline
      trtm_theo == 0, 0,       ### Baseline year, the year patients become eligible
      trtm_theo == 1, sbp_1y,    ### 1st year of treatment: ITT effects from STEP 1 (1 year f/u): mean decrease of 6.2 mmHg from baseline
      trtm_theo >= 2, sbp_2y     ### 2nd year of treatment: ITT effects from STEP 5 (2 year f/u): mean decrease of 5.7 mmHg from baseline
    ))
  ]
  
  # a variable for each individual's baseline SBP (baseline: year == 24 / ys_rollout == 0)
  # *but, some individuals in the dataset does not have entry at baseline_yr, they came in later than year 24
  sp$pop[, baseline_sbp := {                # sp$pop[, ... , by = pid]: grouping the operation by pid
    anchor <- unique(anchor_year)           # So we can distinguish people who have entry at year 24 and who do not
    if (is.na(anchor)) {                    # If no anchor is found,
      sbp_curr_xps                          # the function simply returns the original sbp_curr_xps as-is
    } else {                                # If yes anchor is found, baseline_sbp will take the sbp at year 24
      sbp_curr_xps[year == (year[trtm_theo == 1][1] - 1)]  # The baseline year will be (anchor - 1): one year before uptake
    }
  }, by = pid] #Works!
  
  # a variable for new weight after applying efficacy input during treatment influenced period
  sp$pop[, new_sbp := {
    anchor <- unique(anchor_year)                           # This line finds a single anchor year for each person.
    if (is.na(anchor)) {                                    # If no anchor is found,
      sbp_curr_xps                                          # the function simply returns the original sbp_curr_xps as-is
    } else {                                                # If yes anchor is found,
      fcase(                                                # We need to assign value to the 'new_sbp' based on the time periods
        year < anchor,  sbp_curr_xps,              # Before anchor year/uptake: original values (including baseline year)
        year >= anchor, baseline_sbp + sbp_delta  # After anchor year, the lifetime treatment lead to lifetime effects
      )
    }
  }, by = pid]
  
  # Overwriting the original sbp exposure column with the newly created sbp column
  sp$pop[, sbp_curr_xps := new_sbp]
  
  ########################################################################################
  ###############################          Tchol         #################################
  ########################################################################################
  # a variable for the change in SBP after the uptake of semaglutide
  sp$pop[, `:=`(
    tchol_delta = fcase(
      trtm_theo == 0, 0,          ### Baseline year, the year patients become eligible
      trtm_theo == 1, tchol_1y,   ### 1st year of treatment: ITT effects from STEP 1: mean decrease of 3% from baseline
      trtm_theo >= 2, tchol_2y    ### 2nd year of treatment: ITT effects from STEP 5: mean decrease of 3.3% from baseline
    ))
  ]
  
  # a variable for each individual's baseline Tchol (baseline: year == 24 / ys_rollout == 0)
  # *but, some individuals in the dataset does not have entry at baseline_yr, they came in later than year 24
  sp$pop[, baseline_tchol := {                 # sp$pop[, ... , by = pid]: grouping the operation by pid
    anchor <- unique(anchor_year)           # So we can distinguish people who have entry at year 24 and who do not
    if (is.na(anchor)) {                    # If no anchor is found,
      tchol_curr_xps                        # the function simply returns the original tchol_curr_xps as-is
    } else {                                # If yes anchor is found, baseline_tchol will take the tchol at year 24
      tchol_curr_xps[year == (year[trtm_theo == 1][1] - 1)]  # The baseline year will be (anchor - 1): one year before uptake
    }
  }, by = pid] #Works!
  
  # a variable for new weight after applying efficacy input during treatment influenced period
  sp$pop[, new_tchol := {
    anchor <- unique(anchor_year)                             # This line finds a single anchor year for each person.
    if (is.na(anchor)) {                                      # If no anchor is found,
      tchol_curr_xps                                          # the function simply returns the original tchol_curr_xps as-is
    } else {                                                  # If yes anchor is found,
      fcase(                                                  # We need to assign value to the 'new_tchol' based on the time periods
        year < anchor,    tchol_curr_xps,                  # Before anchor year/uptake: original values (including baseline year)
        year >= anchor,   baseline_tchol*(1 + tchol_delta) # After anchor year, the lifetime treatment lead to lifetime effects
      )
    }
  }, by = pid]
  
  # Overwriting the original tchol exposure column with the newly created tchol column
  sp$pop[, tchol_curr_xps := new_tchol]
  ### Jane Aug 2025, test scenario on sp$pop --> Looks like until here everything worked!!!
  
  ##################### Get rid of unnecessary variables ##########################
  # Delete unnecessary variables from synthpop #
  sp$pop[, c("rankstat_sbp", "rankstat_tchol", "ys_rollout", "uptake_rate",
             "uptake_psyr", "uptake_one", 
             "entry_year", "baseline_bmi", "bmi_shift", "new_bmi",
             "baseline_sbp", "sbp_shift", "new_sbp", "baseline_tchol",
             "tchol_shift", "new_tchol") := NULL]
  
}

##################################################################################################################
#----------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------#
#--------------------------------------------    Scenario 11    -------------------------------------------------#
#-------------------------   Tirzepatide for 10% uptake every year, 2 years treatment   -------------------------#
#----------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------#
##################################################################################################################

scenario_11_fn <- function(sp) {
  
  ###########################################################################################################
  #------------------------------------- Loading uncertainty files -----------------------------------------#
  # Efficacy for bmi at 1st year after treatment
  tbl <- read_fst("./inputs/other_parameters/bmi_1y_samples_tirzp.fst", as.data.table = TRUE)
  bmi_1y <- as.numeric(tbl[mc == sp$mc_aggr, "bmi_1y"])
  
  # Efficacy for bmi at 2nd year after treatment
  tbl <- read_fst("./inputs/other_parameters/bmi_1y_samples_tirzp.fst", as.data.table = TRUE)
  bmi_2y <- as.numeric(tbl[mc == sp$mc_aggr, "bmi_1y"])
  
  # Efficacy for bmi at 3rd year after treatment
  tbl <- read_fst("./inputs/other_parameters/bmi_3y_samples_tirzp.fst", as.data.table = TRUE)
  bmi_3y <- as.numeric(tbl[mc == sp$mc_aggr, "bmi_3y"])
  
  # Efficacy for bmi at 4th year after treatment
  tbl <- read_fst("./inputs/other_parameters/bmi_4y_samples_tirzp.fst", as.data.table = TRUE)
  bmi_4y <- as.numeric(tbl[mc == sp$mc_aggr, "bmi_4y"])
  
  # Efficacy for SBP at 1st year after treatment
  tbl <- read_fst("./inputs/other_parameters/sbp_1y_samples_tirzp.fst", as.data.table = TRUE)
  sbp_1y <- as.numeric(tbl[mc == sp$mc_aggr, "sbp_1y"])
  
  # Efficacy for SBP at 2nd year of treatment
  tbl <- read_fst("./inputs/other_parameters/sbp_1y_samples_tirzp.fst", as.data.table = TRUE)
  sbp_2y <- as.numeric(tbl[mc == sp$mc_aggr, "sbp_1y"])
  
  # Efficacy for total cholesterol at 1st year of treatment
  tbl <- read_fst("./inputs/other_parameters/tchol_1y_samples_tirzp.fst", as.data.table = TRUE)
  tchol_1y <- as.numeric(tbl[mc == sp$mc_aggr, "tchol_1y"])
  
  # Efficacy for total cholesterol at 2nd year of treatment
  tbl <- read_fst("./inputs/other_parameters/tchol_1y_samples_tirzp.fst", as.data.table = TRUE)
  tchol_2y <- as.numeric(tbl[mc == sp$mc_aggr, "tchol_1y"])
  ###########################################################################################################
  
  # Load the pids of uptake patients for each iteration
  persons_bia_pct <- fread(paste0("./inputs/uptake/", sp$mc_aggr, "_uptake_bia_pct.csv"))
  ### fread() is highly optimized: it’s faster than read_csv(), and it returns a data.table automatically.
  
  # Join uptake info back to lifecourse rows
  sp$pop <- merge(sp$pop, persons_bia_pct, by = "pid", all.x = TRUE)
  
  # Create a row-level indicator for uptake event (1 only in the uptake year row)
  sp$pop[, uptake_one := as.integer(!is.na(uptake_year) & year == uptake_year)]
  # --- !is.na(uptake_year): checks whether the uptake_year column is no NA, returns 'TRUE' or 'FALSE'
  # --- year == uptake_year: checks whether the value in 'year' column is equal to the value in 'uptake_year' column.
  #                          also returns 'TRUE' or 'FALSE'
  # --- as.integer(...):     converts the result of the logical expression into an integer.
  #                          In R, as.integer(TRUE) becomes 1 and as.integer(False) becomes 0.
  
  # a variable for the year someone uptaking the drug
  sp$pop[, anchor_year:=year[uptake_one == 1], by = pid]
  
  # a variable for the year someone enters the synthetic population
  # sp$pop[, entry_year:= min(year), by = pid]
  
  # a variable for theorectical treatment trajectory
  ### Step 0: Initialize the column
  sp$pop[, trtm_theo := NA_integer_]
  ### Step 1: Identify first year of treatment uptake
  sp$pop[uptake_one == 1, trtm_theo := 0]
  ### Step 2: Fill the sequence in subsequent years
  sp$pop[, trtm_theo := fifelse(cumsum(!is.na(trtm_theo)) > 0,   # cumsum(): computes a cumulative count of non-NA entries:
                                # fill in a sequence starting from the first non-NA value
                                seq_len(.N) - min(which(!is.na(trtm_theo))),
                                NA_integer_),
         by = pid]
  ### Step 3: Replace NAs with 0
  sp$pop[, trtm_theo := fifelse(is.na(trtm_theo), 0, trtm_theo)]
  
  ########################################################################################
  ################################          BMI         ##################################
  ########################################################################################
  # a variable for the change in BMI after the uptake of Tirzepatide
  sp$pop[, `:=`(
    bmi_delta = fcase(
      ### All values until year 5, are % weight loss relative to baseline
      trtm_theo == 0, 0,       ### Baseline year, the year patients become eligible
      trtm_theo == 1, bmi_1y,  ### replace '-0.209' with 'bmi_1y', and link to the uncertainty file: mc + bmi_1y
      ### 1st year of treatment: ITT effects from SURMOUNT-1 (1 year f/u): 20.9% weight loss
      trtm_theo == 2, bmi_2y,  ### replace '-0.209' with 'bmi_2y', and link to the uncertainty file: mc + bmi_2y
      ### 2nd year of treatment: ITT effects from SURMOUNT-1 (2 year f/u): 20.9% weight loss
      trtm_theo == 3, bmi_3y,  ### SURMOUNT-4: regain ~2/3 of weight loss in 1st year treatment cessation
      ###
      trtm_theo == 4, bmi_4y,  ### weight regain at 2nd year treatment cessation
      trtm_theo >= 5, 0        ### weight return to baseline weight by 3rd year treatment cessation
      ### The efficacy inputs are assigned until here, the rest we will revert to the original exposure column (bmi_curr_xps)
    ))
  ]
  
  # a variable for each individual's baseline weight (baseline: year == 24 / ys_rollout == 0)
  # *but, some individuals in the dataset does not have entry at baseline_yr, they came in later than year 24
  # *and, some individuals came in later than year 24, and because they have a chance to uptake drug begining year 25
  # *they might already uptake the drug at the first year, which means they won't have a year (anchor-1),
  # *so what shall we use as a baseline BMI value here...
  sp$pop[, baseline_bmi := {              # sp$pop[, ... , by = pid]: grouping the operation by pid
    anchor <- unique(anchor_year)         # So we can distinguish people who have entry at year 24 and who do not
    if (is.na(anchor)) {                  # If no anchor is found,
      bmi_curr_xps                        # the function simply returns the original bmi_curr_xps as-is
    } else {                              # If yes anchor is found
      bmi_curr_xps[year == (year[trtm_theo == 1][1] - 1)] # baseline_bmi will take the BMI at the year before trtm_theo==1
    }
  }, by = pid] #Works!
  
  ###############################################################################################################
  #----------------------------  Tutorial: if_else function inside data.table  ---------------------------------#
  ###############################################################################################################
  sp$pop[, bmi_shift := {             # sp$pop[, ... , by = pid]: grouping the operation by pid
    # { ... } operetion per group: inside {}, you can run any custom R code, it will
    # return a vector that gets assigned to 'bmi_shift' for the rows in that group
    anchor <- unique(anchor_year)     # This line finds a single anchor year (e.g., year 30) for each person.
    if (is.na(anchor)) {              # If no anchor is found (to avoid breaking your logic when a person doesn't uptake the drug)
      bmi_curr_xps                    # the function simply returns the original bmi_curr_xps as-is
    } else {                          # If yes anchor is found, You shift the bmi_curr_xps backward in time (type='lag')
      shift(bmi_curr_xps, type = "lag", n = 5) # by 5 years: 5 years is the duration of efficacy inputs (weight loss and reboud)
    }
  }, by = pid] #Works!
  ###############################################################################################################
  #--------------------------------------------  Tutorial: End  ------------------------------------------------#
  ###############################################################################################################
  
  # a variable for new weight after applying efficacy input during treatment influenced period
  sp$pop[, new_bmi := {
    anchor <- unique(anchor_year)                             # This line finds a single anchor year for each person.
    if (is.na(anchor)) {                                      # If no anchor is found,
      bmi_curr_xps                                            # the function simply returns the original bmi_curr_xps as-is
    } else {                                                  # If yes anchor is found,
      fcase(                                                  # We need to assign value to the' new_bmi' based on the time periods
        year < anchor,                       bmi_curr_xps,    # Before anchor year/uptake: original values (including baseline year)
        year >= anchor & year <= anchor + 4, baseline_bmi*(1 + bmi_delta),  # During the 5 years where treatment effects manifest
        year > anchor + 4,                   bmi_shift        # After rebound to baseline: revert to the original bmi_curr_xps
      )
    }
  }, by = pid]
  
  # Overwriting the original bmi exposure column with the newly created bmi column
  sp$pop[, bmi_curr_xps := new_bmi]
  
  ########################################################################################
  ################################          SBP         ##################################
  ########################################################################################
  # a variable for the change in SBP after the uptake of semaglutide
  sp$pop[, `:=`(
    sbp_delta = fcase(
      ### All values until year 5, are % weight loss relative to baseline
      trtm_theo == 0, 0,       ### Baseline year, the year patients become eligible
      trtm_theo == 1, sbp_1y,    ### 1st year of treatment: ITT effects from STEP 1 (1 year f/u): mean decrease of 6.2 mmHg from baseline
      trtm_theo == 2, sbp_2y,    ### 2nd year of treatment: ITT effects from STEP 5 (2 year f/u): mean decrease of 5.7 mmHg from baseline
      trtm_theo >= 3, 0        ### STEP 1 extension: assume mean SBP will revert to baseline level after one year without treatment
      ### The efficacy inputs are assigned until here, the rest we will revert to the original exposure column (sbp_curr_xps)
    ))
  ]
  
  # a variable for each individual's baseline SBP (baseline: year == 24 / ys_rollout == 0)
  # *but, some individuals in the dataset does not have entry at baseline_yr, they came in later than year 24
  sp$pop[, baseline_sbp := {                # sp$pop[, ... , by = pid]: grouping the operation by pid
    anchor <- unique(anchor_year)           # So we can distinguish people who have entry at year 24 and who do not
    if (is.na(anchor)) {                    # If no anchor is found,
      sbp_curr_xps                          # the function simply returns the original sbp_curr_xps as-is
    } else {                                # If yes anchor is found, baseline_sbp will take the sbp at year 24
      sbp_curr_xps[year == (year[trtm_theo == 1][1] - 1)]  # The baseline year will be (anchor - 1): one year before uptake
    }
  }, by = pid] #Works!
  
  ###############################################################################################################
  #----------------------------  Tutorial: if_else function inside data.table  ---------------------------------#
  ###############################################################################################################
  sp$pop[, sbp_shift := {                 # sp$pop[, ... , by = pid]: grouping the operation by pid
    # { ... } operetion per group: inside {}, you can run any custom R code, it will
    # return a vector that gets assigned to 'sbp_shift' for the rows in that group
    
    anchor <- unique(anchor_year)        # This line finds a single anchor year (e.g., year 30) for each person.
    if (is.na(anchor)) {                 # If no anchor is found, (This is useful to avoid breaking your logic when a person doesn't meet your anchor condition)
      sbp_curr_xps                       # the function simply returns the original sbp_curr_xps as-is
    } else {                             # If yes anchor is found, You shift the sbp_curr_xps backward in time (type='lag')
      shift(sbp_curr_xps, type = "lag", n = 3) # by 3 years: 3 years is the duration of efficacy inputs (decrease in SBP and rebound)
    }
  }, by = pid] #Works!
  
  #  A if_else function inside of :={} for creating a new variable
  #  if(condition) {
  #   what should happen if condition is TRUE
  #  } else {
  #   what should happen if condition is FALSE
  #  }
  ###############################################################################################################
  #--------------------------------------------  Tutorial: End  ------------------------------------------------#
  ###############################################################################################################
  
  # a variable for new weight after applying efficacy input during treatment influenced period
  sp$pop[, new_sbp := {
    anchor <- unique(anchor_year)                             # This line finds a single anchor year for each person.
    if (is.na(anchor)) {                                      # If no anchor is found,
      sbp_curr_xps                                            # the function simply returns the original sbp_curr_xps as-is
    } else {                                                  # If yes anchor is found,
      fcase(                                                  # We need to assign value to the 'new_sbp' based on the time periods
        year < anchor,                       sbp_curr_xps,    # Before anchor year/uptake: original values (including baseline year)
        year >= anchor & year <= anchor + 2, baseline_sbp + sbp_delta,  # During the 2 years where treatment effect manifest
        year >= anchor + 3,                  sbp_shift        # After sbp rebound to baseline: revert to the original sbp_curr_xps
      )
    }
  }, by = pid]
  
  # Overwriting the original sbp exposure column with the newly created sbp column
  sp$pop[, sbp_curr_xps := new_sbp]
  
  ########################################################################################
  ###############################          Tchol         #################################
  ########################################################################################
  # a variable for the change in SBP after the uptake of semaglutide
  sp$pop[, `:=`(
    tchol_delta = fcase(
      trtm_theo == 0, 0,        ### Baseline year, the year patients become eligible
      trtm_theo == 1, tchol_1y,    ### 1st year of treatment: ITT effects from STEP 1: mean decrease of 3% from baseline
      trtm_theo == 2, tchol_2y,   ### 2nd year of treatment: ITT effects from STEP 5: mean decrease of 3.3% from baseline
      trtm_theo >= 3, 0         ### STEP 1 extension: assume mean tchol will revert to baseline level after one year without treatment
      ### The efficacy inputs are assigned until here, the rest we will revert to the original exposure column (tchol_curr_xps)
    ))
  ]
  
  # a variable for each individual's baseline Tchol (baseline: year == 24 / ys_rollout == 0)
  # *but, some individuals in the dataset does not have entry at baseline_yr, they came in later than year 24
  sp$pop[, baseline_tchol := {                 # sp$pop[, ... , by = pid]: grouping the operation by pid
    anchor <- unique(anchor_year)           # So we can distinguish people who have entry at year 24 and who do not
    if (is.na(anchor)) {                    # If no anchor is found,
      tchol_curr_xps                        # the function simply returns the original tchol_curr_xps as-is
    } else {                                # If yes anchor is found, baseline_tchol will take the tchol at year 24
      tchol_curr_xps[year == (year[trtm_theo == 1][1] - 1)]  # The baseline year will be (anchor - 1): one year before uptake
    }
  }, by = pid] #Works!
  
  ###############################################################################################################
  #----------------------------  Tutorial: if_else function inside data.table  ---------------------------------#
  ###############################################################################################################
  sp$pop[, tchol_shift := {                 # sp$pop[, ... , by = pid]: grouping the operation by pid
    # { ... } operetion per group: inside {}, you can run any custom R code, it will
    # return a vector that gets assigned to 'tchol_shift' for the rows in that group
    
    anchor <- unique(anchor_year)        # This line finds a single anchor year (e.g., year 30) for each person.
    if (is.na(anchor)) {                 # If no anchor is found, (This is useful to avoid breaking your logic when a person doesn't meet your anchor condition)
      tchol_curr_xps                       # the function simply returns the original tchol_curr_xps as-is
    } else {                             # If yes anchor is found, You shift the tchol_curr_xps backward in time (type='lag')
      shift(tchol_curr_xps, type = "lag", n = 3) # by 3 years: 3 years is the duration of efficacy inputs (decrease in tchol and reboud)
    }
  }, by = pid] #Works!
  
  # test <- sp$pop[pid %in% c(156, 175)]
  ###############################################################################################################
  #--------------------------------------------  Tutorial: End  ------------------------------------------------#
  ###############################################################################################################
  
  # a variable for new weight after applying efficacy input during treatment influenced period
  sp$pop[, new_tchol := {
    anchor <- unique(anchor_year)                             # This line finds a single anchor year for each person.
    if (is.na(anchor)) {                                      # If no anchor is found,
      tchol_curr_xps                                          # the function simply returns the original tchol_curr_xps as-is
    } else {                                                  # If yes anchor is found,
      fcase(                                                  # We need to assign value to the 'new_tchol' based on the time periods
        year < anchor,                       tchol_curr_xps,  # Before anchor year/uptake: original values (including baseline year)
        year >= anchor & year <= anchor + 2, baseline_tchol*(1 + tchol_delta),  # During the 2 years where treatment effect manifest
        year >= anchor + 3,                  tchol_shift      # After tchol rebound to baseline: revert to the original tchol_curr_xps
      )
    }
  }, by = pid]
  
  # Overwriting the original tchol exposure column with the newly created tchol column
  sp$pop[, tchol_curr_xps := new_tchol]
  
  ##################### Get rid of unnecessary variables ##########################
  # Delete unnecessary variables from synthpop #
  sp$pop[, c("rankstat_sbp", "rankstat_tchol", "ys_rollout", "uptake_rate",
             "uptake_psyr", "uptake_one", 
             "entry_year", "baseline_bmi", "bmi_shift", "new_bmi",
             "baseline_sbp", "sbp_shift", "new_sbp", "baseline_tchol",
             "tchol_shift", "new_tchol") := NULL]
  
}

##################################################################################################################
#----------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------#
#--------------------------------------------    Scenario 12    -------------------------------------------------#
#-------------------------   Tirzepatide for 10% uptake every year, lifetime treatment   ------------------------#
#----------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------#
##################################################################################################################

scenario_12_fn <- function(sp) {
  
  ###########################################################################################################
  #------------------------------------- Loading uncertainty files -----------------------------------------#
  #------ For lifetime treatment, we only need the effects at year 1&2, because weight do not rebound ------#
  #---------------------------------------------------------------------------------------------------------#
  # Efficacy for bmi at 1st year after treatment
  tbl <- read_fst("./inputs/other_parameters/bmi_1y_samples_tirzp.fst", as.data.table = TRUE)
  bmi_1y <- as.numeric(tbl[mc == sp$mc_aggr, "bmi_1y"])
  
  # Efficacy for bmi at 2nd year after treatment
  tbl <- read_fst("./inputs/other_parameters/bmi_1y_samples_tirzp.fst", as.data.table = TRUE)
  bmi_2y <- as.numeric(tbl[mc == sp$mc_aggr, "bmi_1y"])
  
  # Efficacy for SBP at 1st year after treatment
  tbl <- read_fst("./inputs/other_parameters/sbp_1y_samples_tirzp.fst", as.data.table = TRUE)
  sbp_1y <- as.numeric(tbl[mc == sp$mc_aggr, "sbp_1y"])
  
  # Efficacy for SBP at 2nd year of treatment
  tbl <- read_fst("./inputs/other_parameters/sbp_1y_samples_tirzp.fst", as.data.table = TRUE)
  sbp_2y <- as.numeric(tbl[mc == sp$mc_aggr, "sbp_1y"])
  
  # Efficacy for total cholesterol at 1st year of treatment
  tbl <- read_fst("./inputs/other_parameters/tchol_1y_samples_tirzp.fst", as.data.table = TRUE)
  tchol_1y <- as.numeric(tbl[mc == sp$mc_aggr, "tchol_1y"])
  
  # Efficacy for total cholesterol at 2nd year of treatment
  tbl <- read_fst("./inputs/other_parameters/tchol_1y_samples_tirzp.fst", as.data.table = TRUE)
  tchol_2y <- as.numeric(tbl[mc == sp$mc_aggr, "tchol_1y"])
  ###########################################################################################################
  
  # Load the pids of uptake patients for each iteration
  persons_bia_pct <- fread(paste0("./inputs/uptake/", sp$mc_aggr, "_uptake_bia_pct.csv"))
  ### fread() is highly optimized: it’s faster than read_csv(), and it returns a data.table automatically.
  
  # Join uptake info back to lifecourse rows
  sp$pop <- merge(sp$pop, persons_bia_pct, by = "pid", all.x = TRUE)
  
  # Create a row-level indicator for uptake event (1 only in the uptake year row)
  sp$pop[, uptake_one := as.integer(!is.na(uptake_year) & year == uptake_year)]
  # --- !is.na(uptake_year): checks whether the uptake_year column is no NA, returns 'TRUE' or 'FALSE'
  # --- year == uptake_year: checks whether the value in 'year' column is equal to the value in 'uptake_year' column.
  #                          also returns 'TRUE' or 'FALSE'
  # --- as.integer(...):     converts the result of the logical expression into an integer.
  #                          In R, as.integer(TRUE) becomes 1 and as.integer(False) becomes 0.
  
  # a variable for the year someone uptaking the drug
  sp$pop[, anchor_year:=year[uptake_one == 1], by = pid]
  
  # a variable for the year someone enters the synthetic population
  # sp$pop[, entry_year:= min(year), by = pid]
  
  # a variable for theorectical treatment trajectory
  ### Step 0: Initialize the column
  sp$pop[, trtm_theo := NA_integer_]
  ### Step 1: Identify first year of treatment uptake
  sp$pop[uptake_one == 1, trtm_theo := 0]
  ### Step 2: Fill the sequence in subsequent years
  sp$pop[, trtm_theo := fifelse(cumsum(!is.na(trtm_theo)) > 0,   
                                # cumsum(): computes a cumulative count of non-NA entries:
                                # fill in a sequence starting from the first non-NA value
                                seq_len(.N) - min(which(!is.na(trtm_theo))),
                                NA_integer_),
         by = pid]
  ### Step 3: Replace NAs with 0
  sp$pop[, trtm_theo := fifelse(is.na(trtm_theo), 0, trtm_theo)]
  
  ########################################################################################
  ################################          BMI         ##################################
  ########################################################################################
  # a variable for the change in BMI after the uptake of semaglutide
  sp$pop[, `:=`(
    bmi_delta = fcase(
      ### All values until year 5, are % weight loss relative to baseline
      trtm_theo == 0, 0,       ### Baseline year, the year patients become eligible
      trtm_theo == 1, bmi_1y,  ### replace '-0.149' with 'bmi_1y', and link to the uncertainty file: mc + bmi_1y
      ### 1st year of treatment: ITT effects from STEP 1 (1 year f/u): 14.9% weight loss
      trtm_theo >= 2, bmi_2y  ### replace '-0.152' with 'bmi_2y', and link to the uncertainty file: mc + bmi_2y
      ### 2nd year of treatment: ITT effects from STEP 5 (2 year f/u): 15.2% weight loss
    ))
  ]
  
  # a variable for each individual's baseline weight (baseline: year == 24 / ys_rollout == 0)
  # *but, some individuals in the dataset does not have entry at baseline_yr, they came in later than year 24
  # *and, some individuals came in later than year 24, and because they have a chance to uptake drug begining year 25
  # *they might already uptake the drug at the first year, which means they won't have a year (anchor-1),
  # *so what shall we use as a baseline BMI value here...
  sp$pop[, baseline_bmi := {              # sp$pop[, ... , by = pid]: grouping the operation by pid
    anchor <- unique(anchor_year)         # So we can distinguish people who have entry at year 24 and who do not
    if (is.na(anchor)) {                  # If no anchor is found,
      bmi_curr_xps                        # the function simply returns the original bmi_curr_xps as-is
    } else {                              # If yes anchor is found
      bmi_curr_xps[year == (year[trtm_theo == 1][1] - 1)] # baseline_bmi will take the BMI at the year before trtm_theo==1
    }
  }, by = pid] #Works!
  
  
  # a variable for new weight after applying efficacy input during treatment influenced period
  sp$pop[, new_bmi := {
    anchor <- unique(anchor_year)                             # This line finds a single anchor year for each person.
    if (is.na(anchor)) {                                      # If no anchor is found,
      bmi_curr_xps                                            # the function simply returns the original bmi_curr_xps as-is
    } else {                                                  # If yes anchor is found,
      fcase(                                                  # We need to assign value to the' new_bmi' based on the time periods
        year < anchor,      bmi_curr_xps,                   # Before anchor year/uptake: original values (including baseline year)
        year >= anchor,     baseline_bmi*(1 + bmi_delta)    # After anchor year, the lifetime treatment lead to lifetime effects
      )
    }
  }, by = pid]
  
  # Overwriting the original bmi exposure column with the newly created bmi column
  sp$pop[, bmi_curr_xps := new_bmi]
  
  ########################################################################################
  ################################          SBP         ##################################
  ########################################################################################
  # a variable for the change in SBP after the uptake of semaglutide
  sp$pop[, `:=`(
    sbp_delta = fcase(
      ### All values until year 5, are % weight loss relative to baseline
      trtm_theo == 0, 0,       ### Baseline year, the year patients become eligible
      trtm_theo == 1, sbp_1y,    ### 1st year of treatment: ITT effects from STEP 1 (1 year f/u): mean decrease of 6.2 mmHg from baseline
      trtm_theo >= 2, sbp_2y     ### 2nd year of treatment: ITT effects from STEP 5 (2 year f/u): mean decrease of 5.7 mmHg from baseline
    ))
  ]
  
  # a variable for each individual's baseline SBP (baseline: year == 24 / ys_rollout == 0)
  # *but, some individuals in the dataset does not have entry at baseline_yr, they came in later than year 24
  sp$pop[, baseline_sbp := {                # sp$pop[, ... , by = pid]: grouping the operation by pid
    anchor <- unique(anchor_year)           # So we can distinguish people who have entry at year 24 and who do not
    if (is.na(anchor)) {                    # If no anchor is found,
      sbp_curr_xps                          # the function simply returns the original sbp_curr_xps as-is
    } else {                                # If yes anchor is found, baseline_sbp will take the sbp at year 24
      sbp_curr_xps[year == (year[trtm_theo == 1][1] - 1)]  # The baseline year will be (anchor - 1): one year before uptake
    }
  }, by = pid] #Works!
  
  # a variable for new weight after applying efficacy input during treatment influenced period
  sp$pop[, new_sbp := {
    anchor <- unique(anchor_year)                           # This line finds a single anchor year for each person.
    if (is.na(anchor)) {                                    # If no anchor is found,
      sbp_curr_xps                                          # the function simply returns the original sbp_curr_xps as-is
    } else {                                                # If yes anchor is found,
      fcase(                                                # We need to assign value to the 'new_sbp' based on the time periods
        year < anchor,  sbp_curr_xps,              # Before anchor year/uptake: original values (including baseline year)
        year >= anchor, baseline_sbp + sbp_delta  # After anchor year, the lifetime treatment lead to lifetime effects
      )
    }
  }, by = pid]
  
  # Overwriting the original sbp exposure column with the newly created sbp column
  sp$pop[, sbp_curr_xps := new_sbp]
  
  ########################################################################################
  ###############################          Tchol         #################################
  ########################################################################################
  # a variable for the change in SBP after the uptake of semaglutide
  sp$pop[, `:=`(
    tchol_delta = fcase(
      trtm_theo == 0, 0,          ### Baseline year, the year patients become eligible
      trtm_theo == 1, tchol_1y,   ### 1st year of treatment: ITT effects from STEP 1: mean decrease of 3% from baseline
      trtm_theo >= 2, tchol_2y    ### 2nd year of treatment: ITT effects from STEP 5: mean decrease of 3.3% from baseline
    ))
  ]
  
  # a variable for each individual's baseline Tchol (baseline: year == 24 / ys_rollout == 0)
  # *but, some individuals in the dataset does not have entry at baseline_yr, they came in later than year 24
  sp$pop[, baseline_tchol := {                 # sp$pop[, ... , by = pid]: grouping the operation by pid
    anchor <- unique(anchor_year)           # So we can distinguish people who have entry at year 24 and who do not
    if (is.na(anchor)) {                    # If no anchor is found,
      tchol_curr_xps                        # the function simply returns the original tchol_curr_xps as-is
    } else {                                # If yes anchor is found, baseline_tchol will take the tchol at year 24
      tchol_curr_xps[year == (year[trtm_theo == 1][1] - 1)]  # The baseline year will be (anchor - 1): one year before uptake
    }
  }, by = pid] #Works!
  
  # a variable for new weight after applying efficacy input during treatment influenced period
  sp$pop[, new_tchol := {
    anchor <- unique(anchor_year)                             # This line finds a single anchor year for each person.
    if (is.na(anchor)) {                                      # If no anchor is found,
      tchol_curr_xps                                          # the function simply returns the original tchol_curr_xps as-is
    } else {                                                  # If yes anchor is found,
      fcase(                                                  # We need to assign value to the 'new_tchol' based on the time periods
        year < anchor,    tchol_curr_xps,                  # Before anchor year/uptake: original values (including baseline year)
        year >= anchor,   baseline_tchol*(1 + tchol_delta) # After anchor year, the lifetime treatment lead to lifetime effects
      )
    }
  }, by = pid]
  
  # Overwriting the original tchol exposure column with the newly created tchol column
  sp$pop[, tchol_curr_xps := new_tchol]
  ### Jane Aug 2025, test scenario on sp$pop --> Looks like until here everything worked!!!
  
  ##################### Get rid of unnecessary variables ##########################
  # Delete unnecessary variables from synthpop #
  sp$pop[, c("rankstat_sbp", "rankstat_tchol", "ys_rollout", "uptake_rate",
             "uptake_psyr", "uptake_one", 
             "entry_year", "baseline_bmi", "bmi_shift", "new_bmi",
             "baseline_sbp", "sbp_shift", "new_sbp", "baseline_tchol",
             "tchol_shift", "new_tchol") := NULL]
  
}

##################################################################################################################
#----------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------#
#--------------------------------------------    Scenario 13    -------------------------------------------------#
#----------------   Tirzepatide for 100,000 patients uptake every year, 2 years treatment   ---------------------#
#----------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------#
##################################################################################################################

scenario_13_fn <- function(sp) {
  
  ###########################################################################################################
  #------------------------------------- Loading uncertainty files -----------------------------------------#
  # Efficacy for bmi at 1st year after treatment
  tbl <- read_fst("./inputs/other_parameters/bmi_1y_samples_tirzp.fst", as.data.table = TRUE)
  bmi_1y <- as.numeric(tbl[mc == sp$mc_aggr, "bmi_1y"])
  
  # Efficacy for bmi at 2nd year after treatment
  tbl <- read_fst("./inputs/other_parameters/bmi_1y_samples_tirzp.fst", as.data.table = TRUE)
  bmi_2y <- as.numeric(tbl[mc == sp$mc_aggr, "bmi_1y"])
  
  # Efficacy for bmi at 3rd year after treatment
  tbl <- read_fst("./inputs/other_parameters/bmi_3y_samples_tirzp.fst", as.data.table = TRUE)
  bmi_3y <- as.numeric(tbl[mc == sp$mc_aggr, "bmi_3y"])
  
  # Efficacy for bmi at 4th year after treatment
  tbl <- read_fst("./inputs/other_parameters/bmi_4y_samples_tirzp.fst", as.data.table = TRUE)
  bmi_4y <- as.numeric(tbl[mc == sp$mc_aggr, "bmi_4y"])
  
  # Efficacy for SBP at 1st year after treatment
  tbl <- read_fst("./inputs/other_parameters/sbp_1y_samples_tirzp.fst", as.data.table = TRUE)
  sbp_1y <- as.numeric(tbl[mc == sp$mc_aggr, "sbp_1y"])
  
  # Efficacy for SBP at 2nd year of treatment
  tbl <- read_fst("./inputs/other_parameters/sbp_1y_samples_tirzp.fst", as.data.table = TRUE)
  sbp_2y <- as.numeric(tbl[mc == sp$mc_aggr, "sbp_1y"])
  
  # Efficacy for total cholesterol at 1st year of treatment
  tbl <- read_fst("./inputs/other_parameters/tchol_1y_samples_tirzp.fst", as.data.table = TRUE)
  tchol_1y <- as.numeric(tbl[mc == sp$mc_aggr, "tchol_1y"])
  
  # Efficacy for total cholesterol at 2nd year of treatment
  tbl <- read_fst("./inputs/other_parameters/tchol_1y_samples_tirzp.fst", as.data.table = TRUE)
  tchol_2y <- as.numeric(tbl[mc == sp$mc_aggr, "tchol_1y"])
  ###########################################################################################################
  
  # Load the pids of uptake patients for each iteration
  persons_bia_N <- fread(paste0("./inputs/uptake/", sp$mc_aggr, "_uptake_bia_N.csv"))
  ### fread() is highly optimized: it’s faster than read_csv(), and it returns a data.table automatically.
  
  # Join uptake info back to lifecourse rows
  sp$pop <- merge(sp$pop, persons_bia_N, by = "pid", all.x = TRUE)
  
  # Create a row-level indicator for uptake event (1 only in the uptake year row)
  sp$pop[, uptake_one := as.integer(!is.na(uptake_year) & year == uptake_year)]
  # --- !is.na(uptake_year): checks whether the uptake_year column is no NA, returns 'TRUE' or 'FALSE'
  # --- year == uptake_year: checks whether the value in 'year' column is equal to the value in 'uptake_year' column.
  #                          also returns 'TRUE' or 'FALSE'
  # --- as.integer(...):     converts the result of the logical expression into an integer.
  #                          In R, as.integer(TRUE) becomes 1 and as.integer(False) becomes 0.
  
  # a variable for the year someone uptaking the drug
  sp$pop[, anchor_year:=year[uptake_one == 1], by = pid]
  
  # a variable for the year someone enters the synthetic population
  # sp$pop[, entry_year:= min(year), by = pid]
  
  # a variable for theorectical treatment trajectory
  ### Step 0: Initialize the column
  sp$pop[, trtm_theo := NA_integer_]
  ### Step 1: Identify first year of treatment uptake
  sp$pop[uptake_one == 1, trtm_theo := 0]
  ### Step 2: Fill the sequence in subsequent years
  sp$pop[, trtm_theo := fifelse(cumsum(!is.na(trtm_theo)) > 0,   # cumsum(): computes a cumulative count of non-NA entries:
                                # fill in a sequence starting from the first non-NA value
                                seq_len(.N) - min(which(!is.na(trtm_theo))),
                                NA_integer_),
         by = pid]
  ### Step 3: Replace NAs with 0
  sp$pop[, trtm_theo := fifelse(is.na(trtm_theo), 0, trtm_theo)]
  
  ########################################################################################
  ################################          BMI         ##################################
  ########################################################################################
  # a variable for the change in BMI after the uptake of Tirzepatide
  sp$pop[, `:=`(
    bmi_delta = fcase(
      ### All values until year 5, are % weight loss relative to baseline
      trtm_theo == 0, 0,       ### Baseline year, the year patients become eligible
      trtm_theo == 1, bmi_1y,  ### replace '-0.209' with 'bmi_1y', and link to the uncertainty file: mc + bmi_1y
      ### 1st year of treatment: ITT effects from SURMOUNT-1 (1 year f/u): 20.9% weight loss
      trtm_theo == 2, bmi_2y,  ### replace '-0.209' with 'bmi_2y', and link to the uncertainty file: mc + bmi_2y
      ### 2nd year of treatment: ITT effects from SURMOUNT-1 (2 year f/u): 20.9% weight loss
      trtm_theo == 3, bmi_3y,  ### SURMOUNT-4: regain ~2/3 of weight loss in 1st year treatment cessation
      ###
      trtm_theo == 4, bmi_4y,  ### weight regain at 2nd year treatment cessation
      trtm_theo >= 5, 0        ### weight return to baseline weight by 3rd year treatment cessation
      ### The efficacy inputs are assigned until here, the rest we will revert to the original exposure column (bmi_curr_xps)
    ))
  ]
  
  # a variable for each individual's baseline weight (baseline: year == 24 / ys_rollout == 0)
  # *but, some individuals in the dataset does not have entry at baseline_yr, they came in later than year 24
  # *and, some individuals came in later than year 24, and because they have a chance to uptake drug begining year 25
  # *they might already uptake the drug at the first year, which means they won't have a year (anchor-1),
  # *so what shall we use as a baseline BMI value here...
  sp$pop[, baseline_bmi := {              # sp$pop[, ... , by = pid]: grouping the operation by pid
    anchor <- unique(anchor_year)         # So we can distinguish people who have entry at year 24 and who do not
    if (is.na(anchor)) {                  # If no anchor is found,
      bmi_curr_xps                        # the function simply returns the original bmi_curr_xps as-is
    } else {                              # If yes anchor is found
      bmi_curr_xps[year == (year[trtm_theo == 1][1] - 1)] # baseline_bmi will take the BMI at the year before trtm_theo==1
    }
  }, by = pid] #Works!
  
  ###############################################################################################################
  #----------------------------  Tutorial: if_else function inside data.table  ---------------------------------#
  ###############################################################################################################
  sp$pop[, bmi_shift := {             # sp$pop[, ... , by = pid]: grouping the operation by pid
    # { ... } operetion per group: inside {}, you can run any custom R code, it will
    # return a vector that gets assigned to 'bmi_shift' for the rows in that group
    anchor <- unique(anchor_year)     # This line finds a single anchor year (e.g., year 30) for each person.
    if (is.na(anchor)) {              # If no anchor is found (to avoid breaking your logic when a person doesn't uptake the drug)
      bmi_curr_xps                    # the function simply returns the original bmi_curr_xps as-is
    } else {                          # If yes anchor is found, You shift the bmi_curr_xps backward in time (type='lag')
      shift(bmi_curr_xps, type = "lag", n = 5) # by 5 years: 5 years is the duration of efficacy inputs (weight loss and reboud)
    }
  }, by = pid] #Works!
  ###############################################################################################################
  #--------------------------------------------  Tutorial: End  ------------------------------------------------#
  ###############################################################################################################
  
  # a variable for new weight after applying efficacy input during treatment influenced period
  sp$pop[, new_bmi := {
    anchor <- unique(anchor_year)                             # This line finds a single anchor year for each person.
    if (is.na(anchor)) {                                      # If no anchor is found,
      bmi_curr_xps                                            # the function simply returns the original bmi_curr_xps as-is
    } else {                                                  # If yes anchor is found,
      fcase(                                                  # We need to assign value to the' new_bmi' based on the time periods
        year < anchor,                       bmi_curr_xps,    # Before anchor year/uptake: original values (including baseline year)
        year >= anchor & year <= anchor + 4, baseline_bmi*(1 + bmi_delta),  # During the 5 years where treatment effects manifest
        year > anchor + 4,                   bmi_shift        # After rebound to baseline: revert to the original bmi_curr_xps
      )
    }
  }, by = pid]
  
  # Overwriting the original bmi exposure column with the newly created bmi column
  sp$pop[, bmi_curr_xps := new_bmi]
  
  ########################################################################################
  ################################          SBP         ##################################
  ########################################################################################
  # a variable for the change in SBP after the uptake of semaglutide
  sp$pop[, `:=`(
    sbp_delta = fcase(
      ### All values until year 5, are % weight loss relative to baseline
      trtm_theo == 0, 0,       ### Baseline year, the year patients become eligible
      trtm_theo == 1, sbp_1y,    ### 1st year of treatment: ITT effects from STEP 1 (1 year f/u): mean decrease of 6.2 mmHg from baseline
      trtm_theo == 2, sbp_2y,    ### 2nd year of treatment: ITT effects from STEP 5 (2 year f/u): mean decrease of 5.7 mmHg from baseline
      trtm_theo >= 3, 0        ### STEP 1 extension: assume mean SBP will revert to baseline level after one year without treatment
      ### The efficacy inputs are assigned until here, the rest we will revert to the original exposure column (sbp_curr_xps)
    ))
  ]
  
  # a variable for each individual's baseline SBP (baseline: year == 24 / ys_rollout == 0)
  # *but, some individuals in the dataset does not have entry at baseline_yr, they came in later than year 24
  sp$pop[, baseline_sbp := {                # sp$pop[, ... , by = pid]: grouping the operation by pid
    anchor <- unique(anchor_year)           # So we can distinguish people who have entry at year 24 and who do not
    if (is.na(anchor)) {                    # If no anchor is found,
      sbp_curr_xps                          # the function simply returns the original sbp_curr_xps as-is
    } else {                                # If yes anchor is found, baseline_sbp will take the sbp at year 24
      sbp_curr_xps[year == (year[trtm_theo == 1][1] - 1)]  # The baseline year will be (anchor - 1): one year before uptake
    }
  }, by = pid] #Works!
  
  ###############################################################################################################
  #----------------------------  Tutorial: if_else function inside data.table  ---------------------------------#
  ###############################################################################################################
  sp$pop[, sbp_shift := {                 # sp$pop[, ... , by = pid]: grouping the operation by pid
    # { ... } operetion per group: inside {}, you can run any custom R code, it will
    # return a vector that gets assigned to 'sbp_shift' for the rows in that group
    
    anchor <- unique(anchor_year)        # This line finds a single anchor year (e.g., year 30) for each person.
    if (is.na(anchor)) {                 # If no anchor is found, (This is useful to avoid breaking your logic when a person doesn't meet your anchor condition)
      sbp_curr_xps                       # the function simply returns the original sbp_curr_xps as-is
    } else {                             # If yes anchor is found, You shift the sbp_curr_xps backward in time (type='lag')
      shift(sbp_curr_xps, type = "lag", n = 3) # by 3 years: 3 years is the duration of efficacy inputs (decrease in SBP and rebound)
    }
  }, by = pid] #Works!
  
  #  A if_else function inside of :={} for creating a new variable
  #  if(condition) {
  #   what should happen if condition is TRUE
  #  } else {
  #   what should happen if condition is FALSE
  #  }
  ###############################################################################################################
  #--------------------------------------------  Tutorial: End  ------------------------------------------------#
  ###############################################################################################################
  
  # a variable for new weight after applying efficacy input during treatment influenced period
  sp$pop[, new_sbp := {
    anchor <- unique(anchor_year)                             # This line finds a single anchor year for each person.
    if (is.na(anchor)) {                                      # If no anchor is found,
      sbp_curr_xps                                            # the function simply returns the original sbp_curr_xps as-is
    } else {                                                  # If yes anchor is found,
      fcase(                                                  # We need to assign value to the 'new_sbp' based on the time periods
        year < anchor,                       sbp_curr_xps,    # Before anchor year/uptake: original values (including baseline year)
        year >= anchor & year <= anchor + 2, baseline_sbp + sbp_delta,  # During the 2 years where treatment effect manifest
        year >= anchor + 3,                  sbp_shift        # After sbp rebound to baseline: revert to the original sbp_curr_xps
      )
    }
  }, by = pid]
  
  # Overwriting the original sbp exposure column with the newly created sbp column
  sp$pop[, sbp_curr_xps := new_sbp]
  
  ########################################################################################
  ###############################          Tchol         #################################
  ########################################################################################
  # a variable for the change in SBP after the uptake of semaglutide
  sp$pop[, `:=`(
    tchol_delta = fcase(
      trtm_theo == 0, 0,        ### Baseline year, the year patients become eligible
      trtm_theo == 1, tchol_1y,    ### 1st year of treatment: ITT effects from STEP 1: mean decrease of 3% from baseline
      trtm_theo == 2, tchol_2y,   ### 2nd year of treatment: ITT effects from STEP 5: mean decrease of 3.3% from baseline
      trtm_theo >= 3, 0         ### STEP 1 extension: assume mean tchol will revert to baseline level after one year without treatment
      ### The efficacy inputs are assigned until here, the rest we will revert to the original exposure column (tchol_curr_xps)
    ))
  ]
  
  # a variable for each individual's baseline Tchol (baseline: year == 24 / ys_rollout == 0)
  # *but, some individuals in the dataset does not have entry at baseline_yr, they came in later than year 24
  sp$pop[, baseline_tchol := {                 # sp$pop[, ... , by = pid]: grouping the operation by pid
    anchor <- unique(anchor_year)           # So we can distinguish people who have entry at year 24 and who do not
    if (is.na(anchor)) {                    # If no anchor is found,
      tchol_curr_xps                        # the function simply returns the original tchol_curr_xps as-is
    } else {                                # If yes anchor is found, baseline_tchol will take the tchol at year 24
      tchol_curr_xps[year == (year[trtm_theo == 1][1] - 1)]  # The baseline year will be (anchor - 1): one year before uptake
    }
  }, by = pid] #Works!
  
  ###############################################################################################################
  #----------------------------  Tutorial: if_else function inside data.table  ---------------------------------#
  ###############################################################################################################
  sp$pop[, tchol_shift := {                 # sp$pop[, ... , by = pid]: grouping the operation by pid
    # { ... } operetion per group: inside {}, you can run any custom R code, it will
    # return a vector that gets assigned to 'tchol_shift' for the rows in that group
    
    anchor <- unique(anchor_year)        # This line finds a single anchor year (e.g., year 30) for each person.
    if (is.na(anchor)) {                 # If no anchor is found, (This is useful to avoid breaking your logic when a person doesn't meet your anchor condition)
      tchol_curr_xps                       # the function simply returns the original tchol_curr_xps as-is
    } else {                             # If yes anchor is found, You shift the tchol_curr_xps backward in time (type='lag')
      shift(tchol_curr_xps, type = "lag", n = 3) # by 3 years: 3 years is the duration of efficacy inputs (decrease in tchol and reboud)
    }
  }, by = pid] #Works!
  
  # test <- sp$pop[pid %in% c(156, 175)]
  ###############################################################################################################
  #--------------------------------------------  Tutorial: End  ------------------------------------------------#
  ###############################################################################################################
  
  # a variable for new weight after applying efficacy input during treatment influenced period
  sp$pop[, new_tchol := {
    anchor <- unique(anchor_year)                             # This line finds a single anchor year for each person.
    if (is.na(anchor)) {                                      # If no anchor is found,
      tchol_curr_xps                                          # the function simply returns the original tchol_curr_xps as-is
    } else {                                                  # If yes anchor is found,
      fcase(                                                  # We need to assign value to the 'new_tchol' based on the time periods
        year < anchor,                       tchol_curr_xps,  # Before anchor year/uptake: original values (including baseline year)
        year >= anchor & year <= anchor + 2, baseline_tchol*(1 + tchol_delta),  # During the 2 years where treatment effect manifest
        year >= anchor + 3,                  tchol_shift      # After tchol rebound to baseline: revert to the original tchol_curr_xps
      )
    }
  }, by = pid]
  
  # Overwriting the original tchol exposure column with the newly created tchol column
  sp$pop[, tchol_curr_xps := new_tchol]
  
  ##################### Get rid of unnecessary variables ##########################
  # Delete unnecessary variables from synthpop #
  sp$pop[, c("rankstat_sbp", "rankstat_tchol", "ys_rollout", "uptake_rate",
             "uptake_psyr", "uptake_one", 
             "entry_year", "baseline_bmi", "bmi_shift", "new_bmi",
             "baseline_sbp", "sbp_shift", "new_sbp", "baseline_tchol",
             "tchol_shift", "new_tchol") := NULL]
  
}

##################################################################################################################
#----------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------#
#--------------------------------------------    Scenario 14    -------------------------------------------------#
#-----------------   Tirzepatide for 100,000 patients uptake every year, lifetime treatment   -------------------#
#----------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------#
##################################################################################################################

scenario_14_fn <- function(sp) {
  
  ###########################################################################################################
  #------------------------------------- Loading uncertainty files -----------------------------------------#
  #------ For lifetime treatment, we only need the effects at year 1&2, because weight do not rebound ------#
  #---------------------------------------------------------------------------------------------------------#
  # Efficacy for bmi at 1st year after treatment
  tbl <- read_fst("./inputs/other_parameters/bmi_1y_samples_tirzp.fst", as.data.table = TRUE)
  bmi_1y <- as.numeric(tbl[mc == sp$mc_aggr, "bmi_1y"])
  
  # Efficacy for bmi at 2nd year after treatment
  tbl <- read_fst("./inputs/other_parameters/bmi_1y_samples_tirzp.fst", as.data.table = TRUE)
  bmi_2y <- as.numeric(tbl[mc == sp$mc_aggr, "bmi_1y"])
  
  # Efficacy for SBP at 1st year after treatment
  tbl <- read_fst("./inputs/other_parameters/sbp_1y_samples_tirzp.fst", as.data.table = TRUE)
  sbp_1y <- as.numeric(tbl[mc == sp$mc_aggr, "sbp_1y"])
  
  # Efficacy for SBP at 2nd year of treatment
  tbl <- read_fst("./inputs/other_parameters/sbp_1y_samples_tirzp.fst", as.data.table = TRUE)
  sbp_2y <- as.numeric(tbl[mc == sp$mc_aggr, "sbp_1y"])
  
  # Efficacy for total cholesterol at 1st year of treatment
  tbl <- read_fst("./inputs/other_parameters/tchol_1y_samples_tirzp.fst", as.data.table = TRUE)
  tchol_1y <- as.numeric(tbl[mc == sp$mc_aggr, "tchol_1y"])
  
  # Efficacy for total cholesterol at 2nd year of treatment
  tbl <- read_fst("./inputs/other_parameters/tchol_1y_samples_tirzp.fst", as.data.table = TRUE)
  tchol_2y <- as.numeric(tbl[mc == sp$mc_aggr, "tchol_1y"])
  ###########################################################################################################
  
  # Load the pids of uptake patients for each iteration
  persons_bia_N <- fread(paste0("./inputs/uptake/", sp$mc_aggr, "_uptake_bia_N.csv"))
  ### fread() is highly optimized: it’s faster than read_csv(), and it returns a data.table automatically.
  
  # Join uptake info back to lifecourse rows
  sp$pop <- merge(sp$pop, persons_bia_N, by = "pid", all.x = TRUE)
  
  # Create a row-level indicator for uptake event (1 only in the uptake year row)
  sp$pop[, uptake_one := as.integer(!is.na(uptake_year) & year == uptake_year)]
  # --- !is.na(uptake_year): checks whether the uptake_year column is no NA, returns 'TRUE' or 'FALSE'
  # --- year == uptake_year: checks whether the value in 'year' column is equal to the value in 'uptake_year' column.
  #                          also returns 'TRUE' or 'FALSE'
  # --- as.integer(...):     converts the result of the logical expression into an integer.
  #                          In R, as.integer(TRUE) becomes 1 and as.integer(False) becomes 0.
  
  # a variable for the year someone uptaking the drug
  sp$pop[, anchor_year:=year[uptake_one == 1], by = pid]
  
  # a variable for the year someone enters the synthetic population
  # sp$pop[, entry_year:= min(year), by = pid]
  
  # a variable for theorectical treatment trajectory
  ### Step 0: Initialize the column
  sp$pop[, trtm_theo := NA_integer_]
  ### Step 1: Identify first year of treatment uptake
  sp$pop[uptake_one == 1, trtm_theo := 0]
  ### Step 2: Fill the sequence in subsequent years
  sp$pop[, trtm_theo := fifelse(cumsum(!is.na(trtm_theo)) > 0,   
                                # cumsum(): computes a cumulative count of non-NA entries:
                                # fill in a sequence starting from the first non-NA value
                                seq_len(.N) - min(which(!is.na(trtm_theo))),
                                NA_integer_),
         by = pid]
  ### Step 3: Replace NAs with 0
  sp$pop[, trtm_theo := fifelse(is.na(trtm_theo), 0, trtm_theo)]
  
  ########################################################################################
  ################################          BMI         ##################################
  ########################################################################################
  # a variable for the change in BMI after the uptake of semaglutide
  sp$pop[, `:=`(
    bmi_delta = fcase(
      ### All values until year 5, are % weight loss relative to baseline
      trtm_theo == 0, 0,       ### Baseline year, the year patients become eligible
      trtm_theo == 1, bmi_1y,  ### replace '-0.149' with 'bmi_1y', and link to the uncertainty file: mc + bmi_1y
      ### 1st year of treatment: ITT effects from STEP 1 (1 year f/u): 14.9% weight loss
      trtm_theo >= 2, bmi_2y  ### replace '-0.152' with 'bmi_2y', and link to the uncertainty file: mc + bmi_2y
      ### 2nd year of treatment: ITT effects from STEP 5 (2 year f/u): 15.2% weight loss
    ))
  ]
  
  # a variable for each individual's baseline weight (baseline: year == 24 / ys_rollout == 0)
  # *but, some individuals in the dataset does not have entry at baseline_yr, they came in later than year 24
  # *and, some individuals came in later than year 24, and because they have a chance to uptake drug begining year 25
  # *they might already uptake the drug at the first year, which means they won't have a year (anchor-1),
  # *so what shall we use as a baseline BMI value here...
  sp$pop[, baseline_bmi := {              # sp$pop[, ... , by = pid]: grouping the operation by pid
    anchor <- unique(anchor_year)         # So we can distinguish people who have entry at year 24 and who do not
    if (is.na(anchor)) {                  # If no anchor is found,
      bmi_curr_xps                        # the function simply returns the original bmi_curr_xps as-is
    } else {                              # If yes anchor is found
      bmi_curr_xps[year == (year[trtm_theo == 1][1] - 1)] # baseline_bmi will take the BMI at the year before trtm_theo==1
    }
  }, by = pid] #Works!
  
  
  # a variable for new weight after applying efficacy input during treatment influenced period
  sp$pop[, new_bmi := {
    anchor <- unique(anchor_year)                             # This line finds a single anchor year for each person.
    if (is.na(anchor)) {                                      # If no anchor is found,
      bmi_curr_xps                                            # the function simply returns the original bmi_curr_xps as-is
    } else {                                                  # If yes anchor is found,
      fcase(                                                  # We need to assign value to the' new_bmi' based on the time periods
        year < anchor,      bmi_curr_xps,                   # Before anchor year/uptake: original values (including baseline year)
        year >= anchor,     baseline_bmi*(1 + bmi_delta)    # After anchor year, the lifetime treatment lead to lifetime effects
      )
    }
  }, by = pid]
  
  # Overwriting the original bmi exposure column with the newly created bmi column
  sp$pop[, bmi_curr_xps := new_bmi]
  
  ########################################################################################
  ################################          SBP         ##################################
  ########################################################################################
  # a variable for the change in SBP after the uptake of semaglutide
  sp$pop[, `:=`(
    sbp_delta = fcase(
      ### All values until year 5, are % weight loss relative to baseline
      trtm_theo == 0, 0,       ### Baseline year, the year patients become eligible
      trtm_theo == 1, sbp_1y,    ### 1st year of treatment: ITT effects from STEP 1 (1 year f/u): mean decrease of 6.2 mmHg from baseline
      trtm_theo >= 2, sbp_2y     ### 2nd year of treatment: ITT effects from STEP 5 (2 year f/u): mean decrease of 5.7 mmHg from baseline
    ))
  ]
  
  # a variable for each individual's baseline SBP (baseline: year == 24 / ys_rollout == 0)
  # *but, some individuals in the dataset does not have entry at baseline_yr, they came in later than year 24
  sp$pop[, baseline_sbp := {                # sp$pop[, ... , by = pid]: grouping the operation by pid
    anchor <- unique(anchor_year)           # So we can distinguish people who have entry at year 24 and who do not
    if (is.na(anchor)) {                    # If no anchor is found,
      sbp_curr_xps                          # the function simply returns the original sbp_curr_xps as-is
    } else {                                # If yes anchor is found, baseline_sbp will take the sbp at year 24
      sbp_curr_xps[year == (year[trtm_theo == 1][1] - 1)]  # The baseline year will be (anchor - 1): one year before uptake
    }
  }, by = pid] #Works!
  
  # a variable for new weight after applying efficacy input during treatment influenced period
  sp$pop[, new_sbp := {
    anchor <- unique(anchor_year)                           # This line finds a single anchor year for each person.
    if (is.na(anchor)) {                                    # If no anchor is found,
      sbp_curr_xps                                          # the function simply returns the original sbp_curr_xps as-is
    } else {                                                # If yes anchor is found,
      fcase(                                                # We need to assign value to the 'new_sbp' based on the time periods
        year < anchor,  sbp_curr_xps,              # Before anchor year/uptake: original values (including baseline year)
        year >= anchor, baseline_sbp + sbp_delta  # After anchor year, the lifetime treatment lead to lifetime effects
      )
    }
  }, by = pid]
  
  # Overwriting the original sbp exposure column with the newly created sbp column
  sp$pop[, sbp_curr_xps := new_sbp]
  
  ########################################################################################
  ###############################          Tchol         #################################
  ########################################################################################
  # a variable for the change in SBP after the uptake of semaglutide
  sp$pop[, `:=`(
    tchol_delta = fcase(
      trtm_theo == 0, 0,          ### Baseline year, the year patients become eligible
      trtm_theo == 1, tchol_1y,   ### 1st year of treatment: ITT effects from STEP 1: mean decrease of 3% from baseline
      trtm_theo >= 2, tchol_2y    ### 2nd year of treatment: ITT effects from STEP 5: mean decrease of 3.3% from baseline
    ))
  ]
  
  # a variable for each individual's baseline Tchol (baseline: year == 24 / ys_rollout == 0)
  # *but, some individuals in the dataset does not have entry at baseline_yr, they came in later than year 24
  sp$pop[, baseline_tchol := {                 # sp$pop[, ... , by = pid]: grouping the operation by pid
    anchor <- unique(anchor_year)           # So we can distinguish people who have entry at year 24 and who do not
    if (is.na(anchor)) {                    # If no anchor is found,
      tchol_curr_xps                        # the function simply returns the original tchol_curr_xps as-is
    } else {                                # If yes anchor is found, baseline_tchol will take the tchol at year 24
      tchol_curr_xps[year == (year[trtm_theo == 1][1] - 1)]  # The baseline year will be (anchor - 1): one year before uptake
    }
  }, by = pid] #Works!
  
  # a variable for new weight after applying efficacy input during treatment influenced period
  sp$pop[, new_tchol := {
    anchor <- unique(anchor_year)                             # This line finds a single anchor year for each person.
    if (is.na(anchor)) {                                      # If no anchor is found,
      tchol_curr_xps                                          # the function simply returns the original tchol_curr_xps as-is
    } else {                                                  # If yes anchor is found,
      fcase(                                                  # We need to assign value to the 'new_tchol' based on the time periods
        year < anchor,    tchol_curr_xps,                  # Before anchor year/uptake: original values (including baseline year)
        year >= anchor,   baseline_tchol*(1 + tchol_delta) # After anchor year, the lifetime treatment lead to lifetime effects
      )
    }
  }, by = pid]
  
  # Overwriting the original tchol exposure column with the newly created tchol column
  sp$pop[, tchol_curr_xps := new_tchol]
  ### Jane Aug 2025, test scenario on sp$pop --> Looks like until here everything worked!!!
  
  ##################### Get rid of unnecessary variables ##########################
  # Delete unnecessary variables from synthpop #
  sp$pop[, c("rankstat_sbp", "rankstat_tchol", "ys_rollout", "uptake_rate",
             "uptake_psyr", "uptake_one", 
             "entry_year", "baseline_bmi", "bmi_shift", "new_bmi",
             "baseline_sbp", "sbp_shift", "new_sbp", "baseline_tchol",
             "tchol_shift", "new_tchol") := NULL]
  
}

