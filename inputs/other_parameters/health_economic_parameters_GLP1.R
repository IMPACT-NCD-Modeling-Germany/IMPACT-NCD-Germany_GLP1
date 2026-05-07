
##### Health economic parameters ----

# Load packages
library(fst)
library(data.table)
library(ggplot2)
library(CKutils)

# Set global vars
# mc_s <- 30 # Number of MC samples -- Necessary?
# Jane April 2026: the treatment costs do not need to have uncertainty and do not need to vary among mc


# Inflation factors:
# infl_2018 <- 107.3/103.4 
# infl_2020 <- 107.3/105.3
# Jane April 2026: I don't think the treatment cost needs to consider inflation


##############################################################################
#------------------- Treatment costs: Semaglutide ---------------------------#
#----------------------------------------------------------------------------#
#------------------------------- Year 0 -------------------------------------#
#----------------------------------------------------------------------------#
# As we assume an intolerance rate after dose escalation, 
# we will apply cost of dose escalation to all eligible patients
itt_cost_dose_escalation_sm <- 752
# 172 + 172 + 172 + 236 (https://fitfursleben.de/wegovy/preis/?utm_source=chatgpt.com)

#------------------------------- Year 1 -------------------------------------#
#----------------------------------------------------------------------------#
# We assume a 8% of intolerance rate after dose escalation
# We assume a 12% discontinuation rate each year during treatment
itt_cost_1st_year_sm <- (1-0.08)*0.88*(277*12) + (1-0.08)*0.12*(0.5*277*12) # This equals to ~2874

#------------------------------- Year 2 -------------------------------------#
#----------------------------------------------------------------------------#
# We assume a 12% discontinuation rate each year during treatment
itt_cost_2nd_year_sm <- (1-0.08)*(1-0.12)*0.88*(277*12) + (1-0.08)*(1-0.12)*0.12*(0.5*277*12) # This equals to ~2529

#--------------------------  Year 3 onwards ---------------------------------#
#----------------------------------------------------------------------------#
# We assume no discontinuation after 2 years in the lifetime scenario
# The effect and the cost will both be the same as the 2nd year
itt_cost_life_sm <- (1-0.08)*(1-0.12)*0.88*(277*12) + (1-0.08)*(1-0.12)*0.12*(0.5*277*12) # This equals to ~2529

##############################################################################
#------------------- Treatment costs: Tirzepatide ---------------------------#
#----------------------------------------------------------------------------#
#------------------------------- Year 0 -------------------------------------#
#----------------------------------------------------------------------------#
# As we assume an intolerance rate after dose escalation, 
# we will apply cost of dose escalation to all eligible patients
itt_cost_dose_escalation_tz <- 1738
# 206 + 277 + 383 + 383 + 489 (https://www.zavamed.com/de/mounjaro.html)

#------------------------------- Year 1 -------------------------------------#
#----------------------------------------------------------------------------#
# We assume a 8% of intolerance rate after dose escalation
# We assume a 15% discontinuation rate each year during treatment
itt_cost_1st_year_tz <- (1-0.08)*0.85*5868 + (1-0.08)*0.15*(0.5*5868) # This equals to ~4993

#------------------------------- Year 2 -------------------------------------#
#----------------------------------------------------------------------------#
# We assume a 15% discontinuation rate each year during treatment
itt_cost_2nd_year_tz <- (1-0.08)*(1-0.15)*0.85*5868 + (1-0.08)*(1-0.15)*0.15*(0.5*5868) # This equals to ~4244

#--------------------------  Year 3 onwards ---------------------------------#
#----------------------------------------------------------------------------#
# We assume no discontinuation after 2 years in the lifetime scenario
# The effect and the cost will both be the same as the 2nd year
itt_cost_life_tz <- (1-0.08)*(1-0.15)*0.85*5868 + (1-0.08)*(1-0.15)*0.15*(0.5*5868) # This equals to ~4244

cost_combine <- data.table(NULL)

#############################################################################################
#-------------------- Assign Treatment costs to different scenarios  -----------------------#
#############################################################################################

  cost_data <- CJ(uptake_group  =    c(1,2,3),
                  scenario      =    c("sc1","sc2","sc3","sc4"), 
                  trtm_year     =    c(0:20))
                  
  
  ## CEA/BIA -- Scenario 1: semaglutide for 2 years
  cost_data[scenario == "sc1", 
            GLP_cost := fifelse(trtm_year == 0, 752,
                                fifelse(trtm_year == 1, 2874,
                                        fifelse(trtm_year == 2, 2529,
                                                           0)))]
  
  ## CEA/BIA -- Scenario 2: semaglutide for lifetime
  cost_data[scenario == "sc2", 
            GLP_cost := fifelse(trtm_year == 0, 752,
                                fifelse(trtm_year == 1, 2874,
                                        fifelse(trtm_year >= 2, 2529,
                                                0)))]
  
  ## CEA/BIA -- Scenario 3: tirzepatide for 2 years
  cost_data[scenario == "sc3", 
            GLP_cost := fifelse(trtm_year == 0, 1738,
                                fifelse(trtm_year == 1, 4993,
                                        fifelse(trtm_year == 2, 4244,
                                                0)))]
  
  ## CEA/BIA -- Scenario 4: tirzepatide for lifetime
  cost_data[scenario == "sc4", 
            GLP_cost := fifelse(trtm_year == 0, 1738,
                                fifelse(trtm_year == 1, 4993,
                                        fifelse(trtm_year >= 2, 4244,
                                                0)))]
  
  
#  cost_data[, cost := cost * infl_2020]
#  cost_data[, `:=`(sex = ifelse(sex == 1, "women", "men"),
#                   mc = i)]
#  cost_combine <- rbind(cost_data, cost_combine)
#}  

write_fst(cost_data, "./inputs/other_parameters/GLP_treatment_costs.fst")

#############################################################################################
#-------------------------- Assumption on Price Trajectory   -------------------------------#
#############################################################################################

##### Full list price #####
price_traj_1 <- data.table( year   =    c(13:45),                               # Prize trajectory is only related to calendar year
                            
                            ## Price trajectory for Semaglutide                 
                            price_traj_sm =    1,                               # list price remains constant over time
                            
                            ## Price trajectory for Tirzepatide                 # list price remain constant over time
                            price_traj_tz =    1
)

write_fst(price_traj_1, "./inputs/other_parameters/GLP_price_traj_1.fst")


price_traj_2 <- data.table( year   =    c(13:45),                               # Prize trajectory is only related to calendar year
                            
                            ## Price trajectory for Semaglutide
                            price_traj_sm =    c(1,1,1,1,1,1,                   # From 2013
                                                 1,1,1,1,1,1,                   # to 2024
                                                 1, 1, 1, 0.95, 0.95, 0.9,      # Before patient expire in 2031
                                                 0.7, 0.7, 0.7, 0.7, 0.7,       # Price drop starting 2031
                                                 0.5, 0.5, 0.5, 0.5, 0.5,       # Price drop starting 2036
                                                 0.3, 0.3, 0.3, 0.3, 0.3),      # Price drop starting 2041
                            
                            ## Price trajectory for Tirzepatide
                            price_traj_tz =    c(1,1,1,1,1,1,                   # From 2013
                                                 1,1,1,1,1,1,                   # to 2024
                                                 1,1,1,1,1,1,                   # Price mostly stay stable until 2031
                                                 0.95, 0.95, 0.9, 0.9, 0.9,      # Price modestly decline due to competitor
                                                 0.7, 0.7, 0.7, 0.7, 0.7,       # Price expire in 2036
                                                 0.5, 0.5, 0.5, 0.5, 0.5)       # Price drop starting 2041
)

write_fst(price_traj_2, "./inputs/other_parameters/GLP_price_traj_2.fst")

##### 50% list price #####
price_traj_3 <- data.table( year   =    c(13:45),                               # Prize trajectory is only related to calendar year
                            
                            ## Price trajectory for Semaglutide                 
                            price_traj_sm =    0.5,                             # 0.5*list price remains constant over time
                            
                            ## Price trajectory for Tirzepatide                 # 0.5*list price remain constant over time
                            price_traj_tz =    0.5
)

write_fst(price_traj_3, "./inputs/other_parameters/GLP_price_traj_3.fst")


price_traj_4 <- data.table( year   =    c(13:45),                               # Prize trajectory is only related to calendar year
                            
                            ## Price trajectory for Semaglutide                 # 0.5*list price
                            price_traj_sm = 0.5 * c(1,1,1,1,1,1,                # From 2013
                                                 1,1,1,1,1,1,                   # to 2024
                                                 1, 1, 1, 0.95, 0.95, 0.9,      # Before patient expire in 2031
                                                 0.7, 0.7, 0.7, 0.7, 0.7,       # Price drop starting 2031
                                                 0.5, 0.5, 0.5, 0.5, 0.5,       # Price drop starting 2036
                                                 0.3, 0.3, 0.3, 0.3, 0.3),      # Price drop starting 2041
                            
                            ## Price trajectory for Tirzepatide
                            price_traj_tz = 0.5 * c(1,1,1,1,1,1,                # From 2013
                                                 1,1,1,1,1,1,                   # to 2024
                                                 1,1,1,1,1,1,                   # Price mostly stay stable until 2031
                                                 0.95, 0.95, 0.9, 0.9, 0.9,     # Price modestly decline due to competitor
                                                 0.7, 0.7, 0.7, 0.7, 0.7,       # Price expire in 2036
                                                 0.5, 0.5, 0.5, 0.5, 0.5)       # Price drop starting 2041
)

write_fst(price_traj_4, "./inputs/other_parameters/GLP_price_traj_4.fst")


##### 20% list price #####
price_traj_5 <- data.table( year   =    c(13:45),                               # Prize trajectory is only related to calendar year
                            
                            ## Price trajectory for Semaglutide                 
                            price_traj_sm =    0.2,                             # 0.2*list price remains constant over time
                            
                            ## Price trajectory for Tirzepatide                 # 0.2*list price remain constant over time
                            price_traj_tz =    0.2
)

write_fst(price_traj_5, "./inputs/other_parameters/GLP_price_traj_5.fst")


price_traj_6 <- data.table( year   =    c(13:45),                               # Prize trajectory is only related to calendar year
                            
                            ## Price trajectory for Semaglutide                 # 0.2*list price
                            price_traj_sm = 0.2 * c(1,1,1,1,1,1,                # From 2013
                                                    1,1,1,1,1,1,                # to 2024
                                                    1, 1, 1, 0.95, 0.95, 0.9,   # Before patient expire in 2031
                                                    0.7, 0.7, 0.7, 0.7, 0.7,    # Price drop starting 2031
                                                    0.5, 0.5, 0.5, 0.5, 0.5,    # Price drop starting 2036
                                                    0.3, 0.3, 0.3, 0.3, 0.3),   # Price drop starting 2041
                            
                            ## Price trajectory for Tirzepatide
                            price_traj_tz = 0.2 * c(1,1,1,1,1,1,                # From 2013
                                                     1,1,1,1,1,1,               # to 2024
                                                     1,1,1,1,1,1,               # Price mostly stay stable until 2031
                                                     0.95, 0.95, 0.9, 0.9, 0.9, # Price modestly decline due to competitor
                                                     0.7, 0.7, 0.7, 0.7, 0.7,   # Price expire in 2036
                                                     0.5, 0.5, 0.5, 0.5, 0.5)   # Price drop starting 2041
)

write_fst(price_traj_6, "./inputs/other_parameters/GLP_price_traj_6.fst")

#################################################################################################
#-----------------------------------------   Testing   -----------------------------------------#
#################################################################################################
## Restructure the trtm_theo variable to have valid values (0,1,2,..) only for the treatment years
test_2 <- test[, trtm_year := {
  first_trt_year <- anchor_year
  
  fifelse(
    year < first_trt_year,
    NA_real_,
    trtm_theo
  )
}, by = pid]

## Merge the cost_data and the lifecourse
lc_test_sc1 <- lc[scenario == 'sc2']

lc_test_sc1[, trtm_year := {
  first_trt_year <- anchor_year
  
  fifelse(
    year < first_trt_year,
    NA_real_,
    trtm_theo
  )
}, by = pid]


absorb_dt(lc_test_sc1, price_index)
absorb_dt(lc_test_sc1, cost_data)

lc_test_sc1[, `:=`(
  
  GLP_cost = fifelse(is.na(GLP_cost), 0, GLP_cost),
  
  trtm_year = NULL
  
)][, treat_cost := {
  
  fifelse(
    scenario == "sc1" | scenario == "sc2", (GLP_cost * price_traj_sm),
    fifelse(
      scenario == "sc3" | scenario == "sc4", (GLP_cost * price_traj_tz),
      0
    )
  )
  
}]

#####################################################################
#---------------------------- Testing  -----------------------------#
#####################################################################

lc[, trtm_year := {
  first_trt_year <- anchor_year
  
  fifelse(
    year < first_trt_year,
    NA_real_,
    trtm_theo
  )
}, by = pid]


absorb_dt(lc, price_index)
absorb_dt(lc, cost_data)

lc[, `:=`(
  
  GLP_cost = fifelse(is.na(GLP_cost), 0, GLP_cost),
  
  trtm_year = NULL
  
)][, treat_cost := {
  
  fifelse(
    scenario == "sc1" | scenario == "sc2", (GLP_cost * price_traj_sm),
    fifelse(
      scenario == "sc3" | scenario == "sc4", (GLP_cost * price_traj_tz),
      0
    )
  )
  
}]

cea_strata <- c("scenario", "uptake_group", "sex", "agegrp", "year")

treat_costs_scl <- lc[, lapply(.SD, function(x){
  
  x <- sum(x * wt, na.rm = TRUE) ## Jane 16 Sep, add na.rm to all sum()
  
  return(x)
  
}), .SDcols = c("treat_cost"),
keyby = c(cea_strata)]


