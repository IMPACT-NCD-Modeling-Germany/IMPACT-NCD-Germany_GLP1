
##### Health economic parameters ----

# Load packages
library(fst)
library(data.table)
library(ggplot2)
library(CKutils)

# Set global vars
mc_s <- 30 # Number of MC samples


#### Healthcare costs ----

# Source: Kähm et al. 2020 Diabetic Medicine; Kähm et al. 2018 Diabetes Care #

# Approach: Baseline costs from Kähm 2020.
#           Uncomplicated diabetes cost from Kähm 2018.
#           CHD and Stroke costs for people with diabetes from Kähm 2018.
#           CHD and Stroke costs for people without diabetes using above - uncomplicated diabetes costs
#           from Kähm 2018.
# Assumptions:  - Costs of CHD in people with T2DM are comparable with costs in people without T2DM!
#               - Assumption: Standard error of mean estimate is 2% due to very high sample size and based on SEs in paper!

# TODO: make variable for SD parameter! Necessary?

par_s <- 80 # Paramters

# Inflation factors:

infl_2018 <- 107.3/103.4 # G:\Meine Ablage\PhD\Publications\2021_Diet_simulation_modeling_Germany\Preparation\data\indirect_cost
infl_2020 <- 107.3/105.3

##############################################################################
#------------------ Cost calculation: Semaglutide ---------------------------#
#----------------------------------------------------------------------------#
#------------------------------- Year 0 -------------------------------------#
#----------------------------------------------------------------------------#
# As we assume an intolerance rate after dose escalation, 
# we will apply cost of dose escalation to all eligible patients
itt_cost_dose_escalation_sm <- 752

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
#------------------ Cost calculation: Tirzepatide ---------------------------#
#----------------------------------------------------------------------------#
#------------------------------- Year 0 -------------------------------------#
#----------------------------------------------------------------------------#
# As we assume an intolerance rate after dose escalation, 
# we will apply cost of dose escalation to all eligible patients
itt_cost_dose_escalation_tz <- 1844

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

#for(i in 1:mc_s){
  
#  ## Ensure replicability #  
#  set.seed(log(i) * 42 + 1337) # Seed is parameter-specific!
  
#  ## Draw quantiles #
#  quantiles <- runif(par_s, min = 0, max = 1) * 0.9999
    
  ####################################################################################
  #-----------------------------------   CEA   --------------------------------------#
  ####################################################################################

  price_index <- data.table(year   =    c(25:45),
                            
                            ## Price trajectory for Semaglutide
                            price_indx_sm =    c(1, 1, 1, 0.95, 0.95, 0.9,      # Before patient expire in 2031
                                                 0.7, 0.7, 0.7, 0.7, 0.7,       # Price drop starting 2031
                                                 0.5, 0.5, 0.5, 0.5, 0.5,       # Price drop starting 2036
                                                 0.3, 0.3, 0.3, 0.3, 0.3),      # Price drop starting 2041
                            
                            ## Price trajectory for Tirzepatide
                            price_indx_tz =    c(1, 1, 1, 1, 1, 1,              # Price mostly stay stable until 2031
                                                 1, 0.95, 0.95, 0.9, 09,        # Price modestly decline due to competitor
                                                 0.7, 0.7, 0.7, 0.7, 0.7,       # Price expire in 2036
                                                 0.5, 0.5, 0.5, 0.5, 0.5)       # Price drop starting 2041
  )


  cost_data <- CJ(uptake_group  =    c(1,2,3),
                  scenario      =    c(1,2,3,4), 
                  trtm_theo     =    c(0:20))
                  
  
  ## CEA/BIA -- Scenario 1: semaglutide for 2 years
  cost_data[scenario == 1, 
            med_cost := fifelse(trtm_theo == 0, 752,
                                fifelse(trtm_theo == 1, 2874,
                                        fifelse(trtm_theo == 2, 2529,
                                                           0)))]
  
  ## CEA/BIA -- Scenario 2: semaglutide for lifetime
  cost_data[scenario == 2, 
            med_cost := fifelse(trtm_theo == 0, 752,
                                fifelse(trtm_theo == 1, 2874,
                                        2529))]
  
  ## CEA/BIA -- Scenario 3: tirzepatide for 2 years
  cost_data[scenario == 3, 
            med_cost := fifelse(trtm_theo == 0, 1844,
                                fifelse(trtm_theo == 1, 4993,
                                        fifelse(trtm_theo == 2, 4244,
                                                0)))]
  
  ## CEA/BIA -- Scenario 4: tirzepatide for lifetime
  cost_data[scenario == 4, 
            med_cost := fifelse(trtm_theo == 0, 1844,
                                fifelse(trtm_theo == 1, 4993,
                                        4244))]
  
  cost_data[, cost := cost * infl_2020]
  
  
  
  cost_data[, `:=`(sex = ifelse(sex == 1, "women", "men"),
                   mc = i)]
  
  cost_combine <- rbind(cost_data, cost_combine)
  
#}  

write_fst(cost_combine, "./inputs/other_parameters/healthcare_costs.fst")

cost_combine[, rn := .I]
tt <- cost_combine[, .(from = min(rn), to = max(rn)), keyby = mc]
write_fst(tt, "./inputs/other_parameters/healthcare_costs_indx.fst", 100L)



#### Indirect and productivity costs ----

# Source: Winter et al. 2008, Ulrich et al. 2016, Icks et al. 2020,
#         Icks et al. 2013, DeStatis ~ Verdienststrukturerhebung, Arbeitskosten, Sozialbeiträge der Arbeitnehmer

# Approach: Detailed in Productivity Cost Estimation document.
#           Categories: Premature death cost, Early retirement, Sick leave, T2DM self-management + time cost for health services
# Assumptions:  - ???

# TODO: make variable for SD parameter! Necessary?

par_s <- 80 # Paramters

indir_cost_combine <- data.table(NULL)

age_groups <- c("<25", agegrp_name(min_age = 25, max_age = 65))

# Construct shell for costs in premature death years beyond age group of death
# (IMPACT lifecourse does not include "dead" years)

indir_cost_pre_mort_shell <- CJ(age_indir_cost = age_groups,
                                sex =            c(1, 0)) # 1 = women, 0 = men

indir_cost_pre_mort_shell[age_indir_cost == "<25" & sex == 1,
                cost_death := 13067]
indir_cost_pre_mort_shell[age_indir_cost == "25-29" & sex == 1,
                cost_death := 28293]
indir_cost_pre_mort_shell[age_indir_cost == "30-34" & sex == 1,
                cost_death := 29700]
indir_cost_pre_mort_shell[age_indir_cost == "35-39" & sex == 1,
                cost_death := 26133]
indir_cost_pre_mort_shell[age_indir_cost == "40-44" & sex == 1,
                cost_death := 25107]
indir_cost_pre_mort_shell[age_indir_cost == "45-49" & sex == 1,
                cost_death := 25431]
indir_cost_pre_mort_shell[age_indir_cost == "50-54" & sex == 1,
                cost_death := 25939]
indir_cost_pre_mort_shell[age_indir_cost == "55-59" & sex == 1,
                cost_death := 25367]
indir_cost_pre_mort_shell[age_indir_cost == "60-64" & sex == 1,
                cost_death := 24168]
indir_cost_pre_mort_shell[age_indir_cost == "65+" & sex == 1,
                cost_death := 5304]

indir_cost_pre_mort_shell[age_indir_cost == "<25" & sex == 0,
                cost_death := 14094]
indir_cost_pre_mort_shell[age_indir_cost == "25-29" & sex == 0,
                cost_death := 32431]
indir_cost_pre_mort_shell[age_indir_cost == "30-34" & sex == 0,
                cost_death := 38100]
indir_cost_pre_mort_shell[age_indir_cost == "35-39" & sex == 0,
                cost_death := 41147]
indir_cost_pre_mort_shell[age_indir_cost == "40-44" & sex == 0,
                cost_death := 42241]
indir_cost_pre_mort_shell[age_indir_cost == "45-49" & sex == 0,
                cost_death := 43841]
indir_cost_pre_mort_shell[age_indir_cost == "50-54" & sex == 0,
                cost_death := 45026]
indir_cost_pre_mort_shell[age_indir_cost == "55-59" & sex == 0,
                cost_death := 43790]
indir_cost_pre_mort_shell[age_indir_cost == "60-64" & sex == 0,
                cost_death := 40812]
indir_cost_pre_mort_shell[age_indir_cost == "65+" & sex == 0,
                cost_death := 5400]

# Add fringe benefits (Sozialbeiträge der Arbeitgeber; Tabelle Seite 16 in Arbeits und Lohnkosten 2020)

fringe_rate <- 1 - 48038/62273

indir_cost_pre_mort_shell[, cost_death := cost_death/(1-fringe_rate)]

# Inflate to 2022

infl_2018 <- 116.00733/105.8160 # Arbeitskosten index 2022/2018

indir_cost_pre_mort_shell[, cost_death := cost_death * infl_2018][is.na(cost_death), cost_death := 0]


setkey(indir_cost_pre_mort_shell, "sex")

indir_cost_pre_mort_shell[, age_id := rep(c(seq(2, 10, 1), 1), 2)]

setkey(indir_cost_pre_mort_shell, "sex", "age_id")

indir_cost_pre_mort <- CJ(age_indir_cost = age_groups,
                          sex =            c(1, 0)) # 1 = women, 0 = men

for(i in unique(indir_cost_pre_mort_shell$age_id)){
  for(j in unique(indir_cost_pre_mort_shell$sex)){
  
    age_grp <- as.character(indir_cost_pre_mort_shell[age_id == i & sex == j, "age_indir_cost"])
    
    cost_death_ij <- 5 * as.numeric(indir_cost_pre_mort_shell[age_id > i & age_id < 10 &
                                                         sex == j, lapply(.SD, sum), .SDcols = "cost_death"])
    
    indir_cost_pre_mort[age_indir_cost == age_grp & sex == j, cost_death_cum := cost_death_ij]
    
  }
}

indir_cost_pre_mort[is.na(cost_death_cum), cost_death_cum := 0]


for(i in 1:mc_s){
  
  ## Ensure replicability #  
  set.seed(log(i) * 42 + 60 + 1337) # Seed is parameter-specific!
  
  ## Draw quantiles #
  quantiles <- runif(par_s, min = 0, max = 1) * 0.9999
  
  indir_cost_data <- CJ(age_indir_cost =  age_groups,
                        sex =             c(1, 0), # 1 = women, 0 = men
                        t2dm_stat =       c(1, 0), # 1 = prevalent year, 0 = no disease
                        stroke_stat =     c(2, 1, 0), # 1/2 = prevalent year, 0 = no disease
                        chd_pre_mort =    c(1, 0), # 1 = premature death, 0 = no disease
                        stroke_pre_mort = c(1, 0),
                        other_pre_mort =  c(1, 0))
  
  indir_cost_data <- indir_cost_data[!(chd_pre_mort == 1 & stroke_pre_mort == 1)]
  indir_cost_data <- indir_cost_data[!(chd_pre_mort == 1 & other_pre_mort == 1)]
  indir_cost_data <- indir_cost_data[!(stroke_pre_mort == 1 & other_pre_mort == 1)]
  

  
  
  ## Premature death costs (Tabelle 3.3.1 Bruttojahresverdientse in Verdienststrukturerhebung 2018) #
  
  # Women
  indir_cost_data[(chd_pre_mort == 1 | stroke_pre_mort == 1 | other_pre_mort == 1) &
                    age_indir_cost == "<25" & sex == 1,
                  cost_death := 13067]
  indir_cost_data[(chd_pre_mort == 1 | stroke_pre_mort == 1 | other_pre_mort == 1) &
                    age_indir_cost == "25-29" & sex == 1,
                  cost_death := 28293]
  indir_cost_data[(chd_pre_mort == 1 | stroke_pre_mort == 1 | other_pre_mort == 1) &
                    age_indir_cost == "30-34" & sex == 1,
                  cost_death := 29700]
  indir_cost_data[(chd_pre_mort == 1 | stroke_pre_mort == 1 | other_pre_mort == 1) &
                    age_indir_cost == "35-39" & sex == 1,
                  cost_death := 26133]
  indir_cost_data[(chd_pre_mort == 1 | stroke_pre_mort == 1 | other_pre_mort == 1) &
                    age_indir_cost == "40-44" & sex == 1,
                  cost_death := 25107]
  indir_cost_data[(chd_pre_mort == 1 | stroke_pre_mort == 1 | other_pre_mort == 1) &
                    age_indir_cost == "45-49" & sex == 1,
                  cost_death := 25431]
  indir_cost_data[(chd_pre_mort == 1 | stroke_pre_mort == 1 | other_pre_mort == 1) &
                    age_indir_cost == "50-54" & sex == 1,
                  cost_death := 25939]
  indir_cost_data[(chd_pre_mort == 1 | stroke_pre_mort == 1 | other_pre_mort == 1) &
                    age_indir_cost == "55-59" & sex == 1,
                  cost_death := 25367]
  indir_cost_data[(chd_pre_mort == 1 | stroke_pre_mort == 1 | other_pre_mort == 1) &
                    age_indir_cost == "60-64" & sex == 1,
                  cost_death := 24168]
  indir_cost_data[(chd_pre_mort == 1 | stroke_pre_mort == 1 | other_pre_mort == 1) &
                    age_indir_cost == "65+" & sex == 1,
                  cost_death := 5304]
  
  # Men
  indir_cost_data[(chd_pre_mort == 1 | stroke_pre_mort == 1 | other_pre_mort == 1) &
                    age_indir_cost == "<25" & sex == 0,
                  cost_death := 14094]
  indir_cost_data[(chd_pre_mort == 1 | stroke_pre_mort == 1 | other_pre_mort == 1) &
                    age_indir_cost == "25-29" & sex == 0,
                  cost_death := 32431]
  indir_cost_data[(chd_pre_mort == 1 | stroke_pre_mort == 1 | other_pre_mort == 1) &
                    age_indir_cost == "30-34" & sex == 0,
                  cost_death := 38100]
  indir_cost_data[(chd_pre_mort == 1 | stroke_pre_mort == 1 | other_pre_mort == 1) &
                    age_indir_cost == "35-39" & sex == 0,
                  cost_death := 41147]
  indir_cost_data[(chd_pre_mort == 1 | stroke_pre_mort == 1 | other_pre_mort == 1) &
                    age_indir_cost == "40-44" & sex == 0,
                  cost_death := 42241]
  indir_cost_data[(chd_pre_mort == 1 | stroke_pre_mort == 1 | other_pre_mort == 1) &
                    age_indir_cost == "45-49" & sex == 0,
                  cost_death := 43841]
  indir_cost_data[(chd_pre_mort == 1 | stroke_pre_mort == 1 | other_pre_mort == 1) &
                    age_indir_cost == "50-54" & sex == 0,
                  cost_death := 45026]
  indir_cost_data[(chd_pre_mort == 1 | stroke_pre_mort == 1 | other_pre_mort == 1) &
                    age_indir_cost == "55-59" & sex == 0,
                  cost_death := 43790]
  indir_cost_data[(chd_pre_mort == 1 | stroke_pre_mort == 1 | other_pre_mort == 1) &
                    age_indir_cost == "60-64" & sex == 0,
                  cost_death := 40812]
  indir_cost_data[(chd_pre_mort == 1 | stroke_pre_mort == 1 | other_pre_mort == 1) &
                    age_indir_cost == "65+" & sex == 0,
                  cost_death := 5400]
  
  # Add fringe benefits (Sozialbeiträge der Arbeitgeber; Tabelle Seite 16 in Arbeits und Lohnkosten 2020)
  
  fringe_rate <- 1 - 48038/62273
  
  indir_cost_data[, cost_death := cost_death/(1-fringe_rate)]
  
  # Inflate to 2022
  
  infl_2018 <- 116.00733/105.8160 # Arbeitskosten index 2022/2018
  
  indir_cost_data[, cost_death := cost_death * infl_2018][is.na(cost_death), cost_death := 0]
  
  # Costs in premature death years beyond age group of death (IMPACT lifecourse does not include "dead" years)
  
  absorb_dt(indir_cost_data, indir_cost_pre_mort)
  
  indir_cost_data[cost_death == 0, cost_death_cum := 0]
  
  
  
  ## Early retirement cost
  
  # Type 2 Diabetes (Ulrich et al. 2016) #
  
  # SE from paper: (4103-3024)/1.96 = 550.5102 => 13.41726% of mean estimate => Assumption: Variation of 15%
  
  indir_cost_data[, cost_rtr_t2dm := ifelse(t2dm_stat == 1,
                                            qnorm(quantiles[1], mean = 4103 - 3344, sd = (4103 - 3344) * 0.15),
                                            qnorm(quantiles[1], mean = 1981 - 1299, sd = (1981 - 1299) * 0.15))]
  
  infl_ulrich <- 46764/36103 # Same source as Ulrich paper but value for 2021
  infl_2021 <- 116.00733/114.94350 # Additional inflation to 2022 using Arbeitskostenindex
  
  indir_cost_data[, cost_rtr_t2dm := cost_rtr_t2dm * infl_ulrich * infl_2021]
  
  # Stroke (Winter et al. 2008) #
  
  #Assumption: Gamma distribution due to sample size!
  shape <- 1130**2/1170**2
  scale <- 1170**2/1130
  
  indir_cost_data[, cost_rtr_stroke := ifelse(stroke_stat %in% c(1, 2),
                                              qgamma(quantiles[2], shape = shape, scale = scale),
                                              0)]
  
  infl_2003 <- 116.00733/77.20250 # Inflation to 2022 using Arbeitskostenindex
  
  indir_cost_data[, cost_rtr_stroke := cost_rtr_stroke * infl_2003]
  
  
  
  
  ## Sick leave cost
  
  # Type 2 Diabetes (Ulrich et al. 2016) #
  
  indir_cost_data[, cost_scklv_t2dm := ifelse(t2dm_stat == 1,
                                              qnorm(quantiles[3], mean = 3344, sd = (3344 - 1995)/1.96),
                                              qnorm(quantiles[3], mean = 1299, sd = (1299 - 1185)/1.96))]
  
  indir_cost_data[, cost_scklv_t2dm := cost_scklv_t2dm * infl_ulrich * infl_2021]
  
  # Stroke (Winter et al. 2008) #
  
  #Assumption: Gamma distribution due to sample size!
  shape <- 130**2/870**2
  scale <- 870**2/130
  
  indir_cost_data[, cost_scklv_stroke := ifelse(stroke_stat %in% c(1, 2),
                                                qgamma(quantiles[4], shape = shape, scale = scale),
                                                0)]
  
  indir_cost_data[, cost_scklv_stroke := cost_scklv_stroke * infl_2003]
  
  
  
  
  ## Type 2 Diabetes Self-Management #
  
  indir_cost_data[, cost_slfmgt_t2dm := ifelse(t2dm_stat == 1,
                                              qnorm(quantiles[5], mean = 2068, sd = (2068 - 1658)/1.96),
                                              0)]

  infl_2014 <- 116.00733/95.36950 # Inflation to 2022 using Arbeitskostenindex
  
  indir_cost_data[, cost_slfmgt_t2dm := cost_slfmgt_t2dm * infl_2014]
  
  
  ## Health Services Time Cost for People with and without Diabetes #
  
  indir_cost_data[, cost_time_t2dm := ifelse(t2dm_stat == 1,
                                             qnorm(quantiles[6], mean = 2447.1, sd = (2447.1 - 804.5)/1.96),
                                             0)]
  
  indir_cost_data[, cost_time := ifelse(t2dm_stat == 0,
                                        qnorm(quantiles[7], mean = 589.2, sd = (589.2 - 435.8)/1.96),
                                        0)]
  
  infl_2011 <- 116.00733/88.9120 # Additional inflation to 2022 using Arbeitskostenindex
  
  indir_cost_data[, cost_time_t2dm := cost_time_t2dm * infl_2011]
  indir_cost_data[, cost_time := cost_time * infl_2011]
  
  
  indir_cost_data[, `:=`(sex = ifelse(sex == 1, "women", "men"),
                         mc = i)]
  
  indir_cost_combine <- rbind(indir_cost_data, indir_cost_combine)
  
}  

indir_cost_combine[age_indir_cost == "65+", (grep("cost_", names(indir_cost_combine), value = TRUE)) := 0]

write_fst(indir_cost_combine, "./inputs/other_parameters/indirect_costs.fst")

indir_cost_combine[, rn := .I]
tt <- indir_cost_combine[, .(from = min(rn), to = max(rn)), keyby = mc]
write_fst(tt, "./inputs/other_parameters/indirect_costs_indx.fst", 100L)




### TESTING FOR COST DATA ###


#se_int <- ((8.023 - 8.012)/1.96)
#se_age50 <- ((-1.129 + 1.149)/1.96)
#se_sex <- ((0.055 - 0.047)/1.96)
#mean <- 8.023 - 1.129 + 0.055
#var <- se_int^2 + se_age^2 + 2 * se_age * 0.5 + 2 * se_sex * 0.5 
#
#shp <- (mean^2)/(var^2) 
#scl <- (var^2)/mean
#df <- as.data.table(exp(rgamma(10000, shape = shp, scale = scl)))
#
##df <- as.data.table(exp(rnorm(10000, mean = mean, sd = var)))
#
#ggplot(df, aes(V1)) + geom_density()
#
#
#exp(qgamma(0.999, shape = shp, scale = scl))
#
#
# ACTUAL UNCERTAINTY IN REGRESSION:
#
# Idea: Like prediction of standard errors! sqrt(t(newdata) %*% vcov(mod) %*% newdata)
#       newdata is predictor values!
#
#
#
# 
# #### OLD/DEPRECEATED: Health utility values ----
# 
# # Source: Laxy et al. 2021 Value in Health #
# 
# util_combine <- data.table(NULL)
# 
# for(i in 1:mc_s){
#  
#   ## Ensure replicability #  
#   set.seed(log(i) * 42/2 + 1337) # Seed is parameter-specific!
#   
#   ## Draw quantiles #
#   quantiles <- runif(par_s, min = 0, max = 1) * 0.9999
#   
#   ## Create data shell #
#   util_data <- CJ(sex =         c(1, 0), # 1 = women, 0 = men
#                   t2dm_stat =   c(1, 0),
#                   chd_stat =    c(3, 2, 1, 0),
#                   stroke_stat = c(3, 2, 1, 0))
#   
#   ## Age and sex utility decrements #
#   util_data[, `:=`(util_incpt = qnorm(quantiles[1], mean = 1.187, sd = 0.019),
#                    util_age = qnorm(quantiles[2], mean = -0.003, sd = 0.0001),
#                    util_sex = sex * qnorm(quantiles[3], mean = -0.029, sd = 0.004))]
#   
#   ## BMI decrement #
#   util_data[, util_bmi := qnorm(quantiles[4], mean = -0.003, sd = 0.0001)]
#   
#   ## Disease-specific utility decrements #
#   # Type 2 Diabetes
#   util_data[, util_disease := fifelse(t2dm_stat == 1 & chd_stat == 0 & stroke_stat == 0,
#                                       qnorm(quantiles[5], mean = -0.028, sd = 0.014), # Only Diabetes
#                               fifelse(t2dm_stat == 1 & chd_stat >= 1 & stroke_stat == 0,
#                                       qnorm(quantiles[5], mean = -0.028, sd = 0.014), ## TBD! Diabetes and CHD
#                               fifelse(t2dm_stat == 1 & chd_stat == 0 & stroke_stat >= 1,
#                                       qnorm(quantiles[6], mean = -0.122, sd = 0.018) +
#                                         qnorm(quantiles[5], mean = -0.028, sd = 0.014), # Diabetes and Stroke
#                               fifelse(t2dm_stat == 1 & chd_stat >= 1 & stroke_stat >= 1,
#                                       qnorm(quantiles[6], mean = -0.122, sd = 0.018) +
#                                         qnorm(quantiles[5], mean = -0.028, sd = 0.014), 0))))] # Diabetes, CHD and Stroke
#   
#   # Coronary Heart Disease and Stroke
#   util_data[, util_disease := fifelse(t2dm_stat == 0 & chd_stat >= 1 & stroke_stat == 0,
#                                       qnorm(quantiles[7], mean = -0.028, sd = 0.010), # Only CHD
#                               fifelse(t2dm_stat == 0 & chd_stat >= 1 & stroke_stat >= 1,
#                                       qnorm(quantiles[8], mean = -0.070, sd = 0.010) +
#                                         qnorm(quantiles[7], mean = -0.028, sd = 0.010), ## TBD! CHD and Stroke
#                               fifelse(t2dm_stat == 0 & chd_stat == 0 & stroke_stat >= 1,
#                                       qnorm(quantiles[8], mean = -0.070, sd = 0.010), util_disease)))] # Only Stroke 
#   
#   util_data[, `:=`(sex = ifelse(sex == 1, "women", "men"),
#                    mc = i)]
#   
#   util_combine <- rbind(util_data, util_combine)
# 
# }
# 
# write_fst(util_combine, "./inputs/other_parameters/health_utility.fst")
# 
# util_combine[, rn := .I]
# tt <- util_combine[, .(from = min(rn), to = max(rn)), keyby = mc]
# write_fst(tt, "./inputs/other_parameters/health_utility_indx.fst", 100L)

