
##########################################################################################
#----------------------------------------------------------------------------------------#
#----------- IMPACT-NCD-Germany/GLP1/inputs/Uncertainty on treatment effects ------------#
#------------------------------- for nutrition therapy ----------------------------------#
#----------------------------------------------------------------------------------------#
##########################################################################################

##########################################################################################
#---------------------------  uncertainty on bmi efficacy  ------------------------------#
##########################################################################################
#library(data.table)
#library(fst)
#### Simulate uncertainty in bmi efficacy input at treatment year 1 -------------------------------
bmi_1y <- -0.024                # Data from STEP 1 RCT paper (Sample size: N=1306 for semaglutide)
# ((13.37-12.44)/1.96)/100       # STEP 1 RCT paper, Table 2: 95% CI for difference between group: −12.44 (–13.37 to –11.51)
# we assumed that, % of between group se on between group mean, will be around the same % of
# control group se on control group mean
# ((13.37-12.44)/1.96)/12.44 --> ~4% --> we will use the % for the control group/placebo arm

bmi_1y_se <- (((13.37-12.44)/1.96)/12.44) * 0.024

n_samples <- 10000

# Ensure replicability
set.seed(log(n_samples) + 1337) # Seed is parameter-specific!

# Draw quantiles
quantiles <- runif(n_samples, min = 0, max = 1) * 0.999

# Create a table with mc and bmi_1y
bmi_1y_samples <- data.table(
  mc = 1:n_samples,
  bmi_1y = qnorm(quantiles, mean = bmi_1y, sd = bmi_1y_se)
)

write_fst(bmi_1y_samples, "bmi_1y_samples_nutr.fst")

###################################################################################################
#### Simulate uncertainty in bmi efficacy input at treatment year 2 -------------------------------
bmi_2y <- -0.026                  # Data from STEP 5 RCT paper (Sample size: N=152 for semaglutide)
bmi_2y_se <- 0.011                # Data from STEP 5 RCT paper (95% CI in Table 2 for semaglutide)

n_samples <- 10000

## Ensure replicability #
set.seed(log(n_samples) + 1337) # Seed is parameter-specific!

## Draw quantiles #
quantiles <- runif(n_samples, min = 0, max = 1) * 0.999

## Create a table with mc and bmi_rct
bmi_2y_samples <- data.table(
  mc = 1:n_samples,
  bmi_2y = qnorm(quantiles, mean = bmi_2y, sd = bmi_2y_se)
)

write_fst(bmi_2y_samples, "bmi_2y_samples_nutr.fst")

###################################################################################################

##########################################################################################
#---------------------------  uncertainty on sbp efficacy  ------------------------------#
##########################################################################################
#### Simulate uncertainty in SBP efficacy input at treatment year 1 -------------------------------
sbp_1y <- -1.1               # Data from STEP 1 RCT paper (Sample size: N=1306 for semaglutide)
# (6.34-5.10)/1.96           # STEP 1 RCT paper, Table 2: 95% CI for difference between group: −5.10 (–6.34 to –3.87)
# we assumed that, % of between group se on between group mean, will be around the same % of
# intervention group se on intervention group mean
# ((6.34-5.10)/1.96)/5.10 --> ~12% --> we will use the % for the intervention group

sbp_1y_se <- (((6.34-5.10)/1.96)/5.10) * 1.1

n_samples <- 10000

# Ensure replicability
set.seed(log(n_samples) + 1337) # Seed is parameter-specific!

# Draw quantiles
quantiles <- runif(n_samples, min = 0, max = 1) * 0.999

# Create a table with mc and sbp_1y
sbp_1y_samples <- data.table(
  mc = 1:n_samples,
  sbp_1y = qnorm(quantiles, mean = sbp_1y, sd = sbp_1y_se)
)

write_fst(sbp_1y_samples, "sbp_1y_samples_nutr.fst")

###################################################################################################
#### Simulate uncertainty in SBP efficacy input at treatment year 2 -------------------------------
sbp_2y <- -1.6                # Data from STEP 5 RCT paper (Sample size: N=152 for semaglutide)
sbp_2y_se <- 1.2              # Data from STEP 5 RCT paper (95% CI in Table 2 for semaglutide)

n_samples <- 10000

## Ensure replicability #
set.seed(log(n_samples) + 1337) # Seed is parameter-specific!

## Draw quantiles #
quantiles <- runif(n_samples, min = 0, max = 1) * 0.999

## Create a table with mc and sbp_rct
sbp_2y_samples <- data.table(
  mc = 1:n_samples,
  sbp_2y = qnorm(quantiles, mean = sbp_2y, sd = sbp_2y_se)
)

write_fst(sbp_2y_samples, "sbp_2y_samples_nutr.fst")

###################################################################################################

##########################################################################################
#----------------------------------------------------------------------------------------#
#----------- IMPACT-NCD-Germany/GLP1/inputs/Uncertainty on treatment effects ------------#
#------------------------------- for Semaglutide treatment ------------------------------#
#----------------------------------------------------------------------------------------#
##########################################################################################

##########################################################################################
#---------------------------  uncertainty on bmi efficacy  ------------------------------#
##########################################################################################

#library(data.table)
#library(fst)
#### Simulate uncertainty in bmi efficacy input at treatment year 1 -------------------------------
bmi_1y <- -0.149                 # Data from STEP 1 RCT paper (Sample size: N=1306 for semaglutide)
# ((13.37-12.44)/1.96)/100       # STEP 1 RCT paper, Table 2: 95% CI for difference between group: −12.44 (–13.37 to –11.51)
# we assumed that, % of between group se on between group mean, will be around the same % of
# intervention group se on intervention group mean
# ((13.37-12.44)/1.96)/12.44 --> ~4% --> we will use the % for the intervention group

bmi_1y_se <- (((13.37-12.44)/1.96)/12.44) * 0.149

n_samples <- 10000

# Ensure replicability
set.seed(log(n_samples) + 1337) # Seed is parameter-specific!

# Draw quantiles
quantiles <- runif(n_samples, min = 0, max = 1) * 0.999

# Create a table with mc and bmi_1y
bmi_1y_samples <- data.table(
  mc = 1:n_samples,
  bmi_1y = qnorm(quantiles, mean = bmi_1y, sd = bmi_1y_se)
)

write_fst(bmi_1y_samples, "bmi_1y_samples_semag.fst")

###################################################################################################
#### Simulate uncertainty in bmi efficacy input at treatment year 2 -------------------------------
bmi_2y <- -0.152                  # Data from STEP 5 RCT paper (Sample size: N=152 for semaglutide)
bmi_2y_se <- 0.009                # Data from STEP 5 RCT paper (95% CI in Table 2 for semaglutide)

n_samples <- 10000

## Ensure replicability #
set.seed(log(n_samples) + 1337) # Seed is parameter-specific!

## Draw quantiles #
quantiles <- runif(n_samples, min = 0, max = 1) * 0.999

## Create a table with mc and bmi_rct
bmi_2y_samples <- data.table(
  mc = 1:n_samples,
  bmi_2y = qnorm(quantiles, mean = bmi_2y, sd = bmi_2y_se)
)

write_fst(bmi_2y_samples, "bmi_2y_samples_semag.fst")

###################################################################################################
#### Simulate uncertainty in bmi efficacy input at treatment year 3 ------------------------------
#### For the 1 year after treatment cessation:  regain 2/3 of weight loss
# mean: 11.6, se_2y: se = sd/sqrt(N)  ---> we can calculate a % of se on mean
(7.7/(sqrt(197)))/11.6                     # 4.7%
# mean: 17.3, se_1y: se = sd/sqrt(N)  ---> we can calculate a % of se on mean
(9.3/(sqrt(228)))/17.3                     # 3.6%
# Get the average of these two %
((7.7/(sqrt(197)))/11.6 + (9.3/(sqrt(228)))/17.3)/2
# we assumed that, the % of se on mean is the same (~4%)
bmi_3y <- -0.05
bmi_3y_se <- 0.04*0.05

n_samples <- 10000

## Ensure replicability #
set.seed(log(n_samples) + 1337) # Seed is parameter-specific!

## Draw quantiles #
quantiles <- runif(n_samples, min = 0, max = 1) * 0.999

## Create a table with mc and bmi_rct
bmi_3y_samples <- data.table(
  mc = 1:n_samples,
  bmi_3y = qnorm(quantiles, mean = bmi_3y, sd = bmi_3y_se)
)

write_fst(bmi_3y_samples, "bmi_3y_samples_semag.fst")

###################################################################################################
#### Simulate uncertainty in bmi efficacy input at treatment year 4 ------------------------------
# we assumed that, the % of se on mean is the same (~4%)
bmi_4y <- -0.025
bmi_4y_se <- 0.04*0.025

n_samples <- 10000

## Ensure replicability #
set.seed(log(n_samples) + 1337) # Seed is parameter-specific!

## Draw quantiles #
quantiles <- runif(n_samples, min = 0, max = 1) * 0.999

## Create a table with mc and bmi_rct
bmi_4y_samples <- data.table(
  mc = 1:n_samples,
  bmi_4y = qnorm(quantiles, mean = bmi_4y, sd = bmi_4y_se)
)

write_fst(bmi_4y_samples, "bmi_4y_samples_semag.fst")

##########################################################################################
#---------------------------  uncertainty on sbp efficacy  ------------------------------#
##########################################################################################
#### Simulate uncertainty in SBP efficacy input at treatment year 1 -------------------------------
sbp_1y <- -6.2               # Data from STEP 1 RCT paper (Sample size: N=1306 for semaglutide)
# (6.34-5.10)/1.96           # STEP 1 RCT paper, Table 2: 95% CI for difference between group: −5.10 (–6.34 to –3.87)
# we assumed that, % of between group se on between group mean, will be around the same % of
# intervention group se on intervention group mean
# ((6.34-5.10)/1.96)/5.10 --> ~12% --> we will use the % for the intervention group

sbp_1y_se <- (((6.34-5.10)/1.96)/5.10) * 6.2

n_samples <- 10000

# Ensure replicability
set.seed(log(n_samples) + 1337) # Seed is parameter-specific!

# Draw quantiles
quantiles <- runif(n_samples, min = 0, max = 1) * 0.999

# Create a table with mc and sbp_1y
sbp_1y_samples <- data.table(
  mc = 1:n_samples,
  sbp_1y = qnorm(quantiles, mean = sbp_1y, sd = sbp_1y_se)
)

write_fst(sbp_1y_samples, "sbp_1y_samples_semag.fst")

###################################################################################################
#### Simulate uncertainty in SBP efficacy input at treatment year 2 -------------------------------
sbp_2y <- -5.7                # Data from STEP 5 RCT paper (Sample size: N=152 for semaglutide)
sbp_2y_se <- 1.1              # Data from STEP 5 RCT paper (95% CI in Table 2 for semaglutide)

n_samples <- 10000

## Ensure replicability #
set.seed(log(n_samples) + 1337) # Seed is parameter-specific!

## Draw quantiles #
quantiles <- runif(n_samples, min = 0, max = 1) * 0.999

## Create a table with mc and sbp_rct
sbp_2y_samples <- data.table(
  mc = 1:n_samples,
  sbp_2y = qnorm(quantiles, mean = sbp_2y, sd = sbp_2y_se)
)

write_fst(sbp_2y_samples, "sbp_2y_samples_semag.fst")

##########################################################################################
#---------------------------  uncertainty on tcl efficacy  ------------------------------#
##########################################################################################
#### Simulate uncertainty in Tchol efficacy input at treatment year 1 -------------------------------
tchol_1y <- -0.03             # Data from STEP 1 RCT paper (Sample size: N=1306 for semaglutide)
# (0.97-0.95)/1.96            # STEP 1 RCT paper, Table 2: 95% CI for difference between group: 0.97 (0.95 to 0.98)
# (5-3)/1.96                  # 95% CI for the % difference (efficacy input): -3 (-5 to -2)
# we assumed that, % of between group se on between group mean, will be around the same % of
# intervention group se on intervention group mean
# ((5-3)/1.96)/3              --> ~34% --> we will use the % for the intervention group
# tchol_1y_se <- (((0.97-0.95)/1.96)/0.97) * 0.03
tchol_1y_se <- (((5-3)/1.96)/3) * 0.03

n_samples <- 10000

# Ensure replicability
set.seed(log(n_samples) + 1337) # Seed is parameter-specific!

# Draw quantiles
quantiles <- runif(n_samples, min = 0, max = 1) * 0.999

# Create a table with mc and tchol_1y
tchol_1y_samples <- data.table(
  mc = 1:n_samples,
  tchol_1y = qnorm(quantiles, mean = tchol_1y, sd = tchol_1y_se)
)

write_fst(tchol_1y_samples, "tchol_1y_samples_semag.fst")

###################################################################################################
#### Simulate uncertainty in tchol efficacy input at treatment year 2 -------------------------------
tchol_2y <- -0.033              # Data from STEP 5 RCT paper (Sample size: N=152 for semaglutide)
# (8.4-4.6)/1.96              # STEP 5 RCT paper, Table 2: 95% CI for difference between group: –4.6 (–8.4 to –0.6)
# we assumed that, % of between group se on between group mean, will be around the same % of
# intervention group se on intervention group mean
# ((8.4-4.6)/1.96)/4.6 --> ~42% --> we will use the % for the intervention group
# ((4.6-0.6)/1.96)/4.6 --> ~42% --> we will use the % for the intervention group

tchol_2y_se <- ((8.4-4.6)/1.96)/4.6 * 0.033

n_samples <- 10000

## Ensure replicability #
set.seed(log(n_samples) + 1337) # Seed is parameter-specific!

## Draw quantiles #
quantiles <- runif(n_samples, min = 0, max = 1) * 0.999

## Create a table with mc and tchol_rct
tchol_2y_samples <- data.table(
  mc = 1:n_samples,
  tchol_2y = qnorm(quantiles, mean = tchol_2y, sd = tchol_2y_se)
)

write_fst(tchol_2y_samples, "tchol_2y_samples_semag.fst")

###################################################################################################

##########################################################################################
#----------------------------------------------------------------------------------------#
#----------- IMPACT-NCD-Germany/GLP1/inputs/Uncertainty on treatment effects ------------#
#------------------------------- for Tirzepatide treatment ------------------------------#
#----------------------------------------------------------------------------------------#
##########################################################################################

##########################################################################################
#---------------------------  uncertainty on bmi efficacy  ------------------------------#
##########################################################################################

#library(data.table)
#library(fst)
#### Simulate uncertainty in bmi efficacy input at treatment year 1 -------------------------------
bmi_1y <- -0.209                 # Data from SURMOUNT-1 RCT paper (Sample size: N=630 for Tirzepatide)
                                 # −20.9 (−21.8 to −19.9): least-square mean (95% CI)
bmi_1y_se <- ((0.218-0.199)/2)/1.96

n_samples <- 10000

# Ensure replicability
set.seed(log(n_samples) + 1337) # Seed is parameter-specific!

# Draw quantiles
quantiles <- runif(n_samples, min = 0, max = 1) * 0.999

# Create a table with mc and bmi_1y
bmi_1y_samples <- data.table(
  mc = 1:n_samples,
  bmi_1y = qnorm(quantiles, mean = bmi_1y, sd = bmi_1y_se)
)

write_fst(bmi_1y_samples, "bmi_1y_samples_tirzp.fst")

###################################################################################################
#### Simulate uncertainty in bmi efficacy input at treatment year 3 ------------------------------
#### For the 1 year after treatment cessation:  regain ~2/3 of weight loss
#### Simulate uncertainty in bmi efficacy input at treatment year 1 -------------------------------
(1*(1-0.209)*(1+0.14)) - 1     # ~ -0.09826 Data from SURMOUNT-4 RCT paper (Sample size: N=355 for treatment withdrawn)
bmi_3y <- -0.10                # Change in body weight from wk36 to 88, %
                               # 14.0 (12.8 to 15.2): least-square mean (95% CI)

(7.3/sqrt(670))/20.9            # se = sd/sqrt(N): 7.3/sqrt(670) --- % weight change from wk 0 to 36
(((0.152-0.128)/2)/1.96)/0.14   # se = (upper CI - lower CI)/(2*1.96) --- % weight change from wk 36 to 88
# Get the average of these two %
((7.3/sqrt(670))/20.9 + (((0.152-0.128)/2)/1.96)/0.14)/2
# we assumed that, the % of se on mean is the same (~3%)

bmi_3y_se <- 0.03*0.10 

n_samples <- 10000

## Ensure replicability #
set.seed(log(n_samples) + 1337) # Seed is parameter-specific!

## Draw quantiles #
quantiles <- runif(n_samples, min = 0, max = 1) * 0.999

## Create a table with mc and bmi_rct
bmi_3y_samples <- data.table(
  mc = 1:n_samples,
  bmi_3y = qnorm(quantiles, mean = bmi_3y, sd = bmi_3y_se)
)

write_fst(bmi_3y_samples, "bmi_3y_samples_tirzp.fst")

###################################################################################################
#### Simulate uncertainty in bmi efficacy input at treatment year 4 ------------------------------
# we assumed that, the % of se on mean is the same (~4%)
bmi_4y <- -0.05
bmi_4y_se <- 0.03*0.05

n_samples <- 10000

## Ensure replicability #
set.seed(log(n_samples) + 1337) # Seed is parameter-specific!

## Draw quantiles #
quantiles <- runif(n_samples, min = 0, max = 1) * 0.999

## Create a table with mc and bmi_rct
bmi_4y_samples <- data.table(
  mc = 1:n_samples,
  bmi_4y = qnorm(quantiles, mean = bmi_4y, sd = bmi_4y_se)
)

write_fst(bmi_4y_samples, "bmi_4y_samples_tirzp.fst")

##########################################################################################
#---------------------------  uncertainty on sbp efficacy  ------------------------------#
##########################################################################################

#### Simulate uncertainty in SBP efficacy input at treatment year 1 -------------------------------
sbp_1y <- -7.2               # Data from SURMOUNT-1 RCT paper (Sample size: N=630 for Tirzepatide)
                             # −7.2 (−7.8 to −6.7): least-square mean (95% CI)

sbp_1y_se <- (7.8-6.7)/(2*1.96)  

n_samples <- 10000

# Ensure replicability
set.seed(log(n_samples) + 1337) # Seed is parameter-specific!

# Draw quantiles
quantiles <- runif(n_samples, min = 0, max = 1) * 0.999

# Create a table with mc and sbp_1y
sbp_1y_samples <- data.table(
  mc = 1:n_samples,
  sbp_1y = qnorm(quantiles, mean = sbp_1y, sd = sbp_1y_se)
)

write_fst(sbp_1y_samples, "sbp_1y_samples_tirzp.fst")

##########################################################################################
#---------------------------  uncertainty on tcl efficacy  ------------------------------#
##########################################################################################
#### Simulate uncertainty in Tchol efficacy input at treatment year 1 -------------------------------
tchol_1y <- -0.048            # Data from SURMOUNT-1 RCT paper (Sample size: N=630 for Tirzepatide)
                              # −4.8 (−5.6 to −4.0): least-square mean (95% CI)
tchol_1y_se <- (0.056-0.04)/(2*1.96)

n_samples <- 10000

# Ensure replicability
set.seed(log(n_samples) + 1337) # Seed is parameter-specific!

# Draw quantiles
quantiles <- runif(n_samples, min = 0, max = 1) * 0.999

# Create a table with mc and tchol_1y
tchol_1y_samples <- data.table(
  mc = 1:n_samples,
  tchol_1y = qnorm(quantiles, mean = tchol_1y, sd = tchol_1y_se)
)

write_fst(tchol_1y_samples, "tchol_1y_samples_tirzp.fst")

###################################################################################################