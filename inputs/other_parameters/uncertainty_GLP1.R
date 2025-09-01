
##########################################################################################
#----------------------------------------------------------------------------------------#
#----- IMPACT-NCD-Germany/GLP1/inputs/other_parameters/uncertainty on bmi efficacy ------#
#----------------------------------------------------------------------------------------#
##########################################################################################
#library(data.table)
#library(fst)
#### Simulate uncertainty in bmi efficacy input at treatment year 1 -------------------------------
bmi_1y <- -0.149                 # Data from STEP 1 RCT paper (Sample size: N=1306 for semaglutide)
# ((13.37-12.44)/1.96)/100       # STEP 1 RCT paper, Table 2: 95% CI for difference between group: −12.44 (–13.37 to –11.51))
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

#bmi_1y_samples[tax_pth > 1, tax_pth := 1] --> I don't think we need a upper limit here

write_fst(bmi_1y_samples, "bmi_1y_samples.fst")
###################################################################################################
#### Simulate uncertainty in bmi efficacy input at treatment year 2 -------------------------------
bmi_2y <- -0.152                   # Data from STEP 5 RCT paper (Sample size: N=152 for semaglutide)
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

#tax_pth_samples[tax_pth > 1, tax_pth := 1]

write_fst(bmi_2y_samples, "bmi_2y_samples.fst")

###################################################################################################
#### Simulate uncertainty in bmi efficacy input at treatment year 3 ------------------------------
#### For the 1 year after treatment cessation:  regain 2/3 of weight loss
# mean: 11.6, se_2y: (7.7/(sqrt(197)))  ---> we can calculate a % of se on mean
(7.7/(sqrt(197)))/11.6                     # 4.7%
# mean: 17.3, se_1y: (9.3/(sqrt(228)))  ---> we can calculate a % of se on mean
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

#tax_pth_samples[tax_pth > 1, tax_pth := 1]

write_fst(bmi_3y_samples, "bmi_3y_samples.fst")

# Organice the uncertainty variables in a separate script (Jane today)