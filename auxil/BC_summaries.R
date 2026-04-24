
library(data.table)

###############################################################################################
#---------------------------------------------------------------------------------------------#
#------------------ Descriptive statistics for synthetic population---------------------------#
#---------------------------------------------------------------------------------------------#
#------------------------------ To loop through the sc0 folder -------------------------------#
#---------------------------------------------------------------------------------------------#
###############################################################################################
analysis_name <- "GLP_final_sc0"
# Path to your folder containing lifecourse tables
lc_dir <- paste0("/mnt/Storage_1/IMPACT_Storage/GLP1/outputs/", analysis_name, "/lifecourse")

# Output directory for summaries
out_dir <- paste0("/mnt/Storage_1/IMPACT_Storage/GLP1/inputs/BC_summary")

# Create output directory if it doesn't exist
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Get all lifecourse table files
lc_files <- list.files(lc_dir, pattern = "lifecourse.csv.gz$", full.names = TRUE)

## For testing, only loop through 2 files
## lc_files_test <- lc_files[1:2]

# Initialise empty lists to collect results across all iterations
overall      <- list()
age_summary  <- list()
bmi_summary  <- list()

# Loop over each lifecourse table
# for (i in seq_along(lc_files)) {
for (i in seq_along(lc_files)) {
  
  cat("Processing file", i, "of", length(lc_files), ":", basename(lc_files[i]), "\n")
  
  # Read the lifecourse table
  lc <- fread(lc_files[i])
  
  # Extract mc iteration number from filename (adjust pattern if needed)
  mc_i <- as.integer(gsub("\\D", "", basename(lc_files[i])))
  lc[, mc := mc_i]
  
  # ---- Age and BMI categories ------------------------------------------------
  lc[, age_cat := cut(age,
                      breaks = c(-Inf, 49, 69, 90),
                      labels = c("30-49", "50-69", "70-90"))]
  
  lc[, bmi_cat := cut(bmi_curr_xps,
                      breaks = c(-Inf, 30, 35, 40, Inf),
                      labels = c("<30", "30-35", "35-40", ">40"))]
  
  lc$bmi_cat <- factor(lc$bmi_cat, ordered = TRUE,
                       levels = c("<30", "30-35", "35-40", ">40"))
  
  # ---- Compute summaries -----------------------------------------------------
  
  overall[[i]]  <- lc[scenario == 'sc0' & year == 25, .(
    popsize      = sum(wt),
    mean_age     = weighted.mean(age, wt, na.rm = TRUE),
    mean_bmi     = weighted.mean(bmi_curr_xps, wt, na.rm = TRUE),
    mean_sbp     = weighted.mean(sbp_curr_xps, wt, na.rm = TRUE),
    mean_chol    = weighted.mean(tchol_curr_xps, wt, na.rm = TRUE),
    pct_male     = 100 * sum((sex == "men") * wt, na.rm = TRUE) / sum(wt),
    pct_t2dm     = 100 * sum((t2dm_prvl > 0) * wt, na.rm = TRUE) / sum(wt),
    pct_chd      = 100 * sum((chd_prvl > 0) * wt, na.rm = TRUE) / sum(wt),
    pct_stroke   = 100 * sum((stroke_prvl > 0) * wt, na.rm = TRUE) / sum(wt),
    pct_obesity  = 100 * sum((obesity_prvl > 0) * wt, na.rm = TRUE) / sum(wt)
  ), by = mc]
  
  age_summary[[i]]  <- lc[scenario == 'sc0' & year == 25, .(
    numerator   = sum(wt),
    denominator = sum(lc[scenario == 'sc0' & year == 25, wt]),
    pct         = 100 * sum(wt) / sum(lc[scenario == 'sc0' & year == 25, wt])
  ), by = c("mc", "age_cat")]
  
  bmi_summary[[i]]  <- lc[scenario == 'sc0' & year == 25, .(
    numerator   = sum(wt),
    denominator = sum(lc[scenario == 'sc0' & year == 25, wt]),
    pct         = 100 * sum(wt) / sum(lc[scenario == 'sc0' & year == 25, wt])
  ), by = c("mc", "bmi_cat")]
  
} # <-- loop ends here

# ---- Bind all iterations together ------------------------------------------
overall_all     <- rbindlist(overall)
age_summary_all <- rbindlist(age_summary)
bmi_summary_all <- rbindlist(bmi_summary)

# ---- Mean and 95% CI across mc iterations ----------------------------------

overall_ci <- overall_all[, .(
  mean_popsize      = mean(popsize,      na.rm = TRUE),
  lower_popsize     = quantile(popsize,      0.025, na.rm = TRUE),
  upper_popsize     = quantile(popsize,      0.975, na.rm = TRUE),
  mean_age          = mean(mean_age,     na.rm = TRUE),
  lower_age         = quantile(mean_age,     0.025, na.rm = TRUE),
  upper_age         = quantile(mean_age,     0.975, na.rm = TRUE),
  mean_bmi          = mean(mean_bmi,     na.rm = TRUE),
  lower_bmi         = quantile(mean_bmi,     0.025, na.rm = TRUE),
  upper_bmi         = quantile(mean_bmi,     0.975, na.rm = TRUE),
  mean_sbp          = mean(mean_sbp,     na.rm = TRUE),
  lower_sbp         = quantile(mean_sbp,     0.025, na.rm = TRUE),
  upper_sbp         = quantile(mean_sbp,     0.975, na.rm = TRUE),
  mean_chol         = mean(mean_chol,    na.rm = TRUE),
  lower_chol        = quantile(mean_chol,    0.025, na.rm = TRUE),
  upper_chol        = quantile(mean_chol,    0.975, na.rm = TRUE),
  mean_pct_male     = mean(pct_male,     na.rm = TRUE),
  lower_pct_male    = quantile(pct_male,     0.025, na.rm = TRUE),
  upper_pct_male    = quantile(pct_male,     0.975, na.rm = TRUE),
  mean_pct_t2dm     = mean(pct_t2dm,     na.rm = TRUE),
  lower_pct_t2dm    = quantile(pct_t2dm,     0.025, na.rm = TRUE),
  upper_pct_t2dm    = quantile(pct_t2dm,     0.975, na.rm = TRUE),
  mean_pct_chd      = mean(pct_chd,      na.rm = TRUE),
  lower_pct_chd     = quantile(pct_chd,      0.025, na.rm = TRUE),
  upper_pct_chd     = quantile(pct_chd,      0.975, na.rm = TRUE),
  mean_pct_stroke   = mean(pct_stroke,   na.rm = TRUE),
  lower_pct_stroke  = quantile(pct_stroke,   0.025, na.rm = TRUE),
  upper_pct_stroke  = quantile(pct_stroke,   0.975, na.rm = TRUE),
  mean_pct_obesity  = mean(pct_obesity,  na.rm = TRUE),
  lower_pct_obesity = quantile(pct_obesity,  0.025, na.rm = TRUE),
  upper_pct_obesity = quantile(pct_obesity,  0.975, na.rm = TRUE)
)]

age_summary_ci <- age_summary_all[, .(
  mean_pct  = mean(pct,  na.rm = TRUE),
  lower_pct = quantile(pct, 0.025, na.rm = TRUE),
  upper_pct = quantile(pct, 0.975, na.rm = TRUE)
), by = age_cat]

bmi_summary_ci <- bmi_summary_all[, .(
  mean_pct  = mean(pct,  na.rm = TRUE),
  lower_pct = quantile(pct, 0.025, na.rm = TRUE),
  upper_pct = quantile(pct, 0.975, na.rm = TRUE)
), by = bmi_cat]

# ---- Write to Excel ---------------------------------------------------------
library(openxlsx)

write.xlsx(list(
  "overall_raw"     = overall_all,
  "overall_ci"      = overall_ci,
  "age_summary_raw" = age_summary_all,
  "age_summary_ci"  = age_summary_ci,
  "bmi_summary_raw" = bmi_summary_all,
  "bmi_summary_ci"  = bmi_summary_ci
), file = file.path(out_dir, "baseline_char_summaries.xlsx"))

###############################################################################################
#---------------------------------------------------------------------------------------------#
#----------------- Descriptive statistics for eligible population in CEA ---------------------#
#---------------------------------------------------------------------------------------------#
#----------------------------- To loop through the cea folder --------------------------------#
#---------------------------------------------------------------------------------------------#
###############################################################################################
analysis_name <- "GLP_final_cea"
# Path to your folder containing lifecourse tables

lc_dir <- paste0("/mnt/Storage_1/IMPACT_Storage/GLP1/outputs/", analysis_name, "/lifecourse")

# Output directory for summaries
out_dir <- paste0("/mnt/Storage_1/IMPACT_Storage/GLP1/inputs/BC_summary")

# Create output directory if it doesn't exist
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Get all lifecourse table files
lc_files <- list.files(lc_dir, pattern = "lifecourse.csv.gz$", full.names = TRUE)

## For testing, only loop through 2 files
## lc_files_test <- lc_files[1:2]

# Initialise empty lists
overall_elig <- list()
age_elig     <- list()
bmi_elig     <- list()

# Loop over each lifecourse table
# for (i in seq_along(lc_files)) {
for (i in seq_along(lc_files)) {
  
  cat("Processing file", i, "of", length(lc_files), ":", basename(lc_files[i]), "\n")
  
  # Read the lifecourse table
  lc <- fread(lc_files[i])
  
  # Extract mc iteration number from filename (adjust pattern if needed)
  mc_i <- as.integer(gsub("\\D", "", basename(lc_files[i])))
  lc[, mc := mc_i]
  
  # ---- Age and BMI categories ------------------------------------------------
  lc[, age_cat := cut(age,
                      breaks = c(-Inf, 49, 69, 90),
                      labels = c("30-49", "50-69", "70-90"))]
  
  lc[, bmi_cat := cut(bmi_curr_xps,
                      breaks = c(-Inf, 30, 35, 40, Inf),
                      labels = c("<30", "30-35", "35-40", ">40"))]
  
  lc$bmi_cat <- factor(lc$bmi_cat, ordered = TRUE,
                       levels = c("<30", "30-35", "35-40", ">40"))
  
  # ---- Compute summaries -----------------------------------------------------
  
  overall_elig[[i]] <- lc[scenario == "sc0" & year == 25 & uptake_group == 1, .(
    popsize      = sum(wt),
    mean_age     = weighted.mean(age, wt, na.rm = TRUE),
    mean_bmi     = weighted.mean(bmi_curr_xps, wt, na.rm = TRUE),
    mean_sbp     = weighted.mean(sbp_curr_xps, wt, na.rm = TRUE),
    mean_chol    = weighted.mean(tchol_curr_xps, wt, na.rm = TRUE),
    pct_male     = 100 * sum((sex == "men") * wt, na.rm = TRUE) / sum(wt),
    pct_t2dm     = 100 * sum((t2dm_prvl > 0) * wt, na.rm = TRUE) / sum(wt),
    pct_chd      = 100 * sum((chd_prvl > 0) * wt, na.rm = TRUE) / sum(wt),
    pct_stroke   = 100 * sum((stroke_prvl > 0) * wt, na.rm = TRUE) / sum(wt),
    pct_obesity  = 100 * sum((obesity_prvl > 0) * wt, na.rm = TRUE) / sum(wt)
  ), by = mc]
  
  age_elig[[i]] <- lc[scenario == "sc0" & year == 25 & uptake_group == 1, .(
    numerator   = sum(wt),
    denominator = sum(lc[scenario == "sc0" & year == 25 & uptake_group == 1, wt]),
    pct         = 100 * sum(wt) / sum(lc[scenario == "sc0" & year == 25 & uptake_group == 1, wt])
  ), by = c("mc", "age_cat")]
  
  bmi_elig[[i]] <- lc[scenario == "sc0" & year == 25 & uptake_group == 1, .(
    numerator   = sum(wt),
    denominator = sum(lc[scenario == "sc0" & year == 25 & uptake_group == 1, wt]),
    pct         = 100 * sum(wt) / sum(lc[scenario == "sc0" & year == 25 & uptake_group == 1, wt])
  ), by = c("mc", "bmi_cat")]
  
} # <-- loop ends here

# ---- Bind all iterations together ------------------------------------------
overall_elig_all <- rbindlist(overall_elig)
age_elig_all     <- rbindlist(age_elig)
bmi_elig_all     <- rbindlist(bmi_elig)

# ---- Mean and 95% CI across mc iterations ----------------------------------

overall_elig_ci <- overall_elig_all[, .(
  mean_popsize      = mean(popsize,      na.rm = TRUE),
  lower_popsize     = quantile(popsize,      0.025, na.rm = TRUE),
  upper_popsize     = quantile(popsize,      0.975, na.rm = TRUE),
  mean_age          = mean(mean_age,     na.rm = TRUE),
  lower_age         = quantile(mean_age,     0.025, na.rm = TRUE),
  upper_age         = quantile(mean_age,     0.975, na.rm = TRUE),
  mean_bmi          = mean(mean_bmi,     na.rm = TRUE),
  lower_bmi         = quantile(mean_bmi,     0.025, na.rm = TRUE),
  upper_bmi         = quantile(mean_bmi,     0.975, na.rm = TRUE),
  mean_sbp          = mean(mean_sbp,     na.rm = TRUE),
  lower_sbp         = quantile(mean_sbp,     0.025, na.rm = TRUE),
  upper_sbp         = quantile(mean_sbp,     0.975, na.rm = TRUE),
  mean_chol         = mean(mean_chol,    na.rm = TRUE),
  lower_chol        = quantile(mean_chol,    0.025, na.rm = TRUE),
  upper_chol        = quantile(mean_chol,    0.975, na.rm = TRUE),
  mean_pct_male     = mean(pct_male,     na.rm = TRUE),
  lower_pct_male    = quantile(pct_male,     0.025, na.rm = TRUE),
  upper_pct_male    = quantile(pct_male,     0.975, na.rm = TRUE),
  mean_pct_t2dm     = mean(pct_t2dm,     na.rm = TRUE),
  lower_pct_t2dm    = quantile(pct_t2dm,     0.025, na.rm = TRUE),
  upper_pct_t2dm    = quantile(pct_t2dm,     0.975, na.rm = TRUE),
  mean_pct_chd      = mean(pct_chd,      na.rm = TRUE),
  lower_pct_chd     = quantile(pct_chd,      0.025, na.rm = TRUE),
  upper_pct_chd     = quantile(pct_chd,      0.975, na.rm = TRUE),
  mean_pct_stroke   = mean(pct_stroke,   na.rm = TRUE),
  lower_pct_stroke  = quantile(pct_stroke,   0.025, na.rm = TRUE),
  upper_pct_stroke  = quantile(pct_stroke,   0.975, na.rm = TRUE),
  mean_pct_obesity  = mean(pct_obesity,  na.rm = TRUE),
  lower_pct_obesity = quantile(pct_obesity,  0.025, na.rm = TRUE),
  upper_pct_obesity = quantile(pct_obesity,  0.975, na.rm = TRUE)
)]

age_elig_ci <- age_elig_all[, .(
  mean_numerator   = mean(numerator,   na.rm = TRUE),
  lower_numerator  = quantile(numerator,   0.025, na.rm = TRUE),
  upper_numerator  = quantile(numerator,   0.975, na.rm = TRUE),
  mean_denominator = mean(denominator, na.rm = TRUE),
  lower_denominator= quantile(denominator, 0.025, na.rm = TRUE),
  upper_denominator= quantile(denominator, 0.975, na.rm = TRUE),
  mean_pct         = mean(pct,         na.rm = TRUE),
  lower_pct        = quantile(pct,         0.025, na.rm = TRUE),
  upper_pct        = quantile(pct,         0.975, na.rm = TRUE)
), by = age_cat]

bmi_elig_ci <- bmi_elig_all[, .(
  mean_numerator   = mean(numerator,   na.rm = TRUE),
  lower_numerator  = quantile(numerator,   0.025, na.rm = TRUE),
  upper_numerator  = quantile(numerator,   0.975, na.rm = TRUE),
  mean_denominator = mean(denominator, na.rm = TRUE),
  lower_denominator= quantile(denominator, 0.025, na.rm = TRUE),
  upper_denominator= quantile(denominator, 0.975, na.rm = TRUE),
  mean_pct         = mean(pct,         na.rm = TRUE),
  lower_pct        = quantile(pct,         0.025, na.rm = TRUE),
  upper_pct        = quantile(pct,         0.975, na.rm = TRUE)
), by = bmi_cat]

# ---- Write to Excel ---------------------------------------------------------
library(openxlsx)

write.xlsx(list(
  "overall_elig_raw" = overall_elig_all,
  "overall_elig_ci"  = overall_elig_ci,
  "age_elig_raw"     = age_elig_all,
  "age_elig_ci"      = age_elig_ci,
  "bmi_elig_raw"     = bmi_elig_all,
  "bmi_elig_ci"      = bmi_elig_ci
), file = file.path(out_dir, "baseline_char_elig_summaries_cea.xlsx"))

###############################################################################################
#---------------------------------------------------------------------------------------------#
#---------------- Descriptive statistics for eligible population in BIA_N --------------------#
#---------------------------------------------------------------------------------------------#
#---------------------------- To loop through the bia_N folder -------------------------------#
#---------------------------------------------------------------------------------------------#
###############################################################################################
analysis_name <- "GLP_final_bia_num"
# Path to your folder containing lifecourse tables

lc_dir <- paste0("/mnt/Storage_1/IMPACT_Storage/GLP1/outputs/", analysis_name, "/lifecourse")

# Output directory for summaries
out_dir <- paste0("/mnt/Storage_1/IMPACT_Storage/GLP1/inputs/BC_summary")

# Create output directory if it doesn't exist
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Get all lifecourse table files
lc_files <- list.files(lc_dir, pattern = "lifecourse.csv.gz$", full.names = TRUE)

## For testing, only loop through 2 files
## lc_files_test <- lc_files[1:2]

# Initialise empty lists
overall_elig <- list()
age_elig     <- list()
bmi_elig     <- list()

# Loop over each lifecourse table
# for (i in seq_along(lc_files)) {
for (i in seq_along(lc_files)) {
  
  cat("Processing file", i, "of", length(lc_files), ":", basename(lc_files[i]), "\n")
  
  # Read the lifecourse table
  lc <- fread(lc_files[i])
  
  # Extract mc iteration number from filename (adjust pattern if needed)
  mc_i <- as.integer(gsub("\\D", "", basename(lc_files[i])))
  lc[, mc := mc_i]
  
  # ---- Age and BMI categories ------------------------------------------------
  lc[, age_cat := cut(age,
                      breaks = c(-Inf, 49, 69, 90),
                      labels = c("30-49", "50-69", "70-90"))]
  
  lc[, bmi_cat := cut(bmi_curr_xps,
                      breaks = c(-Inf, 30, 35, 40, Inf),
                      labels = c("<30", "30-35", "35-40", ">40"))]
  
  lc$bmi_cat <- factor(lc$bmi_cat, ordered = TRUE,
                       levels = c("<30", "30-35", "35-40", ">40"))
  
  # ---- Define the treatment year filter once to avoid repetition -------------
  # trtm_year <- lc[trtm_theo == 1, year][1] - 1
  
  
  # ---- Compute summaries -----------------------------------------------------
  
  overall_elig[[i]] <- lc[scenario == "sc0" & uptake_group == 2 & year == anchor_year, .(
    popsize      = sum(wt),
    mean_age     = weighted.mean(age, wt, na.rm = TRUE),
    mean_bmi     = weighted.mean(bmi_curr_xps, wt, na.rm = TRUE),
    mean_sbp     = weighted.mean(sbp_curr_xps, wt, na.rm = TRUE),
    mean_chol    = weighted.mean(tchol_curr_xps, wt, na.rm = TRUE),
    pct_male     = 100 * sum((sex == "men") * wt, na.rm = TRUE) / sum(wt),
    pct_t2dm     = 100 * sum((t2dm_prvl > 0) * wt, na.rm = TRUE) / sum(wt),
    pct_chd      = 100 * sum((chd_prvl > 0) * wt, na.rm = TRUE) / sum(wt),
    pct_stroke   = 100 * sum((stroke_prvl > 0) * wt, na.rm = TRUE) / sum(wt),
    pct_obesity  = 100 * sum((obesity_prvl > 0) * wt, na.rm = TRUE) / sum(wt)
  ), by = mc]
  
  age_elig[[i]] <- lc[scenario == "sc0" & uptake_group == 2 & year == anchor_year, .(
    numerator   = sum(wt),
    denominator = sum(lc[scenario == "sc0" & uptake_group == 2 & year == anchor_year, wt]),
    pct         = 100 * sum(wt) / sum(lc[scenario == "sc0" & uptake_group == 2 & year == anchor_year, wt])
  ), by = c("mc", "age_cat")]
  
  bmi_elig[[i]] <- lc[scenario == "sc0" & uptake_group == 2 & year == anchor_year, .(
    numerator   = sum(wt),
    denominator = sum(lc[scenario == "sc0" & uptake_group == 2 & year == anchor_year, wt]),
    pct         = 100 * sum(wt) / sum(lc[scenario == "sc0" & uptake_group == 2 & year == anchor_year, wt])
  ), by = c("mc", "bmi_cat")]
  
} # <-- loop ends here

# ---- Bind all iterations together ------------------------------------------
overall_elig_all <- rbindlist(overall_elig)
age_elig_all     <- rbindlist(age_elig)
bmi_elig_all     <- rbindlist(bmi_elig)

# ---- Mean and 95% CI across mc iterations ----------------------------------

overall_elig_ci <- overall_elig_all[, .(
  mean_popsize      = mean(popsize,      na.rm = TRUE),
  lower_popsize     = quantile(popsize,      0.025, na.rm = TRUE),
  upper_popsize     = quantile(popsize,      0.975, na.rm = TRUE),
  mean_age          = mean(mean_age,     na.rm = TRUE),
  lower_age         = quantile(mean_age,     0.025, na.rm = TRUE),
  upper_age         = quantile(mean_age,     0.975, na.rm = TRUE),
  mean_bmi          = mean(mean_bmi,     na.rm = TRUE),
  lower_bmi         = quantile(mean_bmi,     0.025, na.rm = TRUE),
  upper_bmi         = quantile(mean_bmi,     0.975, na.rm = TRUE),
  mean_sbp          = mean(mean_sbp,     na.rm = TRUE),
  lower_sbp         = quantile(mean_sbp,     0.025, na.rm = TRUE),
  upper_sbp         = quantile(mean_sbp,     0.975, na.rm = TRUE),
  mean_chol         = mean(mean_chol,    na.rm = TRUE),
  lower_chol        = quantile(mean_chol,    0.025, na.rm = TRUE),
  upper_chol        = quantile(mean_chol,    0.975, na.rm = TRUE),
  mean_pct_male     = mean(pct_male,     na.rm = TRUE),
  lower_pct_male    = quantile(pct_male,     0.025, na.rm = TRUE),
  upper_pct_male    = quantile(pct_male,     0.975, na.rm = TRUE),
  mean_pct_t2dm     = mean(pct_t2dm,     na.rm = TRUE),
  lower_pct_t2dm    = quantile(pct_t2dm,     0.025, na.rm = TRUE),
  upper_pct_t2dm    = quantile(pct_t2dm,     0.975, na.rm = TRUE),
  mean_pct_chd      = mean(pct_chd,      na.rm = TRUE),
  lower_pct_chd     = quantile(pct_chd,      0.025, na.rm = TRUE),
  upper_pct_chd     = quantile(pct_chd,      0.975, na.rm = TRUE),
  mean_pct_stroke   = mean(pct_stroke,   na.rm = TRUE),
  lower_pct_stroke  = quantile(pct_stroke,   0.025, na.rm = TRUE),
  upper_pct_stroke  = quantile(pct_stroke,   0.975, na.rm = TRUE),
  mean_pct_obesity  = mean(pct_obesity,  na.rm = TRUE),
  lower_pct_obesity = quantile(pct_obesity,  0.025, na.rm = TRUE),
  upper_pct_obesity = quantile(pct_obesity,  0.975, na.rm = TRUE)
)]

age_elig_ci <- age_elig_all[, .(
  mean_numerator    = mean(numerator,    na.rm = TRUE),
  lower_numerator   = quantile(numerator,    0.025, na.rm = TRUE),
  upper_numerator   = quantile(numerator,    0.975, na.rm = TRUE),
  mean_denominator  = mean(denominator,  na.rm = TRUE),
  lower_denominator = quantile(denominator,  0.025, na.rm = TRUE),
  upper_denominator = quantile(denominator,  0.975, na.rm = TRUE),
  mean_pct          = mean(pct,          na.rm = TRUE),
  lower_pct         = quantile(pct,          0.025, na.rm = TRUE),
  upper_pct         = quantile(pct,          0.975, na.rm = TRUE)
), by = age_cat]

bmi_elig_ci <- bmi_elig_all[, .(
  mean_numerator    = mean(numerator,    na.rm = TRUE),
  lower_numerator   = quantile(numerator,    0.025, na.rm = TRUE),
  upper_numerator   = quantile(numerator,    0.975, na.rm = TRUE),
  mean_denominator  = mean(denominator,  na.rm = TRUE),
  lower_denominator = quantile(denominator,  0.025, na.rm = TRUE),
  upper_denominator = quantile(denominator,  0.975, na.rm = TRUE),
  mean_pct          = mean(pct,          na.rm = TRUE),
  lower_pct         = quantile(pct,          0.025, na.rm = TRUE),
  upper_pct         = quantile(pct,          0.975, na.rm = TRUE)
), by = bmi_cat]

# ---- Write to Excel ---------------------------------------------------------
library(openxlsx)

write.xlsx(list(
  "overall_elig_raw" = overall_elig_all,
  "overall_elig_ci"  = overall_elig_ci,
  "age_elig_raw"     = age_elig_all,
  "age_elig_ci"      = age_elig_ci,
  "bmi_elig_raw"     = bmi_elig_all,
  "bmi_elig_ci"      = bmi_elig_ci
), file = file.path(out_dir, "baseline_char_elig_summaries_biaN.xlsx"))

#####################################
lc = fread("4_lifecourse.csv.gz")
table(lc$scenario)
test <- lc[scenario == "sc1" & uptake_group == 2]

###############################################################################################
#---------------------------------------------------------------------------------------------#
#--------------- Descriptive statistics for eligible population in BIA_pct -------------------#
#---------------------------------------------------------------------------------------------#
#---------------------------- To loop through the bia_pct folder -----------------------------#
#---------------------------------------------------------------------------------------------#
###############################################################################################

analysis_name <- "GLP_final_bia_perc"
# Path to your folder containing lifecourse tables

lc_dir <- paste0("/mnt/Storage_1/IMPACT_Storage/GLP1/outputs/", analysis_name, "/lifecourse")

# Output directory for summaries
out_dir <- paste0("/mnt/Storage_1/IMPACT_Storage/GLP1/inputs/BC_summary")

# Create output directory if it doesn't exist
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Get all lifecourse table files
lc_files <- list.files(lc_dir, pattern = "lifecourse.csv.gz$", full.names = TRUE)

## For testing, only loop through 2 files
## lc_files_test <- lc_files[1:2]

# Initialise empty lists
overall_elig <- list()
age_elig     <- list()
bmi_elig     <- list()

# Loop over each lifecourse table
# for (i in seq_along(lc_files)) {
for (i in seq_along(lc_files)) {
  
  cat("Processing file", i, "of", length(lc_files), ":", basename(lc_files[i]), "\n")
  
  # Read the lifecourse table
  lc <- fread(lc_files[i])
  
  # Extract mc iteration number from filename (adjust pattern if needed)
  mc_i <- as.integer(gsub("\\D", "", basename(lc_files[i])))
  lc[, mc := mc_i]
  
  # ---- Age and BMI categories ------------------------------------------------
  lc[, age_cat := cut(age,
                      breaks = c(-Inf, 49, 69, 90),
                      labels = c("30-49", "50-69", "70-90"))]
  
  lc[, bmi_cat := cut(bmi_curr_xps,
                      breaks = c(-Inf, 30, 35, 40, Inf),
                      labels = c("<30", "30-35", "35-40", ">40"))]
  
  lc$bmi_cat <- factor(lc$bmi_cat, ordered = TRUE,
                       levels = c("<30", "30-35", "35-40", ">40"))
  
  # ---- Define the treatment year filter once to avoid repetition -------------
  # trtm_year <- lc[trtm_theo == 1, year][1] - 1
  
  # ---- Compute summaries -----------------------------------------------------
  
  overall_elig[[i]] <- lc[scenario == "sc0" & uptake_group == 3 & year == anchor_year, .(
    popsize      = sum(wt),
    mean_age     = weighted.mean(age, wt, na.rm = TRUE),
    mean_bmi     = weighted.mean(bmi_curr_xps, wt, na.rm = TRUE),
    mean_sbp     = weighted.mean(sbp_curr_xps, wt, na.rm = TRUE),
    mean_chol    = weighted.mean(tchol_curr_xps, wt, na.rm = TRUE),
    pct_male     = 100 * sum((sex == "men") * wt, na.rm = TRUE) / sum(wt),
    pct_t2dm     = 100 * sum((t2dm_prvl > 0) * wt, na.rm = TRUE) / sum(wt),
    pct_chd      = 100 * sum((chd_prvl > 0) * wt, na.rm = TRUE) / sum(wt),
    pct_stroke   = 100 * sum((stroke_prvl > 0) * wt, na.rm = TRUE) / sum(wt),
    pct_obesity  = 100 * sum((obesity_prvl > 0) * wt, na.rm = TRUE) / sum(wt)
  ), by = mc]
  
  age_elig[[i]] <- lc[scenario == "sc0" & uptake_group == 3 & year == anchor_year, .(
    numerator   = sum(wt),
    denominator = sum(lc[scenario == "sc0" & uptake_group == 3 & year == anchor_year, wt]),
    pct         = 100 * sum(wt) / sum(lc[scenario == "sc0" & uptake_group == 3 & year == anchor_year, wt])
  ), by = c("mc", "age_cat")]
  
  bmi_elig[[i]] <- lc[scenario == "sc0" & uptake_group == 3 & year == anchor_year, .(
    numerator   = sum(wt),
    denominator = sum(lc[scenario == "sc0" & uptake_group == 3 & year == anchor_year, wt]),
    pct         = 100 * sum(wt) / sum(lc[scenario == "sc0" & uptake_group == 3 & year == anchor_year, wt])
  ), by = c("mc", "bmi_cat")]
  
} # <-- loop ends here

# ---- Bind all iterations together ------------------------------------------
overall_elig_all <- rbindlist(overall_elig)
age_elig_all     <- rbindlist(age_elig)
bmi_elig_all     <- rbindlist(bmi_elig)

# ---- Mean and 95% CI across mc iterations ----------------------------------

overall_elig_ci <- overall_elig_all[, .(
  mean_popsize      = mean(popsize,      na.rm = TRUE),
  lower_popsize     = quantile(popsize,      0.025, na.rm = TRUE),
  upper_popsize     = quantile(popsize,      0.975, na.rm = TRUE),
  mean_age          = mean(mean_age,     na.rm = TRUE),
  lower_age         = quantile(mean_age,     0.025, na.rm = TRUE),
  upper_age         = quantile(mean_age,     0.975, na.rm = TRUE),
  mean_bmi          = mean(mean_bmi,     na.rm = TRUE),
  lower_bmi         = quantile(mean_bmi,     0.025, na.rm = TRUE),
  upper_bmi         = quantile(mean_bmi,     0.975, na.rm = TRUE),
  mean_sbp          = mean(mean_sbp,     na.rm = TRUE),
  lower_sbp         = quantile(mean_sbp,     0.025, na.rm = TRUE),
  upper_sbp         = quantile(mean_sbp,     0.975, na.rm = TRUE),
  mean_chol         = mean(mean_chol,    na.rm = TRUE),
  lower_chol        = quantile(mean_chol,    0.025, na.rm = TRUE),
  upper_chol        = quantile(mean_chol,    0.975, na.rm = TRUE),
  mean_pct_male     = mean(pct_male,     na.rm = TRUE),
  lower_pct_male    = quantile(pct_male,     0.025, na.rm = TRUE),
  upper_pct_male    = quantile(pct_male,     0.975, na.rm = TRUE),
  mean_pct_t2dm     = mean(pct_t2dm,     na.rm = TRUE),
  lower_pct_t2dm    = quantile(pct_t2dm,     0.025, na.rm = TRUE),
  upper_pct_t2dm    = quantile(pct_t2dm,     0.975, na.rm = TRUE),
  mean_pct_chd      = mean(pct_chd,      na.rm = TRUE),
  lower_pct_chd     = quantile(pct_chd,      0.025, na.rm = TRUE),
  upper_pct_chd     = quantile(pct_chd,      0.975, na.rm = TRUE),
  mean_pct_stroke   = mean(pct_stroke,   na.rm = TRUE),
  lower_pct_stroke  = quantile(pct_stroke,   0.025, na.rm = TRUE),
  upper_pct_stroke  = quantile(pct_stroke,   0.975, na.rm = TRUE),
  mean_pct_obesity  = mean(pct_obesity,  na.rm = TRUE),
  lower_pct_obesity = quantile(pct_obesity,  0.025, na.rm = TRUE),
  upper_pct_obesity = quantile(pct_obesity,  0.975, na.rm = TRUE)
)]

age_elig_ci <- age_elig_all[, .(
  mean_numerator    = mean(numerator,    na.rm = TRUE),
  lower_numerator   = quantile(numerator,    0.025, na.rm = TRUE),
  upper_numerator   = quantile(numerator,    0.975, na.rm = TRUE),
  mean_denominator  = mean(denominator,  na.rm = TRUE),
  lower_denominator = quantile(denominator,  0.025, na.rm = TRUE),
  upper_denominator = quantile(denominator,  0.975, na.rm = TRUE),
  mean_pct          = mean(pct,          na.rm = TRUE),
  lower_pct         = quantile(pct,          0.025, na.rm = TRUE),
  upper_pct         = quantile(pct,          0.975, na.rm = TRUE)
), by = age_cat]

bmi_elig_ci <- bmi_elig_all[, .(
  mean_numerator    = mean(numerator,    na.rm = TRUE),
  lower_numerator   = quantile(numerator,    0.025, na.rm = TRUE),
  upper_numerator   = quantile(numerator,    0.975, na.rm = TRUE),
  mean_denominator  = mean(denominator,  na.rm = TRUE),
  lower_denominator = quantile(denominator,  0.025, na.rm = TRUE),
  upper_denominator = quantile(denominator,  0.975, na.rm = TRUE),
  mean_pct          = mean(pct,          na.rm = TRUE),
  lower_pct         = quantile(pct,          0.025, na.rm = TRUE),
  upper_pct         = quantile(pct,          0.975, na.rm = TRUE)
), by = bmi_cat]

# ---- Write to Excel ---------------------------------------------------------
library(openxlsx)

write.xlsx(list(
  "overall_elig_raw" = overall_elig_all,
  "overall_elig_ci"  = overall_elig_ci,
  "age_elig_raw"     = age_elig_all,
  "age_elig_ci"      = age_elig_ci,
  "bmi_elig_raw"     = bmi_elig_all,
  "bmi_elig_ci"      = bmi_elig_ci
), file = file.path(out_dir, "baseline_char_elig_summaries_biapct.xlsx"))

