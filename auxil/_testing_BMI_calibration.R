
### TEST BMI CALIBRATION ON SP ###

## Load SynthPops for calibration
sps <- grep(".fst", list.files("./inputs/synthpop/"), value = TRUE)

## Load NAKO BMI means for calibration
bmi_clbr <- qread("./inputs/exposure_distributions/bmi_calibration.qs")
bmi_clbr[, sex := as.character(sex)][, sex := ifelse(sex == "male", "men", "women")]

setkey(bmi_clbr, sex, age)

bmi_clbr_fctr_list <- data.table(NULL)

for(i in 1:length(sps)){

  sp <- read_fst(paste0("./inputs/synthpop/", sps[i]), as.data.table = TRUE) # Read SynthPop
  
  bmi_sp <- sp[year == 13, .(bmi_sp_mean = mean(bmi_curr_xps)), by = .(age, sex)] # Summarize mean BMI

  setkey(bmi_sp, sex, age) # Sort dataset
  
  absorb_dt(bmi_sp, bmi_clbr) # Merge with BMI means from NAKO
  
  bmi_sp[, bmi_clbr_fctr := bmi_wtd_mean / bmi_sp_mean] # Calculate calibration factor as ratio of NAKO / SynthPop
  
  m_interp <- lm(bmi_clbr_fctr ~ age + sex, data = bmi_sp) # Interpolate calibration for ages not in NAKO

  bmi_sp_interp <- bmi_sp[is.na(bmi_clbr_fctr),
                          bmi_clbr_fctr := predict(m_interp, newdata = .SD),
                          .SDcol = c("age", "sex")] # Insert interpolated calibration values
  
  bmi_sp_interp[, c("bmi_sp_mean", "bmi_wtd_mean") := NULL] 
  
  bmi_sp_interp[, mc := i] # Add mc
  
  bmi_clbr_fctr_list <- rbind(bmi_clbr_fctr_list, bmi_sp_interp) # Bind to list
  
}

bmi_clbr_fctr_mean <- bmi_clbr_fctr_list[, .(bmi_clbr_fctr = mean(bmi_clbr_fctr)), by = .(age, sex)] # Average over all mc's


### REPLICATE SYNTHPOP BMI SAMPLING TO CHECK CALIBRATION

out_mean_list <- data.table(NULL)
out_original_elig_list <- data.table(NULL)
out_new_elig_list <- data.table(NULL)
out_clbr_elig_list <- data.table(NULL)

for(i in 1:length(sps)){
  
  new_n <- nrow(sp)
  
  # Generate correlated ranks for the individuals ----
  cm_mean <- as.matrix(
    read_fst(
      "./inputs/exposure_distributions/exposure_corr.fst",
      as.data.table = TRUE
    ),
    rownames = "rn"
  )
  
  rank_mtx <- generate_corr_unifs(new_n, cm_mean)
  
  rank_mtx <- rank_mtx * 0.999
  rank_mtx[, "bmi_r"] <-
    rank_mtx[, "bmi_r"] #* 0.90 / 0.999
  
  rank_mtx <- data.table(rank_mtx)
  
  sp[, c(
    "rank_bmi",
    "rank_sbp",
    "rank_tchol"
  ) := rank_mtx]
  
  setkeyv(sp, c("pid", "year"))
  setindexv(sp, c("year", "age", "sex")) #STRATA
  
  sp[, pid_mrk := mk_new_simulant_markers(pid)]
  
  sp[, lapply(.SD,
              fscramble_trajectories,
              pid_mrk),
     .SDcols = patterns("^rank_")]
  
  tbl <-
    read_fst("./inputs/exposure_distributions/bmi_table.fst",
             as.data.table = TRUE)
  
  tbl <- absorb_dt(tbl, bmi_clbr_fctr_mean)
  
  col_nam <-
    setdiff(names(tbl), intersect(names(sp), names(tbl)))
  
  sp <- absorb_dt(sp, tbl)
  
  # Create new test BMI for comparison before calibration
  # Comparison with bmi_curr_xps is not valid because of different quantiles
  sp[, bmi_test := qBCPEo(rank_bmi, mu, sigma, nu, tau)] ## Jane: why 'my_qBCPEo()'?
  sp[bmi_test > 80, bmi_test := 80] #Truncate BMI predictions to avoid unrealistic values.
  
  # Create calibrated BMI using the same rank
  sp[, mu := mu * bmi_clbr_fctr]
  sp[, bmi_clbr := qBCPEo(rank_bmi, mu, sigma, nu, tau)] ## Jane: why 'my_qBCPEo()'?
  sp[bmi_clbr > 80, bmi_clbr := 80] #Truncate BMI predictions to avoid unrealistic values.
  
  sp[, rank_bmi := NULL]
  sp[, (col_nam) := NULL]
  
  sp[, `:=` (
    pid_mrk = NULL
  )]

  sp[year == 13, `:=`(
    original_elig = fcase(
      bmi_curr_xps <  30,              'not eligible',       
      bmi_curr_xps >= 30 & bmi_curr_xps < 35,   'class 1', 
      bmi_curr_xps >= 35 & bmi_curr_xps < 40,   'class 2', 
      bmi_curr_xps >= 40,              'class 3'   
    ),
    new_elig = fcase(
      bmi_test <  30,              'not eligible',       
      bmi_test >= 30 & bmi_test < 35,   'class 1', 
      bmi_test >= 35 & bmi_test < 40,   'class 2', 
      bmi_test >= 40,              'class 3'   
    ),
    clbr_elig = fcase(
      bmi_clbr <  30,              'not eligible',       
      bmi_clbr >= 30 & bmi_clbr < 35,   'class 1', 
      bmi_clbr >= 35 & bmi_clbr < 40,   'class 2', 
      bmi_clbr >= 40,              'class 3'   
    ))
  ]
  
  out_mean <- sp[year == 13, .(original_mean = mean(bmi_curr_xps),
                               new_mean = mean(bmi_test),
                               clbr_mean = mean(bmi_clbr)), by = .(age, sex)][, mc := i]
  
  out_original_elig <- sp[year == 13, .(n = .N), by = .(sex, original_elig)][, pct := n / sum(n) * 100, by = sex][, mc := i]
  out_new_elig <- sp[year == 13, .(n = .N), by = .(sex, new_elig)][, pct := n / sum(n) * 100, by = sex][, mc := i]
  out_clbr_elig <- sp[year == 13, .(n = .N), by = .(sex, clbr_elig)][, pct := n / sum(n) * 100, by = sex][, mc := i]
  
  out_mean_list <- rbind(out_mean_list, out_mean)
  out_original_elig_list <- rbind(out_original_elig_list, out_original_elig)
  out_new_elig_list <- rbind(out_new_elig_list, out_new_elig)
  out_clbr_elig_list <- rbind(out_clbr_elig_list, out_clbr_elig)

}

out_bmi_mean <- out_mean_list[, .(original_bmi_mean = mean(original_mean),
                                  new_bmi_mean = mean(new_mean),
                                  clbr_bmi_mean = mean(clbr_mean)), by = .(age, sex)]

original_elig_mean <- out_original_elig_list[, .(pct_original = mean(pct)), by = .(sex, original_elig)]
new_elig_mean <- out_new_elig_list[, .(pct_new = mean(pct)), by = .(sex, new_elig)]
clbr_elig_mean <- out_clbr_elig_list[, .(pct_clbr = mean(pct)), by = .(sex, clbr_elig)]







# ################################################################################
# ######### TEST BMI ON LIFECOURSE INCLUDING WEIGHT ###
# 
# lcs <- grep(".csv", list.files("./outputs/GLP_Dggoe/lifecourse/"), value = TRUE)
# 
# lc <- fread(paste0("./outputs/GLP_Dggoe/lifecourse/", lcs[n]))
# 
# lc[, `:=`(
#   bmi_group = fcase(
#     bmi_curr_xps <  18.5,            'Underweight',       
#     bmi_curr_xps >= 18.5 & bmi_curr_xps < 25, 'Normal', 
#     bmi_curr_xps >= 25 & bmi_curr_xps < 30,   'Overweight', 
#     bmi_curr_xps >= 30,              'Obesity'   
#   ))
# ]
# lc[, .(weighted_n = sum(wt)), by = .(sex, bmi_group)][
#   , weighted_pct := weighted_n / sum(weighted_n) * 100, by = sex][
#     order(sex, bmi_group)] 
# 
# lc[, `:=`(
#   bmi_elig = fcase(
#     bmi_curr_xps <  30,              'not eligible',       
#     bmi_curr_xps >= 30 & bmi_curr_xps < 35,   'class 1', 
#     bmi_curr_xps >= 35 & bmi_curr_xps < 40,   'class 2', 
#     bmi_curr_xps >= 40,              'class 3'   
#   ))
# ]
# lc[, .(weighted_n = sum(wt)), by = .(sex, bmi_elig)][
#   , weighted_pct := weighted_n / sum(weighted_n) * 100, by = sex][
#     order(sex, bmi_elig)] 
# 
# lc[, mean(bmi_curr_xps)]
# lc[, weighted.mean(bmi_curr_xps, wt)]
# 
# lc[, mean(bmi_curr_xps), by = sex]
# lc[, weighted.mean(bmi_curr_xps, wt), by = sex]
