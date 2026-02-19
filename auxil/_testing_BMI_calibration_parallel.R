library(future.apply)
plan(multisession, workers = 16)

## Load SynthPops for calibration
sps <- grep(".fst", list.files("./inputs/synthpop/"), value = TRUE)

sps <- sps[1:100]

## Load NAKO BMI means for calibration
bmi_clbr <- qread("./inputs/exposure_distributions/bmi_calibration.qs")
bmi_clbr[, sex := as.character(sex)][, sex := ifelse(sex == "male", "men", "women")]
setkey(bmi_clbr, sex, age)

### LOOP 1: BMI CALIBRATION FACTORS ###
bmi_clbr_fctr_list <- future_lapply(seq_along(sps), function(i) {
  
  sp <- read_fst(paste0("./inputs/synthpop/", sps[i]), as.data.table = TRUE)
  
  bmi_sp <- sp[year == 13, .(bmi_sp_mean = mean(bmi_curr_xps)), by = .(age, sex)]
  setkey(bmi_sp, sex, age)
  absorb_dt(bmi_sp, bmi_clbr)
  
  bmi_sp[, bmi_clbr_fctr := bmi_wtd_mean / bmi_sp_mean]
  
  m_interp <- lm(bmi_clbr_fctr ~ age + sex, data = bmi_sp)
  
  bmi_sp[is.na(bmi_clbr_fctr),
         bmi_clbr_fctr := predict(m_interp, newdata = .SD),
         .SDcol = c("age", "sex")]
  
  bmi_sp[, c("bmi_sp_mean", "bmi_wtd_mean") := NULL]
  bmi_sp[, mc := i]
  
  bmi_sp
  
}, future.seed = TRUE, future.packages = c("data.table", "fst", "gamlss", "qs", "CKutils", "dqrng", "IMPACTncdGer"))

bmi_clbr_fctr_list <- rbindlist(bmi_clbr_fctr_list)
bmi_clbr_fctr_mean <- bmi_clbr_fctr_list[, .(bmi_clbr_fctr = mean(bmi_clbr_fctr)), by = .(age, sex)]


### LOOP 2: BMI SAMPLING AND ELIGIBILITY ###

# Load objects that are read-only and shared across workers
cm_mean <- as.matrix(
  read_fst("./inputs/exposure_distributions/exposure_corr.fst", as.data.table = TRUE),
  rownames = "rn"
)
tbl_base <- read_fst("./inputs/exposure_distributions/bmi_table.fst", as.data.table = TRUE)
tbl_base <- absorb_dt(tbl_base, bmi_clbr_fctr_mean)

results_list <- future_lapply(seq_along(sps), function(i) {
  
  sp <- read_fst(paste0("./inputs/synthpop/", sps[i]), as.data.table = TRUE)
  new_n <- nrow(sp)
  
  rank_mtx <- generate_corr_unifs(new_n, cm_mean)
  rank_mtx <- rank_mtx * 0.999
  rank_mtx[, "bmi_r"] <- rank_mtx[, "bmi_r"] #* 0.90 / 0.999
  rank_mtx <- data.table(rank_mtx)
  
  sp[, c("rank_bmi", "rank_sbp", "rank_tchol") := rank_mtx]
  
  setkeyv(sp, c("pid", "year"))
  setindexv(sp, c("year", "age", "sex"))
  
  sp[, pid_mrk := mk_new_simulant_markers(pid)]
  sp[, lapply(.SD, fscramble_trajectories, pid_mrk), .SDcols = patterns("^rank_")]
  
  tbl <- copy(tbl_base)  # copy to avoid cross-worker modification
  col_nam <- setdiff(names(tbl), intersect(names(sp), names(tbl)))
  sp <- absorb_dt(sp, tbl)
  
  sp[, bmi_test := qBCPEo(rank_bmi, mu, sigma, nu, tau)]
  sp[bmi_test > 80, bmi_test := 80]
  
  #sp[, mu := mu * (bmi_clbr_fctr * 0.98)]
  sp[, bmi_clbr := qBCPEo(rank_bmi, mu, sigma, nu, tau)]
  sp[bmi_clbr > 80, bmi_clbr := 80]
  
  sp[, rank_bmi := NULL]
  sp[, (col_nam) := NULL]
  sp[, pid_mrk := NULL]
  
  sp[year == 25 & age <= 72, `:=`(
    original_elig = fcase(
      bmi_curr_xps <  30,                              'not eligible',       
      bmi_curr_xps >= 30 & bmi_curr_xps < 35,   'class 1', 
      bmi_curr_xps >= 35 & bmi_curr_xps < 40,   'class 2', 
      bmi_curr_xps >= 40,                              'class 3'   
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
  
  list(
    mean    = sp[year == 25  & age <= 72, .(original_mean = mean(bmi_curr_xps),
                               new_mean      = mean(bmi_test),
                               clbr_mean     = mean(bmi_clbr)), by = .(age, sex)][, mc := i],
    original_elig = sp[year == 25 & age <= 72, .(n = .N), by = .(sex, original_elig)][, pct := n / sum(n) * 100, by = sex][, mc := i],
    new_elig      = sp[year == 25 & age <= 72, .(n = .N), by = .(sex, new_elig)][, pct := n / sum(n) * 100, by = sex][, mc := i],
    clbr_elig     = sp[year == 25 & age <= 72, .(n = .N), by = .(sex, clbr_elig)][, pct := n / sum(n) * 100, by = sex][, mc := i]
  )
  
}, future.seed = TRUE, future.packages = c("data.table", "fst", "gamlss", "qs", "CKutils", "dqrng", "IMPACTncdGer"))

# Unbind results
out_mean_list          <- rbindlist(lapply(results_list, `[[`, "mean"))
out_original_elig_list <- rbindlist(lapply(results_list, `[[`, "original_elig"))
out_new_elig_list      <- rbindlist(lapply(results_list, `[[`, "new_elig"))
out_clbr_elig_list     <- rbindlist(lapply(results_list, `[[`, "clbr_elig"))

# Aggregate
out_bmi_mean       <- out_mean_list[, .(original_bmi_mean = mean(original_mean),
                                        new_bmi_mean      = mean(new_mean),
                                        clbr_bmi_mean     = mean(clbr_mean)), by = .(age, sex)]

original_elig_mean <- out_original_elig_list[, .(pct_original = mean(pct)), by = .(sex, original_elig)]
new_elig_mean      <- out_new_elig_list[,      .(pct_new      = mean(pct)), by = .(sex, new_elig)]
clbr_elig_mean     <- out_clbr_elig_list[,     .(pct_clbr     = mean(pct)), by = .(sex, clbr_elig)]

