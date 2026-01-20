
#### Process uncertainty outputs ## ----

library(data.table)
library(CKutils)
library(ggplot2)
library(ggthemes)
library(scales)
library(gamlss)
library(fst)

# Note: Analyses are nested in the output folder!
if(Sys.info()["sysname"] == "Windows"){
  dirs <- list.dirs("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Model/IMPACT-NCD-Germany/outputs",
                    recursive = FALSE, full.names = FALSE)
} else {
  dirs <- list.dirs("/media/php-workstation/Storage_1/IMPACT_Storage/GLP1/outputs/",
                    recursive = FALSE, full.names = FALSE)
}
dirs <- dirs[!(dirs %in% c("Test", "manuscript", "appendix"))]
 
# Export options:
plot_format <- "png" # File format for plots

theme_set(new = theme_hc()) # ggplot theme that is applied to plots
theme_update(axis.text.x = element_text(size = 9),
             plot.title = element_text(hjust = 0.5),
             axis.title.y = element_text(angle = 90))

prbl = c(0.5, 0.025, 0.975, 0.1, 0.9) # Quantiles for uncertainty of results

sc_map <- data.table(
  scenario = c("sc2","sc3","sc4","sc5",
               "sc7","sc8","sc9","sc10",
               "sc12","sc13","sc14","sc15"),
  baseline = c(rep("sc1", 4),
               rep("sc6", 4),
               rep("sc11", 4))
)

for(analysis in dirs){

    if(!Sys.info()[1] == "Windows"){

      # Input path for IMPACT results
      in_path <- paste0("/media/php-workstation/Storage_1/IMPACT_Storage/GLP1/outputs/", analysis, "/summaries/")

      # Output path for tables
      out_path_tables <- paste0("/media/php-workstation/Storage_1/IMPACT_Storage/GLP1/outputs/", analysis, "/tables/")

      # Output path for plots
      out_path_plots <- paste0("/media/php-workstation/Storage_1/IMPACT_Storage/GLP1/outputs/", analysis, "/plots/")

    } else {

      # Input path for IMPACT results
      in_path <- paste0("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Model/IMPACT-NCD-Germany/outputs/",
                        analysis, "/summaries/")


      # Output path for tables
      if(!file.exists(paste0("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Model/IMPACT-NCD-Germany/outputs/",
                             analysis, "/tables/"))){
        dir.create(paste0("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Model/IMPACT-NCD-Germany/outputs/",
                          analysis, "/tables/"))
      } # Only created on Windows because always created on Linux!

      out_path_tables <- paste0("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Model/IMPACT-NCD-Germany/outputs/",
                                analysis, "/tables/")

      # Output path for plots
      if(!file.exists(paste0("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Model/IMPACT-NCD-Germany/outputs/",
                             analysis, "/plots/"))){
        dir.create(paste0("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Model/IMPACT-NCD-Germany/outputs/",
                          analysis, "/plots/"))
      } # Only created on Windows because always created on Linux!

      out_path_plots <- paste0("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Model/IMPACT-NCD-Germany/outputs/",
                               analysis, "/plots/")

    }

  ####################################################################################################################
  #------------------------------------------   Jane, Test, 09.Jan.2026   -------------------------------------------#
  ####################################################################################################################

    if("prvl_scaled_up.csv.gz" %in% list.files(in_path)){

        ## Prevalence by age and sex ## ----

        tt <- fread(paste0(in_path, "prvl_scaled_up.csv.gz")
        #tt <- fread("prvl_scaled_up.csv.gz"
        )[, `:=` (year = year + 2000,
                  agegrp = fifelse(agegrp %in% c("30-34", "35-39", "40-44", "45-49"), "30-49",
                                   ifelse(agegrp %in% c("50-54", "55-59", "60-64", "65-69"), "50-69",
                                          "70-90")))]

        outstrata <- c("mc", "year", "sex", "agegrp", "scenario", "uptake_group")

        sc_n <- na.omit(as.numeric(gsub("[^1-9]+", "", unique(tt$scenario))))

        # Rate # --- I don't think this this needs to be change for the GLP1 analysis, Jane

        d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)
        ][, lapply(.SD, function(x) x/popsize), keyby = outstrata]
        d <- melt(d, id.vars = outstrata)
        d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
        setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "prvl_rate_")))

        fwrite(d, paste0(out_path_tables, "prevalence_rate_by_year_agegrp_sex.csv"), sep = ";")

        ggplot(d[disease != "popsize"], aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                                            ymax = `prvl_rate_97.5%`,
                                            colour = scenario, fill = scenario,
                                            linetype = agegrp)) +
          facet_wrap(~ disease + sex, scales = "free") +
          geom_ribbon(alpha = 0.5/5, colour = NA) +
          geom_line() +
          scale_x_continuous(name = "Year") +
          scale_y_continuous(name = "Prevalence") +
          ggtitle("Prevalence rate of diseases by scenario over time") +
         expand_limits(y = 0) +
          theme(legend.title = element_blank())

        ggsave(paste0(out_path_plots, "prevalence_rate_by_year_agegrp_sex.", plot_format),
               height = 9, width = 16)

        # Rate difference # --- I think this this needs to be change for the GLP1 analysis, Jane

        d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)
        ][, lapply(.SD, function(x) x/popsize), keyby = outstrata]

        prvls <- grep("_prvl", names(d), value = TRUE)

        d <- melt(d, id.vars = outstrata)
        d <- dcast(d, mc + year + agegrp + sex + uptake_group ~ scenario + variable)

        # --- Starting here: New, scenario-specific difference calculation, Jane
        # diffs0 <- grep("sc0_", names(d), value = TRUE)[-1]
        # What this line does: Find all column names in 'd' that belong to a specific scenario "sc0_"
        # This will return, like:"sc2_t2dm_prvl","sc2_chd_prvl"
        # grep(..., value=TRUE): grep() searches for matches;
        # 'value = TRUE' return the matching names, not their positions

        ####################################################################################################
        #------------##############--------------###############--------------################-------------#
        ####################################################################################################

        d_out <- data.table(NULL)

        for (k in seq_len(nrow(sc_map))) {

          sc <- sc_map$scenario[k]
          bl <- sc_map$baseline[k]

          sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
          bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

          if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

          # Safety check: same variables, same order
          stopifnot(
            sub(paste0("^", sc), "", sc_cols) ==
              sub(paste0("^", bl), "", bl_cols)
          )

          dd <- copy(d)

          # Compute differences
          for (i in seq_along(sc_cols)) {
            diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
            dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
          }

          # Keep only needed columns
          keep_cols <- c(
            "mc", "year", "sex", "agegrp", "uptake_group",
            grep("^diff_", names(dd), value = TRUE)
          )
          dd <- dd[, ..keep_cols]

          # 5. Uncertainty summarisation
          dd <- melt(
            dd,
            id.vars = c("mc", "year", "sex", "agegrp", "uptake_group")
          )

          dd <- dd[
            ,
            fquantile_byid(value, prbl, id = as.character(variable)),
            keyby = c("year","sex","agegrp","uptake_group")
          ]

          setnames(
            dd,
            c("year", "sex", "agegrp", "uptake_group", "disease",
              percent(prbl, prefix = "prvl_rate_"))
          )

          dd[, scenario := sc]

          d_out <- rbind(d_out, dd, use.names = TRUE)

          # Clean structurally empty rows
          stat_cols <- grep("%$", names(d_out), value = TRUE)
          d_out <- na.omit(d_out, cols=stat_cols)

        }
        ####################################################################################################
        #------------##############--------------###############--------------################-------------#
        ####################################################################################################

        fwrite(d_out, paste0(out_path_tables, "prevalence_rate_diff_by_year_agegrp_sex.csv"), sep = ";")

        ggplot(d_out, aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                          ymax = `prvl_rate_97.5%`,
                          colour = scenario, fill = scenario,
                          linetype = agegrp)) +
          facet_wrap(~ disease + sex, scales = "free") +
          geom_ribbon(alpha = 0.5/5, colour = NA) +
          geom_line() +
          scale_x_continuous(name = "Year") +
          scale_y_continuous(name = "Prevalence") +
          ggtitle("Difference in prevalence rate of diseases compared to baseline over time") +
          expand_limits(y = 0) +
          theme(legend.title = element_blank())

        ggsave(paste0(out_path_plots, "prevalence_rate_diff_by_year_agegrp_sex.", plot_format),
               height = 9, width = 16)

        # Absolute numbers #

        d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]
        d <- melt(d, id.vars = outstrata)
        d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
        setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "prvl_rate_")))

        fwrite(d, paste0(out_path_tables, "prevalence_numbers_by_year_agegrp_sex.csv"), sep = ";")

        ggplot(d[disease != "popsize"], aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                                            ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario,
                                            linetype = agegrp)) +
          facet_wrap(~ disease + sex, scales = "free") +
          geom_ribbon(alpha = 0.5/5, colour = NA) +
          geom_line() +
          scale_x_continuous(name = "Year") +
          scale_y_continuous(name = "Prevalence") +
          ggtitle("Prevalence numbers of diseases by scenario over time") +
          expand_limits(y = 0) +
          theme(legend.title = element_blank())

        ggsave(paste0(out_path_plots, "prevalence_numbers_by_year_agegrp_sex.", plot_format),
               height = 9, width = 16)

        # Absolute difference #

        d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]

        prvls <- grep("_prvl", names(d), value = TRUE)

        d <- melt(d, id.vars = outstrata)
        d <- dcast(d, mc + year + agegrp + sex + uptake_group ~ scenario + variable)

        d_out <- data.table(NULL)

        for (k in seq_len(nrow(sc_map))) {

          sc <- sc_map$scenario[k]
          bl <- sc_map$baseline[k]

          sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
          bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

          if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

          # Safety check: same variables, same order
          stopifnot(
            sub(paste0("^", sc), "", sc_cols) ==
              sub(paste0("^", bl), "", bl_cols)
          )

          dd <- copy(d)

          # Compute differences
          for (i in seq_along(sc_cols)) {
            diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
            dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
          }

          # Keep only needed columns
          keep_cols <- c(
            "mc", "year", "sex", "agegrp", "uptake_group",
            grep("^diff_", names(dd), value = TRUE)
          )
          dd <- dd[, ..keep_cols]

          # 5. Uncertainty summarisation
          dd <- melt(
            dd,
            id.vars = c("mc", "year", "sex", "agegrp", "uptake_group")
          )

          dd <- dd[
            ,
            fquantile_byid(value, prbl, id = as.character(variable)),
            keyby = c("year","sex","agegrp","uptake_group")
          ]

          setnames(
            dd,
            c("year", "sex", "agegrp", "uptake_group", "disease",
              percent(prbl, prefix = "prvl_rate_"))
          )

          dd[, scenario := sc]

          d_out <- rbind(d_out, dd, use.names = TRUE)

          # Clean structurally empty rows
          stat_cols <- grep("%$", names(d_out), value = TRUE)
          d_out <- na.omit(d_out, cols=stat_cols)

        }

        fwrite(d_out, paste0(out_path_tables, "prevalence_numbers_diff_by_year_agegrp_sex.csv"), sep = ";")

        ggplot(d_out, aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                          ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario,
                          linetype = agegrp)) +
          facet_wrap(~ disease + sex, scales = "free") +
          geom_ribbon(alpha = 0.5/5, colour = NA) +
          geom_line() +
          scale_x_continuous(name = "Year") +
          scale_y_continuous(name = "Prevalence") +
          ggtitle("Difference in prevalence numbers of diseases compared to baseline over time") +
          expand_limits(y = 0) +
          theme(legend.title = element_blank())

        ggsave(paste0(out_path_plots, "prevalence_numbers_diff_by_year_agegrp_sex.", plot_format),
               height = 9, width = 16)


        # Case-years prevented or postponed #

        d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]

        prvls <- grep("_prvl", names(d), value = TRUE)

        d <- melt(d, id.vars = outstrata)
        d <- dcast(d, mc + year + agegrp + sex + uptake_group ~ scenario + variable)

        d_out <- data.table(NULL)

        for (k in seq_len(nrow(sc_map))) {

          sc <- sc_map$scenario[k]
          bl <- sc_map$baseline[k]

          sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
          bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

          if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

          # Safety check: same variables, same order
          stopifnot(
            sub(paste0("^", sc), "", sc_cols) ==
              sub(paste0("^", bl), "", bl_cols)
          )

          dd <- copy(d)

          # Compute differences
          for (i in seq_along(sc_cols)) {
            diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
            dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
          }

          # Keep only needed columns
          keep_cols <- c(
            "mc", "year", "sex", "agegrp", "uptake_group",
            grep("^diff_", names(dd), value = TRUE)
          )
          dd <- dd[, ..keep_cols]

          # 5. Uncertainty summarisation
          dd <- melt(dd,
                     id.vars = c("mc", "year", "sex", "agegrp", "uptake_group"))

          dd <- dd[, lapply(.SD, sum),.SDcols = "value",
                   by = c("mc", "sex", "agegrp", "uptake_group", "variable")]

          dd <- dd[ , fquantile_byid(value, prbl, id = as.character(variable)),
            keyby = c("sex","agegrp","uptake_group")
          ]

          setnames(dd,
            c("sex", "agegrp", "uptake_group", "disease",
              percent(prbl, prefix = "prvl_rate_"))
          )

          dd[, scenario := sc]

          d_out <- rbind(d_out, dd, use.names = TRUE)

          # Clean structurally empty rows
          stat_cols <- grep("%$", names(d_out), value = TRUE)
          d_out <- na.omit(d_out, cols=stat_cols)

        }

        fwrite(d_out, paste0(out_path_tables, "case_years_prev_post_by_scenario_agegrp_sex.csv"), sep = ";")


        ## Prevalence by sex ## ----

        tt <- fread(paste0(in_path, "prvl_scaled_up.csv.gz")
        )[, `:=` (year = year + 2000)]
        tt[, grep("cms", names(tt), value = TRUE) := NULL]

        outstrata <- c("mc", "year", "sex", "scenario", "uptake_group")

        # Rate #

        d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)
        ][, lapply(.SD, function(x) x/popsize), keyby = outstrata]
        d <- melt(d, id.vars = outstrata)
        d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
        setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "prvl_rate_")))

        fwrite(d, paste0(out_path_tables, "prevalence_rate_by_year_sex.csv"), sep = ";")

        ggplot(d[disease != "popsize"], aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                          ymax = `prvl_rate_97.5%`,
                          colour = scenario, fill = scenario,
                          linetype = sex)) +
          facet_wrap(~ disease, scales = "free") +
          geom_ribbon(alpha = 0.5/5, colour = NA) +
          geom_line() +
          scale_x_continuous(name = "Year") +
          scale_y_continuous(name = "Prevalence") +
          ggtitle("Prevalence rate of diseases by scenario over time") +
          expand_limits(y = 0) +
          theme(legend.title = element_blank())

        ggsave(paste0(out_path_plots, "prevalence_rate_by_year_sex.", plot_format),
               height = 9, width = 16)

        # Rate difference #

        d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)
        ][, lapply(.SD, function(x) x/popsize), keyby = outstrata]

        prvls <- grep("_prvl", names(d), value = TRUE)

        d <- melt(d, id.vars = outstrata)
        d <- dcast(d, mc + year + sex + uptake_group ~ scenario + variable)

        d_out <- data.table(NULL)

        for (k in seq_len(nrow(sc_map))) {

          sc <- sc_map$scenario[k]
          bl <- sc_map$baseline[k]

          sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
          bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

          if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

          # Safety check: same variables, same order
          stopifnot(
            sub(paste0("^", sc), "", sc_cols) ==
              sub(paste0("^", bl), "", bl_cols)
          )

          dd <- copy(d)

          # Compute differences
          for (i in seq_along(sc_cols)) {
            diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
            dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
          }

          # Keep only needed columns
          keep_cols <- c(
            "mc", "year", "sex", "uptake_group",
            grep("^diff_", names(dd), value = TRUE)
          )
          dd <- dd[, ..keep_cols]

          # 5. Uncertainty summarisation
          dd <- melt(
            dd,
            id.vars = c("mc", "year", "sex", "uptake_group")
          )

          dd <- dd[
            ,
            fquantile_byid(value, prbl, id = as.character(variable)),
            keyby = c("year","sex","uptake_group")
          ]

          setnames(
            dd,
            c("year", "sex", "uptake_group", "disease",
              percent(prbl, prefix = "prvl_rate_"))
          )

          dd[, scenario := sc]

          d_out <- rbind(d_out, dd, use.names = TRUE)

          # Clean structurally empty rows
          stat_cols <- grep("%$", names(d_out), value = TRUE)
          d_out <- na.omit(d_out, cols=stat_cols)

        }

        fwrite(d_out, paste0(out_path_tables, "prevalence_rate_diff_by_year_sex.csv"), sep = ";")

        ggplot(d_out, aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                                            ymax = `prvl_rate_97.5%`,
                          colour = scenario, fill = scenario,
                          linetype = sex)) +
          facet_wrap(~ disease, scales = "free") +
          geom_ribbon(alpha = 0.5/5, colour = NA) +
          geom_line() +
          scale_x_continuous(name = "Year") +
          scale_y_continuous(name = "Prevalence") +
          ggtitle("Difference in prevalence rate of diseases compared to baseline over time") +
          expand_limits(y = 0) +
          theme(legend.title = element_blank())

        ggsave(paste0(out_path_plots, "prevalence_rate_diff_by_year_sex.", plot_format),
               height = 9, width = 16)

        # Absolute numbers #

        d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]
        d <- melt(d, id.vars = outstrata)
        d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
        setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "prvl_rate_")))

        fwrite(d, paste0(out_path_tables, "prevalence_numbers_by_year_sex.csv"), sep = ";")

        ggplot(d[disease != "popsize"], aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                          ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario,
                          linetype = sex)) +
          facet_wrap(~ disease, scales = "free") +
          geom_ribbon(alpha = 0.5/5, colour = NA) +
          geom_line() +
          scale_x_continuous(name = "Year") +
          scale_y_continuous(name = "Prevalence") +
          ggtitle("Prevalence numbers of diseases by scenario over time") +
          expand_limits(y = 0) +
          theme(legend.title = element_blank())

        ggsave(paste0(out_path_plots, "prevalence_numbers_by_year_sex.", plot_format),
               height = 9, width = 16)

        # Absolute difference #

        d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]

        prvls <- grep("_prvl", names(d), value = TRUE)

        d <- melt(d, id.vars = outstrata)
        d <- dcast(d, mc + year + sex + uptake_group ~ scenario + variable)

        d_out <- data.table(NULL)

        for (k in seq_len(nrow(sc_map))) {

          sc <- sc_map$scenario[k]
          bl <- sc_map$baseline[k]

          sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
          bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

          if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

          # Safety check: same variables, same order
          stopifnot(
            sub(paste0("^", sc), "", sc_cols) ==
              sub(paste0("^", bl), "", bl_cols)
          )

          dd <- copy(d)

          # Compute differences
          for (i in seq_along(sc_cols)) {
            diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
            dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
          }

          # Keep only needed columns
          keep_cols <- c(
            "mc", "year", "sex", "uptake_group",
            grep("^diff_", names(dd), value = TRUE)
          )
          dd <- dd[, ..keep_cols]

          # 5. Uncertainty summarisation
          dd <- melt(
            dd,
            id.vars = c("mc", "year", "sex", "uptake_group")
          )

          dd <- dd[
            ,
            fquantile_byid(value, prbl, id = as.character(variable)),
            keyby = c("year","sex","uptake_group")
          ]

          setnames(
            dd,
            c("year", "sex", "uptake_group", "disease",
              percent(prbl, prefix = "prvl_rate_"))
          )

          dd[, scenario := sc]

          d_out <- rbind(d_out, dd, use.names = TRUE)

          # Clean structurally empty rows
          stat_cols <- grep("%$", names(d_out), value = TRUE)
          d_out <- na.omit(d_out, cols=stat_cols)

        }

        fwrite(d_out, paste0(out_path_tables, "prevalence_numbers_diff_by_year_sex.csv"), sep = ";")

        ggplot(d_out, aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                                            ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario,
                          linetype = sex)) +
          facet_wrap(~ disease, scales = "free") +
          geom_ribbon(alpha = 0.5/5, colour = NA) +
          geom_line() +
          scale_x_continuous(name = "Year") +
          scale_y_continuous(name = "Prevalence") +
          ggtitle("Difference in prevalence numbers of diseases compared to baseline over time") +
          expand_limits(y = 0) +
          theme(legend.title = element_blank())

        ggsave(paste0(out_path_plots, "prevalence_numbers_diff_by_year_sex.", plot_format),
               height = 9, width = 16)


        # Case-years prevented or postponed #
        # dd <- dd[, lapply(.SD, sum), .SDcols = "value", by = c("mc", "sex", "variable")]

        d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]

        prvls <- grep("_prvl", names(d), value = TRUE)

        d <- melt(d, id.vars = outstrata)
        d <- dcast(d, mc + year + sex + uptake_group ~ scenario + variable)

        d_out <- data.table(NULL)

        for (k in seq_len(nrow(sc_map))) {

          sc <- sc_map$scenario[k]
          bl <- sc_map$baseline[k]

          sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
          bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

          if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

          # Safety check: same variables, same order
          stopifnot(
            sub(paste0("^", sc), "", sc_cols) ==
              sub(paste0("^", bl), "", bl_cols)
          )

          dd <- copy(d)

          # Compute differences
          for (i in seq_along(sc_cols)) {
            diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
            dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
          }

          # Keep only needed columns
          keep_cols <- c(
            "mc", "year", "sex", "uptake_group",
            grep("^diff_", names(dd), value = TRUE)
          )
          dd <- dd[, ..keep_cols]

          # 5. Uncertainty summarisation
          dd <- melt(dd,
                     id.vars = c("mc", "year", "sex", "uptake_group"))

          dd <- dd[, lapply(.SD, sum),.SDcols = "value",
                   by = c("mc", "sex", "uptake_group", "variable")]

          dd <- dd[ , fquantile_byid(value, prbl, id = as.character(variable)),
                    keyby = c("sex","uptake_group")
          ]

          setnames(dd,
                   c("sex", "uptake_group", "disease",
                     percent(prbl, prefix = "prvl_rate_"))
          )

          dd[, scenario := sc]

          d_out <- rbind(d_out, dd, use.names = TRUE)

          # Clean structurally empty rows
          stat_cols <- grep("%$", names(d_out), value = TRUE)
          d_out <- na.omit(d_out, cols=stat_cols)

        }

        fwrite(d_out, paste0(out_path_tables, "case_years_prev_post_by_scenario_sex.csv"), sep = ";")


        # Cumulative case-years prevented or postponed over time #

        d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]

        prvls <- grep("_prvl", names(d), value = TRUE)

        d <- melt(d, id.vars = outstrata)
        d <- dcast(d, mc + year + sex + uptake_group ~ scenario + variable)

        d_out <- data.table(NULL)

        for (k in seq_len(nrow(sc_map))) {

          sc <- sc_map$scenario[k]
          bl <- sc_map$baseline[k]

          sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
          bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

          if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

          # Safety check: same variables, same order
          stopifnot(
            sub(paste0("^", sc), "", sc_cols) ==
              sub(paste0("^", bl), "", bl_cols)
          )

          dd <- copy(d)

          # Compute differences
          for (i in seq_along(sc_cols)) {
            diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
            dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
          }

          # Keep only needed columns
          keep_cols <- c(
            "mc", "year", "sex", "uptake_group",
            grep("^diff_", names(dd), value = TRUE)
          )
          dd <- dd[, ..keep_cols]

          # 5. Uncertainty summarisation
          dd <- melt(dd,
                     id.vars = c("mc", "year", "sex", "uptake_group"))

          dd[, value := cumsum(value), by = c("mc", "sex", "uptake_group", "variable")]

          dd <- dd[ , fquantile_byid(value, prbl, id = as.character(variable)),
                    keyby = c("sex", "year","uptake_group")
          ]

          setnames(dd,
                   c("sex", "year", "uptake_group", "disease",
                     percent(prbl, prefix = "prvl_rate_"))
          )

          dd[, scenario := sc]

          d_out <- rbind(d_out, dd, use.names = TRUE)

          # Clean structurally empty rows
          stat_cols <- grep("%$", names(d_out), value = TRUE)
          d_out <- na.omit(d_out, cols=stat_cols)

        }

        fwrite(d_out, paste0(out_path_tables, "case_years_prev_post_by_scenario_sex_year.csv"), sep = ";")



        ## Prevalence by age ## ----

        tt <- fread(paste0(in_path, "prvl_scaled_up.csv.gz")
        )[, `:=` (year = year + 2000,
                  agegrp = fifelse(agegrp %in% c("30-34", "35-39", "40-44", "45-49"), "30-49",
                                   ifelse(agegrp %in% c("50-54", "55-59", "60-64", "65-69"), "50-69",
                                          "70-90")))]

        tt[, grep("cms", names(tt), value = TRUE) := NULL]

        outstrata <- c("mc", "year", "agegrp", "scenario", "uptake_group")

        # Rate #

        d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)
        ][, lapply(.SD, function(x) x/popsize), keyby = outstrata]
        d <- melt(d, id.vars = outstrata)
        d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
        setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "prvl_rate_")))

        fwrite(d, paste0(out_path_tables, "prevalence_rate_by_year_agegrp.csv"), sep = ";")

        ggplot(d[disease != "popsize"], aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                                            ymax = `prvl_rate_97.5%`,
                                            colour = scenario, fill = scenario,
                                            linetype = agegrp)) +
          facet_wrap(~ disease, scales = "free") +
          geom_ribbon(alpha = 0.5/5, colour = NA) +
          geom_line() +
          scale_x_continuous(name = "Year") +
          scale_y_continuous(name = "Prevalence") +
          ggtitle("Prevalence rate of diseases by scenario over time") +
          expand_limits(y = 0) +
          theme(legend.title = element_blank())

        ggsave(paste0(out_path_plots, "prevalence_rate_by_year_agegrp.", plot_format),
               height = 9, width = 16)

        # Rate difference #

        d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)
        ][, lapply(.SD, function(x) x/popsize), keyby = outstrata]

        prvls <- grep("_prvl", names(d), value = TRUE)

        d <- melt(d, id.vars = outstrata)
        d <- dcast(d, mc + year + agegrp + uptake_group ~ scenario + variable)

        d_out <- data.table(NULL)

        for (k in seq_len(nrow(sc_map))) {

          sc <- sc_map$scenario[k]
          bl <- sc_map$baseline[k]

          sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
          bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

          if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

          # Safety check: same variables, same order
          stopifnot(
            sub(paste0("^", sc), "", sc_cols) ==
              sub(paste0("^", bl), "", bl_cols)
          )

          dd <- copy(d)

          # Compute differences
          for (i in seq_along(sc_cols)) {
            diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
            dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
          }

          # Keep only needed columns
          keep_cols <- c(
            "mc", "year", "agegrp","uptake_group",
            grep("^diff_", names(dd), value = TRUE)
          )
          dd <- dd[, ..keep_cols]

          # 5. Uncertainty summarisation
          dd <- melt(
            dd,
            id.vars = c("mc", "year", "agegrp","uptake_group")
          )

          dd <- dd[
            ,
            fquantile_byid(value, prbl, id = as.character(variable)),
            keyby = c("year","agegrp","uptake_group")
          ]

          setnames(
            dd,
            c("year","agegrp", "uptake_group", "disease",
              percent(prbl, prefix = "prvl_rate_"))
          )

          dd[, scenario := sc]

          d_out <- rbind(d_out, dd, use.names = TRUE)

          # Clean structurally empty rows
          stat_cols <- grep("%$", names(d_out), value = TRUE)
          d_out <- na.omit(d_out, cols=stat_cols)

        }

        fwrite(d_out, paste0(out_path_tables, "prevalence_rate_diff_by_year_agegrp.csv"), sep = ";")

        ggplot(d_out, aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                          ymax = `prvl_rate_97.5%`,
                          colour = scenario, fill = scenario,
                          linetype = agegrp)) +
          facet_wrap(~ disease, scales = "free") +
          geom_ribbon(alpha = 0.5/5, colour = NA) +
          geom_line() +
          scale_x_continuous(name = "Year") +
          scale_y_continuous(name = "Prevalence") +
          ggtitle("Difference in prevalence rate of diseases compared to baseline over time") +
          expand_limits(y = 0) +
          theme(legend.title = element_blank())

        ggsave(paste0(out_path_plots, "prevalence_rate_diff_by_year_agegrp.", plot_format),
               height = 9, width = 16)

        # Absolute numbers #

        d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]
        d <- melt(d, id.vars = outstrata)
        d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
        setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "prvl_rate_")))

        fwrite(d, paste0(out_path_tables, "prevalence_numbers_by_year_agegrp.csv"), sep = ";")

        ggplot(d[disease != "popsize"], aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                                            ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario,
                                            linetype = agegrp)) +
          facet_wrap(~ disease, scales = "free") +
          geom_ribbon(alpha = 0.5/5, colour = NA) +
          geom_line() +
          scale_x_continuous(name = "Year") +
          scale_y_continuous(name = "Prevalence") +
          ggtitle("Prevalence numbers of diseases by scenario over time") +
          expand_limits(y = 0) +
          theme(legend.title = element_blank())

        ggsave(paste0(out_path_plots, "prevalence_numbers_by_year_agegrp.", plot_format),
               height = 9, width = 16)

        # Absolute difference #

        d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]

        prvls <- grep("_prvl", names(d), value = TRUE)

        d <- melt(d, id.vars = outstrata)
        d <- dcast(d, mc + year + agegrp + uptake_group ~ scenario + variable)

        d_out <- data.table(NULL)

        for (k in seq_len(nrow(sc_map))) {

          sc <- sc_map$scenario[k]
          bl <- sc_map$baseline[k]

          sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
          bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

          if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

          # Safety check: same variables, same order
          stopifnot(
            sub(paste0("^", sc), "", sc_cols) ==
              sub(paste0("^", bl), "", bl_cols)
          )

          dd <- copy(d)

          # Compute differences
          for (i in seq_along(sc_cols)) {
            diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
            dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
          }

          # Keep only needed columns
          keep_cols <- c(
            "mc", "year", "agegrp","uptake_group",
            grep("^diff_", names(dd), value = TRUE)
          )
          dd <- dd[, ..keep_cols]

          # 5. Uncertainty summarisation
          dd <- melt(
            dd,
            id.vars = c("mc", "year", "agegrp","uptake_group")
          )

          dd <- dd[
            ,
            fquantile_byid(value, prbl, id = as.character(variable)),
            keyby = c("year","agegrp","uptake_group")
          ]

          setnames(
            dd,
            c("year","agegrp", "uptake_group", "disease",
              percent(prbl, prefix = "prvl_rate_"))
          )

          dd[, scenario := sc]

          d_out <- rbind(d_out, dd, use.names = TRUE)

          # Clean structurally empty rows
          stat_cols <- grep("%$", names(d_out), value = TRUE)
          d_out <- na.omit(d_out, cols=stat_cols)

        }

        fwrite(d_out, paste0(out_path_tables, "prevalence_numbers_diff_by_year_agegrp.csv"), sep = ";")

        ggplot(d_out, aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                          ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario,
                          linetype = agegrp)) +
          facet_wrap(~ disease, scales = "free") +
          geom_ribbon(alpha = 0.5/5, colour = NA) +
          geom_line() +
          scale_x_continuous(name = "Year") +
          scale_y_continuous(name = "Prevalence") +
          ggtitle("Difference in prevalence numbers of diseases compared to baseline over time") +
          expand_limits(y = 0) +
          theme(legend.title = element_blank())

        ggsave(paste0(out_path_plots, "prevalence_numbers_diff_by_year_agegrp.", plot_format),
               height = 9, width = 16)


        # Case-years prevented or postponed #

        d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]

        prvls <- grep("_prvl", names(d), value = TRUE)

        d <- melt(d, id.vars = outstrata)
        d <- dcast(d, mc + year + agegrp + uptake_group ~ scenario + variable)

        d_out <- data.table(NULL)

        for (k in seq_len(nrow(sc_map))) {

          sc <- sc_map$scenario[k]
          bl <- sc_map$baseline[k]

          sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
          bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

          if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

          # Safety check: same variables, same order
          stopifnot(
            sub(paste0("^", sc), "", sc_cols) ==
              sub(paste0("^", bl), "", bl_cols)
          )

          dd <- copy(d)

          # Compute differences
          for (i in seq_along(sc_cols)) {
            diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
            dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
          }

          # Keep only needed columns
          keep_cols <- c(
            "mc", "year", "agegrp", "uptake_group",
            grep("^diff_", names(dd), value = TRUE)
          )
          dd <- dd[, ..keep_cols]

          # 5. Uncertainty summarisation
          dd <- melt(dd,
                     id.vars = c("mc", "year", "agegrp", "uptake_group"))

          dd <- dd[, lapply(.SD, sum),.SDcols = "value",
                   by = c("mc", "agegrp", "uptake_group", "variable")]

          dd <- dd[ , fquantile_byid(value, prbl, id = as.character(variable)),
                    keyby = c("agegrp","uptake_group")
          ]

          setnames(dd,
                   c("agegrp", "uptake_group", "disease",
                     percent(prbl, prefix = "prvl_rate_"))
          )

          dd[, scenario := sc]

          d_out <- rbind(d_out, dd, use.names = TRUE)

          # Clean structurally empty rows
          stat_cols <- grep("%$", names(d_out), value = TRUE)
          d_out <- na.omit(d_out, cols=stat_cols)

        }

        fwrite(d_out, paste0(out_path_tables, "case_years_prev_post_by_scenario_agegrp.csv"), sep = ";")


        ## Prevalence total ## ----

        tt <- fread(paste0(in_path, "prvl_scaled_up.csv.gz")
        )[, `:=` (year = year + 2000)]
        tt[, grep("cms", names(tt), value = TRUE) := NULL]

        outstrata <- c("mc", "year", "scenario", "uptake_group")

        # Rate #

        d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)
        ][, lapply(.SD, function(x) x/popsize), keyby = outstrata]
        d <- melt(d, id.vars = outstrata)
        d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
        setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "prvl_rate_")))

        fwrite(d, paste0(out_path_tables, "prevalence_rate_by_year.csv"), sep = ";")

        ggplot(d[disease != "popsize"], aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                                            ymax = `prvl_rate_97.5%`,
                                            colour = scenario, fill = scenario)) +
          facet_wrap(~ disease, scales = "free") +
          geom_ribbon(alpha = 0.5/5, colour = NA) +
          geom_line() +
          scale_x_continuous(name = "Year") +
          scale_y_continuous(name = "Prevalence") +
          ggtitle("Prevalence rate of diseases by scenario over time") +
          expand_limits(y = 0) +
          theme(legend.title = element_blank())

        ggsave(paste0(out_path_plots, "prevalence_rate_by_year.", plot_format),
               height = 9, width = 16)

        # Rate difference #

        d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)
        ][, lapply(.SD, function(x) x/popsize), keyby = outstrata]

        prvls <- grep("_prvl", names(d), value = TRUE)

        d <- melt(d, id.vars = outstrata)
        d <- dcast(d, mc + year + uptake_group ~ scenario + variable)

        d_out <- data.table(NULL)

        for (k in seq_len(nrow(sc_map))) {

          sc <- sc_map$scenario[k]
          bl <- sc_map$baseline[k]

          sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
          bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

          if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

          # Safety check: same variables, same order
          stopifnot(
            sub(paste0("^", sc), "", sc_cols) ==
              sub(paste0("^", bl), "", bl_cols)
          )

          dd <- copy(d)

          # Compute differences
          for (i in seq_along(sc_cols)) {
            diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
            dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
          }

          # Keep only needed columns
          keep_cols <- c(
            "mc", "year","uptake_group",
            grep("^diff_", names(dd), value = TRUE)
          )
          dd <- dd[, ..keep_cols]

          # 5. Uncertainty summarisation
          dd <- melt(
            dd,
            id.vars = c("mc", "year","uptake_group")
          )

          dd <- dd[
            ,
            fquantile_byid(value, prbl, id = as.character(variable)),
            keyby = c("year","uptake_group")
          ]

          setnames(
            dd,
            c("year", "uptake_group", "disease",
              percent(prbl, prefix = "prvl_rate_"))
          )

          dd[, scenario := sc]

          d_out <- rbind(d_out, dd, use.names = TRUE)

          # Clean structurally empty rows
          stat_cols <- grep("%$", names(d_out), value = TRUE)
          d_out <- na.omit(d_out, cols=stat_cols)

        }

        fwrite(d_out, paste0(out_path_tables, "prevalence_rate_diff_by_year.csv"), sep = ";")

        ggplot(d_out, aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                          ymax = `prvl_rate_97.5%`,
                          colour = scenario, fill = scenario)) +
          facet_wrap(~ disease, scales = "free") +
          geom_ribbon(alpha = 0.5/5, colour = NA) +
          geom_line() +
          scale_x_continuous(name = "Year") +
          scale_y_continuous(name = "Prevalence") +
          ggtitle("Difference in prevalence rate of diseases compared to baseline over time") +
          expand_limits(y = 0) +
          theme(legend.title = element_blank())

        ggsave(paste0(out_path_plots, "prevalence_rate_diff_by_year.", plot_format),
               height = 9, width = 16)

        # Absolute numbers #

        d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]
        d <- melt(d, id.vars = outstrata)
        d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
        setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "prvl_rate_")))

        fwrite(d, paste0(out_path_tables, "prevalence_numbers_by_year.csv"), sep = ";")

        ggplot(d[disease != "popsize"], aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                                            ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario)) +
          facet_wrap(~ disease, scales = "free") +
          geom_ribbon(alpha = 0.5/5, colour = NA) +
          geom_line() +
          scale_x_continuous(name = "Year") +
          scale_y_continuous(name = "Prevalence") +
          ggtitle("Prevalence numbers of diseases by scenario over time") +
          expand_limits(y = 0) +
          theme(legend.title = element_blank())

        ggsave(paste0(out_path_plots, "prevalence_numbers_by_year.", plot_format),
               height = 9, width = 16)

        # Absolute difference #

        d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]

        prvls <- grep("_prvl", names(d), value = TRUE)

        d <- melt(d, id.vars = outstrata)
        d <- dcast(d, mc + year + uptake_group ~ scenario + variable)

        d_out <- data.table(NULL)

        for (k in seq_len(nrow(sc_map))) {

          sc <- sc_map$scenario[k]
          bl <- sc_map$baseline[k]

          sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
          bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

          if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

          # Safety check: same variables, same order
          stopifnot(
            sub(paste0("^", sc), "", sc_cols) ==
              sub(paste0("^", bl), "", bl_cols)
          )

          dd <- copy(d)

          # Compute differences
          for (i in seq_along(sc_cols)) {
            diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
            dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
          }

          # Keep only needed columns
          keep_cols <- c(
            "mc", "year","uptake_group",
            grep("^diff_", names(dd), value = TRUE)
          )
          dd <- dd[, ..keep_cols]

          # 5. Uncertainty summarisation
          dd <- melt(
            dd,
            id.vars = c("mc", "year","uptake_group")
          )

          dd <- dd[
            ,
            fquantile_byid(value, prbl, id = as.character(variable)),
            keyby = c("year","uptake_group")
          ]

          setnames(
            dd,
            c("year", "uptake_group", "disease",
              percent(prbl, prefix = "prvl_rate_"))
          )

          dd[, scenario := sc]

          d_out <- rbind(d_out, dd, use.names = TRUE)

          # Clean structurally empty rows
          stat_cols <- grep("%$", names(d_out), value = TRUE)
          d_out <- na.omit(d_out, cols=stat_cols)

        }

        fwrite(d_out, paste0(out_path_tables, "prevalence_numbers_diff_by_year.csv"), sep = ";")

        ggplot(d_out, aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                          ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario)) +
          facet_wrap(~ disease, scales = "free") +
          geom_ribbon(alpha = 0.5/5, colour = NA) +
          geom_line() +
          scale_x_continuous(name = "Year") +
          scale_y_continuous(name = "Prevalence") +
          ggtitle("Difference in prevalence numbers of diseases compared to baseline over time") +
          expand_limits(y = 0) +
          theme(legend.title = element_blank())

        ggsave(paste0(out_path_plots, "prevalence_numbers_diff_by_year.", plot_format),
               height = 9, width = 16)


        # Case-years prevented or postponed #
        # dd <- dd[, lapply(.SD, sum), .SDcols = "value", by = c("mc", "variable")]

        d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]

        prvls <- grep("_prvl", names(d), value = TRUE)

        d <- melt(d, id.vars = outstrata)
        d <- dcast(d, mc + year + uptake_group ~ scenario + variable)

        d_out <- data.table(NULL)

        for (k in seq_len(nrow(sc_map))) {

          sc <- sc_map$scenario[k]
          bl <- sc_map$baseline[k]

          sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
          bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

          if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

          # Safety check: same variables, same order
          stopifnot(
            sub(paste0("^", sc), "", sc_cols) ==
              sub(paste0("^", bl), "", bl_cols)
          )

          dd <- copy(d)

          # Compute differences
          for (i in seq_along(sc_cols)) {
            diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
            dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
          }

          # Keep only needed columns
          keep_cols <- c(
            "mc", "year", "uptake_group",
            grep("^diff_", names(dd), value = TRUE)
          )
          dd <- dd[, ..keep_cols]

          # 5. Uncertainty summarisation
          dd <- melt(dd,
                     id.vars = c("mc", "year", "uptake_group"))

          dd <- dd[, lapply(.SD, sum),.SDcols = "value",
                   by = c("mc", "uptake_group", "variable")]

          dd <- dd[ , fquantile_byid(value, prbl, id = as.character(variable)),
                    keyby = c("uptake_group")
          ]

          setnames(dd,
                   c("uptake_group", "disease",
                     percent(prbl, prefix = "prvl_rate_"))
          )

          dd[, scenario := sc]

          d_out <- rbind(d_out, dd, use.names = TRUE)

          # Clean structurally empty rows
          stat_cols <- grep("%$", names(d_out), value = TRUE)
          d_out <- na.omit(d_out, cols=stat_cols)

        }


        fwrite(d_out, paste0(out_path_tables, "case_years_prev_post_by_scenario.csv"), sep = ";")

    }

  if("incd_scaled_up.csv.gz" %in% list.files(in_path)){

    ## Incidence by age and sex ## ----

    tt <- fread(paste0(in_path, "incd_scaled_up.csv.gz")
    )[, `:=` (year = year + 2000,
              agegrp = fifelse(agegrp %in% c("30-34", "35-39", "40-44", "45-49"), "30-49",
                               ifelse(agegrp %in% c("50-54", "55-59", "60-64", "65-69"), "50-69",
                                      "70-90")))]

    tt[, grep("cms", names(tt), value = TRUE) := NULL]

    outstrata <- c("mc", "year", "sex", "agegrp", "scenario", "uptake_group")

    sc_n <- na.omit(as.numeric(gsub("[^1-9]+", "", unique(tt$scenario))))

    # Rate #

    d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)
    ][, lapply(.SD, function(x) x/popsize), keyby = outstrata]
    d <- melt(d, id.vars = outstrata)
    d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
    setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "prvl_rate_")))

    fwrite(d, paste0(out_path_tables, "incidence_rate_by_year_agegrp_sex.csv"), sep = ";")

    ggplot(d[disease != "popsize"], aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                                        ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario,
                                        linetype = agegrp)) +
      facet_wrap(~ disease + sex, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Prevalence") +
      ggtitle("Incidence rate of diseases by scenario over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "incidence_rate_by_year_agegrp_sex.", plot_format),
           height = 9, width = 16)

    # Rate difference #

    d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)
    ][, lapply(.SD, function(x) x/popsize), keyby = outstrata]

    prvls <- grep("_prvl", names(d), value = TRUE)

    d <- melt(d, id.vars = outstrata)
    d <- dcast(d, mc + year + agegrp + sex + uptake_group ~ scenario + variable)

    diffs0 <- grep("sc0_", names(d), value = TRUE)[-1]

    d_out <- data.table(NULL)

    for (k in seq_len(nrow(sc_map))) {

      sc <- sc_map$scenario[k]
      bl <- sc_map$baseline[k]

      sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
      bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

      if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

      # Safety check: same variables, same order
      stopifnot(
        sub(paste0("^", sc), "", sc_cols) ==
          sub(paste0("^", bl), "", bl_cols)
      )

      dd <- copy(d)

      # Compute differences
      for (i in seq_along(sc_cols)) {
        diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
        dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
      }

      # Keep only needed columns
      keep_cols <- c(
        "mc", "year", "sex", "agegrp", "uptake_group",
        grep("^diff_", names(dd), value = TRUE)
      )
      dd <- dd[, ..keep_cols]

      # 5. Uncertainty summarisation
      dd <- melt(
        dd,
        id.vars = c("mc", "year", "sex", "agegrp", "uptake_group")
      )

      dd <- dd[
        ,
        fquantile_byid(value, prbl, id = as.character(variable)),
        keyby = c("year","sex","agegrp","uptake_group")
      ]

      setnames(
        dd,
        c("year", "sex", "agegrp", "uptake_group", "disease",
          percent(prbl, prefix = "prvl_rate_"))
      )

      dd[, scenario := sc]

      d_out <- rbind(d_out, dd, use.names = TRUE)

      # Clean structurally empty rows
      stat_cols <- grep("%$", names(d_out), value = TRUE)
      d_out <- na.omit(d_out, cols=stat_cols)

    }

    fwrite(d_out, paste0(out_path_tables, "incidence_rate_diff_by_year_agegrp_sex.csv"), sep = ";")

    ggplot(d_out, aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                      ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario,
                      linetype = agegrp)) +
      facet_wrap(~ disease + sex, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Prevalence") +
      ggtitle("Difference in incidence rate of diseases compared to baseline over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "incidence_rate_diff_by_year_agegrp_sex.", plot_format),
           height = 9, width = 16)

    # Absolute numbers #

    d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]
    d <- melt(d, id.vars = outstrata)
    d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
    setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "prvl_rate_")))

    fwrite(d, paste0(out_path_tables, "incidence_numbers_by_year_agegrp_sex.csv"), sep = ";")

    ggplot(d[disease != "popsize"], aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                                        ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario,
                                        linetype = agegrp)) +
      facet_wrap(~ disease + sex, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Prevalence") +
      ggtitle("Incidence numbers of diseases by scenario over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "incidence_numbers_by_year_agegrp_sex.", plot_format),
           height = 9, width = 16)

    # Absolute difference #

    d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]

    prvls <- grep("_prvl", names(d), value = TRUE)

    d <- melt(d, id.vars = outstrata)
    d <- dcast(d, mc + year + agegrp + sex + uptake_group ~ scenario + variable)

    d_out <- data.table(NULL)

    for (k in seq_len(nrow(sc_map))) {

      sc <- sc_map$scenario[k]
      bl <- sc_map$baseline[k]

      sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
      bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

      if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

      # Safety check: same variables, same order
      stopifnot(
        sub(paste0("^", sc), "", sc_cols) ==
          sub(paste0("^", bl), "", bl_cols)
      )

      dd <- copy(d)

      # Compute differences
      for (i in seq_along(sc_cols)) {
        diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
        dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
      }

      # Keep only needed columns
      keep_cols <- c(
        "mc", "year", "sex", "agegrp", "uptake_group",
        grep("^diff_", names(dd), value = TRUE)
      )
      dd <- dd[, ..keep_cols]

      # 5. Uncertainty summarisation
      dd <- melt(
        dd,
        id.vars = c("mc", "year", "sex", "agegrp", "uptake_group")
      )

      dd <- dd[
        ,
        fquantile_byid(value, prbl, id = as.character(variable)),
        keyby = c("year","sex","agegrp","uptake_group")
      ]

      setnames(
        dd,
        c("year", "sex", "agegrp", "uptake_group", "disease",
          percent(prbl, prefix = "prvl_rate_"))
      )

      dd[, scenario := sc]

      d_out <- rbind(d_out, dd, use.names = TRUE)

      # Clean structurally empty rows
      stat_cols <- grep("%$", names(d_out), value = TRUE)
      d_out <- na.omit(d_out, cols=stat_cols)

    }

    fwrite(d_out, paste0(out_path_tables, "incidence_numbers_diff_by_year_agegrp_sex.csv"), sep = ";")

    ggplot(d_out, aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                      ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario,
                      linetype = agegrp)) +
      facet_wrap(~ disease + sex, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Prevalence") +
      ggtitle("Difference in incidence numbers of diseases compared to baseline over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "incidence_numbers_diff_by_year_agegrp_sex.", plot_format),
           height = 9, width = 16)

    # Cases prevented or postponed #

    d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]

    prvls <- grep("_prvl", names(d), value = TRUE)

    d <- melt(d, id.vars = outstrata)
    d <- dcast(d, mc + year + agegrp + sex + uptake_group ~ scenario + variable)

    d_out <- data.table(NULL)

    for (k in seq_len(nrow(sc_map))) {

      sc <- sc_map$scenario[k]
      bl <- sc_map$baseline[k]

      sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
      bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

      if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

      # Safety check: same variables, same order
      stopifnot(
        sub(paste0("^", sc), "", sc_cols) ==
          sub(paste0("^", bl), "", bl_cols)
      )

      dd <- copy(d)

      # Compute differences
      for (i in seq_along(sc_cols)) {
        diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
        dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
      }

      # Keep only needed columns
      keep_cols <- c(
        "mc", "year", "sex", "agegrp", "uptake_group",
        grep("^diff_", names(dd), value = TRUE)
      )
      dd <- dd[, ..keep_cols]

      # 5. Uncertainty summarisation
      dd <- melt(dd,
                 id.vars = c("mc", "year", "sex", "agegrp", "uptake_group"))

      dd <- dd[, lapply(.SD, sum),.SDcols = "value",
               by = c("mc", "sex", "agegrp", "uptake_group", "variable")]

      dd <- dd[ , fquantile_byid(value, prbl, id = as.character(variable)),
                keyby = c("sex","agegrp","uptake_group")
      ]

      setnames(dd,
               c("sex", "agegrp", "uptake_group", "disease",
                 percent(prbl, prefix = "prvl_rate_"))
      )

      dd[, scenario := sc]

      d_out <- rbind(d_out, dd, use.names = TRUE)

      # Clean structurally empty rows
      stat_cols <- grep("%$", names(d_out), value = TRUE)
      d_out <- na.omit(d_out, cols=stat_cols)

    }

    fwrite(d_out, paste0(out_path_tables, "cases_prev_post_by_scenario_agegrp_sex.csv"), sep = ";")


    ## Incidence by sex ## ----

    tt <- fread(paste0(in_path, "incd_scaled_up.csv.gz")
    )[, `:=` (year = year + 2000)]
    tt[, grep("cms", names(tt), value = TRUE) := NULL]

    outstrata <- c("mc", "year", "sex", "scenario", "uptake_group")

    # Rate #

    d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)
    ][, lapply(.SD, function(x) x/popsize), keyby = outstrata]
    d <- melt(d, id.vars = outstrata)
    d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
    setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "prvl_rate_")))

    fwrite(d, paste0(out_path_tables, "incidence_rate_by_year_sex.csv"), sep = ";")

    ggplot(d[disease != "popsize"], aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                                        ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario,
                                        linetype = sex)) +
      facet_wrap(~ disease, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Prevalence") +
      ggtitle("Incidence rate of diseases by scenario over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "incidence_rate_by_year_sex.", plot_format),
           height = 9, width = 16)

    # Rate difference #

    d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)
    ][, lapply(.SD, function(x) x/popsize), keyby = outstrata]

    prvls <- grep("_prvl", names(d), value = TRUE)

    d <- melt(d, id.vars = outstrata)
    d <- dcast(d, mc + year + sex + uptake_group ~ scenario + variable)

    d_out <- data.table(NULL)

    for (k in seq_len(nrow(sc_map))) {

      sc <- sc_map$scenario[k]
      bl <- sc_map$baseline[k]

      sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
      bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

      if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

      # Safety check: same variables, same order
      stopifnot(
        sub(paste0("^", sc), "", sc_cols) ==
          sub(paste0("^", bl), "", bl_cols)
      )

      dd <- copy(d)

      # Compute differences
      for (i in seq_along(sc_cols)) {
        diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
        dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
      }

      # Keep only needed columns
      keep_cols <- c(
        "mc", "year", "sex", "uptake_group",
        grep("^diff_", names(dd), value = TRUE)
      )
      dd <- dd[, ..keep_cols]

      # 5. Uncertainty summarisation
      dd <- melt(
        dd,
        id.vars = c("mc", "year", "sex", "uptake_group")
      )

      dd <- dd[
        ,
        fquantile_byid(value, prbl, id = as.character(variable)),
        keyby = c("year","sex","uptake_group")
      ]

      setnames(
        dd,
        c("year", "sex", "uptake_group", "disease",
          percent(prbl, prefix = "prvl_rate_"))
      )

      dd[, scenario := sc]

      d_out <- rbind(d_out, dd, use.names = TRUE)

      # Clean structurally empty rows
      stat_cols <- grep("%$", names(d_out), value = TRUE)
      d_out <- na.omit(d_out, cols=stat_cols)

    }

    fwrite(d_out, paste0(out_path_tables, "incidence_rate_diff_by_year_sex.csv"), sep = ";")

    ggplot(d_out, aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                      ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario,
                      linetype = sex)) +
      facet_wrap(~ disease, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Prevalence") +
      ggtitle("Difference in incidence rate of diseases compared to baseline over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "incidence_rate_diff_by_year_sex.", plot_format),
           height = 9, width = 16)

    # Absolute numbers #

    d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]
    d <- melt(d, id.vars = outstrata)
    d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
    setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "prvl_rate_")))

    fwrite(d, paste0(out_path_tables, "incidence_numbers_by_year_sex.csv"), sep = ";")

    ggplot(d[disease != "popsize"], aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                                        ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario,
                                        linetype = sex)) +
      facet_wrap(~ disease, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Prevalence") +
      ggtitle("Incidence numbers of diseases by scenario over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "incidence_numbers_by_year_sex.", plot_format),
           height = 9, width = 16)

    # Absolute difference #

    d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]

    prvls <- grep("_prvl", names(d), value = TRUE)

    d <- melt(d, id.vars = outstrata)
    d <- dcast(d, mc + year + sex + uptake_group ~ scenario + variable)

    d_out <- data.table(NULL)

    for (k in seq_len(nrow(sc_map))) {

      sc <- sc_map$scenario[k]
      bl <- sc_map$baseline[k]

      sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
      bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

      if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

      # Safety check: same variables, same order
      stopifnot(
        sub(paste0("^", sc), "", sc_cols) ==
          sub(paste0("^", bl), "", bl_cols)
      )

      dd <- copy(d)

      # Compute differences
      for (i in seq_along(sc_cols)) {
        diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
        dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
      }

      # Keep only needed columns
      keep_cols <- c(
        "mc", "year", "sex", "uptake_group",
        grep("^diff_", names(dd), value = TRUE)
      )
      dd <- dd[, ..keep_cols]

      # 5. Uncertainty summarisation
      dd <- melt(
        dd,
        id.vars = c("mc", "year", "sex", "uptake_group")
      )

      dd <- dd[
        ,
        fquantile_byid(value, prbl, id = as.character(variable)),
        keyby = c("year","sex","uptake_group")
      ]

      setnames(
        dd,
        c("year", "sex", "uptake_group", "disease",
          percent(prbl, prefix = "prvl_rate_"))
      )

      dd[, scenario := sc]

      d_out <- rbind(d_out, dd, use.names = TRUE)

      # Clean structurally empty rows
      stat_cols <- grep("%$", names(d_out), value = TRUE)
      d_out <- na.omit(d_out, cols=stat_cols)

    }

    fwrite(d_out, paste0(out_path_tables, "incidence_numbers_diff_by_year_sex.csv"), sep = ";")

    ggplot(d_out, aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                      ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario,
                      linetype = sex)) +
      facet_wrap(~ disease, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Prevalence") +
      ggtitle("Difference in incidence numbers of diseases compared to baseline over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "incidence_numbers_diff_by_year_sex.", plot_format),
           height = 9, width = 16)

    # Cases prevented or postponed #

    d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]

    prvls <- grep("_prvl", names(d), value = TRUE)

    d <- melt(d, id.vars = outstrata)
    d <- dcast(d, mc + year + sex + uptake_group ~ scenario + variable)

    d_out <- data.table(NULL)

    for (k in seq_len(nrow(sc_map))) {

      sc <- sc_map$scenario[k]
      bl <- sc_map$baseline[k]

      sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
      bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

      if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

      # Safety check: same variables, same order
      stopifnot(
        sub(paste0("^", sc), "", sc_cols) ==
          sub(paste0("^", bl), "", bl_cols)
      )

      dd <- copy(d)

      # Compute differences
      for (i in seq_along(sc_cols)) {
        diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
        dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
      }

      # Keep only needed columns
      keep_cols <- c(
        "mc", "year", "sex", "uptake_group",
        grep("^diff_", names(dd), value = TRUE)
      )
      dd <- dd[, ..keep_cols]

      # 5. Uncertainty summarisation
      dd <- melt(dd,
                 id.vars = c("mc", "year", "sex", "uptake_group"))

      dd <- dd[, lapply(.SD, sum),.SDcols = "value",
               by = c("mc", "sex", "uptake_group", "variable")]

      dd <- dd[ , fquantile_byid(value, prbl, id = as.character(variable)),
                keyby = c("sex","uptake_group")
      ]

      setnames(dd,
               c("sex", "uptake_group", "disease",
                 percent(prbl, prefix = "prvl_rate_"))
      )

      dd[, scenario := sc]

      d_out <- rbind(d_out, dd, use.names = TRUE)

      # Clean structurally empty rows
      stat_cols <- grep("%$", names(d_out), value = TRUE)
      d_out <- na.omit(d_out, cols=stat_cols)

    }

    fwrite(d_out, paste0(out_path_tables, "cases_prev_post_by_scenario_sex.csv"), sep = ";")


    # Cumulative cases prevented or postponed over time #

    d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]

    prvls <- grep("_prvl", names(d), value = TRUE)

    d <- melt(d, id.vars = outstrata)
    d <- dcast(d, mc + year + sex + uptake_group ~ scenario + variable)

    d_out <- data.table(NULL)

    for (k in seq_len(nrow(sc_map))) {

      sc <- sc_map$scenario[k]
      bl <- sc_map$baseline[k]

      sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
      bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

      if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

      # Safety check: same variables, same order
      stopifnot(
        sub(paste0("^", sc), "", sc_cols) ==
          sub(paste0("^", bl), "", bl_cols)
      )

      dd <- copy(d)

      # Compute differences
      for (i in seq_along(sc_cols)) {
        diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
        dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
      }

      # Keep only needed columns
      keep_cols <- c(
        "mc", "year", "sex", "uptake_group",
        grep("^diff_", names(dd), value = TRUE)
      )
      dd <- dd[, ..keep_cols]

      # 5. Uncertainty summarisation
      dd <- melt(dd,
                 id.vars = c("mc", "year", "sex", "uptake_group"))

      dd[, value := cumsum(value), by = c("mc", "sex", "uptake_group", "variable")]

      dd <- dd[ , fquantile_byid(value, prbl, id = as.character(variable)),
                keyby = c("sex", "year","uptake_group")
      ]

      setnames(dd,
               c("sex", "year", "uptake_group", "disease",
                 percent(prbl, prefix = "prvl_rate_"))
      )

      dd[, scenario := sc]

      d_out <- rbind(d_out, dd, use.names = TRUE)

      # Clean structurally empty rows
      stat_cols <- grep("%$", names(d_out), value = TRUE)
      d_out <- na.omit(d_out, cols=stat_cols)

    }

    fwrite(d_out, paste0(out_path_tables, "cases_prev_post_by_scenario_sex_year.csv"), sep = ";")

    ## Incidence by age ## ----

    tt <- fread(paste0(in_path, "incd_scaled_up.csv.gz")
    )[, `:=` (year = year + 2000,
              agegrp = fifelse(agegrp %in% c("30-34", "35-39", "40-44", "45-49"), "30-49",
                               ifelse(agegrp %in% c("50-54", "55-59", "60-64", "65-69"), "50-69",
                                      "70-90")))]

    tt[, grep("cms", names(tt), value = TRUE) := NULL]

    outstrata <- c("mc", "year", "agegrp", "scenario", "uptake_group")

    # Rate #

    d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)
    ][, lapply(.SD, function(x) x/popsize), keyby = outstrata]
    d <- melt(d, id.vars = outstrata)
    d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
    setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "prvl_rate_")))

    fwrite(d, paste0(out_path_tables, "incidence_rate_by_year_agegrp.csv"), sep = ";")

    ggplot(d[disease != "popsize"], aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                                        ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario,
                                        linetype = agegrp)) +
      facet_wrap(~ disease, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Prevalence") +
      ggtitle("Incidence rate of diseases by scenario over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "incidence_rate_by_year_agegrp.", plot_format),
           height = 9, width = 16)

    # Rate difference #

    d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)
    ][, lapply(.SD, function(x) x/popsize), keyby = outstrata]

    prvls <- grep("_prvl", names(d), value = TRUE)

    d <- melt(d, id.vars = outstrata)
    d <- dcast(d, mc + year + agegrp + uptake_group ~ scenario + variable)

    d_out <- data.table(NULL)

    for (k in seq_len(nrow(sc_map))) {

      sc <- sc_map$scenario[k]
      bl <- sc_map$baseline[k]

      sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
      bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

      if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

      # Safety check: same variables, same order
      stopifnot(
        sub(paste0("^", sc), "", sc_cols) ==
          sub(paste0("^", bl), "", bl_cols)
      )

      dd <- copy(d)

      # Compute differences
      for (i in seq_along(sc_cols)) {
        diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
        dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
      }

      # Keep only needed columns
      keep_cols <- c(
        "mc", "year", "agegrp","uptake_group",
        grep("^diff_", names(dd), value = TRUE)
      )
      dd <- dd[, ..keep_cols]

      # 5. Uncertainty summarisation
      dd <- melt(
        dd,
        id.vars = c("mc", "year", "agegrp","uptake_group")
      )

      dd <- dd[
        ,
        fquantile_byid(value, prbl, id = as.character(variable)),
        keyby = c("year","agegrp","uptake_group")
      ]

      setnames(
        dd,
        c("year","agegrp", "uptake_group", "disease",
          percent(prbl, prefix = "prvl_rate_"))
      )

      dd[, scenario := sc]

      d_out <- rbind(d_out, dd, use.names = TRUE)

      # Clean structurally empty rows
      stat_cols <- grep("%$", names(d_out), value = TRUE)
      d_out <- na.omit(d_out, cols=stat_cols)

    }

    fwrite(d_out, paste0(out_path_tables, "incidence_rate_diff_by_year_agegrp.csv"), sep = ";")

    ggplot(d_out, aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                      ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario,
                      linetype = agegrp)) +
      facet_wrap(~ disease, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Prevalence") +
      ggtitle("Difference in incidence rate of diseases compared to baseline over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "incidence_rate_diff_by_year_agegrp.", plot_format),
           height = 9, width = 16)

    # Absolute numbers #

    d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]
    d <- melt(d, id.vars = outstrata)
    d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
    setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "prvl_rate_")))

    fwrite(d, paste0(out_path_tables, "incidence_numbers_by_year_agegrp.csv"), sep = ";")

    ggplot(d[disease != "popsize"], aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                                        ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario,
                                        linetype = agegrp)) +
      facet_wrap(~ disease, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Prevalence") +
      ggtitle("Incidence numbers of diseases by scenario over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "incidence_numbers_by_year_agegrp.", plot_format),
           height = 9, width = 16)

    # Absolute difference #

    d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]

    prvls <- grep("_prvl", names(d), value = TRUE)

    d <- melt(d, id.vars = outstrata)
    d <- dcast(d, mc + year + agegrp + uptake_group ~ scenario + variable)

    d_out <- data.table(NULL)

    for (k in seq_len(nrow(sc_map))) {

      sc <- sc_map$scenario[k]
      bl <- sc_map$baseline[k]

      sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
      bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

      if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

      # Safety check: same variables, same order
      stopifnot(
        sub(paste0("^", sc), "", sc_cols) ==
          sub(paste0("^", bl), "", bl_cols)
      )

      dd <- copy(d)

      # Compute differences
      for (i in seq_along(sc_cols)) {
        diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
        dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
      }

      # Keep only needed columns
      keep_cols <- c(
        "mc", "year", "agegrp","uptake_group",
        grep("^diff_", names(dd), value = TRUE)
      )
      dd <- dd[, ..keep_cols]

      # 5. Uncertainty summarisation
      dd <- melt(
        dd,
        id.vars = c("mc", "year", "agegrp","uptake_group")
      )

      dd <- dd[
        ,
        fquantile_byid(value, prbl, id = as.character(variable)),
        keyby = c("year","agegrp","uptake_group")
      ]

      setnames(
        dd,
        c("year","agegrp", "uptake_group", "disease",
          percent(prbl, prefix = "prvl_rate_"))
      )

      dd[, scenario := sc]

      d_out <- rbind(d_out, dd, use.names = TRUE)

      # Clean structurally empty rows
      stat_cols <- grep("%$", names(d_out), value = TRUE)
      d_out <- na.omit(d_out, cols=stat_cols)

    }

    fwrite(d_out, paste0(out_path_tables, "incidence_numbers_diff_by_year_agegrp.csv"), sep = ";")

    ggplot(d_out, aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                      ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario,
                      linetype = agegrp)) +
      facet_wrap(~ disease, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Prevalence") +
      ggtitle("Difference in incidence numbers of diseases compared to baseline over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "incidence_numbers_diff_by_year_agegrp.", plot_format),
           height = 9, width = 16)

    # Cases prevented or postponed #

    d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]

    prvls <- grep("_prvl", names(d), value = TRUE)

    d <- melt(d, id.vars = outstrata)
    d <- dcast(d, mc + year + agegrp + uptake_group ~ scenario + variable)

    d_out <- data.table(NULL)

    for (k in seq_len(nrow(sc_map))) {

      sc <- sc_map$scenario[k]
      bl <- sc_map$baseline[k]

      sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
      bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

      if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

      # Safety check: same variables, same order
      stopifnot(
        sub(paste0("^", sc), "", sc_cols) ==
          sub(paste0("^", bl), "", bl_cols)
      )

      dd <- copy(d)

      # Compute differences
      for (i in seq_along(sc_cols)) {
        diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
        dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
      }

      # Keep only needed columns
      keep_cols <- c(
        "mc", "year", "agegrp", "uptake_group",
        grep("^diff_", names(dd), value = TRUE)
      )
      dd <- dd[, ..keep_cols]

      # 5. Uncertainty summarisation
      dd <- melt(dd,
                 id.vars = c("mc", "year", "agegrp", "uptake_group"))

      dd <- dd[, lapply(.SD, sum),.SDcols = "value",
               by = c("mc", "agegrp", "uptake_group", "variable")]

      dd <- dd[ , fquantile_byid(value, prbl, id = as.character(variable)),
                keyby = c("agegrp","uptake_group")
      ]

      setnames(dd,
               c("agegrp", "uptake_group", "disease",
                 percent(prbl, prefix = "prvl_rate_"))
      )

      dd[, scenario := sc]

      d_out <- rbind(d_out, dd, use.names = TRUE)

      # Clean structurally empty rows
      stat_cols <- grep("%$", names(d_out), value = TRUE)
      d_out <- na.omit(d_out, cols=stat_cols)

    }

    fwrite(d_out, paste0(out_path_tables, "cases_prev_post_by_scenario_agegrp.csv"), sep = ";")


    ## Incidence total ## ----

    tt <- fread(paste0(in_path, "incd_scaled_up.csv.gz")
    )[, `:=` (year = year + 2000)]
    tt[, grep("cms", names(tt), value = TRUE) := NULL]

    outstrata <- c("mc", "year", "scenario", "uptake_group")

    # Rate #

    d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)
    ][, lapply(.SD, function(x) x/popsize), keyby = outstrata]
    d <- melt(d, id.vars = outstrata)
    d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
    setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "prvl_rate_")))

    fwrite(d, paste0(out_path_tables, "incidence_rate_by_year.csv"), sep = ";")

    ggplot(d[disease != "popsize"], aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                                        ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario)) +
      facet_wrap(~ disease, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Prevalence") +
      ggtitle("Incidence rate of diseases by scenario over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "incidence_rate_by_year.", plot_format),
           height = 9, width = 16)

    # Rate difference #

    d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)
    ][, lapply(.SD, function(x) x/popsize), keyby = outstrata]

    prvls <- grep("_prvl", names(d), value = TRUE)

    d <- melt(d, id.vars = outstrata)
    d <- dcast(d, mc + year + uptake_group ~ scenario + variable)

    d_out <- data.table(NULL)

    for (k in seq_len(nrow(sc_map))) {

      sc <- sc_map$scenario[k]
      bl <- sc_map$baseline[k]

      sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
      bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

      if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

      # Safety check: same variables, same order
      stopifnot(
        sub(paste0("^", sc), "", sc_cols) ==
          sub(paste0("^", bl), "", bl_cols)
      )

      dd <- copy(d)

      # Compute differences
      for (i in seq_along(sc_cols)) {
        diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
        dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
      }

      # Keep only needed columns
      keep_cols <- c(
        "mc", "year","uptake_group",
        grep("^diff_", names(dd), value = TRUE)
      )
      dd <- dd[, ..keep_cols]

      # 5. Uncertainty summarisation
      dd <- melt(
        dd,
        id.vars = c("mc", "year","uptake_group")
      )

      dd <- dd[
        ,
        fquantile_byid(value, prbl, id = as.character(variable)),
        keyby = c("year","uptake_group")
      ]

      setnames(
        dd,
        c("year", "uptake_group", "disease",
          percent(prbl, prefix = "prvl_rate_"))
      )

      dd[, scenario := sc]

      d_out <- rbind(d_out, dd, use.names = TRUE)

      # Clean structurally empty rows
      stat_cols <- grep("%$", names(d_out), value = TRUE)
      d_out <- na.omit(d_out, cols=stat_cols)

    }

    fwrite(d_out, paste0(out_path_tables, "incidence_rate_diff_by_year.csv"), sep = ";")

    ggplot(d_out, aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                      ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario)) +
      facet_wrap(~ disease, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Prevalence") +
      ggtitle("Difference in incidence rate of diseases compared to baseline over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "incidence_rate_diff_by_year.", plot_format),
           height = 9, width = 16)

    # Absolute numbers #

    d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]
    d <- melt(d, id.vars = outstrata)
    d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
    setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "prvl_rate_")))

    fwrite(d, paste0(out_path_tables, "incidence_numbers_by_year.csv"), sep = ";")

    ggplot(d[disease != "popsize"], aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                                        ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario)) +
      facet_wrap(~ disease, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Prevalence") +
      ggtitle("Incidence numbers of diseases by scenario over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "incidence_numbers_by_year.", plot_format),
           height = 9, width = 16)

    # Absolute difference #

    d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]

    prvls <- grep("_prvl", names(d), value = TRUE)

    d <- melt(d, id.vars = outstrata)
    d <- dcast(d, mc + year + uptake_group ~ scenario + variable)

    d_out <- data.table(NULL)

    for (k in seq_len(nrow(sc_map))) {

      sc <- sc_map$scenario[k]
      bl <- sc_map$baseline[k]

      sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
      bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

      if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

      # Safety check: same variables, same order
      stopifnot(
        sub(paste0("^", sc), "", sc_cols) ==
          sub(paste0("^", bl), "", bl_cols)
      )

      dd <- copy(d)

      # Compute differences
      for (i in seq_along(sc_cols)) {
        diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
        dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
      }

      # Keep only needed columns
      keep_cols <- c(
        "mc", "year","uptake_group",
        grep("^diff_", names(dd), value = TRUE)
      )
      dd <- dd[, ..keep_cols]

      # 5. Uncertainty summarisation
      dd <- melt(
        dd,
        id.vars = c("mc", "year","uptake_group")
      )

      dd <- dd[
        ,
        fquantile_byid(value, prbl, id = as.character(variable)),
        keyby = c("year","uptake_group")
      ]

      setnames(
        dd,
        c("year", "uptake_group", "disease",
          percent(prbl, prefix = "prvl_rate_"))
      )

      dd[, scenario := sc]

      d_out <- rbind(d_out, dd, use.names = TRUE)

      # Clean structurally empty rows
      stat_cols <- grep("%$", names(d_out), value = TRUE)
      d_out <- na.omit(d_out, cols=stat_cols)

    }

    fwrite(d_out, paste0(out_path_tables, "incidence_numbers_diff_by_year.csv"), sep = ";")

    ggplot(d_out, aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                      ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario)) +
      facet_wrap(~ disease, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Prevalence") +
      ggtitle("Difference in incidence numbers of diseases compared to baseline over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "incidence_numbers_diff_by_year.", plot_format),
           height = 9, width = 16)

    # Cases prevented or postponed #

    d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]

    prvls <- grep("_prvl", names(d), value = TRUE)

    d <- melt(d, id.vars = outstrata)
    d <- dcast(d, mc + year + uptake_group ~ scenario + variable)

    d_out <- data.table(NULL)

    for (k in seq_len(nrow(sc_map))) {

      sc <- sc_map$scenario[k]
      bl <- sc_map$baseline[k]

      sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
      bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

      if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

      # Safety check: same variables, same order
      stopifnot(
        sub(paste0("^", sc), "", sc_cols) ==
          sub(paste0("^", bl), "", bl_cols)
      )

      dd <- copy(d)

      # Compute differences
      for (i in seq_along(sc_cols)) {
        diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
        dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
      }

      # Keep only needed columns
      keep_cols <- c(
        "mc", "year", "uptake_group",
        grep("^diff_", names(dd), value = TRUE)
      )
      dd <- dd[, ..keep_cols]

      # 5. Uncertainty summarisation
      dd <- melt(dd,
                 id.vars = c("mc", "year", "uptake_group"))

      dd <- dd[, lapply(.SD, sum),.SDcols = "value",
               by = c("mc", "uptake_group", "variable")]

      dd <- dd[ , fquantile_byid(value, prbl, id = as.character(variable)),
                keyby = c("uptake_group")
      ]

      setnames(dd,
               c("uptake_group", "disease",
                 percent(prbl, prefix = "prvl_rate_"))
      )

      dd[, scenario := sc]

      d_out <- rbind(d_out, dd, use.names = TRUE)

      # Clean structurally empty rows
      stat_cols <- grep("%$", names(d_out), value = TRUE)
      d_out <- na.omit(d_out, cols=stat_cols)

    }

    fwrite(d_out, paste0(out_path_tables, "cases_prev_post_by_scenario.csv"), sep = ";")

  }

  if("dis_mrtl_scaled_up.csv.gz" %in% list.files(in_path)){

    ## Disease-specific Mortality by age and sex ## ----

    # WARNING: For some reason some iterations have a trailing comma!
    file_lines <- readLines(paste0(in_path, "dis_mrtl_scaled_up.csv.gz"))
    writeLines(gsub(",+$", "", file_lines), paste0(in_path, "dis_mrtl_scaled_up.csv.gz"))

    tt <- fread(paste0(in_path, "dis_mrtl_scaled_up.csv.gz"), fill = TRUE,
    )[, `:=` (year = year + 2000,
              agegrp = fifelse(agegrp %in% c("30-34", "35-39", "40-44", "45-49"), "30-49",
                               ifelse(agegrp %in% c("50-54", "55-59", "60-64", "65-69"), "50-69",
                                      "70-90")))]


    outstrata <- c("mc", "agegrp", "sex", "year", "scenario", "uptake_group")

    sc_n <- na.omit(as.numeric(gsub("[^1-9]+", "", unique(tt$scenario))))

    # Rate #

    d <- tt[, lapply(.SD, sum), .SDcols = c("nonmodelled", "chd", "stroke", "popsize"), keyby = eval(outstrata)
    ][, lapply(.SD, function(x) x/popsize), keyby = outstrata]
    d <- melt(d, id.vars = outstrata)
    d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
    setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "mrtl_rate_")))

    fwrite(d, paste0(out_path_tables, "disease_mortality_rate_by_year_age_sex.csv"), sep = ";")

    ggplot(d[disease != "popsize"], aes(x = year, y = `mrtl_rate_50.0%`,
                                        ymin = `mrtl_rate_2.5%`,
                                        ymax = `mrtl_rate_97.5%`,
                                        colour = agegrp, fill = agegrp, linetype = scenario)) +
      facet_wrap(~ disease + sex, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Mortality") +
      ggtitle("Mortality rate of diseases by scenario over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "disease_mortality_rate_by_year_age_sex.", plot_format),
           height = 9, width = 16)

    # Rate difference #

    d <- tt[, lapply(.SD, sum), .SDcols = c("nonmodelled", "chd", "stroke", "popsize"), keyby = eval(outstrata)
    ][, lapply(.SD, function(x) x/popsize), keyby = outstrata]

    prvls <- c("nonmodelled", "chd", "stroke")

    d <- melt(d, id.vars = outstrata)
    d <- dcast(d, mc + year + agegrp + sex + uptake_group ~ scenario + variable)

    d_out <- data.table(NULL)

    for (k in seq_len(nrow(sc_map))) {

      sc <- sc_map$scenario[k]
      bl <- sc_map$baseline[k]

      sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
      bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

      if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

      # Safety check: same variables, same order
      stopifnot(
        sub(paste0("^", sc), "", sc_cols) ==
          sub(paste0("^", bl), "", bl_cols)
      )

      dd <- copy(d)

      # Compute differences
      for (i in seq_along(sc_cols)) {
        diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
        dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
      }

      # Keep only needed columns
      keep_cols <- c(
        "mc", "year", "agegrp", "sex", "uptake_group",
        grep("^diff_", names(dd), value = TRUE)
      )
      dd <- dd[, ..keep_cols]

      # 5. Uncertainty summarisation
      dd <- melt(
        dd,
        id.vars = c("mc", "year", "agegrp", "sex", "uptake_group")
      )

      dd <- dd[
        ,
        fquantile_byid(value, prbl, id = as.character(variable)),
        keyby = c("year", "agegrp", "sex","uptake_group")
      ]

      setnames(
        dd,
        c("year", "agegrp", "sex", "uptake_group", "disease",
          percent(prbl, prefix = "mrtl_rate_"))
      )

      dd[, scenario := sc]

      d_out <- rbind(d_out, dd, use.names = TRUE)

      # Clean structurally empty rows
      stat_cols <- grep("%$", names(d_out), value = TRUE)
      d_out <- na.omit(d_out, cols=stat_cols)

    }

    fwrite(d_out, paste0(out_path_tables, "disease_mortality_rate_diff_by_year_age_sex.csv"), sep = ";")

    ggplot(d_out, aes(x = year, y = `mrtl_rate_50.0%`,
                      ymin = `mrtl_rate_2.5%`,
                      ymax = `mrtl_rate_97.5%`,
                      colour = agegrp, fill = agegrp, linetype = scenario)) +
      facet_wrap(~ disease + sex, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Mortality") +
      ggtitle("Difference in mortality rate of diseases compared to baseline over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "disease_mortality_rate_diff_by_year_age_sex.", plot_format),
           height = 9, width = 16)


    # Absolute numbers #

    d <- tt[, lapply(.SD, sum), .SDcols = c("nonmodelled", "chd", "stroke", "popsize"), keyby = eval(outstrata)]
    d <- melt(d, id.vars = outstrata)
    d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
    setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "mrtl_rate_")))

    fwrite(d, paste0(out_path_tables, "disease_mortality_numbers_by_year_age_sex.csv"), sep = ";")

    ggplot(d[disease != "popsize"], aes(x = year, y = `mrtl_rate_50.0%`,
                                        ymin = `mrtl_rate_2.5%`,
                                        ymax = `mrtl_rate_97.5%`,
                                        colour = agegrp, fill = agegrp, linetype = scenario)) +
      facet_wrap(~ disease + sex, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Mortality") +
      ggtitle("Mortality numbers of diseases by scenario over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "disease_mortality_numbers_by_year_age_sex.", plot_format),
           height = 9, width = 16)

    # Absolute difference #

    d <- tt[, lapply(.SD, sum), .SDcols = c("nonmodelled", "chd", "stroke", "popsize"), keyby = eval(outstrata)]

    prvls <- c("nonmodelled", "chd", "stroke")

    d <- melt(d, id.vars = outstrata)
    d <- dcast(d, mc + year + agegrp + sex + uptake_group ~ scenario + variable)

    d_out <- data.table(NULL)

    for (k in seq_len(nrow(sc_map))) {

      sc <- sc_map$scenario[k]
      bl <- sc_map$baseline[k]

      sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
      bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

      if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

      # Safety check: same variables, same order
      stopifnot(
        sub(paste0("^", sc), "", sc_cols) ==
          sub(paste0("^", bl), "", bl_cols)
      )

      dd <- copy(d)

      # Compute differences
      for (i in seq_along(sc_cols)) {
        diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
        dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
      }

      # Keep only needed columns
      keep_cols <- c(
        "mc", "year", "sex", "agegrp", "uptake_group",
        grep("^diff_", names(dd), value = TRUE)
      )
      dd <- dd[, ..keep_cols]

      # 5. Uncertainty summarisation
      dd <- melt(
        dd,
        id.vars = c("mc", "year", "sex", "agegrp", "uptake_group")
      )

      dd <- dd[
        ,
        fquantile_byid(value, prbl, id = as.character(variable)),
        keyby = c("year","sex","agegrp","uptake_group")
      ]

      setnames(
        dd,
        c("year", "sex", "agegrp", "uptake_group", "disease",
          percent(prbl, prefix = "mrtl_rate_"))
      )

      dd[, scenario := sc]

      d_out <- rbind(d_out, dd, use.names = TRUE)

      # Clean structurally empty rows
      stat_cols <- grep("%$", names(d_out), value = TRUE)
      d_out <- na.omit(d_out, cols=stat_cols)

    }


    fwrite(d_out, paste0(out_path_tables, "disease_mortality_numbers_diff_by_year_age_sex.csv"), sep = ";")

    ggplot(d_out, aes(x = year, y = `mrtl_rate_50.0%`,
                      ymin = `mrtl_rate_2.5%`,
                      ymax = `mrtl_rate_97.5%`,
                      colour = agegrp, fill = agegrp, linetype = scenario)) +
      facet_wrap(~ disease + sex, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Mortality") +
      ggtitle("Difference in mortality numbers of diseases compared to baseline over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "disease_mortality_numbers_diff_by_year_age_sex.", plot_format),
           height = 9, width = 16)


    ## Disease-specific Mortality by sex ## ----

    # WARNING: For some reason some iterations have a trailing comma!
    file_lines <- readLines(paste0(in_path, "dis_mrtl_scaled_up.csv.gz"))
    writeLines(gsub(",+$", "", file_lines), paste0(in_path, "dis_mrtl_scaled_up.csv.gz"))

    tt <- fread(paste0(in_path, "dis_mrtl_scaled_up.csv.gz"), fill = TRUE,
    )[, `:=` (year = year + 2000)]

    outstrata <- c("mc", "sex", "year", "scenario", "uptake_group")

    # Rate #

    d <- tt[, lapply(.SD, sum), .SDcols = c("nonmodelled", "chd", "stroke", "popsize"), keyby = eval(outstrata)
    ][, lapply(.SD, function(x) x/popsize), keyby = outstrata]
    d <- melt(d, id.vars = outstrata)
    d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
    setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "mrtl_rate_")))

    fwrite(d, paste0(out_path_tables, "disease_mortality_rate_by_year_sex.csv"), sep = ";")

    ggplot(d[disease != "popsize"], aes(x = year, y = `mrtl_rate_50.0%`,
                                        ymin = `mrtl_rate_2.5%`,
                                        ymax = `mrtl_rate_97.5%`,
                                        linetype = scenario)) +
      facet_wrap(~ disease + sex, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Mortality") +
      ggtitle("Mortality rate of diseases by scenario over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "disease_mortality_rate_by_year_sex.", plot_format),
           height = 9, width = 16)

    # Rate difference #

    d <- tt[, lapply(.SD, sum), .SDcols = c("nonmodelled", "chd", "stroke", "popsize"), keyby = eval(outstrata)
    ][, lapply(.SD, function(x) x/popsize), keyby = outstrata]

    prvls <- c("nonmodelled", "chd", "stroke")

    d <- melt(d, id.vars = outstrata)
    d <- dcast(d, mc + year + sex + uptake_group ~ scenario + variable)

    d_out <- data.table(NULL)

    for (k in seq_len(nrow(sc_map))) {

      sc <- sc_map$scenario[k]
      bl <- sc_map$baseline[k]

      sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
      bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

      if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

      # Safety check: same variables, same order
      stopifnot(
        sub(paste0("^", sc), "", sc_cols) ==
          sub(paste0("^", bl), "", bl_cols)
      )

      dd <- copy(d)

      # Compute differences
      for (i in seq_along(sc_cols)) {
        diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
        dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
      }

      # Keep only needed columns
      keep_cols <- c(
        "mc", "year", "sex", "uptake_group",
        grep("^diff_", names(dd), value = TRUE)
      )
      dd <- dd[, ..keep_cols]

      # 5. Uncertainty summarisation
      dd <- melt(
        dd,
        id.vars = c("mc", "year", "sex", "uptake_group")
      )

      dd <- dd[
        ,
        fquantile_byid(value, prbl, id = as.character(variable)),
        keyby = c("year","sex","uptake_group")
      ]

      setnames(
        dd,
        c("year", "sex", "uptake_group", "disease",
          percent(prbl, prefix = "mrtl_rate_"))
      )

      dd[, scenario := sc]

      d_out <- rbind(d_out, dd, use.names = TRUE)

      # Clean structurally empty rows
      stat_cols <- grep("%$", names(d_out), value = TRUE)
      d_out <- na.omit(d_out, cols=stat_cols)

    }

    fwrite(d_out, paste0(out_path_tables, "disease_mortality_rate_diff_by_year_sex.csv"), sep = ";")

    ggplot(d_out, aes(x = year, y = `mrtl_rate_50.0%`,
                      ymin = `mrtl_rate_2.5%`,
                      ymax = `mrtl_rate_97.5%`,
                      linetype = scenario)) +
      facet_wrap(~ disease + sex, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Mortality") +
      ggtitle("Difference in mortality rate of diseases compared to baseline over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "disease_mortality_rate_diff_by_year_sex.", plot_format),
           height = 9, width = 16)


    # Absolute numbers #

    d <- tt[, lapply(.SD, sum), .SDcols = c("nonmodelled", "chd", "stroke", "popsize"), keyby = eval(outstrata)]
    d <- melt(d, id.vars = outstrata)
    d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
    setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "mrtl_rate_")))

    fwrite(d, paste0(out_path_tables, "disease_mortality_numbers_by_year_sex.csv"), sep = ";")

    ggplot(d[disease != "popsize"], aes(x = year, y = `mrtl_rate_50.0%`,
                                        ymin = `mrtl_rate_2.5%`,
                                        ymax = `mrtl_rate_97.5%`,
                                        linetype = scenario)) +
      facet_wrap(~ disease + sex, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Mortality") +
      ggtitle("Mortality numbers of diseases by scenario over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "disease_mortality_numbers_by_year_sex.", plot_format),
           height = 9, width = 16)

    # Absolute difference #

    d <- tt[, lapply(.SD, sum), .SDcols = c("nonmodelled", "chd", "stroke", "popsize"), keyby = eval(outstrata)]

    prvls <- c("nonmodelled", "chd", "stroke")

    d <- melt(d, id.vars = outstrata)
    d <- dcast(d, mc + year + sex + uptake_group ~ scenario + variable)

    d_out <- data.table(NULL)

    for (k in seq_len(nrow(sc_map))) {

      sc <- sc_map$scenario[k]
      bl <- sc_map$baseline[k]

      sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
      bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

      if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

      # Safety check: same variables, same order
      stopifnot(
        sub(paste0("^", sc), "", sc_cols) ==
          sub(paste0("^", bl), "", bl_cols)
      )

      dd <- copy(d)

      # Compute differences
      for (i in seq_along(sc_cols)) {
        diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
        dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
      }

      # Keep only needed columns
      keep_cols <- c(
        "mc", "year", "sex", "uptake_group",
        grep("^diff_", names(dd), value = TRUE)
      )
      dd <- dd[, ..keep_cols]

      # 5. Uncertainty summarisation
      dd <- melt(
        dd,
        id.vars = c("mc", "year", "sex", "uptake_group")
      )

      dd <- dd[
        ,
        fquantile_byid(value, prbl, id = as.character(variable)),
        keyby = c("year","sex","uptake_group")
      ]

      setnames(
        dd,
        c("year", "sex", "uptake_group", "disease",
          percent(prbl, prefix = "mrtl_rate_"))
      )

      dd[, scenario := sc]

      d_out <- rbind(d_out, dd, use.names = TRUE)

      # Clean structurally empty rows
      stat_cols <- grep("%$", names(d_out), value = TRUE)
      d_out <- na.omit(d_out, cols=stat_cols)

    }

    fwrite(d_out, paste0(out_path_tables, "disease_mortality_numbers_diff_by_year_sex.csv"), sep = ";")

    ggplot(d_out, aes(x = year, y = `mrtl_rate_50.0%`,
                      ymin = `mrtl_rate_2.5%`,
                      ymax = `mrtl_rate_97.5%`,
                      linetype = scenario)) +
      facet_wrap(~ disease + sex, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Mortality") +
      ggtitle("Difference in mortality numbers of diseases compared to baseline over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "disease_mortality_numbers_diff_by_year_sex.", plot_format),
           height = 9, width = 16)


    ## Disease-specific Mortality by age ## ----

    # WARNING: For some reason some iterations have a trailing comma!
    file_lines <- readLines(paste0(in_path, "dis_mrtl_scaled_up.csv.gz"))
    writeLines(gsub(",+$", "", file_lines), paste0(in_path, "dis_mrtl_scaled_up.csv.gz"))

    tt <- fread(paste0(in_path, "dis_mrtl_scaled_up.csv.gz"), fill = TRUE,
    )[, `:=` (year = year + 2000,
              agegrp = fifelse(agegrp %in% c("30-34", "35-39", "40-44", "45-49"), "30-49",
                               ifelse(agegrp %in% c("50-54", "55-59", "60-64", "65-69"), "50-69",
                                      "70-90")))]


    outstrata <- c("mc", "agegrp", "year", "scenario", "uptake_group")

    # Rate #

    d <- tt[, lapply(.SD, sum), .SDcols = c("nonmodelled", "chd", "stroke", "popsize"), keyby = eval(outstrata)
    ][, lapply(.SD, function(x) x/popsize), keyby = outstrata]
    d <- melt(d, id.vars = outstrata)
    d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
    setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "mrtl_rate_")))

    fwrite(d, paste0(out_path_tables, "disease_mortality_rate_by_year_age.csv"), sep = ";")

    ggplot(d[disease != "popsize"], aes(x = year, y = `mrtl_rate_50.0%`,
                                        ymin = `mrtl_rate_2.5%`,
                                        ymax = `mrtl_rate_97.5%`,
                                        colour = agegrp, fill = agegrp, linetype = scenario)) +
      facet_wrap(~ disease, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Mortality") +
      ggtitle("Mortality rate of diseases by scenario over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "disease_mortality_rate_by_year_age.", plot_format),
           height = 9, width = 16)

    # Rate difference #

    d <- tt[, lapply(.SD, sum), .SDcols = c("nonmodelled", "chd", "stroke", "popsize"), keyby = eval(outstrata)
    ][, lapply(.SD, function(x) x/popsize), keyby = outstrata]

    prvls <- c("nonmodelled", "chd", "stroke")

    d <- melt(d, id.vars = outstrata)
    d <- dcast(d, mc + year + agegrp + uptake_group ~ scenario + variable)

    d_out <- data.table(NULL)

    for (k in seq_len(nrow(sc_map))) {

      sc <- sc_map$scenario[k]
      bl <- sc_map$baseline[k]

      sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
      bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

      if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

      # Safety check: same variables, same order
      stopifnot(
        sub(paste0("^", sc), "", sc_cols) ==
          sub(paste0("^", bl), "", bl_cols)
      )

      dd <- copy(d)

      # Compute differences
      for (i in seq_along(sc_cols)) {
        diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
        dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
      }

      # Keep only needed columns
      keep_cols <- c(
        "mc", "year", "agegrp","uptake_group",
        grep("^diff_", names(dd), value = TRUE)
      )
      dd <- dd[, ..keep_cols]

      # 5. Uncertainty summarisation
      dd <- melt(
        dd,
        id.vars = c("mc", "year", "agegrp","uptake_group")
      )

      dd <- dd[
        ,
        fquantile_byid(value, prbl, id = as.character(variable)),
        keyby = c("year","agegrp","uptake_group")
      ]

      setnames(
        dd,
        c("year","agegrp", "uptake_group", "disease",
          percent(prbl, prefix = "mrtl_rate_"))
      )

      dd[, scenario := sc]

      d_out <- rbind(d_out, dd, use.names = TRUE)

      # Clean structurally empty rows
      stat_cols <- grep("%$", names(d_out), value = TRUE)
      d_out <- na.omit(d_out, cols=stat_cols)

    }

    fwrite(d_out, paste0(out_path_tables, "disease_mortality_rate_diff_by_year_age.csv"), sep = ";")

    ggplot(d_out, aes(x = year, y = `mrtl_rate_50.0%`,
                      ymin = `mrtl_rate_2.5%`,
                      ymax = `mrtl_rate_97.5%`,
                      colour = agegrp, fill = agegrp, linetype = scenario)) +
      facet_wrap(~ disease, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Mortality") +
      ggtitle("Difference in mortality rate of diseases compared to baseline over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "disease_mortality_rate_diff_by_year_age.", plot_format),
           height = 9, width = 16)


    # Absolute numbers #

    d <- tt[, lapply(.SD, sum), .SDcols = c("nonmodelled", "chd", "stroke", "popsize"), keyby = eval(outstrata)]
    d <- melt(d, id.vars = outstrata)
    d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
    setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "mrtl_rate_")))

    fwrite(d, paste0(out_path_tables, "disease_mortality_numbers_by_year_age.csv"), sep = ";")

    ggplot(d[disease != "popsize"], aes(x = year, y = `mrtl_rate_50.0%`,
                                        ymin = `mrtl_rate_2.5%`,
                                        ymax = `mrtl_rate_97.5%`,
                                        colour = agegrp, fill = agegrp, linetype = scenario)) +
      facet_wrap(~ disease, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Mortality") +
      ggtitle("Mortality numbers of diseases by scenario over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "disease_mortality_numbers_by_year_age.", plot_format),
           height = 9, width = 16)

    # Absolute difference #

    d <- tt[, lapply(.SD, sum), .SDcols = c("nonmodelled", "chd", "stroke", "popsize"), keyby = eval(outstrata)]

    prvls <- c("nonmodelled", "chd", "stroke")

    d <- melt(d, id.vars = outstrata)
    d <- dcast(d, mc + year + agegrp + uptake_group ~ scenario + variable)

    d_out <- data.table(NULL)

    for (k in seq_len(nrow(sc_map))) {

      sc <- sc_map$scenario[k]
      bl <- sc_map$baseline[k]

      sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
      bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

      if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

      # Safety check: same variables, same order
      stopifnot(
        sub(paste0("^", sc), "", sc_cols) ==
          sub(paste0("^", bl), "", bl_cols)
      )

      dd <- copy(d)

      # Compute differences
      for (i in seq_along(sc_cols)) {
        diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
        dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
      }

      # Keep only needed columns
      keep_cols <- c(
        "mc", "year", "agegrp","uptake_group",
        grep("^diff_", names(dd), value = TRUE)
      )
      dd <- dd[, ..keep_cols]

      # 5. Uncertainty summarisation
      dd <- melt(
        dd,
        id.vars = c("mc", "year", "agegrp","uptake_group")
      )

      dd <- dd[
        ,
        fquantile_byid(value, prbl, id = as.character(variable)),
        keyby = c("year","agegrp","uptake_group")
      ]

      setnames(
        dd,
        c("year","agegrp", "uptake_group", "disease",
          percent(prbl, prefix = "mrtl_rate_"))
      )

      dd[, scenario := sc]

      d_out <- rbind(d_out, dd, use.names = TRUE)

      # Clean structurally empty rows
      stat_cols <- grep("%$", names(d_out), value = TRUE)
      d_out <- na.omit(d_out, cols=stat_cols)

    }

    fwrite(d_out, paste0(out_path_tables, "disease_mortality_numbers_diff_by_year_age.csv"), sep = ";")

    ggplot(d_out, aes(x = year, y = `mrtl_rate_50.0%`,
                      ymin = `mrtl_rate_2.5%`,
                      ymax = `mrtl_rate_97.5%`,
                      colour = agegrp, fill = agegrp, linetype = scenario)) +
      facet_wrap(~ disease, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Mortality") +
      ggtitle("Difference in mortality numbers of diseases compared to baseline over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "disease_mortality_numbers_diff_by_year_age.", plot_format),
           height = 9, width = 16)


    ## Disease-specific Mortality total ## ----

    # WARNING: For some reason some iterations have a trailing comma!
    file_lines <- readLines(paste0(in_path, "dis_mrtl_scaled_up.csv.gz"))
    writeLines(gsub(",+$", "", file_lines), paste0(in_path, "dis_mrtl_scaled_up.csv.gz"))

    tt <- fread(paste0(in_path, "dis_mrtl_scaled_up.csv.gz"), fill = TRUE,
    )[, `:=` (year = year + 2000)]

    outstrata <- c("mc", "year", "scenario", "uptake_group")

    # Rate #

    d <- tt[, lapply(.SD, sum), .SDcols = c("nonmodelled", "chd", "stroke", "popsize"), keyby = eval(outstrata)
    ][, lapply(.SD, function(x) x/popsize), keyby = outstrata]
    d <- melt(d, id.vars = outstrata)
    d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
    setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "mrtl_rate_")))

    fwrite(d, paste0(out_path_tables, "disease_mortality_rate_by_year.csv"), sep = ";")

    ggplot(d[disease != "popsize"], aes(x = year, y = `mrtl_rate_50.0%`,
                                        ymin = `mrtl_rate_2.5%`,
                                        ymax = `mrtl_rate_97.5%`,
                                        linetype = scenario)) +
      facet_wrap(~ disease, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Mortality") +
      ggtitle("Mortality rate of diseases by scenario over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "disease_mortality_rate_by_year.", plot_format),
           height = 9, width = 16)

    # Rate difference #

    d <- tt[, lapply(.SD, sum), .SDcols = c("nonmodelled", "chd", "stroke", "popsize"), keyby = eval(outstrata)
    ][, lapply(.SD, function(x) x/popsize), keyby = outstrata]

    prvls <- c("nonmodelled", "chd", "stroke")

    d <- melt(d, id.vars = outstrata)
    d <- dcast(d, mc + year + uptake_group ~ scenario + variable)

    d_out <- data.table(NULL)

    for (k in seq_len(nrow(sc_map))) {

      sc <- sc_map$scenario[k]
      bl <- sc_map$baseline[k]

      sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
      bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

      if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

      # Safety check: same variables, same order
      stopifnot(
        sub(paste0("^", sc), "", sc_cols) ==
          sub(paste0("^", bl), "", bl_cols)
      )

      dd <- copy(d)

      # Compute differences
      for (i in seq_along(sc_cols)) {
        diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
        dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
      }

      # Keep only needed columns
      keep_cols <- c(
        "mc", "year","uptake_group",
        grep("^diff_", names(dd), value = TRUE)
      )
      dd <- dd[, ..keep_cols]

      # 5. Uncertainty summarisation
      dd <- melt(
        dd,
        id.vars = c("mc", "year","uptake_group")
      )

      dd <- dd[
        ,
        fquantile_byid(value, prbl, id = as.character(variable)),
        keyby = c("year","uptake_group")
      ]

      setnames(
        dd,
        c("year", "uptake_group", "disease",
          percent(prbl, prefix = "mrtl_rate_"))
      )

      dd[, scenario := sc]

      d_out <- rbind(d_out, dd, use.names = TRUE)

      # Clean structurally empty rows
      stat_cols <- grep("%$", names(d_out), value = TRUE)
      d_out <- na.omit(d_out, cols=stat_cols)

    }

    fwrite(d_out, paste0(out_path_tables, "disease_mortality_rate_diff_by_year.csv"), sep = ";")

    ggplot(d_out, aes(x = year, y = `mrtl_rate_50.0%`,
                      ymin = `mrtl_rate_2.5%`,
                      ymax = `mrtl_rate_97.5%`,
                      linetype = scenario)) +
      facet_wrap(~ disease, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Mortality") +
      ggtitle("Difference in mortality rate of diseases compared to baseline over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "disease_mortality_rate_diff_by_year.", plot_format),
           height = 9, width = 16)


    # Absolute numbers #

    d <- tt[, lapply(.SD, sum), .SDcols = c("nonmodelled", "chd", "stroke", "popsize"), keyby = eval(outstrata)]
    d <- melt(d, id.vars = outstrata)
    d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
    setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "mrtl_rate_")))

    fwrite(d, paste0(out_path_tables, "disease_mortality_numbers_by_year.csv"), sep = ";")

    ggplot(d[disease != "popsize"], aes(x = year, y = `mrtl_rate_50.0%`,
                                        ymin = `mrtl_rate_2.5%`,
                                        ymax = `mrtl_rate_97.5%`,
                                        linetype = scenario)) +
      facet_wrap(~ disease, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Mortality") +
      ggtitle("Mortality numbers of diseases by scenario over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "disease_mortality_numbers_by_year.", plot_format),
           height = 9, width = 16)

    # Absolute difference #

    d <- tt[, lapply(.SD, sum), .SDcols = c("nonmodelled", "chd", "stroke", "popsize"), keyby = eval(outstrata)]

    prvls <- c("nonmodelled", "chd", "stroke")

    d <- melt(d, id.vars = outstrata)
    d <- dcast(d, mc + year + uptake_group ~ scenario + variable)

    d_out <- data.table(NULL)

    for (k in seq_len(nrow(sc_map))) {

      sc <- sc_map$scenario[k]
      bl <- sc_map$baseline[k]

      sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
      bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

      if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

      # Safety check: same variables, same order
      stopifnot(
        sub(paste0("^", sc), "", sc_cols) ==
          sub(paste0("^", bl), "", bl_cols)
      )

      dd <- copy(d)

      # Compute differences
      for (i in seq_along(sc_cols)) {
        diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
        dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
      }

      # Keep only needed columns
      keep_cols <- c(
        "mc", "year","uptake_group",
        grep("^diff_", names(dd), value = TRUE)
      )
      dd <- dd[, ..keep_cols]

      # 5. Uncertainty summarisation
      dd <- melt(
        dd,
        id.vars = c("mc", "year","uptake_group")
      )

      dd <- dd[
        ,
        fquantile_byid(value, prbl, id = as.character(variable)),
        keyby = c("year","uptake_group")
      ]

      setnames(
        dd,
        c("year", "uptake_group", "disease",
          percent(prbl, prefix = "mrtl_rate_"))
      )

      dd[, scenario := sc]

      d_out <- rbind(d_out, dd, use.names = TRUE)

      # Clean structurally empty rows
      stat_cols <- grep("%$", names(d_out), value = TRUE)
      d_out <- na.omit(d_out, cols=stat_cols)

    }

    fwrite(d_out, paste0(out_path_tables, "disease_mortality_numbers_diff_by_year.csv"), sep = ";")

    ggplot(d_out, aes(x = year, y = `mrtl_rate_50.0%`,
                      ymin = `mrtl_rate_2.5%`,
                      ymax = `mrtl_rate_97.5%`,
                      linetype = scenario)) +
      facet_wrap(~ disease, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Mortality") +
      ggtitle("Difference in mortality numbers of diseases compared to baseline over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "disease_mortality_numbers_diff_by_year.", plot_format),
           height = 9, width = 16)
  }

  if("mrtl_scaled_up.csv.gz" %in% list.files(in_path)){

    ## All-cause Mortality by age and sex ## ----

    tt <- fread(paste0(in_path, "mrtl_scaled_up.csv.gz")
    )[, `:=` (year = year + 2000,
              agegrp = fifelse(agegrp %in% c("30-34", "35-39", "40-44", "45-49"), "30-49",
                               ifelse(agegrp %in% c("50-54", "55-59", "60-64", "65-69"), "50-69",
                                      "70-90")))]


    outstrata <- c("mc", "agegrp", "sex", "year", "scenario", "uptake_group")

    sc_n <- na.omit(as.numeric(gsub("[^1-9]+", "", unique(tt$scenario))))

    # Rate #

    d <- tt[, lapply(.SD, sum), .SDcols = c("all_cause_mrtl", "popsize"), keyby = eval(outstrata)
    ][, lapply(.SD, function(x) x/popsize), keyby = outstrata]
    d <- melt(d, id.vars = outstrata)
    d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
    setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "mrtl_rate_")))

    fwrite(d, paste0(out_path_tables, "mortality_rate_by_year_age_sex.csv"), sep = ";")

    ggplot(d[disease != "popsize"], aes(x = year, y = `mrtl_rate_50.0%`,
                                        ymin = `mrtl_rate_2.5%`,
                                        ymax = `mrtl_rate_97.5%`,
                                        colour = agegrp, fill = agegrp, linetype = scenario)) +
      facet_wrap(~ disease + sex, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Mortality") +
      ggtitle("Mortality rate by scenario over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "mortality_rate_by_year_age_sex.", plot_format),
           height = 9, width = 16)

    # Rate difference #

    d <- tt[, lapply(.SD, sum), .SDcols = c("all_cause_mrtl", "popsize"), keyby = eval(outstrata)
    ][, lapply(.SD, function(x) x/popsize), keyby = outstrata]

    prvls <- c("all_cause_mrtl")

    d <- melt(d, id.vars = outstrata)
    d <- dcast(d, mc + year + agegrp + sex + uptake_group ~ scenario + variable)

    d_out <- data.table(NULL)

    for (k in seq_len(nrow(sc_map))) {

      sc <- sc_map$scenario[k]
      bl <- sc_map$baseline[k]

      sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
      bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

      if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

      # Safety check: same variables, same order
      stopifnot(
        sub(paste0("^", sc), "", sc_cols) ==
          sub(paste0("^", bl), "", bl_cols)
      )

      dd <- copy(d)

      # Compute differences
      for (i in seq_along(sc_cols)) {
        diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
        dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
      }

      # Keep only needed columns
      keep_cols <- c(
        "mc", "year", "sex", "agegrp", "uptake_group",
        grep("^diff_", names(dd), value = TRUE)
      )
      dd <- dd[, ..keep_cols]

      # 5. Uncertainty summarisation
      dd <- melt(
        dd,
        id.vars = c("mc", "year", "sex", "agegrp", "uptake_group")
      )

      dd <- dd[
        ,
        fquantile_byid(value, prbl, id = as.character(variable)),
        keyby = c("year","sex","agegrp","uptake_group")
      ]

      setnames(
        dd,
        c("year", "sex", "agegrp", "uptake_group", "disease",
          percent(prbl, prefix = "mrtl_rate_"))
      )

      dd[, scenario := sc]

      d_out <- rbind(d_out, dd, use.names = TRUE)

      # Clean structurally empty rows
      stat_cols <- grep("%$", names(d_out), value = TRUE)
      d_out <- na.omit(d_out, cols=stat_cols)

    }

    fwrite(d_out, paste0(out_path_tables, "mortality_rate_diff_by_year_age_sex.csv"), sep = ";")

    ggplot(d_out, aes(x = year, y = `mrtl_rate_50.0%`,
                      ymin = `mrtl_rate_2.5%`,
                      ymax = `mrtl_rate_97.5%`,
                      colour = agegrp, fill = agegrp, linetype = scenario)) +
      facet_wrap(~ disease + sex, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Mortality") +
      ggtitle("Difference in mortality rate compared to baseline over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "mortality_rate_diff_by_year_age_sex.", plot_format),
           height = 9, width = 16)


    # Absolute numbers #

    d <- tt[, lapply(.SD, sum), .SDcols = c("all_cause_mrtl"), keyby = eval(outstrata)]
    d <- melt(d, id.vars = outstrata)
    d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
    setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "mrtl_rate_")))

    fwrite(d, paste0(out_path_tables, "mortality_numbers_by_year_age_sex.csv"), sep = ";")

    ggplot(d, aes(x = year, y = `mrtl_rate_50.0%`,
                  ymin = `mrtl_rate_2.5%`,
                  ymax = `mrtl_rate_97.5%`,
                  colour = agegrp, fill = agegrp, linetype = scenario)) +
      facet_wrap(~ disease + sex, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Mortality") +
      ggtitle("Mortality numbers by scenario over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "mortality_numbers_by_year_age_sex.", plot_format),
           height = 9, width = 16)

    # Absolute difference #

    d <- tt[, lapply(.SD, sum), .SDcols = c("all_cause_mrtl"), keyby = eval(outstrata)]

    prvls <- c("all_cause_mrtl")

    d <- melt(d, id.vars = outstrata)
    d <- dcast(d, mc + year + agegrp + sex + uptake_group ~ scenario + variable)

    d_out <- data.table(NULL)

    for (k in seq_len(nrow(sc_map))) {

      sc <- sc_map$scenario[k]
      bl <- sc_map$baseline[k]

      sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
      bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

      if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

      # Safety check: same variables, same order
      stopifnot(
        sub(paste0("^", sc), "", sc_cols) ==
          sub(paste0("^", bl), "", bl_cols)
      )

      dd <- copy(d)

      # Compute differences
      for (i in seq_along(sc_cols)) {
        diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
        dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
      }

      # Keep only needed columns
      keep_cols <- c(
        "mc", "year", "sex", "agegrp", "uptake_group",
        grep("^diff_", names(dd), value = TRUE)
      )
      dd <- dd[, ..keep_cols]

      # 5. Uncertainty summarisation
      dd <- melt(
        dd,
        id.vars = c("mc", "year", "sex", "agegrp", "uptake_group")
      )

      dd <- dd[
        ,
        fquantile_byid(value, prbl, id = as.character(variable)),
        keyby = c("year","sex","agegrp","uptake_group")
      ]

      setnames(
        dd,
        c("year", "sex", "agegrp", "uptake_group", "disease",
          percent(prbl, prefix = "mrtl_rate_"))
      )

      dd[, scenario := sc]

      d_out <- rbind(d_out, dd, use.names = TRUE)

      # Clean structurally empty rows
      stat_cols <- grep("%$", names(d_out), value = TRUE)
      d_out <- na.omit(d_out, cols=stat_cols)

    }

    fwrite(d_out, paste0(out_path_tables, "mortality_numbers_diff_by_year_age_sex.csv"), sep = ";")

    ggplot(d_out, aes(x = year, y = `mrtl_rate_50.0%`,
                      ymin = `mrtl_rate_2.5%`,
                      ymax = `mrtl_rate_97.5%`,
                      colour = agegrp, fill = agegrp, linetype = scenario)) +
      facet_wrap(~ disease + sex, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Mortality") +
      ggtitle("Difference in mortality numbers compared to baseline over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "mortality_numbers_diff_by_year_age_sex.", plot_format),
           height = 9, width = 16)


    # Deaths prevented or postponed #

    d <- tt[, lapply(.SD, sum), .SDcols = c("all_cause_mrtl"), keyby = eval(outstrata)]

    prvls <- c("all_cause_mrtl")

    d <- melt(d, id.vars = outstrata)
    d <- dcast(d, mc + sex + agegrp + uptake_group ~ scenario + variable, fun.aggregate = sum) # sum over years

    d_out <- data.table(NULL)

    for (k in seq_len(nrow(sc_map))) {

      sc <- sc_map$scenario[k]
      bl <- sc_map$baseline[k]

      sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
      bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

      if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

      # Safety check: same variables, same order
      stopifnot(
        sub(paste0("^", sc), "", sc_cols) ==
          sub(paste0("^", bl), "", bl_cols)
      )

      dd <- copy(d)

      # Compute differences
      for (i in seq_along(sc_cols)) {
        diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
        dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
      }

      # Keep only needed columns
      keep_cols <- c(
        "mc", "sex", "agegrp", "uptake_group",
        grep("^diff_", names(dd), value = TRUE)
      )
      dd <- dd[, ..keep_cols]

      # 5. Uncertainty summarisation
      dd <- melt(
        dd,
        id.vars = c("mc", "sex", "agegrp", "uptake_group")
      )

      dd <- dd[
        ,
        fquantile_byid(value, prbl, id = as.character(variable)),
        keyby = c("sex","agegrp","uptake_group")
      ]

      setnames(
        dd,
        c("sex", "agegrp", "uptake_group", "disease",
          percent(prbl, prefix = "mrtl_rate_"))
      )

      dd[, scenario := sc]

      d_out <- rbind(d_out, dd, use.names = TRUE)

      # Clean structurally empty rows
      stat_cols <- grep("%$", names(d_out), value = TRUE)
      d_out <- na.omit(d_out, cols=stat_cols)

    }

    fwrite(d_out, paste0(out_path_tables, "deaths_prev_post_by_scenario_agegrp_sex.csv"), sep = ";")


    ## All-cause Mortality by sex ## ----

    tt <- fread(paste0(in_path, "mrtl_scaled_up.csv.gz")
    )[, `:=` (year = year + 2000,
              agegrp = fifelse(agegrp %in% c("30-34", "35-39", "40-44", "45-49"), "30-49",
                               ifelse(agegrp %in% c("50-54", "55-59", "60-64", "65-69"), "50-69",
                                      "70-90")))]


    outstrata <- c("mc", "sex", "year", "scenario", "uptake_group")

    sc_n <- na.omit(as.numeric(gsub("[^1-9]+", "", unique(tt$scenario))))

    # Rate #

    d <- tt[, lapply(.SD, sum), .SDcols = c("all_cause_mrtl", "popsize"), keyby = eval(outstrata)
    ][, lapply(.SD, function(x) x/popsize), keyby = outstrata]
    d <- melt(d, id.vars = outstrata)
    d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
    setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "mrtl_rate_")))

    fwrite(d, paste0(out_path_tables, "mortality_rate_by_year_sex.csv"), sep = ";")

    ggplot(d[disease != "popsize"], aes(x = year, y = `mrtl_rate_50.0%`,
                                        ymin = `mrtl_rate_2.5%`,
                                        ymax = `mrtl_rate_97.5%`,
                                        linetype = scenario)) +
      facet_wrap(~ disease + sex, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Mortality") +
      ggtitle("Mortality rate by scenario over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "mortality_rate_by_year_sex.", plot_format),
           height = 9, width = 16)

    # Rate difference #

    d <- tt[, lapply(.SD, sum), .SDcols = c("all_cause_mrtl", "popsize"), keyby = eval(outstrata)
    ][, lapply(.SD, function(x) x/popsize), keyby = outstrata]

    prvls <- c("all_cause_mrtl")

    d <- melt(d, id.vars = outstrata)
    d <- dcast(d, mc + year + sex + uptake_group ~ scenario + variable, fun.aggregate = sum) # sum over ages

    d_out <- data.table(NULL)

    for (k in seq_len(nrow(sc_map))) {

      sc <- sc_map$scenario[k]
      bl <- sc_map$baseline[k]

      sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
      bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

      if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

      # Safety check: same variables, same order
      stopifnot(
        sub(paste0("^", sc), "", sc_cols) ==
          sub(paste0("^", bl), "", bl_cols)
      )

      dd <- copy(d)

      # Compute differences
      for (i in seq_along(sc_cols)) {
        diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
        dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
      }

      # Keep only needed columns
      keep_cols <- c(
        "mc", "year", "sex", "uptake_group",
        grep("^diff_", names(dd), value = TRUE)
      )
      dd <- dd[, ..keep_cols]

      # 5. Uncertainty summarisation
      dd <- melt(
        dd,
        id.vars = c("mc", "year", "sex", "uptake_group")
      )

      dd <- dd[
        ,
        fquantile_byid(value, prbl, id = as.character(variable)),
        keyby = c("year","sex","uptake_group")
      ]

      setnames(
        dd,
        c("year", "sex", "uptake_group", "disease",
          percent(prbl, prefix = "mrtl_rate_"))
      )

      dd[, scenario := sc]

      d_out <- rbind(d_out, dd, use.names = TRUE)

      # Clean structurally empty rows
      stat_cols <- grep("%$", names(d_out), value = TRUE)
      d_out <- na.omit(d_out, cols=stat_cols)

    }

    fwrite(d_out, paste0(out_path_tables, "mortality_rate_diff_by_year_sex.csv"), sep = ";")

    ggplot(d_out, aes(x = year, y = `mrtl_rate_50.0%`,
                      ymin = `mrtl_rate_2.5%`,
                      ymax = `mrtl_rate_97.5%`,
                      linetype = scenario)) +
      facet_wrap(~ disease + sex, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Mortality") +
      ggtitle("Difference in mortality rate compared to baseline over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "mortality_rate_diff_by_year_sex.", plot_format),
           height = 9, width = 16)


    # Absolute numbers #

    d <- tt[, lapply(.SD, sum), .SDcols = c("all_cause_mrtl"), keyby = eval(outstrata)]
    d <- melt(d, id.vars = outstrata)
    d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
    setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "mrtl_rate_")))

    fwrite(d, paste0(out_path_tables, "mortality_numbers_by_year_sex.csv"), sep = ";")

    ggplot(d, aes(x = year, y = `mrtl_rate_50.0%`,
                  ymin = `mrtl_rate_2.5%`,
                  ymax = `mrtl_rate_97.5%`,
                  linetype = scenario)) +
      facet_wrap(~ disease + sex, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Mortality") +
      ggtitle("Mortality numbers by scenario over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "mortality_numbers_by_year_sex.", plot_format),
           height = 9, width = 16)

    # Absolute difference #

    d <- tt[, lapply(.SD, sum), .SDcols = c("all_cause_mrtl"), keyby = eval(outstrata)]

    prvls <- c("all_cause_mrtl")

    d <- melt(d, id.vars = outstrata)
    d <- dcast(d, mc + year + sex + uptake_group ~ scenario + variable, fun.aggregate = sum)

    d_out <- data.table(NULL)

    for (k in seq_len(nrow(sc_map))) {

      sc <- sc_map$scenario[k]
      bl <- sc_map$baseline[k]

      sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
      bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

      if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

      # Safety check: same variables, same order
      stopifnot(
        sub(paste0("^", sc), "", sc_cols) ==
          sub(paste0("^", bl), "", bl_cols)
      )

      dd <- copy(d)

      # Compute differences
      for (i in seq_along(sc_cols)) {
        diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
        dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
      }

      # Keep only needed columns
      keep_cols <- c(
        "mc", "year", "sex", "uptake_group",
        grep("^diff_", names(dd), value = TRUE)
      )
      dd <- dd[, ..keep_cols]

      # 5. Uncertainty summarisation
      dd <- melt(
        dd,
        id.vars = c("mc", "year", "sex", "uptake_group")
      )

      dd <- dd[
        ,
        fquantile_byid(value, prbl, id = as.character(variable)),
        keyby = c("year","sex","uptake_group")
      ]

      setnames(
        dd,
        c("year", "sex", "uptake_group", "disease",
          percent(prbl, prefix = "mrtl_rate_"))
      )

      dd[, scenario := sc]

      d_out <- rbind(d_out, dd, use.names = TRUE)

      # Clean structurally empty rows
      stat_cols <- grep("%$", names(d_out), value = TRUE)
      d_out <- na.omit(d_out, cols=stat_cols)

    }

    fwrite(d_out, paste0(out_path_tables, "mortality_numbers_diff_by_year_sex.csv"), sep = ";")

    ggplot(d_out, aes(x = year, y = `mrtl_rate_50.0%`,
                      ymin = `mrtl_rate_2.5%`,
                      ymax = `mrtl_rate_97.5%`,
                      linetype = scenario)) +
      facet_wrap(~ disease + sex, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Mortality") +
      ggtitle("Difference in mortality numbers compared to baseline over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "mortality_numbers_diff_by_year_sex.", plot_format),
           height = 9, width = 16)


    # Deaths prevented or postponed #

    d <- tt[, lapply(.SD, sum), .SDcols = c("all_cause_mrtl"), keyby = eval(outstrata)]

    prvls <- c("all_cause_mrtl")

    d <- melt(d, id.vars = outstrata)
    d <- dcast(d, mc + sex + uptake_group ~ scenario + variable, fun.aggregate = sum) # sum over years

    d_out <- data.table(NULL)

    for (k in seq_len(nrow(sc_map))) {

      sc <- sc_map$scenario[k]
      bl <- sc_map$baseline[k]

      sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
      bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

      if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

      # Safety check: same variables, same order
      stopifnot(
        sub(paste0("^", sc), "", sc_cols) ==
          sub(paste0("^", bl), "", bl_cols)
      )

      dd <- copy(d)

      # Compute differences
      for (i in seq_along(sc_cols)) {
        diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
        dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
      }

      # Keep only needed columns
      keep_cols <- c(
        "mc", "sex", "uptake_group",
        grep("^diff_", names(dd), value = TRUE)
      )
      dd <- dd[, ..keep_cols]

      # 5. Uncertainty summarisation
      dd <- melt(
        dd,
        id.vars = c("mc", "sex", "uptake_group")
      )

      dd <- dd[
        ,
        fquantile_byid(value, prbl, id = as.character(variable)),
        keyby = c("sex","uptake_group")
      ]

      setnames(
        dd,
        c("sex", "uptake_group", "disease",
          percent(prbl, prefix = "mrtl_rate_"))
      )

      dd[, scenario := sc]

      d_out <- rbind(d_out, dd, use.names = TRUE)

      # Clean structurally empty rows
      stat_cols <- grep("%$", names(d_out), value = TRUE)
      d_out <- na.omit(d_out, cols=stat_cols)

    }

    fwrite(d_out, paste0(out_path_tables, "deaths_prev_post_by_scenario_sex.csv"), sep = ";")


    ## All-cause Mortality by age ## ----

    tt <- fread(paste0(in_path, "mrtl_scaled_up.csv.gz")
    )[, `:=` (year = year + 2000,
              agegrp = fifelse(agegrp %in% c("30-34", "35-39", "40-44", "45-49"), "30-49",
                               ifelse(agegrp %in% c("50-54", "55-59", "60-64", "65-69"), "50-69",
                                      "70-90")))]


    outstrata <- c("mc", "agegrp", "year", "scenario", "uptake_group")

    sc_n <- na.omit(as.numeric(gsub("[^1-9]+", "", unique(tt$scenario))))

    # Rate #

    d <- tt[, lapply(.SD, sum), .SDcols = c("all_cause_mrtl", "popsize"), keyby = eval(outstrata)
    ][, lapply(.SD, function(x) x/popsize), keyby = outstrata]
    d <- melt(d, id.vars = outstrata)
    d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
    setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "mrtl_rate_")))

    fwrite(d, paste0(out_path_tables, "mortality_rate_by_year_age.csv"), sep = ";")

    ggplot(d[disease != "popsize"], aes(x = year, y = `mrtl_rate_50.0%`,
                                        ymin = `mrtl_rate_2.5%`,
                                        ymax = `mrtl_rate_97.5%`,
                                        linetype = scenario)) +
      facet_wrap(~ disease + agegrp, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Mortality") +
      ggtitle("Mortality rate by scenario over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "mortality_rate_by_year_age.", plot_format),
           height = 9, width = 16)

    # Rate difference #

    d <- tt[, lapply(.SD, sum), .SDcols = c("all_cause_mrtl", "popsize"), keyby = eval(outstrata)
    ][, lapply(.SD, function(x) x/popsize), keyby = outstrata]

    prvls <- c("all_cause_mrtl")

    d <- melt(d, id.vars = outstrata)
    d <- dcast(d, mc + year + agegrp + uptake_group ~ scenario + variable, fun.aggregate = sum) # sum over sexes

    d_out <- data.table(NULL)

    for (k in seq_len(nrow(sc_map))) {

      sc <- sc_map$scenario[k]
      bl <- sc_map$baseline[k]

      sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
      bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

      if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

      # Safety check: same variables, same order
      stopifnot(
        sub(paste0("^", sc), "", sc_cols) ==
          sub(paste0("^", bl), "", bl_cols)
      )

      dd <- copy(d)

      # Compute differences
      for (i in seq_along(sc_cols)) {
        diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
        dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
      }

      # Keep only needed columns
      keep_cols <- c(
        "mc", "year", "agegrp","uptake_group",
        grep("^diff_", names(dd), value = TRUE)
      )
      dd <- dd[, ..keep_cols]

      # 5. Uncertainty summarisation
      dd <- melt(
        dd,
        id.vars = c("mc", "year", "agegrp","uptake_group")
      )

      dd <- dd[
        ,
        fquantile_byid(value, prbl, id = as.character(variable)),
        keyby = c("year","agegrp","uptake_group")
      ]

      setnames(
        dd,
        c("year","agegrp", "uptake_group", "disease",
          percent(prbl, prefix = "mrtl_rate_"))
      )

      dd[, scenario := sc]

      d_out <- rbind(d_out, dd, use.names = TRUE)

      # Clean structurally empty rows
      stat_cols <- grep("%$", names(d_out), value = TRUE)
      d_out <- na.omit(d_out, cols=stat_cols)

    }

    fwrite(d_out, paste0(out_path_tables, "mortality_rate_diff_by_year_age.csv"), sep = ";")

    ggplot(d_out, aes(x = year, y = `mrtl_rate_50.0%`,
                      ymin = `mrtl_rate_2.5%`,
                      ymax = `mrtl_rate_97.5%`,
                      linetype = scenario)) +
      facet_wrap(~ disease + agegrp, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Mortality") +
      ggtitle("Difference in mortality rate compared to baseline over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "mortality_rate_diff_by_year_age.", plot_format),
           height = 9, width = 16)


    # Absolute numbers #

    d <- tt[, lapply(.SD, sum), .SDcols = c("all_cause_mrtl"), keyby = eval(outstrata)]
    d <- melt(d, id.vars = outstrata)
    d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
    setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "mrtl_rate_")))

    fwrite(d, paste0(out_path_tables, "mortality_numbers_by_year_age.csv"), sep = ";")

    ggplot(d, aes(x = year, y = `mrtl_rate_50.0%`,
                  ymin = `mrtl_rate_2.5%`,
                  ymax = `mrtl_rate_97.5%`,
                  linetype = scenario)) +
      facet_wrap(~ disease + agegrp, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Mortality") +
      ggtitle("Mortality numbers by scenario over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "mortality_numbers_by_year_age.", plot_format),
           height = 9, width = 16)

    # Absolute difference #

    d <- tt[, lapply(.SD, sum), .SDcols = c("all_cause_mrtl"), keyby = eval(outstrata)]

    prvls <- c("all_cause_mrtl")

    d <- melt(d, id.vars = outstrata)
    d <- dcast(d, mc + year + agegrp + uptake_group ~ scenario + variable, fun.aggregate = sum)

    d_out <- data.table(NULL)

    for (k in seq_len(nrow(sc_map))) {

      sc <- sc_map$scenario[k]
      bl <- sc_map$baseline[k]

      sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
      bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

      if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

      # Safety check: same variables, same order
      stopifnot(
        sub(paste0("^", sc), "", sc_cols) ==
          sub(paste0("^", bl), "", bl_cols)
      )

      dd <- copy(d)

      # Compute differences
      for (i in seq_along(sc_cols)) {
        diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
        dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
      }

      # Keep only needed columns
      keep_cols <- c(
        "mc", "year", "agegrp","uptake_group",
        grep("^diff_", names(dd), value = TRUE)
      )
      dd <- dd[, ..keep_cols]

      # 5. Uncertainty summarisation
      dd <- melt(
        dd,
        id.vars = c("mc", "year", "agegrp","uptake_group")
      )

      dd <- dd[
        ,
        fquantile_byid(value, prbl, id = as.character(variable)),
        keyby = c("year","agegrp","uptake_group")
      ]

      setnames(
        dd,
        c("year","agegrp", "uptake_group", "disease",
          percent(prbl, prefix = "mrtl_rate_"))
      )

      dd[, scenario := sc]

      d_out <- rbind(d_out, dd, use.names = TRUE)

      # Clean structurally empty rows
      stat_cols <- grep("%$", names(d_out), value = TRUE)
      d_out <- na.omit(d_out, cols=stat_cols)

    }

    fwrite(d_out, paste0(out_path_tables, "mortality_numbers_diff_by_year_age.csv"), sep = ";")

    ggplot(d_out, aes(x = year, y = `mrtl_rate_50.0%`,
                      ymin = `mrtl_rate_2.5%`,
                      ymax = `mrtl_rate_97.5%`,
                      linetype = scenario)) +
      facet_wrap(~ disease + agegrp, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Mortality") +
      ggtitle("Difference in mortality numbers compared to baseline over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "mortality_numbers_diff_by_year_age.", plot_format),
           height = 9, width = 16)


    # Deaths prevented or postponed #

    d <- tt[, lapply(.SD, sum), .SDcols = c("all_cause_mrtl"), keyby = eval(outstrata)]

    prvls <- c("all_cause_mrtl")

    d <- melt(d, id.vars = outstrata)
    d <- dcast(d, mc + agegrp + uptake_group ~ scenario + variable, fun.aggregate = sum) # sum over years

    d_out <- data.table(NULL)

    for (k in seq_len(nrow(sc_map))) {

      sc <- sc_map$scenario[k]
      bl <- sc_map$baseline[k]

      sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
      bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

      if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

      # Safety check: same variables, same order
      stopifnot(
        sub(paste0("^", sc), "", sc_cols) ==
          sub(paste0("^", bl), "", bl_cols)
      )

      dd <- copy(d)

      # Compute differences
      for (i in seq_along(sc_cols)) {
        diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
        dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
      }

      # Keep only needed columns
      keep_cols <- c(
        "mc", "agegrp", "uptake_group",
        grep("^diff_", names(dd), value = TRUE)
      )
      dd <- dd[, ..keep_cols]

      # 5. Uncertainty summarisation
      dd <- melt(
        dd,
        id.vars = c("mc", "agegrp", "uptake_group")
      )

      dd <- dd[
        ,
        fquantile_byid(value, prbl, id = as.character(variable)),
        keyby = c("agegrp","uptake_group")
      ]

      setnames(
        dd,
        c("agegrp", "uptake_group", "disease",
          percent(prbl, prefix = "mrtl_rate_"))
      )

      dd[, scenario := sc]

      d_out <- rbind(d_out, dd, use.names = TRUE)

      # Clean structurally empty rows
      stat_cols <- grep("%$", names(d_out), value = TRUE)
      d_out <- na.omit(d_out, cols=stat_cols)

    }

    fwrite(d_out, paste0(out_path_tables, "deaths_prev_post_by_scenario_age.csv"), sep = ";")

    ## All-cause Mortality total ## ----

    tt <- fread(paste0(in_path, "mrtl_scaled_up.csv.gz")
    )[, `:=` (year = year + 2000,
              agegrp = fifelse(agegrp %in% c("30-34", "35-39", "40-44", "45-49"), "30-49",
                               ifelse(agegrp %in% c("50-54", "55-59", "60-64", "65-69"), "50-69",
                                      "70-90")))]


    outstrata <- c("mc", "year", "scenario", "uptake_group")

    sc_n <- na.omit(as.numeric(gsub("[^1-9]+", "", unique(tt$scenario))))

    # Rate #

    d <- tt[, lapply(.SD, sum), .SDcols = c("all_cause_mrtl", "popsize"), keyby = eval(outstrata)
    ][, lapply(.SD, function(x) x/popsize), keyby = outstrata]
    d <- melt(d, id.vars = outstrata)
    d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
    setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "mrtl_rate_")))

    fwrite(d, paste0(out_path_tables, "mortality_rate_by_year.csv"), sep = ";")

    ggplot(d[disease != "popsize"], aes(x = year, y = `mrtl_rate_50.0%`,
                                        ymin = `mrtl_rate_2.5%`,
                                        ymax = `mrtl_rate_97.5%`,
                                        linetype = scenario)) +
      facet_wrap(~ disease, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Mortality") +
      ggtitle("Mortality rate by scenario over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "mortality_rate_by_year.", plot_format),
           height = 9, width = 16)

    # Rate difference #

    d <- tt[, lapply(.SD, sum), .SDcols = c("all_cause_mrtl", "popsize"), keyby = eval(outstrata)
    ][, lapply(.SD, function(x) x/popsize), keyby = outstrata]

    prvls <- c("all_cause_mrtl")

    d <- melt(d, id.vars = outstrata)
    d <- dcast(d, mc + year + uptake_group ~ scenario + variable, fun.aggregate = sum) # sum over sexes

    d_out <- data.table(NULL)

    for (k in seq_len(nrow(sc_map))) {

      sc <- sc_map$scenario[k]
      bl <- sc_map$baseline[k]

      sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
      bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

      if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

      # Safety check: same variables, same order
      stopifnot(
        sub(paste0("^", sc), "", sc_cols) ==
          sub(paste0("^", bl), "", bl_cols)
      )

      dd <- copy(d)

      # Compute differences
      for (i in seq_along(sc_cols)) {
        diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
        dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
      }

      # Keep only needed columns
      keep_cols <- c(
        "mc", "year","uptake_group",
        grep("^diff_", names(dd), value = TRUE)
      )
      dd <- dd[, ..keep_cols]

      # 5. Uncertainty summarisation
      dd <- melt(
        dd,
        id.vars = c("mc", "year","uptake_group")
      )

      dd <- dd[
        ,
        fquantile_byid(value, prbl, id = as.character(variable)),
        keyby = c("year","uptake_group")
      ]

      setnames(
        dd,
        c("year", "uptake_group", "disease",
          percent(prbl, prefix = "mrtl_rate_"))
      )

      dd[, scenario := sc]

      d_out <- rbind(d_out, dd, use.names = TRUE)

      # Clean structurally empty rows
      stat_cols <- grep("%$", names(d_out), value = TRUE)
      d_out <- na.omit(d_out, cols=stat_cols)

    }

    fwrite(d_out, paste0(out_path_tables, "mortality_rate_diff_by_year.csv"), sep = ";")

    ggplot(d_out, aes(x = year, y = `mrtl_rate_50.0%`,
                      ymin = `mrtl_rate_2.5%`,
                      ymax = `mrtl_rate_97.5%`,
                      linetype = scenario)) +
      facet_wrap(~ disease, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Mortality") +
      ggtitle("Difference in mortality rate compared to baseline over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "mortality_rate_diff_by_year.", plot_format),
           height = 9, width = 16)


    # Absolute numbers #

    d <- tt[, lapply(.SD, sum), .SDcols = c("all_cause_mrtl"), keyby = eval(outstrata)]
    d <- melt(d, id.vars = outstrata)
    d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
    setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "mrtl_rate_")))

    fwrite(d, paste0(out_path_tables, "mortality_numbers_by_year.csv"), sep = ";")

    ggplot(d, aes(x = year, y = `mrtl_rate_50.0%`,
                  ymin = `mrtl_rate_2.5%`,
                  ymax = `mrtl_rate_97.5%`,
                  linetype = scenario)) +
      facet_wrap(~ disease, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Mortality") +
      ggtitle("Mortality numbers by scenario over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "mortality_numbers_by_year.", plot_format),
           height = 9, width = 16)

    # Absolute difference #

    d <- tt[, lapply(.SD, sum), .SDcols = c("all_cause_mrtl"), keyby = eval(outstrata)]

    prvls <- c("all_cause_mrtl")

    d <- melt(d, id.vars = outstrata)
    d <- dcast(d, mc + year + uptake_group ~ scenario + variable, fun.aggregate = sum)

    d_out <- data.table(NULL)

    for (k in seq_len(nrow(sc_map))) {

      sc <- sc_map$scenario[k]
      bl <- sc_map$baseline[k]

      sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
      bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

      if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

      # Safety check: same variables, same order
      stopifnot(
        sub(paste0("^", sc), "", sc_cols) ==
          sub(paste0("^", bl), "", bl_cols)
      )

      dd <- copy(d)

      # Compute differences
      for (i in seq_along(sc_cols)) {
        diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
        dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
      }

      # Keep only needed columns
      keep_cols <- c(
        "mc", "year","uptake_group",
        grep("^diff_", names(dd), value = TRUE)
      )
      dd <- dd[, ..keep_cols]

      # 5. Uncertainty summarisation
      dd <- melt(
        dd,
        id.vars = c("mc", "year","uptake_group")
      )

      dd <- dd[
        ,
        fquantile_byid(value, prbl, id = as.character(variable)),
        keyby = c("year","uptake_group")
      ]

      setnames(
        dd,
        c("year", "uptake_group", "disease",
          percent(prbl, prefix = "mrtl_rate_"))
      )

      dd[, scenario := sc]

      d_out <- rbind(d_out, dd, use.names = TRUE)

      # Clean structurally empty rows
      stat_cols <- grep("%$", names(d_out), value = TRUE)
      d_out <- na.omit(d_out, cols=stat_cols)

    }

    fwrite(d_out, paste0(out_path_tables, "mortality_numbers_diff_by_year.csv"), sep = ";")

    ggplot(d_out, aes(x = year, y = `mrtl_rate_50.0%`,
                      ymin = `mrtl_rate_2.5%`,
                      ymax = `mrtl_rate_97.5%`,
                      linetype = scenario)) +
      facet_wrap(~ disease, scales = "free") +
      geom_ribbon(alpha = 0.5/5, colour = NA) +
      geom_line() +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Mortality") +
      ggtitle("Difference in mortality numbers compared to baseline over time") +
      expand_limits(y = 0) +
      theme(legend.title = element_blank())

    ggsave(paste0(out_path_plots, "mortality_numbers_diff_by_year.", plot_format),
           height = 9, width = 16)


    # Deaths prevented or postponed #

    d <- tt[, lapply(.SD, sum), .SDcols = c("all_cause_mrtl"), keyby = eval(outstrata)]

    prvls <- c("all_cause_mrtl")

    d <- melt(d, id.vars = outstrata)
    d <- dcast(d, mc + uptake_group ~ scenario + variable, fun.aggregate = sum) # sum over years

    d_out <- data.table(NULL)

    for (k in seq_len(nrow(sc_map))) {

      sc <- sc_map$scenario[k]
      bl <- sc_map$baseline[k]

      sc_cols <- grep(paste0("^", sc, "_"), names(d), value = TRUE)
      bl_cols <- grep(paste0("^", bl, "_"), names(d), value = TRUE)

      if (length(sc_cols) == 0L || length(bl_cols) == 0L) next

      # Safety check: same variables, same order
      stopifnot(
        sub(paste0("^", sc), "", sc_cols) ==
          sub(paste0("^", bl), "", bl_cols)
      )

      dd <- copy(d)

      # Compute differences
      for (i in seq_along(sc_cols)) {
        diff_name <- paste0("diff_", sub(paste0("^", sc, "_"), "", sc_cols[i]))
        dd[, (diff_name) := get(sc_cols[i]) - get(bl_cols[i])]
      }

      # Keep only needed columns
      keep_cols <- c(
        "mc", "uptake_group",
        grep("^diff_", names(dd), value = TRUE)
      )
      dd <- dd[, ..keep_cols]

      # 5. Uncertainty summarisation
      dd <- melt(
        dd,
        id.vars = c("mc", "uptake_group")
      )

      dd <- dd[
        ,
        fquantile_byid(value, prbl, id = as.character(variable)),
        keyby = c("uptake_group")
      ]

      setnames(
        dd,
        c("uptake_group", "disease",
          percent(prbl, prefix = "mrtl_rate_"))
      )

      dd[, scenario := sc]

      d_out <- rbind(d_out, dd, use.names = TRUE)

      # Clean structurally empty rows
      stat_cols <- grep("%$", names(d_out), value = TRUE)
      d_out <- na.omit(d_out, cols=stat_cols)

    }

    fwrite(d_out, paste0(out_path_tables, "deaths_prev_post_by_scenario.csv"), sep = ";")
  }

  if("ly_scaled_up.csv.gz" %in% list.files(in_path)){

    ## Life years lived by sex ## ----

    tt <- fread(paste0(in_path, "ly_scaled_up.csv.gz")
    )[, `:=` (year = year + 2000)]

    outstrata <- c("mc", "sex", "year", "uptake_group")

    sc_n <- unique(tt$scenario)

    # Wide format: one column per scenario
    ttt <- dcast(
      tt,
      mc + year + sex + uptake_group ~ scenario,
      value.var = "LY",
      fun.aggregate = sum
    )

    # Loop through scenario–baseline pairs
    for(k in seq_len(nrow(sc_map))) {

      sc <- sc_map$scenario[k]
      bl <- sc_map$baseline[k]

      # Create diff column: sc - baseline
      ttt[, paste0(sc, "_diff") := get(sc) - get(bl)]
    }
    # -------------------------------------

    # Melt and compute quantiles
    d <- melt(ttt, id.vars = outstrata)
    d <- d[, fquantile_byid(value, prbl, id = as.character(variable)),
           keyby = eval(setdiff(outstrata, "mc"))]

    setnames(d, c(setdiff(outstrata, "mc"), "scenario", percent(prbl, prefix = "LY_diff_")))

    # Export: absolute LY and LY differences
    fwrite(
      d[!(scenario %in% grep("_diff", unique(d$scenario), value = TRUE))],
      paste0(out_path_tables, "life_years_by_year_sex.csv"),
      sep = ";"
    )

    fwrite(
      d[(scenario %in% grep("_diff", unique(d$scenario), value = TRUE))],
      paste0(out_path_tables, "life_years_diff_by_year_sex.csv"),
      sep = ";"
    )

    ## Life years gained by sex ##

    d <- melt(ttt, id.vars = outstrata)
    d <- d[, lapply(.SD, sum), .SDcols = "value", by = c("mc", "variable", "uptake_group","sex")]
    d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = c("sex","uptake_group")]

    setnames(d, c("sex", "uptake_group", "scenario", percent(prbl, prefix = "LY_diff_")))

    fwrite(
      d[(scenario %in% grep("_diff", unique(d$scenario), value = TRUE))],
      paste0(out_path_tables, "life_years_gained_by_sex.csv"),
      sep = ";"
    )

    ## Life years lived by age ## ----

    tt <- fread(paste0(in_path, "ly_scaled_up.csv.gz")
    )[, `:=` (
      year = year + 2000,
      agegrp = fifelse(agegrp %in% c("30-34","35-39","40-44","45-49"), "30-49",
                       fifelse(agegrp %in% c("50-54","55-59","60-64","65-69"), "50-69",
                               "70-90"))
    )]

    outstrata <- c("mc", "agegrp", "year", "uptake_group")

    sc_n <- unique(tt$scenario)

    # Wide format: one column per scenario
    ttt <- dcast(
      tt,
      mc + year + agegrp + uptake_group ~ scenario,
      value.var = "LY",
      fun.aggregate = sum
    )

    # Loop through scenario–baseline pairs

    for(k in seq_len(nrow(sc_map))) {

      sc <- sc_map$scenario[k]
      bl <- sc_map$baseline[k]

      ttt[, paste0(sc, "_diff") := get(sc) - get(bl)]
    }

    # -------------------------------------

    # Melt and compute quantiles
    d <- melt(ttt, id.vars = outstrata)
    d <- d[, fquantile_byid(value, prbl, id = as.character(variable)),
           keyby = eval(setdiff(outstrata, "mc"))]

    setnames(d, c(setdiff(outstrata, "mc"), "scenario", percent(prbl, prefix = "LY_diff_")))

    # Export absolute LY and LY differences
    fwrite(
      d[!(scenario %in% grep("_diff", unique(d$scenario), value = TRUE))],
      paste0(out_path_tables, "life_years_by_year_age.csv"),
      sep = ";"
    )

    fwrite(
      d[(scenario %in% grep("_diff", unique(d$scenario), value = TRUE))],
      paste0(out_path_tables, "life_years_diff_by_year_age.csv"),
      sep = ";"
    )

    ## Life years gained by age ## ----

    # Cumulate difference in life years lived over years:
    d <- melt(ttt, id.vars = outstrata)
    d <- d[, lapply(.SD, sum), .SDcols = "value",
           by = c("mc", "variable", "uptake_group", "agegrp")]

    d <- d[, fquantile_byid(value, prbl, id = as.character(variable)),
           keyby = c("agegrp", "uptake_group")]

    setnames(d, c("agegrp", "uptake_group", "scenario", percent(prbl, prefix = "LY_diff_")))

    fwrite(
      d[(scenario %in% grep("_diff", unique(d$scenario), value = TRUE))],
      paste0(out_path_tables, "life_years_gained_by_age.csv"),
      sep = ";"
    )

    ## Life years gained by age ## ----

    # Cumulate difference in life years lived over years:

    d <- melt(ttt, id.vars = outstrata)
    d <- d[, lapply(.SD, sum), .SDcols = "value", by = c("mc", "variable", "uptake_group", "agegrp")]
    d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = c("agegrp","uptake_group")]

    setnames(d, c("agegrp", "uptake_group", "scenario", percent(prbl, prefix = "LY_diff_")))

    fwrite(d[(scenario %in% grep("_diff", unique(d$scenario), value = TRUE))],
           paste0(out_path_tables, "life_years_gained_by_age.csv"), sep = ";")


    ## Life years lived total ## ----

    tt <- fread(paste0(in_path, "ly_scaled_up.csv.gz")
    )[, `:=` (year = year + 2000)]

    outstrata <- c("mc", "year", "uptake_group")

    # Wide format: sum over sex + age groups
    ttt <- dcast(
      tt,
      mc + year + uptake_group ~ scenario,
      value.var = "LY",
      fun.aggregate = sum
    )

    # Loop through scenario–baseline pairs

    for(k in seq_len(nrow(sc_map))) {

      sc <- sc_map$scenario[k]
      bl <- sc_map$baseline[k]

      ttt[, paste0(sc, "_diff") := get(sc) - get(bl)]
    }
    # -------------------------------------

    # Melt and compute quantiles
    d <- melt(ttt, id.vars = outstrata)
    d <- d[, fquantile_byid(value, prbl, id = as.character(variable)),
           keyby = eval(setdiff(outstrata, "mc"))]

    setnames(d, c(setdiff(outstrata, "mc"), "scenario", percent(prbl, prefix = "LY_diff_")))

    # Export absolute LY and LY differences
    fwrite(
      d[!(scenario %in% grep("_diff", unique(d$scenario), value = TRUE))],
      paste0(out_path_tables, "life_years_by_year.csv"),
      sep = ";"
    )

    fwrite(
      d[(scenario %in% grep("_diff", unique(d$scenario), value = TRUE))],
      paste0(out_path_tables, "life_years_diff_by_year.csv"),
      sep = ";"
    )

    ## Life years gained total ## ----

    # Cumulate difference in life years lived over years:
    d <- melt(ttt, id.vars = outstrata)
    d <- d[, lapply(.SD, sum), .SDcols = "value", by = c("mc", "uptake_group", "variable")]

    d <- d[, fquantile_byid(value, prbl, id = as.character(variable)),
           keyby = c("uptake_group")]

    setnames(d, c("uptake_group", "scenario", percent(prbl, prefix = "LY_diff_")))

    fwrite(
      d[(scenario %in% grep("_diff", unique(d$scenario), value = TRUE))],
      paste0(out_path_tables, "life_years_gained.csv"),
      sep = ";"
    )

  }

  if(length(grep("cea_results", list.files(in_path))) > 0){

    ## Costs and QALYs, total, by sex and by agegrp ## ----

    cea_disc <- grep("cea_results", list.files(in_path), value = TRUE)

    for(j in cea_disc){

      disc <- gsub("[^0-9]", "", j)

      cea <- fread(paste0(in_path, j))
      cea[, `:=` (year = year + 2000,
                  agegrp = fifelse(agegrp %in% c("30-34", "35-39", "40-44", "45-49"), "30-49",
                                   ifelse(agegrp %in% c("50-54", "55-59", "60-64", "65-69"), "50-69",
                                          "70-90")))]

      cea[, analysis := analysis]

      export_vars <- grep("^incr_.*_scl$", names(cea), value = TRUE)

      ## Total ##
      cea_tot <- cea[, lapply(.SD, sum),
                     .SDcols = !c("analysis","scenario","sex","agegrp","mc"),
                     by = c("scenario","mc","analysis")]

      ## Sex ##
      cea_sex <- cea[, lapply(.SD, sum),
                     .SDcols = !c("analysis","scenario","sex","agegrp","mc"),
                     by = c("scenario","mc","analysis","sex")]

      ## Age ##
      cea_age <- cea[, lapply(.SD, sum),
                     .SDcols = !c("analysis","scenario","sex","agegrp","mc"),
                     by = c("scenario","mc","analysis","agegrp")]

      ## Year ##
      cea_year <- cea[, lapply(.SD, sum),
                      .SDcols = !c("analysis","scenario","sex","agegrp","mc"),
                      by = c("scenario","mc","analysis","year")]

      for(i in export_vars){

        # Total results #

        dd <- cea_tot[, fquantile_byid(get(i), prbl, id = analysis), keyby = c("scenario")]
        setnames(dd, c("scenario", "analysis", percent(prbl, prefix = paste0(i, "_"))))

        dd <- na.omit(dd)

        fwrite(dd, paste0(out_path_tables, i, "_", analysis, "_disc_", disc, ".csv"), sep = ";")

        # Sex results #

        dd <- cea_sex[, fquantile_byid(get(i), prbl, id = analysis), keyby = c("sex", "scenario")]
        setnames(dd, c("sex", "scenario", "analysis", percent(prbl, prefix = paste0(i, "_"))))

        dd <- na.omit(dd)

        fwrite(dd, paste0(out_path_tables, i, "_", analysis, "_disc_", disc, "_by_sex.csv"), sep = ";")

        # Age results #

        dd <- cea_age[, fquantile_byid(get(i), prbl, id = analysis), keyby = c("agegrp", "scenario")]
        setnames(dd, c("agegrp", "scenario", "analysis", percent(prbl, prefix = paste0(i, "_"))))

        dd <- na.omit(dd)

        fwrite(dd, paste0(out_path_tables, i, "_", analysis, "_disc_", disc, "_by_age.csv"), sep = ";")

        # Year results #

        dd <- cea_year[, fquantile_byid(get(i), prbl, id = analysis), keyby = c("year", "scenario")]
        setnames(dd, c("year", "scenario", "analysis", percent(prbl, prefix = paste0(i, "_"))))

        dd <- na.omit(dd)

        fwrite(dd, paste0(out_path_tables, i, "_", analysis, "_disc_", disc, "_by_year.csv"), sep = ";")

      }
    }
  }

  ####################################################################################################################
  #------------------------------------------   Jane, Test, 09.Jan.2026   -------------------------------------------#
  ####################################################################################################################
}

