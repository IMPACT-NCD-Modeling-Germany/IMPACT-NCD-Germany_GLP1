% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Disease_class.R
\name{Disease}
\alias{Disease}
\title{R6 Class representing an disease}
\description{
A disease has a sim_prm list that holds the simulation parameters.
}
\details{
To be completed...
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{name}}{The name of the disease.}

\item{\code{friendly_name}}{A friendly name for the disease.}

\item{\code{meta}}{Disease metadata including type.}

\item{\code{notes}}{Any notes regarding the disease.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Disease-new}{\code{Disease$new()}}
\item \href{#method-Disease-gen_parf_files}{\code{Disease$gen_parf_files()}}
\item \href{#method-Disease-gen_parf}{\code{Disease$gen_parf()}}
\item \href{#method-Disease-set_init_prvl}{\code{Disease$set_init_prvl()}}
\item \href{#method-Disease-set_rr}{\code{Disease$set_rr()}}
\item \href{#method-Disease-set_incd_prb}{\code{Disease$set_incd_prb()}}
\item \href{#method-Disease-set_dgns_prb}{\code{Disease$set_dgns_prb()}}
\item \href{#method-Disease-set_mrtl_prb}{\code{Disease$set_mrtl_prb()}}
\item \href{#method-Disease-del_parf_file}{\code{Disease$del_parf_file()}}
\item \href{#method-Disease-get_incd}{\code{Disease$get_incd()}}
\item \href{#method-Disease-get_dur}{\code{Disease$get_dur()}}
\item \href{#method-Disease-get_prvl}{\code{Disease$get_prvl()}}
\item \href{#method-Disease-get_ftlt}{\code{Disease$get_ftlt()}}
\item \href{#method-Disease-get_seed}{\code{Disease$get_seed()}}
\item \href{#method-Disease-get_rr}{\code{Disease$get_rr()}}
\item \href{#method-Disease-del_stochastic_effect}{\code{Disease$del_stochastic_effect()}}
\item \href{#method-Disease-get_parf}{\code{Disease$get_parf()}}
\item \href{#method-Disease-get_parf_filename}{\code{Disease$get_parf_filename()}}
\item \href{#method-Disease-harmonise_epi_tables}{\code{Disease$harmonise_epi_tables()}}
\item \href{#method-Disease-to_cpp}{\code{Disease$to_cpp()}}
\item \href{#method-Disease-to_cpp_Peter}{\code{Disease$to_cpp_Peter()}}
\item \href{#method-Disease-print}{\code{Disease$print()}}
\item \href{#method-Disease-clone}{\code{Disease$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Disease-new"></a>}}
\if{latex}{\out{\hypertarget{method-Disease-new}{}}}
\subsection{Method \code{new()}}{
Create a new disease object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Disease$new(name, friendly_name, meta, notes = NA_character_, design_, RR)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{name}}{A string with disease name.}

\item{\code{friendly_name}}{A string with disease friendly name.}

\item{\code{meta}}{A list with the disease type and other information for
incidence, diagnosis, and mortality.}

\item{\code{notes}}{A string with any notes.}

\item{\code{design_}}{A design object with the simulation parameters.}

\item{\code{RR}}{A list of exposure objects.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new `Disease` object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Disease-gen_parf_files"></a>}}
\if{latex}{\out{\hypertarget{method-Disease-gen_parf_files}{}}}
\subsection{Method \code{gen_parf_files()}}{
Generates PARF and stores it to disk if one doesn not
  exists already.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Disease$gen_parf_files(
  design_ = design,
  diseases_ = diseases,
  popsize = 50000,
  check = design_$sim_prm$logs,
  keep_intermediate_file = TRUE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{design_}}{A design object with the simulation parameters.}

\item{\code{diseases_}}{A list of Disease objects}

\item{\code{popsize}}{The population size for each stratum}

\item{\code{check}}{Check for NAs in parf_dt.}

\item{\code{keep_intermediate_file}}{Whether to keep the intermediate synthpop file}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The PARF data.table if it was created, otherwise `NULL`.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Disease-gen_parf"></a>}}
\if{latex}{\out{\hypertarget{method-Disease-gen_parf}{}}}
\subsection{Method \code{gen_parf()}}{
Read PARF file from disk. If missing, generates PARF and
  writes it to disk.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Disease$gen_parf(
  sp = sp,
  design_ = design,
  diseases_ = diseases,
  scenario_p_zero = 1,
  perc_change_m0 = 1,
  popsize = 5e+05,
  check = design_$sim_prm$logs,
  keep_intermediate_file = TRUE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{sp}}{A synthpop object}

\item{\code{design_}}{A design object with the simulation parameters.}

\item{\code{diseases_}}{A list of Disease objects}

\item{\code{popsize}}{The population size for each stratum}

\item{\code{check}}{Check for NAs in parf_dt.}

\item{\code{keep_intermediate_file}}{Whether to keep the intermediate synthpop file}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The invisible self for chaining.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Disease-set_init_prvl"></a>}}
\if{latex}{\out{\hypertarget{method-Disease-set_init_prvl}{}}}
\subsection{Method \code{set_init_prvl()}}{
Set disease prevalence & diagnosis in a new col in sp$pop.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Disease$set_init_prvl(sp, design_ = design)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{sp}}{A synthetic population.}

\item{\code{design_}}{A design object with the simulation parameters.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The invisible self for chaining.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Disease-set_rr"></a>}}
\if{latex}{\out{\hypertarget{method-Disease-set_rr}{}}}
\subsection{Method \code{set_rr()}}{
Set disease incidence probability in a new col in sp$pop.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Disease$set_rr(
  sp,
  design_ = design,
  checkNAs = design_$sim_prm$logs,
  forPARF = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{sp}}{A synthetic population.}

\item{\code{design_}}{A design object with the simulation parameters.}

\item{\code{checkNAs}}{If `TRUE`, prints the table of NAs before they get
overwritten with 1. Note that for some exposures, NAs are expected
for certain levels of exposure (i.e. for active days).}

\item{\code{forPARF}}{Set TRUE when applied on the specialised forPARF
SynthPop}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The invisible self for chaining.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Disease-set_incd_prb"></a>}}
\if{latex}{\out{\hypertarget{method-Disease-set_incd_prb}{}}}
\subsection{Method \code{set_incd_prb()}}{
Set disease incident probability in a new col in sp$pop.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Disease$set_incd_prb(sp, design_ = design)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{sp}}{A synthetic population.}

\item{\code{design_}}{A design object with the simulation parameters.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The invisible self for chaining.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Disease-set_dgns_prb"></a>}}
\if{latex}{\out{\hypertarget{method-Disease-set_dgns_prb}{}}}
\subsection{Method \code{set_dgns_prb()}}{
Set diagnosis probability in a new col in sp$pop.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Disease$set_dgns_prb(sp, design_ = design)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{sp}}{A synthetic population.}

\item{\code{design_}}{A design object with the simulation parameters.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The invisible self for chaining.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Disease-set_mrtl_prb"></a>}}
\if{latex}{\out{\hypertarget{method-Disease-set_mrtl_prb}{}}}
\subsection{Method \code{set_mrtl_prb()}}{
Set disease case fatality when relevant, in a new col in
  sp$pop.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Disease$set_mrtl_prb(sp, design_ = design)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{sp}}{A synthetic population.}

\item{\code{design_}}{A design object with the simulation parameters.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The invisible self for chaining.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Disease-del_parf_file"></a>}}
\if{latex}{\out{\hypertarget{method-Disease-del_parf_file}{}}}
\subsection{Method \code{del_parf_file()}}{
Deletes the PARF file from disk.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Disease$del_parf_file(invert = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{invert}}{deletes all other disease relevant PARF file except those
that are associated to the current settings.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The invisible self for chaining.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Disease-get_incd"></a>}}
\if{latex}{\out{\hypertarget{method-Disease-get_incd}{}}}
\subsection{Method \code{get_incd()}}{
Get disease incident probability.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Disease$get_incd(year_, mc_ = sp$mc_aggr)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{year_}}{A vector of years to return. All if missing.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A data.table with disease incident probabilities unless
  incidence type: Universal when it returns data.table(NULL).
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Disease-get_dur"></a>}}
\if{latex}{\out{\hypertarget{method-Disease-get_dur}{}}}
\subsection{Method \code{get_dur()}}{
Get disease duration distribution parameters.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Disease$get_dur(mc_ = sp$mc_aggr)}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A data.table with duration distribution parameters. unless
  incidence type: Universal when it returns data.table(NULL).
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Disease-get_prvl"></a>}}
\if{latex}{\out{\hypertarget{method-Disease-get_prvl}{}}}
\subsection{Method \code{get_prvl()}}{
Get disease prevalent probability.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Disease$get_prvl(year_, mc_ = sp$mc_aggr)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{year_}}{A vector of years to return. All if missing.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A data.table with disease prevalent probabilities unless
  incidence type: Universal when it returns data.table(NULL).
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Disease-get_ftlt"></a>}}
\if{latex}{\out{\hypertarget{method-Disease-get_ftlt}{}}}
\subsection{Method \code{get_ftlt()}}{
Get disease case fatality probability.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Disease$get_ftlt(year_, mc_ = sp$mc_aggr)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{year_}}{A vector of years to return. All if missing.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A data.table with disease case fatality probabilities unless
  mortality type: Non-fatal when it returns data.table(NULL).
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Disease-get_seed"></a>}}
\if{latex}{\out{\hypertarget{method-Disease-get_seed}{}}}
\subsection{Method \code{get_seed()}}{
Get seed for RNG.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Disease$get_seed()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A seed for the RNG that is produced by the digest of disease
  name and outcome.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Disease-get_rr"></a>}}
\if{latex}{\out{\hypertarget{method-Disease-get_rr}{}}}
\subsection{Method \code{get_rr()}}{
Get the list of rr for all relevant exposures.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Disease$get_rr()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A list of exposure objects.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Disease-del_stochastic_effect"></a>}}
\if{latex}{\out{\hypertarget{method-Disease-del_stochastic_effect}{}}}
\subsection{Method \code{del_stochastic_effect()}}{
Deletes the stochastic effect files and indices from disk
  for all relevant RR.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Disease$del_stochastic_effect()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
The invisible self for chaining.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Disease-get_parf"></a>}}
\if{latex}{\out{\hypertarget{method-Disease-get_parf}{}}}
\subsection{Method \code{get_parf()}}{
Get the PARF by age/sex/dimd/ethnicity/sha.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Disease$get_parf(what)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{what}}{Columns to return (p0, m0, or parf)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A data.table with PARF.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Disease-get_parf_filename"></a>}}
\if{latex}{\out{\hypertarget{method-Disease-get_parf_filename}{}}}
\subsection{Method \code{get_parf_filename()}}{
Get the PARF filename.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Disease$get_parf_filename()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A data.table with PARF.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Disease-harmonise_epi_tables"></a>}}
\if{latex}{\out{\hypertarget{method-Disease-harmonise_epi_tables}{}}}
\subsection{Method \code{harmonise_epi_tables()}}{
Harmonises classes and levels between the synthetic
  population and the incidence/prevalence/fatality tables. It saves the
  harmonised table to disk, overwriting the existing one.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Disease$harmonise_epi_tables(sp)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{sp}}{A synthetic population.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The invisible self for chaining.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Disease-to_cpp"></a>}}
\if{latex}{\out{\hypertarget{method-Disease-to_cpp}{}}}
\subsection{Method \code{to_cpp()}}{
Returns a list to pass to the C++ side for Chris' parser.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Disease$to_cpp(sp, design_ = design, scenario_suffix = "")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{sp}}{A synthetic population.}

\item{\code{design_}}{A design object with the simulation parameters.}

\item{\code{scenario_suffix}}{the suffix to identify columns from different
scenarios}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A list.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Disease-to_cpp_Peter"></a>}}
\if{latex}{\out{\hypertarget{method-Disease-to_cpp_Peter}{}}}
\subsection{Method \code{to_cpp_Peter()}}{
Returns a list to pass to the C++ side for Peter's parser.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Disease$to_cpp_Peter(sp, design_ = design, scenario_suffix = "")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{sp}}{A synthetic population.}

\item{\code{design_}}{A design object with the simulation parameters.}

\item{\code{scenario_suffix}}{the suffix to identify columns from different
scenarios}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A list.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Disease-print"></a>}}
\if{latex}{\out{\hypertarget{method-Disease-print}{}}}
\subsection{Method \code{print()}}{
Print the simulation parameters.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Disease$print()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
The invisible self for chaining.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Disease-clone"></a>}}
\if{latex}{\out{\hypertarget{method-Disease-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Disease$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
