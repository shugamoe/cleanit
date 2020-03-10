plan  <- drake_plan(
  aboutWfr = wflow_build(knitr_in("analysis/about.Rmd"), verbose = T),
  licenseWfr = wflow_build(knitr_in("analysis/license.Rmd"), verbose = T),
  summaryStatsWfr = wflow_build(knitr_in("analysis/summaryStats.Rmd"), verbose = T),
  looseDrillDownWfr = wflow_build(knitr_in("analysis/looseJoinDrilldown.Rmd"), verbose = T),
  indexWfr = wflow_build(knitr_in("analysis/index.Rmd"), view = T, verbose = T)
)
