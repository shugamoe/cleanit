plan  <- drake_plan(
  aboutWfr = wflow_publish(knitr_in("analysis/about.Rmd"), verbose = T),
  licenseWfr = wflow_publish(knitr_in("analysis/license.Rmd"), verbose = T),
  summaryStatsWfr = wflow_publish(knitr_in("analysis/summaryStats.Rmd"), verbose = T),
  indexWfr = wflow_publish(knitr_in("analysis/index.Rmd"), view = T, verbose = T)
)