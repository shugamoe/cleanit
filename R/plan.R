plan  <- drake_plan(
  summaryStatsWfr = wflow_publish(knitr_in("analysis/summaryStats.Rmd"), view = T, verbose = T)
)
