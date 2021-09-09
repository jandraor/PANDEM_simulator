#!/usr/bin/env Rscript

devtools::load_all("./package")
setwd("./package/R")
plumber::pr("REST_framework.R") |>
  plumber::pr_run(host = "0.0.0.0", port = 8000)
