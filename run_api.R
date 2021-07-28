#!/usr/bin/env Rscript

library(plumber)
pr("REST_framework.R") |> pr_run(host="0.0.0.0", port = 8000)