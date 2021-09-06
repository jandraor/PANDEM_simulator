#!/usr/bin/env Rscript

library(plumber)
files <- file.path("./R",list.files("./R"))
sapply(files, source)
pr("REST_framework.R") |> pr_run(host="0.0.0.0", port = 8000)
