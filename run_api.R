#!/usr/bin/env Rscript

library(plumber)
setwd("./R")
source("./create_sim_folder.R")
source("./run_model.R")
pr("./REST_framework.R") |> pr_run(host = "0.0.0.0", port = 8000)
