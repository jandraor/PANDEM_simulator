#!/usr/bin/env Rscript

library(plumber)
setwd("./R")
source("./R/create_sim_folder.R")
pr("./R/REST_framework.R") |> pr_run(host = "0.0.0.0", port = 8000)
