## @knitr load_packages

libs_used <-
  c("plyr",
    "tidyverse",
    "knitr",
    "patchwork",
    "cowplot",
    "scales",
    "circular",
    # "ggthemes",
    # "devtools",
    # "latex2exp",
    # "knitr",
    # "fpp3",
    "shiny",
    # "dlm",
    # "FKF",
    "here",
    # "FKF.SP",
    # "future",
    # "doFuture",
    # "NFCP",
    "stats",
    "imputeTS",
    "sarima",
    "lubridate",
    "forecast")

libs_needed <- libs_used[!libs_used %in% installed.packages()]

sapply(libs_needed, install.packages, dependencies = TRUE)
sapply(libs_used, require, character = TRUE)

rm(libs_used, libs_needed)

# renv::install("devtools")
# devtools::install_github('Mikata-Project/ggthemr')
renv::install("ggthemes")
renv::install("scales")
nrenv::install('Mikata-Project/ggthemr')
library(ggthemr)
ggthemr::ggthemr("pale")
