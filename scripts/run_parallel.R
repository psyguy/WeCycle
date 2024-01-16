
# Reading and cleaning the empirical data ---------------------------------

# Reading the complete dataset (cleaned earlier)
d_w <- readRDS("data_private/d_w.rds")

# Extracting the relevant item (PA) and making DOW columns
d_pa <- d_w %>%
  filter(item == "avg.pa") %>%
  mutate(
    weekday = lubridate::wday(date,
                              week_start = 1,
                              label = TRUE),
    weekday_num = lubridate::wday(date,
                                  week_start = 1,
                                  label = FALSE)
    ) %>% select(id, t, y, weekday, weekday_num)

# Saving the relevant empirical data
saveRDS(d_pa, "data_private/d_pa.rds")

# Running the fitting and estimation --------------------------------------

# Initialization
Sys.setenv("OMP_THREAD_LIMIT" = 46)
options(warn = -1)
library(future)
library(doFuture)

# Reading the empirical data
d_pa <- readRDS("data_private/d_pa.rds")

# Making the table of conditions (i.e, model orders etc.)
conditions_table <- expand.grid(
  id = 1:98,
  mu = c("c", "d", "h"),
  ar = c(0, 1),
  ma = c(0, 1),
  sar = c(0, 1),
  sma = c(0, 1)
)

# Launching parallel computation
registerDoFuture()
plan("multisession",
     workers = 46)

# Running the fitting and estimation in parallel
(t_start <- Sys.time())
fit_logs <- foreach(i = 1:nrow(conditions_table)) %dopar% {

  t_c <- conditions_table[i, ]
  # Getting the data
  d_pa_i <- d_pa %>% filter(id == t_c$id)

  dump <- tryCatch({
    paste0(t_c$mu,
           "+sarma(",
           t_c$ar,
           ",",
           t_c$ma,
           ")(",
           t_c$sar,
           ",",
           t_c$sma,
           ")") %>%
      m_fit(
        d_pa_i,
        .,
        id = t_c$id,
        save_fit = TRUE,
        save_est = TRUE
      )
  }, error = function(e)
    NULL)

  paste(i,
        "done at",
        Sys.time()) %>%
    print()
}
(t_end <- Sys.time())
t_end - t_start

# Harvesting the estimates from est_* files -------------------------------

# Listing the estimated files
est_files <- list.files("ests", pattern = ".rds", full.names = TRUE)

# Launching parallel computation
registerDoFuture()
plan("multisession",
     workers = 46)

# Running the harvest
Sys.time()
harvest <- foreach(
  i = 1:length(est_files),
  .combine = rbind,
  .errorhandling = 'remove'
) %dopar% {
  file_name <- est_files[i]
  ee <- readRDS(file_name)
  mu <- gsub(".*_([chd])\\+.*",
             "\\1",
             file_name)

  nums <- file_name %>%
    str_extract_all("\\d+") %>%
    unlist() %>%
    as.numeric()

  harv <- ee$estimates %>%
    cbind(
      id = nums[1],
      mu = mu,
      ar = nums[2],
      ma = nums[3],
      sar = nums[4],
      sma = nums[5],
      .
    ) %>%
    cbind(ee$information_criteria)
  harv
}
Sys.time()

# Adding more descriptive variables to the results dataframe
results_estimates <- harvest %>%
  ungroup() %>%
  mutate(
    error_name = paste0("sarma",
                        "(", ar, ",", ma, ")",
                        "(", sar, ",", sma, ")"),
    error_name_words = case_when(
      error_name == "sarma(0,0)(0,0)" ~ "01_wn",
      error_name == "sarma(1,0)(0,0)" ~ "02_ar(1)",
      error_name == "sarma(0,1)(0,0)" ~ "03_ma(1)",
      error_name == "sarma(1,1)(0,0)" ~ "04_arma(1,1)",
      error_name == "sarma(0,0)(1,0)" ~ "05_sar(0)(1)",
      error_name == "sarma(1,0)(1,0)" ~ "06_sar(1)(1)",
      error_name == "sarma(0,0)(0,1)" ~ "07_sma(0)(1)",
      error_name == "sarma(0,1)(0,1)" ~ "08_sma(1)(1)",
      error_name == "sarma(0,0)(1,1)" ~ "09_sarma(0,0)(1,1)",
      error_name == "sarma(0,1)(1,0)" ~ "10_sarma(0,1)(1,0)",
      error_name == "sarma(0,1)(1,1)" ~ "11_sarma(0,1)(1,1)",
      error_name == "sarma(1,0)(0,1)" ~ "12_sarma(1,0)(0,1)",
      error_name == "sarma(1,0)(1,1)" ~ "13_sarma(1,0)(1,1)",
      error_name == "sarma(1,1)(0,1)" ~ "14_sarma(1,1)(0,1)",
      error_name == "sarma(1,1)(1,0)" ~ "15_sarma(1,1)(1,0)",
      error_name == "sarma(1,1)(1,1)" ~ "16_sarma(1,1)(1,1)"
    ) %>%
      as.factor(),
    model_name = paste0(mu, "+", error_name),
    timescale_daily = (ar == 1 | ma == 1),
    timescale_weekly = (sar == 1 | sma == 1),
    error_timescale = case_when(
      timescale_daily & !timescale_weekly ~ "daily",
      !timescale_daily & timescale_weekly ~ "weekly",
      timescale_daily &
        timescale_weekly ~ "daily + weekly"
    ) %>%
      factor(levels = c("daily", "weekly", "daily + weekly")),
    model_timescale = paste0(mu, " + ", error_timescale),
    dynamics_ar = (ar == 1 | sar == 1),
    dynamics_ma = (ma == 1 | sma == 1),
    error_dynamics = case_when(
      dynamics_ar & !dynamics_ma ~ "(s)ar",
      !dynamics_ar & dynamics_ma ~ "(s)ma",
      dynamics_ar &
        dynamics_ma ~ "(s)ar + (s)ma",
      TRUE ~ "wn"
    ) %>%
      factor(levels = c("wn", "(s)ar", "(s)ma", "(s)ar + (s)ma")),
    model_dynamics = paste0(mu, " + ", error_dynamics),
    .after = sma
  )

# Saving the processed estimates & ICs extracted from est_ files
saveRDS(harv, "results_estimates.rds")

# Reporting the results ---------------------------------------------------

# Reading the results dataframe
results_estimates <- readRDS("results_estimates.rds")

# Counting selected models
r_s <- results_estimates %>%
  filter(parameter == "sigma2") %>%
  select(-parameter:-sig) %>%
  group_by(id) %>%
  mutate(
    win_AIC = case_when(min(AIC) == AIC ~ 1,
                        TRUE ~ 0),
    win_AICc = case_when(min(AICc) == AICc ~ 1,
                         TRUE ~ 0),
    win_BIC = case_when(min(BIC) == BIC ~ 1,
                        TRUE ~ 0)
  ) %>%
  group_by(across(mu:model_dynamics),
           .add = FALSE) %>%
  summarise(
    n_w_AIC = sum(win_AIC),
    n_w_AICc = sum(win_AICc),
    n_w_BIC = sum(win_BIC)
  )

## Making the tables
## For other ICs, change AICc to AIC or BIC

# Making the detailed table with 16 rows
xtabs(n_w_AICc  ~ error_name_words + mu, r_s)

# Main body of the summarized table
xtabs(n_w_AICc ~ error_dynamics + mu + error_timescale,
      r_s)
# Rows total per mu (Total a)
xtabs(n_w_AICc ~ error_dynamics + mu,
      r_s)
# Rows sum all mu (Sum b)
xtabs(n_w_AICc ~ error_dynamics,
      r_s)
# Columns total per mu (Total c)
xtabs(n_w_AICc ~  error_timescale + mu ,
      r_s)
# Column sum all mu (Sum d)
xtabs(n_w_AICc ~ error_timescale,
      r_s)
# Totals per mu
xtabs(n_w_AICc ~ mu,
      r_s)
