l_d_empirical <- list(d_pa %>% filter(id == 70),
            d_pa %>% filter(id == 14),
            d_pa %>% filter(id == 12),
            d_pa %>% filter(id == 43),
            d_pa %>% filter(id == 97),
            d_pa %>% filter(id == 20)
)

l_labels_empirical <- paste("Person", LETTERS[1:6])

plot_rows(l_d_empirical,
          l_labels_empirical,
          max_t = 122,
          max_period = 31,
          save_name = "rows-empirical-31.pdf")


# Fixed parameters for the mean structure ---------------------------------

fixed_mu <- 0 # 1
# fixed_dowe <- c(0.5, 1, 1, -2, 1, 3, -1)
fixed_dowe <- c(0.1, .3, .3, -.7, .3, .9, -.5)
fixed_c <- 0.1
fixed_amp <- 0.8 # 1.5
fixed_phase_peak <- 6 #2


# Fixed parameters for the daily processes --------------------------------

## Fixed sigma2 for daily, weekly, and seasonal models
fixed_daily_sigma2_c <- 9/9
fixed_daily_sigma2_d <- 9/9
fixed_daily_sigma2_h <- 9/9

fixed_weekly_sigma2_c <- 9/9
fixed_weekly_sigma2_d <- 9/9
fixed_weekly_sigma2_h <- 9/9

fixed_seasonal_sigma2_c <- 9/9 # fixed_daily_sigma2_c + fixed_weekly_sigma2_c
fixed_seasonal_sigma2_d <- 9/9 # fixed_daily_sigma2_d + fixed_weekly_sigma2_d
fixed_seasonal_sigma2_h <- 9/9 # fixed_daily_sigma2_h + fixed_weekly_sigma2_h

## Fixed ar parameter for daily models with c, d, and h means
fixed_daily_ma_c <- 0.6
fixed_daily_ma_d <- 0.6
fixed_daily_ma_h <- 0.6

## Fixed ar parameter for daily models with c, d, and h means
fixed_daily_ar_c <- 0.7
fixed_daily_ar_d <- 0.7
fixed_daily_ar_h <- 0.7


# Fixed parameters for the weekly processes -------------------------------

## Fixed ar parameter for weekly models with c, d, and h means
fixed_weekly_ar_c <- 0.4
fixed_weekly_ar_d <- 0.4
fixed_weekly_ar_h <- 0.4

## Fixed ar parameter for weekly models with c, d, and h means
fixed_weekly_ma_c <- 0.5
fixed_weekly_ma_d <- 0.5
fixed_weekly_ma_h <- 0.5

# Non-seasonal daily plots ------------------------------------------------

### 7-row figure for constant mean

l_d_sim_w <- list(
  m_sim(
    c = fixed_c,
    wee = fixed_wee,
    sigma2 = fixed_daily_sigma2_d,
    # amp = fixed_amp,
    peak_shift = fixed_peak_shift
  ) %>% data_shaper(),
  m_sim(
    c = fixed_c,
    wee = fixed_wee,
    sigma2 = fixed_daily_sigma2_h,
    ma = fixed_daily_ma_h,
    # amp = fixed_amp,
    peak_shift = fixed_peak_shift
  ) %>% data_shaper(),
  m_sim(
    c = fixed_c,
    wee = fixed_wee,
    sigma2 = fixed_daily_sigma2_h,
    ar = fixed_daily_ar_h,
    # amp = fixed_amp,
    peak_shift = fixed_peak_shift
  ) %>% data_shaper(),
  m_sim(
    c = fixed_c,
    wee = fixed_wee,
    sigma2 = fixed_daily_sigma2_h,
    ar = fixed_daily_ar_h,
    ma = fixed_daily_ma_h,
    # amp = fixed_amp,
    peak_shift = fixed_peak_shift
  ) %>% data_shaper(),
  m_sim(
    c = fixed_c,
    wee = fixed_wee,
    sigma2 = fixed_seasonal_sigma2_h,
    ma = fixed_daily_ma_h,
    sma = fixed_weekly_ma_h,
    # amp = fixed_amp,
    peak_shift = fixed_peak_shift
  ) %>% data_shaper(),
  m_sim(
    c = fixed_c,
    wee = fixed_wee,
    sigma2 = fixed_seasonal_sigma2_h,
    ar = fixed_daily_ar_h,
    sar = fixed_weekly_ar_h,
    # amp = fixed_amp,
    peak_shift = fixed_peak_shift
  ) %>% data_shaper(),
  m_sim(
    c = fixed_c,
    wee = fixed_wee,
    sigma2 = fixed_seasonal_sigma2_h,
    ma = fixed_daily_ma_h,
    sma = fixed_weekly_ma_h,
    ar = fixed_daily_ar_h,
    sar = fixed_weekly_ar_h,
    # amp = fixed_amp,
    peak_shift = fixed_peak_shift
  ) %>% data_shaper()
)

plot_rows(l_d_sim_w,
          c("WN", "MA", "AR", "ARMA", "SMA", "SAR", "SARMA") %>%
            paste0("W+",.),
          save_name = "rows-sim-7-w.pdf")


## Turning warnings back on
options(warn = 0)
