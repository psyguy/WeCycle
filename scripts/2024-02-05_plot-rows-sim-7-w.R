##########################################################
####################### 2023-09-12 #######################
## This file plots all the profiles needed in the paper ##
## Re-run if changes needed (e.g., to parameter values) ##
## See 2023-08-07_man_figures if you needed 3-row plots ##
##########################################################


# # Initialization ----------------------------------------------------------
#
# source(here::here("scripts",
#                   "0_requirements.R"))
# # source(here::here("scripts",
# #                   "codes_manuscript-text.R"))
# source(here::here("scripts",
#                   "oop_implementation.R"))
# source(here::here("scripts",
#                   "man_functions.R"))
# readRDS(here::here("empirical-data-private",
#                    "d_w.rds"))

## Suppressing warnings
options(warn = -1)

# Setting plot specifications ---------------------------------------------

theme_set(ggthemes::theme_few())

fig_width <- 35
fig_height <- 15

fig_folder <- "figures"
# fig_folder <- "C:/Users/haqiq001/Dropbox/Apps/Overleaf/Manuscript_weekly-cycles/figures"


# Fixed parameters for the mean structure ---------------------------------

fixed_mu <- 0 # 1
fixed_dowe <- c(0.5, 1, 1, -2, 1, 3, -1)
fixed_c <- 0.5
fixed_wee <- 2.5
fixed_amp <- 0 # 1.5
fixed_peak_shift <- 6 #2


# Fixed parameters for the daily processes --------------------------------

## Fixed sigma2 for daily, weekly, and seasonal models
fixed_daily_sigma2_c <- 9
fixed_daily_sigma2_d <- 9
fixed_daily_sigma2_h <- 9

fixed_weekly_sigma2_c <- 9
fixed_weekly_sigma2_d <- 9
fixed_weekly_sigma2_h <- 9

fixed_seasonal_sigma2_c <- 9 # fixed_daily_sigma2_c + fixed_weekly_sigma2_c
fixed_seasonal_sigma2_d <- 9 # fixed_daily_sigma2_d + fixed_weekly_sigma2_d
fixed_seasonal_sigma2_h <- 9 # fixed_daily_sigma2_h + fixed_weekly_sigma2_h

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


### 7-row figure for harmonic mean

p_daily_wn_w <- m_sim(
  c = fixed_c,
  wee = fixed_wee,
  sigma2 = fixed_daily_sigma2_d,
  amp = fixed_amp,
  peak_shift = fixed_peak_shift) %>%
  plot_row(remove_titles = FALSE)

p_daily_ma_w <- m_sim(
  c = fixed_c,
  wee = fixed_wee,
  sigma2 = fixed_daily_sigma2_h,
  ma = fixed_daily_ma_h,
  amp = fixed_amp,
  peak_shift = fixed_peak_shift) %>%
  plot_row()

p_daily_ar_w <- m_sim(
  c = fixed_c,
  wee = fixed_wee,
  sigma2 = fixed_daily_sigma2_h,
  ar = fixed_daily_ar_h,
  amp = fixed_amp,
  peak_shift = fixed_peak_shift) %>%
  plot_row()

p_daily_arma_w <- m_sim(c = fixed_c,
                        wee = fixed_wee,
                        sigma2 = fixed_daily_sigma2_h,
                        ar = fixed_daily_ar_h,
                        ma = fixed_daily_ma_h,
                        amp = fixed_amp,
                        peak_shift = fixed_peak_shift) %>%
  plot_row()

p_seasonal_ma_w <- m_sim(
  c = fixed_c,
  wee = fixed_wee,
  sigma2 = fixed_seasonal_sigma2_h,
  ma = fixed_daily_ma_h,
  sma = fixed_weekly_ma_h,
  amp = fixed_amp,
  peak_shift = fixed_peak_shift) %>%
  plot_row()

p_seasonal_ar_w <- m_sim(
  c = fixed_c,
  wee = fixed_wee,
  sigma2 = fixed_seasonal_sigma2_h,
  ar = fixed_daily_ar_h,
  sar = fixed_weekly_ar_h,
  amp = fixed_amp,
  peak_shift = fixed_peak_shift) %>%
  plot_row()

p_seasonal_arma_w <- m_sim(
  c = fixed_c,
  wee = fixed_wee,
  sigma2 = fixed_seasonal_sigma2_h,
  ma = fixed_daily_ma_h,
  sma = fixed_weekly_ma_h,
  ar = fixed_daily_ar_h,
  sar = fixed_weekly_ar_h,
  amp = fixed_amp,
  peak_shift = fixed_peak_shift
) %>%
  plot_row(remove_xlab = FALSE)

l_p_h <- cowplot::plot_grid(
  p_daily_wn_w,
  p_daily_ma_w,
  p_daily_ar_w,
  p_daily_arma_w,
  p_seasonal_ma_w,
  p_seasonal_ar_w,
  p_seasonal_arma_w,
  rel_heights = c(1.1, 1, 1, 1, 1, 1, 1.1),
  ncol = 1
)
ggsave(
  here::here(fig_folder,
             "rows-sim-7-w.pdf"),
  l_p_h,
  width = fig_width,
  height = fig_height * 1.1 * 7/3,
  units = "cm"
)


## Turning warnings back on
options(warn = 0)
