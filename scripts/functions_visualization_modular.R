data_shaper <- function(d) {

  weekdays <- c("Mon",
                "Tue",
                "Wed",
                "Thu",
                "Fri",
                "Sat",
                "Sun")

  if (!is.data.frame(d))
    d <- data.frame(
      t = 1:length(d),
      y = d,
      weekday = rep(weekdays, length.out = length(d)),
      week_num = rep(1:ceiling(length(d) / 7), each = 7)[1:length(d)]
    )

  if ("date" %in% colnames(d))
    d <- d %>%
      mutate(
        weekday = lubridate::wday(date,
                                  week_start = 1,
                                  label = TRUE),
        weekday_num = lubridate::wday(date,
                                      week_start = 1,
                                      label = FALSE)
      )

  if (!("weekday_num" %in% colnames(d)))
    d$weekday_num <- match(d$weekday, weekdays)

  if (!("week_num" %in% colnames(d))) {
    # Initialize week number and an empty vector to store week numbers
    week_number <- 1
    week_numbers <- numeric(length(d$weekday_num))
    # Check if the sequence starts with a day other than Monday and adjust week_number accordingly
    if (d$weekday_num[1] != 1) {
      week_number <- 1
    } else {
      week_number <-
        2  # Start from week 2 if the first day is Monday, to handle edge cases
    }
    # Iterate through the days, increasing week number after encountering a Sunday
    for (i in 1:length(d$weekday_num)) {
      week_numbers[i] <- week_number
      if (d$weekday_num[i] == 7 &&
          i != length(d$weekday_num)) {
        # Check for Sunday and not the last element
        week_number <- week_number + 1
      }
    }
    d$week_num <- week_numbers
  }

  # Substituting NA's for implicit missing values
  d_out <- d %>%
    right_join(data.frame(t = min(d$t):max(d$t)),
               by = "t") %>%
    mutate(weekday = weekday %>% factor(weekdays)) %>%
    arrange(t)

  return(d_out)

  }

## @knitr plot_rowsss

plot_hist <- function(d = NULL,
                      title = " ",
                      subtitle = NULL,
                      remove_titles = TRUE,
                      remove_xlab = TRUE,
                      scale_rel = 0.9,
                      max_acf.lag = 35,
                      max_period = 15,
                      ymin = 0-0.1,
                      ymax = 4+0.1,
                      max_t = 140,
                      max_weeks = 25,
                      col_weekly = "lightsteelblue4",
                      col_dowe.line = "mediumorchid4",
                      col_dowe.point = "deeppink1",
                      col_ts = "lightsteelblue4",
                      col_ts.point = "cornflowerblue",
                      col_hist = "cornflowerblue",
                      col_acf = "darkolivegreen3",
                      col_pacf = "darkorange3",
                      col_spec = "darkorchid4",
                      col_hlines = "dimgray") {

  # Making sure the optimal theme is in place
  theme_set(ggthemes::theme_few())

  breaks_y <- seq(floor(ymin),
                  ceiling(ymax))
  # Making sure the limits are not off
  ymin <- min(min(d$y), ymin)
  ymax <- max(max(d$y), ymax)

  p_out <- d %>%
    data_shaper() %>%
    mutate(group_mean = mean(y,
                             na.rm = TRUE)) %>%
    group_by(weekday,
             .add = TRUE) %>%
    mutate(weekday_mean = mean(y,
                               na.rm = TRUE)) %>%
    ungroup() %>%
    ggplot() +
    aes(x = y) +
    geom_histogram(
      aes(y = after_stat(ndensity)),
      center = 0,
      bins = 40,
      fill = col_hist
    ) +
    geom_vline(
      aes(xintercept = group_mean),
      linetype = "dashed",
      linewidth = rel(scale_rel * 0.2)
    ) +
    labs(subtitle = "Distribution",
         x = "y",
         y = title) +
    scale_x_continuous(breaks = breaks_y) +
    xlim(ymin, ymax) +
    theme(
      panel.grid.major = element_blank(),
      # axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      axis.title.y = element_text(size = rel(1.4*scale_rel)),
      axis.text.y = element_blank()
    )

    if (remove_titles)
      p_out <- p_out + theme(plot.subtitle = element_blank())
    if (remove_xlab)
      p_out <- p_out + xlab(NULL)

  p_out

  }


plot_seq <- function(d = NULL,
                     title = NULL,
                     subtitle = NULL,
                     remove_titles = TRUE,
                     remove_xlab = TRUE,
                     scale_rel = 0.9,
                     max_acf.lag = 35,
                     max_period = 15,
                     ymin = 0-0.1,
                     ymax = 4+0.1,
                     max_t = 140,
                     max_weeks = 25,
                     col_weekly = "lightsteelblue4",
                     col_dowe.line = "mediumorchid4",
                     col_dowe.point = "deeppink1",
                     col_ts = "lightsteelblue4",
                     col_ts.point = "cornflowerblue",
                     col_hist = "cornflowerblue",
                     col_acf = "darkolivegreen3",
                     col_pacf = "darkorange3",
                     col_spec = "darkorchid4",
                     col_hlines = "dimgray") {

  # Making sure the limits are not off
  ymin <- min(min(d$y), ymin)
  ymax <- max(max(d$y), ymax)

  p_out <- d %>%
    data_shaper() %>%
    mutate(group_mean = mean(y,
                             na.rm = TRUE)) %>%
    group_by(weekday,
             .add = TRUE) %>%
    mutate(weekday_mean = mean(y,
                               na.rm = TRUE)) %>%
    ungroup() %>%
    ggplot() +
    aes(x = t,
        y = y) +
    geom_hline(
      aes(yintercept = group_mean),
      linetype = "dashed",
      linewidth = rel(scale_rel * 0.2)
    ) +
    geom_line(
      color = col_ts,
      alpha = 1,
      linewidth = rel(scale_rel * 0.2)
    ) +
    geom_point(aes(y = y),
               color = col_ts.point,
               size = rel(scale_rel * 0.12)) +
    scale_y_continuous(# breaks = breaks_y,
      limits = c(ymin, ymax)) +
    xlim(0, max_t) +
    labs(subtitle = "Sequence plot",
         x = "t",
         y = "y")

    if (remove_titles)
      p_out <- p_out + theme(plot.subtitle = element_blank())
    if (remove_xlab)
      p_out <- p_out + xlab(NULL)
    p_out

    }

plot_dowe <- function(d = NULL,
                     title = NULL,
                     subtitle = NULL,
                     remove_titles = TRUE,
                     remove_xlab = TRUE,
                     scale_rel = 0.9,
                     max_acf.lag = 35,
                     max_period = 15,
                     ymin = 0-0.1,
                     ymax = 4+0.1,
                     max_t = 140,
                     max_weeks = 25,
                     col_weekly = "lightsteelblue4",
                     col_dowe.line = "mediumorchid4",
                     col_dowe.point = "deeppink1",
                     col_ts = "lightsteelblue4",
                     col_ts.point = "cornflowerblue",
                     col_hist = "cornflowerblue",
                     col_acf = "darkolivegreen3",
                     col_pacf = "darkorange3",
                     col_spec = "darkorchid4",
                     col_hlines = "dimgray") {

  # Making sure the limits are not off
  ymin <- min(min(d$y), ymin)
  ymax <- max(max(d$y), ymax)

  p_out <- d %>%
    data_shaper() %>%
    mutate(group_mean = mean(y,
                             na.rm = TRUE)) %>%
    group_by(weekday,
             .add = TRUE) %>%
    mutate(weekday_mean = mean(y,
                               na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(week_num,
             .add = FALSE) %>%
    filter(week_num <= max_weeks) %>%
    ggplot() +
    aes(x = weekday,
        y = y,
        group = week_num) +
    geom_hline(
      aes(yintercept = group_mean),
      linetype = "dashed",
      linewidth = rel(scale_rel * 0.2),
      alpha = 1
    ) +
    geom_line(
      alpha = 0.5,
      color = col_weekly,
      linewidth = rel(scale_rel * 0.4)
    ) +
    geom_point(aes(y = y),
               alpha = 0.6,
               color = col_ts.point,
               size = rel(scale_rel * 0.12)) +
    geom_line(
      aes(y = weekday_mean),
      color = col_dowe.line,
      alpha = 1,
      linewidth = rel(scale_rel * 1)
    ) +
    geom_point(aes(y = weekday_mean),
               color = col_dowe.point,
               size = rel(scale_rel * 0.8)) +
    labs(subtitle = "Weekly plot",
         x = "Weekdays",
         y = "y") +
    scale_y_continuous(#breaks = breaks_y,
      limits = c(ymin, ymax)) +
    theme(panel.grid.major.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 90,
                                     size = rel(1 * scale_rel)))

  if (remove_titles)
    p_out <- p_out + theme(plot.subtitle = element_blank())
  if (remove_xlab)
    p_out <- p_out + xlab(NULL)
  p_out

}

plot_psd <- function(d = NULL,
                      title = NULL,
                      subtitle = NULL,
                      remove_titles = TRUE,
                      remove_xlab = TRUE,
                      scale_rel = 0.9,
                      max_acf.lag = 35,
                      max_period = 15,
                      ymin = 0-0.1,
                      ymax = 4+0.1,
                      max_t = 140,
                      max_weeks = 25,
                      col_weekly = "lightsteelblue4",
                      col_dowe.line = "mediumorchid4",
                      col_dowe.point = "deeppink1",
                      col_ts = "lightsteelblue4",
                      col_ts.point = "cornflowerblue",
                      col_hist = "cornflowerblue",
                      col_acf = "darkolivegreen3",
                      col_pacf = "darkorange3",
                      col_spec = "darkorchid4",
                      col_hlines = "dimgray") {

    # Making sure the optimal theme is in place
  theme_set(ggthemes::theme_few())

  d <- d %>% data_shaper()

  # Imputing the missing values using seasonal Kalman smoothing
  y_imp <- d$y %>%
    ts(frequency = 7) %>%
    imputeTS::na_kalman()

  ## Calculate Fourier components
  y_imp <- y_imp %>% as.numeric
  n <- length(y_imp)
  Freq = (1:n - 1) / n
  var_component <- Mod(fft(scale(y_imp, scale = FALSE))) ^ 2 / n^2
  df_fourier <- data.frame(Freq = Freq,
                           rel_power = 100*var_component/var(y_imp))%>%
    mutate(Period = round(1 / Freq, 1)) %>%
    filter(Freq != 0,
           Freq <= 0.5,
           Period <= max_period) %>%
    summarise(rel_power = sum(rel_power),
              .by = Period) %>%
    mutate(Freq = 1 / Period,
           .before = 1)

  max_var_component <- max(df_fourier$rel_power)

  p_out <- df_fourier %>%
    ggplot() +
    aes(
      x = Period,
      xend = Period,
      y = rel_power,
      yend = 0
    ) +
    geom_rect(
      aes(
        xmin = 6.5,
        xmax = 7.5,
        ymin = 0,
        ymax = Inf
      ),
      alpha = 0.7,
      fill = "azure2"
    ) +
    geom_segment(linewidth = rel(scale_rel * 0.5),
                 color = col_spec) +
    scale_x_continuous(breaks =
                         seq(0,
                             max_period - 0.5,
                             7)) +
    scale_y_continuous(# labels = scaleFUN,
      breaks = scales::breaks_pretty(4),
      limits = c(0, max(1, max_var_component))) +
    theme(
      # axis.title.y = element_blank(),
      # axis.ticks.y = element_blank(),
      # axis.line.y = element_blank(),
      # axis.text.y = element_blank(),
      panel.grid.major = element_blank()
    ) +
    labs(subtitle = "Power spectral density",
         x = "Period (in days)",
         y = "% total power")

  if(max_var_component < 1)
    p_out <- p_out +
    scale_y_continuous(breaks = c(0,1),
                       limits = c(0, 1))

  if (remove_titles)
    p_out <- p_out + theme(plot.subtitle = element_blank())
  if (remove_xlab)
    p_out <- p_out + xlab(NULL)
  p_out

}


plot_acf <- function(d = NULL,
                     title = NULL,
                     subtitle = NULL,
                     remove_titles = TRUE,
                     remove_xlab = TRUE,
                     scale_rel = 0.9,
                     max_acf.lag = 35,
                     max_period = 15,
                     ymin = 0-0.1,
                     ymax = 4+0.1,
                     max_t = 140,
                     max_weeks = 25,
                     col_weekly = "lightsteelblue4",
                     col_dowe.line = "mediumorchid4",
                     col_dowe.point = "deeppink1",
                     col_ts = "lightsteelblue4",
                     col_ts.point = "cornflowerblue",
                     col_hist = "cornflowerblue",
                     col_acf = "darkolivegreen3",
                     col_pacf = "darkorange3",
                     col_spec = "darkorchid4",
                     col_hlines = "dimgray") {

  d <- d %>% data_shaper()

  # Imputing the missing values using seasonal Kalman smoothing
  y_imp <- d$y %>%
    ts(frequency = 7) %>%
    imputeTS::na_kalman()

  breaks_acf <- seq(0,
                    max_acf.lag,
                    by = 14 * floor(max_acf.lag / 7 / 3))

  df_acf <- data.frame(
    lag = c(0:max_acf.lag),
    acf = stats::acf(y_imp,
                     lag.max = max_acf.lag,
                     plot = FALSE)$acf %>%
      as.numeric()
  )

  p_out <- df_acf %>%
    ggplot(aes(x = lag,
               y = acf)) +
    geom_segment(
      aes(
        x = lag,
        xend = lag,
        y = 0,
        yend = acf
      ),
      linewidth = rel(scale_rel * 35 / max_acf.lag / 2),
      color = col_acf,
      lineend = "butt"
    ) +
    geom_hline(
      yintercept = 0,
      linewidth = rel(scale_rel * 0.3),
      linetype = "solid",
      color = col_hlines
    ) +
    scale_x_continuous(breaks = breaks_acf) +
    scale_y_continuous(breaks = c(-.5, 0, 0.5, 1),
                       limits = c(-.25, 1.1)) +
    labs(subtitle = "ACF",
         x = "Lag",
         y = "") +
    theme(
      panel.grid.major = element_blank(),
      axis.title.y = element_blank()
    )

  if (remove_titles)
    p_out <- p_out + theme(plot.subtitle = element_blank())
  if (remove_xlab)
    p_out <- p_out + xlab(NULL)
  p_out

}

plot_pacf <- function(d = NULL,
                     title = NULL,
                     subtitle = NULL,
                     remove_titles = TRUE,
                     remove_xlab = TRUE,
                     scale_rel = 0.9,
                     max_acf.lag = 35,
                     max_period = 15,
                     ymin = 0-0.1,
                     ymax = 4+0.1,
                     max_t = 140,
                     max_weeks = 25,
                     col_weekly = "lightsteelblue4",
                     col_dowe.line = "mediumorchid4",
                     col_dowe.point = "deeppink1",
                     col_ts = "lightsteelblue4",
                     col_ts.point = "cornflowerblue",
                     col_hist = "cornflowerblue",
                     col_acf = "darkolivegreen3",
                     col_pacf = "darkorange3",
                     col_spec = "darkorchid4",
                     col_hlines = "dimgray") {

  d <- d %>% data_shaper()

  # Imputing the missing values using seasonal Kalman smoothing
  y_imp <- d$y %>%
    ts(frequency = 7) %>%
    imputeTS::na_kalman()

  breaks_acf <- seq(0,
                    max_acf.lag,
                    by = 14 * floor(max_acf.lag / 7 / 3))

  df_pacf <- data.frame(
    lag = c(0:max_acf.lag),
    pacf = stats::pacf(y_imp,
                       lag.max = max_acf.lag,
                       plot = FALSE)$acf %>%
      as.numeric() %>%
      c(0, .)
  )

  p_out <- df_pacf %>%
    ggplot(aes(x = lag,
               y = pacf)) +
    geom_segment(
      aes(
        x = lag,
        xend = lag,
        y = 0,
        yend = pacf
      ),
      linewidth = rel(scale_rel * 35 / max_acf.lag / 2),
      color = col_pacf,
      lineend = "butt"
    ) +
    geom_hline(
      yintercept = 0,
      linewidth = rel(scale_rel * 0.3),
      linetype = "solid",
      color = col_hlines
    ) +
    scale_x_continuous(breaks = breaks_acf) +
    scale_y_continuous(breaks = c(-.5, 0, 0.5, 1),
                       limits = c(-.25, 1.1)) +
    labs(subtitle = "PACF",
         x = "Lag",
         y = "") +
    theme(
      panel.grid.major = element_blank(),
      axis.title.y = element_blank()
    )

  if (remove_titles)
    p_out <- p_out + theme(plot.subtitle = element_blank())
  if (remove_xlab)
    p_out <- p_out + xlab(NULL)
  p_out

}

