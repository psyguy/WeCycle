## @knitr data_prep




## @knitr m_sim

m_sim <- function(n = 10000,
                  ar = 0,
                  ma = 0,
                  sar = 0,
                  sma = 0,
                  c = 0,
                  iorder = 0,
                  siorder = 0,
                  sigma2 = 4,
                  dowe = rep(0, 7),
                  amp = 0,
                  peak_shift = 1,
                  burnin = 500,
                  seed = 0) {

  ## Generating (time-varying) mean structure mu_t
  # Generating sequence of d_t
  d_t <- cbind(
    Mon = rep(c(1, 0, 0, 0, 0, 0, 0), length.out = n),
    Tue = rep(c(0, 1, 0, 0, 0, 0, 0), length.out = n),
    Wed = rep(c(0, 0, 1, 0, 0, 0, 0), length.out = n),
    Thu = rep(c(0, 0, 0, 1, 0, 0, 0), length.out = n),
    Fri = rep(c(0, 0, 0, 0, 1, 0, 0), length.out = n),
    Sat = rep(c(0, 0, 0, 0, 0, 1, 0), length.out = n),
    Sun = rep(c(0, 0, 0, 0, 0, 0, 1), length.out = n)
  ) %*%
    dowe %>%
    as.numeric()

  # Generating sequence of h_t (if amp = 0, h_t = c)
  h_t <- c + amp*cos((2*pi/7)*((1:n) - peak_shift))

  # Adding d_t and h_t together for extra capabilities
  mu_t <- d_t + h_t

  ## Generating stochastic component a_t
  # Setting the seed
  set.seed(seed)

  # Making the list of model specifications
  l_model = list(ar = ar,
                 ma = ma,
                 sar = sar,
                 sma = sma,
                 iorder = iorder,
                 siorder = siorder,
                 nseasons = 7,
                 sigma2 = sigma2
  )

  # simulating the data
  if (sum(abs(as.numeric(l_model))) > 7 + sigma2)
    a_t <- sarima::sim_sarima(n = n,
                              model = l_model,
                              n.start = burnin)
  else
    a_t <- rnorm(n + burnin,
                 mean = 0,
                 sd = sqrt(sigma2)) %>%
    tail(n)

  ## Making the complete time series
  y_t <- mu_t + a_t
  return(y_t)
}
