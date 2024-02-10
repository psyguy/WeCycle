plot_rows <- function(l_d,
                      l_labels = NULL,
                      save_name = "plot-rows.pdf",
                      save_dir = "figures",
                      ...){

  n_rows <- length(l_d)

  l_hist <- list()
  l_seq <- list()
  l_dowe <- list()
  l_psd <- list()
  l_acf <- list()
  l_pacf <- list()

  title_r <- NULL

  for(r in 1:n_rows){

    d <- l_d[[r]]

    if(length(l_labels) == n_rows)
      title_r <- l_labels[[r]]

    rm_titles <- TRUE
    rm_xlab <- TRUE


    if(r == 1){
      rm_titles <- FALSE
      rm_xlab <- TRUE
    }

    if(r == n_rows){
      rm_titles <- TRUE
      rm_xlab <- FALSE
    }

    if(n_rows == 1){
      rm_titles <- FALSE
      rm_xlab <- FALSE
    }


    l_hist[[r]] <-  #label_plot(r) +
      plot_hist(d,
                title = title_r,
                remove_titles = rm_titles,
                remove_xlab = rm_xlab,
                ...)
    l_seq[[r]] <-  plot_seq(d,
                            remove_titles = rm_titles,
                            remove_xlab = rm_xlab,
                            ...)
    l_dowe[[r]] <-  plot_dowe(d,
                              remove_titles = rm_titles,
                              remove_xlab = rm_xlab,
                              ...)
    l_psd[[r]] <-  plot_psd(d,
                            remove_titles = rm_titles,
                            remove_xlab = rm_xlab,
                            ...)
    l_acf[[r]] <-  plot_acf(d,
                            remove_titles = rm_titles,
                            remove_xlab = rm_xlab,
                            ...)
    l_pacf[[r]] <-  plot_pacf(d,
                              remove_titles = rm_titles,
                              remove_xlab = rm_xlab,
                              ...)

  }


  p_tot <- cowplot::plot_grid(
    l_hist %>% wrap_plots(ncol = 1),
    l_seq %>% wrap_plots(ncol = 1),
    l_dowe %>% wrap_plots(ncol = 1),
    l_psd %>% wrap_plots(ncol = 1),
    l_acf %>% wrap_plots(ncol = 1),
    l_pacf %>% wrap_plots(ncol = 1),
    nrow = 1,
    rel_widths = c(.75, 2, 1, 1, 1, 1)
  )

  if(is.character(save_name))
    ggsave(
      here(save_dir, save_name),
      p_tot,
      width = 40,
      height = 4*n_rows + 0.5,
      units = "cm"
    )

}

