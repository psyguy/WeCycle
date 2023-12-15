Sys.setenv("OMP_THREAD_LIMIT" = 46)
options(warn = -1)

library(doFuture)
library(future)
registerDoFuture()
# plan("multicore")
# cl <- parallel::makeCluster(46)
plan("multisession",
     workers = 46)


table_conditions <- expand.grid(id = 1:98,
                                mu = c("c", "d", "h"),
                                ar = c(0,1),
                                ma = c(0,1),
                                sar = c(0,1),
                                sma = c(0,1))

Sys.time()

fit_logs <- foreach(i = 1:nrow(table_conditions)) %dopar% {

  t_c <- table_conditions[i,]

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
    m_fit(d_pa_i,
          .,
          id = t_c$id,
          save_folder_fit = "fff",
          save_fit = TRUE,
          save_est = FALSE)
  }, error=function(e) NULL)

      paste(i,
            "done at",
            Sys.time()) %>%
        print()
    }

Sys.time()

#
# registerDoFuture()
# plan("multisession",
#      workers = 46)
#
# fit_files <- list.files("fits", pattern = ".rds")
#
# # for(i in 1:length(fit_files[1:50])){
# #
# #   %>%
# #     mutate(id = id_,
# #            model = paste(model_what_, collapse = " "),
# #            .before = 1) %>%
# #     mutate(time_taken = (Sys.time() - time_start) %>%
# #              as.numeric() %>%
# #              round(4))
# #
# #   paste(id_,
# #         paste(model_what_, collapse = " "),
# #         "took",
# #         df_tmp$time_taken,
# #         "seconds") %>%
# #     print()
# #   df <- rbind(df, df_tmp)
# #
# # }
#
#
# Sys.time()
#
# fits_raw <-
#   foreach(i = 1:length(fit_files),
#           # .combine = rbind,
#           .errorhandling = 'remove') %dopar% {
#             print(i)
#             ff <- readRDS(paste0(output_dir,
#                            "/",
#                            fit_files[i]))
#             ff$filename <- fit_files[i]
#             ff
#           }
#
# Sys.time()
#
# saveRDS(fits_raw,
#         "fits_raw_all-combinations_2023-06-12.rds")
#
# # information_criteria <-
# #
# #   foreach(
# #     i = 1:length(fit_files),
# #     .combine = rbind,
# #     .errorhandling = 'remove'
# #   ) %dopar% {
# #
# #     dd <- readRDS(paste0(output_dir,
# #                          "/",
# #                          fit_files[i]))
# #     dd$information_criteria %>%
# #       as.data.frame() %>%
# #       mutate(file_name = fit_files[i],
# #              time_taken = dd$time_taken) %>%
# #       mutate(id = file_name %>%
# #                gsub(".*_id-", "", .) %>%
# #                gsub("_model.*", "", .),
# #              model = file_name %>%
# #                gsub(".*_model-", "", .) %>%
# #                gsub(".rds", "", .) %>%
# #                gsub("-", " ", .)
# #       )
# #   }
#
#
# d <- readRDS("fits/item-avg.pa_id-1_model-harmonic-ar-ma-sar-sma.rds")
#
#
# xx <- information_criteria %>%
#   select(-AIC.1, -ll_abs) %>%
#   filter(!grepl("dif",model)) %>%
#   ungroup() %>%
#   pivot_longer(cols = AIC:BIC,
#                names_to = "criterion_name",
#                values_to = "criterion_value") %>%
#   group_by(criterion_name, id) %>%
#   # filter(rank(x, ties.method="first")==1)
#   slice_min(order_by =criterion_value) %>%
# # %>%
#   # filter(criterion_name == "AICc") %>%
#   # pull(model) %>%
#   ungroup() %>%
#   select(model, criterion_name) %>%
#   table()
#
# round(xx/.98,1)
#
# colSums(xx)
