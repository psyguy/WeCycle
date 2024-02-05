r_b <- results_estimates %>%
  filter(parameter == "sigma2",
         # id %in% 1:10,
         item %in% c("pa", "na")) %>%
  select(-parameter:-sig,
         -AIC,
         -BIC,
         -ll_abs) %>%
  group_by(id, item) %>%
  mutate(win_AICc = case_when(min(AICc) == AICc ~ 1)) %>%
  filter(win_AICc == 1)
  group_by(across(mu:model_dynamics),
           item,
           id,
           .add = FALSE) %>%
  # filter(win_AICc == 1) %>%
  # mutate(pa_model_name = model_name,
  #        na_model_name = model_name,
  #        pa_error_name_words = error_name_words,
  #        na_error_name_words = error_name_words,
  #        pa_error_dynamics = error_dynamics,
  #        na_error_dynamics = error_dynamics,
  #        pa_error_timescale = error_timescale,
  #        na_error_timescale = error_timescale,
  #        pa_mu = mu,
  #        na_mu = mu) %>%
  arrange(id, item) %>%
  summarise(n_w_AICc = sum(win_AICc)) %>%
  mutate(
    pa_model_name = model_name,
    na_model_name = model_name,
    pa_mu = mu,
    na_mu = mu
  )


# Merge the datasets by 'id'
merged_data <- merge(r_b %>% filter(item=="pa"),
                       r_b %>% filter(item=="na"),
                       by = "id")

# Create the confusion table
confusion_table <- table(merged_data$mu.x, merged_data$mu.y)

table(merged_data$mu.x,
      merged_data$mu.y) %>%
  write.csv("mu.csv")
table(merged_data$error_dynamics.x,
      merged_data$error_dynamics.y) %>%
  write.csv("error_dynamics.csv")
table(merged_data$error_timescale.x,
      merged_data$error_timescale.y) %>%
  write.csv("error_timescale.csv")
table(merged_data$error_name_words.x,
      merged_data$error_name_words.y) %>%
  write.csv("error_name_words.csv")
table(merged_data$model_name.x,
      merged_data$model_name.y) %>%
  write.csv("model_name.csv")




r_t_tmp <- r_b %>%
  # filter(item %in% c("pa", "na")) %>%

r_t_pa <- r_t_tmp %>%
  filter(item == "pa")

r_t_na <- r_t_tmp %>%
  filter(item == "na")

xtabs(n_w_AICc ~ pa_model_name + na_model_name,
      r_t)

xtabs(n_w_AICc ~ pa_error_name_words + na_error_name_words,
      r_t_tmp)

xtabs(n_w_AICc ~ pa_error_dynamics + na_error_dynamics,
      r_t)

xtabs(n_w_AICc ~ pa_error_timescale + na_error_timescale,
      r_t)

xtabs(n_w_AICc ~ pa_mu + na_mu,
      r_t)

  # pivot_wider(id_cols = "model_name",
  #             names_from = "item",
  #             values_from = "win_AICc")

r_t_pa <- r_t_base %>%
  filter(item == "pa") %>%
  # filter(n_w_AICc == 1) %>%
  mutate(pa_model_name = model_name) %>%
  ungroup() %>%
  select(pa_model_name, n_w_AICc)

r_t_na <- r_t_base %>%
  filter(item == "na") %>%
  # filter(n_w_AICc == 1) %>%
  mutate(na_model_name = model_name) %>%
  ungroup() %>%
  select(na_model_name, n_w_AICc)

r_t <- r_t_pa %>%
  # select(pa_win_AICc, ) %>%
  full_join(r_t_na,
            by = )
  # filter(pa_AICc  == 1,
         # na_AICc == 1)

# r_t %>% xtabs(pa_n_w_AICc ~ na_n_w_AICc,
#                .)
#
# table(r_t$pa_n_w_AICc, r_t$na_n_w_AICc)

# mutate(item = item %>%
  #          as.character() %>%
  #          as.factor()) %>%
  # pivot_wider(names_from = "item",
  #             id_expand = TRUE,
  #             values_from = "n_w_AICc")
