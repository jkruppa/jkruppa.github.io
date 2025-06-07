p_t_u_tbl <- map_dfr(c(4,5,6,7,8), \(n_val){
  map_dfr(1:10000, \(...){
    #    sim_tbl <- tibble(y = c(rnorm(n_val, 10, 2), rnorm(n_val, 10, 2)),
    #                      x = gl(2, n_val, labels = c("A", "B")))
    sim_tbl <- tibble(y = c(rlnorm(n_val, 5, 1), rlnorm(n_val, 5, 1)),
                      x = gl(2, n_val, labels = c("A", "B"))) |> 
      mutate(y_rank = rank(y))
    wilcox_p <- wilcox.test(y ~ x, data = sim_tbl)$p.value
    ttest_p <- t.test(y_rank ~ x, data = sim_tbl)$p.value
    tibble(n = as_factor(n_val),
           wilcox = wilcox_p,
           ttest = ttest_p,
           dev = ttest - wilcox)
  })
}, .progress = FALSE) 