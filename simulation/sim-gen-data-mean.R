comb_vec <- c("5, 5", "5, 4", "5, 3", "4, 4", "4, 3", "3, 3")

min_wilcox_eff_tbl <- map_dfr(c(seq(1, 5, by = 0.5), 10), \(delta_val){
  map_dfr(comb_vec, \(comb){
    map_dfr(1:1000, \(...){ 
      n <- str_split(comb, ",", simplify = TRUE) |> as.numeric()
      sim_tbl <- tibble(y = c(rnorm(n[1], 5, 1), rnorm(n[2], 5+delta_val, 1)),
                        x = as_factor(c(rep("A", n[1]), rep("B", n[2]))))
      wilcox_p <- wilcox.test(y ~ x, data = sim_tbl)$p.value
      tibble(n = comb,
             p = wilcox_p)
    }) |> 
      group_by(n) |> 
      summarise(p_min = min(p),
                p_mean = mean(p),
                p_max = max(p))
  }) |> 
    mutate(delta_val,
           n = as_factor(n))
})
