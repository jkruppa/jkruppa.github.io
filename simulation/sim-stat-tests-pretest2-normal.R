nonnormal_sim_tbl <- map_dfr(c(3,4,5,6,7,8,9,11,12), \(n_val){
  map_dfr(1:1000, \(...){
    sim_tbl <- tibble(y = c(rchisq(n_val, 5), rchisq(n_val, 2), rchisq(n_val, 10)),
                      x = gl(3, n_val, labels = c("A", "B", "C")))
    ols_lst <- lm(y~x, sim_tbl) |> 
      ols_test_normality() 
    
    ## all oberservations
    shapiro_full_p <- sim_tbl |> pull(y) |>  
      shapiro.test() |> pluck("p.value")  
    
    tibble(n = n_val,
           kolmogorv = ols_lst[[1]][["p.value"]],
           shapiro = ols_lst[[2]][["p.value"]],
           shapiro_full = shapiro_full_p,
           cramer = ols_lst[[3]][["p.value"]],
           anderson = ols_lst[[4]][["p.value"]]) |> 
      mutate(kolmogorv = kolmogorv <= 0.05,
             shapiro = shapiro <= 0.05,
             shapiro_full = shapiro_full <= 0.05,
             cramer = cramer <= 0.05,
             anderson = anderson <= 0.05) |> 
      pivot_longer(cols = kolmogorv:anderson,
                   names_to = "type",
                   values_to = "bool") |> 
      mutate(type = factor(type, 
                           levels = c("anderson", "cramer", "kolmogorv", 
                                      "shapiro", "shapiro_full"),
                           labels = c("Anderson", "Cramer", "Kolmogorv", 
                                      "Shapiro",  "Shapiro (gesamt)")))
  }) |> 
    group_by(n, type) |> 
    dplyr::summarise(rate = mean(bool))
})

normal_sim_tbl <- map_dfr(c(3,4,5,6,7,8,9,11,12), \(n_val){
  map_dfr(1:1000, \(...){
    sim_tbl <- tibble(y = c(rnorm(n_val, 5, 1), rnorm(n_val, 5, 3), rnorm(n_val, 5, 5)),
                      x = gl(3, n_val, labels = c("A", "B", "C")))
    ols_lst <- lm(y~x, sim_tbl) |> 
      ols_test_normality() 
    
    ## all oberservations
    shapiro_full_p <- sim_tbl |> pull(y) |>  
      shapiro.test() |> pluck("p.value")  
    
    tibble(n = n_val,
           kolmogorv = ols_lst[[1]][["p.value"]],
           shapiro = ols_lst[[2]][["p.value"]],
           shapiro_full = shapiro_full_p,
           cramer = ols_lst[[3]][["p.value"]],
           anderson = ols_lst[[4]][["p.value"]]) |> 
      mutate(kolmogorv = kolmogorv <= 0.05,
             shapiro = shapiro <= 0.05,
             shapiro_full = shapiro_full <= 0.05,
             cramer = cramer <= 0.05,
             anderson = anderson <= 0.05) |> 
      pivot_longer(cols = kolmogorv:anderson,
                   names_to = "type",
                   values_to = "bool") |> 
      mutate(type = factor(type, 
                           levels = c("anderson", "cramer", "kolmogorv", 
                                      "shapiro", "shapiro_full"),
                           labels = c("Anderson", "Cramer", "Kolmogorv", 
                                      "Shapiro",  "Shapiro (gesamt)")))
  }) |> 
    group_by(n, type) |> 
    dplyr::summarise(rate = 1-mean(bool))
})