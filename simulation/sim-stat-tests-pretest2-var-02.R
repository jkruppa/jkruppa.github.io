var_hetero_2_sim_tbl <- map_dfr(c(3,4,5,6,7,8,9,11,12), \(n_val){
  map_dfr(1:1000, \(...){
    sim_tbl <- tibble(y = c(rnorm(n_val, 5, 1), rnorm(n_val, 5, 3), rnorm(n_val, 5, 5)),
                      x = gl(3, n_val, labels = c("A", "B", "C")))
    
    bp_p <- lmtest::bptest(y ~ x, data = sim_tbl)$p.value 
    ncv_p <- lm(y ~ x, data = sim_tbl) |> 
      car::ncvTest() |> 
      pluck("p")
    white_p <- lm(y ~ x, data = sim_tbl) |> 
      skedastic::white() |> 
      pluck("p.value")
  
    tibble(n = n_val,
           bp = bp_p,
           ncv = ncv_p,
           white = white_p) |> 
      mutate(bp_5 = bp <= 0.05,
             ncv_5 = ncv <= 0.05,
             white_5 = white <= 0.05,
             bp_20 = bp <= 0.2,
             ncv_20 = ncv <= 0.2,
             white_20 = white <= 0.2) |> 
      pivot_longer(cols = bp_5:white_20,
                   names_to = c("type", "alpha"),
                   names_sep = "_",
                   values_to = "bool") |> 
      mutate(type = factor(type,
                           levels = c("bp", "ncv", "white"),
                           labels = c("Breusch-Pagan", "NCV", "White")),
             alpha = factor(alpha, levels = c(5, 20), labels = c("5%", "20%")))
  }) |> 
    group_by(n, type, alpha) |> 
    dplyr::summarise(rate = mean(bool))
})

var_homo_2_sim_tbl <- map_dfr(c(3,4,5,6,7,8,9,11,12), \(n_val){
  map_dfr(1:1000, \(...){
    sim_tbl <- tibble(y = c(rnorm(n_val, 5, 3), rnorm(n_val, 5, 3), rnorm(n_val, 5, 3)),
                      x = gl(3, n_val, labels = c("A", "B", "C")))
    
    bp_p <- lmtest::bptest(y ~ x, data = sim_tbl)$p.value 
    ncv_p <- lm(y ~ x, data = sim_tbl) |> 
      car::ncvTest() |> 
      pluck("p")
    white_p <- lm(y ~ x, data = sim_tbl) |> 
      skedastic::white() |> 
      pluck("p.value")
    
    tibble(n = n_val,
           bp = bp_p,
           ncv = ncv_p,
           white = white_p) |> 
      mutate(bp_5 = bp <= 0.05,
             ncv_5 = ncv <= 0.05,
             white_5 = white <= 0.05,
             bp_20 = bp <= 0.2,
             ncv_20 = ncv <= 0.2,
             white_20 = white <= 0.2) |> 
      pivot_longer(cols = bp_5:white_20,
                   names_to = c("type", "alpha"),
                   names_sep = "_",
                   values_to = "bool") |> 
      mutate(type = factor(type,
                           levels = c("bp", "ncv", "white"),
                           labels = c("Breusch-Pagan", "NCV", "White")),
             alpha = factor(alpha, levels = c(5, 20), labels = c("5%", "20%")))
    
  }) |> 
    group_by(n, type, alpha) |> 
    dplyr::summarise(rate = 1-mean(bool))
})
