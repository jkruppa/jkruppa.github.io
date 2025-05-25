var_hetero_sim_tbl <- map_dfr(c(3,4,5,6,7,8,9,11,12), \(n_val){
  map_dfr(1:1000, \(...){
    sim_tbl <- tibble(y = c(rnorm(n_val, 5, 1), rnorm(n_val, 5, 3), rnorm(n_val, 5, 5)),
                      x = gl(3, n_val, labels = c("A", "B", "C")))
    levene_p <- car::leveneTest(y ~ x, data = sim_tbl)[,3][1]
    bartlett_p <- bartlett.test(y ~ x, data = sim_tbl)$p.value
    
    tibble(n = n_val,
           levene = levene_p,
           bartlett = bartlett_p) |> 
      mutate(levene = levene <= 0.05,
             bartlett = bartlett <= 0.05) |> 
      pivot_longer(cols = levene:bartlett,
                   names_to = "type",
                   values_to = "bool") |> 
      mutate(type = factor(type, labels = c("Bartlett", "Levene")))
  }) |> 
    group_by(n, type) |> 
    dplyr::summarise(rate = mean(bool))
})

var_homo_sim_tbl <- map_dfr(c(3,4,5,6,7,8,9,11,12), \(n_val){
  map_dfr(1:1000, \(...){
    sim_tbl <- tibble(y = c(rnorm(n_val, 5, 3), rnorm(n_val, 5, 3), rnorm(n_val, 5, 3)),
                      x = gl(3, n_val, labels = c("A", "B", "C")))
    levene_p <- car::leveneTest(y ~ x, data = sim_tbl)[,3][1]
    bartlett_p <- bartlett.test(y ~ x, data = sim_tbl)$p.value
    
    tibble(n = n_val,
           levene = levene_p,
           bartlett = bartlett_p) |> 
      mutate(levene = levene <= 0.05,
             bartlett = bartlett <= 0.05) |> 
      pivot_longer(cols = levene:bartlett,
                   names_to = "type",
                   values_to = "bool") |> 
      mutate(type = factor(type, labels = c("Bartlett", "Levene")))
  }) |> 
    group_by(n, type) |> 
    dplyr::summarise(rate = 1-mean(bool))
})