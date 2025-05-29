var_hetero_sim_tbl <- map_dfr(c(3,4,5,6,7,8,9,11,12), \(n_val){
  map_dfr(1:1000, \(...){
    sim_tbl <- tibble(y = c(rnorm(n_val, 5, 1), rnorm(n_val, 5, 3), rnorm(n_val, 5, 5)),
                      x = gl(3, n_val, labels = c("A", "B", "C")))
    levene_p <- car::leveneTest(y ~ x, data = sim_tbl)[,3][1]
    bartlett_p <- bartlett.test(y ~ x, data = sim_tbl)$p.value
    fligner_p <- fligner.test(y ~ x, data = sim_tbl)$p.value
    
    tibble(n = n_val,
           levene = levene_p,
           bartlett = bartlett_p,
           fligner = fligner_p) |> 
      mutate(levene_5 = levene <= 0.05,
             bartlett_5 = bartlett <= 0.05,
             fligner_5 = fligner <= 0.05,
             levene_20 = levene <= 0.2,
             bartlett_20 = bartlett <= 0.2,
             fligner_20 = fligner <= 0.2) |> 
      pivot_longer(cols = levene_5:fligner_20,
                   names_to = c("type", "alpha"),
                   names_sep = "_",
                   values_to = "bool") |> 
      mutate(type = factor(type,
                           levels = c("bartlett", "levene", "fligner"),
                           labels = c("Bartlett", "Levene", "Fligner")),
             alpha = factor(alpha, levels = c(5, 20), labels = c("5%", "20%")))
  }) |> 
    group_by(n, type, alpha) |> 
    dplyr::summarise(rate = mean(bool))
})

var_homo_sim_tbl <- map_dfr(c(3,4,5,6,7,8,9,11,12), \(n_val){
  map_dfr(1:1000, \(...){
    sim_tbl <- tibble(y = c(rnorm(n_val, 5, 3), rnorm(n_val, 5, 3), rnorm(n_val, 5, 3)),
                      x = gl(3, n_val, labels = c("A", "B", "C")))
    levene_p <- car::leveneTest(y ~ x, data = sim_tbl)[,3][1]
    bartlett_p <- bartlett.test(y ~ x, data = sim_tbl)$p.value
    fligner_p <- fligner.test(y ~ x, data = sim_tbl)$p.value
    
    tibble(n = n_val,
           levene = levene_p,
           bartlett = bartlett_p,
           fligner = fligner_p) |> 
      mutate(levene_5 = levene <= 0.05,
             bartlett_5 = bartlett <= 0.05,
             fligner_5 = fligner <= 0.05,
             levene_20 = levene <= 0.2,
             bartlett_20 = bartlett <= 0.2,
             fligner_20 = fligner <= 0.2) |> 
      pivot_longer(cols = levene_5:fligner_20,
                   names_to = c("type", "alpha"),
                   names_sep = "_",
                   values_to = "bool") |> 
      mutate(type = factor(type,
                           levels = c("bartlett", "levene", "fligner"),
                           labels = c("Bartlett", "Levene", "Fligner")),
             alpha = factor(alpha, levels = c(5, 20), labels = c("5%", "20%")))
  }) |> 
    group_by(n, type, alpha) |> 
    dplyr::summarise(rate = 1-mean(bool))
})
