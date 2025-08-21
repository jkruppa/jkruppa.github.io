mean_sim_tbl <- map_dfr(c(2, 3, 4, 5, 10, 15, 20, 50, 1000), \(n_val){
  map_dfr(1:1000, \(...){
    sim_vec <- rnorm(n_val, 0, 1)
    sim_mean <- mean(sim_vec)
    
    tibble(n = n_val,
           mean = sim_mean)
  })
})
