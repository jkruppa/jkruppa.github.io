p1_sample <- min_wilcox_eff_tbl |> 
  mutate(n = fct_rev(n)) |> 
  ggplot(aes(delta_val, p_mean, color = n)) +
  theme_minimal() +
  geom_point(show.legend = FALSE) +
  geom_line() +
  geom_hline(yintercept = 0.05, color = "gray50", linewidth = 1) +
  scale_color_okabeito() +
  scale_y_continuous(breaks = c(0, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5),
                     limits = c(0, 0.5),
                     labels = scales::percent) +
  scale_x_continuous(breaks = 1:10) +
  labs(x = "Mittelwertsdifferenz",     
       y = "Mittlerer p-Wert",
       color = expression(n[1]*","~n[2])) +  
  theme(axis.title.y = element_text(size = 14, face = 2),
        axis.text.y = element_text(size = 12), 
        axis.title.x = element_text(size = 14, face = 2),
        axis.text.x = element_text(size = 12),        
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 2),
        legend.position = c(0.8, 0.65),
        legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid", 
                                         colour ="gray50")) 

p2_sample <- min_wilcox_eff_tbl |> 
  mutate(n = fct_rev(n)) |> 
  ggplot(aes(delta_val, p_min, color = n)) +
  theme_minimal() +
  geom_point(show.legend = FALSE) +
  geom_line() +
  geom_hline(yintercept = 0.05, color = "gray50", linewidth = 1) +
  scale_color_okabeito() +
  scale_y_continuous(breaks = c(0, 0.05, 0.1, 0.15),
                     limits = c(0, 0.15),
                     labels = scales::percent) +
  scale_x_continuous(breaks = 1:10) +
  labs(x = "Mittelwertsdifferenz",     
       y = "Minimal p-Wert",
       color = expression(n[1]*","~n[2])) +  
  theme(axis.title.y = element_text(size = 14, face = 2),
        axis.text.y = element_text(size = 12), 
        axis.title.x = element_text(size = 14, face = 2),
        axis.text.x = element_text(size = 12),        
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = 2),
        legend.position = "none") 