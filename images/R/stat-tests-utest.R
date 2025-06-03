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

set.seed(20250602)
p_small_d <- tibble(rsp = c(rnorm(10000, 5, 1), rnorm(10000, 6, 1)),
                    trt = gl(2, 10000, labels = c("A", "B"))) |> 
  ggplot(aes(x = rsp, fill = trt)) + 
  theme_minimal() +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = c(5, 6), color = "black", linewidth = 0.7) +
  scale_fill_okabeito() +
  scale_x_continuous(breaks = c(5,6)) +
  scale_y_continuous(limits = c(0, 0.43)) +
  geom_segment(aes(x = 5, xend = 6, y = 0.425, yend = 0.425),
               linetype = 2, color = "gray50") +
  annotate("label", x = 5.5, y = 0.425, label = expression(Delta~"="~1),
           size = 5) +  
  annotate("text", x = 2, y = 0.4, label = "p = 0.72",
           size = 4) +  
  labs(title = "Mittelwertsdifferenz ist klein",
       subtitle = "Nicht signifikanter Unterschied") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(), 
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12),        
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = 2),
        legend.position = "none") 

p_mid_d <- tibble(rsp = c(rnorm(10000, 5, 1), rnorm(10000, 8, 1)),
                  trt = gl(2, 10000, labels = c("A", "B"))) |> 
  ggplot(aes(x = rsp, fill = trt)) + 
  theme_minimal() +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = c(5, 8), color = "black", linewidth = 0.7) +
  scale_fill_okabeito() +
  scale_x_continuous(breaks = c(5,8)) +
  scale_y_continuous(limits = c(0, 0.43)) +
  geom_segment(aes(x = 5, xend = 8, y = 0.425, yend = 0.425),
               linetype = 2, color = "gray50") +
  annotate("label", x = 6.5, y = 0.425, label = expression(Delta~"="~3),
           size = 5) +  
  annotate("text", x = 2, y = 0.4, label = "p = 0.06",
           size = 4) +  
  labs(title = "Mittelwertsdifferenz ist moderat",
       subtitle = "") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(), 
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12),        
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = 2),
        legend.position = "none") 

p_high_d <- tibble(rsp = c(rnorm(10000, 5, 1), rnorm(10000, 12, 1)),
                   trt = gl(2, 10000, labels = c("A", "B"))) |> 
  ggplot(aes(x = rsp, fill = trt)) + 
  theme_minimal() +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = c(5, 12), color = "black", linewidth = 0.75) +
  scale_fill_okabeito() +
  scale_x_continuous(breaks = c(5,12)) +
  scale_y_continuous(limits = c(0, 0.43)) +
  geom_segment(aes(x = 5, xend = 12, y = 0.425, yend = 0.425),
               linetype = 2, color = "gray50") +
  annotate("label", x = 8.5, y = 0.425, label = expression(Delta~"="~7),
           size = 5) +
  annotate("text", x = 2.2, y = 0.4, label = "p = <0.001",
           size = 4) +  
  labs(title = "Mittelwertsdifferenz ist groß",
       subtitle = "Signifikanter Unterschied") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(), 
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12),        
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = 2),
        legend.position = "none") 