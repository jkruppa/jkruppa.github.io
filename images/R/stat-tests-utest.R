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

set.seed(20250605)
shift_tbl <- tibble(A = c(rnorm(10, 5, 2), 7, 8, 13, 14, 14),
                    B = A + 5) |> 
  gather()

p_shift <- shift_tbl |> 
  ggplot(aes(x = value, fill = key)) +
  theme_minimal() +
  geom_density(trim = FALSE, alpha = 0.5) +
  geom_vline(xintercept = c(5, 10), color = "black", linewidth = 0.7) +
  geom_segment(aes(x = 5, xend = 10, y = 0.14, yend = 0.14),
               linetype = 2, color = "gray50",
               arrow = arrow(length = unit(0.03, "npc"), type = "closed")) +
  annotate("label", x = 7.5, y = 0.14, label = expression(a>0),
           size = 5) +  
  scale_x_continuous(limits = c(-2, 24), 
                     breaks = c(5.4, 10.4),
                     labels = c(expression(tilde(y)[dog]),
                                expression(tilde(y)[cat]))) +
  scale_fill_okabeito() +  
  scale_y_continuous(limits = c(NA, 0.15)) +
  labs(title = "Gleiche Verteilungsform mit unterschiedlicher Lage",
       subtitle = "Die beiden Verteilungen sind um den Wert a im Median verschoben") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(), 
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14),        
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = 2),
        legend.position = "none") 


hypo_null_tbl <- tibble(Dog = c(NA, NA, NA, 4, NA, NA, NA),
                        Cat = c(1,2,3,5,6,7,8)) |> 
  gather() |> 
  na.omit() |> 
  mutate(col = as_factor(c(1, 2, 2, 2, 3, 3, 3,3)),
         key = fct_rev(as_factor(key)))

p1_hypo <- hypo_null_tbl |> 
  ggplot(aes(x = value, y = key, fill = col)) +
  theme_minimal() +
  geom_point(shape = 21, size = 5, show.legend = FALSE) +
  scale_fill_okabeito() +
  annotate("text", x = c(1, 2, 3, 5, 6, 7, 8)+0.2, y = 1-0.15, 
           label = c(expression(c[1]), expression(c[2]), expression(c[3]),
                     expression(c[4]), expression(c[5]), expression(c[6]),
                     expression(c[7]))) +
  annotate("text", x = c(4)+0.2, y = 2-0.15, 
           label = expression(d[1])) +
  annotate("label", x = 6.1, y = 2, hjust = "left",
           label = expression(Pr~(Dog<Cat)~"="~frac(3, 7)~"≈"~0.5))+
  labs(x = "", y = "") +
  theme(axis.title.y = element_text(size = 14, face = 2),
        axis.text.y = element_text(size = 12), 
        axis.title.x = element_text(size = 14, face = 2),
        axis.text.x = element_blank(),
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = 2),
        legend.position = "none")

hypo_alt_tbl <- tibble(Dog = c(1, NA, NA, NA, NA, NA, NA),
                       Cat = c(2,3,4,5,6,7,8)) |> 
  gather() |> 
  na.omit() |> 
  mutate(col = as_factor(c(1, 3, 3, 3, 3, 3, 3,3)),
         key = fct_rev(as_factor(key)))

p2_hypo <- hypo_alt_tbl |> 
  ggplot(aes(x = value, y = key, fill = col)) +
  theme_minimal() +
  geom_point(shape = 21, size = 5, show.legend = FALSE) +
  scale_fill_okabeito(order = c(1,3)) +
  annotate("text", x = c(2, 3, 4, 5, 6, 7, 8)+0.2, y = 1-0.15, 
           label = c(expression(c[1]), expression(c[2]), expression(c[3]),
                     expression(c[4]), expression(c[5]), expression(c[6]),
                     expression(c[7]))) +
  annotate("text", x = c(1)+0.2, y = 2-0.15, 
           label = expression(d[1])) +
  annotate("label", x = 6.1, y = 2, hjust = "left",
           label = expression(Pr~(Dog<Cat)~"="~frac(0, 7)~"≈"~0))+
  labs(x = "Sprungweite in [cm]", y = "") +
  theme(axis.title.y = element_text(size = 14, face = 2),
        axis.text.y = element_text(size = 12), 
        axis.title.x = element_text(size = 14, face = 2),
        axis.text.x = element_blank(),
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = 2),
        legend.position = "none")


rank_tbl <- tibble(id = 1:14, 
                   trt = gl(2, 7, label = c("A", "B")),
                   Orginal = c(c(1.2, 2.1, 3.5, 4.1, 6.2, 6.5, 7.1), 
                               c(4.7, 6.3, 6.8, 7.3, 8.2, 9.1, 10.3)),
                   Rangiert = rank(Orginal))


p_rank_intro <- rank_tbl |> 
  pivot_longer(cols = Orginal:Rangiert,
               values_to = "rsp",
               names_to = "type") |> 
  mutate(type = as_factor(type)) |> 
  ggplot(aes(x = type, y = rsp, fill = trt)) +
  theme_minimal() +
  geom_line(aes(group = id, color = trt), position = position_dodge(0.15),
            alpha = 0.75) +
  geom_point(shape = 21, size = 4, position = position_dodge(0.2)) +
  scale_y_continuous(limits = c(1, NA), breaks = 1:15,
                     sec.axis = sec_axis(~ ., breaks = 1:15)) +
  geom_text(aes(label = rsp), position = position_dodge(0.5), size = 2.5) +
  scale_color_okabeito() +
  scale_fill_okabeito() +
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 2),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12),  
        axis.text.y = element_blank(),
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "none")
