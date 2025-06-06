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




p_hypo_null <- tibble(X = c(1:4, 1:4),
       Y = c(rep(1, 4), rep(2, 4))) |> 
  ggplot(aes(x = X, y = Y, fill = as_factor(Y))) +
  theme_minimal() +
  annotate("segment", 
           x = c(rep(1, 4), rep(2, 4), rep(3, 4), rep(4, 4)), 
           xend = rep(1:4, times = 4), 
           y = rep(2, 16), 
           yend = rep(1, 16), color = "gray50", linewidth = 0.25) +
  annotate("segment", 
           x = 1:4, 
           xend = 1:4, 
           y = rep(2, 4), 
           yend = rep(1, 4), color = "#009E73", linewidth = 1) +
  annotate("segment", 
           x = c(2, 3, 4, 3, 4, 4),  
           xend = c(rep(1, 3), rep(2, 2), rep(3, 1)),
           y = rep(1, 6), 
           yend = rep(2, 6), color = "#CC79A7", linewidth = 1) +
  geom_point(shape = 21, size = 5, show.legend = FALSE, fill = "gray75") +
  scale_x_continuous(limits = c(1, 7), breaks = 1:4) +
  scale_y_continuous(breaks = c(1,2), labels = c("Y", "X")) +
  annotate("label", x = c(1.4, 1.8, 2.2, 2.425, 2.8, 3.4),
           y = 1.6, label = 1:6, fill = "#CC79A7", size = 3) +
  annotate("label", x = 4.5, y = 1.75, hjust = "left",
           label = expression(Pr~(X<Y)~"="~frac(6, 16)~"="~0.375), 
           color = "#CC79A7", size = 5) +
  annotate("label", x = c(1:4),
           y = 1.5, label = 1:4, fill = "#009E73", size = 3) +
  annotate("label", x = 4.5, y = 1.5, hjust = "left",
           label = expression(frac(1, 2)~Pr~(X~"="~Y)~"="~frac(1, 2)%.%frac(4, 16)~"="~0.125), 
           color = "#009E73", size = 5) +
  labs(x = "", y = "") +
  annotate("text", x = 4.5, y = 1.3, hjust = "left",
           label = "Nullhypothese gilt da:", 
           color = "black", size = 5, fontface = 2) +
  annotate("label", x = 4.5, y = 1.18, hjust = "left",
           label = expression(0.375+0.125~"="~0.5), 
           color = "black", size = 5) +
  theme(axis.title.y = element_text(size = 14, face = 2),
        axis.text.y = element_text(size = 14, face = 2), 
        axis.title.x = element_text(size = 14, face = 2),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = 2),
        legend.position = "none")


rank_tbl <- tibble(id = 1:14, 
                   trt = gl(2, 7, label = c("A", "B")),
                   Original = c(c(1.2, 2.1, 3.5, 4.1, 6.2, 6.5, 7.1), 
                               c(4.7, 6.3, 6.8, 7.3, 8.2, 9.1, 10.3)),
                   Rangiert = rank(Original))


p_rank_intro <- rank_tbl |> 
  pivot_longer(cols = Original:Rangiert,
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


p_lognormal_t_u <- ggplot(data.frame(x = c(0, 1250)), aes(x)) +
  theme_minimal() +
  stat_function(fun = dlnorm, linewidth = 1, args = list(meanlog = 5, sd = 1), 
                color = "#E89F00") +
  stat_function(fun = dlnorm, linewidth = 1, args = list(meanlog = 5, sd = 1), 
                geom = "area", fill = "#E89F00", alpha = 0.25) +
  geom_vline(xintercept = c(0)) + 
  geom_hline(yintercept = c(0)) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 14, face = 2),
        axis.text.x = element_text(size = 12),        
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12),
        panel.grid = element_blank(),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 2),
        legend.position = "none") +
  labs(x = "Messwerte", y = "",
       title = "Log-normal Verteilung",
       subtitle = "Datengrundlage für den Vergleich t-Test und U-Test")



p_p_t_u_1 <- p_t_u_tbl |> 
  ggplot(aes(x = wilcox, y = ttest, color = n)) +
  theme_minimal() +
  geom_point2(alpha = 0.2, show.legend = FALSE) +
  annotate("segment", x = 0, xend = 1, y = 0, yend = 1,
           color = "gray70", size = 1) +
  stat_summary(fun = "mean", geom = "line") +
  coord_cartesian(xlim=c(0, 0.5), ylim=c(0, 0.5)) +
  scale_color_okabeito() +
  labs(x = "U-Test", y = "Welch t-Test",
       title = "Vergleich der p-Werte", 
       subtitle = "Rangtransformiert vs. orginal Daten",
       caption = expression(n[sim]~"="~10000),
       color = expression(n[Gruppe])) +
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
        legend.position = c(0.9, 0.35),
        legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid", 
                                         colour ="gray50")) 

p_p_t_u_2 <- p_t_u_tbl |> 
  #filter(wilcox != 1) |> 
  ggplot(aes(x = wilcox, y = dev, color = n)) +
  theme_minimal() +
  stat_summary(fun = "mean", geom = "line") +
  scale_y_continuous(limits = c(-0.1, 0)) +
  scale_x_continuous(#limits = c(0, 0.52), 
    breaks = c(0.05, 0.25, 0.5, 0.75, 1)) +
  scale_color_okabeito() +
  annotate("segment", x = 0, xend = 1, y = 0, yend = 0,
           color = "gray70", size = 1) +
  annotate("segment", x = 0.05, xend = 0.05, y = 0, yend = -0.1,
           color = "gray70", size = 1, linetype = 2) +
  labs(x = "U-Test", y = "Welch t-Test",
       title = "Abweichung der p-Werte", 
       subtitle = "Wie stark weichen die p-Werte des t-Test ab?",
       caption = expression(n[sim]~"="~10000),
       color = expression(n[Gruppe])) +
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
        legend.position = "none") 