p1kurt <- ggplot(data.frame(x = c(-3.25, 3.25)), aes(x)) +
  theme_minimal() +
  stat_function(fun = dnorm, linewidth = 1, args = list(mean = 0, sd = 1), 
                geom = "area", xlim = c(-4, 4), fill = "#E89F00", alpha = 0.5) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 0.4)) +
  stat_function(fun = dnorm, linewidth = 1, args = list(mean = 0, sd = 1), 
                xlim = c(-4, 4), color = "#E89F00") +
  geom_hline(yintercept = 0, gray = "gray50") +
  scale_x_continuous(breaks = 0, 
                     labels = "Mittelwert = Median = Modus",
                     guide = guide_axis(n.dodge = 2)) +
  labs(x = "", y = "") +
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 2),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 16, face = 2),
        axis.text.x = element_text(size = 12),        
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "top")

p2kurt <- ggplot(data.frame(x = c(-3.25, 3.25)), aes(x)) +
  theme_minimal() +
  stat_function(fun = dbeta, linewidth = 1, args = list(shape1 = 5, shape2 = 2), 
                xlim = c(0.15, 1), fill = "#E89F00", alpha = 0.5, geom = "area") +
  geom_segment(aes(x = 0.625, xend = 0.625, y = 0, yend = 1.71),
               linetype = 11) +
  geom_segment(aes(x = 0.8, xend = 0.8, y = 0, yend = 2.46)) +
  geom_segment(aes(x = 0.45, xend = 0.45, y = 0, yend = 0.68)) +
  stat_function(fun = dbeta, linewidth = 1, args = list(shape1 = 5, shape2 = 2), 
                xlim = c(0.15, 1), color = "#E89F00") +
  geom_hline(yintercept = 0, gray = "gray50") +
  labs(x = "", y = "", title = "Linksschief",
       subtitle = "Negative Schiefe") +
  scale_x_continuous(breaks = c(0.45, 0.625, 0.8), 
                     labels = c("Mittelwert", "Median", "Modus"),
                     guide = guide_axis(n.dodge = 2)) +
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 2),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 16, face = 2),
        axis.text.x = element_text(size = 12),        
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "top")

p3kurt <- ggplot(data.frame(x = c(-3.25, 3.25)), aes(x)) +
  theme_minimal() +
  stat_function(fun = dchisq, linewidth = 1, args = list(df = 4), 
                xlim = c(0, 16), fill = "#E89F00", alpha = 0.5, geom = "area") +
  geom_segment(aes(x = 2, xend = 2, y = 0, yend = 0.183)) +
  geom_segment(aes(x = 5, xend = 5, y = 0, yend = 0.102),
               linetype = 11) +
  geom_segment(aes(x = 10, xend = 10, y = 0, yend = 0.017)) +
  stat_function(fun = dchisq, linewidth = 1, args = list(df = 4), 
                xlim = c(0, 16), color = "#E89F00") +
  geom_hline(yintercept = 0, gray = "gray50") +
  scale_x_continuous(breaks = c(2, 5, 10), 
                     labels = c("Modus", "Median", "Mittelwert"),
                     guide = guide_axis(n.dodge = 2))  +
  labs(x = "", y = "", title = "Rechtsschief",
       subtitle = "Positive Schiefe") +
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 2),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 16, face = 2),
        axis.text.x = element_text(size = 12),        
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "top")