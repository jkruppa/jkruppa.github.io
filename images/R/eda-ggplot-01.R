simple_tbl <- tibble(jump_length = c(1.2, 1.8, 1.3, 1.7, 2.6, 1.8, 2.7),
                     weight = c(0.8, 1, 1.2, 1.9, 2, 2.7, 2.8))

p1 <- ggplot(simple_tbl, aes(weight, jump_length)) +
  theme_minimal() +
  xlim(0.5, 3) + ylim(0, 3.5) +
  geom_vline(xintercept = 0.5) +
  geom_hline(yintercept = 0) +
  labs(x = "Gewicht der Hundeflöhe in [mg]", y = "Sprungeweite der Hundeflöhe in [cm]") +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 12, face = 2),
        axis.title.y = element_text(size = 12, face = 2),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        plot.title = element_text(size = 16, face = 2),
        plot.subtitle = element_text(size = 14, face = 3)) +
  stat_smooth(method = "lm", se = FALSE, fullrange = TRUE,
              linewidth = 0.75, color = "#56B4E9") +
  annotate("text", hjust = "left", x = 1, y = 3, color = "#0072B2",
           label = "Lineare Regression", size = 6) +
  geom_curve(x = 2.02, y = 2.975, xend = 2.65, yend = 2.35, linewidth = 0.5,
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = -0.3, alpha = 0.3, color = "#0072B2") +
  stat_smooth(aes(weight, jump_length), method = "lm",
              formula = y ~ poly(x, 5), se = FALSE, geom = "line", 
              color = "#E69F00", fullrange = TRUE, linewidth = 0.75) +
  annotate("text", hjust = "left", x = 1.75, y = 0.25, color = "#D55E00",
           label = "Nicht lineare Regression", size = 6) +
  geom_curve(x = 1.73, y = 0.25, xend = 1.5, yend = 0.56, linewidth = 0.5,
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = -0.3, alpha = 0.3, color = "#D55E00") +
  geom_point(size = 3, color = "gray50") +
  ggtitle("Zusammenhang", subtitle = "Hängt die Sprungweite vom Gewicht ab?") +
  ylim(-0.2, 3)

stat_tbl <- tibble(x = rep(c(2, 7, 12), each = 5), 
                   y = c(2.8, 3.3, 4.5, 2.3, 3.0,
                         6.1, 5.9, 5.6, 4.8, 4.6,
                         1.2, 4.2, 2.5, 3.2, 1.8)) |> 
  group_by(x) |> 
  summarise(mean(y), sd(y), median(y), 
            st = quantile(y, probs = 0.25),
            rd = quantile(y, probs = 0.75))

p2 <- tibble(x = rep(c(2, 7, 12), each = 5), 
             y = c(2.8, 3.3, 4.5, 2.3, 3.0,
                   6.1, 5.9, 5.6, 4.8, 4.6,
                   1.2, 4.2, 2.5, 3.2, 1.8)) |> 
  ggplot(aes(x, y)) +
  geom_point(size = 3, color = "gray50") +
  theme_minimal() +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  ## Barplot
  annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 3.18, fill = "#E69F00",
           color = "black",  linewidth = 0.5, alpha = 0.5) +
  geom_segment(x = 1, y = 3.18, xend = 1, yend = 4, color = "black",
               linewidth = 0.75, linetype = 1) +
  annotate("rect", xmin = 5.5, xmax = 6.5, ymin = 0, ymax = 5.4, fill = "#009E73",
           color = "black",  linewidth = 0.5, alpha = 0.5) +
  geom_segment(x = 6, y = 5.4, xend = 6, yend = 6.06, color = "black",
               linewidth = 0.75, linetype = 1) +
  annotate("rect", xmin = 10.5, xmax = 11.5, ymin = 0, ymax = 2.58, fill = "#0072B2",
           color = "black",  linewidth = 0.5, alpha = 0.5) +
  geom_segment(x = 11, y = 2.58, xend = 11, yend = 3.76, color = "black",
               linewidth = 0.75, linetype = 1) +
  ## Boxplot
  annotate("rect", xmin = 2.5, xmax = 3.5, ymin = 2.8, ymax = 3.3, fill = "#E69F00",
           color = "black",  linewidth = 0.5, alpha = 0.5) +
  geom_segment(x = 2.5, y = 3.18, xend = 3.5, yend = 3.18, color = "black",
               linewidth = 0.75, linetype = 1) +
  geom_segment(x = 3, y = 2.8, xend = 3, yend = 2.3, color = "black",
               linewidth = 0.75, linetype = 1) +
  geom_segment(x = 3, y = 3.3, xend = 3, yend = 4.5, color = "black",
               linewidth = 0.75, linetype = 1) +
  annotate("rect", xmin = 7.5, xmax = 8.5, ymin = 4.8, ymax = 5.9, fill = "#009E73",
           color = "black",  linewidth = 0.5, alpha = 0.5) +
  geom_segment(x = 7.5, y = 5.6, xend = 8.5, yend = 5.6, color = "black",
               linewidth = 0.75, linetype = 1) +
  geom_segment(x = 8, y = 5.9, xend = 8, yend = 6.1, color = "black",
               linewidth = 0.75, linetype = 1) +
  geom_segment(x = 8, y = 4.8, xend = 8, yend = 4.6, color = "black",
               linewidth = 0.75, linetype = 1)  +
  annotate("rect", xmin = 12.5, xmax = 13.5, ymin = 1.8, ymax = 3.2, fill = "#0072B2",
           color = "black",  linewidth = 0.5, alpha = 0.5) +
  geom_segment(x = 12.5, y = 2.5, xend = 13.5, yend = 2.5, color = "black",
               linewidth = 0.75, linetype = 1) +
  geom_segment(x = 13, y = 1.8, xend = 13, yend = 1.2, color = "black",
               linewidth = 0.75, linetype = 1) +
  geom_segment(x = 13, y = 3.2, xend = 13, yend = 4.2, color = "black",
               linewidth = 0.75, linetype = 1) +
  ## axis
  geom_segment(x = 2, y = 0, xend = 2, yend = -0.15, color = "black",
               linewidth = 0.5, linetype = 1) +
  geom_segment(x = 7, y = 0, xend = 7, yend = -0.15, color = "black",
               linewidth = 0.5, linetype = 1) +
  geom_segment(x = 12, y = 0, xend = 12, yend = -0.15, color = "black",
               linewidth = 0.5, linetype = 1) +
  annotate("text", x = c(2, 7, 12), y = -0.35, label = c("Hund", "Katze", "Fuchs"),
           size = 4, face = 2) +
  labs(x = "Tierart", 
       y = "Sprungeweite der Flöhe in [cm]",
       title = "Unterschied",
       subtitle = "Springen Flöhe verschiedener Tierarten gleich weit?") +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 12, face = 2),
        axis.title.y = element_text(size = 12, face = 2),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        plot.title = element_text(size = 16, face = 2),
        plot.subtitle = element_text(size = 14, face = 3)) +
  annotate("text", hjust = "left", x = 1.75, y = 6, color = "#D55E00",
           label = "Barplot", size = 6) +
  geom_curve(x = 1.6, y = 5.95, xend = 0.7, yend = 3.25, linewidth = 0.5,
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = 0.4, alpha = 0.3, color = "#D55E00") +
  annotate("text", hjust = "right", x = 11.75, y = 5.5, color = "#56B4E9",
           label = "Boxplot", size = 6) +
  geom_curve(x = 11.8, y = 5.45, xend = 13.25, yend = 3.25, linewidth = 0.5,
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = -0.4, alpha = 0.3, color = "#56B4E9") +
  ylim(-0.4, NA)


