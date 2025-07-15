simple_fit <- lm(jump_length ~ weight, data = cov1_tbl) |> 
  augment()

p1_theo_00 <- ggplot(cov1_tbl, aes(weight, jump_length)) +
  theme_minimal() +
  xlim(0, 3.5) + ylim(0, 3.5) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  stat_smooth(method = "lm", se = FALSE, fullrange = TRUE, color = "#56B4E9") +
  geom_point(size =  2) +
  annotate("text", x = 2, y = 1, label = "Gerade durch die Punkte", size =5, 
           fontface = 3, color = "gray50", hjust = "left") +
  annotate("label", x = 0.25, y = 3, label = "Wie lautet die Geradengleichung?\nLiegen die Punkte auf der Geraden?", size = 4, 
           fontface = 3, color = "black", hjust = "left", fill = "#56B4E9",
           alpha = 0.5) +
  geom_curve(aes(x = 1.975, y = 1, xend = 1.5, yend = 1.65),
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = -0.2, color = "gray50") +
  geom_text(aes(label = 1:7), position = position_nudge(0.05, -0.1), 
            color = "gray25") +
  labs(x = "Einflussvariable (x)", y = "Messwert (y)") +
  theme(panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 12),
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "grey80", color = NA))

p1_theo_01 <- ggplot(cov1_tbl, aes(weight, jump_length)) +
  theme_minimal() +
  xlim(0, 3.5) + ylim(0, 3.5) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_segment(x = simple_fit$weight, y = simple_fit$jump_length, 
               xend = simple_fit$weight, yend = simple_fit$.fitted, 
               color = "#009E73",
               linewidth = 0.5, linetype = 1) +
  geom_segment(x = 1.035, y = 1.5, 
               xend = 2, yend = 1.5, 
               color = "#D55E00",
               linewidth = 0.5, linetype = 1) +
  geom_segment(x = 2, y = 1.5, 
               xend = 2, yend = 2, 
               color = "#D55E00",
               linewidth = 0.5, linetype = 1) +
  annotate("text", x = 0.3, y = 0.5, label = expression(beta[0]), size = 7,
           color = "#E69F00") +
  geom_curve(x = 0.2, y = 0.5, xend = 0, yend = 0.95,
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = -0.3, alpha = 0.3,
             color = "#E69F00") +
  annotate("text", x = 2.4, y = 1.7, label = expression(1%.%beta[1]), size = 7,
           color = "#E69F00") +
  geom_curve(x = 2.25, y = 1.7, xend = 2.02, yend = 1.75,
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = -0.3, alpha = 0.3,
             color = "#E69F00") +
  annotate("text", x = 1, y = 1.35, label = expression(x), size = 7, 
           color = "#D55E00") +
  annotate("text", x = 2, y = 1.35, label = expression(x+1), size = 7, 
           color = "#D55E00") +
  annotate("text", x = simple_fit$weight + 0.06, 
           y = simple_fit$.fitted + simple_fit$.resid/2,
           label = c(expression(epsilon[1]), expression(epsilon[2]), expression(epsilon[3]),
                     expression(epsilon[4]), expression(epsilon[5]), expression(epsilon[6]),
                     expression(epsilon[7])),
           color = "#009E73") +
  stat_smooth(method = "lm", se = FALSE, fullrange = TRUE, color = "#56B4E9") +
  geom_point(size =  2) +
  labs(x = "Einflussvariable (x)", y = "Messwert (y)") +
  theme(panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 12),
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "grey80", color = NA))