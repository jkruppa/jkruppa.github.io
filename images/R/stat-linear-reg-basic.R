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
             arrow = arrow(length = unit(0.02, "npc"), type = "closed"),
             curvature = -0.2, color = "gray50") +
  geom_text(aes(label = c("ID: 1", 2:7)), position = position_nudge(0.05, -0.15), 
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
             arrow = arrow(length = unit(0.02, "npc"), type = "closed"),
             curvature = -0.3, alpha = 0.3,
             color = "#E69F00") +
  annotate("text", x = 2.4, y = 1.7, label = expression(1%.%beta[1]), size = 7,
           color = "#E69F00") +
  geom_curve(x = 2.25, y = 1.7, xend = 2.02, yend = 1.75,
             arrow = arrow(length = unit(0.02, "npc"), type = "closed"),
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

zero_fit <- lm(jump_length ~ 0 + I(weight^2), data = cov1_tbl) |> 
  augment()

simple_fit <- lm(jump_length ~ weight, data = cov1_tbl) |> 
  augment()

epsilon <- simple_fit$.fitted - simple_fit$jump_length

epsilon_zero <- zero_fit$.fitted - zero_fit$jump_length


p_square_01 <- ggplot(cov1_tbl, aes(weight, jump_length)) +
  theme_minimal() +
  xlim(0, 4) + ylim(0, 4) +
  geom_tile(aes(x = weight - epsilon_zero/2, 
                y = jump_length + epsilon_zero/2, 
                width = epsilon_zero, height = epsilon_zero), fill = "#D55E00",
            alpha = 0.2, color = "#D55E00", linewidth = 0.5) +
  geom_tile(aes(x = weight + epsilon/2, 
                y = jump_length + epsilon/2, 
                width = epsilon, height = epsilon), fill = "#56B4E9",
            alpha = 0.2, color = "#56B4E9", linewidth = 0.5) +
  stat_smooth(method = "lm", se = FALSE, fullrange = TRUE, color = "#56B4E9") +
  geom_function(fun = \(x) 0 + 0.3756 * x^2, color = "#D55E00", linewidth = 1) +
  geom_function(fun = \(x) 0.9686 + 0.5096 * x, color = "#56B4E9", linewidth = 1) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_point(size =  2) +
  annotate("text", x = 0.5, y = 3.75, hjust = "left", label = "f(x) = 0.97 + 0.51x",
           color = "#56B4E9", size = 5) +
  annotate("text", x = 0.5, y = 3.5, hjust = "left", label = "f(x) = 0 + 0.38x²",
           color = "#D55E00", size = 5) +
  labs(x = "Einflussvariable (x)", y = "Messwert (y)",
       title = "Methode der kleinsten Quadrate",
       subtitle = "Die Abweichungsquadrate für zwei Gradengleichungen") +
  theme(panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 12),
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "grey80", color = NA))


p_square_02 <- ggplot(cov1_tbl, aes(weight, jump_length)) +
  theme_minimal() +
  xlim(0, 4) + ylim(0, 4) +
  geom_tile(aes(x = seq(0.5, 3.5, length.out = 7),
                y = 3, 
                width = epsilon, height = epsilon), fill = "#56B4E9",
            alpha = 0.25, color = "#56B4E9", linewidth = 0.5) +
  geom_tile(aes(x = seq(0.5, 3.5, length.out = 7),
                y = 1, 
                width = epsilon_zero, height = epsilon_zero), fill = "#D55E00",
            alpha = 0.25, color = "#D55E00", linewidth = 0.5) +
  labs(x = "Einflussvariable (x)", y = "Messwert (y)",
       title = "Abweichungsquadrate",
       subtitle = "Welche Fläche der Abweichungsquadrate ist kleiner?") +
  theme(panel.grid = element_blank(),
        plot.background = element_blank(),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(face = "italic"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        strip.text = element_text(face = "bold"),
        strip.background = element_blank())

p_lm_summary_explained <- tibble(x = 0:10, y = 0:10) |> 
  ggplot(aes(x, y)) +
  xlim(c(0, 10)) + ylim(c(0, 10)) +
  theme_void() +
  geom_image(data = tibble(x = 5, y = 5),
             aes(image = "images/regression_summary_00.png"), size = 0.95) +
  ## Residuen
  geom_tile(aes(x = 5, y = 6.6, width = 5.9, height = 1.5), fill = "#56B4E9",
            alpha = 0.005, color = "#56B4E9", linewidth = 0.25) +
  annotate("label", x = 2, y = 7.4, label = "Informationen zu den Residuen", size = 2.5, 
           fontface = 2, fill = "#56B4E9", alpha = 1, hjust = "left") +
  annotate("label", x = 0.5, y = 7.4, label = "Median ≈ 0\n1st ≈ 3rd\nmin ≈ max", size = 3.5, 
           fontface = 1, fill = "#56B4E9", alpha = 0.5, hjust = "left") +  
  annotate("label", x = 0.45, y = 8.25, label = "Optimal", size = 2.5, 
           fontface = 2, fill = "#56B4E9", alpha = 1, hjust = "left") +
  geom_curve(aes(x = 1, y = 6.6, xend = 2, yend = 6.2),
             arrow = arrow(length = unit(0.01, "npc"), type = "closed"),
             curvature = 0.3, color = "#56B4E9") +
  ## Koeffizienten
  geom_tile(aes(x = 5, y = 4.8, width = 5.9, height = 1.8), fill = "#009E73",
            alpha = 0.005, color = "#009E73", linewidth = 0.25) +
  annotate("label", x = 2, y = 5.75, label = "Informationen zu den Koeffizienten", size = 2.5, 
           fontface = 2, fill = "#009E73", alpha = 1, hjust = "left") +
  annotate("label", x = 8.5, y = 5, label = expression(H[0]*":"~beta[0]*"="*0), size = 3.5, 
           fontface = 1, fill =  "#009E73", alpha = 0.5, hjust = "left") +  
  annotate("label", x = 8.5, y = 4.4, label = expression(H[0]*":"~beta[1]*"="*0), size = 3.5, 
           fontface = 1, fill =  "#009E73", alpha = 0.5, hjust = "left") +  
  annotate("label", x = 8.45, y = 5.4, label = "Hypothesen", size = 2.5, 
           fontface = 2, fill = "#009E73", alpha = 1, hjust = "left") +
  geom_curve(aes(x = 8.4, y = 5, xend = 6.65, yend = 4.6),
             arrow = arrow(length = unit(0.01, "npc"), type = "closed"),
             curvature = 0.1, color = "#009E73") +
  geom_curve(aes(x = 8.4, y = 4.3, xend = 6.65, yend = 4.1),
             arrow = arrow(length = unit(0.01, "npc"), type = "closed"),
             curvature = -0.1, color = "#009E73") +
  annotate("text", x = 1.5, y = 4.75, label = "y-Achsenabschnitt", size = 4, 
           fontface = 3, color = "gray50", hjust = "right") +
  annotate("text", x = 1.5, y = 4, label = "Steigung", size = 4, 
           fontface = 3, color = "gray50", hjust = "right") +
  geom_curve(aes(x = 1.6, y = 4.75, xend = 2.1, yend = 4.6),
             arrow = arrow(length = unit(0.01, "npc"), type = "closed"),
             curvature = -0.1, color = "gray50") +
  geom_curve(aes(x = 1.6, y = 4, xend = 2.1, yend = 4.1),
             arrow = arrow(length = unit(0.01, "npc"), type = "closed"),
             curvature = 0.1, color = "gray50") +
  ## ANOVA
  geom_tile(aes(x = 5, y = 1.7, width = 5.9, height = 0.4), fill = "#E69F00",
            alpha = 0.005, color = "#E69F00", linewidth = 0.25) +
  annotate("label", x = 2, y = 1.4, label = "Informationen zu der ANOVA", size = 2.5, 
           fontface = 2, fill = "#E69F00", alpha = 1, hjust = "left") +
  ## Modelgüte
  geom_tile(aes(x = 5, y = 2.4, width = 5.9, height = 0.9), fill = "#D55E00",
            alpha = 0.005, color = "#D55E00", linewidth = 0.25) +
  annotate("label", x = 2, y = 2.9, label = "Informationen zu der Modelgüte", size = 2.5, 
           fontface = 2, fill = "#D55E00", alpha = 1, hjust = "left") +
  annotate("label", x = 5, y = 0.75, label = "Bestimmtheitsmaß > 0.7", size = 3.5, 
           fontface = 1, fill = "#D55E00", alpha = 0.5, hjust = "left") +
  annotate("label", x = 4.95, y = 1.1, label = "Optimal", size = 2.5, 
           fontface = 2, fill = "#D55E00", alpha = 1, hjust = "left") +
  geom_curve(aes(x = 4.9, y = 0.7, xend = 4.4, yend = 2),
             arrow = arrow(length = unit(0.01, "npc"), type = "closed"),
             curvature = -0.3, color = "#D55E00") +
  annotate("label", x = 0.6, y = 2.2, label = "RSE ≈ 0", size = 3.5, 
           fontface = 1, fill = "#D55E00", alpha = 0.5, hjust = "left") +
  annotate("label", x = 0.55, y = 2.55, label = "Optimal", size = 2.5, 
           fontface = 2, fill = "#D55E00", alpha = 1, hjust = "left") +
  geom_curve(aes(x = 1.5, y = 2.2, xend = 2.1, yend = 2.4),
             arrow = arrow(length = unit(0.01, "npc"), type = "closed"),
             curvature = 0.1, color = "#D55E00") +
  annotate("text", x = 3, y = 9, label = "Modellaufruf", size = 4, 
           fontface = 3, color = "gray50", hjust = "left")  +
  geom_curve(aes(x = 2.9, y = 9, xend = 2.3, yend = 8.5),
             arrow = arrow(length = unit(0.01, "npc"), type = "closed"),
             curvature = 0.3, color = "gray50")

