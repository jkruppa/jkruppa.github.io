p_lm_summary_cov2_explained <- tibble(x = 0:10, y = 0:10) |> 
  ggplot(aes(x, y)) +
  xlim(c(0, 10)) + ylim(c(0, 10)) +
  theme_void() +
  geom_image(data = tibble(x = 5, y = 5),
             aes(image = "images/regression_summary_mult_01.png"), size = 0.95) +
  ## Residuen
  geom_tile(aes(x = 5, y = 6.7, width = 5.9, height = 1.5), fill = "#56B4E9",
            alpha = 0.005, color = "#56B4E9", linewidth = 0.25) +
  annotate("label", x = 2, y = 7.45, label = "Informationen zu den Residuen", size = 2.5, 
           fontface = 2, fill = "#56B4E9", alpha = 1, hjust = "left") +
  annotate("label", x = 0.5, y = 7.4, label = "Median ≈ 0\n1st ≈ 3rd\nmin ≈ max", size = 3.5, 
           fontface = 1, fill = "#56B4E9", alpha = 0.5, hjust = "left") +  
  annotate("label", x = 0.45, y = 8.25, label = "Optimal", size = 2.5, 
           fontface = 2, fill = "#56B4E9", alpha = 1, hjust = "left") +
  geom_curve(aes(x = 1, y = 6.6, xend = 2, yend = 6.6),
             arrow = arrow(length = unit(0.01, "npc"), type = "closed"),
             curvature = 0.3, color = "#56B4E9") +
  ## Koeffizienten
  geom_tile(aes(x = 5, y = 4.8, width = 5.9, height = 2), fill = "#009E73",
            alpha = 0.005, color = "#009E73", linewidth = 0.25) +
  annotate("label", x = 2, y = 5.85, label = "Informationen zu den Koeffizienten", size = 2.5, 
           fontface = 2, fill = "#009E73", alpha = 1, hjust = "left") +
  annotate("label", x = 8.5, y = 5, label = expression(H[0]*":"~beta[0]*"="*0), size = 3.5, 
           fontface = 1, fill =  "#009E73", alpha = 0.5, hjust = "left") +  
  annotate("label", x = 8.5, y = 4.4, label = expression(H[0]*":"~beta[weight]*"="*0), size = 3.5, 
           fontface = 1, fill =  "#009E73", alpha = 0.5, hjust = "left") +  
  annotate("label", x = 8.5, y = 3.8, label = expression(H[0]*":"~beta[count_leg]*"="*0), size = 3.5, 
           fontface = 1, fill =  "#009E73", alpha = 0.5, hjust = "left") + 
  annotate("label", x = 8.45, y = 5.4, label = "Hypothesen", size = 2.5, 
           fontface = 2, fill = "#009E73", alpha = 1, hjust = "left") +
  geom_curve(aes(x = 8.4, y = 5, xend = 6.3, yend = 4.9),
             arrow = arrow(length = unit(0.01, "npc"), type = "closed"),
             curvature = 0.2, color = "#009E73") +
  geom_curve(aes(x = 8.4, y = 4.3, xend = 6.3, yend = 4.4),
             arrow = arrow(length = unit(0.01, "npc"), type = "closed"),
             curvature = -0.1, color = "#009E73") +
  geom_curve(aes(x = 8.4, y = 3.8, xend = 6.3, yend = 3.95),
             arrow = arrow(length = unit(0.01, "npc"), type = "closed"),
             curvature = -0.3, color = "#009E73") +
  annotate("text", x = 1.5, y = 5, label = "y-Achsenabschnitt", size = 4, 
           fontface = 3, color = "gray50", hjust = "right") +
  annotate("text", x = 1.5, y = 4.25, label = expression(Steigung~beta[weight]), 
           size = 4, fontface = 3, color = "gray50", hjust = "right") +
  annotate("text", x = 1.5, y = 3.5, label = expression(Steigung~beta[count_leg]), 
           size = 4, fontface = 3, color = "gray50", hjust = "right") +
  geom_curve(aes(x = 1.6, y = 5, xend = 3.6, yend = 5),
             arrow = arrow(length = unit(0.01, "npc"), type = "closed"),
             curvature = -0.1, color = "gray50") +
  geom_curve(aes(x = 1.6, y = 4.25, xend = 3.5, yend = 4.3),
             arrow = arrow(length = unit(0.01, "npc"), type = "closed"),
             curvature = 0.1, color = "gray50") +
  geom_curve(aes(x = 1.6, y = 3.5, xend = 3.6, yend = 3.9),
             arrow = arrow(length = unit(0.01, "npc"), type = "closed"),
             curvature = 0.1, color = "gray50") +
  ## ANOVA
  geom_tile(aes(x = 5, y = 1.8, width = 5.9, height = 0.4), fill = "#E69F00",
            alpha = 0.005, color = "#E69F00", linewidth = 0.25) +
  annotate("label", x = 2, y = 1.5, label = "Informationen zu der ANOVA", size = 2.5, 
           fontface = 2, fill = "#E69F00", alpha = 1, hjust = "left") +
  ## Modelgüte
  geom_tile(aes(x = 5, y = 2.4, width = 5.9, height = 0.75), fill = "#D55E00",
            alpha = 0.005, color = "#D55E00", linewidth = 0.25) +
  annotate("label", x = 2, y = 2.85, label = "Informationen zu der Modelgüte", size = 2.5, 
           fontface = 2, fill = "#D55E00", alpha = 1, hjust = "left") +
  annotate("label", x = 5, y = 0.6, label = "Bestimmtheitsmaß > 0.7", size = 3.5, 
           fontface = 1, fill = "#D55E00", alpha = 0.5, hjust = "left") +
  annotate("label", x = 4.95, y = 0.95, label = "Optimal", size = 2.5, 
           fontface = 2, fill = "#D55E00", alpha = 1, hjust = "left") +
  geom_curve(aes(x = 4.95, y = 0.6, xend = 4.2, yend = 2),
             arrow = arrow(length = unit(0.01, "npc"), type = "closed"),
             curvature = -0.3, color = "#D55E00") +
  annotate("label", x = 0.6, y = 2.2, label = "RSE ≈ 0", size = 3.5, 
           fontface = 1, fill = "#D55E00", alpha = 0.5, hjust = "left") +
  annotate("label", x = 0.55, y = 2.55, label = "Optimal", size = 2.5, 
           fontface = 2, fill = "#D55E00", alpha = 1, hjust = "left") +
  geom_curve(aes(x = 1.5, y = 2.2, xend = 2.1, yend = 2.5),
             arrow = arrow(length = unit(0.01, "npc"), type = "closed"),
             curvature = 0.1, color = "#D55E00") +
  annotate("text", x = 3, y = 9, label = "Modellaufruf - Zweikovariat", size = 4, 
           fontface = 3, color = "gray50", hjust = "left")  +
  geom_curve(aes(x = 2.9, y = 9, xend = 2.4, yend = 8.5),
             arrow = arrow(length = unit(0.01, "npc"), type = "closed"),
             curvature = 0.3, color = "gray50")