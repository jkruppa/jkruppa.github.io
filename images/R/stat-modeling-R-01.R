pacman::p_load(tidyverse, readxl, knitr, kableExtra, see,
               latex2exp, patchwork)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

set.seed(20240329)

simple_tbl <- tibble(jump_length = c(1.2, 1.5, 1.8, 1.3, 1.7, 2.6, 1.8, 2.7, 2.6),
                     weight = c(0.8, 0.9, 1, 1.2, 1.9, 2, 2.7, 2.8, 2.9),
                     grp = as_factor(c("Gruppe 1", "Gruppe 1", "Gruppe 1", "Gruppe 1", "Gruppe 2", "Gruppe 2", "Gruppe 2", "Gruppe 2", "Gruppe 2")))

fit_1 <- lm(jump_length ~ weight, data = simple_tbl)

pred_tbl <- bind_rows(mutate(simple_tbl, status = "beobachtet"),
                      tibble(weight = c(1.7, 1.4, 2.1, 3.0),
                             jump_length = predict(fit_1, 
                                                   newdata = tibble(weight)),
                             status = "vorhergesagt"))

p11 <- ggplot(filter(pred_tbl, status == "beobachtet"), 
              aes(weight, jump_length, color = status, shape = status)) +
  stat_smooth(method = "lm", se = FALSE, fullrange = TRUE, 
              color = "gray") +
  labs(x = expression(x[1]), y = "y", color = "", shape = "",
       title = "Kausales Modell") +
  geom_point(size = 4) +
  theme_minimal() +
  scale_color_okabeito() +
  xlim(0, 3.5) + ylim(0, 3.5) +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 12),
        title = element_text(size = 14, face = "bold")) +
  annotate("text", x = 0.5, y = 3, hjust = "left", color = "black", size = 4, 
           label = TeX(r"($y = 0.02 + 1.04 \cdot x$)")) +
  annotate("text", x = 0.5, y = 2.8, hjust = "left", color = "black", size = 4, 
           label = TeX(r"($r = 0.76\; (p<0.001)$)")) +
  annotate("text", x = 0.5, y = 2.6, hjust = "left", color = "black", size = 4, 
           label = TeX(r"($R^2 = 0.58$)")) +
  geom_curve(x = 1.6, y = 3, xend = 2.5, yend = 2.3,
             arrow = arrow(length = unit(0.03, "npc")),
             curvature = -0.5, color = "black") 

p12 <- ggplot(pred_tbl, aes(weight, jump_length, color = status, shape = status)) +
  stat_smooth(method = "lm", se = FALSE, fullrange = TRUE, 
              color = "gray", linetype = 2) +
  labs(x = expression(x[1]), y = "y", color = "", shape = "",
       title = "Prädiktives Modell") +
  geom_point(size = 4) +
  theme_minimal() +
  scale_color_okabeito() +
  xlim(0, 3.5) + ylim(0, 3.5) +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 12),
        title = element_text(size = 14, face = "bold")) +
  annotate("text", x = 0.5, y = 3, hjust = "left", color = cbbPalette[3], 
           size = 4, label = "vorhergesagt", fontface = 2) +
  annotate("text", x = 2, y = 0.5, hjust = "left", color = cbbPalette[2], 
           size = 4, label = "beobachtet", fontface = 2) +
  geom_curve(x = 1.4, y = 3, xend = 2.1, yend = 2.2,
             arrow = arrow(length = unit(0.03, "npc")),
             curvature = -0.5, color = cbbPalette[3]) +
  geom_curve(x = 1.9, y = 0.5, xend = 0.8, yend = 1.1,
             arrow = arrow(length = unit(0.03, "npc")),
             curvature = -0.5, color = cbbPalette[2])  

p13 <- ggplot(filter(pred_tbl, status == "beobachtet"),
              aes(weight, jump_length, color = grp)) +
  stat_ellipse(level = 0.82) + 
  labs(x = expression(x[1]), y = expression(x[2]), color = "Label",
       title = "Clusteranalyse") +
  geom_point(size = 4) +
  theme_minimal() +
  scale_color_okabeito() +
  xlim(0, 3.5) + ylim(0, 3.5) +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 12),
        title = element_text(size = 14, face = "bold")) +
  annotate("text", x = 1, y = 1.5, hjust = "center", color = cbbPalette[2], 
           size = 20, label = "1", alpha = 0.5) +
  annotate("text", x = 2.5, y = 2.25, hjust = "center", color = cbbPalette[3], 
           size = 20, label = "2", alpha = 0.5)
