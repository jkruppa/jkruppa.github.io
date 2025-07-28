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

group_tbl <- tibble(f = gl(2, 5, labels = c("A.1", "A.2")),
       y = c(0.9, 1.5, 1.7, 1.3, 1.2,  
             2.7, 2.5, 2.8, 2.1, 2.0)) 

group_fit <- lm(y ~f, data = group_tbl)

p14 <- 
  group_tbl |> 
ggplot(aes(as.numeric(f), y)) +
  theme_minimal() +
  geom_line(aes(y = predict(group_fit)), color = "#CC79A7", linewidth = 1) +
  geom_point(color = "gray50", alpha = 0.5, size = 4) +
  stat_summary(fun.data=mean_sdl, , fun.args = list(mult = 1), 
               geom="pointrange", shape = 23, 
               fill = c("#56B4E9", "#E69F00"), size = 0.75) +
  annotate("text", x = 0.75, y = 3, hjust = "left", color = "#CC79A7", size = 4, 
           label = "Steigung als Differenz\nder Mittelwerte") +
  geom_curve(x = 1.3, y = 2.95, xend = 1.75, yend = 2.2,
             arrow = arrow(length = unit(0.02, "npc"), type = "closed"),
             curvature = -0.4, color = "#CC79A7") +
  ylim(0.25, 3.25) + 
  xlim(0.5, 2.5) +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 12),
        plot.subtitle = element_text(size = 12, face = "italic"),
        title = element_text(size = 14, face = "bold")) +
  labs(x = "Faktor A", y = "Messwert (y)",
       title = "Kausales Modell",
       subtitle = "Was sind die Gruppenmittelwerte von Y für A?") 


p11 <- 
  ggplot(filter(pred_tbl, status == "beobachtet"), 
              aes(weight, jump_length, color = status, shape = status)) +
  labs(x = expression(x[1]), y = "y", color = "", shape = "",
       title = "Kausales Modell",
       subtitle = "Wenn X sich ändert, wie ändert sich Y?") +
  geom_point(size = 4) +
  theme_minimal() +
  scale_color_okabeito() +
  xlim(0.25, 3.25) + ylim(0.25, 3.25) +
  theme(legend.position = "none",
       axis.text.x = element_blank(),
        axis.text.y = element_blank(),
       plot.subtitle = element_text(size = 12, face = "italic"),
        axis.text = element_text(size = 12),
        title = element_text(size = 14, face = "bold")) +
  annotate("text", x = 0.5, y = 3, hjust = "left", color = "#56B4E9", size = 4, 
           label = TeX(r"($y = 0.02 + 1.04 \cdot x$)")) +
  annotate("text", x = 0.5, y = 2.8, hjust = "left", color = "#56B4E9", size = 4, 
           label = TeX(r"($r = 0.76\; (p<0.001)$)")) +
  annotate("text", x = 0.5, y = 2.6, hjust = "left", color = "#56B4E9", size = 4, 
           label = TeX(r"($R^2 = 0.58$)")) +
  geom_curve(x = 1.6, y = 3, xend = 2.5, yend = 2.3,
             arrow = arrow(length = unit(0.03, "npc")),
             curvature = -0.5, color = "#56B4E9") +
  geom_smooth(method = "lm", se = FALSE, color = "#56B4E9") +
  geom_smooth(method = "loess", se = FALSE, color = "#E69F00") +    
  annotate("text", x = 2, y = 1, hjust = "left", color = "#E69F00", size = 4, 
           label = TeX(r"($y = 1.04 \cdot x - 2.11 \cdot x^2$)")) +
  annotate("text", x = 2, y = 0.8, hjust = "left", color = "#E69F00", size = 4, 
           label = TeX(r"($r = 0.89\; (p<0.001)$)")) +
  annotate("text", x = 2, y = 0.6, hjust = "left", color ="#E69F00", size = 4, 
           label = TeX(r"($R^2 = 0.71$)"))   +
  geom_curve(aes(x = 1.9, y = 1, xend = 1.3, yend = 1.5),
             arrow = arrow(length = unit(0.02, "npc"), type = "closed"),
             curvature = -0.5, color = "#E69F00") +
  labs(x = "Einflussvariable (x)", y = "Messwert (y)")

p12 <- ggplot(pred_tbl, aes(weight, jump_length, color = status, shape = status)) +
  stat_smooth(method = "lm", se = FALSE, fullrange = TRUE, 
              color = "gray", linetype = 1) +
  labs(x = expression(x[1]), y = "y", color = "", shape = "",
       title = "Prädiktives Modell",
       subtitle = "Welche Werte von Y sagt das Modell für X vorraus?") +
  geom_point(size = 4) +
  theme_minimal() +
  scale_color_okabeito() +
  xlim(0.25, 3.25) + ylim(0.25, 3.25) +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 12),
        plot.subtitle = element_text(size = 12, face = "italic"),
        title = element_text(size = 14, face = "bold")) +
  annotate("text", x = 0.5, y = 3, hjust = "left", color = cbbPalette[3], 
           size = 4, label = "vorhergesagt", fontface = 2) +
  annotate("text", x = 2, y = 0.5, hjust = "left", color = cbbPalette[2], 
           size = 4, label = "beobachtet", fontface = 2) +
  geom_curve(x = 1.4, y = 3, xend = 2.1, yend = 2.2,
             arrow = arrow(length = unit(0.02, "npc"), type = "closed"),
             curvature = -0.5, color = cbbPalette[3]) +
  geom_curve(x = 1.9, y = 0.5, xend = 0.8, yend = 1.1,
             arrow = arrow(length = unit(0.02, "npc"), type = "closed"),
             curvature = -0.5, color = cbbPalette[2])  +
  labs(x = "Einflussvariable (x)", y = "Messwert (y)")

p13 <- ggplot(filter(pred_tbl, status == "beobachtet"),
              aes(weight, jump_length, color = grp)) +
  stat_ellipse(level = 0.82) + 
  labs(x = expression(x[1]), y = expression(x[2]), color = "Label",
       title = "Clusteranalyse") +
  geom_point(size = 4) +
  theme_minimal() +
  scale_color_okabeito() +
  xlim(0.25, 3.25) + ylim(0.25, 3.25) +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 12),
        title = element_text(size = 14, face = "bold")) +
  annotate("text", x = 1, y = 1.5, hjust = "center", color = cbbPalette[2], 
           size = 20, label = "1", alpha = 0.5) +
  annotate("text", x = 2.5, y = 2.25, hjust = "center", color = cbbPalette[3], 
           size = 20, label = "2", alpha = 0.5)
