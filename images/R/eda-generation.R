p_rr_or <- tibble(x = c(0,  1, 2, 3), y = 1) |> 
  ggplot(aes(x, y)) +
  theme_minimal() +
  geom_rect(aes(xmin = 0.8, xmax = 1.2, ymin = 0, ymax = 0.6), fill = "#E69F00",
            color = "black",  linewidth = 0.75) +
  geom_rect(aes(xmin = 0.8, xmax = 1.2, ymin = 0.6, ymax = 1), fill = "#56B4E9",
            color = "black",  linewidth = 0.75) +
  geom_vline(xintercept = 0.5,  linewidth = 1) +
  geom_hline(yintercept = 0,  linewidth = 1) +
  ## Ratio
  annotate("text", x = 2.5, y = 0.8, label = "Ratio =", hjust = "right", size = 6, fontface = 2) +
  geom_segment(x = 2.55, y = 0.8, xend = 3.5, yend = 0.8, color = "black",
               linewidth = 0.75, linetype = 1) +
  geom_rect(aes(xmin = 3.05-0.1, xmax = 3.05+0.1, ymin = 0.82, ymax = 0.98), fill = "#56B4E9",
            color = "black",  linewidth = 0.75) +
  geom_rect(aes(xmin = 3.05-0.1-0.25, xmax = 3.05+0.1-0.25, ymin = 0.62, ymax = 0.78), fill = "#56B4E9",
            color = "black",  linewidth = 0.75) +
  geom_rect(aes(xmin = 3.05-0.1+0.25, xmax = 3.05+0.1+0.25, ymin = 0.62, ymax = 0.78), fill = "#E69F00",
            color = "black",  linewidth = 0.75) +
  annotate("text", x = 3.05, y = 0.7, label = "+", hjust = "center", size = 10, fontface = 2) +
  ## Odds
  annotate("text", x = 2.5, y = 0.8-0.55, label = "Odd =", hjust = "right", size = 6, fontface = 2) +
  geom_segment(x = 2.55, y = 0.8-0.55, xend = 3.5, yend = 0.8-0.55, color = "black",
               linewidth = 0.75, linetype = 1) +
  geom_rect(aes(xmin = 3.05-0.1, xmax = 3.05+0.1, ymin = 0.82-0.55, ymax = 0.98-0.55), fill = "#56B4E9",
            color = "black",  linewidth = 0.75) +
  geom_rect(aes(xmin = 3.05-0.1, xmax = 3.05+0.1, ymin = 0.62-0.55, ymax = 0.78-0.55), fill = "#E69F00",
            color = "black",  linewidth = 0.75) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = scales::percent) +
  scale_x_continuous(limits = c(0.5, 3.75), breaks = c(1), label = c("A.1")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(), 
        legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"),
        title = element_text(size = 14, face = "bold")) +
  labs(x = "Faktor A", y = "Messwert (y)",
       title = "Anteilsdifferenz",
       subtitle = "Vergleich Verhältnis (Ratio) und Chance (Odd) innerhalb einer Gruppe") 

p_rr <- tibble(x = c(0,  1, 2, 3), y = 1) |> 
  ggplot(aes(x, y)) +
  theme_minimal() +
  geom_rect(aes(xmin = 0.8, xmax = 1.2, ymin = 0, ymax = 0.6), fill = "#E69F00",
            color = "black",  linewidth = 0.75) +
  geom_rect(aes(xmin = 0.8, xmax = 1.2, ymin = 0.6, ymax = 1), fill = "#56B4E9",
            color = "black",  linewidth = 0.75) +
  geom_rect(aes(xmin = 0.8+0.5, xmax = 1.2+0.5, ymin = 0, ymax =0.2), fill = "#009E73",
            color = "black",  linewidth = 0.75) +
  geom_rect(aes(xmin = 0.8+0.5, xmax = 1.2+0.5, ymin = 0.2, ymax =1), fill = "#CC79A7",
            color = "black",  linewidth = 0.75) +
  geom_vline(xintercept = 0.5,  linewidth = 1) +
  geom_hline(yintercept = 0,  linewidth = 1) +
  ## oben
  annotate("text", x = 2.5, y = 0.525, label = "RR =", hjust = "right", size = 6, fontface = 2) +
  geom_segment(x = 2.55, y = 0.525, xend = 3.5, yend = 0.525, color = "black",
               linewidth = 1, linetype = 1) +
  ## oben
  geom_segment(x = 2.55+0.1, y = 0.8, xend = 3.5-0.05, yend = 0.8, color = "black",
               linewidth = 0.75, linetype = 1) +
  geom_rect(aes(xmin = 3.05-0.1, xmax = 3.05+0.1, ymin = 0.82, ymax = 0.98), fill = "#56B4E9",
            color = "black",  linewidth = 0.75) +
  geom_rect(aes(xmin = 3.05-0.1-0.25, xmax = 3.05+0.1-0.25, ymin = 0.62, ymax = 0.78), fill = "#56B4E9",
            color = "black",  linewidth = 0.75) +
  geom_rect(aes(xmin = 3.05-0.1+0.25, xmax = 3.05+0.1+0.25, ymin = 0.62, ymax = 0.78), fill = "#E69F00",
            color = "black",  linewidth = 0.75) +
  annotate("text", x = 3.05, y = 0.7, label = "+", hjust = "center", size = 10, fontface = 2) +
  ## unten
  geom_segment(x = 2.55+0.1, y = 0.8-0.55, xend = 3.5-0.05, yend = 0.8-0.55, color = "black",
               linewidth = 0.75, linetype = 1) +
  geom_rect(aes(xmin = 3.05-0.1, xmax = 3.05+0.1, ymin = 0.82-0.55, ymax = 0.98-0.55), fill = "#CC79A7",
            color = "black",  linewidth = 0.75) +
  geom_rect(aes(xmin = 3.05-0.1-0.25, xmax = 3.05+0.1-0.25, ymin = 0.62-0.55, ymax = 0.78-0.55), fill = "#CC79A7",
            color = "black",  linewidth = 0.75) +
  geom_rect(aes(xmin = 3.05-0.1+0.25, xmax = 3.05+0.1+0.25, ymin = 0.62-0.55, ymax = 0.78-0.55), fill = "#009E73",
            color = "black",  linewidth = 0.75) +
  annotate("text", x = 3.05, y = 0.7-0.55, label = "+", hjust = "center", size = 10, fontface = 2) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = scales::percent) +
  scale_x_continuous(limits = c(0.5, 3.75), breaks = c(1, 1.5), label = c("A.1", "A.2")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(), 
        legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"),
        title = element_text(size = 14, face = "bold")) +
  labs(x = "Faktor A", y = "Messwert (y)",
       title = "Anteilsdifferenz",
       subtitle = "Risikoverhältnis (Risk Ratio, abk. RR) zwischen zwei Gruppen") 

p_or <- tibble(x = c(0,  1, 2, 3), y = 1) |> 
  ggplot(aes(x, y)) +
  theme_minimal() +
  geom_rect(aes(xmin = 0.8, xmax = 1.2, ymin = 0, ymax = 0.6), fill = "#E69F00",
            color = "black",  linewidth = 0.75) +
  geom_rect(aes(xmin = 0.8, xmax = 1.2, ymin = 0.6, ymax = 1), fill = "#56B4E9",
            color = "black",  linewidth = 0.75) +
  geom_rect(aes(xmin = 0.8+0.5, xmax = 1.2+0.5, ymin = 0, ymax =0.2), fill = "#009E73",
            color = "black",  linewidth = 0.75) +
  geom_rect(aes(xmin = 0.8+0.5, xmax = 1.2+0.5, ymin = 0.2, ymax =1), fill = "#CC79A7",
            color = "black",  linewidth = 0.75) +
  geom_vline(xintercept = 0.5,  linewidth = 1) +
  geom_hline(yintercept = 0,  linewidth = 1) +
  ## oben
  annotate("text", x = 2.5-0.2, y = 0.525, label = "OR =", hjust = "right", size = 6, fontface = 2) +
  geom_segment(x = 2.55-0.2, y = 0.525, xend = 3.5-0.6, yend = 0.525, color = "black",
               linewidth = 1, linetype = 1) +
  ## oben
  geom_segment(x = 2.55+0.1-0.2, y = 0.8, xend = 3.5-0.05-0.6, yend = 0.8, color = "black",
               linewidth = 0.75, linetype = 1) +
  geom_rect(aes(xmin = 3.05-0.1-0.4, xmax = 3.05+0.1-0.4, ymin = 0.82, ymax = 0.98), fill = "#56B4E9",
            color = "black",  linewidth = 0.75) +
  geom_rect(aes(xmin = 3.05-0.1-0.4, xmax = 3.05+0.1-0.4, ymin = 0.62, ymax = 0.78), fill = "#E69F00",
            color = "black",  linewidth = 0.75) +
  ## unten
  geom_segment(x = 2.55+0.1-0.2, y = 0.8-0.55, xend = 3.5-0.05-0.6, yend = 0.8-0.55, color = "black",
               linewidth = 0.75, linetype = 1) +
  geom_rect(aes(xmin = 3.05-0.1-0.4, xmax = 3.05+0.1-0.4, ymin = 0.82-0.55, ymax = 0.98-0.55), fill = "#CC79A7",
            color = "black",  linewidth = 0.75) +
  geom_rect(aes(xmin = 3.05-0.1-0.4, xmax = 3.05+0.1-0.4, ymin = 0.62-0.55, ymax = 0.78-0.55), fill = "#009E73",
            color = "black",  linewidth = 0.75) +
  ## gleich rechts
  annotate("text", x = 3, y = 0.525, label = "=", hjust = "center", size = 6, fontface = 2) + 
  geom_segment(x = 3.1, y = 0.525, xend = 3.8, yend = 0.525, color = "black",
               linewidth = 1, linetype = 1) +  
  geom_rect(aes(xmin = 3.25-0.1, xmax = 3.25+0.1, ymin = 0.62, ymax = 0.78), fill = "#56B4E9",
            color = "black",  linewidth = 0.75) +
  geom_rect(aes(xmin = 3.65-0.1, xmax = 3.65+0.1, ymin = 0.62, ymax = 0.78), fill = "#009E73",
            color = "black",  linewidth = 0.75) +
  geom_rect(aes(xmin = 3.25-0.1, xmax = 3.25+0.1, ymin = 0.82-0.55, ymax = 0.98-0.55), fill = "#E69F00",
            color = "black",  linewidth = 0.75) +
  geom_rect(aes(xmin = 3.65-0.1, xmax = 3.65+0.1, ymin = 0.82-0.55, ymax = 0.98-0.55), fill = "#CC79A7",
            color = "black",  linewidth = 0.75) +
  annotate("text", x = 3.45, y = c(0.36, 0.71), label = "x", hjust = "center", size = 10, fontface = 2) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = scales::percent) +
  scale_x_continuous(limits = c(0.5, 3.75), breaks = c(1, 1.5), label = c("A.1", "A.2")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(), 
        legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"),
        title = element_text(size = 14, face = "bold")) +
  labs(x = "Faktor A", y = "Messwert (y)",
       title = "Anteilsdifferenz",
       subtitle = "Chancenverhältnis (Odds Ratio, abk. OR) zwischen zwei Gruppen") 


p1_square_cov <- tibble(x = c(0.5, 1.5, 2, 2.5, 3),
                        y = 3 + 0 * x + rnorm(length(x), 0, 0.001)) |> 
  ggplot(aes(x, y)) +
  theme_minimal() +
  geom_smooth(method = "lm", formula = y ~ x, color = "#CC79A7", linewidth = 1,
              fullrange = TRUE, se = TRUE, fill = "#CC79A7", alpha = 0.2) +
  geom_point(color = "gray50", alpha = 0.5, size = 3) +
  scale_y_continuous(limits = c(0, 9), breaks = c(0, 2, 4, 6, 8)) +
  scale_x_continuous(limits = c(0, 3.5), breaks = c(0, 1, 2, 3)) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"),
        title = element_text(size = 14, face = "bold")) +
  labs(x = "Kovariate", y = "Messwert (y)",
       title = "Kein Effekt und keine Streuung",
       subtitle = "") 

p2_square_cov <- tibble(x = c(0.5, 1.5, 2, 2.5, 3),
                        y = 3 + 1.5 * x + rnorm(length(x), 0, 0.001)) |> 
  ggplot(aes(x, y)) +
  theme_minimal() +
  geom_smooth(method = "lm", formula = y ~ x, color = "#CC79A7", linewidth = 1,
              fullrange = TRUE, se = TRUE, fill = "#CC79A7", alpha = 0.2) +
  geom_point(color = "gray50", alpha = 0.5, size = 3) +
  scale_y_continuous(limits = c(0, 9), breaks = c(0, 2, 4, 6, 8)) +
  scale_x_continuous(limits = c(0, 3.5), breaks = c(0, 1, 2, 3)) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"),
        title = element_text(size = 14, face = "bold")) +
  labs(x = "Kovariate", y = "Messwert (y)",
       title = "Effekt und keine Streuung",
       subtitle = "") 

p3_square_cov <- tibble(x = c(0.5, 1.5, 2, 2.5, 3),
                        y = 3 + 0 * x + rnorm(length(x), 0, 0.4)) |> 
  ggplot(aes(x, y)) +
  theme_minimal() +
  geom_smooth(method = "lm", formula = y ~ x, color = "#CC79A7", linewidth = 1,
              fullrange = TRUE, se = TRUE, fill = "#CC79A7", alpha = 0.2) +
  geom_point(color = "gray50", alpha = 0.5, size = 3) +
  scale_y_continuous(limits = c(0, 9), breaks = c(0, 2, 4, 6, 8)) +
  scale_x_continuous(limits = c(0, 3.5), breaks = c(0, 1, 2, 3)) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"),
        title = element_text(size = 14, face = "bold")) +
  labs(x = "Kovariate", y = "Messwert (y)",
       title = "Kein Effekt und Streuung",
       subtitle = "")


p4_square_cov <- tibble(x = c(0.5, 1.5, 2, 2.5, 3),
                        y = 3 + 1.5 * x + rnorm(length(x), 0, 0.4)) |> 
  ggplot(aes(x, y)) +
  theme_minimal() +
  geom_smooth(method = "lm", formula = y ~ x, color = "#CC79A7", linewidth = 1,
              fullrange = TRUE, se = TRUE, fill = "#CC79A7", alpha = 0.2) +
  geom_point(color = "gray50", alpha = 0.5, size = 3) +
  scale_y_continuous(limits = c(0, 9), breaks = c(0, 2, 4, 6, 8)) +
  scale_x_continuous(limits = c(0, 3.5), breaks = c(0, 1, 2, 3)) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"),
        title = element_text(size = 14, face = "bold")) +
  labs(x = "Kovariate", y = "Messwert (y)",
       title = "Effekt und Streuung",
       subtitle = "")
