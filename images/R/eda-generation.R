p_rr_intro <- tibble(x = c(0,  1, 2, 3), y = 1) |> 
  ggplot(aes(x, y)) +
  theme_minimal() +
  geom_rect(aes(xmin = 0.8, xmax = 1.2, ymin = 0, ymax = 0.6), fill = "#E69F00",
            color = "black",  linewidth = 0.75) +
  geom_rect(aes(xmin = 0.8, xmax = 1.2, ymin = 0.6, ymax = 1), fill = "#56B4E9",
            color = "black",  linewidth = 0.75) +
  geom_vline(xintercept = 0.5,  linewidth = 1) +
  geom_hline(yintercept = 0,  linewidth = 1) +
  ## Ratio
  annotate("text", x = 2.5, y = 0.8-0.275, label = "Ratio =", hjust = "right", size = 6, fontface = 2) +
  geom_segment(x = 2.55, y = 0.8-0.275, xend = 3.5, yend = 0.8-0.275, color = "black",
               linewidth = 0.75, linetype = 1) +
  geom_rect(aes(xmin = 3.05-0.1, xmax = 3.05+0.1, ymin = 0.82-0.275, ymax = 0.98-0.275), fill = "#56B4E9",
            color = "black",  linewidth = 0.75) +
  geom_rect(aes(xmin = 3.05-0.1-0.25, xmax = 3.05+0.1-0.25, ymin = 0.62-0.275, ymax = 0.78-0.275), fill = "#56B4E9",
            color = "black",  linewidth = 0.75) +
  geom_rect(aes(xmin = 3.05-0.1+0.25, xmax = 3.05+0.1+0.25, ymin = 0.62-0.275, ymax = 0.78-0.275), fill = "#E69F00",
            color = "black",  linewidth = 0.75) +
  annotate("text", x = 3.05, y = 0.7-0.275, label = "+", hjust = "center", size = 10, fontface = 2) +
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
       title = "Verhältnis (Ratio)",
       subtitle = "Anteil von blauen zu allen Beobachtungen") 

p_or_intro <- 
  tibble(x = c(0,  1, 2, 3), y = 1) |> 
  ggplot(aes(x, y)) +
  theme_minimal() +
  geom_rect(aes(xmin = 0.8, xmax = 1.2, ymin = 0, ymax = 0.6), fill = "#E69F00",
            color = "black",  linewidth = 0.75) +
  geom_rect(aes(xmin = 0.8, xmax = 1.2, ymin = 0.6, ymax = 1), fill = "#56B4E9",
            color = "black",  linewidth = 0.75) +
  geom_vline(xintercept = 0.5,  linewidth = 1) +
  geom_hline(yintercept = 0,  linewidth = 1) +
  ## Odds
  annotate("text", x = 2.5, y = 0.8-0.275, label = "Odd =", hjust = "right", size = 6, fontface = 2) +
  geom_segment(x = 2.55, y = 0.8-0.275, xend = 3.5, yend = 0.8-0.275, color = "black",
               linewidth = 0.75, linetype = 1) +
  geom_rect(aes(xmin = 3.05-0.1, xmax = 3.05+0.1, ymin = 0.82-0.275, ymax = 0.98-0.275), fill = "#56B4E9",
            color = "black",  linewidth = 0.75) +
  geom_rect(aes(xmin = 3.05-0.1, xmax = 3.05+0.1, ymin = 0.62-0.275, ymax = 0.78-0.275), fill = "#E69F00",
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
       title = "Chance (Odd)",
       subtitle = "Anteil von blauen zu orangen Beobachtungen") 

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
       title = "Risikoverhältnis",
       subtitle = "Risk Ratio (abk. RR) zwischen zwei Gruppen") 

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
  annotate("text", x = 3.45, y = c(0.36, 0.71), label = "x", hjust = "center", size = 7, fontface = 2) +
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
       title = "Chancenverhältnis",
       subtitle = "Odds Ratio (abk. OR) zwischen zwei Gruppen") 


p1_square_cov <- tibble(x = c(0.5, 1.5, 2, 2.5, 3),
                        y = 3 + 0 * x + rnorm(length(x), 0, 0.001)) |> 
  ggplot(aes(x, y)) +
  theme_minimal() +
  geom_smooth(method = "lm", formula = y ~ x, color = "#CC79A7", linewidth = 1,
              fullrange = TRUE, se = TRUE, fill = "#CC79A7", alpha = 0.2) +
  geom_point(color = "gray50", alpha = 0.5, size = 3) +
  scale_y_continuous(limits = c(0, 9), breaks = c(0, 2, 4, 6, 8)) +
  scale_x_continuous(limits = c(0, 3.5), breaks = c(0, 1, 2, 3)) +
  stat_poly_eq(use_label("eq"), coef.digits = 1) +
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
  stat_poly_eq(use_label("eq"), coef.digits = 2) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"),
        title = element_text(size = 14, face = "bold")) +
  labs(x = "Kovariate", y = "Messwert (y)",
       title = "Effekt und keine Streuung",
       subtitle = "") 

p3_square_cov <- 
  tibble(x = c(0.5, 1.5, 2, 2.5, 3),
                        y = 3 + 0 * x + rnorm(length(x), 0, 0.4)) |> 
  ggplot(aes(x, y)) +
  theme_minimal() +
  geom_smooth(method = "lm", formula = y ~ x, color = "#CC79A7", linewidth = 1,
              fullrange = TRUE, se = TRUE, fill = "#CC79A7", alpha = 0.2) +
  geom_point(color = "gray50", alpha = 0.5, size = 3) +
  scale_y_continuous(limits = c(0, 9), breaks = c(0, 2, 4, 6, 8)) +
  scale_x_continuous(limits = c(0, 3.5), breaks = c(0, 1, 2, 3)) +
  stat_poly_eq(use_label("eq"), coef.digits = 2) +
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
  stat_poly_eq(use_label("eq"), coef.digits = 2) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"),
        title = element_text(size = 14, face = "bold")) +
  labs(x = "Kovariate", y = "Messwert (y)",
       title = "Effekt und Streuung",
       subtitle = "")

p4_square_fac <- 
  tibble(f = c(rep(1, 6), rep(2, 6)),
                        y = c(4, 5, 5.5, 6.5, 7, 9,  
                              6, 8, 8.5, 9.5, 10, 11)) |> 
  ggplot(aes(f, y)) +
  theme_minimal() +
  geom_smooth(method = "lm", formula = y ~ x, color = "#CC79A7", linewidth = 1,
              fullrange = TRUE, se = TRUE, fill = "#CC79A7", alpha = 0.2) +
  geom_point(color = "gray50", alpha = 0.5, size = 3) +
  stat_summary(fun.data=mean_sdl, , fun.args = list(mult = 1), 
               geom="pointrange", shape = 23, 
               fill = c("#56B4E9", "#E69F00"), size = 0.75) +
  annotate("label", x = c(1, 2), y = 12, label = c(expression(n[1]~"="~6), 
                                                   expression(n[2]~"="~6))) +
  annotate("label", x = 1.5, y = 7.5, label = expression(atop(bold(Delta[abs]~"="~+"2.7"),
                                                              bold(Delta[rel]~"="%*%"1.4"))),
           color = "#CC79A7", size = 5) +
  scale_y_continuous(limits = c(3, 12.5), breaks = c(6, 9)) +
  scale_x_continuous(limits = c(0.5, 2.5),breaks = c(1, 2), label = c("A.1", "A.2")) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"),
        title = element_text(size = 14, face = "bold")) +
  labs(x = "Faktor A", y = "Messwert (y)",
       title = "Effekt und Streuung",
       subtitle = "") 

p1_square_fac <- tibble(f = c(rep(1, 6), rep(2, 6)),
                        y = c(7.499, 7.5, 7.5, 7.5, 7.5, 7.501,  
                              7.499, 7.5, 7.5, 7.5, 7.5, 7.501)) |> 
  ggplot(aes(f, y)) +
  theme_minimal() +
  geom_smooth(method = "lm", formula = y ~ x, color = "#CC79A7", linewidth = 1,
              fullrange = TRUE, se = TRUE, fill = "#CC79A7", alpha = 0.2) +
  geom_point(color = "gray50", alpha = 0.5, size = 3) +
  stat_summary(fun.data=mean_sdl, , fun.args = list(mult = 1), 
               geom="pointrange", shape = 23, 
               fill = c("#56B4E9", "#E69F00"), size = 0.75) +
  annotate("label", x = c(1, 2), y = 12, label = c(expression(n[1]~"="~6), 
                                                   expression(n[2]~"="~6))) +
  annotate("label", x = 1.5, y = 7.5, label = expression(atop(bold(Delta[abs]~"="~+"0.0"),
                                                              bold(Delta[rel]~"="%*%"1.0"))),
           color = "#CC79A7", size = 5) +
  scale_y_continuous(limits = c(3, 12.5), breaks = c(6, 9)) +
  scale_x_continuous(limits = c(0.5, 2.5),breaks = c(1, 2), label = c("A.1", "A.2")) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"),
        title = element_text(size = 14, face = "bold")) +
  labs(x = "Faktor A", y = "Messwert (y)",
       title = "Kein Effekt und keine Streuung",
       subtitle = "")


p2_square_fac <- tibble(f = c(rep(1, 6), rep(2, 6)),
                        y = c(5.999, 6, 6, 6, 6, 6.001,  
                              8.999, 9, 9, 9, 9, 9.001)) |> 
  ggplot(aes(f, y)) +
  theme_minimal() +
  geom_smooth(method = "lm", formula = y ~ x, color = "#CC79A7", linewidth = 1,
              fullrange = TRUE, se = TRUE, fill = "#CC79A7", alpha = 0.2) +
  geom_point(color = "gray50", alpha = 0.5, size = 3) +
  stat_summary(fun.data=mean_sdl, , fun.args = list(mult = 1), 
               geom="pointrange", shape = 23, 
               fill = c("#56B4E9", "#E69F00"), size = 0.75) +
  annotate("label", x = c(1, 2), y = 12, label = c(expression(n[1]~"="~6), 
                                                   expression(n[2]~"="~6))) +
  annotate("label", x = 1.5, y = 7.5, label = expression(atop(bold(Delta[abs]~"="~+"3.0"),
                                                              bold(Delta[rel]~"="%*%"1.5"))),
           color = "#CC79A7", size = 5) +
  scale_y_continuous(limits = c(3, 12.5), breaks = c(6, 9)) +
  scale_x_continuous(limits = c(0.5, 2.5),breaks = c(1, 2), label = c("A.1", "A.2")) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"),
        title = element_text(size = 14, face = "bold")) +
  labs(x = "Faktor A", y = "Messwert (y)",
       title = "Effekt und keine Streuung",
       subtitle = "")

p3_square_fac <- tibble(f = c(rep(1, 6), rep(2, 6)),
                        y = c(5, 6, 7, 8, 9, 11,  
                              3, 6, 7, 8, 9, 10)) |> 
  ggplot(aes(f, y)) +
  theme_minimal() +
  geom_smooth(method = "lm", formula = y ~ x, color = "#CC79A7", linewidth = 1,
              fullrange = TRUE, se = TRUE, fill = "#CC79A7", alpha = 0.2) +
  geom_point(color = "gray50", alpha = 0.5, size = 3) +
  stat_summary(fun.data=mean_sdl, , fun.args = list(mult = 1), 
               geom="pointrange", shape = 23, 
               fill = c("#56B4E9", "#E69F00"), size = 0.75) +
  annotate("label", x = c(1, 2), y = 12, label = c(expression(n[1]~"="~6), 
                                                   expression(n[2]~"="~6))) +
  annotate("label", x = 1.5, y = 7.5, label = expression(atop(bold(Delta[abs]~"="~-"0.5"),
                                                              bold(Delta[rel]~"="%*%"0.93"))),
           color = "#CC79A7", size = 5) +
  scale_y_continuous(limits = c(3, 12.5), breaks = c(6, 9)) +
  scale_x_continuous(limits = c(0.5, 2.5),breaks = c(1, 2), label = c("A.1", "A.2")) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"),
        title = element_text(size = 14, face = "bold")) +
  labs(x = "Faktor A", y = "Messwert (y)",
       title = "Kein Effekt und Streuung",
       subtitle = "")


p_venn_gen <- ggplot() +
  theme_void() +
  geom_circle(aes(x0 = 1, y0 = 1, r = 1), 
              fill = "#56B4E9", alpha = 0.5) +
  geom_circle(aes(x0 = (1+2.25)/2, y0 = 0, r = 1),
              fill = "#009E73", alpha = 0.5) +
  geom_circle(aes(x0 = 2.25, y0 = 1, r = 1), 
              fill = "#D55E00", alpha = 0.4) +
  annotate("text", x = (1+2.25)/2, y = 0.7, label = "Daten", size = 9, fontface = 2) +
  annotate("text", x = 0.6, y = 1.6, label = "Effekt", size = 9, fontface = 2) +
  annotate("text", x = 0.6, y = 1.1, label = "Wie groß ist\nder Unterschied?", 
           size = 6, fontface = 3) +
  annotate("text", x = 2.5, y = 1.6, label = "Streuung", size = 9, fontface = 2) +
  annotate("text", x = 2.6, y = 1.1, label = "Wie unterschiedlich\nsind die generierten\nMesswerte?", size = 6, fontface = 3) +
  annotate("text", x = (1+2.25)/2, y = -0.75, label = "Fallzahl", size = 9, fontface = 2) +
  annotate("text", x = (1+2.25)/2, y = -0.25, label = "Wie viele\nBeobachtungen sollen\ngeneriert werden?", size = 6, fontface = 3) +
  labs(x = "", y = "", fill = "",
       title = "Dreiklang der Generierung von Daten",
       subtitle = "Zusammenhang von Effekt, Streuung und Fallzahl") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic")) 

p_theo_observed <- 
  ggplot(data.frame(x = c(-3.25, 3.25)), aes(x)) +
  theme_minimal() +
  stat_function(fun = dnorm, linewidth = 1, args = list(mean = 0, sd = 2), 
                xlim = c(-8.25, 8.25), color = "#E89F00") +
  stat_function(fun = dnorm, xlim = c(-8.25, 8.25), args = list(mean = 0, sd = 2), 
                geom = "area", fill = "#E89F00", alpha = 0.25) +
  geom_segment(data = tibble(x = c(1.3, 2.1, 3.7, 4.2, 5.7), 
                             xend = c(1.3, 2.1, 3.7, 4.2, 5.7),
                             y = rep(-0.1, 5), yend = rep(-0.175, 5)), 
               aes(x, xend = xend, y, yend = yend), color = "#009E73",
               linewidth = 0.75, linetype = 1) +
  annotate("point", x = mean(c(1.3, 2.1, 3.7, 4.2, 5.7)), y = (-0.1 + -0.175)/2, 
           shape = 23, fill = "#CC79A7", size = 2.5) +
  annotate("label", x = -6, y = (-0.1 + -0.175)/2+0.05, label = "Simulation 1",
           fill = "#009E73", size = 2, alpha = 0.5) +
  geom_segment(data = tibble(x = c(-5.3, -4.6, -2.1, -2.5, 1.1), 
                             xend = c(-5.3, -4.6, -2.1, -2.5, 1.1),
                             y = rep(-0.3, 5), yend = rep(-0.375, 5)), 
               aes(x, xend = xend, y, yend = yend), color = "#009E73",
               linewidth = 0.75, linetype = 1) +
  annotate("point", x = mean(c(-5.3, -4.6, -2.1, -2.5, 1.1)), y = (-0.3 + -0.375)/2, 
           shape = 23, fill = "#CC79A7", size = 2.5) +
  annotate("label", x = -6, y = (-0.3 + -0.375)/2+0.05, label = "Simulation 2",
             fill = "#009E73", size = 2, alpha = 0.5) +
  geom_segment(data = tibble(x = c(-0.3, -1.6, -0.1, 2.5, 3.1), 
                             xend = c(-0.3, -1.6, -0.1, 2.5, 3.1),
                             y = rep(-0.5, 5), yend = rep(-0.575, 5)), 
               aes(x, xend = xend, y, yend = yend), color = "#009E73",
               linewidth = 0.75, linetype = 1) +
  annotate("point", x = mean(c(-0.3, -1.6, -0.1, 2.5, 3.1)), y = (-0.5 + -0.575)/2, 
           shape = 23, fill = "#CC79A7", size = 2.5) +
    annotate("label", x = -6, y = (-0.5 + -0.575)/2+0.05, label = "Simulation 3",
             fill = "#009E73", size = 2, alpha = 0.5) +
  geom_segment(data = tibble(x = c(2.3, 3.7, -1.0, 2.9, -2.81), 
                             xend = c(2.3, 3.7, -1.0, 2.9, -2.81),
                             y = rep(-0.7, 5), yend = rep(-0.775, 5)), 
               aes(x, xend = xend, y, yend = yend), color = "#009E73",
               linewidth = 0.75, linetype = 1) +
  annotate("point", x = mean(c(2.3, 3.7, -1.0, 2.9, -2.81)), y = (-0.7 + -0.775)/2, 
           shape = 23, fill = "#CC79A7", size = 2.5) +
    annotate("label", x = -6, y = (-0.7 + -0.775)/2+0.05, label = "Simulation 4",
             fill = "#009E73", size = 2, alpha = 0.5) +
  geom_segment(data = tibble(x = c(0.9, 2.8, 0.2, 2.9, 2.0), 
                             xend = c(0.9, 2.8, 0.2, 2.9, 2.0),
                             y = rep(-0.9, 5), yend = rep(-0.975, 5)), 
               aes(x, xend = xend, y, yend = yend), color = "#009E73",
               linewidth = 0.75, linetype = 1) +
  annotate("point", x = mean(c(0.9, 2.8, 0.2, 2.9, 2.0)), y = (-0.9 + -0.975)/2, 
           shape = 23, fill = "#CC79A7", size = 2.5) +
    annotate("label", x = -6, y = (-0.9 + -0.975)/2+0.05, label = "Simulation 5",
             fill = "#009E73", size = 2, alpha = 0.5) +
  geom_vline(xintercept = c(0)) + 
  geom_hline(yintercept = c(0)) + 
  scale_y_continuous(breaks = c((-0.1 + -0.175)/2, (-0.3 + -0.375)/2,
                                (-0.5 + -0.575)/2, (-0.7 + -0.775)/2, (-0.9 + -0.975)/2)) +
  scale_x_continuous(breaks = c(-4, -2, 0, 2, 4), limits = c(-6, 6)) +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"),
        title = element_text(size = 14, face = "bold")) +
  labs(x = "", y = "",
       title = "Theoretische Verteilung vs. beobachtete Werte",
       subtitle = "Bei einer kleinen Stichprobe weicht der beobachtete vom theoretischen Mittelwert ab") 

p1_mean <- mean_sim_tbl |> 
  filter(n != 1000) |> 
  ggplot(aes(as_factor(n), mean)) +
  theme_modeling() +
  geom_violin(fill = "#E69F00") +
  geom_hline(yintercept = 0, color = "gray25") +
  ylim(c(-2.5, 2.5)) +
  labs(x = "Simulierte Anzahl an Beobachtungen (Fallzahl)",
       y = "Beobachteter Mittelwert")

p2_mean <- mean_sim_tbl |> 
  filter(n == 1000) |> 
  ggplot(aes(as_factor(n), mean)) +
  theme_modeling() +
  geom_violin(fill = "#56B4E9") +
  geom_hline(yintercept = 0, color = "gray25") +
  ylim(c(-2.5, 2.5)) +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank())

p_barplot_intro <- tibble(x = c(0,  1, 2, 3), y = 12) |> 
  ggplot(aes(x, y)) +
  theme_minimal() +
  geom_rect(aes(xmin = 0.8, xmax = 1.2, ymin = 0, ymax = 6), fill = "#E69F00",
            color = "black",  linewidth = 0.75) +
  geom_segment(x = 1, y = 6, xend = 1, yend = 9.27, color = "black",
               linewidth = 0.75, linetype = 1) +
  geom_segment(x = 0.9, y = 9.27, xend = 1.1, yend = 9.27, color = "black",
               linewidth = 0.75, linetype = 1) +
  geom_rect(aes(xmin = 0.8+1.25, xmax = 1.2+1.25, ymin = 0, ymax = 9), fill = "#56B4E9",
            color = "black",  linewidth = 0.75) +
  geom_segment(x = 1+1.25, y = 9, xend = 1+1.25, yend = 12.2, color = "black",
               linewidth = 0.75, linetype = 1) +
  geom_segment(x = 0.9+1.25, y = 12.2, xend = 1.1+1.25, yend = 12.2, color = "black",
               linewidth = 0.75, linetype = 1) +
  geom_vline(xintercept = 0,  linewidth = 1) +
  geom_hline(yintercept = 0,  linewidth = 1) +
  scale_x_continuous(breaks = c(1, 2.25), label = c("A.1", "A.2")) +
  scale_y_continuous(breaks = c(0, 6, 9)) +
  labs(x = "Faktor A", y = "Messwert (Y)",
       title = "Säulendiagramm",
       subtitle = "") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(), 
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 12))


group_tbl <- tibble(f = c(rep(1, 6), rep(2, 6)),
                    y = c(4, 5, 5.5, 6.5, 7, 8,  
                          7, 8, 8.5, 9.5, 10, 11)) 

group_fit <- lm(y ~f, data = group_tbl)


p_effect_intro <- group_tbl |> 
  ggplot(aes(f, y)) +
  theme_minimal() +
  geom_line(aes(y = predict(group_fit)), color = "#CC79A7", linewidth = 1) +
  geom_point(color = "gray50", alpha = 0.5, size = 3) +
  stat_summary(fun.data=mean_sdl, , fun.args = list(mult = 1), 
               geom="pointrange", shape = 23, 
               fill = c("#E69F00", "#56B4E9"), size = 0.75) +
  annotate("label", x = c(1, 2), y = 13, label = c(expression(n[1]~"="~6), 
                                                   expression(n[2]~"="~6))) +
  annotate("label", x = 1.5, y = 7.5, label = expression(atop(bold(Delta[abs]~"="~+"3.0"),
                                                              bold(Delta[rel]~"="%*%"1.5"))),
           color = "#CC79A7", size = 5) +
  scale_y_continuous(limits = c(0, 12), breaks = c(6, 9)) +
  scale_x_continuous(limits = c(0.8, 2.2),breaks = c(1, 2), label = c("A.1", "A.2")) +
  geom_vline(xintercept = 0.8,  linewidth = 1) +
  geom_hline(yintercept = 0,  linewidth = 1) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"),
        title = element_text(size = 14, face = "bold")) +
  labs(x = "Faktor A", y = "Messwert (y)",
       title = "Mittelwertsdifferenz",
       subtitle = "Absoluter und relativer Effekt zweier Gruppen") 

p_fac2_excel <- tibble(x = 0:10, y = 0:10) |> 
  ggplot(aes(x, y)) +
  xlim(c(0, 10)) + ylim(c(0, 10)) +
  theme_void() +
  geom_image(data = tibble(x = 5, y = 4.8),
             aes(image = "images/gen-data-fac2-excel.png"), size = 1.1) +
  geom_tile(aes(x = 5.525, y = 4.6, width = 4.125, height = 8.65), fill = "#009E73",
            alpha = 0.005, color = "#009E73", linewidth = 0.25) +
  geom_tile(aes(x = 8, y = 4.6, width = 0.73, height = 8.65), fill = "#56B4E9",
            alpha = 0.005, color = "#56B4E9", linewidth = 0.25) +
  geom_tile(aes(x = 2.65, y = 4.6, width = 1.6, height = 8.65), fill = "#E69F00",
            alpha = 0.005, color = "#E69F00", linewidth = 0.25) +
  annotate("label", x = 8.6, y = 8,  size = 4, fontface = 2, color = "#56B4E9", hjust = "left",
           label = "Additiver Effekt:\nC+D+E+F+G") +
  geom_curve(aes(x = 9.5, y = 8.6, xend = 8, yend = 8.9),
             arrow = arrow(length = unit(0.01, "npc"), type = "closed"),
             curvature = 0.4, color = "#56B4E9") +
  annotate("label", x = 6.5, y = 10,  size = 4, fontface = 2, color = "gray50", hjust = "left",
           label = "=NORMINV(ZUFALLSZAHL();0;2)") +
  geom_curve(aes(x = 6.45, y = 10, xend = 6.85, yend = 8.7),
             arrow = arrow(length = unit(0.01, "npc"), type = "closed"),
             curvature = 0.9, color = "gray50") +
  annotate("label", x = 3.4, y = 0.2, label = "Informationen zu den Effekten", size = 2.5, 
           fontface = 2, fill = "#009E73", alpha = 1, hjust = "left") +
  annotate("label", x = 1.8, y = 9.05, label = "3 Level", size = 2.5, 
           fontface = 2, fill = "#E69F00", alpha = 1, hjust = "left") +
  annotate("label", x = 2.6, y = 9.05, label = "2 Level", size = 2.5, 
           fontface = 2, fill = "#E69F00", alpha = 1, hjust = "left") 

p_fac1_excel <- tibble(x = 0:10, y = 0:10) |> 
  ggplot(aes(x, y)) +
  xlim(c(0, 10)) + ylim(c(0, 10)) +
  theme_void() +
  geom_image(data = tibble(x = 5, y = 4.9),
             aes(image = "images/gen-data-fac1-excel.png"), size = 0.75) +
  geom_tile(aes(x = 5.15, y = 4.7, width = 2.7, height = 9.1), fill = "#009E73",
            alpha = 0.005, color = "#009E73", linewidth = 0.25) +
  geom_tile(aes(x = 6.915, y = 4.7, width = 0.73, height = 9.1), fill = "#56B4E9",
            alpha = 0.005, color = "#56B4E9", linewidth = 0.25) +
  geom_tile(aes(x = 3.39, y = 4.7, width = 0.76, height = 9.1), fill = "#E69F00",
            alpha = 0.005, color = "#E69F00", linewidth = 0.25) +
  annotate("label", x = 8, y = 8,  size = 4, fontface = 2, color = "#56B4E9", hjust = "left",
           label = "Additiver Effekt:\nB+C+D") +
  geom_curve(aes(x = 8.4, y = 8.6, xend = 7.215, yend = 9),
             arrow = arrow(length = unit(0.01, "npc"), type = "closed"),
             curvature = 0.3, color = "#56B4E9") +
  annotate("label", x = 6.5, y = 10,  size = 4, fontface = 2, color = "gray50", hjust = "left",
           label = "=NORMINV(ZUFALLSZAHL();0;2)") +
  geom_curve(aes(x = 6.45, y = 10, xend = 5.9, yend = 9.2),
             arrow = arrow(length = unit(0.01, "npc"), type = "closed"),
             curvature = 0.6, color = "gray50") +
  annotate("label", x = 3.7, y = 0.1, label = "Informationen zu den Effekten", size = 2.5, 
           fontface = 2, fill = "#009E73", alpha = 1, hjust = "left") +
  annotate("label", x = 2.9, y = 9.35, label = "3 Level", size = 2.5, 
           fontface = 2, fill = "#E69F00", alpha = 1, hjust = "left") 


p1_ord_theo <- ggplot(data.frame(x = c(-3.25, 3.25)), aes(x)) +
  theme_modeling() +
  geom_vline(xintercept = 0) +
  stat_function(fun = dnorm, xlim = c(-3.25, -0.8),
                geom = "area", alpha = 0.5, aes(fill = "1")) +
  stat_function(fun = dnorm, xlim = c(-0.8, 0.2),
                geom = "area", alpha = 0.5, aes(fill = "2")) +
  stat_function(fun = dnorm, xlim = c(0.2, 0.7),
                geom = "area", alpha = 0.5, aes(fill = "3")) +
  stat_function(fun = dnorm, xlim = c(0.7, 3.25),
                geom = "area", alpha = 0.5, aes(fill = "4")) +
  geom_segment(x = c(-0.8), xend = c(-0.8),
               y = c(0), yend = c(0.29), color = "#CC79A7",
               linewidth = 1, linetype = 1) +  
  geom_segment(x = c(0.2), xend = c(0.2),
               y = c(0), yend = c(0.39), color = "#CC79A7",
               linewidth = 1, linetype = 1) + 
  geom_segment(x = c(0.7), xend = c(0.7),
               y = c(0), yend = c(0.31), color = "#CC79A7",
               linewidth = 1, linetype = 1) + 
  stat_function(fun = dnorm, linewidth = 1) + 
  scale_x_continuous(breaks = c(-0.8, 0.2, 0.7), 
                     labels = c(-0.8, 0.2, 0.7)) +
  labs(fill = "Antwort", x = "", y = "",
       title = "Verteilung der latenten Variable",
       subtitle = "Antwortverteilung für Level A.1") +
  theme(legend.position = "top",
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  scale_fill_okabeito(labels = c("trifft gar nicht zu", "trifft nicht zu",
                                 "trifft zu", "trifft voll zu"))

p2_ord_theo <- ggplot(data.frame(x = c(-3.25, 3.25)), aes(x)) +
  theme_modeling() +
  geom_vline(xintercept = 0) +
  stat_function(fun = dnorm, xlim = c(-3.25, -0.1),
                geom = "area", alpha = 0.5, aes(fill = "1")) +
  stat_function(fun = dnorm, xlim = c(-0.1, 0.9),
                geom = "area", alpha = 0.5, aes(fill = "2")) +
  stat_function(fun = dnorm, xlim = c(0.9, 1.4),
                geom = "area", alpha = 0.5, aes(fill = "3")) +
  stat_function(fun = dnorm, xlim = c(1.4, 3.25),
                geom = "area", alpha = 0.5, aes(fill = "4")) +
  geom_segment(x = c(-0.8), xend = c(-0.8),
               y = c(0), yend = c(0.29), color = "#CC79A7",
               linewidth = 1, linetype = 1) +  
  geom_segment(x = c(0.2), xend = c(0.2),
               y = c(0), yend = c(0.39), color = "#CC79A7",
               linewidth = 1, linetype = 1) + 
  geom_segment(x = c(0.7), xend = c(0.7),
               y = c(0), yend = c(0.31), color = "#CC79A7",
               linewidth = 1, linetype = 1) + 
  annotate("label", hjust = "left", x = 2, y = 0.35, label = "z + 0.7", size = 5) +
  stat_function(fun = dnorm, linewidth = 1) + 
  scale_x_continuous(breaks = c(-0.1, 0.9, 1.4), 
                     labels = c(-0.1, 0.9, 1.4)) +
  labs(fill = "Antwort", x = "Latente Variable (z)", y = "",
       subtitle = "Antwortverteilung für Level A.2") +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  scale_fill_okabeito(labels = c("trifft gar nicht zu", "trifft nicht zu",
                                 "trifft zu", "trifft voll zu"))