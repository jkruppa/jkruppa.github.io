pacman::p_load(tidyverse, readxl, knitr, kableExtra, broom,
               see, patchwork, ggbeeswarm, ggforce, latex2exp)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

set.seed(20240407)

truth_tbl <- tibble(grp = as_factor("truth"),
                    x = rnorm(200, 20, 7),
                    y = rnorm(200, 70, 7),
                    color = as_factor(sample(1:6, 200, replace = TRUE)),
                    shape = as_factor(sample(1:4, 200, replace = TRUE)))

data_theory_tbl <- tibble(grp = as_factor("data"),
                          x = rnorm(12, 90, 2),
                          y = rnorm(12, 80, 2),
                          color = as_factor(sample(1:3, 12, replace = TRUE)),
                          shape = as_factor(sample(1:2, 12, replace = TRUE)))

t1_tbl <- tibble(grp = as_factor("t1"),
                 x = rnorm(12, 70, 3),
                 y = rnorm(12, 45, 3),
                 color = as_factor(sample(2:4, 12, replace = TRUE)),
                 shape = as_factor(sample(2:3, 12, replace = TRUE)))

t2_tbl <- tibble(grp = as_factor("t1"),
                 x = rnorm(12, 60, 2),
                 y = rnorm(12, 25, 2),
                 color = as_factor(sample(4:6, 12, replace = TRUE)),
                 shape = as_factor(sample(2:3, 12, replace = TRUE)))

t3_tbl <- tibble(grp = as_factor("t1"),
                 x = rnorm(12, 35, 2),
                 y = rnorm(12, 10, 2),
                 color = as_factor(sample(1:4, 12, replace = TRUE)),
                 shape = as_factor(sample(3:4, 12, replace = TRUE)))

t4_tbl <- tibble(grp = as_factor("t1"),
                 x = rnorm(12, 5, 3),
                 y = rnorm(12, 10, 3),
                 color = as_factor(sample(3:6, 12, replace = TRUE)),
                 shape = as_factor(sample(1:2, 12, replace = TRUE)))

dist_tbl <- tibble(x_raw = seq(-4, 4, 0.01),
                   y_raw = dnorm(x_raw))

p <- bind_rows(truth_tbl,
          data_theory_tbl,
          t1_tbl,
          t2_tbl,
          t3_tbl,
          t4_tbl) |> 
  ggplot(aes(x, y)) +
  theme_void() +
  #theme_minimal() +
  geom_jitter(aes(fill = color, shape = shape), size = 4, width = 5, height = 5) +
  annotate("text", x = 60, y = 105, label = "Randomisierung", size = 5, fontface = 2) +
  annotate("text", x = 60, y = 101, label = "(Strukturgleichheit)", size = 5, fontface = 3) +
  geom_curve(x = 36, y = 90, xend = 85, yend = 85,
             arrow = arrow(length = unit(0.01, "npc"), type = "closed"),
             curvature = -0.5, color = "black", linewidth = 0.5) +
  geom_curve(x = 91, y = 38, xend = 43, yend = 72,
             arrow = arrow(length = unit(0.01, "npc"), type = "closed"),
             curvature = 0.6, color = "black", linewidth = 0.5) +
  geom_curve(x = 99, y = 83, xend = 110, yend = 68,
             arrow = arrow(length = unit(0.01, "npc"), type = "closed"),
             curvature = -0.2, color = "black", linewidth = 0.5) +
  geom_curve(x = 110, y = 60, xend = 105, yend = 11,
             arrow = arrow(length = unit(0.01, "npc"), type = "closed"),
             curvature = -0.3, color = "black", linewidth = 0.5) +
  annotate("text", x = 20, y = 102, label = "Grundgesamtheit", size = 5, fontface = 2) +
  geom_ellipse(aes(x0 = 20, y0 = 70, a = 22, b = 29, angle = 0), color = "black") +
  annotate("text", x = 93, y = 98, label = "Stichprobe", size = 5, fontface = 2) +
  annotate("text", x = 93, y = 94, label = "Daten (D)", size = 5, fontface = 3) +
  geom_ellipse(aes(x0 = 91, y0 = 80, a = 12, b = 7, angle = 20), color = "black") +
  annotate("text", x = 110, y = 66, label = "Teststatistik", size = 5, fontface = 2) +
  annotate("label", x = 110, y = 60, label = expression(T[D]), size = 7) +
  annotate("text", x = 60, y = 77, label = "Rückschluß", size = 5, fontface = 2) +
  annotate("text", x = 60, y = 70, label = expression(Gilt~die~H[0]*"?"), 
           size = 5, fontface = 3) +
  geom_ellipse(aes(x0 = 7, y0 = 10, a = 10, b = 10, angle = 0), color = "gray") +
  geom_ellipse(aes(x0 = 36, y0 = 10, a = 10, b = 10, angle = 0), color = "gray") +
  geom_ellipse(aes(x0 = 60, y0 = 24, a = 10, b = 10, angle = 0), color = "gray") +
  geom_ellipse(aes(x0 = 70, y0 = 44, a = 10, b = 10, angle = 0), color = "gray") +
  geom_curve(x = 34, y = 42, xend = 59, yend = 44,
             arrow = arrow(length = unit(0.01, "npc"), type = "closed"),
             curvature = -0.2, color = "gray", linewidth = 0.5) +
  geom_curve(x = 35, y = 41, xend = 51, yend = 30,
             arrow = arrow(length = unit(0.01, "npc"), type = "closed"),
             curvature = -0.2, color = "gray", linewidth = 0.5) +
  geom_curve(x = 30, y = 40, xend = 35, yend = 21,
             arrow = arrow(length = unit(0.01, "npc"), type = "closed"),
             curvature = -0.2, color = "gray", linewidth = 0.5) +
  geom_curve(x = 20, y = 40, xend = 8, yend = 21,
             arrow = arrow(length = unit(0.01, "npc"), type = "closed"),
             curvature = 0.2, color = "gray", linewidth = 0.5) +
  annotate("label", x = 25, y = 42, label = expression(H[0]~wahr), 
           size = 7) +
  geom_line(data = dist_tbl, aes(x = (x_raw + 13)*7, y = y_raw*80+5),
            linewidth = 1, color = "gray") +
  geom_segment(x = 65, y = 5, xend = 115, yend = 5, color = "gray") +
  geom_segment(x = 91, y = 5, xend = 91, yend = 37, color = "gray") +
  geom_segment(x = 104, y = 5, xend = 104, yend = 10.5) +
  annotate("text", x = 91, y = 3, label = "0") +
  annotate("text", x = 104, y = 3, label = expression(T[D])) +
  annotate("text", x = 91, y = -1, label = expression(Testverteilung~"("*T[1]*","*"..."*","*T[p]*")"), size = 5, 
           fontface = 2) +
  annotate("text", x = 21.5, y = 12, label = "...", size = 12, fontface = 2,
           color = "gray") +
  annotate("label", x = 81, y = 42, label = expression(T[1]), size = 7) +
  annotate("label", x = 70, y = 18, label = expression(T[2]), size = 7) +
  annotate("label", x = 46, y = 3, label = expression(T[3]), size = 7) +
  annotate("label", x = 16.5, y = 3, label = expression(T[p]), size = 7) +
  scale_x_continuous(limits = c(-3, 115)) +
  scale_y_continuous(limits = c(-4, 105)) +
  scale_shape_manual(values = 21:24) +
  theme(legend.position = "none") +
  scale_fill_okabeito() 

