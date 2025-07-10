p1 <- ggplot() +
  annotate("text", -0.5, 0, label = "g", size = 10, fontface = 2, color = "#CC79A7", hjust = "left") +
  annotate("text", 0.9, 0, label = "er", size = 10, fontface = 2, color = "#0072B2", hjust = "left") +
  annotate("text", 0, 0, label = "lm", size = 10, fontface = 2, hjust = "left") +
  scale_x_continuous(limits = c(-5, 5)) +
  scale_y_continuous(limits = c(-5, 5)) +
  annotate("text", x = -3, y = -3, label = "generalized", size = 6, fontface = 3, color = "gray50") +
  annotate("text", x = -3, y = -4.5, label = "glm(y ~ x, family = ...)", size = 5, 
           fontface = 3, color = "#E69F00") +
  annotate("text", x = 3.5, y = 3, label = "linear model", size = 6, fontface = 3, color = "gray50") +
  annotate("text", x = 3.5, y = 4.5, label = "lm(y ~ x)", size = 5, 
           fontface = 3, color = "#E69F00") +
  annotate("text", x = 3.5, y = -3, label = "mixed", size = 6, 
           fontface = 3, color = "gray50") +
  annotate("text", x = 3.5, y = -4.5, label = "lmer(y ~ x + (1|z))", size = 5, 
           fontface = 3, color = "#E69F00") +
  geom_curve(aes(x = -1.8, y = -3, xend = -0.4, yend = -1.3),
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = 0.2, color = "gray50") +
  geom_curve(aes(x = 2.3, y = 3, xend = 0.5, yend = 0.6),
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = 0.2, color = "gray50") +
  geom_curve(aes(x = 2.9, y = -3, xend = 1.25, yend = -1),
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = -0.2, color = "gray50") +
  theme_void()

p2 <- ggplot() +
  annotate("text", -10, 2.5, label = "simple", size = 7, fontface = 2, hjust = "left") +
  annotate("text", -10, -2.5, label = "multiple", size = 7, fontface = 2, hjust = "left") +
  annotate("text", -5, 7.5, label = "Gaussian", size = 7, fontface = 2, hjust = "left") +
  annotate("text", -5, 3.75, label = "Poisson", size = 7, fontface = 2, hjust = "left") +
  annotate("text", -5, 0, label = "Ordinal", size = 7, fontface = 2, hjust = "left") +
  annotate("text", -3.75, -3.5, label = expression(cdots), size = 10, hjust = "center") +
  annotate("text", -5, -7, label = "Logistic", size = 7, fontface = 2, hjust = "left") +
  annotate("text", -0.5, 0, label = "gemischte", size = 7, fontface = 2, hjust = "left") +
  annotate("text", 4.5, 0, label = "lineare Regression", size = 7, fontface = 2, hjust = "left") +
  annotate("text", c(-6.25, -1.25, 3.5), 0, label = "+", size = 8, fontface = 2, hjust = "left",
           color = "#CC79A7") +
  scale_x_continuous(limits = c(-10.5, 11)) +
  scale_y_continuous(limits = c(-12, 12)) +
  ## Klammern X
  annotate("segment", x = c(-10.25, -7), y = -5, xend = c(-10.25, -7), yend = 5, 
           color = "#CC79A7", linewidth = 1) +
  annotate("segment", x = c(-10.25, -7), y = -5, xend = c(-10, -7.25), yend = -5, 
           color = "#CC79A7", linewidth = 1) +
  annotate("segment", x = c(-10.25, -7), y = 5, xend = c(-10, -7.25), yend = 5, 
           color = "#CC79A7", linewidth = 1) +
  annotate("label", x = -6.75, y = 7, label = "X", fontface = 2, fill = "#CC79A7",
           alpha = 0.5) +
  ## Klammern Y
  annotate("segment", x = c(-5.25, -1.5), y = -10, xend = c(-5.25, -1.5), yend = 10, 
           color = "#CC79A7", linewidth = 1) +
  annotate("segment", x = c(-5.25, -1.5), y = -10, xend = c(-5, -1.75), yend = -10, 
           color = "#CC79A7", linewidth = 1) +
  annotate("segment", x = c(-5.25, -1.5), y = 10, xend = c(-5, -1.75), yend = 10, 
           color = "#CC79A7", linewidth = 1) +
  annotate("label", x = -1.3, y = 12, label = "Y", fontface = 2, fill = "#CC79A7",
           alpha = 0.5) +
  ## optional
  geom_curve(aes(x = -0.45, y = -3.5, xend = -0.45, yend = 2.75), linewidth = 1,
             curvature = -0.3, color = "#CC79A7") +
  geom_curve(aes(x = 3.05, y = -3.5, xend = 3.05, yend = 2.75), linewidth = 1,
             curvature = 0.3, color = "#CC79A7") +
  ## text
  annotate("text", x = -10, y = 10, label = expression(x[1]), size = 5, 
           fontface = 3, color = "#E69F00", hjust = "left") +
  geom_curve(aes(x = -9.5, y = 10, xend = -8.5, yend = 5),
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = -0.3, color = "gray50") +
  annotate("text", x = -8.5, y = -10, label = expression(x[1]~"+"~cdots~"+"~x[p]), size = 5, 
           fontface = 3, color = "#E69F00", hjust = "left") +
  geom_curve(aes(x = -8.6, y = -9.8, xend = -9.5, yend = -5),
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = -0.3, color = "gray50") +
  annotate("text", x = 3, y = 7.5, label = "optional", size = 5, 
           fontface = 3, color = "#E69F00", hjust = "left") +
  geom_curve(aes(x = 2.9, y = 7.5, xend = 1.2, yend = 2.6),
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = 0.3, color = "gray50") +
  annotate("text", x = 0, y = -10, label = "Verteilungsfamilie", size = 5, 
           fontface = 3, color = "#E69F00", hjust = "left") +
  geom_curve(aes(x = -0.1, y = -10.3, xend = -3.5, yend = -10),
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = -0.2, color = "gray50") +
  theme_void()