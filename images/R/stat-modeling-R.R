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

p_lhs_rhs_detail <- ggplot() +
  theme_void() +
  annotate("text", 0, 0, label = "~", size = 10, fontface = 2, hjust = "center") +
  annotate("text", 0, -2, label = "X erklärt Y", size = 5,  fontface = 3, color = "gray50") +
  annotate("text", -1.5, 0, label = "Y", size = 10, fontface = 2, hjust = "center") +
  annotate("text", 1.5, 0, label = "X", size = 10, fontface = 2, hjust = "center") +
  ## x seite
  annotate("label", 5, 4, label = expression(x[1]~"+"~x[2]~"+"~cdots), size = 6, 
           fontface = 2, hjust = "left") +
  annotate("label", x = 6.8, y = 5, label = "kontinuierlich", size = 3, 
           fontface = 2, fill = "#009E73", hjust = "left") + 
  annotate("label", 5, 0, label = expression(f[A]~"+"~f[B]~"+"~f[A]%*%f[B]), size = 6, 
           fontface = 2, hjust = "left") +
  annotate("label", x = 6.5, y = 1, label = "faktoriell / kategorial", size = 3, 
           fontface = 2, fill = "#009E73", hjust = "left") + 
  annotate("label", 5, -4, label = expression(x[1]~"+"~f[A]~"+"~cdots), size = 6, 
           fontface = 2, hjust = "left") +
  annotate("label", x = 7.3, y = -3, label = "kombiniert", size = 3, 
           fontface = 2, fill = "#009E73", hjust = "left") + 
  geom_curve(aes(x = 2, y = 1, xend = 4.75, yend = 4), linewidth = 0.75,
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = -0.3, color = "gray50") +
  geom_curve(aes(x = 2, y = 0, xend = 4.75, yend = 0), linewidth = 0.75,
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = 0, color = "gray50") +
  geom_curve(aes(x = 2, y = -1, xend = 4.75, yend = -4), linewidth = 0.75,
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = 0.3, color = "gray50") +
  ## y seite
  annotate("label", -10, c(-5, 0, 2.5, 5), 
           label = c("Logistic", "Ordinal", "Poisson", "Gaussian"), size = 6, 
           fontface = 2, hjust = "left") +
  annotate("label", x = -8.6, y = 5.9, label = "Kommazahlen", size = 3, 
           fontface = 2, fill = "#CC79A7", hjust = "left") + 
  annotate("label", x = -8.3, y = 3.4, label = "Zähldaten", size = 3, 
           fontface = 2, fill = "#CC79A7", hjust = "left") + 
  annotate("label", x = -9.1, y = 0.9, label = "Benotungen", size = 3, 
           fontface = 2, fill = "#CC79A7", hjust = "left") + 
  annotate("label", x = -8.2, y = -4.1, label = "0/1 Daten", size = 3, 
           fontface = 2, fill = "#CC79A7", hjust = "left") + 
  annotate("text", x = -8.3, y = -2, label = "...", size = 10, fontface = 2, hjust = "center") +  
  geom_curve(aes(x = -2, y = 1, xend = -6, yend = 5), linewidth = 0.75,
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = 0.3, color = "gray50") +
  geom_curve(aes(x = -2, y = 0.5, xend = -6.5, yend = 2.5), linewidth = 0.75,
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = 0.2, color = "gray50") +
  geom_curve(aes(x = -2, y = 0, xend = -6.75, yend = 0), linewidth = 0.75,
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = 0, color = "gray50") +
  geom_curve(aes(x = -2, y = -1, xend = -6.5, yend = -5), linewidth = 0.75,
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = -0.3, color = "gray50") +
  annotate("label", x = -3, y = 5.5, label = "LHS", size = 6, 
           fontface = 3, fill = "#E69F00", alpha = 0.5) +
  annotate("label", x = 3, y = 5.5, label = "RHS", size = 6, 
           fontface = 3, fill = "#E69F00", alpha = 0.5) +
  scale_x_continuous(limits = c(-10, 10)) +
  scale_y_continuous(limits = c(-6, 6)) +
  labs(title = "Eine schrecklich nette Familie",
       caption = "LHS = left hand side\nRHS = right hand side")  +
  theme(plot.caption = element_text(face = "italic"),
        plot.title = element_text(size = 16, face = "bold"))

p_lhs_rhs <- ggplot() +
  theme_void() +
  annotate("text", 0, 0, label = "~", size = 10, fontface = 2, hjust = "center") +
  annotate("text", -1.5, 0, label = "Y", size = 10, fontface = 2, hjust = "center") +
  annotate("text", 1.5, 0, label = "X", size = 10, fontface = 2, hjust = "center") +
  annotate("text", x = 4, y = -5, label = "Tilde", size = 6, 
           fontface = 3, color = "gray50") +
  annotate("label", x = -4, y = 3, label = "LHS", size = 6, 
           fontface = 3, fill = "#E69F00", alpha = 0.5) +
  annotate("label", x = 4, y = 3, label = "RHS", size = 6, 
           fontface = 3, fill = "#E69F00", alpha = 0.5) +
  geom_curve(aes(x = 3.1, y = -5.4, xend = 0, yend = -1),
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = -0.35, color = "gray50") +
  scale_x_continuous(limits = c(-10, 10)) +
  scale_y_continuous(limits = c(-6, 6)) +
  labs(title = "Modellschreibweise",
       caption = "LHS = left hand side\nRHS = right hand side")  +
  theme(plot.caption = element_text(face = "italic"),
        plot.title = element_text(size = 16, face = "bold"))

p_simple_model <- ggplot() +
  theme_void() +
  annotate("text", 0, 0, 
           label = expression(bold(Y)~"~"~bold(beta)[0]~"+"~bold(beta)[1]%.%bold(X[1])~"+"~bold(epsilon)), 
           size = 10, fontface = 2, hjust = "center") +
  annotate("text", x = 7, y = -7.5, label = "Residuen", size = 6, 
           fontface = 3, color = "gray50") +
  geom_curve(aes(x = 5.3, y = -7.75, xend = 4.5, yend = -2),
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = -0.6, color = "gray50") +
  annotate("text", x = -7.8, y = -9.5, label = "Koeffizienten", size = 6, 
           fontface = 3, color = "gray50") +
  geom_curve(aes(x = -5.5, y = -9.75, xend = -2.5, yend = -4),
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = 0.2, color = "gray50") +
  geom_curve(aes(x = -5.5, y = -9.75, xend = 0, yend = -3.5),
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = 0.2, color = "gray50") +
  annotate("label", x = -7, y = 7, label = "Messwert", size = 5, 
           fontface = 3, fill = "#E69F00", alpha = 0.5) +
  annotate("label", x = 7, y = 7, label = "Erklärende Variable(n)", size = 5, 
           fontface = 3, fill = "#E69F00", alpha = 0.5) +
  geom_curve(aes(x = -7, y =4.5, xend = -5, yend = 0.5),
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = 0.2, color = "#E69F00") +
  geom_curve(aes(x = 3.5, y = 7, xend = 2.2, yend = 3.5),
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = 0.2, color = "#E69F00") +
  annotate("label", x = 0.35, y = 5, label = "Steigung", size = 3, 
           fontface = 2, fill = "#009E73", alpha = 0.5) +  
  annotate("label", x = -2.1, y = 5, label = "Intercept", size = 3, 
           fontface = 2, fill = "#009E73", alpha = 0.5) +  
  scale_x_continuous(limits = c(-10, 10)) +
  scale_y_continuous(limits = c(-10, 10)) +
  labs(title = "Simples lineares Modell",
       caption = "Y = Messwert, Endpunkt, Outcome oder Response\nX = Erklärende Variable(n) oder Einflussvariable(n)")  +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.caption = element_text(face = "italic")) 

p_mult_model <- ggplot() +
  theme_void() +
  annotate("text", 0, 0, 
           label = expression(bold(Y)~"~"~bold(beta)[0]~"+"~bold(beta)[1]%.%bold(X[1])~"+"~cdots~"+"~bold(beta)[p]%.%bold(X[p])~"+"~bold(epsilon)), 
           size = 10, fontface = 2, hjust = "center") +
  annotate("text", x = 6, y = -7.5, label = "Residuen", size = 6, 
           fontface = 3, color = "gray50") +
  geom_curve(aes(x = 7.75, y = -7.75, xend = 8.25, yend = -2),
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = 0.6, color = "gray50") +
  annotate("text", x = -8, y = -9.5, label = "Koeffizienten", size = 6, 
           fontface = 3, color = "gray50") +
  geom_curve(aes(x = -5.5, y = -9.75, xend = -3.75, yend = -4),
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = 0.2, color = "gray50") +
  geom_curve(aes(x = -5.5, y = -9.75, xend = 3.2, yend = -1),
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = 0.1, color = "gray50") +
  geom_curve(aes(x = -5.5, y = -9.75, xend = -5.5, yend = -3),
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = 0.6, color = "gray50") +
  annotate("label", x = -8.5, y = 7, label = "Messwert", size = 5, 
           fontface = 3, fill = "#E69F00", alpha = 0.5) +
  annotate("label", x = 7, y = 7, label = "Erklärende Variable(n)", size = 5, 
           fontface = 3, fill = "#E69F00", alpha = 0.5) +
  geom_curve(aes(x = -9.5, y =4.5, xend = -8.5, yend = 0.5),
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = 0.2, color = "#E69F00") +
  geom_curve(aes(x = 3.3, y = 7, xend = -1.5, yend = 3.5),
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = 0.2, color = "#E69F00") +
  geom_curve(aes(x = 7, y = 4.5, xend = 6.3, yend = 1),
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = -0.3, color = "#E69F00") +
  scale_x_continuous(limits = c(-10, 10)) +
  scale_y_continuous(limits = c(-10, 10)) +
  labs(title = "Multiples lineares Modell",
       caption = "Y = Messwert, Endpunkt, Outcome oder Response\nX = Erklärende Variable(n) oder Einflussvariable(n)")  +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.caption = element_text(face = "italic")) 
p_2fac_model <- ggplot() +
  theme_void() +
  annotate("text", 0, 0, 
           label = expression(bold(Y)~"~"~bold(f[A])~"+"~bold(f[B])~"+"~bold(f[A]*":"~f[B])), 
           size = 10, fontface = 2, hjust = "center") +
  annotate("text", x = -6, y = -7, label = "Faktor A", size = 6, 
           fontface = 3, color = "gray50") +
  annotate("text", x = 3.5, y = -7, label = "Faktor B", size = 6, 
           fontface = 3, color = "gray50") +
  annotate("text", x = 8, y = 7.5, label = "Interaktionsterm", size = 6, 
           fontface = 3, color = "gray50") +
  geom_curve(aes(x = 1.8, y = -7, xend = 0, yend = -3),
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = -0.35, color = "gray50") +
  geom_curve(aes(x = -4.25, y = -7, xend = -2.5, yend = -3),
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = 0.35, color = "gray50") +
  geom_curve(aes(x = 5, y = 7.5, xend = 3.5, yend = 3),
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = 0.35, color = "gray50") +
  scale_x_continuous(limits = c(-10, 10)) +
  scale_y_continuous(limits = c(-10, 10)) +
  labs(title = "Zweifaktorielles Modell")  +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.caption = element_text(face = "italic")) 

p_1fac_model <- ggplot() +
  theme_void() +
  annotate("text", 0, 0, 
           label = expression(bold(Y)~"~"~bold(f[A])), 
           size = 10, fontface = 2, hjust = "center") +
  annotate("text", x = 3.5, y = -7, label = "Faktor A", size = 6, 
           fontface = 3, color = "gray50") +
  geom_curve(aes(x = 1.8, y = -7, xend = 1, yend = -3),
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = -0.35, color = "gray50") +
  scale_x_continuous(limits = c(-10, 10)) +
  scale_y_continuous(limits = c(-10, 10)) +
  labs(title = "Einfaktorielles Modell")  +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.caption = element_text(face = "italic")) 

p_mixed_model <- ggplot() +
  theme_void() +
  annotate("text", 0, 0, 
           label = expression(bold(Y)~"~"~bold(X[1])~"+"~cdots~"+"~bold(X[p])~"+"~bold((1*"|"*Z))), 
           size = 10, fontface = 2, hjust = "center") +
  annotate("label", x = -8.5, y = 7, label = "Messwert", size = 5, 
           fontface = 3, fill = "#E69F00", alpha = 0.5) +
  annotate("label", x = -1.3, y = -7, label = "Feste Effekte", size = 5, 
           fontface = 3, fill = "#E69F00", alpha = 0.5) +
  annotate("label", x = 8, y = 7, label = "Zufälliger Effekt", size = 5, 
           fontface = 3, fill = "#E69F00", alpha = 0.5) +
  geom_curve(aes(x = -8.5, y =4.5, xend = -6.7, yend = 0.5),
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = 0.2, color = "#E69F00") +
  geom_curve(aes(x = 8, y = 4.5, xend = 6.6, yend = 0.5),
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = -0.2, color = "#E69F00") +
  geom_curve(aes(x = -3.5, y = -7, xend = -4, yend = -2.5),
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = -0.2, color = "#E69F00") +
  geom_curve(aes(x = 0.8, y = -7, xend = 1.3, yend = -2.5),
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = 0.2, color = "#E69F00") +
  scale_x_continuous(limits = c(-10, 10)) +
  scale_y_continuous(limits = c(-10, 10)) +
  labs(title = "Gemischtes Modell",
       caption = "Y = Messwert oder Endpunkt\nX = Feste Effekt(e) oder fixed effects\nZ = Zufällige(r) Effekt(e) oder random effects")  +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.caption = element_text(face = "italic")) 
