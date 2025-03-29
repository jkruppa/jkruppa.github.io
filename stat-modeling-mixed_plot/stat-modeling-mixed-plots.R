set.seed(20250336)

p1 <- tibble(x = c(2, 3, 5, 7, 8,
                   1.5, 2, 4, 5, 7,
                   0.5, 1, 3, 4, 6.5),
             y = c(c(2, 3, 5, 7, 8)*1.2+5+rnorm(5, 0, 0.5),
                   c(1.5, 2, 4, 5, 7)*0.25+5+rnorm(5, 0, 0.5),
                   c(0.5, 1, 3, 4, 6.5)*-1.05+5+rnorm(5, 0, 0.5)),
             id = as_factor(rep(1:3, each = 5))) |> 
  ggplot(aes(x, y, color = id)) +
  theme_void() +
  geom_point() +
  stat_smooth(geom = "line", method = "lm", fullrange = TRUE, alpha = 0.5) +
  scale_x_continuous(limits = c(0, 8.5)) +
  scale_y_continuous(limits = c(-5, 15)) +
  scale_color_okabeito() +
  labs(title = "Fixed intercept, random slope",
       subtitle = "y ~ x + (0 + x | id)") +
  annotate("text", x = 8.5, y = c(14, 6, -3), size = 5,
           label = c(expression(beta[1]), expression(beta[2]), expression(beta[3])),
           color = c("#E69F00", "#56B4E9", "#009E73")) +
  annotate("text", x = 0, y = 5, size = 5, hjust = "right",
           label = c(expression(alpha))) +
  geom_hline(yintercept = -5) +
  geom_vline(xintercept = 0.02) +  
  theme(plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none")

p2 <- tibble(x = c(2, 3, 5, 7, 8,
                   1.5, 2, 4, 5, 7,
                   0.5, 1, 3, 4, 6.5),
             y = c(c(2, 3, 5, 7, 8)*-1.05+12+rnorm(5, 0, 0.5),
                   c(1.5, 2, 4, 5, 7)*-1.05+8+rnorm(5, 0, 0.5),
                   c(0.5, 1, 3, 4, 6.5)*-1.05+4+rnorm(5, 0, 0.5)),
             id = as_factor(rep(1:3, each = 5))) |> 
  ggplot(aes(x, y, color = id)) +
  theme_void() +
  geom_point() +
  stat_smooth(geom = "line", method = "lm", fullrange = TRUE, alpha = 0.5) +
  scale_x_continuous(limits = c(0, 8.5)) +
  scale_y_continuous(limits = c(-5, 15)) +
  scale_color_okabeito() +
  labs(title = "Random intercept, fixed slope",
       subtitle = "y ~ x + (1 | id)") +
  annotate("text", x = 0, y = c(12, 8, 4), size = 5, hjust = "right",
           label = c(expression(alpha[1]), expression(alpha[2]), expression(alpha[3])),
           color = c("#E69F00", "#56B4E9", "#009E73")) +
  annotate("text", x = 8.5, y = 0, size = 5, hjust = "right",
           label = c(expression(beta))) +
  geom_hline(yintercept = -5) +
  geom_vline(xintercept = 0.02) +  
  theme(plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none")

set.seed(20250336)
p3 <- tibble(x = c(2, 3, 5, 7, 8,
                   1.5, 2, 4, 5, 7,
                   0.5, 1, 3, 4, 6.5),
             y = c(c(2, 3, 5, 7, 8)*-1.9+12+rnorm(5, 0, 0.5),
                   c(1.5, 2, 4, 5, 7)*0.1+8+rnorm(5, 0, 0.5),
                   c(0.5, 1, 3, 4, 6.5)*1.15+4+rnorm(5, 0, 0.5)),
             id = as_factor(rep(1:3, each = 5))) |> 
  ggplot(aes(x, y, color = id)) +
  theme_void() +
  geom_point() +
  stat_smooth(geom = "line", method = "lm", fullrange = TRUE, alpha = 0.5) +
  scale_x_continuous(limits = c(0, 8.5)) +
  scale_y_continuous(limits = c(-5, 15)) +
  scale_color_okabeito() +
  labs(title = "Random intercept, random slope",
       subtitle = "y ~ x + (x | id)") +
  annotate("text", x = 0, y = c(12, 8, 4), size = 5, hjust = "right",
           label = c(expression(alpha[1]), expression(alpha[2]), expression(alpha[3])),
           color = c("#E69F00", "#56B4E9", "#009E73")) +
  annotate("text", x = 8.5, y = c(-3, 7, 12), size = 5, hjust = "right",
           label = c(expression(beta["1"]), expression(beta["2"]), expression(beta["3"])),
           color = c("#E69F00", "#56B4E9", "#009E73")) +
  geom_hline(yintercept = -5) +
  geom_vline(xintercept = 0.02) +  
  theme(plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none")


set.seed(20250336)
p4 <- tibble(x = c(2, 3, 5, 7, 8,
                   1.5, 2, 4, 5, 7,
                   0.5, 1, 3, 4, 6.5),
             y = c(c(2, 3, 5, 7, 8)*-1.9+12+rnorm(5, 0, 0.5),
                   c(1.5, 2, 4, 5, 7)*0.1+8+rnorm(5, 0, 0.5),
                   c(0.5, 1, 3, 4, 6.5)*1.15+4+rnorm(5, 0, 0.5)),
             id = as_factor(rep(1:3, each = 5))) |> 
  ggplot(aes(x, y)) +
  theme_void() +
  geom_point() +
  stat_smooth(geom = "line", method = "lm", fullrange = TRUE, alpha = 0.5) +
  scale_x_continuous(limits = c(0, 8.5)) +
  scale_y_continuous(limits = c(-5, 15)) +
  scale_color_okabeito() +
  labs(title = "Fixed intercept, fixed slope",
       subtitle = "y ~ x") +
  annotate("text", x = 0, y = 8.5, size = 5, hjust = "right",
           label = c(expression(alpha))) +
  annotate("text", x = 8.5, y = 4.5, size = 5, hjust = "right",
           label = c(expression(beta))) +
  geom_hline(yintercept = -5) +
  geom_vline(xintercept = 0.02) +  
  theme(plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none")