p1kurt <- ggplot(data.frame(x = c(-3.25, 3.25)), aes(x)) +
  theme_minimal() +
  stat_function(fun = dnorm, linewidth = 1, args = list(mean = 0, sd = 1), 
                geom = "area", xlim = c(-4, 4), fill = "#E89F00", alpha = 0.5) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 0.4)) +
  stat_function(fun = dnorm, linewidth = 1, args = list(mean = 0, sd = 1), 
                xlim = c(-4, 4), color = "#E89F00") +
  geom_hline(yintercept = 0, gray = "gray50") +
  scale_x_continuous(breaks = 0, 
                     labels = "Mittelwert = Median = Modus",
                     guide = guide_axis(n.dodge = 2)) +
  labs(x = "", y = "") +
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 2),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 16, face = 2),
        axis.text.x = element_text(size = 12),        
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "top")

p2kurt <- ggplot(data.frame(x = c(-3.25, 3.25)), aes(x)) +
  theme_minimal() +
  stat_function(fun = dbeta, linewidth = 1, args = list(shape1 = 5, shape2 = 2), 
                xlim = c(0.15, 1), fill = "#E89F00", alpha = 0.5, geom = "area") +
  geom_segment(aes(x = 0.625, xend = 0.625, y = 0, yend = 1.71),
               linetype = 11) +
  geom_segment(aes(x = 0.8, xend = 0.8, y = 0, yend = 2.46)) +
  geom_segment(aes(x = 0.45, xend = 0.45, y = 0, yend = 0.68)) +
  stat_function(fun = dbeta, linewidth = 1, args = list(shape1 = 5, shape2 = 2), 
                xlim = c(0.15, 1), color = "#E89F00") +
  geom_hline(yintercept = 0, gray = "gray50") +
  labs(x = "", y = "", title = "Linksschief",
       subtitle = "Negative Schiefe") +
  scale_x_continuous(breaks = c(0.45, 0.625, 0.8), 
                     labels = c("Mittelwert", "Median", "Modus"),
                     guide = guide_axis(n.dodge = 2)) +
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 2),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 16, face = 2),
        axis.text.x = element_text(size = 12),        
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "top")

p3kurt <- ggplot(data.frame(x = c(-3.25, 3.25)), aes(x)) +
  theme_minimal() +
  stat_function(fun = dchisq, linewidth = 1, args = list(df = 4), 
                xlim = c(0, 16), fill = "#E89F00", alpha = 0.5, geom = "area") +
  geom_segment(aes(x = 2, xend = 2, y = 0, yend = 0.183)) +
  geom_segment(aes(x = 5, xend = 5, y = 0, yend = 0.102),
               linetype = 11) +
  geom_segment(aes(x = 10, xend = 10, y = 0, yend = 0.017)) +
  stat_function(fun = dchisq, linewidth = 1, args = list(df = 4), 
                xlim = c(0, 16), color = "#E89F00") +
  geom_hline(yintercept = 0, gray = "gray50") +
  scale_x_continuous(breaks = c(2, 5, 10), 
                     labels = c("Modus", "Median", "Mittelwert"),
                     guide = guide_axis(n.dodge = 2))  +
  labs(x = "", y = "", title = "Rechtsschief",
       subtitle = "Positive Schiefe") +
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 2),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 16, face = 2),
        axis.text.x = element_text(size = 12),        
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "top")


p0norm_dens <- data.frame(x = c(-3.25, 3.25)) |> 
  ggplot(aes(x)) +
  theme_void() +
  stat_function(fun = dnorm, 
                geom = "line", color = "black") +
  stat_function(fun = dnorm, 
                geom = "area", fill = "#F0E442", alpha = 0.8) +
  geom_vline(xintercept = 0, linewidth = 1) +
  labs(title = "Theoretische Normalverteilung",
       subtitle = "Densityplot und Boxplot.",
       caption = "") +
  theme(plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12))

p0norm_box <- tibble(y = rnorm(100000, 0, 1)) |> 
  ggplot(aes(x = 1, y)) +
  theme_void() +
  geom_boxplot(fill = "#F0E442", outliers = FALSE, alpha = 0.8) +
  coord_flip() +
  labs(caption = "Mittelwert und Median sind gleich.") +
  theme(plot.caption = element_text(size = 12))

norm_theo_tbl <- tibble(x = seq(-10, 10, 0.1),
                        x_1 = dnorm(x, -2, 2),
                        x_2 = dnorm(x, 1.75, 1.75)) |> 
  mutate(x_max = pmax(x_1, x_2))

p1theo <- norm_theo_tbl |> 
  ggplot(aes(x = x, y = x_max)) +
  theme_void() +
  geom_line() +
  geom_area(fill = "#E69F00", alpha = 0.8) + 
  geom_vline(xintercept = -0.125, linewidth = 1) +
  labs(title = "Zweigipflige Verteilung",
       subtitle = "Nicht normalverteilt") +
  theme(plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12)) 

p2theo <- data.frame(x = c(-6.25, 6.25)) |> 
  ggplot(aes(x)) +
  theme_void() +
  stat_function(fun = dt, args = list(df = 1.5),
                geom = "line", color = "black") + 
  stat_function(fun = dt, args = list(df = 1.5),
                geom = "area", fill = "#0072B2", alpha = 0.8) + 
  geom_vline(xintercept = 0, linewidth = 1) +
  labs(title = "Schmale Verteilung",
       subtitle = "Approximativ normalverteilt") +
  theme(plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12))

p3theo <- data.frame(x = c(0, 15.25)) |> 
  ggplot(aes(x)) +
  theme_void() +
  stat_function(fun = dchisq, args = list(df = 4),
                geom = "line", color = "black") + 
  stat_function(fun = dchisq, args = list(df = 4),
                geom = "area", fill = "#009E73", alpha = 0.8) + 
  geom_vline(xintercept = 2, linewidth = 1) +
  labs(title = "Linksschiefe Verteilung",
       subtitle = "Nicht normalverteilt") +
  theme(plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12))


set.seed(20250513)
norm_stat_tbl <- tibble(
  x_1 = rnorm(20, -3, 2),
  x_2 = rnorm(20, 3, 2)) |> 
  as_vector() |> 
  as_tibble()

shapiro_p <- shapiro.test(norm_stat_tbl$value)$p.value |> 
  scales::pvalue(add_p = TRUE)

p1sample <- norm_stat_tbl  |> 
  ggplot(aes(x = value)) +
  theme_void() +
  geom_density(trim = FALSE, fill = "#E69F00", alpha = 0.8) +
  annotate("label", -12, 0.09, label = shapiro_p, hjust = "left") +
  xlim(-12.5, 12.5) +
  geom_vline(xintercept = -0.6, linewidth = 1) +
  labs(title = "Zweigipflige Verteilung",
       subtitle = "Nicht normalverteilt", 
       caption = "p > 0.05; normalverteilt") +
  theme(plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12))

set.seed(2025051)
t_stat_tbl <- tibble(
  x_1 = rt(20, 3.5)) |> 
  as_vector() |> 
  as_tibble()

shapiro_p <- shapiro.test(t_stat_tbl$value)$p.value |> 
  scales::pvalue(add_p = TRUE)

p2sample <- t_stat_tbl |> 
  ggplot(aes(x = value)) +
  theme_void() +
  geom_density(trim = FALSE, fill = "#0072B2", alpha = 0.8) +
  xlim(-5.5, 10) +
  geom_vline(xintercept = -0.1, linewidth = 1) +
  annotate("label", -5.5, 0.32, label = shapiro_p, hjust = "left") +
  annotate("text", 4, 0.15, label = "Ohne", hjust = "center") +
  annotate("text", 4, 0.125, label = "p=0.235", hjust = "center") +
  geom_curve(x = 4, y = 0.11, xend = 6, yend = 0.05,
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = -0.2, color = "black", alpha = 0.3) +
  labs(title = "Schmale Verteilung",
       subtitle = "Approximativ normalverteilt", 
       caption = "p < 0.05; nicht normalverteilt") +
  theme(plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12))

set.seed(2025085) #2025051
chi_stat_tbl <- tibble(
  x_1 = rchisq(20, 5)) |> 
  as_vector() |> 
  as_tibble()

shapiro_p <- shapiro.test(chi_stat_tbl$value)$p.value |> 
  scales::pvalue(add_p = TRUE)

p3sample <- chi_stat_tbl |> 
  ggplot(aes(x = value)) +
  theme_void() +
  geom_density(trim = FALSE, fill = "#009E73", alpha = 0.8) +
  xlim(-5.5, 17.5) +
  geom_vline(xintercept = 3, linewidth = 1) +
  annotate("label", -5.5, 0.105, label = shapiro_p, hjust = "left") +
  labs(title = "Linksschiefe Verteilung",
       subtitle = "Nicht normalverteilt", 
       caption = "p > 0.05; normalverteilt") +
  theme(plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12))

set.seed(20250513)
norm_stat_small_tbl <- tibble(
  y = c(rnorm(5,  3, 2), rnorm(20, 3, 2), rnorm(40, 3, 2)),
  x = c(gl(1, 5, labels = "A"), gl(1, 20, labels = "B"), gl(1, 40, labels = "C")))

p1_small <- norm_stat_small_tbl |>
  filter(x == "A") |> 
  ggplot(aes(x = y)) +
  theme_void() +
  ylim(NA, 2) +
  geom_histogram(fill = "#E69F00", alpha = 0.8) +  
  labs(title = "Normalverteilung",
       subtitle = "Kleine Fallzahl (n = 5)") +
  theme(plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12))

p2_small <- norm_stat_small_tbl |>
  filter(x == "A") |> 
  ggplot(aes(x = y)) +
  theme_void() +
  geom_density(trim = FALSE, fill = "#E69F00", alpha = 0.8) 

p3_small <- norm_stat_small_tbl |>
  filter(x == "A") |> 
  ggplot(aes(x = y)) +
  theme_void() +
  geom_boxplot(fill = "#E69F00", outliers = FALSE, alpha = 0.8) 

p4_small <- norm_stat_small_tbl |>
  filter(x == "B") |> 
  ggplot(aes(x = y)) +
  theme_void() +
  geom_histogram(fill = "#0072B2", alpha = 0.8) +  
  labs(title = "Normalverteilung",
       subtitle = "Moderate Fallzahl (n = 20)") +
  theme(plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12))

p5_small <- norm_stat_small_tbl |>
  filter(x == "B") |> 
  ggplot(aes(x = y)) +
  theme_void() +
  geom_density(trim = FALSE, fill = "#0072B2", alpha = 0.8) 

p6_small <- norm_stat_small_tbl |>
  filter(x == "B") |> 
  ggplot(aes(x = y)) +
  theme_void() +
  geom_boxplot(fill = "#0072B2", outliers = FALSE, alpha = 0.8)

p7_small <- norm_stat_small_tbl |>
  filter(x == "C") |> 
  ggplot(aes(x = y)) +
  theme_void() +
  geom_histogram(fill = "#009E73", alpha = 0.8) +  
  labs(title = "Normalverteilung",
       subtitle = "Große Fallzahl (n = 40)") +
  theme(plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12))

p8_small <- norm_stat_small_tbl |>
  filter(x == "C") |> 
  ggplot(aes(x = y)) +
  theme_void() +
  geom_density(trim = FALSE, fill = "#009E73", alpha = 0.8) 

p9_small <- norm_stat_small_tbl |>
  filter(x == "C") |> 
  ggplot(aes(x = y)) +
  theme_void() +
  geom_boxplot(fill = "#009E73", outliers = FALSE, alpha = 0.8) 
