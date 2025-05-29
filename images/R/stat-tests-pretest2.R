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
                geom = "area", fill = "#F5C710", alpha = 0.8) +
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
  geom_boxplot(fill = "#F5C710", outliers = TRUE, alpha = 0.8, 
               outlier.shape = NA) +
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

p1_norm_small <- norm_stat_small_tbl |>
  filter(x == "A") |> 
  ggplot(aes(x = y)) +
  theme_void() +
  ylim(NA, 6) +
  geom_histogram(fill = "#E69F00", alpha = 0.8) +  
  labs(title = "Normalverteilung",
       subtitle = "Kleine Fallzahl (n = 5)") +
  theme(plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12))

p2_norm_small <- norm_stat_small_tbl |>
  filter(x == "A") |> 
  ggplot(aes(x = y)) +
  theme_void() +
  geom_density(trim = FALSE, fill = "#E69F00", alpha = 0.8) 

p3_norm_small <- norm_stat_small_tbl |>
  filter(x == "A") |> 
  ggplot(aes(x = y)) +
  theme_void() +
  geom_boxplot(fill = "#E69F00", outlier.shape = NA, alpha = 0.8) 

p4_norm_small <- norm_stat_small_tbl |>
  filter(x == "A") |> 
  ggplot(aes(x = x, y = y)) +
  theme_void() +
  geom_violindot(alpha = 0.8, size_dots = 4, fill = "#E69F00") +
  coord_flip() +
  theme(legend.position = "none")  

p5_norm_small <- norm_stat_small_tbl |>
  filter(x == "B") |> 
  ggplot(aes(x = y)) +
  theme_void() +
  ylim(NA, 6) +
  geom_histogram(fill = "#0072B2", alpha = 0.8) +  
  labs(title = "Normalverteilung",
       subtitle = "Moderate Fallzahl (n = 20)") +
  theme(plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12))

p6_norm_small <- norm_stat_small_tbl |>
  filter(x == "B") |> 
  ggplot(aes(x = y)) +
  theme_void() +
  geom_density(trim = FALSE, fill = "#0072B2", alpha = 0.8) 

p7_norm_small <- norm_stat_small_tbl |>
  filter(x == "B") |> 
  ggplot(aes(x = y)) +
  theme_void() +
  geom_boxplot(fill = "#0072B2", outlier.shape = NA, alpha = 0.8)

p8_norm_small <- norm_stat_small_tbl |>
  filter(x == "B") |> 
  ggplot(aes(x = x, y = y)) +
  theme_void() +
  geom_violindot(alpha = 0.8, size_dots = 4, fill = "#0072B2") +
  coord_flip() +
  theme(legend.position = "none")  

p9_norm_small <- norm_stat_small_tbl |>
  filter(x == "C") |> 
  ggplot(aes(x = y)) +
  theme_void() +
  geom_histogram(fill = "#009E73", alpha = 0.8) +  
  labs(title = "Normalverteilung",
       subtitle = "Große Fallzahl (n = 40)") +
  theme(plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12))

p10_norm_small <- norm_stat_small_tbl |>
  filter(x == "C") |> 
  ggplot(aes(x = y)) +
  theme_void() +
  geom_density(trim = FALSE, fill = "#009E73", alpha = 0.8) 

p11_norm_small <- norm_stat_small_tbl |>
  filter(x == "C") |> 
  ggplot(aes(x = y)) +
  theme_void() +
  geom_boxplot(fill = "#009E73", outlier.shape = NA, alpha = 0.8) 

p12_norm_small <- norm_stat_small_tbl |>
  filter(x == "C") |> 
  ggplot(aes(x = x, y = y)) +
  theme_void() +
  geom_violindot(alpha = 0.8, size_dots = 4, fill = "#009E73") +
  coord_flip() +
  theme(legend.position = "none")  

set.seed(20250513)
var_stat_theo_tbl <- tibble(
  y = c(rnorm(1e6, 6, 2), rnorm(1e6,  3, 2)),
  x = c(gl(2, 1e6, labels = c("A", "B"))))

p0var_dens <- var_stat_theo_tbl |> 
  ggplot(aes(x = y, fill = x)) +
  theme_void() +
  geom_density(trim = FALSE, alpha = 0.8)  +
  geom_vline(xintercept = c(3, 6), linewidth = 1) +
  labs(title = "Theoretische Varianzhomogenität zwischen zwei Gruppen",
       subtitle = "Densityplot und Boxplot.",
       caption = "") +
  theme(legend.position = "none",
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12)) +
  scale_fill_okabeito(order = c(4,8))

p0var_box <- var_stat_theo_tbl |> 
  ggplot(aes(x = y, fill = x)) +
  theme_void() +
  geom_boxplot(outliers = TRUE, alpha = 0.8, outlier.shape = NA) +
  labs(caption = "IQR und Whiskers sind gleich.") +
  theme(legend.position = "none",
        plot.caption = element_text(size = 12))  +
  scale_fill_okabeito(order = c(4,8))

set.seed(20250513)
var_stat_small_tbl <- tibble(
  y = c(rnorm(5,  3, 2), rnorm(5,  3, 2),
        rnorm(20, 3, 2), rnorm(20, 3, 2),
        rnorm(40, 3, 2), rnorm(40, 3, 2)),
  g = c(gl(1, 10, labels = "A"), 
        gl(1, 40, labels = "B"), 
        gl(1, 80, labels = "C")),
  x = c(gl(2, 5, labels = c("A", "B")), 
        gl(2, 20, labels = c("A", "B")), 
        gl(2, 40, labels = c("A", "B"))))

p1_var_small <- var_stat_small_tbl |>
  filter(g == "A") |> 
  ggplot(aes(x = y, fill = x)) +
  theme_void() +
  ylim(NA, 4) +
  geom_histogram(alpha = 0.8,
                 position = position_dodge()) +  
  labs(title = "Varianzhomogenität",
       subtitle = "Kleine Fallzahl (n = 5)") +
  theme(legend.position = "none",
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12)) +
  scale_fill_okabeito(order = c(1,8))

p2_var_small <- var_stat_small_tbl |>
  filter(g == "A") |> 
  ggplot(aes(x = y, fill = x)) +
  theme_void() +
  geom_density(trim = FALSE, alpha = 0.8)  +
  theme(legend.position = "none") +
  scale_fill_okabeito(order = c(1,8))

p3_var_small <- var_stat_small_tbl |>
  filter(g == "A") |> 
  ggplot(aes(x = y, fill = x)) +
  theme_void() +
  geom_boxplot(outlier.shape = NA, alpha = 0.8) +
  theme(legend.position = "none") +
  scale_fill_okabeito(order = c(1,8)) 

p4_var_small <- var_stat_small_tbl |>
  filter(g == "A") |> 
  ggplot(aes(x = x, y = y, fill = x)) +
  theme_void() +
  geom_violindot(alpha = 0.8, size_dots = 4) +
  coord_flip() +
  theme(legend.position = "none") +
  scale_fill_okabeito(order = c(1,8)) 

##

p5_var_small <- var_stat_small_tbl |>
  filter(g == "B") |> 
  ggplot(aes(x = y, fill = x)) +
  theme_void() +
  ylim(NA, 4) +
  geom_histogram(alpha = 0.8,
                 position = position_dodge()) +  
  labs(title = "Varianzhomogenität",
       subtitle = "Moderate Fallzahl (n = 20)") +
  theme(legend.position = "none",
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12)) +
  scale_fill_okabeito(order = c(5,8))

p6_var_small <- var_stat_small_tbl |>
  filter(g == "B") |> 
  ggplot(aes(x = y, fill = x)) +
  theme_void() +
  geom_density(trim = FALSE, alpha = 0.8)  +
  theme(legend.position = "none") +
  scale_fill_okabeito(order = c(5,8))

p7_var_small <- var_stat_small_tbl |>
  filter(g == "B") |> 
  ggplot(aes(x = y, fill = x)) +
  theme_void() +
  geom_boxplot(outlier.shape = NA, alpha = 0.8) +
  theme(legend.position = "none") +
  scale_fill_okabeito(order = c(5,8)) 

p8_var_small <- var_stat_small_tbl |>
  filter(g == "B") |> 
  ggplot(aes(x = x, y = y, fill = x)) +
  theme_void() +
  geom_violindot(alpha = 0.8, size_dots = 4) +
  coord_flip() +
  theme(legend.position = "none") +
  scale_fill_okabeito(order = c(5,8)) 

##

p9_var_small <- var_stat_small_tbl |>
  filter(g == "C") |> 
  ggplot(aes(x = y, fill = x)) +
  theme_void() +
  geom_histogram(alpha = 0.8,
                 position = position_dodge()) +  
  labs(title = "Varianzhomogenität",
       subtitle = "Große Fallzahl (n = 40)") +
  theme(legend.position = "none",
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12)) +
  scale_fill_okabeito(order = c(3,8))

p10_var_small <- var_stat_small_tbl |>
  filter(g == "C") |> 
  ggplot(aes(x = y, fill = x)) +
  theme_void() +
  geom_density(trim = FALSE, alpha = 0.8)  +
  theme(legend.position = "none") +
  scale_fill_okabeito(order = c(3,8))

p11_var_small <- var_stat_small_tbl |>
  filter(g == "C") |> 
  ggplot(aes(x = y, fill = x)) +
  theme_void() +
  geom_boxplot(outlier.shape = NA, alpha = 0.8) +
  theme(legend.position = "none") +
  scale_fill_okabeito(order = c(3,8)) 

p12_var_small <- var_stat_small_tbl |>
  filter(g == "B") |> 
  ggplot(aes(x = x, y = y, fill = x)) +
  theme_void() +
  geom_violindot(alpha = 0.8, size_dots = 4) +
  coord_flip() +
  theme(legend.position = "none") +
  scale_fill_okabeito(order = c(3,8)) 

set.seed(20250513)
var_stat_theo_control_tbl <- tibble(
  y = c(rnorm(10, 2, 1), 
        rnorm(10, 10, 5),
        rnorm(10, 4, 3),
        rnorm(10, 8, 4),
        rnorm(10, 6, 3)),
  x = c(gl(5, 10, labels = c("C-", "C+", "A", "B", "C"))))

p1_var_theo_example <- ggplot(var_stat_theo_control_tbl, aes(x, y, fill = x)) +
  theme_minimal() +
  geom_boxplot(outlier.shape = NA) +
  labs(title = "Kontrollen vs. Behandlung",
       subtitle = "Varianzheterogenität durch Kontrollen\nnegative/positive Infektion (C-/C+)",
       x = "Behandlung", y = "Anzahl Pathogen") +
  scale_fill_okabeito() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 16, face = 2),
        axis.title.y = element_text(size = 16, face = 2),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"))

set.seed(20250513)
var_stat_theo_time_tbl <- tibble(
  y = c(rnorm(10, 2, 1), 
        rnorm(10, 5, 2.5),
        rnorm(10, 10, 5),
        rnorm(10, 20, 12),
        rnorm(10, 40, 22)),
  x = c(gl(5, 10, labels = c("t0", "t1", "t2", "t3", "t4"))))

p2_var_theo_example <- ggplot(var_stat_theo_time_tbl, aes(x, y, fill = x)) +
  theme_minimal() +
  geom_boxplot(outlier.shape = NA) +
  labs(title = "Messungen über die Zeit",
       subtitle = "Varianzheterogenität durch zeitlichen\nVerlauf der Messungen",
       x = "Messzeitpunkt", y = "Frischgewicht") +
  scale_fill_okabeito() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 16, face = 2),
        axis.title.y = element_text(size = 16, face = 2),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"))

set.seed(20250513)
var_stat_theo_many_tbl <- tibble(
  y = c(rnorm(5, 3, 2), 
        rnorm(5, 5, 2),
        rnorm(5, 7, 2),
        rnorm(5, 3, 2),
        rnorm(5, 10, 2),
        rnorm(5, 8, 2),
        rnorm(5, 4, 2)),
  x = c(gl(7, 5, labels = LETTERS[1:7])))

p3_var_theo_example <- ggplot(var_stat_theo_many_tbl, aes(x, y, fill = x)) +
  theme_minimal() +
  geom_boxplot(outlier.shape = NA) +
  labs(title = "Viele Behandlunggruppen",
       subtitle = "Theoretische Varianzhomogenität\nin den Gruppen (n = 5)",
       x = "Behandlung", y = "Messwert") +
  scale_fill_okabeito() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 16, face = 2),
        axis.title.y = element_text(size = 16, face = 2),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"))


set.seed(20250513)
var_stat_theo_wirk_tbl <- tibble(
  y = c(rnorm(10, 20, 1), 
        rnorm(10, 12, 3),
        rnorm(10, 8, 6)),
  x = c(gl(3, 10, labels = c("Boden / Wurzel", "Boden / Blatt", "Boden / Spross"))))

p4_var_theo_example <- ggplot(var_stat_theo_wirk_tbl, aes(x, y, fill = x)) +
  theme_minimal() +
  geom_boxplot(outlier.shape = NA) +
  labs(title = "Wirkmechanismus",
       subtitle = "Varianzheterogenität durch räumliche\nTrennung des Wirkorts",
       x = "Quelle / Verstoffwechselung", y = "Al3+ / Organische Säure") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_fill_okabeito() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 16, face = 2),
        axis.title.y = element_text(size = 16, face = 2),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"))

p1_var_hetero <-  var_hetero_sim_tbl |> 
  ggplot(aes(n, rate, 
             shape = alpha)) +
  theme_minimal() +
  geom_smooth(data = filter(var_hetero_sim_tbl, type == "Bartlett"), 
              aes(color = type, linetype = alpha),
              method = "loess", se = FALSE) +
  geom_smooth(data = filter(var_hetero_sim_tbl, type == "Levene"), 
              aes(color = type, linetype = alpha),
              method = "lm", se = FALSE) +
    geom_smooth(data = filter(var_hetero_sim_tbl, type == "Fligner"), 
                aes(color = type, linetype = alpha),
                method = "lm", se = FALSE) +
  geom_point(aes(fill = type), show.legend = FALSE) +
  scale_shape_manual(values = c(21, 22, 24)) +
  labs(x = "Fallzahl in der Gruppe", y = "Erkannte Varianzheterogenität",
       title = "Varianzheterogene Gruppen", 
       subtitle = "Erkennung mit einem statistischen Test",
       caption = expression(n[sim]~"="~1000),
       color = "", linetype = expression(alpha), 
       shape = expression(alpha),
       fill = "") +
  scale_y_continuous(breaks = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1),
                     labels = scales::percent_format(accuracy = 1),
                     limits = c(0,1)) +
  scale_x_continuous(breaks = c(3:12)) +
  scale_color_okabeito() +
  scale_fill_okabeito() +  
  theme(axis.title.y = element_text(size = 14, face = 2),
        axis.text.y = element_text(size = 12), 
        axis.title.x = element_text(size = 14, face = 2),
        axis.text.x = element_text(size = 12),        
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 2),
        legend.position = "top",
        legend.key.width = unit(7, 'mm')) +
    guides(linetype = guide_legend(override.aes = list(color = "black"),
                                   nrow = 2),
           shape = guide_legend(override.aes = list(size = 3),
                                nrow = 2),
           color = guide_legend(nrow = 2))
  
p2_var_homo <- 
  var_homo_sim_tbl |> 
  ggplot(aes(n, rate, 
             shape = alpha)) +
  theme_minimal() +
  geom_point(aes(fill = type), show.legend = FALSE) +
  scale_shape_manual(values = c(21, 22, 24)) +
  geom_smooth(aes(color = type, linetype = alpha), method = "lm", se = FALSE) +
  labs(x = "Fallzahl in der Gruppe", y = "Erkannte Varianzhomogenität",
       title = "Varianzhomogene Gruppen", 
       subtitle = "Erkennung mit einem statistischen Test",
       caption = expression(n[sim]~"="~1000),
       linetype = expression(alpha), 
       shape = expression(alpha),
       color = "") +
  scale_y_continuous(breaks = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1),
                     labels = scales::percent_format(accuracy = 1),
                     limits = c(0,1)) +
  scale_x_continuous(breaks = c(3:12)) +
    scale_color_okabeito() +
    scale_fill_okabeito() +  
  theme(axis.title.y = element_text(size = 14, face = 2),
        axis.text.y = element_text(size = 12), 
        axis.title.x = element_text(size = 14, face = 2),
        axis.text.x = element_text(size = 12),        
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 2),
        legend.position = "top") +
    guides(linetype = guide_legend(override.aes = list(color = "black"),
                                   nrow = 2),
           shape = guide_legend(override.aes = list(size = 3),
                                nrow = 2),
           color = guide_legend(nrow = 2))

p1_nonnormal_sim <- nonnormal_sim_tbl |> 
  ggplot(aes(n, rate, color = type)) +
  theme_minimal() +
  geom_smooth(method = "lm", se = FALSE,
              formula = y ~ log(x)) +
  geom_point(show.legend = FALSE) +
  labs(x = "Fallzahl in der Gruppe", y = "Erkannte Nichtnormalverteilung",
       title = "Nicht normalverteiler Messwert", 
       subtitle = "Erkennung mit einem statistischen Test",
       caption = expression(n[sim]~"="~1000),
       color = "") +
  scale_y_continuous(breaks = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1),
                     limits = c(-0.05,1.05),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = c(3:12)) +
  scale_color_okabeito(order = c(1,3,4,5,7)) +
  theme(axis.title.y = element_text(size = 14, face = 2),
        axis.text.y = element_text(size = 12), 
        axis.title.x = element_text(size = 14, face = 2),
        axis.text.x = element_text(size = 12),        
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "top") + 
  guides(color = guide_legend(nrow = 2)) 

p2_normal_sim <- normal_sim_tbl |> 
  ggplot(aes(n, rate, color = type)) +
  theme_minimal() +
  geom_smooth(method = "lm", se = FALSE,
              formula = y ~ log(x)) +
  geom_point(show.legend = FALSE) +
  labs(x = "Fallzahl in der Gruppe", y = "Erkannte Normalverteilung",
       title = "Normalverteiler Messwert", 
       subtitle = "Erkennung mit einem statistischen Test",
       caption = expression(n[sim]~"="~1000),
       color = "") +
  scale_y_continuous(breaks = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1),
                     limits = c(-0.05,1.05),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = c(3:12)) +
  scale_color_okabeito(order = c(1,3,4,5,7)) +
  theme(axis.title.y = element_text(size = 14, face = 2),
        axis.text.y = element_text(size = 12), 
        axis.title.x = element_text(size = 14, face = 2),
        axis.text.x = element_text(size = 12),        
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "top") + 
  guides(color = guide_legend(nrow = 2)) 


