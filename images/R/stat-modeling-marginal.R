a_line <- \(x) x^3 + -8*x^2 + 10*x + 10
a_slope <- \(x) 10 - 16*x + 3*x^2

find_intercept <- function(x1, y1, slope) {
  intercept <- slope * (-x1) + y1
  return(intercept)
}

slope_annotations <- tibble(x = c(-1, 2.5, 6)) |> 
  mutate(y = a_line(x),
         slope = a_slope(x),
         intercept = find_intercept(x, y, slope),
         nice_label = glue("x: {x}; y: {y}<br>",
                           "Steigung (dy/dx): **{slope}**"))

p1_intro_00_1 <- enzyme_tbl|> 
  ggplot(aes(x, y)) +
  geom_point(color = "gray50", alpha = 0.5) +
  theme_marginal() +
  geom_function(fun = a_line, linewidth = 1, color = cb_pal[2]) +
  geom_richtext(aes(x = 4.25, y = 50, 
                    label = "f(x) = x³ - 8x² + 10x + 10<br>f'(x) = 3x² -16x +10")) +
  scale_x_continuous(breaks = c(-1, 2.5, 6), limits = c(-2, 8)) +
  scale_y_continuous(breaks = c(-50, 0, 50, 100), limits = c(-55, 75)) +
  geom_abline(data = slope_annotations,
              aes(slope = slope, intercept = intercept),
              linewidth = 0.5, linetype = "21", color = cb_pal[4]) +
  geom_point(data = slope_annotations, aes(x = x, y = y),
             shape = 23, fill = "#009E73", size = 3) +
  labs(x = "Korrigierter pH-Wert (X)", y = "Standardisierte Enzymaktivität (Y)",
       title = "Steigung (eng. slope)", subtitle = "Wenn X sich ändert, wie ändert sich dann Y?") 

p2_intro_00_2 <- enzyme_tbl|>
  ggplot(aes(x, y)) +
  geom_point(color = "gray50", alpha = 0.5) +
  theme_marginal() +
  annotate("segment", x = -1, y = -55, xend = -1, yend = -9, color = "#CC79A7",
           linetype = 21, linewidth = 0.5) +
  annotate("segment", x = 2.5, y = -55, xend = 2.5, yend = 0.625, color = "#CC79A7",
           linetype = 21, linewidth = 0.5) +
  annotate("segment", x = 6, y = -55, xend = 6, yend = -2, color = "#CC79A7",
           linetype = 21, linewidth = 0.5) +
  geom_function(fun = a_line, linewidth = 1, color = "#56B4E9") +
  geom_point(data = slope_annotations, aes(x = x, y = y),
             shape = 23, fill = "#CC79A7", size = 3) +
  scale_x_continuous(breaks = c(-1, 2.5, 6), limits = c(-2, 8)) +
  scale_y_continuous(breaks = c(-50, 0, 50, 100), limits = c(-55, 75)) +
  labs(x = "Korrigierter pH-Wert (X)", y = "Standardisierte Enzymaktivität (Y)",
       title = "Vorhersage (eng. prediction)", subtitle = "Welche Werte für Y sagt das Modell für X vorraus?") +
  theme(panel.grid.major.x = element_blank())

## -----------------------------------------------------------------------------

a_line <- function(x) (2 * x) - 1

slope_annotations <- tibble(x = c(-0.25, 1.2, 2.4)) |> 
  mutate(y = a_line(x)) |> 
  mutate(nice_y = y + 1) |> 
  mutate(nice_label = glue("x: {x}; y: {y}<br>",
                           "Steigung (dy/dx): **{2}**"))

p1_intro_01 <- ggplot() +
  theme_marginal() +
  geom_vline(xintercept = 0, linewidth = 0.5, color = "grey50") +
  geom_hline(yintercept = 0, linewidth = 0.5, color = "grey50") +
  geom_function(fun = a_line, linewidth = 1, color = cb_pal[2]) +
  geom_richtext(aes(x = 1, y = 4.25, label = "f(x) = 2x - 1<br>f'(x) = 2")) +
  scale_x_continuous(breaks = -2:5, limits = c(-1, 3)) +
  scale_y_continuous(breaks = -3:9) +
  labs(x = "x", y = "y") 

p2_intro_01 <- ggplot() +
  theme_marginal() +
  geom_vline(xintercept = 0, linewidth = 0.5, color = "grey50") +
  geom_hline(yintercept = 0, linewidth = 0.5, color = "grey50") +
  geom_function(fun = a_line, linewidth = 1, color = cb_pal[2]) +
  geom_point(data = slope_annotations, aes(x = x, y = y)) +
  geom_richtext(data = slope_annotations, 
                aes(x = x, y = y, label = nice_label),
                nudge_y = 0.55) +
  scale_x_continuous(breaks = -2:5, limits = c(-1, 3)) +
  scale_y_continuous(breaks = -3:9) +
  labs(x = "x", y = "y") 

## -----------------------------------------------------------------------------

u_cookies <- function(x) (-0.5 * x^2) + (5 * x)
u_slope <- function(x) -x + 5

slope_annotations <- tibble(x = c(1, 4, 10)) |> 
  mutate(y = u_cookies(x),
         slope = u_slope(x),
         intercept = find_intercept(x, y, slope),
         nice_label = glue("x: {x}; y: {y}<br>",
                           "Steigung (dy/dx): **{slope}**"))


p1_intro_02 <- ggplot() +
  theme_marginal() +
  geom_vline(xintercept = 0, linewidth = 0.5, color = "grey50") +
  geom_hline(yintercept = 0, linewidth = 0.5, color = "grey50") +
  geom_function(fun = u_cookies, linewidth = 1, color = cb_pal[3],
                xlim = c(0, 12)) +
  geom_richtext(aes(x = 10, y = 12, label = "f(x) = -0.5x² + 5x<br>f'(x) = -x + 5")) +
  scale_x_continuous(breaks = seq(0, 12, 2), limits = c(-1.5, 12.5)) +
  coord_cartesian(ylim = c(-10, 15))

p2_intro_02 <- ggplot() +
  theme_marginal() +
  geom_vline(xintercept = 0, linewidth = 0.5, color = "grey50") +
  geom_hline(yintercept = 0, linewidth = 0.5, color = "grey50") +
  geom_function(fun = u_cookies, linewidth = 1, color = cb_pal[3],
                xlim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 12, 2), limits = c(-1.5, 12.5)) +
  geom_abline(data = slope_annotations,
              aes(slope = slope, intercept = intercept),
              linewidth = 0.5, linetype = "21", color = cb_pal[4]) +
  geom_point(data = slope_annotations, aes(x = x, y = y),
             size = 3) +
  geom_richtext(data = slope_annotations, aes(x = x, y = y, label = nice_label),
                nudge_y = 2) +
  coord_cartesian(ylim = c(-10, 15))
## -----------------------------------------------------------------------------

p1_intro_00 <- ggplot(modell_line_tbl, aes(x, y)) +
  theme_marginal() +
  geom_point2() +
  theme(axis.text = element_blank()) +
  geom_function(fun = \(x) 1.5 + 0.75 * x, color = "#E69F00", linewidth = 1) 

p2_intro_00 <- ggplot(modell_square_tbl, aes(x, y)) +
  theme_marginal() +
  geom_point2() +
  theme(axis.text = element_blank()) +
  geom_function(fun = \(x) 1.5 + 0.75 * -x^2, color = "#56B4E9", linewidth = 1)

## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------

