a_line <- \(x) x^3 + -8*x^2 + 10*x + 10
a_slope <- \(x) 10 - 16*x + 3*x^2

find_intercept <- function(x1, y1, slope) {
  intercept <- slope * (-x1) + y1
  return(intercept)
}

slope_annotations <- tibble(x = c(-1, 2, 6)) |> 
  mutate(y = a_line(x),
         slope = a_slope(x),
         intercept = find_intercept(x, y, slope),
         nice_label = glue("x: {x}; y: {y}<br>",
                           "Steigung (dy/dx): **{slope}**"))

enzyme_tbl <- read_excel("data/enzyme_kinetic.xlsx")


p1_intro_00_1 <- enzyme_tbl|> 
  ggplot(aes(ph, activity)) +
  geom_point(color = "gray50", alpha = 0.5) +
  theme_marginal() +
  geom_function(fun = a_line, linewidth = 1, color = cb_pal[2]) +
#  geom_richtext(aes(x = 4.25, y = 50, 
#                    label = "y = x³ - 8x² + 10x + 10<br>y' = 3x² -16x +10")) +
  scale_x_continuous(breaks = c(-1, 2, 6), limits = c(-2, 8)) +
  scale_y_continuous(breaks = c(-50, 0, 50, 100), limits = c(-55, 75)) +
  geom_abline(data = slope_annotations,
              aes(slope = slope, intercept = intercept),
              linewidth = 0.5, linetype = "21", color = cb_pal[4]) +
  geom_point(data = slope_annotations, aes(x = x, y = y),
             shape = 23, fill = "#009E73", size = 3) +
  geom_label(data = slope_annotations, aes(x = x, y = y, label = round(slope, 1)),
             alpha = 0.5, fill = "#009E73",
             size = 3, position = position_nudge(x = c(0.5, 0.7, 0.5), y = -4)) +
  labs(x = "Standardisierter pH-Wert (X)", y = "Standardisierte Enzymaktivität (Y)",
       title = "Steigung", subtitle = "Wenn X sich ändert, wie ändert sich dann Y?") 

p2_intro_00_2 <- 
  enzyme_tbl|>
  ggplot(aes(ph, activity)) +
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
  geom_label(data = slope_annotations, aes(x = x, y = y, label = round(y, 1)),
             alpha = 0.5, fill = "#CC79A7",
             size = 3, position = position_nudge(x = c(0.5, 0.6, 0.5), y = -4)) +
  scale_x_continuous(breaks = c(-1, 2.5, 6), limits = c(-2, 8)) +
  scale_y_continuous(breaks = c(-50, 0, 50, 100), limits = c(-55, 75)) +
  labs(x = "Standardisierter pH-Wert (X)", y = "Standardisierte Enzymaktivität (Y)",
       title = "Vorhersage", subtitle = "Welche Werte für Y sagt das Modell für X vorraus?") +
#    annotate("text", x = 2.5, y = -35, label = expression(y[vorhergesagt]), size = 5,
#             color = "#CC79A7") +
#    geom_curve(x = 1.8, y = -35, xend = -0.9, yend = -10.5, color = "gray25",
#               arrow = arrow(length = unit(0.01, "npc"), type = "closed"),
#               curvature = -0.25, linewidth = 0.25) +
  theme(panel.grid.major.x = element_blank())


p3_intro_00_3 <- enzyme_tbl |> 
  mutate(grp = factor(grp, levels = c("niedrig", "mittel", "hoch"))) |> 
  ggplot(aes(grp, activity)) +
  theme_marginal() +
  geom_point(color = "gray50", alpha = 0.5) +
  stat_summary(fun.data=mean_sdl, mult=1, 
               geom="pointrange", shape = 23, fill = "#CC79A7", size = 0.75) +
  stat_summary(fun = mean, geom = "label", aes(label = round(..y..,2)),
               size = 3, position = position_nudge(x = .26, y = -1),
               fill = "#CC79A7", alpha = 0.5) +
  labs(x = "Gruppierter pH-Wert (A)", y = "Standardisierte Enzymaktivität (Y)",
       title = "Einfaktorielle Vorhersage", subtitle = "Was sind die Gruppenmittelwerte von Y für A?") 

p4_intro_00_4 <- enzyme_tbl |> 
  mutate(grp = factor(grp, levels = c("niedrig", "mittel", "hoch"))) |> 
  ggplot(aes(grp, activity, group = type)) +
  theme_marginal() +
  geom_point(color = "gray50", alpha = 0.5,
             position = position_dodge(0.5)) +
  stat_summary(fun.data=mean_sdl, mult=1, 
               geom="pointrange", shape = 23, 
               aes(fill = type), size = 0.75,
               position = position_dodge(0.5)) +
  stat_summary(fun = mean, geom = "label", aes(label = round(..y..,2), fill = type),
               size = 3, position = position_nudge(x = c(-.4, -.38, -.38, .38, .38, .38),
                                                   y = c(-1, 0, -1, -2, -1, -1)),
               alpha = 0.5, show.legend = FALSE) +
  scale_fill_okabeito() +
  labs(x = "Gruppierter pH-Wert (A)", y = "Standardisierte Enzymaktivität (Y)",
       fill = "Gruppe (B)",
       title = "Zweifaktorielle Vorhersage", subtitle = "Was sind die Gruppenmittelwerte von Y für B in A?") +
  theme(legend.position = "top",
        legend.title = element_text(size = 11, face = 2))

p1_slope_00 <- 
  enzyme_tbl|> 
  ggplot(aes(ph, activity)) +
  theme_marginal()  + 
  geom_point(color = "gray50", alpha = 0.5) +
  geom_function(fun = a_line, linewidth = 2, color = "black", alpha = 0.5) +
  geom_line(aes(y = predict(poly_fit), color = "poly()"), linewidth = 1) +
  labs(x = "Standardisierter pH-Wert (X)", y = "Standardisierte Enzymaktivität (Y)",
       title = "Modellierte Steigung", subtitle = "Wenn X sich ändert, wie ändert sich dann Y?",
       color = "Methode") +
  geom_abline(data = slope_annotations,
              aes(slope = slope, intercept = intercept),
              linewidth = 0.5, linetype = "21", color = "black", alpha = 0.5) +
  geom_abline(aes(slope = c(28.3), 
                  intercept = find_intercept(c(-1), c(-4.545), c(28.3)), 
                  color = "poly()"),
              linewidth = 0.5, linetype = "21", show.legend = FALSE) +
  geom_abline(aes(slope = c(-10.3), 
                  intercept = find_intercept(c(2), c(8.951), c(-10.3)), 
                  color = "poly()"),
              linewidth = 0.5, linetype = "21", show.legend = FALSE) +
  geom_abline(aes(slope = c(21.4), 
                  intercept = find_intercept(c(6), c(-0.545), c(21.4)), 
                  color = "poly()"),
              linewidth = 0.5, linetype = "21", show.legend = FALSE) +
  geom_point(aes(-1, -4.545, fill = "poly()"),
             shape = 23, size = 3, show.legend = FALSE) +  
  geom_point(aes(2, 8.951, fill = "poly()"),
             shape = 23, size = 3, show.legend = FALSE) +  
  geom_point(aes(6, -0.545, fill = "poly()"),
             shape = 23, size = 3, show.legend = FALSE) +  
  annotate("label", x = c(-1, 2, 6) + 0.7, y = c(-4.545, 8.951, -0.545) - 4, 
           label = c(28.3, -10.3, 21.4), alpha = 0.5, fill = "#009E73", size = 3) +
  scale_color_okabeito(order = 3) +
  scale_fill_okabeito(order = 3) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = c(-1, 2.5, 6), limits = c(-2, 8)) +
  scale_y_continuous(breaks = c(-50, 0, 50, 100), limits = c(-55, 75)) +
  theme(panel.grid.major.x = element_blank())


p1_slope_01 <- 
  enzyme_tbl|> 
  ggplot(aes(ph, activity)) +
  theme_marginal()  + 
  geom_point(color = "gray50", alpha = 0.5) +
  geom_function(fun = a_line, linewidth = 2, color = "black", alpha = 0.5) +
  geom_line(aes(y = predict(gam_fit), color = "poly()"), linewidth = 1) +
  labs(x = "Standardisierter pH-Wert (X)", y = "Standardisierte Enzymaktivität (Y)",
       title = "Modellierte Steigung", subtitle = "Wenn X sich ändert, wie ändert sich dann Y?",
       color = "Methode") +
  geom_abline(data = slope_annotations,
              aes(slope = slope, intercept = intercept),
              linewidth = 0.5, linetype = "21", color = "black", alpha = 0.5) +
  geom_abline(aes(slope = c(28.7), 
                  intercept = find_intercept(c(-1), c(-5.73), c(28.7)), 
                  color = "poly()"),
              linewidth = 0.5, linetype = "21", show.legend = FALSE) +
  geom_abline(aes(slope = c(-11.3), 
                  intercept = find_intercept(c(2), c(8.94), c(-11.3)), 
                  color = "poly()"),
              linewidth = 0.5, linetype = "21", show.legend = FALSE) +
  geom_abline(aes(slope = c(18.1), 
                  intercept = find_intercept(c(6), c(-3.21), c(18.1)), 
                  color = "poly()"),
              linewidth = 0.5, linetype = "21", show.legend = FALSE) +
  geom_point(aes(-1, -5.73, fill = "poly()"),
             shape = 23, size = 3, show.legend = FALSE) +  
  geom_point(aes(2, 8.94, fill = "poly()"),
             shape = 23, size = 3, show.legend = FALSE) +  
  geom_point(aes(6, -3.21, fill = "poly()"),
             shape = 23, size = 3, show.legend = FALSE) +  
  annotate("label", x = c(-1, 2, 6) + 0.7, y = c(-5.73, 8.94, -3.21) - 4, 
           label = c(28.7, -11.3, 18.1), alpha = 0.5, fill = "#009E73", size = 3) +
  scale_color_okabeito(order = 3) +
  scale_fill_okabeito(order = 3) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = c(-1, 2.5, 6), limits = c(-2, 8)) +
  scale_y_continuous(breaks = c(-50, 0, 50, 100), limits = c(-55, 75)) +
  theme(panel.grid.major.x = element_blank())

p1_slope_02 <- 
  enzyme_tbl|> 
  ggplot(aes(ph, activity)) +
  theme_marginal()  + 
  geom_point(color = "gray50", alpha = 0.5) +
  geom_function(fun = a_line, linewidth = 2, color = "black", alpha = 0.5) +
  geom_line(aes(y = predict(loess_fit), color = "poly()"), linewidth = 1) +
  labs(x = "Standardisierter pH-Wert (X)", y = "Standardisierte Enzymaktivität (Y)",
       title = "Modellierte Steigung", subtitle = "Wenn X sich ändert, wie ändert sich dann Y?",
       color = "Methode") +
  geom_abline(data = slope_annotations,
              aes(slope = slope, intercept = intercept),
              linewidth = 0.5, linetype = "21", color = "black", alpha = 0.5) +
  geom_abline(aes(slope = c(22.37), 
                  intercept = find_intercept(c(-1), c(-9.33), c(22.37)), 
                  color = "poly()"),
              linewidth = 0.5, linetype = "21", show.legend = FALSE) +
  geom_abline(aes(slope = c(-4.74), 
                  intercept = find_intercept(c(2), c(8.54), c(-4.74)), 
                  color = "poly()"),
              linewidth = 0.5, linetype = "21", show.legend = FALSE) +
  geom_abline(aes(slope = c(21.85), 
                  intercept = find_intercept(c(6), c(4.93), c(21.85)), 
                  color = "poly()"),
              linewidth = 0.5, linetype = "21", show.legend = FALSE) +
  geom_point(aes(-1, -9.33, fill = "poly()"),
             shape = 23, size = 3, show.legend = FALSE) +  
  geom_point(aes(2, 8.54, fill = "poly()"),
             shape = 23, size = 3, show.legend = FALSE) +  
  geom_point(aes(6, 4.93, fill = "poly()"),
             shape = 23, size = 3, show.legend = FALSE) +  
  annotate("label", x = c(-1, 2, 6) + 0.7, y = c(-9.33, 8.54, 4.93) - 4, 
           label = c(22.37, -4.74, 21.85), alpha = 0.5, fill = "#009E73", size = 3) +
  scale_color_okabeito(order = 3) +
  scale_fill_okabeito(order = 3) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = c(-1, 2.5, 6), limits = c(-2, 8)) +
  scale_y_continuous(breaks = c(-50, 0, 50, 100), limits = c(-55, 75)) +
  theme(panel.grid.major.x = element_blank())


gam_fit <- gam(activity ~ s(ph), data = enzyme_tbl)
poly_fit <- lm(activity ~ poly(ph, 3), data = enzyme_tbl)
loess_fit <- loess(activity ~ ph, data = enzyme_tbl)
gam_pred <- predictions(gam_fit, newdata = datagrid(ph = c(-1, 2, 6)))
poly_pred <- predictions(poly_fit, newdata = datagrid(ph = c(-1, 2, 6)))
loess_pred <- predictions(loess_fit, newdata = datagrid(ph = c(-1, 2, 6)))

p1_predict_00 <- 
  enzyme_tbl|> 
  ggplot(aes(ph, activity)) +
  theme_marginal()  + 
  geom_point(color = "gray50", alpha = 0.5) +
  geom_function(fun = a_line, linewidth = 2, color = "black", alpha = 0.5) +
  geom_line(aes(y = predict(gam_fit), color = "gam()"), linewidth = 1) +
  geom_line(aes(y = predict(loess_fit), color = "loess()"), linewidth = 1) +
  geom_line(aes(y = predict(poly_fit), color = "poly()"), linewidth = 1) +
  geom_point(data = gam_pred, aes(ph, estimate, fill = "gam()"),
             show.legend = FALSE, shape = 21, size = 3) +
  geom_point(data = loess_pred, aes(ph, estimate, fill = "loess()"),
             show.legend = FALSE, shape = 21, size = 3) +
  geom_label(data = loess_pred, aes(ph+0.5, estimate + c(-6.5, 8.5, 4), fill = "loess()", 
                                    label = round(estimate)),
             show.legend = FALSE, size = 3, alpha = 0.5) +
  geom_label(data = gam_pred, aes(ph+0.5, estimate + c(-3, 1, -3), fill = "gam()", 
                                  label = round(estimate)),
             show.legend = FALSE, size = 3, alpha = 0.5) +
  geom_label(data = poly_pred, aes(ph+0.5, estimate + c(3, -6, 2), fill = "poly()", 
                                   label = round(estimate)),
             show.legend = FALSE, size = 3, alpha = 0.5) +
  geom_point(data = poly_pred, aes(ph, estimate, fill = "poly()"),
             show.legend = FALSE, shape = 21, size = 3) +
  labs(x = "Standardisierter pH-Wert (X)", y = "Standardisierte Enzymaktivität (Y)",
       title = "Modellierte Vorhersage", subtitle = "Welche Werte für Y sagt das Modell für X vorraus?",
       color = "Methode") +
  scale_color_okabeito() +
  scale_fill_okabeito() +
  scale_x_continuous(breaks = c(-1, 2.5, 6), limits = c(-2, 8)) +
  scale_y_continuous(breaks = c(-50, 0, 50, 100), limits = c(-55, 75)) 

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
a_line <- \(x) x^3 + -8*x^2 + 10*x + 10
a_slope <- \(x) 10 - 16*x + 3*x^2

p1_model_enzyme_01 <- enzyme_tbl|> 
  ggplot(aes(ph, activity)) +
  geom_point() +
  theme_marginal() +
  geom_function(fun = a_line, linewidth = 1, color = cb_pal[2]) +
  geom_richtext(aes(x = 4.25, y = 50, size = 5,
                    label = "f(x) = x³ - 8x² + 10x + 10<br>f'(x) = 3x² -16x +10")) +
  scale_x_continuous(breaks = c(-1, 2.5, 6), limits = c(-2, 8)) +
  scale_y_continuous(breaks = c(-50, 0, 50, 100), limits = c(-55, 75)) +
  labs(x = "Korrigierter pH-Wert", y = "Standardisierte Enzymaktivität") +
  theme(legend.position = "none")

p1_model_enzyme_02 <- enzyme_tbl |> 
  mutate(grp = factor(grp, levels = c("niedrig", "mittel", "hoch"))) |> 
  ggplot(aes(grp, activity, fill = grp)) +
  theme_marginal() +
  geom_boxplot() +
  labs(x = "Gruppierter pH-Wert", y = "Standardisierte Enzymaktivität") +
  scale_fill_okabeito() +
  theme(legend.position = "none")

p2_model_enzyme_02 <- enzyme_tbl |> 
  mutate(grp = factor(grp, levels = c("niedrig", "mittel", "hoch"))) |> 
  ggplot(aes(grp, activity, fill = type)) +
  theme_marginal() +
  geom_boxplot() +
  scale_fill_okabeito() +
  labs(x = "Gruppierter pH-Wert", y = "Standardisierte Enzymaktivität",
       fill = "") +
  theme(legend.position = "top",
        legend.title = element_text(size = 11, face = 2))
## -----------------------------------------------------------------------------
flea_model_tbl <- read_excel("data/fleas_model_data.xlsx") |> 
  mutate(feeding = as_factor(feeding),
         stage = as_factor(stage),
         bonitur = as.numeric(bonitur),
         infected = factor(infected, labels = c("healthy", "infected"))) |> 
  select(feeding, stage, jump_length, weight, hatched, count_leg,  bonitur, infected)

p1_model_flea <- ggplot(flea_model_tbl, aes(x = feeding, y = jump_length, fill = stage)) +
  theme_marginal() +
  geom_boxplot() +
  scale_fill_okabeito() +
  theme(legend.position = "top") +
  labs(x = "Ernährungsform", y = "Sprungweite in [cm]",
       fill = "") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))

p2_model_flea <- ggplot(flea_model_tbl, aes(x = feeding, y = log(count_leg), fill = stage)) +
  theme_marginal() +
  geom_boxplot() +
  scale_fill_okabeito() +
  theme(legend.position = c(0.5, 1)) +
  labs(x = "Ernährungsform", y = "Anzahl Beinhaare [log]",
       fill = "") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))

p3_model_flea <- ggplot(flea_model_tbl, aes(x = feeding, y = bonitur, fill = stage)) +
  theme_marginal() +
  geom_dotplot(binaxis = "y", stackdir = "center") +
  facet_wrap(~stage) +
  scale_fill_okabeito() +
  theme(legend.position = "none") +
  labs(x = "Ernährungsform", y = "Boniturnoten",
       fill = "") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))

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

