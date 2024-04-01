pacman::p_load(tidyverse, readxl, knitr, kableExtra, gghalves,
               ggdist, patchwork, see, ggmosaic, ggsignif,
               latex2exp)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

set.seed(20240329)

cross_tbl <- tibble(x_num = seq(0, 5, 0.1),
                    x_fct = gl(3, 17, labels = c("A", "B", "C")),
                    y_normal = 0 + 1.2 * x_num + rnorm(length(x_num), 0, 2),
                    y_pois = round(0 + 0.8 * x_num + rnorm(length(x_num), 0, 1)),
                    y_grad = case_when(y_normal < 1 ~ 0,
                                       y_normal >= 1 & y_normal < 3 ~ 1,
                                       y_normal >= 3 ~ 2),
                    y_beta = (y_normal - min(y_normal))/(max(y_normal) - min(y_normal)),
                    y_binom = ifelse(y_normal <= 3, 0, 1))

p41 <- gg_template %+%
  filter(cross_tbl, y_grad >= 0) + 
  aes(x_num, y_grad, color = as_factor(y_grad)) +
  geom_point() +
  scale_y_continuous(breaks = c(0, 1, 2), labels = c(0, 1, 2)) +
  labs(x = "Kontinuierlich", y = "Kategoriell (>2 Level)",
       title = "Ordinale Regression",
       subtitle = "Regressionsgleichung, Korrelation und\nBestimmtheitsmaß") +
  annotate("text", x = 1, y = 1.5, hjust = "center", color = "black", size = 4, 
           label = "Stärkste Ausprägung", fontface = 2) +
  geom_curve(x = 2.1, y = 1.5, xend = 4, yend = 1.95,
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = 0.5, color = "black") +
  annotate("text", x = 3.8, y = 0.5, hjust = "center", color = "black", size = 4, 
           label = "Schwächste Ausprägung", fontface = 2) +
  geom_curve(x = 2.5, y = 0.5, xend = 1, yend = 0.05,
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = 0.5, color = "black") 

p42 <- gg_template %+%
  filter(cross_tbl, y_grad >= 0) + 
  geom_mosaic(aes(x = product(y_grad, x_fct), fill = y_grad)) +
  labs(x = "Kategoriell (>2 Level)", y = "Kategoriell (>2 Level)",
       title = "Multipler Gruppenvergleich",
       subtitle = "Chi Quadrat Test, Paarweiser Vergleich und\ncompact letter display")

p43 <- gg_template %+%
  filter(cross_tbl, x_fct != "C" & y_grad >= 0) + 
  geom_mosaic(aes(x = product(y_grad, x_fct), fill = y_grad)) +
  labs(x = "Kategoriell (2 Level)", y = "Kategoriell (>2 Level)",
       title = "Gruppenvergleich",
       subtitle = "Chi Quadrat Test")