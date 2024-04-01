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

gg_template <- ggplot() +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        title = element_text(size = 14, face = "bold"),
        plot.caption = element_text(face = "italic"),
        legend.position = "none") +
  scale_color_okabeito() +
  scale_fill_okabeito() 

p11 <- gg_template %+%
  cross_tbl + 
  aes(x_num, y_normal) +
  geom_point() +
  labs(x = "Kontinuierlich", y = "Kontinuierlich",
       title = "Lineare Regression",
       subtitle = "Regressionsgleichung, Korrelation und\nBestimmtheitsmaß") +
  annotate("text", x = 1, y = 7, hjust = "left", color = "black", size = 4, 
           label = TeX(r"($y = 0.02 + 1.04 \cdot x$)")) +
  annotate("text", x = 1, y = 6.25, hjust = "left", color = "black", size = 4, 
           label = TeX(r"($r = 0.76\; (p<0.001)$)")) +
  annotate("text", x = 1, y = 5.5, hjust = "left", color = "black", size = 4, 
           label = TeX(r"($R^2 = 0.58$)")) +
  geom_smooth(method = "lm", se = FALSE, color = "gray") +
  geom_curve(x = 2.6, y = 7, xend = 4.5, yend = 4.8,
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = -0.5) 

p12 <- gg_template %+%
  cross_tbl + 
  aes(x_fct, y_normal, fill = x_fct) +
  geom_boxplot(width = 0.15, outlier.shape = NA) +
  stat_dots(side = "left", justification = 1.12, binwidth = 0.01,
            dotsize = 25) +
  annotate("text", x = 1, y = 9, hjust = "right", color = "black", size = 4, 
           label = TeX(r"($ANOVA < 0.05$)")) +
  annotate("text", x = c(1, 2, 3), y = c(4, 8, 9),
           label = c("a", "ab", "b"), size = 5, fontface = 2) +
  labs(x = "Kategoriell (>2 Level)", y = "Kontinuierlich",
       title = "Multipler Gruppenvergleich",
       subtitle = "ANOVA, Paarweiser Vergleich und\ncompact letter display",
       caption = "Auch mit Kruskal-Wallis-Test möglich.") +
  scale_y_continuous(expand = expand_scale(mult = c(0.1, 0.1))) 

p13 <- gg_template %+%
  filter(cross_tbl, x_fct != "C") + 
  aes(x_fct, y_normal, fill = x_fct) +
  geom_boxplot(width = 0.15, outlier.shape = NA) +
  stat_dots(side = "left", justification = 1.12, binwidth = 0.01,
            dotsize = 23) +
  geom_signif(comparisons = list(c("A", "B")),
              map_signif_level = TRUE, textsize = 6) +
  labs(x = "Kategoriell (2 Level)", y = "Kontinuierlich",
       title = "Gruppenvergleich",
       subtitle = "Student t-Test, Welch t-Test oder\nWilcoxon-Mann-Whitney-Test") +
  scale_y_continuous(expand = expand_scale(mult = c(0.1, 0.1)))
