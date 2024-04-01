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

pois_fit <- glm(y_pois ~ x_num, data = filter(cross_tbl, y_pois >= 0), family = poisson)

gg_template <- ggplot() +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        #axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        title = element_text(size = 14, face = "bold"),
        plot.caption = element_text(face = "italic"),
        legend.position = "none") +
  scale_color_okabeito() +
  scale_fill_okabeito() 

p21 <- gg_template %+%
  filter(cross_tbl, y_pois >= 0) + 
  aes(x_num, y_pois) +
  geom_point() +
  geom_hline(yintercept = 0, color = "gray") +
  labs(x = "Kontinuierlich", y = "Kontinuierlich",
       title = "Poisson Regression",
       subtitle = "Regressionsgleichung, Korrelation und\nBestimmtheitsmaß") +
  annotate("text", x = 4, y = 1.5, hjust = "left", color = "black", size = 4, 
           label = "0 Zählungen", fontface = 2) +
  geom_curve(x = 3.9, y = 1.5, xend = 2, yend = 0.1,
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = 0.5) +
  scale_y_continuous(labels = c(0, "", "", ""),
                     expand = expand_scale(mult = c(0.05, 0.1))) +
  geom_line(aes(y = predict(pois_fit, type = "response")), color = "gray",
            linewidth = 1) 


p22 <- gg_template %+%
  filter(cross_tbl, y_pois >= 0) +  
  aes(x_fct, y_pois, fill = x_fct) +
  geom_boxplot(width = 0.15, outlier.shape = NA) +
  geom_hline(yintercept = 0, color = "gray") +
  stat_dots(side = "left", justification = 1.12, binwidth = 0.01,
            dotsize = 15) +
  annotate("text", x = 2, y = 6, hjust = "right", color = "black", size = 4, 
           label = TeX(r"($Kruskal-Wallis-Test < 0.05$)")) +
  annotate("text", x = c(1, 2, 3), y = c(3, 5, 6),
           label = c("a", "ab", "b"), size = 5, fontface = 2) +
  labs(x = "Kategoriell (>2 Level)", y = "Kontinuierlich",
       title = "Multipler Gruppenvergleich",
       subtitle = "Kruskal-Wallis-Test, Paarweiser Vergleich und\ncompact letter display",
       caption = "Auch mit ANOVA möglich.") +
  scale_y_continuous(labels = c(0, "", "", ""),
                     expand = expand_scale(mult = c(0.05, 0.1)))

p23 <- gg_template %+%
  filter(cross_tbl, x_fct != "C" & y_pois >= 0) + 
  aes(x_fct, y_pois, fill = x_fct) +
  geom_boxplot(width = 0.15, outlier.shape = NA) +
  geom_hline(yintercept = 0, color = "gray") +
  stat_dots(side = "left", justification = 1.12, binwidth = 0.01,
            dotsize = 12) +
  geom_signif(comparisons = list(c("A", "B")),
              map_signif_level = TRUE, textsize = 6) +
  labs(x = "Kategoriell (2 Level)", y = "Kontinuierlich",
       title = "Gruppenvergleich",
       subtitle = "Wilcoxon-Mann-Whitney-Test",
       caption = "Auch mit Welch t-Test möglich.") +
  scale_y_continuous(labels = c(0, "", "", "", "", ""),
                     expand = expand_scale(mult = c(0.05, 0.1)))