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

beta_fit <- betareg::betareg(y_beta ~ x_num, data = filter(cross_tbl, y_beta > 0 & y_beta < 1))

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

p31 <- gg_template %+%
  filter(cross_tbl, y_beta > 0 & y_beta < 1) + 
  geom_hline(yintercept = c(0, 1), color = "gray") +
  aes(x_num, y_beta) +
  geom_point() +
  annotate("text", x = 4, y = 0.1, hjust = "left", color = "black", size = 4, 
           label = "0%", fontface = 2) +
  geom_curve(x = 3.9, y = 0.1, xend = 2, yend = 0.01,
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = 0.5) +
  annotate("text", x = 1, y = 0.9, hjust = "left", color = "black", size = 4, 
           label = "100%", fontface = 2) +
  geom_curve(x = 1.6, y = 0.9, xend = 3.5, yend = 0.99,
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = 0.5) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Kontinuierlich", y = "Kontinuierlich",
       title = "Beta Regression",
       subtitle = "Regressionsgleichung, Korrelation und\nBestimmtheitsmaß") +
  geom_line(aes(y = predict(beta_fit, type = "response")), color = "gray",
            linewidth = 1)

p32 <- gg_template %+%
  filter(cross_tbl, y_beta >= 0 & y_beta < 0.8) +  
  aes(x_fct, y_beta, fill = x_fct) +
  geom_boxplot(width = 0.15, outlier.shape = NA) +
  stat_dots(side = "left", justification = 1.12, binwidth = 0.005,
            dotsize = 5) +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0,1)) +
  labs(x = "Kategoriell (>2 Level)", y = "Kontinuierlich",
       title = "Multipler Gruppenvergleich",
       subtitle = "Kruskal-Wallis-Test, Paarweiser Vergleich und\ncompact letter display",
       caption = "Auch mit ANOVA möglich.") +
  annotate("text", x = 2, y = 0.9, hjust = "right", color = "black", size = 4, 
           label = TeX(r"(Kruskal-Wallis-Test < 0.05$)")) +
  annotate("text", x = c(1, 2, 3), y = c(0.6, 0.75, 0.85),
           label = c("a", "ab", "b"), size = 5, fontface = 2) 

p33 <- gg_template %+%
  filter(cross_tbl, x_fct != "C" & y_beta >= 0 & y_beta < 0.8) + 
  aes(x_fct, y_beta, fill = x_fct) +
  geom_boxplot(width = 0.15, outlier.shape = NA) +
  stat_dots(side = "left", justification = 1.12, binwidth = 0.005,
            dotsize = 5) + 
  scale_y_continuous(labels = scales::percent,
                     limits = c(0,1)) +
  geom_signif(comparisons = list(c("A", "B")),
              map_signif_level = TRUE, textsize = 6) +
  labs(x = "Kategoriell (2 Level)", y = "Kontinuierlich",
       title = "Gruppenvergleich",
       subtitle = "Wilcoxon-Mann-Whitney-Test",
       caption = "Auch mit Welch t-Test möglich.")