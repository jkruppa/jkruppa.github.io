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

sum_prob_tbl <- filter(cross_tbl, y_binom >= 0) |> 
  group_by(x_fct) |> 
  summarise(y_binom_sd = sd(y_binom),
            y_binom = mean(y_binom))

p61 <- gg_template %+%
  filter(cross_tbl, y_binom >= 0 & x_fct != "B") + 
  aes(x_num, y_binom, color = as_factor(y_binom)) +
  geom_point(shape = 21, aes(fill = as_factor(y_binom)), color = "black") + 
  scale_y_continuous(breaks = c(0, 1), labels = c(0, 1),
                     expand = expand_scale(mult = c(0.05, 0.15))) +
  labs(x = "Kontinuierlich", y = "Kategoriell (2 Level)",
       title = "Probability Model",
       subtitle = "Regressionsgleichung, Korrelation und\nBestimmtheitsmaß") +
  geom_smooth(method = "lm", se = FALSE, color = "gray", fullrange = TRUE) +
  #scale_x_continuous(expand = expand_scale(mult = c(0.1, 0.1))) +
  xlim(-0.8, 5.8) +
  annotate("text", x = 1, y = 0.75, hjust = "center", color = "black", size = 4, 
           label = "Werte >1 geschätzt", fontface = 2) +
  geom_curve(x = 1, y = 0.8, xend = 5, yend = 1.05,
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = -0.5, color = "black") +
  annotate("text", x = 3.8, y = 0.25, hjust = "center", color = "black", size = 4, 
           label = "Werte <0 geschätzt", fontface = 2) +
  geom_curve(x = 3.8, y = 0.2, xend = 0, yend = -0.05,
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = -0.5, color = "black")

p62 <- gg_template %+%
  filter(cross_tbl, y_binom >= 0) +
  aes(x_fct, y_binom, fill = as_factor(y_binom)) +
  scale_y_continuous(breaks = c(0, 1), labels = c(0, 1),
                     expand = expand_scale(mult = c(0.05, 0.1))) +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5) +  
  geom_errorbar(data = sum_prob_tbl, aes(ymax = y_binom + y_binom_sd, 
                                         ymin = y_binom - y_binom_sd), width=.1) +
  geom_errorbar(data = sum_prob_tbl, aes(ymax = y_binom + y_binom_sd/sqrt(51), 
                                         ymin = y_binom - y_binom_sd/sqrt(51)), 
                width=.1, position = position_nudge(0.1), color = "black", alpha = 0.7) +
  geom_point(data = sum_prob_tbl, aes(y = y_binom), shape = 23, size = 4,
             fill = "#CC79A7") +
  annotate("text", x = 1.15, y = 0.48, hjust = "center", color = "black", size = 4, 
           label = "SD", fontface = 3) +
  annotate("text", x = 1.22, y = 0.21, hjust = "center", color = "black", size = 4, 
           label = "SE", fontface = 3) +
  labs(x = "Kategoriell (>2 Level)", y = "Kategoriell (2 Level)",
       title = "Multipler Gruppenvergleich",
       subtitle = "ANOVA, Paarweiser Vergleich und\ncompact letter display",
       caption = "Auch mit Kruskal-Wallis-Test möglich.") +
  annotate("text", x = c(1, 2, 3), y = c(0.525, 0.925, 1.3),
           label = c("a", "ab", "b"), size = 5, fontface = 2) +
  annotate("text", x = 1, y = 1.2, hjust = "center", color = "black", size = 4, 
           label = TeX(r"($ANOVA < 0.05$)")) 

p63 <- gg_template %+%
  filter(cross_tbl, x_fct != "B" & y_binom >= 0) + 
  aes(x_fct, y_binom, fill = as_factor(y_binom)) +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5) +  
  geom_errorbar(data = filter(sum_prob_tbl, x_fct != "B"), 
                aes(ymax = y_binom + y_binom_sd, ymin = y_binom - y_binom_sd), width=.1) +
  geom_errorbar(data = filter(sum_prob_tbl, x_fct != "B"), 
                aes(ymax = y_binom + y_binom_sd/sqrt(51), 
                    ymin = y_binom - y_binom_sd/sqrt(51)), 
                width=.1, position = position_nudge(0.1), color = "black", alpha = 0.7) +
  geom_point(data = filter(sum_prob_tbl, x_fct != "B"), 
             aes(y = y_binom), shape = 23, size = 4, fill = "#CC79A7") +
  labs(x = "Kategoriell (2 Level)", y = "Kategoriell (2 Level)",
       title = "Gruppenvergleich",
       subtitle = "Student t-Test, Welch t-Test oder\nWilcoxon-Mann-Whitney-Test") +
  geom_signif(comparisons = list(c("A", "C")),
              map_signif_level = TRUE, textsize = 6) +
  scale_y_continuous(expand = expand_scale(mult = c(0.05, 0.1)),
                     breaks = c(0, 1), labels = c(0, 1))