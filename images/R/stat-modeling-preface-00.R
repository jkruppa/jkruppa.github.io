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
        axis.text.y = element_text(color = "#CC79A7", face = "bold", size = 14),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        title = element_text(size = 14, face = "bold"),
        plot.caption = element_text(face = "italic"),
        legend.position = "none") +
  scale_color_okabeito() +
  scale_fill_okabeito() 

stat_tbl <- cross_tbl |> 
  group_by(x_fct) |> 
  summarise(mean_normal = mean(y_normal),
            mean_pois = mean(y_pois),
            mean_grad = mean(y_grad),
            mean_beta = mean(y_beta),
            mean_binom = mean(y_binom),
            sd_normal = sd(y_normal),
            sd_pois = sd(y_pois),
            sd_grad = sd(y_grad),
            sd_beta = sd(y_beta),
            sd_binom = sd(y_binom))

p01 <- gg_template %+%
  stat_tbl +
  aes(x_fct, mean_normal, fill = x_fct) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_normal-(sd_normal/sqrt(51)), 
                    ymax = mean_normal+(sd_normal/sqrt(51))),
                width = 0.2) +
  labs(x = "Kategoriell (>2 Level)", y = "Mittlerer Ertrag [t/ha]",
       title = "Gaussian",
       subtitle = "ANOVA, Paarweiser Vergleich und\ncompact letter display") +
  annotate("text", x = 1.5, y = 6, hjust = "right", color = "black", size = 4, 
           label = TeX(r"($ANOVA < 0.05$)")) +
  annotate("text", x = c(1, 2, 3), y = c(1.6, 3, 5.4),
           label = c("a", "ab", "b"), size = 5, fontface = 2) 

p02 <- gg_template %+%
  stat_tbl +
  aes(x_fct, mean_pois, fill = x_fct) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_pois-(sd_pois/sqrt(51)), 
                    ymax = mean_pois+(sd_pois/sqrt(51))),
                width = 0.2) +
  labs(x = "Kategoriell (>2 Level)", y = "Mittlerer Befall [Anzahl/Parzelle]",
       title = "Poisson",
       subtitle = "ANOVA, Paarweiser Vergleich und\ncompact letter display") +
  annotate("text", x = 1.5, y = 4, hjust = "right", color = "black", size = 4, 
           label = TeX(r"($ANOVA < 0.05$)")) +
  annotate("text", x = c(1, 2, 3), y = c(1, 2.2, 3.8),
           label = c("a", "b", "c"), size = 5, fontface = 2) 

p03 <- gg_template %+%
  stat_tbl +
  aes(x_fct, mean_beta, fill = x_fct) +
  geom_bar(stat = "identity") +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  geom_errorbar(aes(ymin = mean_beta-(sd_beta/sqrt(51)), 
                    ymax = mean_beta+(sd_beta/sqrt(51))),
                width = 0.2) +
  labs(x = "Kategoriell (>2 Level)", y = "Mittlerer Anteil [%]",
       title = "Beta",
       subtitle = "ANOVA, Paarweiser Vergleich und\ncompact letter display") +
  annotate("text", x = 1.5, y = 1, hjust = "right", color = "black", size = 4, 
           label = TeX(r"($ANOVA < 0.05$)")) +
  annotate("text", x = c(1, 2, 3), y = c(0.32, 0.5, 0.73),
           label = c("a", "ab", "b"), size = 5, fontface = 2) 

p04 <- gg_template %+%
  stat_tbl +
  aes(x_fct, mean_grad, fill = x_fct) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_grad-(sd_grad/sqrt(51)), 
                    ymax = mean_grad+(sd_grad/sqrt(51))),
                width = 0.2) +
  labs(x = "Kategoriell (>2 Level)", y = "Mittlere Note [Likert-Skala]",
       title = "Ordinal",
       subtitle = "ANOVA, Paarweiser Vergleich und\ncompact letter display") +
  annotate("text", x = 1.5, y = 2, hjust = "right", color = "black", size = 4, 
           label = TeX(r"($ANOVA < 0.05$)")) +
  annotate("text", x = c(1, 2, 3), y = c(0.8, 1.3, 2.1),
           label = c("a", "b", "c"), size = 5, fontface = 2) 

p05 <- gg_template %+%
  stat_tbl +
  aes(x_fct, mean_binom, fill = x_fct) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_binom-(sd_binom/sqrt(51)), 
                    ymax = mean_binom+(sd_binom/sqrt(51))),
                width = 0.2) +
  labs(x = "Kategoriell (>2 Level)", y = "Mittlerer Anteil infiziert [%]",
       title = "Binomial",
       subtitle = "ANOVA, Paarweiser Vergleich und\ncompact letter display") +
  annotate("text", x = 1.5, y = 1, hjust = "right", color = "black", size = 4, 
           label = TeX(r"($ANOVA < 0.05$)")) +
  annotate("text", x = c(1, 2, 3), y = c(0.25, 0.5, 1),
           label = c("a", "ab", "b"), size = 5, fontface = 2) 