f1_ancova_theo_tbl <- tibble(rsp = c(7,8,9, 2,3,4, 11,12,13),
                             fa = gl(3, 3, labels = c("A.1", "A.2", "A.3")),
                             cov = seq(1, 9, 1)*0.8)

ancova_theo_p <- ggplot(f1_ancova_theo_tbl, aes(cov, rsp)) +
  theme_minimal() +
  geom_hline(yintercept = 0, color = "gray50") +
  geom_vline(xintercept = 0, color = "gray50") +
  geom_function(fun = \(x){6 + 1.25*x}, color = "#CC79A7") +
  geom_point() +
  geom_segment(x = 0.8, y = 8, xend = 2.4, yend = 8, color = "#D55E00") +
  annotate("text", x = 2.4+0.1, y = 8, label = expression(bar(y)[A.1]), 
           size = 5, color = "#D55E00", hjust = "left") +
  geom_segment(x = 3.2, y = 3, xend = 4.8, yend = 3, color = "#0072B2",) +
  geom_segment(x = 4, y = 3, xend = 4, yend = 11, color = "#0072B2", 
               linetype = 2) +
  annotate("text", x = 4.8+0.1, y = 3, label = expression(bar(y)[A.2]), 
           size = 5, color = "#0072B2", hjust = "left") +
  annotate("label", x = 4+0.1, y = (3+11)/2, label = expression(beta[A.2]~"="~-8), size = 5,
           color = "#0072B2", hjust = "left") +
  geom_segment(x = 5.6, y = 12, xend = 7.2, yend = 12, color = "#009E73") +
  geom_segment(x = 6.4, y = 12, xend = 6.4, yend = 14, color = "#009E73",
               linetype = 2) +
  annotate("text", x = 7.2+0.1, y = 12, label = expression(bar(y)[A.3]), 
           size = 5, color = "#009E73", hjust = "left") +
  annotate("label", x = 6.4-0.1, y = 13, label = expression(beta[A.3]~"="~-2), size = 5,
           color = "#009E73", hjust = "right") +
  geom_segment(x = 0, y = 6, xend = 1, yend = 6, color = "#CC79A7",
               linetype = 2) +
  geom_segment(x = 1, y = 6, xend = 1, yend = 7.25, color = "#CC79A7",
               linetype = 2) +
  annotate("text", x = 0.5, y = 5.5, label = expression(x+1), 
           size = 5, color = "#CC79A7", hjust = "center") +
  annotate("label", x = 1+0.1, y = (6 + 7.25)/2, label = expression(beta[c]~"="~1.25), 
           size = 5, color = "#CC79A7", hjust = "left") +
  scale_x_continuous(breaks = c(0, 1, 1.6, 4.0, 6.4),   
                     labels = c(0, 1, expression(bar(c)[A.1]~"="~1.6), 
                                expression(bar(c)[A.2]~"="~4.0), 
                                expression(bar(c)[A.3]~"="~6.4)),
                     limits = c(0, 7.5)) +
  scale_y_continuous(breaks = c(8, 3, 6, 7.25, 11, 12, 14),
                     labels = c(expression(bar(y)[A.1]~"="~8),
                                expression(bar(y)[A.2]~"="~3),
                                expression(beta[0]~"="~6),
                                7.25, 11, 
                                expression(bar(y)[A.3]~"="~12), 14)) +
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 2),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 16, face = 2),
        axis.text.x = element_text(size = 11),        
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12, face = "italic"),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "top") + 
  labs(x = "Kovariate C", y = "", fill = "",
       #title = "Effekt A / Effekt B",
       #subtitle = "Boxen im Faktor A und Faktor B gleich verschoben",
       #caption = "Kein Fehler in der Generierung der Daten"
  )