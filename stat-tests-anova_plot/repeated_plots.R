repeated_fac1_p <- tibble(pos = 1:4, y = 1, labels = c(23.72, 37.57,	36.67, 25.89)) |> 
  ggplot(aes(pos, y, label = labels)) +
  theme_minimal() +
  geom_segment(x = 0.25, y = 1, xend = 4.75, yend = 1,
               color = "black", linewidth = 0.5, 
               arrow = arrow(length = unit(0.15, "inches"))) +
  geom_label(size = 8) +
  scale_x_continuous(limits = c(0.25, 4.75), name = "", breaks = 1:4,
                     labels = c(expression(t[1]), expression(t[2]),
                                expression(t[3]), expression(t[4]))) +
  scale_y_continuous(name = "") +  
  labs(title = "Design der einfaktoriellen repeated ANOVA",
       subtitle = "Die Sprungweite desselben Katzenflohs wird 4-mal wiederholt gemessen (n = 1)") +
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 2),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 16, face = 2),
        axis.text.x = element_text(size = 14),        
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "top") 


repeated_fac2_p <- ggplot() + 
  aes(pos, y) +
  theme_minimal() +
  annotate("segment", x = 1.4, y = 3, xend = 5.5, yend = 3,
           color = "black", linewidth = 1) +
  annotate("segment", x = 1.5, y = seq(1.5, 2.5, 0.5), 
           xend = 5.5, yend = seq(1.5, 2.5, 0.5), 
           color = "black", linewidth = 1) + 
  annotate("segment", x = 1.5, y = 1, xend = 5.6, yend = 1,
           color = "black", linewidth = 1,
           arrow = arrow(length = unit(0.15, "inches"))) +
  annotate("curve", x = 5.5, y = c(3:2), 
           xend = 5.5, yend = c(2.5, 1.5),
           color = "black", linewidth = 1, curvature = -0.25) +
  annotate("curve", x = 1.5, y = c(2.5, 1.5), 
           xend = 1.5, yend = c(2:1),
           color = "black", linewidth = 1, curvature = 0.25) +
  geom_label(data = tibble(pos = rep(2:5, 3), y = rep(c(1 , 2, 3), each = 4), 
                           labels = c("34.85", "50.21", "50.40", "40.92",
                                      "50.37", "57.87", "66.84", "45.45",
                                      "51.01", "44.54", "42.65", "38.74")), 
             aes(label = labels), size = 6) +
  geom_label(data = tibble(pos = 0.5, y = c(1 , 2, 3), 
                           labels = c("ctrl", "blood", "ketchup")), 
             aes(label = labels), size = 6, fill = "#56B4E9") +
  scale_x_continuous(limits = c(0.25, 5.75), name = "", breaks = c(0.5, 2:5),
                     labels = c("feeding", expression(t[1]), expression(t[2]),
                                expression(t[3]), expression(t[4]))) +
  scale_y_continuous(limits = c(0.5, 3.5), name = "") +  
  labs(title = "Design der zweifaktoriellen repeated ANOVA",
       subtitle = "Die Sprungweite desselben Katzenflohs wird 12-mal wiederholt gemessen (n = 1)") +  
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 2),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 16, face = 2),
        axis.text.x = element_text(size = 14),        
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "top") 

repeated_fac3_p <- ggplot() + 
  aes(pos, y) +
  theme_minimal() +
  annotate("segment", x = 1.4, y = 6, xend = 5.5, yend = 6,
           color = "black", linewidth = 1) +
  annotate("segment", x = 1.5, y = seq(1.5, 5.5, 0.5), 
           xend = 5.5, yend = seq(1.5, 5.5, 0.5), 
           color = "black", linewidth = 1) +  
  annotate("segment", x = 1.5, y = 1, xend = 5.6, yend = 1,
           color = "black", linewidth = 1,
           arrow = arrow(length = unit(0.15, "inches"))) +
  annotate("curve", x = 5.5, y = c(6:2), 
           xend = 5.5, yend = c(5.5, 4.5, 3.5, 2.5, 1.5),
           color = "black", linewidth = 1, curvature = -0.25) +
  annotate("curve", x = 1.5, y = c(5.5, 4.5, 3.5, 2.5, 1.5), 
           xend = 1.5, yend = c(5:1),
           color = "black", linewidth = 1, curvature = 0.25) +
  geom_label(data = tibble(pos = rep(2:5, 6), y = rep(1:6, each = 4), 
                           labels = c("34.85", "50.21", "50.40", "40.92",
                                      "50.37", "57.87", "66.84", "45.45",
                                      "51.01", "44.54", "42.65", "38.74",
                                      "31.02", "42.39",	"26.64", "11.59",
                                      "41.69", "46.92",	"41.77", "27.76",
                                      "38.97", "46.21",	"43.79", "18.68")), 
             aes(label = labels), size = 6) +
  geom_label(data = tibble(pos = 0.5-1, y = 1:6, 
                           labels = rep(c("ctrl", "blood", "ketchup"), each = 2)), 
             aes(label = labels), size = 6, fill = "#56B4E9") +
  geom_label(data = tibble(pos = 0.5, y = 1:6, 
                           labels = rep(c("no", "yes"), 3)), 
             aes(label = labels), size = 6, fill = "#E69F00") +
  scale_x_continuous(limits = c(-0.75, 5.75), name = "", breaks = c(-0.5, 0.5, 2:5),
                     labels = c("feeding", "workout", expression(t[1]), expression(t[2]),
                                expression(t[3]), expression(t[4]))) +
  scale_y_continuous(limits = c(0.5, 6.5), name = "") +  
  labs(title = "Design der dreifaktoriellen repeated ANOVA",
       subtitle = "Die Sprungweite desselben Katzenflohs wird 24-mal wiederholt gemessen (n = 1)") +  
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 2),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 16, face = 2),
        axis.text.x = element_text(size = 14),        
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "top") 


mixed_fac2_p <- ggplot() + 
  aes(pos, y) +
  theme_minimal() +
  annotate("segment", x = 1.4, y = seq(1, 3, 1), xend = 5.6, yend = seq(1, 3, 1),
           color = "black", linewidth = 1,
           arrow = arrow(length = unit(0.15, "inches"))) +
  geom_label(data = tibble(pos = rep(2:5, 3), y = rep(c(1 , 2, 3), each = 4), 
                           labels = c("34.85", "50.21", "50.40", "40.92",
                                      "50.37", "57.87", "66.84", "45.45",
                                      "51.01", "44.54", "42.65", "38.74")), 
             aes(label = labels), size = 6) +
  geom_label(data = tibble(pos = 0.5, y = c(1 , 2, 3), 
                           labels = c("ctrl", "blood", "ketchup")), 
             aes(label = labels), size = 6, fill = "#56B4E9") +
  scale_x_continuous(limits = c(0.25, 5.75), name = "", breaks = c(0.5, 2:5),
                     labels = c("feeding", expression(t[1]), expression(t[2]),
                                expression(t[3]), expression(t[4]))) +
  scale_y_continuous(limits = c(0.5, 3.5), name = "") +  
  labs(title = "Design der zweifaktoriellen mixed ANOVA",
       subtitle = "Jede Faktorkombination mit unterschiedlichen Flöhen gemessen (n = 3)") +  
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 2),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 16, face = 2),
        axis.text.x = element_text(size = 14),        
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "top") 


mixed_fac3_p <- ggplot() + 
  aes(pos, y) +
  theme_minimal() +
  annotate("segment", x = 1.4, y = 1:6, xend = 5.65, yend = 1:6,
           color = "black", linewidth = 1,
           arrow = arrow(length = unit(0.15, "inches"))) +
  geom_label(data = tibble(pos = rep(2:5, 6), y = rep(1:6, each = 4), 
                           labels = c("34.85", "50.21", "50.40", "40.92",
                                      "50.37", "57.87", "66.84", "45.45",
                                      "51.01", "44.54", "42.65", "38.74",
                                      "31.02", "42.39",	"26.64", "11.59",
                                      "41.69", "46.92",	"41.77", "27.76",
                                      "38.97", "46.21",	"43.79", "18.68")), 
             aes(label = labels), size = 6) +
  geom_label(data = tibble(pos = 0.5-1, y = 1:6, 
                           labels = rep(c("ctrl", "blood", "ketchup"), each = 2)), 
             aes(label = labels), size = 6, fill = "#56B4E9") +
  geom_label(data = tibble(pos = 0.5, y = 1:6, 
                           labels = rep(c("no", "yes"), 3)), 
             aes(label = labels), size = 6, fill = "#E69F00") +
  scale_x_continuous(limits = c(-0.75, 5.75), name = "", breaks = c(-0.5, 0.5, 2:5),
                     labels = c("feeding", "workout", expression(t[1]), expression(t[2]),
                                expression(t[3]), expression(t[4]))) +
  scale_y_continuous(limits = c(0.5, 6.5), name = "") +  
  labs(title = "Design der dreifaktoriellen mixed ANOVA",
       subtitle = "Jede Faktorkombination mit unterschiedlichen Flöhen gemessen (n = 6)") +  
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 2),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 16, face = 2),
        axis.text.x = element_text(size = 14),        
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "top") 

mixed_theo_fac2_p <- ggplot() + 
  aes(pos, y) +
  theme_minimal() +
  annotate("segment", x = 1.4, y = seq(1, 3, 1), xend = 5.6, yend = seq(1, 3, 1),
           color = "black", linewidth = 1,
           arrow = arrow(length = unit(0.15, "inches"))) +
  geom_label(data = tibble(pos = rep(2:5, 3), y = rep(c(1 , 2, 3), each = 4), 
                           labels = c("34.85", "50.21", "50.40", "40.92",
                                      "50.37", "57.87", "66.84", "45.45",
                                      "51.01", "44.54", "42.65", "38.74")), 
             aes(label = labels), size = 6) +
  geom_label(data = tibble(pos = 0.5, y = c(1 , 2, 3), 
                           labels = c("A.1", "A.2", "A.3")), 
             aes(label = labels), size = 6, fill = "#56B4E9") +
  geom_label(data = tibble(pos = 1.25, y = c(1,2,3), 
                           labels = c(expression(ID[3]),
                           expression(ID[2]),
                           expression(ID[1]))), 
             aes(label = labels), size = 5, fill = "gray80", parse = TRUE) +
  scale_x_continuous(limits = c(0.25, 5.75), name = "", breaks = c(0.5, 2:5),
                     labels = c(expression(f[A]), expression(t[1]), expression(t[2]),
                                expression(t[3]), expression(t[4]))) +
  scale_y_continuous(limits = c(0.5, 3.5), name = "") +  
  labs(title = "Design der zweifaktoriellen mixed ANOVA",
       subtitle = "Jede Faktorkombination mit unterschiedlichen Flöhen gemessen (n = 3)") +  
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 2),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 16, face = 2),
        axis.text.x = element_text(size = 14),        
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "top")




repeated_theo_fac2_p <- ggplot() + 
  aes(pos, y) +
  theme_minimal() +
  annotate("segment", x = 1.4, y = 3, xend = 5.5, yend = 3,
           color = "black", linewidth = 1) +
  annotate("segment", x = 1.5, y = seq(1.5, 2.5, 0.5), 
           xend = 5.5, yend = seq(1.5, 2.5, 0.5), 
           color = "black", linewidth = 1) + 
  annotate("segment", x = 1.5, y = 1, xend = 5.6, yend = 1,
           color = "black", linewidth = 1,
           arrow = arrow(length = unit(0.15, "inches"))) +
  annotate("curve", x = 5.5, y = c(3:2), 
           xend = 5.5, yend = c(2.5, 1.5),
           color = "black", linewidth = 1, curvature = -0.25) +
  annotate("curve", x = 1.5, y = c(2.5, 1.5), 
           xend = 1.5, yend = c(2:1),
           color = "black", linewidth = 1, curvature = 0.25) +
  geom_label(data = tibble(pos = rep(2:5, 3), y = rep(c(1 , 2, 3), each = 4), 
                           labels = c("34.85", "50.21", "50.40", "40.92",
                                      "50.37", "57.87", "66.84", "45.45",
                                      "51.01", "44.54", "42.65", "38.74")), 
             aes(label = labels), size = 6) +
  geom_label(data = tibble(pos = 0.5, y = c(1 , 2, 3), 
                           labels = c("A.1", "A.2", "A.3")), 
             aes(label = labels), size = 6, fill = "#56B4E9") +
  geom_label(data = tibble(pos = 1.25, y = c(3), 
                           labels = expression(ID[1])), 
             aes(label = labels), size = 5, fill = "gray80", parse = TRUE) +
  scale_x_continuous(limits = c(0.25, 5.75), name = "", breaks = c(0.5, 2:5),
                     labels = c(expression(f[A]), expression(t[1]), expression(t[2]),
                                expression(t[3]), expression(t[4]))) +
  scale_y_continuous(limits = c(0.5, 3.5), name = "") +  
  labs(title = "Design der zweifaktoriellen repeated ANOVA",
       subtitle = "Die Sprungweite desselben Katzenflohs wird 12-mal wiederholt gemessen (n = 1)") +  
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 2),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 16, face = 2),
        axis.text.x = element_text(size = 14),        
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 12),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "top")