
## ---------------------------------------------------------------------------
## f statistic
p_f_stat <- ggplot(data.frame(x = c(0, 5)), aes(x)) +
  theme_minimal() +
  stat_function(fun = df, xlim = c(0, 2.5), args = c(df1 = 5, df2 = 10),
                geom = "area", fill = "#E69F00", alpha = 0.25) +
  geom_segment(x = 2.5, y = 0, xend = 2.5, yend = 0.095, color = "#CC79A7",
               linewidth = 1) +
  stat_function(fun = df, xlim = c(2.5, 5), args = c(df1 = 5, df2 = 10),
                geom = "area", fill = "#CC79A7", alpha = 0.25) +
  geom_segment(x = 3.5, y = 0, xend = 3.5, yend = 0.036, color = "#0072B2",
               linewidth = 1) +
  stat_function(fun = df, xlim = c(3.5, 5), args = c(df1 = 5, df2 = 10),
                geom = "area", fill = "#0072B2", alpha = 0.5) +
  stat_function(fun = df, linewidth = 1, args = c(df1 = 5, df2 = 10)) + 
  annotate("text", x = 3.5, y = 0.15, label = expression(alpha==5*'%'), size = 6,
           color = "#CC79A7") +
  annotate("text", x = 4, y = 0.35, label = expression(Pr(F[D]*'|'*H[0])), size = 6,
           color = "#0072B2") +
  geom_curve(x = 3.15, y = 0.14, xend = 2.75, yend = 0.03,
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = 0.5, color = "black", alpha = 0.3) +
  geom_curve(x = 4.45, y = 0.35, xend = 4, yend = 0.01,
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = -0.6, color = "black", alpha = 0.3) +
  scale_x_continuous(breaks = c(0, 2.5, 3.5), 
                     labels = c(expression(0),
                                expression(F[alpha*'='*5*'%']),
                                expression(F[D]))) +
  theme(axis.text = element_text(size = 16),
        axis.text.y = element_blank()) +
  labs(x = "", y = "",
       title = expression(Signifikante~Teststatistik~F[D]),
       subtitle = expression(Die~berechnete~Teststatistik~F[D]~ist~größer~als~der~kritische~Wert~F[alpha*'='*5*'%']))

## ---------------------------------------------------------------------------
## function to get data
get_data_tbl <- function(mean = c(7, 7, 7), sd = c(2, 2, 2), ng = 13) {
  assump_tbl <- tibble(A = rnorm(ng, mean[1], sd[1]),
                       B = rnorm(ng, mean[2], sd[2]),
                       C = rnorm(ng, mean[3], sd[3])) |> 
    gather(key = trt, value = rsp)
  stat <- plyr::ddply(assump_tbl, "trt", summarize,
                      mean = mean(rsp, na.rm = TRUE),
                      sd = round(sd(rsp, na.rm = TRUE), 2),
                      var = round(var(rsp, na.rm = TRUE), 2),
                      max = max(mean - min(rsp), max(rsp) - mean)*1.05) |> 
    mutate(pos = c(4, 14, 24),
           rsp = 0)
  grand_mean <- mean(assump_tbl$rsp, na.rm = TRUE)
  plot_tbl <- assump_tbl |> 
    mutate(pos = c(seq(1, 7, 0.5), seq(11, 17, 0.5), seq(21, 27, 0.5)),
           grand_mean = grand_mean,
           grp_mean = rep(stat$mean, each = 13),
           beta = grp_mean - grand_mean,
           epsilon = rsp - grp_mean) 
  return(lst(plot = plot_tbl,
             stat = stat,
             grand_mean = grand_mean))
}

## ---------------------------------------------------------------------------
## example ggplot

fac1_tbl <- read_xlsx("data/flea_dog_cat_fox.xlsx") |> 
  mutate(animal = as_factor(animal))

stat <- plyr::ddply(fac1_tbl, "animal", summarize,
                    mean = mean(jump_length, na.rm = TRUE),
                    sd = round(sd(jump_length, na.rm = TRUE), 2))
grand_mean <- mean(fac1_tbl$jump_length, na.rm = TRUE)

fac1_tbl <- fac1_tbl |> 
  mutate(pos = c(1:7, 11:17, 21:27),
         grand_mean = grand_mean,
         grp_mean = rep(stat$mean, each = 7),
         beta = grp_mean - grand_mean,
         epsilon = jump_length - grp_mean) 


p_example_fac1 <- fac1_tbl |> 
  ggplot() +
  aes(pos, jump_length) +
  theme_minimal() +
  geom_segment(data = dplyr::filter(fac1_tbl, animal == "fox"), x = 21:27, 
               aes(y = jump_length), 
               xend = 21:27, yend = rep(9.16, 7), color = "gray70",
               linewidth = 0.5) +
  geom_segment(data = dplyr::filter(fac1_tbl, animal == "cat"), x = 11:17, 
               aes(y = jump_length), 
               xend = 11:17, yend = rep(4.74, 7), color = "gray70",
               linewidth = 0.5) +
  geom_segment(data = dplyr::filter(fac1_tbl, animal == "dog"), x = 1:7, 
               aes(y = jump_length), 
               xend = 1:7, yend = rep(8.13, 7), color = "gray70",
               linewidth = 0.5) +
  geom_segment(x = 1, y = 8.13, xend = 7, yend = 8.13, color = "#D55E00", 
               linewidth = 1) +
  geom_segment(x = 4.5, y = grand_mean, xend = 4.5, yend = 8.13, color = "#D55E00", 
               linewidth = 0.5) +
  annotate("text", x = 4.75, y = grand_mean + 0.35, label = expression(beta[A.1]), size = 5,
           color = "#D55E00", hjust = "left") +
  annotate("text", x = -1.25, y = 8.13, label = expression(bar(y)[A.1]), size = 4,
           color = "#D55E00", hjust = "left") +
  geom_segment(x = 11, y = 4.74, xend = 17, yend = 4.74, color = "#0072B2", 
               linewidth = 1) +
  geom_segment(x = 14.5, y = grand_mean, xend = 14.5, yend = 4.74, color = "#0072B2", 
               linewidth = 0.5) +
  annotate("text", x = 12.25, y = grand_mean - 1, label = expression(beta[A.2]), size = 5,
           color = "#0072B2", hjust = "left") +
  annotate("text", x = 17.5, y = 4.74, label = expression(bar(y)[A.2]), size = 4,
           color = "#0072B2", hjust = "left") +
  geom_segment(x = 21, y = 9.16, xend = 27, yend = 9.16, color = "#009E73",
               linewidth = 1) +
  geom_segment(x = 24.5, y = grand_mean, xend = 24.5, yend = 9.16, color = "#009E73",
               linewidth = 0.5) +
  annotate("text", x = 24.75, y = grand_mean + 0.8, label = expression(beta[A.3]), size = 5,
           color = "#009E73", hjust = "left") +
  annotate("text", x = 18.5, y = 9.16, label = expression(bar(y)[A.3]), size = 4,
           color = "#009E73", hjust = "left") +
  geom_point() +
  geom_hline(yintercept = grand_mean, color = "gray50", linewidth = 1) +
  scale_x_continuous(breaks = c(4, 14, 24), labels = c("A.1", "A.2", "A.3")) +
  scale_y_continuous(breaks = grand_mean, label = "") +  
  theme(axis.text = element_text(size = 14),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 16, face = 2)) + 
  labs(x = "Faktor A", y = "") +
  annotate("text", x = fac1_tbl$pos + 0.75, 
           y = grand_mean + fac1_tbl$beta + fac1_tbl$epsilon/2,
           label = c(expression(epsilon[1]), expression(epsilon[2]), expression(epsilon[3]),
                     expression(epsilon[4]), expression(epsilon[5]), expression(epsilon[6]),
                     expression(epsilon[7]), expression(epsilon[8]), expression(epsilon[9]),
                     expression(epsilon[10]), expression(epsilon[11]), expression(epsilon[12]),
                     expression(epsilon[13]), expression(epsilon[14]), expression(epsilon[15]),
                     expression(epsilon[16]), expression(epsilon[17]), expression(epsilon[18]),
                     expression(epsilon[19]), expression(epsilon[20]), expression(epsilon[21])),
           color = "gray20")

## ---------------------------------------------------------------------------

p_example_fac_1_total <- fac1_tbl |> 
  mutate(pos = rep(1:21)) |> 
  ggplot() +
  aes(pos, jump_length) +
  theme_minimal() +
  geom_segment(data = dplyr::filter(fac1_tbl, animal == "fox"), x = 15:21, 
               aes(y = jump_length), 
               xend = 15:21, yend = grand_mean, color = "gray70",
               linewidth = 0.5) +
  geom_segment(data = dplyr::filter(fac1_tbl, animal == "cat"), x = 8:14, 
               aes(y = jump_length), 
               xend = 8:14, yend = grand_mean, color = "gray70",
               linewidth = 0.5) +
  geom_segment(data = dplyr::filter(fac1_tbl, animal == "dog"), x = 1:7, 
               aes(y = jump_length), 
               xend = 1:7, yend = grand_mean, color = "gray70",
               linewidth = 0.5) +
  geom_point() +
  geom_hline(yintercept = grand_mean, color = "gray50", linewidth = 1) +
  scale_x_continuous(breaks = 11, labels = "") +
  scale_y_continuous(breaks = grand_mean, labels = expression(beta[0])) +  
  theme(axis.text = element_text(size = 14),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 16, face = 2)) + 
  labs(x = "", y = "")

## ---------------------------------------------------------------------------

get_intro_data_tbl <- function(mean = c(7, 7, 7), sd = c(2, 2, 2), ng = c(4, 3, 5)) {
  assump_tbl <- tibble(rsp = c(rnorm(ng[1], mean[1], sd[1]),
                               rnorm(ng[2], mean[2], sd[2]),
                               rnorm(ng[3], mean[3], sd[3])),
                       trt = as_factor(rep(c("A", "B", "C"), ng))) 
  stat <- plyr::ddply(assump_tbl, "trt", summarize,
                      mean = mean(rsp, na.rm = TRUE),
                      sd = round(sd(rsp, na.rm = TRUE), 2),
                      var = round(var(rsp, na.rm = TRUE), 2),
                      max = max(mean - min(rsp), max(rsp) - mean)*1.05) |> 
    mutate(pos = c(2, 9, 18),
           rsp = 0)
  grand_mean <- mean(assump_tbl$rsp, na.rm = TRUE)
  plot_tbl <- assump_tbl |> 
    mutate(pos = c(seq(1, ng[1], 1), 
                   seq(from = ng[1] + 4, to = ng[1] + ng[2] + 3, by = 1), 
                   seq(ng[1] + ng[2] + 7, ng[1] + ng[2] + ng[3] + 6, 1)),
           grand_mean = grand_mean,
           grp_mean = rep(stat$mean, ng),
           beta = grp_mean - grand_mean,
           epsilon = rsp - grp_mean) 
  return(lst(plot = plot_tbl,
             stat = stat,
             grand_mean = grand_mean))
}

## ---------------------------------------------------------------------------

get_ex_lst_plot <- function(ex_lst){
  p <- ex_lst$plot |> 
    ggplot() +
    aes(pos, rsp) +
    theme_minimal() +
    geom_hline(yintercept = ex_lst$grand_mean, color = "gray75", linewidth = 1) +
    geom_point2(color = "gray25") +
    geom_segment(x = 1, y = ex_lst$stat$mean[1], xend = 7, yend = ex_lst$stat$mean[1], 
                 color = "#D55E00", linewidth = 1) +
    annotate("text", x = 7.5, y = ex_lst$stat$mean[1], label = expression(bar(y)[A.1]), 
             size = 4, color = "#D55E00", hjust = "left") +
    geom_segment(x = 11, y = ex_lst$stat$mean[2], xend = 17, yend = ex_lst$stat$mean[2],
                 color = "#0072B2", linewidth = 1) +
    annotate("text", x = 17.5, y = ex_lst$stat$mean[2], label = expression(bar(y)[A.2]), 
             size = 4, color = "#0072B2", hjust = "left") +
    geom_segment(x = 21, y = ex_lst$stat$mean[3], xend = 27, yend = ex_lst$stat$mean[3],
                 color = "#009E73", linewidth = 1) +
    annotate("text", x = 27.5, y = ex_lst$stat$mean[3], label = expression(bar(y)[A.3]), 
             size = 4, color = "#009E73", hjust = "left") +
    geom_errorbar(data = ex_lst$stat, aes(ymin = mean - max, ymax = mean + max), 
                  width = 0.5, color = c("#D55E00", "#0072B2", "#009E73"), linetype = 11)  +
    scale_x_continuous(breaks = c(4, 14, 24), labels = c("A.1", "A.2", "A.3"),
                       limits = c(NA, 29))  +
    scale_y_continuous(breaks = ex_lst$grand_mean, labels = expression(beta[0]),
                       limits = c(-4, 14)) +  
    theme(axis.text = element_text(size = 14),
          axis.text.y = element_text(size = 16),
          plot.title = element_text(size = 17),
          plot.subtitle = element_text(size = 12, face = "italic")) + 
    labs(x = "", y = "") 
  return(p)
}


## ---------------------------------------------------------------------------


get_ex_lst_intro_plot <- function(ex_lst){
  p <- ex_lst$plot |> 
    ggplot() +
    aes(pos, rsp) +
    theme_minimal() +
    geom_hline(yintercept = ex_lst$grand_mean, color = "gray75", linewidth = 1) +
    geom_point(color = "gray50", size = 2) +
    geom_segment(x = 1, y = ex_lst$stat$mean[1], xend = 4, yend = ex_lst$stat$mean[1], 
                 color = "#D55E00", linewidth = 1.5) +
    annotate("text", x = 4.5, y = ex_lst$stat$mean[1], label = expression(bar(y)[A.1]), 
             size = 5, color = "#D55E00", hjust = "left") +
    geom_segment(x = 8, y = ex_lst$stat$mean[2], xend = 10, yend = ex_lst$stat$mean[2],
                 color = "#0072B2", linewidth = 1.5) +
    annotate("text", x = 10.5, y = ex_lst$stat$mean[2], label = expression(bar(y)[A.2]), 
             size = 5, color = "#0072B2", hjust = "left") +
    geom_segment(x = 14, y = ex_lst$stat$mean[3], xend = 18, yend = ex_lst$stat$mean[3],
                 color = "#009E73", linewidth = 1.5) +
    annotate("text", x = 18.5, y = ex_lst$stat$mean[3], label = expression(bar(y)[A.3]), 
             size = 5, color = "#009E73", hjust = "left") +
    scale_x_continuous(breaks = c(2, 9, 18), labels = c("A.1", "A.2", "A.3"),
                       limits = c(NA, 20))  +
    scale_y_continuous(breaks = ex_lst$grand_mean, labels = expression(beta[0]),
                       limits = c(0, 10)) +  
    theme(axis.text = element_text(size = 14),
          axis.text.y = element_text(size = 16),
          plot.title = element_text(size = 17),
          plot.subtitle = element_text(size = 12, face = "italic")) + 
    labs(x = "", y = "") 
  return(p)
}

## ---------------------------------------------------------------------------


p1_inter_line_theo <- tibble(mean = c(2, 5, 1, 5, 8, 4),
                             fA = c("A.1", "A.2", "A.3", "A.1", "A.2", "A.3"),
                             fB = c("B.1", "B.1", "B.1", "B.2", "B.2", "B.2")) |> 
  ggplot(aes(fA, mean, color = fB, group = fB)) +
  theme_minimal() +
  geom_point() +
  geom_line() +
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 2),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 16, face = 2),
        axis.text.x = element_text(size = 14),        
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        legend.position = "top") + 
  labs(x = "Faktor A", y = "", color = "Faktor B",
       title = "Keine Interaktion",
       subtitle = "Linien laufen parallel",
       caption = expression(p-Wert~f[A]%*%f[B]%~~%0.80)) +
  scale_color_okabeito() +
  ylim(0, 10)

p2_inter_line_theo <- tibble(mean = c(4, 5, 1, 5, 9, 1.5),
                             fA = c("A.1", "A.2", "A.3", "A.1", "A.2", "A.3"),
                             fB = c("B.1", "B.1", "B.1", "B.2", "B.2", "B.2")) |> 
  ggplot(aes(fA, mean, color = fB, group = fB)) +
  theme_minimal() +
  geom_point() +
  geom_line() +
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 2),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 16, face = 2),
        axis.text.x = element_text(size = 14),        
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        legend.position = "top") + 
  labs(x = "Faktor A", y = "", color = "Faktor B",
       title = "Schwache Interaktion",
       subtitle = "Linien laufen aufeinander zu",
       caption = expression(p-Wert~f[A]%*%f[B]%~~%0.05)) +
  scale_color_okabeito() +
  ylim(0, 10)

p3_inter_line_theo <- tibble(mean = c(4, 5, 4, 5, 9, 1.5),
                             fA = c("A.1", "A.2", "A.3", "A.1", "A.2", "A.3"),
                             fB = c("B.1", "B.1", "B.1", "B.2", "B.2", "B.2")) |> 
  ggplot(aes(fA, mean, color = fB, group = fB)) +
  theme_minimal() +
  geom_point() +
  geom_line() +
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 2),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 16, face = 2),
        axis.text.x = element_text(size = 14),        
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        legend.position = "top") + 
  labs(x = "Faktor A", y = "", color = "Faktor B",
       title = "Starke Interaktion",
       subtitle = "Linien kreuzen sich",
       caption = expression(p-Wert~f[A]%*%f[B]%~~%0.01)) +
  scale_color_okabeito() +
  ylim(0, 10)



p1_inter_bar_theo <- tibble(mean = c(2, 5, 1, 5, 8, 4),
                            sd = c(1.1, 1.2, 0.34, 0.9, 0.25, 1.2),
                            fA = c("A.1", "A.2", "A.3", "A.1", "A.2", "A.3"),
                            fB = c("B.1", "B.1", "B.1", "B.2", "B.2", "B.2")) |> 
  ggplot(aes(fA, mean, fill = fB, group = fB)) +
  theme_minimal() +
  geom_bar(stat = "identity", 
           position = position_dodge(width = 0.9, preserve = "single")) +
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd),
                width = 0.2,  
                position = position_dodge(width = 0.9, preserve = "single")) +
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 2),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 16, face = 2),
        axis.text.x = element_text(size = 14),        
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        legend.position = "top") + 
  labs(x = "Faktor A", y = "", fill = "Faktor B",
       title = "Starke Interaktion",
       subtitle = "Säulen folgen gleichem Muster",
       caption = expression(p-Wert~f[A]%*%f[B]%~~%0.01)) +
  scale_fill_okabeito() +
  ylim(0, 10)

p2_inter_bar_theo <- tibble(mean = c(4, 5, 1, 5, 9, 1.5),
                            sd = c(1.1, 1.2, 0.25, 0.9, 0.23, 1.2),
                            fA = c("A.1", "A.2", "A.3", "A.1", "A.2", "A.3"),
                            fB = c("B.1", "B.1", "B.1", "B.2", "B.2", "B.2")) |> 
  ggplot(aes(fA, mean, fill = fB, group = fB)) +
  theme_minimal() +
  geom_bar(stat = "identity", 
           position = position_dodge(width = 0.9, preserve = "single")) +
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd),
                width = 0.2,  
                position = position_dodge(width = 0.9, preserve = "single")) +
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 2),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 16, face = 2),
        axis.text.x = element_text(size = 14),        
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        legend.position = "top") + 
  labs(x = "Faktor A", y = "", fill = "Faktor B",
       title = "Starke Interaktion",
       subtitle = "Säulen sind unregelmäßig",
       caption = expression(p-Wert~f[A]%*%f[B]%~~%0.01)) +
  scale_fill_okabeito() +
  ylim(0, 10)


p3_inter_bar_theo <- tibble(mean = c(4, 5, 4, 5, 9, 1.5),
                            sd = c(1.1, 1.2, 1.1, 0.9, 0.85, 1.2),
                            fA = c("A.1", "A.2", "A.3", "A.1", "A.2", "A.3"),
                            fB = c("B.1", "B.1", "B.1", "B.2", "B.2", "B.2")) |> 
  ggplot(aes(fA, mean, fill = fB, group = fB)) +
  theme_minimal() +
  geom_bar(stat = "identity", 
           position = position_dodge(width = 0.9, preserve = "single")) +
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd),
                width = 0.2,  
                position = position_dodge(width = 0.9, preserve = "single")) +
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 2),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 16, face = 2),
        axis.text.x = element_text(size = 14),        
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        legend.position = "top") + 
  labs(x = "Faktor A", y = "", fill = "Faktor B",
       title = "Starke Interaktion",
       subtitle = "Säulen sind stark unregelmäßig",
       caption = expression(p-Wert~f[A]%*%f[B]%~~%0.01)) +
  scale_fill_okabeito() +
  ylim(0, 10)


p1_inter_box_theo <- tibble(rsp = c(rnorm(17, 2, 1), rnorm(17, 5, 1), rnorm(17, 1, 1),
                                    rnorm(17, 5, 1), rnorm(17, 8, 1), rnorm(17, 4, 1)),
                            fA = rep(c("A.1", "A.2", "A.3"), each = 17, times = 2),
                            fB = rep(c("B.1", "B.2"), each = 3*17*2/2)) |> 
  ggplot(aes(fA, rsp, fill = fB)) +
  theme_minimal() +
  geom_boxplot(outliers = FALSE) +
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 2),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 16, face = 2),
        axis.text.x = element_text(size = 14),        
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        legend.position = "top") + 
  labs(x = "Faktor A", y = "", fill = "Faktor B",
       title = "Keine Interaktion",
       subtitle = "Linien laufen parallel",
       caption = expression(p-Wert~f[A]%*%f[B]%~~%0.80)) +
  scale_fill_okabeito() +
  ylim(0, 10)

p2_inter_box_theo <- tibble(rsp = c(rnorm(17, 4, 1), rnorm(17, 5, 1), rnorm(17, 1, 1),
                                    rnorm(17, 5, 1), rnorm(17, 9, 1), rnorm(17, 1.5, 1)),
                            fA = rep(c("A.1", "A.2", "A.3"), each = 17, times = 2),
                            fB = rep(c("B.1", "B.2"), each = 3*17*2/2)) |> 
  ggplot(aes(fA, rsp, fill = fB)) +
  theme_minimal() +
  geom_boxplot(outliers = FALSE) +
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 2),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 16, face = 2),
        axis.text.x = element_text(size = 14),        
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        legend.position = "top") + 
  labs(x = "Faktor A", y = "", fill = "Faktor B",
       title = "Keine Interaktion",
       subtitle = "Linien laufen parallel",
       caption = expression(p-Wert~f[A]%*%f[B]%~~%0.05)) +
  scale_fill_okabeito() +
  ylim(0, 10)

p3_inter_box_theo <- tibble(rsp = c(rnorm(17, 4, 1), rnorm(17, 5, 1), rnorm(17, 4, 1),
                                    rnorm(17, 5, 1), rnorm(17, 9, 1), rnorm(17, 1.5, 1)),
                            fA = rep(c("A.1", "A.2", "A.3"), each = 17, times = 2),
                            fB = rep(c("B.1", "B.2"), each = 3*17*2/2)) |> 
  ggplot(aes(fA, rsp, fill = fB)) +
  theme_minimal() +
  geom_boxplot(outliers = FALSE) +
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = 2),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 16, face = 2),
        axis.text.x = element_text(size = 14),        
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        legend.position = "top") + 
  labs(x = "Faktor A", y = "", fill = "Faktor B",
       title = "Keine Interaktion",
       subtitle = "Linien laufen parallel",
       caption = expression(p-Wert~f[A]%*%f[B]%~~%0.01)) +
  scale_fill_okabeito() +
  ylim(0, 10)

## ---------------------------------------------------------------------------

p1_inter_venn <- ggplot() +
  theme_void() +
  geom_circle(aes(x0 = 1, y0 = 1, r = 0.9), 
              fill = "#0072B2", alpha = 0.5) +
  geom_circle(aes(x0 = 2.25, y0 = 1, r = 1.15), 
              fill = "#D55E00", alpha = 0.5) +
  geom_ellipse(aes(x0 = (1+2.25)/2, y0 = 0.7, a = 2, b = 1.75, angle = 0),
               fill = "black", alpha = 0.1) +
  ylim(-1.25, 2.5) + xlim(-0.5, 3.75) +
  annotate("text", x = 0.7, y = 1, label = expression(bold(SS[A])), size = 7) +
  annotate("text", x = 2.5, y = 1, label = expression(bold(SS[B])), size = 7) +
  annotate("text", x = 1.5, y = 1, label = expression(bold(SS[A%*%B])), size = 7) +
  annotate("text", x = 1, y = -0.5, label = expression(bold(SS[Error])), size = 7) +
  annotate("label", x = 3.25, y = -0.75, label = expression(bold(SS[Total])), size = 9) +
  labs(x = "", y = "", fill = "",
       title = "Starke Interaktion",
       subtitle = "Sum of Squares (SS) lassen sich nicht eindeutig zuordnen") +
  theme(plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        legend.position = "top") 

p2_inter_venn <- ggplot() +
  theme_void() +
  geom_circle(aes(x0 = 0.75, y0 = 1, r = 0.8), 
              fill = "#0072B2", alpha = 0.5) +
  geom_circle(aes(x0 = 2.5, y0 = 1, r = 0.9), 
              fill = "#D55E00", alpha = 0.5) +
  geom_ellipse(aes(x0 = (1+2.25)/2, y0 = 0.7, a = 2, b = 1.75, angle = 0),
               fill = "black", alpha = 0.1) +
  ylim(-1.25, 2.5) + xlim(-0.5, 3.75) +
  annotate("text", x = 0.75, y = 1, label = expression(bold(SS[A])), size = 7) +
  annotate("text", x = 2.5, y = 1, label = expression(bold(SS[B])), size = 7) +
  annotate("text", x = 1, y = -0.5, label = expression(bold(SS[Error])), size = 7) +
  annotate("label", x = 3.25, y = -0.75, label = expression(bold(SS[Total])), size = 9) +
  labs(x = "", y = "", fill = "",
       title = "Keine Interaktion",
       subtitle = "Sum of Squares (SS) lassen sich eindeutig zuordnen") +
  theme(plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        legend.position = "top") 

## ---------------------------------------------------------------------------

p1_eta_venn <- ggplot() +
  theme_void() +
  geom_circle(aes(x0 = 1, y0 = 1, r = 0.9), 
              fill = "#0072B2", alpha = 0.5) +
  geom_ellipse(aes(x0 = (1+2.25)/2, y0 = 0.7, a = 2, b = 1.75, angle = 0),
               fill = "black", alpha = 0.1) +
  ylim(-1.1, 2.5) + xlim(-0.5, 3.8) +
  annotate("text", x = 0.7, y = 1, label = expression(bold(SS[A])), size = 7) +
  annotate("text", x = 1, y = -0.5, label = expression(bold(SS[Error])), size = 7) +
  annotate("label", x = 3.25, y = -0.75, label = expression(bold(SS[Total])), size = 9) +
  labs(x = "", y = "", fill = "",
       title = "Geringer Effekt",
       subtitle = expression(SS[A]~erklärt~geringen~Anteil~von~SS[Total]),
       caption = expression(eta^2%~~%0.2)) +
  theme(plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        legend.position = "top",
        plot.caption = element_text(size = 12)) 

p2_eta_venn <- ggplot() +
  theme_void() +
  geom_ellipse(aes(x0 = 1.9, y0 = 0.7, a = 1.7, b = 1.6, angle = 0),
               fill = "#0072B2", alpha = 0.5) +
  geom_ellipse(aes(x0 = (1+2.25)/2, y0 = 0.7, a = 2, b = 1.75, angle = 0),
               fill = "black", alpha = 0.1) +
  ylim(-1.1, 2.5) + xlim(-0.5, 3.8) +
  annotate("text", x = 1.9, y = 0.7, label = expression(bold(SS[A])), size = 7) +
  annotate("label", x = -0.1, y = 0.7, label = expression(bold(SS[Error])), size = 7,
           fill = "gray90") +
  annotate("label", x = 3.25, y = -0.75, label = expression(bold(SS[Total])), size = 9) +
  labs(x = "", y = "", fill = "",
       title = "Starker Effekt",
       subtitle = expression(SS[A]~erklärt~großen~Anteil~von~SS[Total]),
       caption = expression(eta^2%~~%0.8)) +
  theme(plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 12, face = "italic"),
        legend.position = "top",
        plot.caption = element_text(size = 12)) 