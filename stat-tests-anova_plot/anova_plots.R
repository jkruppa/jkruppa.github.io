
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
  geom_segment(x = 1, y = 8.13, xend = 7, yend = 8.13, color = "#E69F00", 
               linewidth = 1) +
  geom_segment(x = 4.5, y = grand_mean, xend = 4.5, yend = 8.13, color = "#E69F00", 
               linewidth = 0.5) +
  annotate("text", x = 4.75, y = grand_mean + 0.35, label = expression(beta[A.1]), size = 5,
           color = "#E69F00", hjust = "left") +
  annotate("text", x = -1.25, y = 8.13, label = expression(bar(y)[A.1]), size = 5,
           color = "#E69F00", hjust = "left") +
  geom_segment(x = 11, y = 4.74, xend = 17, yend = 4.74, color = "#56B4E9", 
               linewidth = 1) +
  geom_segment(x = 14.5, y = grand_mean, xend = 14.5, yend = 4.74, color = "#56B4E9", 
               linewidth = 0.5) +
  annotate("text", x = 12.25, y = grand_mean - 1, label = expression(beta[A.2]), size = 5,
           color = "#56B4E9", hjust = "left") +
  annotate("text", x = 17.5, y = 4.74, label = expression(bar(y)[A.2]), size = 5,
           color = "#56B4E9", hjust = "left") +
  geom_segment(x = 21, y = 9.16, xend = 27, yend = 9.16, color = "#009E73",
               linewidth = 1) +
  geom_segment(x = 24.5, y = grand_mean, xend = 24.5, yend = 9.16, color = "#009E73",
               linewidth = 0.5) +
  annotate("text", x = 24.75, y = grand_mean + 0.8, label = expression(beta[A.3]), size = 5,
           color = "#009E73", hjust = "left") +
  annotate("text", x = 18.5, y = 9.16, label = expression(bar(y)[A.3]), size = 5,
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
  scale_y_continuous(breaks = grand_mean, labels = expression(bar(Y)[Total])) +  
  theme(axis.text = element_text(size = 14),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 16, face = 2)) + 
  labs(x = "", y = "")
