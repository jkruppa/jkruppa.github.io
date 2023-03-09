library(tidyverse) ## laden eines zusätzlichen Pakets
library(readxl)

example_tbl <- read_excel("C:/Users/jokruppa/Desktop/example_data.xlsx")

example_tbl <- example_tbl %>% 
  mutate(N = as_factor(N),
         block = as_factor(block))

ggplot(data = example_tbl, 
       aes(x = N, y = freshmatter, fill = block)) +
  theme_bw() +
  geom_boxplot() +
  ylim(0, NA) +
  labs(x = "Mein Stickstoff!", 
       y = "Frischegewicht im Mondlicht",
       fill = "Tisch")

