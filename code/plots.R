# --------------------------------------------------
# February, 2021
# Author: Bruna Wundervald
# --------------------------------------------------
library(tidyverse)
options(scipen = 10000)

source("code/data_reading.R")

# Figure 3 --------------------------------------------
fig_3 <- data_list[[1]] %>% 
  mutate(ind = 1:n()) %>% 
  gather(genre, value, -ind) 


p3 <- fig_3 %>% 
  na.omit() %>% 
  group_by(genre) %>% 
  mutate(mean_val = mean(value)) %>% 
  ggplot(aes(x = ind, y = value)) +
  geom_line(aes(y = mean_val), colour = 1, 
            size = 0.5, linetype = "dashed") +
  geom_line(colour = "#f5c04a", size = 0.5) +
  facet_wrap( ~ genre, scales = 'free_x', ncol = 2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 7)) +
  theme_minimal(18) +
  theme(axis.text.x = element_text(angle = 90),
        strip.text  = element_text(face = 'bold')) +
  labs(x = "Time", y = "DFA exponents")


ggsave(plot = p3, filename = "plots/figure_3.pdf", 
       width = 13, height = 10)

# Figure 4 --------------------------------------------
fig_4 <- data_list[[2]] %>% 
  mutate(ind = 1:n()) %>% 
  gather(genre, value, -ind) 


p4 <- fig_4 %>% 
  na.omit() %>% 
  ggplot(aes(x = ind, y = value)) +
  #geom_line(aes(y = mean_val), colour = 1, 
  #          size = 0.3, linetype = "dashed") +
  geom_line(colour = "#f5c04a", size = 0.8) +
  facet_wrap( ~ genre, scales = 'free_x', ncol = 2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 7)) +
  theme_minimal(18) +
  theme(axis.text.x = element_text(angle = 90),
        strip.text  = element_text(face = 'bold')) +
  labs(x = "Time", y = "EI values")

p4
ggsave(plot = p4, filename = "plots/figure_4.pdf", 
       width = 13, height = 10)

# Figure 5 --------------------------------------------
fig_5 <- data_list[[3]] %>% 
  mutate(ind = 1:n()) %>% 
  gather(genre, value, -ind) 


p5 <- fig_5 %>% 
  na.omit() %>% 
  group_by(genre) %>% 
  mutate(mean_val = mean(value)) %>% 
  ggplot(aes(x = ind, y = value)) +
  geom_line(aes(y = mean_val), colour = 1, 
           size = 0.3, linetype = "dashed") +
  geom_line(colour = "#f5c04a", size = 0.5) +
  facet_wrap( ~ genre, scales = 'free_x', ncol = 2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 7)) +
  theme_minimal(18) +
  theme(axis.text.x = element_text(angle = 90),
        strip.text  = element_text(face = 'bold')) +
  labs(x = "Time", y = "DFA exponents")

#p5
ggsave(plot = p5, filename = "plots/figure_5.pdf", 
       width = 7, height = 5)
