library(magrittr)
library(dplyr)
library(readr)


data <- read.delim("files/day_01_input.txt", header = F) %>%
  as_tibble() %>%
  setNames("depth") %>%
  mutate(inc = depth > lag(depth))

answer_1 <- sum(data$inc, na.rm = T)

data2 <- data %>%
  select(depth) %>%
  mutate(three_meas = depth + lead(depth) + lead(depth, 2),
         inc = three_meas > lag(three_meas))

answer_2 <- sum(data2$inc, na.rm = T)
