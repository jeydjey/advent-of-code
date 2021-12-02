library(magrittr)
library(dplyr)
library(readr)

data <- read_delim("files/day_02_input.txt", col_names = c("direction", "move"), col_types = "ci",  delim = " ")

horizontal <- data %>%
  filter(direction == "forward") %>%
  summarise(move = sum(move)) %>%
  pull

vertical <- data %>%
  filter(direction != "forward") %>%
  mutate(move = ifelse(direction == "up", move * -1, move)) %>%
  summarise(move = sum(move)) %>%
  pull

answer_1 <- horizontal * vertical

pos <- data %>%
  mutate(move = ifelse(direction == "up", move * -1, move)) %>%
  mutate(aim = ifelse(direction != "forward", move, 0),
         horizontal = ifelse(direction == "forward", move, 0)) %>%
  mutate(vertical = cumsum(aim) * horizontal)

answer_2 <- sum(pos$horizontal) * sum(pos$vertical)
