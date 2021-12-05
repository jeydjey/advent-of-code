
library(magrittr)

data <- read.table("files/day_05_input.txt", sep = " ") %>%
  dplyr::select(V1, V3) %>%
  tidyr::separate(col = V1, into = c("x1", "y1"), sep = ",", convert = T) %>%
  tidyr::separate(col = V3, into = c("x2", "y2"), sep = ",", convert = T)


d1 <- data %>%
  tibble::as_tibble() %>%
  dplyr::filter(x1 == x2 | y1 == y2) %>%
  dplyr::mutate(lstart = ifelse(x1 == x2, y1, x1),
                   lend = ifelse(x1 == x2, y2, x2)) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(line = list(lstart:lend)) %>%
  tidyr::unnest(line) %>%
  dplyr::mutate(x = ifelse(x1 == x2, x1, line),
                y = ifelse(x1 == x2, line, y1)) %>%
  dplyr::select(x,y)

answer_1 <- d1 %>% dplyr::count(x, y) %>% dplyr::filter(n>1) %>% nrow()

d2 <- data %>%
  tibble::as_tibble() %>%
  dplyr::filter(x1 != x2, y1 != y2) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(x = list(x1:x2), y = list(y1:y2)) %>%
  tidyr::unnest(c(x, y)) %>%
  dplyr::select(x,y) %>%
  dplyr::bind_rows(d1)

answer_2 <- d2 %>% dplyr::count(x, y) %>% dplyr::filter(n>1) %>% nrow()
