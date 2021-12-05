library(magrittr)
library(rlang)

data <- readr::read_lines("files/day_04_input.txt")

num_draw <- data[1] %>%
  strsplit(",") %>%
  sapply(as.integer, simplify = T) %>%
  unlist()

boards <- data[2:length(data)] %>%
  .[sapply(., stringi::stri_length, simplify = T)>0] %>%
  gsub(pattern = "\\s{2}", replacement = " ", x = .) %>%
  gsub(pattern = "^\\s", replacement = "", x = .) %>%
  strsplit("\\s") %>%
  lapply(as.integer) %>%
  lapply(matrix, ncol = 5) %>%
  do.call(rbind, .)

tboards <- boards %>%
  split(rep(1:ceiling(nrow(.)/5), each = 5,
            length.out = nrow(.))) %>%
  lapply(matrix, ncol = 5) %>%
  lapply(t) %>%
  do.call(rbind, .)

matched <- tmatched <- matrix(nrow = 500, ncol = 6)

for(d in num_draw) {

  matched[which(boards==d, arr.ind = T)] <- d
  tmatched[which(tboards==d, arr.ind = T)] <- d
  matched[,6] <- rowSums(matched[,1:5])
  tmatched[,6] <- rowSums(tmatched[,1:5])

  win_row <- which(matched[,6]>0)
  win_col <- which(tmatched[,6]>0)

  if(length(win_row)>0 | length(win_col)>0){
    break
  }
}

mstart <- if(win_row %% 10 > 5) win_row %/% 10 * 10 + 6 else win_row %/% 10 * 10 +1

win_board <- boards[mstart:(mstart+4),]
answer_1 <- sum(win_board[which(is.na(matched[mstart:(mstart+4),1:5]), arr.ind = T)]) * d

call <- tibble::tibble(value = num_draw[,1], order = 1:100)

boards2 <- boards %>% as_tibble(.name_repair = "unique") %>%
  dplyr::mutate(rowIndex = rep(1:5, length.out = 500),
                board = rep(1:100, each =5)) %>%
  tidyr::pivot_longer(cols = c(1:5), names_to = "colIndex") %>%
  dplyr::mutate(colIndex = as.integer(factor(colIndex)),
                board = factor(board)) %>%
  dplyr::left_join(call, by = "value")

rbind(
  aggregate(order~colIndex+board, boards2, max) %>% aggregate(order~board, ., min),
  aggregate(order~rowIndex+board, boards2, max) %>% aggregate(order~board, ., min)
) %>%
  aggregate(order~board, ., min) %>%
  dplyr::slice(which.max(order))

boards2 %>% dplyr::filter(board==17,order>82) %>% dplyr::select(value) %>% sum() * dplyr::filter(call,order==82)[1,1]

