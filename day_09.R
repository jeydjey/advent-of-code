
library(magrittr)

data <- read.table("files/day_09_input.txt", sep = "", colClasses = "character") %>%
  tidyr::separate(col = V1, into = paste0("V", 0:100), sep = "", convert = T) %>%
  dplyr::select(-V0)

hd <- data %>%
  dplyr::mutate(dplyr::across(.fns = ~(.x < dplyr::lag(.x, default = 9) & .x < dplyr::lead(.x, default = 9))))

vd <- data %>%
  t() %>%
  tibble::as_tibble() %>%
  dplyr::mutate(dplyr::across(.fns = ~(.x < dplyr::lag(.x, default = 9) & .x < dplyr::lead(.x, default = 9)))) %>%
  t %>%
  tibble::as_tibble()

match <- dplyr::bind_rows(
  which(hd == T, arr.ind = T) %>% tibble::as_tibble(),
  which(vd == T, arr.ind = T) %>% tibble::as_tibble()
) %>%
  dplyr::count(row, col) %>%
  dplyr::filter(n>1) %>%
  dplyr::select(-n) %>%
  as.matrix()

answer_1 <- (data[match]+1) %>% sum()


d2 <- data %>%
  dplyr::mutate(dplyr::across(.fns = ~as.numeric(.x!=9))) %>%
  as.matrix()

d3 <- data %>%
  as.matrix()

d <- d2

i <- 2
for(x in 1:100) {
  for(y in 1:100) {

    if(d[x,y]==0)
      next

    if(d[x,y]==1) {
      if(y>1) {
        if(d[x,y-1]>1) {
          d[x,y] <- d[x,y-1]
        }
      }
    }
    if(d[x,y]==1) {
      if(x>1) {
        if(d[x-1,y]>1) {
          d[x,y] <- d[x-1,y]
        }
      }
    }
    if(d[x,y]>1) {
      if(x>1) {
        if(d[x-1,y]>1 && d[x-1,y] != d[x,y]) {
          d[which(d == d[x-1,y], arr.ind = T)] <- d[x,y]
        }
      }
      if(y>1) {
        if(d[x,y-1]>1 && d[x,y-1] != d[x,y]) {
          d[which(d == d[x,y-1], arr.ind = T)] <- d[x,y]
        }
      }
    }
    if(d[x,y]==1) {
      d[x,y] <- i
      i <- i+1
    }

  }
}

res <- d %>%
  tibble::as_tibble() %>%
  tidyr::pivot_longer(tidyr::everything(), names_to = "cols", values_to = "val") %>%
  dplyr::filter(val > 0) %>%
  dplyr::count(val) %>%
  dplyr::slice_max(order_by = n, n = 3)

answer_2 <- res$n %>% prod()

