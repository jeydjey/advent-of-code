
library(magrittr)

data <- readr::read_lines("files/day_15_input.txt") %>% strsplit("") %>% unlist() %>% as.numeric() %>%
  matrix(nrow = 100, byrow = T)

spath <- expand.grid(1:100, 1:100) %>%
  dplyr::arrange(Var1+Var2) %>%
  setNames(c("row", "col"))

path <- matrix(as.numeric(NA), 100, 100)

data[1,1] <- 0
path[1,1] <- 0

for(i in 1:nrow(spath)) {
  rw <- spath[i,1]
  cl <- spath[i,2]
  path[rw,cl] <- path[rw,cl] + data[rw,cl]
  if(rw <= 99)
    path[rw+1,cl] <- min(path[rw+1,cl], path[rw,cl], na.rm = T)
  if(cl <= 99)
    path[rw,cl+1] <- min(path[rw,cl+1], path[rw,cl], na.rm = T)
}

answer_1 <- path[100,100]




