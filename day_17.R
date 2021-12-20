
library(magrittr)

target <- readr::read_lines("files/day_17_input.txt") %>%
  strsplit(" ") %>%
  unlist() %>%
  .[grep("(x|y)", .)]

tx <- target[1] %>%
  gsub("(x=|,)", "", .) %>%
  gsub("\\.\\.", ":", .) %>%
  rlang::parse_expr(.) %>%
  eval()

ty <- target[2] %>%
  gsub("(x=|,)", "", .) %>%
  gsub("\\.\\.", ":", .) %>%
  rlang::parse_expr(.) %>%
  eval()

#maximum x velocity is 22, minimum is 20 to land within x boundaries

sim_throw <- function(x_velo, y_velo, x_bound, y_bound) {
  cat(paste0("y_velo: ", y_velo), sep = "\n")
  x <- 0
  y <- 0
  while(T) {
    x <- x+x_velo
    y <- y+y_velo
    x_velo <- max(0, x_velo-1)
    y_velo <- y_velo-1
    if(x %in% x_bound & y %in% y_bound)
      return(T)
    if(x > max(x_bound) | (x_velo == 0 & x < min(x_bound)) | y < min(y_bound))
      return(F)
  }
}

res <- purrr::map(1:300, ~sim_throw(22, .x, tx, ty)) %>%
  setNames(1:300)

res %>%
  unlist() %>%
  which(.) %>%
  max()

res <- 114*(114+1)/2

#pt2

poss_x <- max(tx):20
poss_y <- min(ty):114

poss <- expand.grid(poss_x, poss_y)

res2 <- poss %>%
  purrr::pmap(~sim_throw(.x, .y, tx, ty))

res2 %>%
  unlist() %>%
  which(.) %>%
  length()
