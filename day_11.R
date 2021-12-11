library(magrittr)

data <- read.table("files/day_11_input.txt") %>%
  tidyr::separate(V1, paste0("V", 0:10), sep = "", convert = T) %>%
  dplyr::select(-V0)

d <- list()

d$data <- data %>% as.matrix()

d$flash <- integer(100)

for(i in 1:100) {
  d$data <- d$data+1

  d <- flash(d, i)

}

answer_1 <- d$flash %>% sum()

flash <- function(d, i, flashed = NULL) {

  flasher <- which(d$data>9, arr.ind = T)
  d$flash[i] <- d$flash[i] + nrow(flasher)
  if(nrow(flasher)==0) return(d)

  for(j in 1:nrow(flasher)) {
    neigh <- neighbours(flasher[j,])
    d$data[neigh] <- d$data[neigh] +1
  }

  if(!is.null(flashed)) {
    flashed <- rbind(flashed, flasher)
  } else {
    flashed <- flasher
  }

  d$data[flashed] <- 0

  flash(d, i, flashed)

}

neighbours <- function(input, upper = 10) {

  expand.grid(max(input[1]-1, 1):min(input[1]+1, upper), max(input[2]-1, 1):min(input[2]+1, upper)) %>%
    as.matrix()

}

d <- list()

d$data <- data %>% as.matrix()

d$flash <- integer(10000)

bool <- T
i <- 0
while(bool) {
  i <- i+1
  d$data <- d$data+1
  d <- flash(d, i)
  if(d$flash[i] == 100 || i == 10000) {
    bool <- F
    cat(i)
  }

}
