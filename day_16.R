library(magrittr)
library(R.utils)

input <- readr::read_lines("files/day_16_input.txt") %>% strsplit("") %>%
  purrr::map(~strtoi(.x, 16) %>% intToBin()) %>%
  unlist() %>%
  paste0(collapse = "")

test <- "9C0141080250320F1802104A08" %>% strsplit("") %>%
  purrr::map(~strtoi(.x, 16) %>% intToBin()) %>%
  unlist() %>%
  paste0(collapse = "")


bitstoval <- function(bits) {
  t <- strsplit(bits, "") %>% unlist() %>% split(rep(1:(length(.)/5), each = 5)) %>%
    purrr::map(~paste0(.x, collapse = "") %>% substr(2, 5)) %>%
    unlist() %>%
    paste0(collapse = "")
  sum(2^(which(rev(unlist(strsplit(t, "")) == 1))-1))
}

decode <- function(input) {

  #browser()
  out <- list()
  out$version <- substr(input, 1,3) %>% strtoi(2)
  out$type <- substr(input, 4,6) %>% strtoi(2)
  out$bit_length <- 6
  if(out$type == 4) {
    while(substr(input, out$bit_length+1, out$bit_length+1) != "0")
      out$bit_length <- out$bit_length+5
    out$bit_length <- out$bit_length+5
    out$value <- bitstoval(substr(input, 7, out$bit_length))
    #out$bit_length <- out$bit_length + 4 - out$bit_length %% 4
    return(out)
  }
  out$indicator <- substr(input, 7,7)
  out$bit_length <- 7

  if(out$indicator == "0") {
    out$packets_length <- strtoi(substr(input, 8, 22), 2)
    out$bit_length <- 22 + out$packets_length
    out$sub_packets <- list()

    i <- 1
    next_packet <- T
    sub_packets <- substr(input, 23, out$bit_length)
    while(next_packet) {
      out$sub_packets[[i]] <- decode(sub_packets)
      if(out$sub_packets[[i]]$bit_length+1 <= stringi::stri_length(sub_packets))
        sub_packets <- substr(sub_packets, out$sub_packets[[i]]$bit_length+1, stringi::stri_length(sub_packets))
      else
        next_packet <- F
      i <- i+1
    }
    out$packets_count <- i-1
    #out$bit_length <- out$bit_length + 8 - out$bit_length %% 8
    return(out)
  }

  out$packets_count <- strtoi(substr(input, 8, 18), 2)
  out$bit_length <- 18
  out$sub_packets <- list()
  sub_packets <- substr(input, 19, stringi::stri_length(input))
  for(i in 1:out$packets_count) {
    out$sub_packets[[i]] <- decode(sub_packets)
    if(out$sub_packets[[i]]$bit_length+1 <= stringi::stri_length(sub_packets)) {
      sub_packets <- substr(sub_packets, out$sub_packets[[i]]$bit_length+1, stringi::stri_length(sub_packets))
    } else {
      sub_packets <- ""
    }
    out$bit_length <- out$bit_length + out$sub_packets[[i]]$bit_length
    if(sub_packets == "")
      break
  }
  #out$bit_length <- out$bit_length + 8 - out$bit_length %% 8
  return(out)
}

data <- decode(input)
data <- decode("00111000000000000110111101000101001010010001001000000000")

flatten <- unlist(data)
flatten[grep("version", names(flatten))] %>% as.numeric() %>% sum()

eval_trans <- function(x) {

  if(x$type == 4)
    return(x$value)

  if(x$type < 4) {
    act <- switch(as.character(x$type),
                  "0" = sum,
                  "1" = prod,
                  "2" = min,
                  "3" = max)

    return(lapply(x$sub_packets, eval_trans) %>% unlist() %>% act())

  } else {
    act <- switch(as.character(x$type),
                  "5" = get(">"),
                  "6" = get("<"),
                  "7" = get("=="))

    return(as.integer(act(eval_trans(x$sub_packets[[1]]), eval_trans(x$sub_packets[[2]]))))
  }

}

answer_2 <- data %>% eval_trans()
