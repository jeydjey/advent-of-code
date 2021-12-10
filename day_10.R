
library(magrittr)

data <- read.table("files/day_10_input.txt", sep = "")

score <- c(")" = 3, "]" = 57, "}" = 1197, ">" = 25137, "0" = 0)

p <- c("(" = ")", "[" = "]", "{" = "}", "<" = ">")

input <- data$V1[1]

find_error <- function(input, p) {

  char <- stringi::stri_extract_all(input, regex = ".") %>% unlist()

  s <- character(length = length(char))

  i <- 1

  for(j in char) {

    if(j %in% names(p)) {
      s[i] <- j
      i <- i+1
    } else {
      if(i == 1 | p[s[i-1]] != j)
        return(j)
      i <- i-1
    }

  }

  return("0")
}

d1 <- data$V1 %>%
  purrr::map(~find_error(.x, p = p)) %>%
  unlist()

answer_1 <- d1 %>%
  dplyr::recode(!!!score) %>%
  sum()

score <- c(")" = 1, "]" = 2, "}" = 3, ">" = 4)

d2 <- data$V1[which(d1=="0")]

complete_error <- function(input, p, score) {

  char <- stringi::stri_extract_all(input, regex = ".") %>% unlist()

  s <- character(length = length(char))

  i <- 1

  for(j in char) {

    if(j %in% names(p)) {
      s[i] <- j
      i <- i+1
    } else {
      i <- i-1
      s[i] <- ""
    }
  }
  s <- s[which(s!="")]

  l <- dplyr::recode(rev(s), !!!p) %>%
    dplyr::recode(!!!score)

  r <- 0

  for(k in l) {
    r <- r*5+k
  }

  return(r)

}

answer_2 <- purrr::map(d2, ~complete_error(.x, p = p, score = score)) %>% unlist() %>% median()
