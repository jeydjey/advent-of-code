
library(magrittr)

data <- read.table("files/day_08_input.txt", sep = "|") %>%
  tibble::as_tibble() %>%
  dplyr::rename(patterns = V1, output = V2) %>%
  dplyr::mutate(dplyr::across(.fns = ~stringi::stri_trim_both(.x))) %>%
  dplyr::mutate(id = dplyr::row_number())

p1 <- data %>%
  dplyr::select(id, output) %>%
  dplyr::mutate(sep_out = stringi::stri_extract_all(output, regex = "\\w+")) %>%
  tidyr::unnest(sep_out) %>%
  dplyr::mutate(match = stringi::stri_length(sep_out) %in% c(2, 3, 4, 7))

answer_1 <- p1$match %>% sum()


p1 %>%
  dplyr::mutate(match = dplyr::recode(stringi::stri_length(sep_out),
                                      "2"=1, "3" = 7, "4" = 4, "7" = 8,
                                      .default = as.numeric(NA)))

encode <- function(pattern) {

  pattern <- pattern %>% stringi::stri_extract_all(regex = "\\w+") %>% unlist()

  code <- c(a = 2^6,
            b = 2^5,
            c = 2^4,
            d = 2^3,
            e = 2^2,
            f = 2^1,
            g = 2^0)

  #convert to code
  binary <- purrr::map(pattern, ~stringi::stri_extract_all(.x, regex = "\\w") %>%
                         unlist() %>%
                         dplyr::recode(!!!code) %>%
                         sum()) %>%
    unlist()

  d <- integer(10)
  s <- character(7)

  #find 1,2,3,4
  d[1] <- which(stringi::stri_length(pattern)==2)
  d[4] <- which(stringi::stri_length(pattern)==4)
  d[7] <- which(stringi::stri_length(pattern)==3)
  d[8] <- which(stringi::stri_length(pattern)==7)

  #find upper horizontal
  s[1] <- names(which(code == bitwXor(binary[d[7]], binary[d[1]])))

  #find lower horizontal
  t <- binary - bitwAnd(binary, bitwOr(binary[d[7]], binary[d[4]]))
  s[4] <- names(which(code == t[which.max(t %in% code)]))

  #find middle horizontal
  t <- binary - bitwAnd(binary, binary[d[1]] + code[s[1]] + code[s[4]])
  s[7] <- names(which(code == t[which.max(t %in% code)]))

  #find upper left
  s[6] <- names(which(code == bitwXor(binary[d[4]], binary[d[1]] + code[s[7]])))

  #find lower left
  s[5] <- names(which(code == bitwXor(binary[d[8]], binary[d[4]] + code[s[1]] + code[s[4]])))

  #find upper right
  t <- binary - bitwAnd(binary, code[s[1]] + code[s[4]] +  code[s[7]] + code[s[5]])
  s[2] <- names(which(code == t[which.max(t %in% code)]))

  #find lower right
  t <- binary[d[1]] - code[s[2]]
  s[3] <- names(which(code == t[which.max(t %in% code)]))



  return(
    c("1" = code[s[2]] + code[s[3]],
      "2" = code[s[1]] + code[s[2]] + code[s[7]] + code[s[5]] + code[s[4]],
      "3" = code[s[1]] + code[s[2]] + code[s[7]] + code[s[3]] + code[s[4]],
      "4" = code[s[6]] + code[s[7]] + code[s[2]] + code[s[3]],
      "5" = code[s[1]] + code[s[6]] + code[s[7]] + code[s[3]] + code[s[4]],
      "6" = code[s[1]] + code[s[6]] + code[s[7]] + code[s[3]] + code[s[4]] + code[s[5]],
      "7" = code[s[1]] + code[s[2]] + code[s[3]],
      "8" = sum(code),
      "9" = code[s[1]] + code[s[2]] + code[s[6]] + code[s[7]] + code[s[3]] + code[s[4]],
      "0" = code[s[1]] + code[s[2]] + code[s[3]] + code[s[4]] + code[s[5]] + code[s[6]]
    ) %>% setNames(paste0(c(1:9,0)))
  )

}

decode <- function(output, encoded) {

  output <- output %>% stringi::stri_extract_all(regex = "\\w+") %>% unlist()

  code <- c(a = 2^6,
            b = 2^5,
            c = 2^4,
            d = 2^3,
            e = 2^2,
            f = 2^1,
            g = 2^0)

  #convert to code
  binary <- purrr::map(output, ~stringi::stri_extract_all(.x, regex = "\\w") %>%
                         unlist() %>%
                         dplyr::recode(!!!code) %>%
                         sum()) %>%
    unlist()


  return(as.integer(paste0(names(sapply(binary, function(x) which(encoded == x), simplify = T)), collapse ="")))

}

encoded <- data$patterns %>%
  purrr::map(~encode(.x)) %>%
  .[[195]]

answer_2 <- unlist(purrr::map(1:length(encoded), ~decode(data$output[.x], encoded[[.x]]))) %>% sum()



