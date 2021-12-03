library(magrittr)
library(dplyr)
library(readr)
library(tidyr)

data <- read_delim("files/day_03_input.txt", col_names = c("bits"), col_types = "c",  delim = " ") %>%
  tidyr::separate(col = bits, into = paste0("bit", 0:12), sep = "", remove = T, convert = T) %>%
  select(-bit0)

count <- data %>%
  summarise(dplyr::across(.fns = mean))

gamma <- count %>%
  mutate(across(.fns = ~as.integer(.x>0.5))) %>%
  unite(col = "bits", dplyr::all_of(paste0("bit", 1:12)), sep = "")

epsilon <- count %>%
  mutate(across(.fns = ~as.integer(.x<0.5))) %>%
  unite(col = "bits", dplyr::all_of(paste0("bit", 1:12)), sep = "")

answer1 <- gamma$bits %>% strtoi(2) * epsilon$bits %>% strtoi(2)

oxy <- data

for(i in 1:12) {
  if(nrow(oxy)==1)
    next
  oxy <- oxy %>%
    filter(!!rlang::sym(paste0("bit", i)) == as.integer(mean(!!rlang::sym(paste0("bit", i)))>=0.5))
}

oxy_val <- oxy %>% unite(col = "bits", dplyr::all_of(paste0("bit", 1:12)), sep = "") %>% pull() %>%
  strtoi(2)

co2 <- data

for(i in 1:12) {
  if(nrow(co2)==1)
    next
  co2 <- co2 %>%
    filter(!!rlang::sym(paste0("bit", i)) == as.integer(!(mean(!!rlang::sym(paste0("bit", i)))>=0.5)))
}

co2_val <- co2 %>% unite(col = "bits", dplyr::all_of(paste0("bit", 1:12)), sep = "") %>% pull() %>%
  strtoi(2)

answer2 <- co2_val * oxy_val
