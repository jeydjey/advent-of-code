
library(magrittr)

data <- read.table("files/day_06_input.txt", sep = ",") %>%
  tidyr::pivot_longer(tidyselect::everything(), names_to = "lantern", values_to = "counter") %>%
  dplyr::mutate(lantern = as.integer(gsub("^(V)(\\d*)$", "\\2", lantern)),
                day = 0)

lanterns <- data

for(i in 1:80) {

  lanterns <- dplyr::mutate(lanterns, counter = counter-1) %>%
    dplyr::bind_rows(
      dplyr::filter(., counter == -1) %>%
        dplyr::mutate(counter = 8,
                      day = i)
    ) %>%
    dplyr::mutate(lantern = dplyr::row_number(),
                  counter = ifelse(counter == -1, 6, counter))

}

answer_1 <- nrow(lanterns)

lan2 <- data %>%
  dplyr::group_by(counter) %>%
  dplyr::summarise(fish = dplyr::n(), .groups = "drop")

for(i in 1:256) {

lan2 <- lan2 %>%
  dplyr::mutate(counter = counter-1) %>%
  dplyr::bind_rows(
    dplyr::filter(., counter == -1) %>%
      dplyr::mutate(counter = 8)
  ) %>%
  dplyr::mutate(counter = ifelse(counter == -1, 6, counter)) %>%
  dplyr::group_by(counter) %>%
  dplyr::summarise(fish = sum(fish), .groups = "drop")


}

answer_2 <- prettyNum(sum(lan2$fish), big.mark = ",", scientific = F)
