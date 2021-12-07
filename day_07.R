library(magrittr)

data <- read.table("files/day_07_input.txt", sep = ",") %>%
  tidyr::pivot_longer(tidyselect::everything(), names_to = "crab", values_to = "pos") %>%
  dplyr::mutate(crab = as.integer(gsub("^(V)(\\d*)$", "\\2", crab)))

summary(data$pos)

pos1 <- data %>%
  dplyr::mutate(target = median(pos),
                fuel = abs(pos-target))

answer_1 <- pos1$fuel %>% sum()

pos2 <- data %>%
  dplyr::full_join(tibble::tibble(target = min(.$pos):max(.$pos)), by = character()) %>%
  dplyr::mutate(fuel = abs(target - pos) * (abs(target - pos) + 1) / 2) %>% #gauss for the win
  dplyr::group_by(target) %>%
  dplyr::summarise(fuel = sum(fuel), .groups = "drop") %>%
  dplyr::slice_min(fuel)

answer_2 <- pos2$fuel
