library(magrittr)

poly <- read.table("files/day_14_input.txt", nrows = 1) %>%
  dplyr::pull()

map <- read.table("files/day_14_input.txt", skip = 1, sep = " ") %>%
  dplyr::mutate(regex = as.character(glue::glue("(?<={substr(V1, 1,1)})(?={substr(V1,2,2)})"))) %>%
  dplyr::select(match = V1, regex, replacement = V3) %>%
  tibble::as_tibble() %>%
  split(.$match)

res <- poly

for(i in 1:10) {
  res <- purrr::map(1:(stringi::stri_length(res)-1), ~substr(res, .x, .x+1)) %>%
    purrr::map(~stringi::stri_replace_first_regex(.x, pattern = map[[.x]]$regex, replacement = map[[.x]]$replacement))
  res <- c(res[[1]], purrr::map(2:length(res), ~substr(res[[.x]], 2, stringi::stri_length(res[[.x]])))) %>%
    unlist() %>% paste0(collapse = "")
}

count <- purrr::map(LETTERS, ~stringi::stri_count_fixed(res, .x)) %>%
  setNames(LETTERS) %>%
  as.vector() %>%
  unlist() %>%
  .[which(.>0)]

answer_1 <- max(count)-min(count)

map <- purrr::map(map, ~list(insert = dplyr::pull(.x, replacement),
                             count = 0))

start <- purrr::map(1:(stringi::stri_length(poly)-1), ~substr(poly, .x, .x+1))

cnt <- map

for(p in start) {
  cnt[[p]]$count <- cnt[[p]]$count + 1
}

for(i in 1:40) {
  temp <- map
  for(p in names(cnt)) {
   t <- cnt[[p]]$count
   if(t==0) next
   rpl <- cnt[[p]]$insert
   temp[[paste0(substr(p,1,1), rpl)]]$count <- temp[[paste0(substr(p,1,1), rpl)]]$count + t
   temp[[paste0(rpl, substr(p,2,2))]]$count <- temp[[paste0(rpl, substr(p,2,2))]]$count + t
  }
  cnt <- temp
}

cnt <- cnt %>%
  rlist::list.filter(count >0) %>%
  purrr::map(~rlist::list.remove(.x, "insert"))

res2 <- rlist::list.stack(cnt) %>%
  dplyr::mutate(id = names(cnt)) %>%
  tidyr::separate(id, into = c("l0", "l1", "l2"), sep = "") %>%
  dplyr::select(-l0) %>%
  dplyr::bind_rows(
    tibble::tibble(l2 = "S", count = 1)
  ) %>%
  dplyr::group_by(l2) %>%
  dplyr::summarise(count = sum(count), .groups = "drop")


answer_2 <- max(res2$count)-min(res2$count)
prettyNum(answer_2, scientific = F)
