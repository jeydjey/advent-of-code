
library(magrittr)

data <- read.table("files/day_12_input.txt", sep = "-") %>%
  setNames(c("from_t", "to_t")) %>%
  dplyr::mutate(
    from = dplyr::case_when(
      from_t == "start" ~from_t,
      to_t == "start" ~to_t,
      from_t == "end" ~to_t,
      T ~from_t
    ),
    to = dplyr::case_when(
      from_t == "start" ~to_t,
      to_t == "start" ~from_t,
      from_t == "end" ~from_t,
      T ~to_t
    )
  ) %>%
  dplyr::select(from, to)


start <- data %>% dplyr::filter(from == "start") %>% dplyr::mutate(id = dplyr::row_number())
end <- data %>% dplyr::filter(to == "end")
middle <- data %>% dplyr::filter(from != "start", to != "end")
map <- dplyr::bind_rows(
  middle,
  middle %>% setNames(rev(names(.))),
  end
)

find_path <- function(routes, map, task) {

  bool <- T
  while(bool) {

    smalls <- routes %>%
      dplyr::select(id, tidyselect::starts_with("to")) %>%
      tidyr::pivot_longer(tidyselect::starts_with("to"), names_to = "to", values_to = "val") %>%
      dplyr::filter(!is.na(val), grepl("[a-z]+", .$val))

    if(task==1) {
      smalls <- dplyr::distinct(smalls)
    } else {
      smalls <- smalls %>%
        dplyr::count(id, val) %>%
        dplyr::group_by(id) %>%
        dplyr::filter(max(n)>1) %>%
        dplyr::ungroup()
    }

    to <- grep("to_\\d+", names(routes), value = T) %>%
      gsub("(to_)(\\d+)", replacement = "\\2", .) %>%
      as.numeric() %>%
      max()

    routes <- routes %>%
      dplyr::left_join(
        map, by = setNames("from", paste0("to_", to))
      ) %>%
      dplyr::anti_join(
        smalls, by = c("id", "to" = "val")
      ) %>%
      dplyr::mutate(id = dplyr::row_number()) %>%
      dplyr::rename(!!rlang::sym(paste0("to_", to+1)) := to)

    if(dplyr::filter(routes, !(is.na(!!rlang::sym(paste0("to_", to+1))) |
                               !!rlang::sym(paste0("to_", to+1)) == "end")) %>% nrow() == 0) {
      bool <- F
    }

  }

  return(routes)
}

routes <- start %>% dplyr::rename(to_1 = to) %>%
  dplyr::select(id, from, to_1)

answer_1 <- find_path(routes, map, 1) %>% nrow()

answer_2 <- find_path(routes, map, 2) %>% nrow()
