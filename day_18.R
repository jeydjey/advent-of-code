
library(magrittr)

input <- readr::read_lines("files/day_18_input.txt") %>%
  lapply(jsonlite::fromJSON) %>%
  rapply(function(x) {if(!is.list(x) & length(x)>1) list(x) else x}, how = "replace")

test <- input[[1]]

l <- test


add_to <- function(l, add = 0, lr) {
  if(!is.list(l)) {
    l[lr(1,length(l))] <- l[lr(1,length(l))]+add
    return(l)
  }
  add_to(l[[lr(1,length(l))]])
}

explode <- function(l, depth = 5) {
  #browser()
  if(depth == 2) {
    l$add <- l[[1]]
    l[[1]] <- NULL
    return(l)
  } else {
    n <- which((purrr::map_int(l, purrr::vec_depth)==depth-1) == T)[1]
    l[[n]] <- explode(l[[n]], depth = depth-1)
    val <- l[[n]]$add
    l[[n]]$add <- NULL
  }
    if(n-1>0 && val[1]>0) {
      l[[n-1]] <- add_to(l[[n-1]], val[1], max)
      if(length(l[[n]]==0))
        l[[n-1]] <- c(l[[n-1]], 0)
      val[1] <- 0
    }
    if(n+1<= length(l) && val[2]>0) {
      l[[n+1]] <- add_to(l[[n+1]], val[2], min)
      if(length(l[[n]]==0))
        l[[n+1]] <- c(0, l[[n-1]])
      val[2] <- 0
    }
    l[["add"]] <- val
    if(length(l[[n]])==0)
      l[[n]]<-NULL

    return(l)

}

splits <- function(l) {
  if(!is.list(l)) {
    l <- lapply(l, as.list)
    n <- which(purrr::map_lgl(l, ~(max(unlist(.x))>9)))
    l[[n]] <- c(floor(l[[n]]/2), ceiling(l[[n]]/2))
    return(l)
  }
  n <- which(purrr::map_lgl(l, ~(max(unlist(.x))>9)))
  l[[n]] <- splits(l[[n]])
  return(l)
}


reduce_snailfish <- function(l) {
  print(l)
  browser()
  while(purrr::vec_depth(l)==4 | max(unlist(l)) > 9){
    while(purrr::vec_depth(l)==4){
      l <- explode(l)
      l$add <- NULL
    }
    if(max(unlist(l)) > 9) {
      l <- splits(l)
    }
  }
  return(l)

}

snailfish <- purrr::map(input, reduce_snailfish)
