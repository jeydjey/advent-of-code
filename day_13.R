
library(magrittr)

dot <- read.table("files/day_13_input.txt", sep = ",", col.names = c("col", "row"), nrows = 750) %>%
  dplyr::select(row, col) %>%
  dplyr::mutate(row = row+1,
                col = col+1) %>%
  as.matrix()

folds <- read.table("files/day_13_input.txt", sep = " ", col.names = c(NA, NA, "instr"), skip = 750) %>%
  dplyr::select(instr) %>%
  tidyr::separate(instr, c("direction", "line"), sep = "=", convert = T) %>%
  dplyr::mutate(line = line+1)

paper <- matrix(data = rep(F, max(dot[,1])*max(dot[,2])), nrow = max(dot[,1]), ncol = max(dot[,2]), byrow = T)
paper[dot] <- T

fold <- function(paper, direction, line) {

  if(direction=="y")
    paper <- t(paper)

  p1 <- paper[, 1:(line-1)]
  p2 <- paper[, ncol(paper):(line+1)]

  paper <- p1|p2

  if(direction=="y")
    paper <- t(paper)

  return(paper)

}

paper <- fold(paper, direction = folds[1,1], line = folds[1,2])

answer_1 <- sum(paper)

for(i in 2:nrow(folds)) {

  paper <- fold(paper, folds[i, 1], folds[i, 2])

}

paper[which(paper==T, arr.ind = T)] <- "#"
paper[which(paper=="FALSE", arr.ind = T)] <- ""

View(paper)
