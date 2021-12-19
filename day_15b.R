
library(igraph)

input <- readr::read_lines("files/day_15_input.txt") %>% strsplit("") %>% unlist() %>% as.numeric()

cave_network <- make_lattice(dimvector = c(100,100), directed = T, mutual = T)

# assign risk levels to vertices

V(cave_network)$risk <- input

# edges inherit risk from vertices

edgelist <- get.edgelist(cave_network)
E(cave_network)$weight <- V(cave_network)[edgelist[,2]]$risk

#for (i in 1:10000) {
#cave_network <- set_edge_attr(cave_network, name = "weight", index = E(cave_network)[to(V(cave_network)[i])], value = input[i])
#}

# find shortest path

distances(cave_network, v = 1, to = 10000, mode = "out")


bigger_caves <- input %>% matrix(ncol = 100)

bigger_caves <- cbind(bigger_caves,
                      bigger_caves + 1,
                      bigger_caves + 2,
                      bigger_caves + 3,
                      bigger_caves + 4)

bigger_caves <- rbind(bigger_caves,
                      bigger_caves + 1,
                      bigger_caves + 2,
                      bigger_caves + 3,
                      bigger_caves + 4)

bigger_caves[bigger_caves > 9] <- bigger_caves[bigger_caves > 9] - 9

cave_network <- make_lattice(dimvector = c(500,500), directed = T, mutual = T)

# assign risk levels

V(cave_network)$risk <- bigger_caves

edgelist <- get.edgelist(cave_network)
E(cave_network)$weight <- V(cave_network)[edgelist[,2]]$risk

distances(cave_network, v = 1, to = 250000, mode = "out")
