x1 <- c(1, 1, 0, 1, 0, 0, 1, 0, 1, 0)
x2 <- c(0, 1, 0, 1, 0, 1, 1, 0, 1, 0)
x3 <- c(0, 1, 1, 1, 0, 1, 1, 0, 1, 0)

D <- data.frame(x1, x2, x3)

n <- ncol(D)      # Number of nodes
u <- 2            # Upper limit to the number of parents
order <- names(D) # Order of nodes

start.time <- Sys.time()
K2(n, u, order, D, l=FALSE) 
end.time <- Sys.time()
cat("K2 algorithm without log:",end.time - start.time,"sec \n")

cat("\n")

start.time <- Sys.time()
K2(n, u, order, D, l=TRUE) 
end.time <- Sys.time()
cat("K2 algorithm with log:",end.time - start.time,"sec \n")

dataset <- BNDataset(data = D,
                    discreteness = rep('d',3),
                    variables = c("x1", "x2", "x3"),
                    starts.from = 0,
                    node.sizes = c(2,2,2)) # Cardinality

layers <- c(1,2,3) # Equivalent to order defined above
u <- 2
net <- learn.network(algo = "sm", x = dataset, layering = layers, max.parents = u)
start.time <- Sys.time()
net <- learn.network(algo = "sm", x = dataset, layering = layers, max.parents = u)
end.time <- Sys.time()
cat("Time of computation:",end.time - start.time,"sec")

plot(net)