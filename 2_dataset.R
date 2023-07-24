m = 10

x1 <- rbinom(m, 1, 1/2)
x2 <- rep(0,m)
x3 <- rep(0,m)

idx <- which(x1 == 1)

rand <- runif(length(idx))
x2[idx[rand >= 0.20]] <- 1

rand <- runif(length(idx))
x3[idx[rand >= 0.40]] <- 1

D <- data.frame(x1, x2, x3)
n <- ncol(D)
u <- 2
order <- names(D)

# start.time <- Sys.time()
# K2(n, u, order, D, l= FALSE)
# cat('\n')
# end.time <- Sys.time()
# cat("Time of computation:",end.time - start.time,"sec")

start.time <- Sys.time()
K2(n, u, order, D, l= FALSE)
end.time <- Sys.time()
cat("Without log:",end.time - start.time,"sec \n")

cat("\n")

start.time <- Sys.time()
K2(n, u, order, D, l= TRUE)
end.time <- Sys.time()
cat("With log:",end.time - start.time,"sec \n")


dataset <- BNDataset(data = D,
                    discreteness = rep('d',3),
                    variables = c("x1", "x2", "x3"),
                    starts.from = 0,
                    node.sizes = c(2,2,2))

layers <- c(1,2,3)
u <- 2

start.time <- Sys.time()
net <- learn.network(algo = "sm", x = dataset, layering = layers, max.parents = u)
end.time <- Sys.time()
cat("Time of computation:",end.time - start.time,"sec")

plot(net)