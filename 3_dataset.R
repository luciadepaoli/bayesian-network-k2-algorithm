m <- 50

x1 <- rbinom(m, 1, prob = 1/5)
x2 <- sample(0:3, m, replace = T)
x3 <- rep(0,m)
x4 <- rep(0,m)

idx2 <- which(x1 == 1)
rand <- runif(length(idx2))
x3[idx2[rand >= 0.20]] <- 1

idx2 <- which(x2 == 3)
rand <- runif(length(idx2))
x3[idx2[rand >= 0.10]] <- 2

idx3 <- which(x3 == 1)
rand <- runif(length(idx3))
x4[idx3[rand >= 0.30]] <- 1

idx3 <- which(x2 == 1)
rand <- runif(length(idx3))
x4[idx3[rand >= 0.20]] <- 2

D <- data.frame(x1, x2, x3, x4)

n <- ncol(D)
u <- 3
order <- names(D)

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
                    discreteness = rep('d',4),
                    variables = c("x1", "x2", "x3", 'x4'),
                    starts.from = 0,
                    node.sizes = c(2,4,3,3))

layers <- c(1,2,3,4)
u <- 2

start.time <- Sys.time()
net <- learn.network(algo = "sm", x = dataset, layering = layers, max.parents = u)
end.time <- Sys.time()
cat("Time of computation:",end.time - start.time,"sec")

plot(net)