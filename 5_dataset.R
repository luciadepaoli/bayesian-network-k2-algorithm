data <- data.frame(raw.data(asia()))

n <- ncol(data)
u <- n - 1
order <- c('Asia','Smoke','Tubercolosys',
           'LungCancer','Either','Bronchitis','X-ray','Dyspnea')

names(data) <- c('Asia', 'Tubercolosys', 'Smoke', 'LungCancer' ,'Bronchitis', 'Either' ,'X-ray', 'Dyspnea')

start.time <- Sys.time()
K2(n, u, order, data, l= TRUE)
end.time <- Sys.time()
cat("With log:",end.time - start.time,"sec \n")

dataset <- BNDataset(data = data,
                    discreteness = rep('d',8),
                    variables = c('Asia', 'Tubercolosys', 'Smoke', 'LungCancer' ,'Bronchitis', 'Either' ,'X-ray', 'Dyspnea'),
                    starts.from = 1,
                    node.sizes = c(2,2,2,2,2,2,2,2))

layers <- c(1, 3, 2, 4, 6, 5, 7, 8)
u <- 2

start.time <- Sys.time()
net <- learn.network(algo = "sm", x = dataset, max.parents = u, layering = layers)
end.time <- Sys.time()
cat("",end.time - start.time,"sec \n")
plot(net)

start.time <- Sys.time()
net2 <- learn.network(algo = "sm", x = dataset)
end.time <- Sys.time()
cat("",end.time - start.time,"sec \n")
plot(net2)