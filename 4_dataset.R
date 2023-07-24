head(learning.test,10)

converter<-function(A){
    n<-length(A)
    for (i in 1:n){
        if (A[i] == "a"){
            A[i]<-as.numeric(1)
        }
        else if( A[i]=="b"){
             A[i]<-as.numeric(2)
        }
        else{
            A[i]<-as.numeric(3)
        }
    }
    return(as.numeric(A))
}
learning.test <- data.frame(lapply(learning.test, as.character), stringsAsFactors=FALSE)
data <- data.frame(apply(learning.test, MARGIN=2, converter ))

head(data,10)

n <- ncol(data)
u <- n - 1
order <- c('F', 'C','A','B','E','D')

start.time <- Sys.time()
K2(n, u, order, data, l=TRUE)
end.time <- Sys.time()
cat("With log:",end.time - start.time,"sec \n")

dataset <- BNDataset(data = data,
                    discreteness = rep('d',6),
                    variables = c('A', 'B','C','D','E','F'),
                    starts.from = 1,
                    node.sizes = c(3,3,3,3,3,2))

layers <- c(3, 4, 2, 6, 5, 1)
u <- n - 1

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