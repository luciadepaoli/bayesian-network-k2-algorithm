library("bnstruct")
#library("dplyr")
library("bnlearn") # Used to load dataset Asia and Learning test

# Alpha i j k with fixed i, j
alpha <- function(data, k, parents, phi, pi, vi){

    if (is.null(pi) == FALSE){
            return (length(which(data == vi[k] & apply(parents, 1, function(x)
                identical(as.numeric(x), phi)))))
        }
    else {return (length(which(data == vi[k])))}
}

f <- function(i, pi, data, l){
    if (l==TRUE) {f <- 0}
    else {f <- 1 }
    uniq <- lapply(data[pi], unique)
    Phi <- expand.grid(uniq)                          # List of possible
                                                      # instantiations of 
                                                      # the parents
    Vi <- as.numeric(unlist(unique(data[i]))) # List of possible value for x_i
    r <- length(Vi)
    q <- nrow(Phi)
    x_i <- data[[i]] # x_i in the dataset
    x_parents <- data[pi] # x_parents in the dataset
    
    if(q==0){q<-1} # When there are no parents
    
    for (j in 1:q){
        Phi_j <- as.numeric(Phi[j, ]) # Fix a combination of instantiations
                                      # for the parents
        prod_2 <- 1
        Nij <- 0
        k <- seq(1:r)

        # List of alpha value for all values of k
        a <- sapply(k, alpha, data = x_i, parents = x_parents, 
                            phi = Phi_j, pi = pi, vi = Vi)
        if (l == FALSE){
            Nij <- sum(a)
            prod_2 <- prod(factorial(a))
            f <- f * factorial(r-1) / factorial(Nij + r - 1) * prod_2
            }
        
        else{
            if (any(a==0)) {a[a==0] <- 0.01}  
            Nij <- sum(a)
            prod2 <- sum(a * log(a) - a)
            f <- f + ((r-1) * log(r-1) - (r-1)) - ((Nij + r - 1)
                    * log(Nij + r - 1) - (Nij + r - 1)) + prod2
            }
        }
    return(f)
}

K2 <- function(n, u, order, D, l = FALSE){
    
    D <- D[order] # Order database respect to order nodes
    
    for (i in 1:n){
    pi <- NULL # Set of parents
    P_old <- f(i, pi, D, l)
    PROCEED <- TRUE

    while( PROCEED == TRUE && length(pi) < u ){
        Pred <- order[1 : i - 1]   # Precedent nodes
        nodes <- setdiff(Pred, pi) # Difference between actual parents and
                                   # previous nodes
        P_max <- NULL # List of P for different z

        # Find the z that maximizes P(i, pi)
        if (length(nodes) !=0){
        for (j in 1:(length(nodes))){
                z <- nodes[j] 
                pi_new <- c(pi, z)
                P_max <- c(P_max, f(i, pi_new, D, l))
            }
        }

        # For the first iteraction
        else {
            P_max <- c(P_max, f(i, pi, D, l))
            }

        z <- nodes[which.max(P_max)] # z that maximizes P(i, pi)
        pi_new <- c(pi,z)  
        P_new <- max(P_max)

        if (P_new > P_old){
            P_old <- P_new
            pi <- c(pi,z)
        }

        else{PROCEED <- FALSE}
        }
    cat ("Node: ", order[i], " Parents: ", pi, "\n");
    }
}