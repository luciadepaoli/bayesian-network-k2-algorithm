#WITHOUT LOGARITHM:

time_n_l <- NULL

N_n_l <- seq(3,190,1)

for (i in N_n_l){
    
    m <- i
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
    
    start.time <- Sys.time()
    K2(n, u, order, D, l= FALSE)
    cat('\n')
    end.time <- Sys.time()
    time_n_l<-c(time_n_l,end.time - start.time)
    }

#WITH LOGARITHM:

time_l <- NULL

N_l <- seq(10,1000,10)

for (i in N_l){
    m <- i
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
    start.time <- Sys.time()
    K2(n, u, order, D, l= TRUE)
    cat('\n')
    end.time <- Sys.time()
    time_l<-c(time_l,end.time - start.time)
    }

x <- seq(0,50000,1)
model_n_l = lm(time_n_l~N_n_l)
model_n_l
model_l = lm(time_l~N_l)
model_l

q_n_l <- 5.569e-03
q_l <- 0.0043390

m_n_l <- 5.019e-05
m_l <- 0.0000544   

y_n_l <- m_n_l * x + q_n_l
y_l <- m_l * x + q_l

options(repr.plot.width=18, repr.plot.height=10)
plot(x, y_n_l, type = "b", 
     col = "red",main = "Time of computation", xlab = "N", ylab = "T(n)",
    cex.main=2,cex.axis=1.3,cex.lab=1.5)

lines(x, y_l,  col = "blue", type = "b", lty = 2)

legend(0,2, inset=0.05,legend=c("Without log","With log"), col=c("red","blue"),
       lty=5:5, cex=1.5,box.lty=0,bty = "n")
grid()

counter_n_l <- NULL
counter_l <- NULL

# D n=3 m=10
x <- sample(0:1, 30, replace = TRUE)
D3 <- matrix(x, 10, 3)
D3 <- data.frame(D3)
n <- ncol(D3)
order <- names(D3)
u <- n

start.time <- Sys.time()
K2(n, u, order, D3, l=FALSE)
end.time <- Sys.time()
counter_n_l<-c(counter_n_l,end.time-start.time)

start.time <- Sys.time()
K2(n, u, order, D3, l=TRUE)
end.time <- Sys.time()
counter_l<-c(counter_l,end.time-start.time)

# D n=5 m=10
x <- sample(0:1, 50, replace = TRUE)
D5 <- matrix(x, 10, 5)
D5 <- data.frame(D5)
n <- ncol(D5)
order <- names(D5)
u <- n

start.time <- Sys.time()
K2(n, u, order, D5, l=FALSE)
end.time <- Sys.time()
counter_n_l<-c(counter_n_l,end.time-start.time)

start.time <- Sys.time()
K2(n, u, order, D5, l=TRUE)
end.time <- Sys.time()
counter_l<-c(counter_l,end.time-start.time)

# D n=7 m=10
x <- sample(0:1, 70, replace = TRUE)
D7 <- matrix(x, 10, 7)
D7 <- data.frame(D7)
n <- ncol(D7)
order <- names(D7)
u <- n

start.time <- Sys.time()
K2(n, u, order, D7, l=FALSE)
end.time <- Sys.time()
counter_n_l<-c(counter_n_l,end.time-start.time)

start.time <- Sys.time()
K2(n, u, order, D7, l=TRUE)
end.time <- Sys.time()
counter_l<-c(counter_l,end.time-start.time)

# D n=10 m=10
x <- sample(0:1, 100, replace = TRUE)
D10 <- matrix(x, 10, 10)
D10 <- data.frame(D10)
n <- ncol(D10)
order <- names(D10)
u <- n

start.time <- Sys.time()
K2(n, u, order, D10, l=FALSE)
end.time <- Sys.time()
counter_n_l<-c(counter_n_l,end.time-start.time)

start.time <- Sys.time()
K2(n, u, order, D10, l=TRUE)
end.time <- Sys.time()
counter_l<-c(counter_l,end.time-start.time)

# D n=12 m=10
x <- sample(0:1, 120, replace = TRUE)
D12 <- matrix(x, 10, 12)
D12 <- data.frame(D12)
n <- ncol(D12)
order <- names(D12)
u <- n

start.time <- Sys.time()
K2(n, u, order, D12, l=FALSE)
end.time <- Sys.time()
counter_n_l<-c(counter_n_l,end.time-start.time)

start.time <- Sys.time()
K2(n, u, order, D12, l=TRUE)
end.time <- Sys.time()
counter_l<-c(counter_l,end.time-start.time)

# D n=15 m=10
x <- sample(0:1, 150, replace = TRUE)
D15 <- matrix(x, 10, 15)
D15 <- data.frame(D15)
n <- ncol(D15)
order <- names(D15)
u <- n

start.time <- Sys.time()
K2(n, u, order, D15, l=FALSE)
end.time <- Sys.time()
counter_n_l<-c(counter_n_l,end.time-start.time)

start.time <- Sys.time()
K2(n, u, order, D15, l=TRUE)
end.time <- Sys.time()
counter_l<-c(counter_l,end.time-start.time)

# D n=20 m=10
x <- sample(0:1, 200, replace = TRUE)
D20 <- matrix(x, 10, 20)
D20 <- data.frame(D20)
n <- ncol(D20)
order <- names(D20)
u <- n

start.time <- Sys.time()
K2(n, u, order, D20, l=FALSE)
end.time <- Sys.time()
counter_n_l<-c(counter_n_l,end.time-start.time)

start.time <- Sys.time()
K2(n, u, order, D20, l=TRUE)
end.time <- Sys.time()
counter_l <- c(counter_l,end.time-start.time)

par(mfrow=c(1,2))
x <- seq(1,7,1)
x_dummy <- seq(1,7,0.1)
y_dummy <- x_dummy**3
y_dummy <- 15*y_dummy / sum(y_dummy)
fit_n_l <- lm(counter_n_l~poly(x,4,raw=TRUE))
fit_l <- lm(counter_l~poly(x,4,raw=TRUE))

plot(1:7,counter_n_l,xlim=c(1,7), xlab="Step", ylab="Time (s)", main="Checking the growth trend (no log)",
    cex.main=2,cex.axis=1.3,cex.lab=1.5)
lines(x, predict(fit_n_l, data.frame(x=x)),lwd=3, col="red")
lines(x_dummy,y_dummy, lwd=3 ,col="blue")
legend(1,1.0, inset=0.05,legend=c("data's fit","10*x^4"), col=c("red","blue"),
       lty=5:5, cex=1.5,box.lty=0,bty = "n")
grid()

x_dummy <- seq(1,7,0.1)
y_dummy <- x_dummy**3
y_dummy <- 5*y_dummy / sum(y_dummy)
plot(1:7,counter_l,xlim=c(1,7),  xlab="Step", ylab="Time (s)", main="Checking the growth trend (with log)",
    cex.main=2,cex.axis=1.3,cex.lab=1.5)
lines(x, predict(fit_l, data.frame(x=x)),lwd=3, col="red")
lines(x_dummy,y_dummy,lwd=3,col="blue")
legend(1,0.3, inset=0.05,legend=c("data's fit","10*x^4"), col=c("red","blue"),
       lty=5:5, cex=1.5,box.lty=0,bty = "n")
grid()
