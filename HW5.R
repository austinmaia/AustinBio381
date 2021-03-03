z <- c(x=1.1, a=2.2, b=3.3)
print(z)

print(z[1]^(z[2]^z[3]))

print((z[1]^z[2])^z[3])

print((3*(z[1]^3)) + (2*(z[1]^2)) +1)

q2a <- c(seq(from=1, to=8), seq(from=7, to=1))
print(q2a)

b <- seq(from=1, to=5)
b2 <- rep(x=b, each=2)
typeof(b2)
q2b <- c(b2)
typeof(q2b)
print(q2b)

c <- seq(from=5, to=1)
c2 <- rep(x=c,times=c[c])
print(c2)
typeof(c2)

coOrd <- runif(2)
polar <- asin(coOrd)

queue <- c("sheep", "fox", "owl", "ant")
print(queue)
queue <- c(queue, "serpent")
queue <- c(queue[-c(1)])
queue <- c("donkey", queue)
queue <- c(queue[-which(queue == "serpent")])
queue <- c(queue[-which(queue == "owl")])
queue <- c(queue[c(1,2)], "aphid", queue[c(3)])
print(queue)
which(queue == "aphid")

x <- 1:100
x <- x[which(x %% 2 != 0)]
x <- x[which(x %% 3 != 0)]
x <- x[which(x %% 7 != 0)]

print(x)




