## Question 1
n_dims <- runif(1, min=3, max=7)
print(n_dims)

my_vec <- 1:n_dims^2
newvec <- sample(my_vec)
m <- matrix(data=newvec, ncol=n_dims, nrow=n_dims)
print(m)

t(m)
print(m)

firstandlast <- c(m[1,], m[nrow(m),])
sum(firstandlast)
mean(firstandlast, trim=0)

e_m <- eigen(m)
typeof(e_m$values)
typeof(e_m$vectors)


## Question 2
my_matrix <- matrix(runif(16), nrow=4, ncol=4)

my_logical <- c(runif(100))
my_logical <- my_logical > 0.5

my_letters <- sample(letters)

my_list <- list(mat=my_matrix, log=my_logical, let=my_letters)

list2 <- list(my_list$mat[2,2], my_list$log[2], my_list$let[2])

typeof(list2[[1]])
typeof(list2[[2]])
typeof(list2[[3]])

subset <- c(list2[[1]], list2[[2]], list2[[3]])

## Question 3

my_unis <- runif(26, min = 0, max = 10)
my_letters <- sample(LETTERS)
my_frame <- data.frame(my_unis, my_letters)
my_frame[sample(1:26, 4), 1] <- NA
which(!complete.cases(my_frame$my_unis))
my_frame <- data.frame(my_frame$my_unis, sort(my_frame$my_letters))
mean(my_frame[[1]], na.rm = TRUE)




