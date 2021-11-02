install.packages("psych")
x <- c(110,112,95,96,79,92,102,81,84,92)
mean(x)
var(x)
sd(x)
z <- (x-mean(x))/sd(x)
z
y <-c(10,12,9,9,8,10,11,9,8,10)
cov (x,y)
cor(x,y)
r_square<-0.8174603^2
r_square
lm(y~x)
A
b1 <- c(2,3,4)
b2 <- c(3,2,2)
B <- matrix(data = c(b1, b2), nrow =3)
B
A+B
t(A) #transpose of A
A%*%t(B) #%*% is the matrix multiplication sign
u <- c(5,7,9,22)
v <- c(15,12,19,13)
w <- t(u)%*%v
w
u%*%t(v)