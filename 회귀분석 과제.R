# 2.3 #

a <- matrix(c(2, 1, 1, 4), nrow = 2)

plot(a[1, ], a[2, ], xlim = c(0, 5), ylim = c(0, 5),
     xlab = "X축", ylab = "Y축")

plot(a[, 1], a[, 2], xlim = c(0, 5), ylim = c(0, 5),
     xlab = "X축", ylab = "Y축")

library(Matrix)
rankMatrix(a)

lambda=eigen(a)
lambda$values
lambda$vectors

library(MASS)
ginv(a)

library(OpenMx)
P=lambda$vectors; P
L=vec2diag(lambda$values); L
P%*%L%*%t(P)

g_a=ginv(a)
g_a_eigen=eigen(g_a)
g_a_eigen$values
g_a_eigen$vectors

#A는 양정치행렬이다.

ta=t(a)
ta_a=ta%*%a; ta_a

ta_a_eigen=eigen(ta_a)
ta_a_eigen$values
ta_a_eigen$vectors

ginv(ta_a)

a_squared <- a%*%a; a_squared

a_squared_eigen=eigen(a_squared)
a_squared_eigen$values
a_squared_eigen$vectors

Pas=a_squared_eigen$vectors; Pas
Las=vec2diag(a_squared_eigen$values); Las
Pas%*%Las%*%t(Pas)

# 2.4 #

lam <- matrix(c(1, 0, 0,
                0, 9, 0,
                0, 0, 16), nrow = 3)

rankMatrix(lam)

lam_eigen=eigen(lam)
lam_eigen$values #고유값
lam_eigen$vectors #고유벡터(단위 고유벡터)

gin_lam=ginv(lam); gin_lam

Plam=lam_eigen$vectors;Plam
Llam=vec2diag(lam_eigen$values);Llam
Plam%*%Llam%*%t(Plam)

glam_eigen=eigen(gin_lam)
glam_eigen$values #고유값
glam_eigen$vectors #고유벡터

#lambda는 양정치행렬이다.

diag(1/sqrt(diag(lam))) %*% lam %*% diag(1/sqrt(diag(lam)))

det_lam=det(lam)
prod_values=lam_eigen$values[1]*lam_eigen$values[2]*lam_eigen$values[3]
det_lam==prod_values

tlam=sum(diag(lam))
plus_values=lam_eigen$values[1]+lam_eigen$values[2]+lam_eigen$values[3]
tlam==plus_values

# 2.5 #

a2 <- matrix(c(5, -4, 3,
               -4, 8, 6, 
               3, 6, 9), nrow = 3)

a2_eigen=eigen(a2)
a2_eigen$values #고유값
a2_eigen$vectors #고유벡터

ta2=sum(diag(a2))
aplus_values=a2_eigen$values[1]+a2_eigen$values[2]+a2_eigen$values[3]
ta2==aplus_values

det_a2=det(a2)
prod_a2=a2_eigen$values[1]*a2_eigen$values[2]*a2_eigen$values[3]
det_a2==prod_a2

ginv(a2)
