set.seed(4154)

n <- 20
m <- 900
u0 <- 4
u1 <- 5
alpha <- 0.1
df <- 2*n

q_crit <- qchisq(1 - alpha, 2*n)

medias <- rowMeans(matrix(rexp(n * m, rate = 1 / u1), nrow = m))

T0 <- (df * medias) / u0

beta_h <- mean(T0 <= q_crit)

print(paste("beta hat: " , beta_h))

beta_teorico <- pchisq((u0 / u1) * q_crit, df)
print(paste("beta: " , beta_teorico))

R_final = round(beta_h / beta_teorico, 4)
print(paste("9 final:" , R_final))
