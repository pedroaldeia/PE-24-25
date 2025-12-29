######################################### Exercício 4 ##############################################

E_analitico = 25*gamma(1+ 1/5)
set.seed(1742)
amostra <- rweibull(7500, shape = 5, scale = 25)
E_montecarlo <- mean(amostra)
E_montecarlo
E_analitico
abs(E_analitico - E_montecarlo)

######################################### Exercício 5 ##############################################

set.seed(1420)
n <- 41000


dados <- matrix(sample(1:6, 3 * n, replace = TRUE), ncol = 3, byrow = TRUE)

somas <- rowSums(dados)

freq_9 <- sum(somas == 9) / n
freq_10 <- sum(somas == 10) / n

diferenca <- round(abs(freq_10 - freq_9), 4)
diferenca

######################################### Exercício 6 ##############################################

n <- 9
x <- 4.2
m <- 130
set.seed(5347)

pn_exact <- function(n, x) {
  if (x < 0) return(0)
  if (x > n) return(1)
  k <- 0:floor(x)
  sum_term <- sum((-1)^k * choose(n, k) * (x - k)^n)
  return(sum_term / factorial(n))
}

p_n <- pn_exact(n, x)

mu <- n / 2
sigma <- sqrt(n / 12)
pn_TLC <- pnorm(x, mean = mu, sd = sigma)

samples <- matrix(runif(m * n), nrow = m, ncol = n)
S_n_sim <- rowSums(samples)
pn_sim <- mean(S_n_sim <= x)

abs_dev_TLC <- abs(p_n - pn_TLC)

abs_dev_sim <- abs(p_n - pn_sim)

dev_ratio <- abs_dev_TLC / abs_dev_sim

cat(sprintf("Valor exato de p_n: %.6f\n", p_n))
cat(sprintf("Aproximação via TLC: %.6f\n", pn_TLC))
cat(sprintf("Aproximação via simulação: %.6f\n", pn_sim))
cat(sprintf("Desvio absoluto TLC: %.6f\n", abs_dev_TLC))
cat(sprintf("Desvio absoluto simulação: %.6f\n", abs_dev_sim))
cat(sprintf("Quociente dos desvios (TLC / sim): %.4f\n", dev_ratio)) 

######################################### Exercício 7 ##############################################

n <- 13
sum_x <- 72.96
sum_log_x <- 22.4

f <- function(alpha) {
  log(alpha) - digamma(alpha) - (log(sum_x / n) - (sum_log_x / n))
}

alpha_hat <- uniroot(f, interval = c(0.001, 265.5))$root

lambda_hat <- alpha_hat / (sum_x / n)

mode <- (alpha_hat - 1) / lambda_hat

mode_rounded <- round(mode, 2)
cat("Comprimento modal estimado:", mode_rounded, "cm\n")

######################################### Exercício 8 ##############################################

set.seed(1254)
m <- 1000
n <- 12
mu <- 0.9
sigma <- 0.9
gamma <- 0.91

z <- qnorm(1 - (1 - gamma)/2)

conta <- 0
for (i in 1:m) {
  amostra <- rnorm(n, mean = mu, sd = sigma)
  media_amostra <- mean(amostra)
  erro <- z * sigma / sqrt(n)
  li <- media_amostra - erro
  ls <- media_amostra + erro
  
  if (li <= mu && mu <= ls) {
    conta <- conta + 1
  }
}

prop <- conta / m

quociente <- round(prop / gamma, 4)

cat("Proporção de ICs que contêm a média:", prop, "\n")
cat("Quociente entre proporção e gamma:", quociente, "\n")

######################################### Exercício 9 ##############################################
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

######################################### Exercício 10 #############################################
dados <- c(2.3, 2.7, 5.2, 0.7, 2.9, 0.6, 2.6, 2.2, 3.8, 0.5, 4.9, 5.4, 3.7, 0.4, 4, 3.6, 2, 0.8, 2.5, 2.8, 1.7, 3.3, 1.5, 0.4, 6.4, 1.5, 6, 2.1, 0.4, 4.6, 3.1, 4.4, 4, 2.1, 5, 3.3, 4.7, 3.4, 4.3, 4.5, 2.3, 0.5, 4.9, 3.5, 1.8, 1.9, 2.6, 4.3, 4.6, 5.2, 1.6, 2.8, 2.4, 2.8, 1.8, 3.6, 0.8, 5.1, 1.4, 3.2, 1, 6.3, 3.6, 3.6, 1.8, 0.9, 4.6, 2.5, 5.8, 0.6, 3.3, 3.2, 6.6, 2.6, 2.5, 1.5, 4.1, 1.7, 2.1, 1.5, 0.4, 4.8, 0.4, 1.5, 4.2, 3.3, 1.2, 8.1, 2.4, 2.8, 2.1, 6.3, 4.2, 1.3, 6, 1.3, 3.7, 2.5, 6.6, 2.7, 1.4, 2, 0.7, 4.3, 3.4, 4.3, 4, 4, 0.8, 2.3, 2.5, 5.4, 4.3, 0.5, 3.9, 2.2, 3.4, 1.3, 2.4, 4.7, 2, 1.3, 4.4, 2.9, 2.1, 2.5, 1.6, 2.3, 4.4, 1.9, 1.9, 1.7, 2, 4.2, 3.4, 3.9, 4.3, 1.3, 2.9, 2.2, 5.1, 2.3, 1.9, 2.9, 5.2, 3.4, 2.6, 2.4, 3.2, 1.3, 3.1, 5.1, 1.4, 4.2, 0.9, 1.3, 2.1, 2.6, 6.2, 1.6, 2.7, 1.7, 2.3, 3.3, 2.8, 1.2, 2.6, 1.5, 2, 2.8, 2.5, 2, 1.2, 2.2, 2.6, 2.5, 6, 1.9, 3, 3.8, 1.9, 3.2, 3.1, 1.8, 2.6, 1.9, 3.5, 3.7, 1.8, 2.2, 2, 1.3, 2, 1.1, 2.2, 3.1, 2.9, 1.3, 0.2, 3.9)
set.seed(5885)
subamostra <- sample(dados, 160, replace = FALSE)

sigma <- 2.4
k <- 5
probs <- seq(0, 1, length.out = k + 1)  

rayleigh_q <- function(p, sigma) sqrt(-2 * sigma^2 * log(1 - p))
limites <- rayleigh_q(probs, sigma)
limites[1] <- 0  

observed <- hist(subamostra, breaks = limites, plot = FALSE)$counts

expected <- rep(length(subamostra) / k, k)

resultado <- chisq.test(observed, p = rep(1 / k, k), rescale.p = TRUE)
print(resultado$p.value)

