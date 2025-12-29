# Parâmetros
n <- 9
x <- 4.2
m <- 130
set.seed(5347)

# 1. Valor exato via Irwin-Hall
pn_exact <- function(n, x) {
  if (x < 0) return(0)
  if (x > n) return(1)
  k <- 0:floor(x)
  sum_term <- sum((-1)^k * choose(n, k) * (x - k)^n)
  return(sum_term / factorial(n))
}

p_n <- pn_exact(n, x)

# 2a. Teorema do Limite Central
# Sn ~ N(n/2, n/12)
mu <- n / 2
sigma <- sqrt(n / 12)
pn_TLC <- pnorm(x, mean = mu, sd = sigma)

# 2b. Simulação
samples <- matrix(runif(m * n), nrow = m, ncol = n)
S_n_sim <- rowSums(samples)
pn_sim <- mean(S_n_sim <= x)

# 3. Desvio absoluto entre p_n e p_n,TLC
abs_dev_TLC <- abs(p_n - pn_TLC)

# 4. Desvio absoluto entre p_n e p_n,sim
abs_dev_sim <- abs(p_n - pn_sim)

# 5. Quociente dos desvios
dev_ratio <- abs_dev_TLC / abs_dev_sim

# Apresentação dos resultados
cat(sprintf("Valor exato de p_n: %.6f\n", p_n))
cat(sprintf("Aproximação via TLC: %.6f\n", pn_TLC))
cat(sprintf("Aproximação via simulação: %.6f\n", pn_sim))
cat(sprintf("Desvio absoluto TLC: %.6f\n", abs_dev_TLC))
cat(sprintf("Desvio absoluto simulação: %.6f\n", abs_dev_sim))
cat(sprintf("Quociente dos desvios (TLC / sim): %.4f\n", dev_ratio))
