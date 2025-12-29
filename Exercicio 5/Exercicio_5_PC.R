set.seed(1420)  # fixar semente
n <- 41000

# gerar uma matriz de 41000 linhas e 3 colunas (cada linha = 1 jogada)
dados <- matrix(sample(1:6, 3 * n, replace = TRUE), ncol = 3)

# somar os dados por jogada
somas <- rowSums(dados)

# contar frequências
freq_9 <- sum(somas == 9) / n
freq_10 <- sum(somas == 10) / n

# diferença
diferenca <- round(abs(freq_10 - freq_9), 4)
diferenca
