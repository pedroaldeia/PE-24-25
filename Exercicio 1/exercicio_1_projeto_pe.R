library(ggplot2)

dados <- read.csv("/home/pedro/Downloads/winequality-white-q5.csv")
dados$sqrt_total_sulfur <- sqrt(wine_data$total.sulfur.dioxide)

ggplot(dados, aes(x = factor(quality), y = sqrt_total_sulfur, fill = factor(quality))) +
  geom_boxplot(alpha = 0.2) +
  labs(
    title = "Boxplot of sqrt(Total Sulfur Dioxide) by Wine Quality",
    x = "Wine Quality",
    y = "sqrt(Total Sulfur Dioxide)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


