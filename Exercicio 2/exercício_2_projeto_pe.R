library(ggplot2)
library(readxl)

dados <- read_excel("/home/pedro/Downloads/wine_prod_EU.xlsx")

dados = dados[dados$'Product Group' != "Non-Vinified" & dados$Category != "-" & dados$Year == 1998,]

dados$Country <- ifelse(
  dados$'Member State' %in% c("France", "Italy", "Spain"),
  dados$'Member State',
  "Others"
)

ggplot(dados, aes(x = Country, y = Production, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Production of Wine in the European Union in 1998",
    x = "Country",
    y = "Wine Production (in 10³hl)",
    fill = "Wine Category"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

