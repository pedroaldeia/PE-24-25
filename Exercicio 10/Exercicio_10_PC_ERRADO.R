# Original data (200 observations)
original_data_str <- "2.3, 2.7, 5.2, 0.7, 2.9, 0.6, 2.6, 2.2, 3.8, 0.5, 4.9, 5.4, 3.7, 0.4, 4, 3.6, 2, 0.8, 2.5, 2.8, 1.7, 3.3, 1.5, 0.4, 6.4, 1.5, 6, 2.1, 0.4, 4.6, 3.1, 4.4, 4, 2.1, 5, 3.3, 4.7, 3.4, 4.3, 4.5, 2.3, 0.5, 4.9, 3.5, 1.8, 1.9, 2.6, 4.3, 4.6, 5.2, 1.6, 2.8, 2.4, 2.8, 1.8, 3.6, 0.8, 5.1, 1.4, 3.2, 1, 6.3, 3.6, 3.6, 1.8, 0.9, 4.6, 2.5, 5.8, 0.6, 3.3, 3.2, 6.6, 2.6, 2.5, 1.5, 4.1, 1.7, 2.1, 1.5, 0.4, 4.8, 0.4, 1.5, 4.2, 3.3, 1.2, 8.1, 2.4, 2.8, 2.1, 6.3, 4.2, 1.3, 6, 1.3, 3.7, 2.5, 6.6, 2.7, 1.4, 2, 0.7, 4.3, 3.4, 4.3, 4, 4, 0.8, 2.3, 2.5, 5.4, 4.3, 0.5, 3.9, 2.2, 3.4, 1.3, 2.4, 4.7, 2, 1.3, 4.4, 2.9, 2.1, 2.5, 1.6, 2.3, 4.4, 1.9, 1.9, 1.7, 4.2, 3.4, 3.9, 4.3, 1.3, 2.9, 2.2, 5.1, 2.3, 1.9, 2.9, 5.2, 3.4, 2.6, 2.4, 3.2, 1.3, 3.1, 5.1, 1.4, 4.2, 0.9, 1.3, 2.1, 2.6, 6.2, 1.6, 2.7, 1.7, 2.3, 3.3, 2.8, 1.2, 2.6, 1.5, 2, 2.8, 2.5, 2, 1.2, 2.2, 2.6, 2.5, 6, 1.9, 3, 3.8, 1.9, 3.2, 3.1, 1.8, 2.6, 1.9, 3.5, 3.7, 1.8, 2.2, 2, 1.3, 2, 1.1, 2.2, 3.1, 2.9, 1.3, 0.2, 3.9"
original_data <- as.numeric(unlist(strsplit(original_data_str, ", ")))

# Set the seed
set.seed(5885)

# Select a subsample of dimension n = 160
n_subsample <- 160
subsample <- sample(original_data, n_subsample, replace = FALSE)

# Parameters for Rayleigh distribution under H0
sigma_H0 <- 2.4

# Define class boundaries for 5 equiprobable classes
# F(x) = 1 - exp(-x^2 / (2*sigma^2))
# To find q_i such that F(q_i) = i/5
# q_i = sqrt(-2 * sigma^2 * log(1 - i/5))

class_probs <- c(0.2, 0.4, 0.6, 0.8, 1.0)
class_boundaries <- sqrt(-2 * sigma_H0^2 * log(1 - class_probs))

# The lower boundary of the first class is 0.
# The class intervals will be:
# (0, class_boundaries[1]]
# (class_boundaries[1], class_boundaries[2]]
# (class_boundaries[2], class_boundaries[3]]
# (class_boundaries[3], class_boundaries[4]]
# (class_boundaries[4], Inf)

# Calculate observed frequencies
observed_freq <- hist(subsample,
                      breaks = c(0, class_boundaries, Inf),
                      plot = FALSE)$counts

# Check if any observed frequency is zero or too small (though not strictly required for this problem, it's a good practice for chi-square tests)
# For chi-square test, expected frequencies should ideally be >= 5. Here, all expected are 32.

# Calculate expected frequencies
expected_freq <- rep(n_subsample / length(class_probs), length(class_probs))

# Perform the chi-square goodness-of-fit test
chi_square_statistic <- sum((observed_freq - expected_freq)^2 / expected_freq)

# Degrees of freedom: k - 1 - p (k=number of classes, p=number of parameters estimated from data)
# Here, k=5, and sigma is given by H0, so p=0.
df <- length(class_probs) - 1

# Calculate the p-value
p_value <- 1 - pchisq(chi_square_statistic, df = df)

# Print results
cat("Observed Frequencies:", observed_freq, "\n")
cat("Expected Frequencies:", expected_freq, "\n")
cat("Chi-Square Statistic:", chi_square_statistic, "\n")
cat("Degrees of Freedom:", df, "\n")
cat("P-value:", p_value, "\n\n")

# Decision at different significance levels
alpha_01 <- 0.01
alpha_05 <- 0.05
alpha_10 <- 0.10

decision_01 <- ifelse(p_value <= alpha_01, "Reject H0", "Do not reject H0")
decision_05 <- ifelse(p_value <= alpha_05, "Reject H0", "Do not reject H0")
decision_10 <- ifelse(p_value <= alpha_10, "Reject H0", "Do not reject H0")

cat("Decision at alpha = 0.01:", decision_01, "\n")
cat("Decision at alpha = 0.05:", decision_05, "\n")
cat("Decision at alpha = 0.10:", decision_10, "\n")

# Now check the options
# a. Rejeitar H0 aos n.s. de 5% e 10% e não rejeitar H0 ao n.s. de 1%.
# b. Rejeitar H0 aos n.s. de 1%, 5% e 10%.
# c. Rejeitar H0 ao n.s. de 10% e não rejeitar H0 aos n.s. de 1% e 5%.
# d. Teste é inconclusivo.
# e. Não rejeitar H0 aos n.s. de 1%, 5% e 10%.