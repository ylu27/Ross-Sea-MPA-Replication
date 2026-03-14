# Load necessary packages
library(showtext)
library(sysfonts)
library(readstata13) # for read.dta13
library(lmtest)  # for coeftest
library(sandwich) # for vcovCL
library(dplyr)   # for mutate and sample
library(ggplot2) # for plotting

# Add and register the Times New Roman font
font_add("Times New Roman", "C:/Windows/Fonts/times.ttf")
showtext_auto()

# Load the data
data <- read.dta13("C:/Users/satoshiy/Dropbox/00 - Research/2104 - Ross Sea MPA/01 - Manuscript/Yifan share/data/DiD/Placebo test - Permutation simulation/placebo_sy.dta")

# Define the regression function
run_regression <- function(data, formula) {
  model <- lm(as.formula(formula), data = data)
  coeftest_result <- coeftest(model, vcov = vcovCL, cluster = ~mmsi)
  list(coefficient = coeftest_result["treatment:postmpa", "Estimate"])
}

# Define the permutation test function
permutation_test <- function(data, formula, n_permutations = 1000) {
  original_results <- run_regression(data, formula)
  permuted_coefficients <- numeric(n_permutations)
  
  for (i in 1:n_permutations) {
    data_permuted <- data %>% mutate(treatment = sample(treatment))
    permuted_results <- run_regression(data_permuted, formula)
    permuted_coefficients[i] <- permuted_results$coefficient
  }
  
  p_value <- mean(abs(permuted_coefficients) >= abs(original_results$coefficient))
  
  list(original_coefficient = original_results$coefficient,
       permuted_coefficients = permuted_coefficients,
       p_value = p_value)
}

# Define the formula for dfish
formula_dfish <- "dfish ~ treatment * postmpa + factor(mmsi) + factor(month)"

# Run permutation test for dfish
results_dfish <- permutation_test(data, formula_dfish, n_permutations = 1000)

# Print results
print(results_dfish)

# Calculate additional statistics
mean_permuted_coef <- mean(results_dfish$permuted_coefficients, na.rm = TRUE)

# Print the results
cat("Actual Coefficient:", results_dfish$original_coefficient, "\n")
cat("Mean Placebo Coefficient:", mean_permuted_coef, "\n")
cat("P-Value:", results_dfish$p_value, "\n")

# Plot histogram with additional information for dfish
p1 <- ggplot(data.frame(permuted_coefficients = results_dfish$permuted_coefficients), aes(x = permuted_coefficients)) +
  geom_histogram(bins = 100, fill = "steelblue", alpha = 0.7) +
  geom_vline(aes(xintercept = results_dfish$original_coefficient), color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = mean_permuted_coef), color = "black", linetype = "dashed", size = 1) +
  labs(title = "(a) Permutation Test for Fishing Hours",
       x = "Coefficient",
       y = "Frequency") +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman", size = 96),
        axis.text = element_text(family = "Times New Roman", size = 80),
        axis.title = element_text(family = "Times New Roman", size = 96),
        plot.title = element_text(family = "Times New Roman", size = 112, hjust = 0.5),
        plot.margin = margin(2, 2, 2, 2, "cm")) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  annotate("text", x = results_dfish$original_coefficient, y = max(table(cut(results_dfish$permuted_coefficients, breaks = 100))) * 0.7, 
           label = paste("Actual Coefficient:", round(results_dfish$original_coefficient, 6),
                         "\nMean Permutation Coefficient:", round(mean_permuted_coef, 6),
                         "\nP-Value:", round(results_dfish$p_value, 6)),
           hjust = -0.1, vjust = 1, size = 24, color = "black", family = "Times New Roman", lineheight = 0.8, parse = FALSE)

# Save the plot
ggsave("Permutation_Test_for_Fishing_Hours_sy.png", plot = p1, dpi = 300, width = 16, height = 12)

write.csv(results_dfish$permuted_coefficients, "dfish_permuted_coefficients.csv")

##############################################################################################################
# Define the formula for dcruise
formula_dcruise <- "dcruise ~ treatment * postmpa + factor(mmsi) + factor(month)"

# Run permutation test for dcruise
results_dcruise <- permutation_test(data, formula_dcruise, n_permutations = 1000)

# Print results
print(results_dcruise)

# Calculate additional statistics
mean_permuted_coef <- mean(results_dcruise$permuted_coefficients, na.rm = TRUE)

# Print the results
cat("Actual Coefficient:", results_dcruise$original_coefficient, "\n")
cat("Mean Placebo Coefficient:", mean_permuted_coef, "\n")
cat("P-Value:", results_dcruise$p_value, "\n")

# Plot histogram with additional information for Non-fishing hours
p2 <- ggplot(data.frame(permuted_coefficients = results_dcruise$permuted_coefficients), aes(x = permuted_coefficients)) +
  geom_histogram(bins = 100, fill = "steelblue", alpha = 0.7) +
  geom_vline(aes(xintercept = results_dcruise$original_coefficient), color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = mean_permuted_coef), color = "black", linetype = "dashed", size = 1) +
  labs(title = "(b) Permutation Test for Non-fishing Hours",
       x = "Coefficient",
       y = "Frequency") +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman", size = 96),
        axis.text = element_text(family = "Times New Roman", size = 80),
        axis.title = element_text(family = "Times New Roman", size = 96),
        plot.title = element_text(family = "Times New Roman", size = 112, hjust = 0.5),
        plot.margin = margin(2, 2, 2, 2, "cm")) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  annotate("text", x = results_dcruise$original_coefficient, y = max(table(cut(results_dcruise$permuted_coefficients, breaks = 100))) * 0.7, 
           label = paste("Actual Coefficient:", round(results_dcruise$original_coefficient, 6),
                         "\nMean Permutation Coefficient:", round(mean_permuted_coef, 6),
                         "\nP-Value:", round(results_dcruise$p_value, 6)),
           hjust = 1.1, vjust = 1, size = 24, color = "black", family = "Times New Roman", lineheight = 0.8, parse = FALSE)

# Save the plot
ggsave("Permutation_Test_for_dcruise.png", plot = p2, dpi = 300, width = 16, height = 12)

write.csv(results_dcruise$permuted_coefficients, "dcruise_permuted_coefficients.csv")

##############################################################################################################
# Define the formula for ccamlr_fishing
formula_ccamlr <- "ccamlr_fishing ~ treatment * postmpa + factor(mmsi) + factor(month)"

# Run permutation test for ccamlr_fishing
results_ccamlr <- permutation_test(data, formula_ccamlr, n_permutations = 1000)

# Print results
print(results_ccamlr)

# Calculate additional statistics
mean_permuted_coef <- mean(results_ccamlr$permuted_coefficients, na.rm = TRUE)

# Plot histogram with additional information for ccamlr_fishing
p3 <- ggplot(data.frame(permuted_coefficients = results_ccamlr$permuted_coefficients), aes(x = permuted_coefficients)) +
  geom_histogram(bins = 70, fill = "steelblue", alpha = 0.7) +
  geom_vline(aes(xintercept = results_ccamlr$original_coefficient), color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = mean_permuted_coef), color = "black", linetype = "dashed", size = 1) +
  labs(title = "(c) Permutation Test for CCAMLR fishing",
       x = "Coefficient",
       y = "Frequency") +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman", size = 96),
        axis.text = element_text(family = "Times New Roman", size = 80),
        axis.title = element_text(family = "Times New Roman", size = 96),
        plot.title = element_text(family = "Times New Roman", size = 112, hjust = 0.5),
        plot.margin = margin(2, 2, 2, 2, "cm")) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  annotate("text", x = -0.04, y = 65, 
           label = paste("Actual Coefficient:", round(results_ccamlr$original_coefficient, 6),
                         "\nMean Permutation Coefficient:", round(mean_permuted_coef, 6),
                         "\nP-Value:", round(results_ccamlr$p_value, 6)),
           hjust = 0, vjust = 1, size = 24, color = "black", family = "Times New Roman", lineheight = 0.8, parse = FALSE)

# Save the plot
ggsave("Permutation_Test_for_CCAMLR_fishing.png", plot = p3, dpi = 300, width = 16, height = 12)

write.csv(results_ccamlr$permuted_coefficients, "CCAMLR_fishing_permuted_coefficients.csv")
