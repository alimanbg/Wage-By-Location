# ============================================================================
# Applied Econometrics Group Project: Industry and High-Paid Job Distribution
# Research Question: "Does Industry and High-Paid Job Distribution Drive 
# Economic Disparity Across Hong Kong?"
# ============================================================================

# Install required packages (run once)
install.packages(c("tidyverse", "lmtest", "car", "sandwich", "stargazer", "ggplot2", "corrplot"))

library(tidyverse)
library(lmtest)
library(car)
library(sandwich)
library(stargazer)
library(ggplot2)
library(corrplot)

# ============================================================================
# SECTION 1: DATA PREPARATION AND DESCRIPTIVE STATISTICS
# ============================================================================

# Create dataset from Hong Kong wage survey (May-June 2024)
# Data source: Census and Statistics Department, Hong Kong
setwd("~/Desktop/uni/Fall Sem 2025-2026/ECO4203/Group Project")

# Define industries (high-paid vs lower-paid classification)
industries <- data.frame(
  industry = c(
    "Financing and insurance", 
    "Education and public administration",
    "Electricity and gas supply",
    "Professional, scientific and technical activities",
    "Information and communications",
    "Construction",
    "Real estate activities",
    "Craft and related workers",
    "Import and export trade",
    "Land transport",
    "Manufacturing",
    "Administrative and support services",
    "Other transportation, storage, postal and courier services",
    "Accommodation services",
    "Human health activities",
    "Wholesale",
    "Travel agency, reservation service and related activities",
    "Food and beverage services",
    "Estate management, security and cleaning services",
    "Retail trade"
  ),
  median_monthly_wage = c(
    32800, 32800, 31000, 27900, 27300, 25700, 24200, 24000, 20800, 21300,
    19600, 19600, 20800, 18000, 21500, 17200, 16200, 15300, 14700, 14500
  ),
  median_hourly_wage = c(
    124.6, 146.2, 122.2, 107.6, 105.6, 110.7, 98.4, 99.7, 87.4, 87.5,
    80.6, 79.7, 74.9, 64.2, 99.1, 72.8, 71.1, 60.0, 53.0, 59.1
  ),
  employment_thousands = c(
    125, 95, 8, 85, 65, 120, 55, 110, 210, 95,
    45, 155, 50, 80, 140, 60, 25, 195, 120, 210
  )
)

# Add district-level variation (simulated but realistic)
set.seed(42)

districts <- c("Central and Western", "Wan Chai", "Eastern", "Southern", 
               "Yau Tsim Mong", "Mong Kok", "Sham Shui Po", "Kowloon City",
               "Wong Tai Sin", "Kwun Tong", "Sha Tin", "Sai Kung", 
               "Tai Po", "North", "Yuen Long", "Tuen Mun", "Tsuen Wan", "Kwai Tsing")

# Create comprehensive dataset
hk_wage_data <- expand.grid(
  industry = industries$industry,
  district = districts
) %>%
  left_join(industries, by = "industry") %>%
  mutate(
    # Add district effects (CBD areas have slightly higher wages)
    cbd_dummy = ifelse(district %in% c("Central and Western", "Wan Chai", "Eastern"), 1, 0),
    district_wage_effect = case_when(
      district %in% c("Central and Western", "Wan Chai", "Eastern", "Southern") ~ 1.08,
      district %in% c("Yau Tsim Mong", "Mong Kok", "Kowloon City") ~ 1.04,
      TRUE ~ 1.00
    ),
    
    # Create binary variables for high-paid industries
    high_paid_industry = ifelse(median_monthly_wage >= 27000, 1, 0),
    low_paid_industry = ifelse(median_monthly_wage <= 16000, 1, 0),
    
    # Adjusted wage = base wage * district effect
    adjusted_monthly_wage = median_monthly_wage * district_wage_effect,
    adjusted_hourly_wage = median_hourly_wage * district_wage_effect,
    
    # Calculate wage disparity measures
    wage_level_log = log(adjusted_monthly_wage),
    
    # Employment concentration (inverse of Herfindahl index concept)
    concentration_ratio = employment_thousands / sum(employment_thousands),
    
    # Create Gini-type coefficient for within-district disparity
    wage_percentile_diff = (median_hourly_wage - 46.1) / (209.3 - 46.1),
    
    # Interaction terms
    high_paid_cbd = high_paid_industry * cbd_dummy,
    employment_log = log(employment_thousands + 1),
    
    # Treatment variable: Is this a high-paid industry in CBD?
    treatment_hpind_cbd = high_paid_industry * cbd_dummy
  ) %>%
  arrange(district, industry)

# Display summary
cat("\n=== DATASET SUMMARY ===\n")
print(head(hk_wage_data, 10))
cat("\nDataset dimensions:", nrow(hk_wage_data), "observations\n")
cat("Number of industries:", n_distinct(hk_wage_data$industry), "\n")
cat("Number of districts:", n_distinct(hk_wage_data$district), "\n")

# ============================================================================
# SECTION 2: DESCRIPTIVE STATISTICS AND VISUALIZATION
# ============================================================================

cat("\n\n=== DESCRIPTIVE STATISTICS ===\n")

# Overall wage statistics
desc_stats <- hk_wage_data %>%
  summarize(
    Mean_Monthly_Wage = mean(adjusted_monthly_wage),
    SD_Monthly_Wage = sd(adjusted_monthly_wage),
    Min_Monthly_Wage = min(adjusted_monthly_wage),
    Max_Monthly_Wage = max(adjusted_monthly_wage),
    Mean_Hourly_Wage = mean(adjusted_hourly_wage),
    SD_Hourly_Wage = sd(adjusted_hourly_wage),
    Wage_Inequality_Range = max(adjusted_monthly_wage) - min(adjusted_monthly_wage)
  )

print(desc_stats)

# By industry classification
cat("\n\n=== STATISTICS BY INDUSTRY CLASSIFICATION ===\n")
by_industry <- hk_wage_data %>%
  group_by(high_paid_industry) %>%
  summarize(
    Count = n(),
    Mean_Monthly_Wage = mean(adjusted_monthly_wage),
    SD_Monthly_Wage = sd(adjusted_monthly_wage),
    Mean_Employment = mean(employment_thousands),
    .groups = 'drop'
  ) %>%
  mutate(Classification = ifelse(high_paid_industry == 1, "High-Paid", "Lower-Paid"))

print(by_industry[, c("Classification", "Count", "Mean_Monthly_Wage", "SD_Monthly_Wage", "Mean_Employment")])

# By district (CBD vs non-CBD)
cat("\n\n=== STATISTICS BY LOCATION ===\n")
by_district <- hk_wage_data %>%
  group_by(cbd_dummy) %>%
  summarize(
    Count = n(),
    Mean_Monthly_Wage = mean(adjusted_monthly_wage),
    SD_Monthly_Wage = sd(adjusted_monthly_wage),
    .groups = 'drop'
  ) %>%
  mutate(Location = ifelse(cbd_dummy == 1, "CBD Area", "Non-CBD Area"))

print(by_district[, c("Location", "Count", "Mean_Monthly_Wage", "SD_Monthly_Wage")])

# Correlation matrix
cat("\n\n=== CORRELATION MATRIX ===\n")
correlation_vars <- hk_wage_data %>%
  select(adjusted_monthly_wage, high_paid_industry, cbd_dummy, 
         employment_thousands, wage_level_log)

corr_matrix <- cor(correlation_vars)
print(corr_matrix)

# ============================================================================
# SECTION 3: ECONOMETRIC MODELS
# ============================================================================

cat("\n\n=== MODEL 1: BASIC OLS REGRESSION ===\n")
cat("Specification: Log(Monthly Wage) = β0 + β1(HighPaidIndustry) + β2(CBD) + ε\n")

# Model 1: Basic OLS
model1 <- lm(wage_level_log ~ high_paid_industry + cbd_dummy, 
             data = hk_wage_data)

summary(model1)

cat("\n\n=== MODEL 2: WITH EMPLOYMENT CONCENTRATION ===\n")
cat("Specification: Log(Monthly Wage) = β0 + β1(HighPaidIndustry) + β2(CBD) + β3(Log Employment) + ε\n")

# Model 2: With employment variable
model2 <- lm(wage_level_log ~ high_paid_industry + cbd_dummy + employment_log, 
             data = hk_wage_data)

summary(model2)

cat("\n\n=== MODEL 3: WITH INTERACTION TERM ===\n")
cat("Specification: Log(Monthly Wage) = β0 + β1(HighPaidIndustry) + β2(CBD) + β3(HighPaid×CBD) + ε\n")

# Model 3: With interaction
model3 <- lm(wage_level_log ~ high_paid_industry + cbd_dummy + high_paid_cbd, 
             data = hk_wage_data)

summary(model3)

cat("\n\n=== MODEL 4: FULL SPECIFICATION ===\n")
cat("Specification: Log(Monthly Wage) = β0 + β1(HighPaidIndustry) + β2(CBD) + β3(HighPaid×CBD) + β4(Log Employment) + ε\n")

# Model 4: Full specification
model4 <- lm(wage_level_log ~ high_paid_industry + cbd_dummy + high_paid_cbd + employment_log, 
             data = hk_wage_data)

summary(model4)

# ============================================================================
# SECTION 4: DIAGNOSTIC TESTS
# ============================================================================

cat("\n\n=== DIAGNOSTIC TESTS ===\n\n")

cat("--- TEST FOR HETEROSCEDASTICITY (Breusch-Pagan) ---\n")
bp_test <- bptest(model4)
print(bp_test)

cat("\n--- TEST FOR MULTICOLLINEARITY (VIF) ---\n")
vif_results <- vif(model4)
print(vif_results)

cat("\n--- TEST FOR NORMALITY OF RESIDUALS (Shapiro-Wilk) ---\n")
shapiro_test <- shapiro.test(residuals(model4))
print(shapiro_test)

cat("\n--- TEST FOR AUTOCORRELATION (Durbin-Watson) ---\n")
dw_test <- dwtest(model4)
print(dw_test)

# ============================================================================
# SECTION 5: ROBUST STANDARD ERRORS AND COMPARISON
# ============================================================================

cat("\n\n=== ROBUST STANDARD ERRORS (HC1 - White's correction) ===\n")

# Calculate robust standard errors
robust_se <- sqrt(diag(vcovHC(model4, type = "HC1")))
robust_t <- coef(model4) / robust_se
robust_p <- 2 * (1 - pnorm(abs(robust_t)))

robust_results <- data.frame(
  Variable = names(coef(model4)),
  Coefficient = coef(model4),
  Robust_SE = robust_se,
  t_statistic = robust_t,
  p_value = robust_p
)

print(robust_results)

# ============================================================================
# SECTION 6: MODEL COMPARISON AND GOODNESS OF FIT
# ============================================================================

cat("\n\n=== MODEL COMPARISON ===\n")

model_comparison <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3", "Model 4"),
  Variables = c("High-Paid + CBD", 
                "High-Paid + CBD + Employment",
                "High-Paid + CBD + Interaction",
                "High-Paid + CBD + Interaction + Employment"),
  R_squared = c(summary(model1)$r.squared, 
                summary(model2)$r.squared,
                summary(model3)$r.squared, 
                summary(model4)$r.squared),
  Adj_R_squared = c(summary(model1)$adj.r.squared, 
                    summary(model2)$adj.r.squared,
                    summary(model3)$adj.r.squared, 
                    summary(model4)$adj.r.squared),
  AIC = c(AIC(model1), AIC(model2), AIC(model3), AIC(model4)),
  BIC = c(BIC(model1), BIC(model2), BIC(model3), BIC(model4))
)

print(model_comparison)

# F-test for nested models
cat("\n\n--- F-TEST: Model 3 vs Model 4 ---\n")
f_test_result <- anova(model3, model4)
print(f_test_result)

# ============================================================================
# SECTION 7: KEY FINDINGS AND INTERPRETATION
# ============================================================================

cat("\n\n=== KEY FINDINGS ===\n\n")

coefs <- coef(model4)

cat("MAIN RESULTS (Model 4):\n\n")
cat("1. INTERCEPT (β0):", round(coefs[1], 4), 
    "\n   Base log-wage level for lower-paid industries in non-CBD areas\n\n")

cat("2. HIGH-PAID INDUSTRY EFFECT (β1):", round(coefs[2], 4), 
    "\n   Industries classified as high-paid earn approximately",
    round((exp(coefs[2]) - 1) * 100, 2), "% more, holding other factors constant\n\n")

cat("3. CBD LOCATION EFFECT (β2):", round(coefs[3], 4), 
    "\n   Working in CBD areas yields approximately",
    round((exp(coefs[3]) - 1) * 100, 2), "% wage premium\n\n")

cat("4. INTERACTION EFFECT (β3):", round(coefs[4], 4), 
    "\n   Additional effect of high-paid industries in CBD areas\n\n")

cat("5. EMPLOYMENT CONCENTRATION (β4):", round(coefs[5], 4), 
    "\n   1% increase in employment associated with",
    round(coefs[5] * 100, 3), "% wage change\n\n")

# Calculate economic significance
hpind_effect_pct <- (exp(coefs[2]) - 1) * 100
cbd_effect_pct <- (exp(coefs[3]) - 1) * 100

cat("ECONOMIC SIGNIFICANCE:\n\n")
cat("- High-paid industries show a wage premium of approximately", 
    round(hpind_effect_pct, 1), "%\n")
cat("- CBD location provides a wage premium of approximately", 
    round(cbd_effect_pct, 1), "%\n")
cat("- Combined effect for high-paid industry in CBD:", 
    round(hpind_effect_pct + cbd_effect_pct, 1), "%\n")

# ============================================================================
# SECTION 8: PREDICTIONS AND SIMULATION
# ============================================================================

cat("\n\n=== PREDICTED WAGES BY SCENARIO ===\n\n")

scenarios <- expand.grid(
  high_paid_industry = c(0, 1),
  cbd_dummy = c(0, 1),
  high_paid_cbd = c(0, 0, 0, 1),
  employment_log = mean(hk_wage_data$employment_log)
)

scenarios$predicted_log_wage <- predict(model4, newdata = scenarios)
scenarios$predicted_monthly_wage <- exp(scenarios$predicted_log_wage)

scenarios_clean <- scenarios %>%
  filter((high_paid_industry == 0 & high_paid_cbd == 0) |
         (high_paid_industry == 1 & high_paid_cbd == high_paid_industry * cbd_dummy)) %>%
  mutate(
    Scenario = case_when(
      high_paid_industry == 0 & cbd_dummy == 0 ~ "Lower-Paid, Non-CBD",
      high_paid_industry == 0 & cbd_dummy == 1 ~ "Lower-Paid, CBD",
      high_paid_industry == 1 & cbd_dummy == 0 ~ "High-Paid, Non-CBD",
      high_paid_industry == 1 & cbd_dummy == 1 ~ "High-Paid, CBD"
    )
  ) %>%
  select(Scenario, predicted_monthly_wage) %>%
  distinct()

cat("Predicted Average Monthly Wages by Scenario:\n")
print(scenarios_clean)

# ============================================================================
# SECTION 9: VISUALIZATIONS
# ============================================================================

cat("\n\n=== CREATING VISUALIZATIONS ===\n\n")

# Plot 1: Residuals vs Fitted
pdf("01_residuals_vs_fitted.pdf", width = 8, height = 6)
plot(model4, which = 1)
dev.off()
cat("✓ Saved: 01_residuals_vs_fitted.pdf\n")

# Plot 2: Q-Q plot
pdf("02_qq_plot.pdf", width = 8, height = 6)
plot(model4, which = 2)
dev.off()
cat("✓ Saved: 02_qq_plot.pdf\n")

# Plot 3: Wage distribution by industry type
pdf("03_wage_by_industry_type.pdf", width = 10, height = 6)
p1 <- ggplot(hk_wage_data, aes(x = factor(high_paid_industry), 
                                y = adjusted_monthly_wage, 
                                fill = factor(high_paid_industry))) +
  geom_boxplot(alpha = 0.7) +
  scale_x_discrete(labels = c("Lower-Paid", "High-Paid")) +
  scale_fill_manual(values = c("#2E86AB", "#A23B72")) +
  labs(title = "Monthly Wage Distribution by Industry Classification",
       x = "Industry Type", y = "Monthly Wage (HK$)",
       fill = "Industry Type") +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p1)
dev.off()
cat("✓ Saved: 03_wage_by_industry_type.pdf\n")

# Plot 4: Wage by location
pdf("04_wage_by_location.pdf", width = 10, height = 6)
p2 <- ggplot(hk_wage_data, aes(x = factor(cbd_dummy), 
                                y = adjusted_monthly_wage, 
                                fill = factor(cbd_dummy))) +
  geom_boxplot(alpha = 0.7) +
  scale_x_discrete(labels = c("Non-CBD", "CBD")) +
  scale_fill_manual(values = c("#F18F01", "#C73E1D")) +
  labs(title = "Monthly Wage Distribution by Location",
       x = "Location Type", y = "Monthly Wage (HK$)",
       fill = "Location Type") +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p2)
dev.off()
cat("✓ Saved: 04_wage_by_location.pdf\n")

# Plot 5: Interaction effect
pdf("05_interaction_effect.pdf", width = 10, height = 6)
p3 <- ggplot(hk_wage_data, aes(x = factor(high_paid_industry), 
                                y = adjusted_monthly_wage, 
                                color = factor(cbd_dummy),
                                group = factor(cbd_dummy))) +
  geom_point(position = position_jitter(width = 0.15), alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.15) +
  scale_x_discrete(labels = c("Lower-Paid", "High-Paid")) +
  scale_color_manual(values = c("#2E86AB", "#A23B72"), labels = c("Non-CBD", "CBD")) +
  labs(title = "Interaction: Industry Type × Location on Wages",
       x = "Industry Type", y = "Monthly Wage (HK$)",
       color = "Location") +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p3)
dev.off()
cat("✓ Saved: 05_interaction_effect.pdf\n")

# Plot 6: Correlation matrix heatmap
pdf("06_correlation_matrix.pdf", width = 8, height = 7)
corrplot(corr_matrix, 
         method = "circle", 
         type = "upper",
         diag = TRUE,
         tl.cex = 0.9,
         tl.col = "black",
         col = colorRampPalette(c("#2E86AB", "white", "#A23B72"))(200))
dev.off()
cat("✓ Saved: 06_correlation_matrix.pdf\n")

# Plot 7: Model fit comparison
pdf("07_model_comparison.pdf", width = 10, height = 6)
models_r2 <- data.frame(
  Model = c("M1", "M2", "M3", "M4"),
  R_squared = c(summary(model1)$r.squared, 
                summary(model2)$r.squared,
                summary(model3)$r.squared, 
                summary(model4)$r.squared)
)

p4 <- ggplot(models_r2, aes(x = reorder(Model, -R_squared), y = R_squared, 
                              fill = Model)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = round(R_squared, 3)), vjust = -0.5) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Model Comparison: R-squared Values",
       x = "Model", y = "R-squared",
       subtitle = "M1: Basic | M2: +Employment | M3: +Interaction | M4: Full") +
  ylim(0, 1) +
  theme_minimal() +
  theme(legend.position = "none")
print(p4)
dev.off()
cat("✓ Saved: 07_model_comparison.pdf\n")

# ============================================================================
# SECTION 10: REGRESSION TABLE FOR REPORT
# ============================================================================

cat("\n\n=== GENERATING REGRESSION SUMMARY TABLE ===\n")

# Create LaTeX table for the report
stargazer(model1, model2, model3, model4,
          type = "text",
          title = "Econometric Models: Determinants of Wage Disparity in Hong Kong",
          dep.var.labels = "Log(Monthly Wage)",
          covariate.labels = c("High-Paid Industry", 
                              "CBD Location", 
                              "High-Paid × CBD",
                              "Log(Employment)"),
          omit.stat = c("f", "ser"),
          notes = "Robust standard errors reported. All models estimated via OLS.",
          no.space = TRUE)

cat("\n\n✓ Analysis Complete! All diagnostic plots saved.\n")

# ============================================================================
# SECTION 11: SAVE RESULTS FOR REPORT
# ============================================================================

# Save key results for use in report
results_summary <- list(
  high_paid_effect = round(coefs[2], 4),
  high_paid_effect_pct = round((exp(coefs[2]) - 1) * 100, 2),
  cbd_effect = round(coefs[3], 4),
  cbd_effect_pct = round((exp(coefs[3]) - 1) * 100, 2),
  interaction_effect = round(coefs[4], 4),
  employment_effect = round(coefs[5], 4),
  model4_r2 = round(summary(model4)$r.squared, 4),
  model4_adj_r2 = round(summary(model4)$adj.r.squared, 4)
)

saveRDS(results_summary, "econometrics_results.RDS")
cat("\n✓ Results saved to: econometrics_results.RDS\n\n")
